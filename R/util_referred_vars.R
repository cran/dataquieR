#' For a group of variables (original) the function provides all original plus
#' referred variables in the metadata and a new item_level metadata
#' including information on the original variables and the referred variables
#'
#' @param meta_data [data.frame] old name for `item_level`
#' @param meta_data_segment [data.frame] -- optional: Segment level metadata
#' @param meta_data_dataframe [data.frame] -- optional if `study_data` is present:
#'                                                     Data frame level metadata
#' @param meta_data_cross_item [data.frame] -- optional: Cross-item level
#'                                                                 metadata
#' @param meta_data_item_computation [data.frame] -- optional: Computed items
#'                                                                     metadata
#' @param resp_vars [variable list] the name of the original variables.
#' @param label_col [variable attribute] the name of the column in the metadata
#'                                       with labels of variables
#' @param id_vars [variable] a vector containing the name/s of the variables
#'                            containing ids
#' @param vars_in_subgroup [variable] a vector containing the name/s of the
#'                                    variable/s mentioned inside the
#'                                    subgroup rule
#' @param strata_column [variable] name of a study variable used to stratify the
#'                                    report by and to add as referred variable
#'
#'
#' @return a named list containing the referred variables and
#' a new item_level metadata including information on the original variables
#' and the referred variables
#'
#' @noRd

util_referred_vars <- function(resp_vars,
                               id_vars = character(0),
                               vars_in_subgroup = character(0),
                               meta_data,
                               label_col,
                               meta_data_segment = NULL,
                               meta_data_dataframe = NULL,
                               meta_data_cross_item = NULL,
                               meta_data_item_computation = NULL,
                               strata_column = NULL) {
  #Preparation-----
    key_cols <- util_variable_references(meta_data)
  #checks ----
    #TODO: add checks

   # util_expect_data_frame(meta_data,
    #col_names = list(DATAFRAMES = is.character))

  #-----
  vars <- resp_vars
  repeat {
    # referred variables from key_cols----
    referred_vars <-
      unique(unlist(meta_data[meta_data[[VAR_NAMES]] %in%
                                vars, key_cols], recursive = TRUE))
    # referred variables from cross-item level metadata----
    referred_vars <-
      c(referred_vars, unlist(
        util_parse_assignments(meta_data_cross_item[[VARIABLE_LIST]],
                               multi_variate_text = TRUE)
      ))


    # referred variables from cross-item level metadata from *_LIMITS columns----
    # Define a vector of the LIMITS column names to iterate over
    limit_cols <- c("HARD_LIMITS", "SOFT_LIMITS", "DETECTION_LIMITS")

    # if at least one limit column is non-empty
    if (any(sapply(limit_cols, function(col)
      any(!util_empty(meta_data_cross_item[[col]]))))) {

      # Pre-calculate needles_var_names and needles once
      needles_var_names <- unique(c(meta_data[[VAR_NAMES]],
                                    meta_data[[label_col]],
                                    meta_data[[LABEL]],
                                    meta_data[[LONG_LABEL]],
                                    meta_data[["ORIGINAL_VAR_NAMES"]],
                                    meta_data[["ORIGINAL_LABEL"]]))
      needles <- paste0("[", needles_var_names, "]")

      results_list <- lapply(
        limit_cols,
        function(col) {
          if (any(!util_empty(meta_data_cross_item[[col]]))) {
            x <- vapply(setNames(needles, nm = needles_var_names),
                        grepl,
                        setNames(nm = meta_data_cross_item[[col]]),
                        fixed = TRUE,
                        FUN.VALUE = logical(length =
                                              nrow(meta_data_cross_item))
            )
            # Ensure x is a matrix
            if (is.vector(x)) {
              x <- as.matrix(t(x))
            }
            # Extract variable names, ensure unique and sorted per row
            variablelist_raw <-
              unname(lapply(as.data.frame(t(x)),
                            function(xx) unique(sort(colnames(x)[xx]))))
            # Collapse the list of variable names into a single string
            variablelist <-
              lapply(variablelist_raw,
                     paste0,
                     collapse = sprintf(" %s ", SPLIT_CHAR))
            return(variablelist)
          }
          return(NULL) # Return NULL if the limit column is empty
        }
      )
      variable_list_limits <- unique(unname(unlist(results_list)))
      #remove empty results
      variable_list_limits <- variable_list_limits[variable_list_limits != ""]
      # separate variables as single ones
      variable_list_limits <- lapply(X = variable_list_limits,
                                     FUN = util_parse_assignments)
      # get a vector of variables from all limits
      variable_list_limits <- unique(unname(unlist(variable_list_limits)))


      #merge the variablea to the referred_vars
      referred_vars <-
        unique(c(referred_vars, variable_list_limits))

    }

    # referred variables from meta_data_item_computation metadata----
    referred_vars <-
      c(referred_vars, unlist(
        util_parse_assignments(meta_data_item_computation[[VARIABLE_LIST]],
                               multi_variate_text = TRUE)
      ))

    # looks to all possible labels and
    # return var_names
    referred_vars <-
      unique(util_find_var_by_meta(unique(
        unlist(
          util_parse_assignments(referred_vars,
                                 multi_variate_text = TRUE)
        )
      ), meta_data = meta_data))
    #remove na's
    referred_vars <-
      referred_vars[!is.na(referred_vars)]

    if (all(referred_vars %in% vars)) {
      #if all variables are already present
      # than it stops
      break
    } else {
      # otherwise add the new variables and
      # restart the loop
      vars <-
        union(vars, referred_vars)
    }
  }

  resp_vars_plus_referred <- vars
  rm(vars)

  # Add the var of strata_column if present
  if (!is.null(strata_column)) {
    var_split <- strata_column
    names(var_split) <- NULL
    resp_vars_plus_referred <- unique(c(resp_vars_plus_referred, var_split))
    rm(var_split)
  }
  # Add the var of id_vars if present
  if (length(id_vars) != 0) {
    resp_vars_plus_referred <- unique(c(resp_vars_plus_referred, id_vars))
  }
  # Add the var of vars_in_subgroup if present
  if (length(vars_in_subgroup) != 0) {
    resp_vars_plus_referred <- unique(c(resp_vars_plus_referred,
                                        vars_in_subgroup))
  }
  # Create a temporary subset of item_level metadata with only
  # resp_vars and referred vars
  temp_subset <-
    meta_data[meta_data$VAR_NAMES %in% resp_vars_plus_referred,
              , drop = FALSE]
  not_resp_vars <- setdiff(resp_vars_plus_referred, resp_vars)

  # Give a warning if not all resp_vars are present in the item_level metadata
  if(length(setdiff(resp_vars, temp_subset$VAR_NAMES)) > 0 )  {
    util_warning(
      "Variables in resp_vars that are not present in the item_level metadata: %s",
      setdiff(resp_vars, temp_subset$VAR_NAMES),
      applicability_problem = TRUE
    )
  }

  # Add all ID vars from the dataframe_level metadata----
  # obtain the code of the data frames
  if(!is.null(meta_data_dataframe) ||
     nrow(meta_data_dataframe) > 0) {
    if(DATAFRAMES %in% colnames(meta_data) &&
       DF_CODE %in% colnames(meta_data_dataframe)) {
      itemlevel_subset_df <-
        temp_subset[, c(VAR_NAMES, DATAFRAMES), drop = FALSE]

      dfr_code <- unique(unname(unlist(
        util_parse_assignments(
          itemlevel_subset_df$DATAFRAMES,
          split_char = SPLIT_CHAR,
          multi_variate_text = TRUE) )))

      #obtain the id from the dataframe interested by the resp-vars and referred vars
      id_vars_referred <- meta_data_dataframe[meta_data_dataframe[[DF_CODE]] %in%
                                                dfr_code, DF_ID_VARS, drop = TRUE]
      rm(dfr_code)
    } else {
      #obtain the id from the dataframe interested by the resp-vars and referred vars
      id_vars_referred <- meta_data_dataframe[, DF_ID_VARS, drop = TRUE]
    }

  }  else {
    id_vars_referred <- character(0)
  }


if(!is.null(meta_data_segment)) {
    #Obtain the ID from segment level if present
    list_seg <-
      unique(temp_subset[, STUDY_SEGMENT, drop = TRUE])
    #obtain the id from the segment interested by the resp-vars and referred vars
    id_vars_seg_referred <- meta_data_segment[meta_data_segment[[STUDY_SEGMENT]] %in%
                                                list_seg, SEGMENT_ID_VARS, drop = TRUE]
    #Put together all the id vars
    id_vars_referred <- unique(c(id_vars_referred,id_vars_seg_referred))
    rm(list_seg, id_vars_seg_referred)

  }

   # reduce to vector without | in case multiple vars stated in one cell
  id_vars_referred <-
    unlist(sapply(id_vars_referred,
                  function(x) {
                    util_parse_assignments(x, split_char = c(SPLIT_CHAR),
                                           multi_variate_text = TRUE,
                                           split_on_any_split_char = TRUE)
                  })) #it also get rid of eventual NA in the id_vars_referred
  names(id_vars_referred) <- NULL
  id_vars_referred <- unique(id_vars_referred)

  #Create a new complete list of variables with resp_vars, referred_vars and id vars
  resp_vars_complete <- union(resp_vars_plus_referred, id_vars_referred)
  rm(resp_vars_plus_referred)



  #obtain the metadata for the vars----
  # Set the VARIABLE_ROLE of referred_vars as suppress
  vars_referred <- union(id_vars_referred, not_resp_vars)
  vars_referred <- setdiff(vars_referred, resp_vars)
  rm(id_vars_referred, not_resp_vars)


  md_complete <-
    meta_data[meta_data$VAR_NAMES %in% resp_vars_complete,
              , drop = FALSE]

  md_complete <- md_complete %>%
    dplyr::mutate(VARIABLE_ROLE = ifelse(VAR_NAMES %in% vars_referred,
                                         "suppress",
                                         VARIABLE_ROLE))


  # Create the new item level metadata with only rows of resp vars and referred+id
  rm(temp_subset, vars_referred, key_cols)

 return(list(vars_complete = resp_vars_complete,
             md_complete = md_complete))

}








