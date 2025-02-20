#' Generate a stratified full DQ report
#'
#' @param resp_vars [variable] the names of the measurement variables, if
#'                             missing or `NULL`, all variables will be included
#' @param id_vars [variable] a vector containing the name/s of the variables
#'                            containing ids, to
#'                            be used to merge multiple data frames if provided
#'                            in `study_data` and to be add to referred vars
#' @param study_data [data.frame] the data frame that contains the measurements:
#'                                it can be an R object (e.g., `bia`), a
#'                                data frame (e.g., `"C:/Users/data/bia.dta"`),
#'                                a vector containing data frames files (e.g.,
#'                                c(`"C:/Users/data/bia.dta"`,
#'                                `C:/Users/data/biames.dta"`)), or it can be
#'                                left empty and the data frames are provided
#'                                in the data frame level metadata. If only the
#'                                file name without path is provided
#'                                (e.g., `"bia.dta"`), the file
#'                                name needs the extension and the path must be
#'                                provided in the argument `input_dir`. It can
#'                                also contain only the file name in case of
#'                                example data from the package `dataquieR`
#'                                (e.g., `"study_data"` or `"ship"`)
#' @param item_level [data.frame] the data frame that contains metadata
#'                               attributes of study data
#' @param meta_data [data.frame] old name for `item_level`
#' @param meta_data_segment [data.frame] -- optional: Segment level metadata
#' @param meta_data_dataframe [data.frame] -- optional if `study_data` is
#'                                            present: Data frame level metadata
#' @param meta_data_cross_item [data.frame] -- optional: Cross-item level
#'                                                                 metadata
#' @param meta_data_item_computation [data.frame] -- optional: Computed items
#'                                                                     metadata
#' @param ... arguments to be passed through to [dq_report] or [dq_report2]
#' @param label_col [variable attribute] the name of the column in the
#'                                       metadata containing the labels of
#'                                       the variables
#' @param meta_data_v2 [character] path or file name of the workbook like
#'                                 metadata file, see
#'                                 [`prep_load_workbook_like_file`] for details.
#'                                 **ALL LOADED DATAFRAMES WILL BE PURGED**,
#'                                 using [`prep_purge_data_frame_cache`],
#'                                 if you specify `meta_data_v2`
#' @param segment_column [variable attribute] name of a metadata attribute
#'                                             usable to split the report in
#'                                             sections of variables, e.g. all
#'                                             blood-pressure related variables.
#'                                             By default,
#'                                             reports are split by
#'                                             [STUDY_SEGMENT] if available and
#'                                             no segment_column nor
#'                                             strata_column or subgroup
#'                                             are defined.
#'                                             To create an un-split report
#'                                             please write explicitly
#'                                             the argument
#'                                             'segment_column = NULL'
#' @param strata_column [variable] name of a study variable to stratify the
#'                                    report by, e.g. the study centers.
#'                                    Both labels and `VAR_NAMES` are accepted.
#'                                    In case of NAs in the selected variable,
#'                                    a separate report containing the NAs
#'                                    subset will be created
#' @param strata_select [character] if given, the strata of strata_column
#'                                       are limited to the content of this
#'                                       vector. A character vector or a regular
#'                                       expression can be provided
#'                                       (e.g., "^a.*$"). This argument can not
#'                                       be used if no strata_column is
#'                                       provided
#' @param segment_select [character] if given, the levels of segment_column
#'                                       are limited to the content of this
#'                                       vector. A character vector or a
#'                                       regular expression (e.g., ".*_EXAM$")
#'                                       can be provided.
#'                                       This argument can not be used if no
#'                                       segment_column is provided.
#' @param input_dir [character] if given, the study data files that have
#'                              no path and that are not URL are searched in
#'                              this directory. Also `meta_data_v2` is searched
#'                              in this directory if no path is provided
#' @param advanced_options [list] options to set during report computation,
#'                                see [options()]
#' @param output_dir [character] if given, the output is not returned but saved
#'                               in this directory
#' @param missing_tables [character] the name of the data frame containing the
#'                                   missing codes, it can be a vector if more
#'                                   than one table is provided. Example:
#'                                   `c("missing_table1", "missing_table2")`
#' @param also_print [logical] if `output_dir` is not `NULL`, also create
#'                             `HTML` output for each report using
#'                             [print.dataquieR_resultset2()]
#'                               written to the path `output_dir`
#' @param disable_plotly [logical] do not use `plotly`, even if installed
#' @param selection_type [character] optional, can only be specified if a
#'                                              `strata_select` or
#'                                              `strata_exclude` is specified.
#'                                              If not present the
#'                                              function try to guess what the
#'                                              user typed as `strata_select` or
#'                                               `strata_exclude`.
#'                                              There are 3 options:
#'                                              `value` indicating that the
#'                                              stratum selected is a value and
#'                                              not a value_label.
#'                                              For example `"0"`;
#'                                              `v_label` indicating that the
#'                                              stratum specified is a label.
#'                                              For example `"male"`.
#'                                              `regex` indicating that the user
#'                                              specified strata using a regular
#'                                              expression. For example `"^Ber"`
#'                                              to select all strata starting
#'                                              with that letters
#' @param segment_exclude [character] optional, can only be specified if a
#'                                              `segment_column` is specified.
#'                                               The levels of `segment_column`
#'                                               will not include the content of
#'                                               this argument.
#'                                               A character vector or
#'                                               a regular  expression can be
#'                                               provided  (e.g., "^STU").
#' @param strata_exclude [character] optional, can only be specified if a
#'                                              `strata_column` is specified.
#'                                              The strata of `strata_column`
#'                                               will not include the content of
#'                                               this argument.
#'                                               A character vector or
#'                                               a regular  expression can be
#'                                               provided  (e.g., "^STU").
#' @param subgroup [character] optional, to define subgroups of cases. Rules are
#'                                      to be written as `REDCap` rules.
#'                                      Only VAR_NAMES are accepted in the rules.
#' @param cross_item_level [data.frame] alias for `meta_data_cross_item`
#' @param `cross-item_level` [data.frame] alias for `meta_data_cross_item`
#' @param segment_level [data.frame] alias for `meta_data_segment`
#' @param dataframe_level [data.frame] alias for `meta_data_dataframe`
#' @param item_computation_level [data.frame] alias for
#'                               `meta_data_item_computation`
#' @param storr_factory [function] `NULL`, or
#'                        a function returning a `storr` object as
#'                        back-end for the report's results. If used with
#'                        `cores > 1`, the storage must be accessible from all
#'                        cores and capable of concurrent writing according
#'                        to `storr`. Hint: `dataquieR` currently only supports
#'                        `storr::storr_rds()`, officially, while other back-
#'                        ends may nevertheless work, yet, they are not tested.
#' @param amend [logical] if there is already data in.`storr_factory`,
#'                        use it anyways -- unsupported, so far!
#' @param view [logical] open the returned report
#'
#' @return [invisible()]. named [list] of named [list]s of [dq_report2] reports
#'         or, if `output_dir` has been specified, `invisible(NULL)`
#'
#' @seealso [dq_report]
#'
#' @export
#'
#' @examples
#' \dontrun{ # really long-running example.
#' prep_load_workbook_like_file("meta_data_v2")
#' rep <- dq_report_by("study_data", label_col =
#'   LABEL, strata_column = "CENTER_0")
#' rep <- dq_report_by("study_data",
#'   label_col = LABEL, strata_column = "CENTER_0",
#'   segment_column = NULL
#' )
#' unlink("/tmp/testRep/", force = TRUE, recursive = TRUE)
#' dq_report_by("study_data",
#'   label_col = LABEL, strata_column = "CENTER_0",
#'   segment_column = STUDY_SEGMENT, output_dir = "/tmp/testRep"
#' )
#' unlink("/tmp/testRep/", force = TRUE, recursive = TRUE)
#' dq_report_by("study_data",
#'   label_col = LABEL, strata_column = "CENTER_0",
#'   segment_column = NULL, output_dir = "/tmp/testRep"
#' )
#' dq_report_by("study_data",
#'   label_col = LABEL,
#'   segment_column = STUDY_SEGMENT, output_dir = "/tmp/testRep"
#' )
#' dq_report_by("study_data",
#'   label_col = LABEL,
#'   segment_column = STUDY_SEGMENT, output_dir = "/tmp/testRep",
#'   also_print = TRUE
#' )
#' dq_report_by(study_data = "study_data", meta_data_v2 = "meta_data_v2",
#'   advanced_options = list(dataquieR.study_data_cache_max = 0,
#'   dataquieR.study_data_cache_metrics = TRUE,
#'   dataquieR.study_data_cache_metrics_env = environment()),
#'   cores = NULL, dimensions = "int")
#' dq_report_by(study_data = "study_data", meta_data_v2 = "meta_data_v2",
#'   advanced_options = list(dataquieR.study_data_cache_max = 0),
#'   cores = NULL, dimensions = "int")
#' }
dq_report_by <- function(study_data,
                         item_level = "item_level",
                         meta_data_segment = "segment_level",
                         meta_data_dataframe = "dataframe_level",
                         meta_data_cross_item = "cross-item_level",
                         meta_data_item_computation = "item_computation_level",
                         missing_tables = NULL,
                         label_col,
                         meta_data_v2,
                         segment_column = NULL,
                         strata_column = NULL,
                         strata_select = NULL,
                         selection_type = NULL,
                         segment_select = NULL,
                         segment_exclude = NULL,
                         strata_exclude = NULL,
                         subgroup = NULL,
                         resp_vars = character(0),
                         id_vars = NULL,
                         advanced_options =  list(),
                         storr_factory = NULL,
                         amend = FALSE,
                         ...,
                         output_dir = NULL,
                         input_dir = NULL,
                         also_print = FALSE,
                         disable_plotly = FALSE,
                         view = TRUE,
                         meta_data = item_level,
                         cross_item_level,
                         `cross-item_level`,
                         segment_level,
                         dataframe_level,
                         item_computation_level) {

  util_stop_if_not(is.list(advanced_options))
  util_expect_scalar(view, check_type = is.logical)

  old_O <- options(c(
    list(
      dataquieR.CONDITIONS_WITH_STACKTRACE = FALSE,
      dataquieR.ERRORS_WITH_CALLER = FALSE,
      dataquieR.MESSAGES_WITH_CALLER = FALSE,
      dataquieR.WARNINGS_WITH_CALLER = FALSE,
      DT.warn.size = FALSE
    ),
    advanced_options
  ))
  on.exit(options(old_O))

  # store the call to use it later for the technical info in the reports
  call_report_by <- paste(deparse(sys.call()), collapse = "")

  # ?re-think the next line with
  # https://gitlab.com/libreumg/dataquier/-/issues/482
  # clear the study data cache
  # util_purge_study_data_cache()

  # Check the arguments (exception of segment_select and segment_exclude)----

  # check if both strata_column and strata_select are present
  if (!is.null(strata_select) && is.null(strata_column)) {
    util_error("strata_column is needed for selecting the strata")
  }

  # check if both strata_column and strata_exclude are present
  if (!is.null(strata_exclude) && is.null(strata_column)) {
    util_error("strata_column is needed for excluding the strata")
  }

  # check if both strata_select and strata_exclude are present
  # to use selection_type
  if (!is.null(selection_type) &&
      (is.null(strata_select) && is.null(strata_exclude))) {
    util_error(
      c(
        "selection_type can only be specified if",
        "strata_select or strata_exclude are present"
      )
    )
  }

  # check if the provided selection_type is acceptable
  if (!is.null(selection_type)) {
    util_stop_if_not(selection_type %in%
                       c("value", "v_label", "regex"),
                     label =
                       'The selection_type can only be "value", "v_label", or "regex"')
  }

  # check if subgroup rule is a string
  if (!is.null(subgroup)) {
    util_expect_scalar(
      arg_name = subgroup,
      allow_more_than_one = FALSE,
      check_type = is.character
    )
  }

  ### check output directory only if also_print == TRUE
  # if users specify an output dir, check if it is a scalar, a character
  # string, check if also_print is a scalar and logical,
  # stop if the specified dir already exists
  # provide an error if the dir can not be created
  if((!missing (also_print) && also_print == TRUE) ) {
    if (!missing(output_dir)) {
      util_expect_scalar(output_dir, check_type = is.character)
      util_expect_scalar(also_print, check_type = is.logical)
      if (dir.exists(output_dir)) {
        util_error("%s already exists. Remove the %s first",
                   dQuote(output_dir),
                   sQuote("output_dir"))
      }
      if (!dir.create(output_dir, FALSE, TRUE)) {
        util_error("Could not create %s", dQuote(output_dir))
      }
    }
  }

  ### check input directory
  # if users specify an input_dir, check if it is a scalar and a character
  # string, and stop if the specified dir does not exists
  # provide an error if the dir do not exists
  if (!missing(input_dir)) {
    util_expect_scalar(input_dir, check_type = is.character)
    if (!dir.exists(input_dir)) {
      util_error(
        "%s does not exist. Provide an %s containing the study data",
        dQuote(input_dir),
        sQuote("input_dir"))
    }
  }

  # check if resp_vars is a string vector
  if (length(resp_vars) > 0) {
    util_expect_scalar(
      arg_name = resp_vars,
      allow_more_than_one = TRUE,
      check_type = is.character
    )
  } else {
    resp_vars_complete <- character(0)
  }


  # Check and import item_level metadata ----
  ### load meta_data_v2 and item_level metadata
  # in case of presence of meta_data_v2 purge the cache and load it
  if (!missing(meta_data_v2)) {
    util_message("Have %s set, so I'll remove all loaded data frames",
                 sQuote("meta_data_v2"))
    prep_purge_data_frame_cache()
    #try to import the metadata, and if not possible, try to add
    # the path if provided
    m <- try(prep_load_workbook_like_file(meta_data_v2), silent = TRUE)
    if (inherits(m, "try-error")) {
      # in case the name of the file is provided without a path and there is an
      # input_dir, the name is fixed adding the path at the beginning
      if (!is.null(input_dir)) {
        if (!grepl(.Platform$file.sep, meta_data_v2, fixed = TRUE)) {
          if (endsWith(input_dir, .Platform$file.sep)) {
            input_dir <- substr(input_dir, 1, nchar(input_dir) - 1)
          }
          meta_data_v2 <- file.path(input_dir, meta_data_v2)
        }
      }
      prep_load_workbook_like_file(meta_data_v2)
    }

    # check if item level metadata is in the cache and provide an error if not;
    # if it is not present predicts item-level from the data
    if (!is.data.frame(meta_data) &&
        (length(meta_data) != 1 || (!is.character(meta_data)) ||
         !exists(meta_data, .dataframe_environment()) )) {
      w <- paste(
        "Did not find any sheet named %s in %s, is this",
        "really dataquieR version 2 metadata?"
      )
      if (requireNamespace("cli", quietly = TRUE)) {
        w <- cli::bg_red(cli::col_br_yellow(w))
      }
      util_warning(w, dQuote(meta_data),
                   dQuote(meta_data_v2),
                   immediate = TRUE)
    }
  }

  # check if meta_data (item_level) is a data frame,
  # if not look for it in the cache and in the file system
  util_expect_data_frame(meta_data)

  util_ck_arg_aliases()

  # Getting name of file indicated by user to add it as argument in util_verify_names
  # (not including names from dataframe_level metadata)
  if (!missing(study_data)) {
    # 1. the study_data argument is specified by the user, and is only 1 data frame
    ## 1a. the data frame is an object in the environment
    if (is.data.frame(study_data)) {
       # get the name of the study data from the function call
      name_sd <- head(as.character(substitute(study_data)), 1)

    } else if (length(study_data) == 1 &&
              is.character(study_data)) {
      # 1b. the name of a data frame (not loaded yet) was indicated by the user
      # add the path before the name of the study data
      if (!is.null(input_dir)) {
        if (!grepl(.Platform$file.sep, study_data, fixed = TRUE)) {
          if (endsWith(input_dir, .Platform$file.sep)) {
            input_dir <- substr(input_dir, 1, nchar(input_dir) - 1)
          }
          name_sd <- file.path(input_dir, study_data)
        }
      } else {
        name_sd <- study_data
      }

    } else if (!is.data.frame(study_data) && length(study_data) > 1) {
      ## 1c. two or more study data indicated as vector by the user
      # if the study names are a vector of names without path and they are
      # not URL, add the input_dir before the names of each study data
      # create a list containing all names of the study data
      if (!is.null(input_dir)) {
        name_sd <- vapply(study_data, function(x) {
          res <- x
          if (!grepl(.Platform$file.sep, x, fixed = TRUE)) {
            if (endsWith(input_dir, .Platform$file.sep)) {
              input_dir <- substr(input_dir, 1, nchar(input_dir) - 1)
            }
            res <- file.path(input_dir, x)
          }
          return(res)
        }, FUN.VALUE = character(1))
        names(name_sd) <- NULL
      } else {
        name_sd <- study_data
      }
    }
  } else {
    name_sd <- character(0)
  }


  util_verify_names(name_of_study_data = name_sd)
  rm(name_sd)



  ##back-compatibility for column names in item_level_metadata
  my_args <- list(...)
  if ("cause_label_df" %in% names(my_args)) {
    cause_label_df <- my_args$cause_label_df
  } else {
    cause_label_df <- rlang::missing_arg()
  }

  # check if meta_data (item_level) contains the column VAR_NAMES
  suppressWarnings(util_ensure_in(
    VAR_NAMES,
    names(meta_data),
    error = TRUE,
    err_msg =
      sprintf("Did not find the mandatory column %%s in the %s.",
        sQuote("meta_data"))
  ))

  # define label_col as "LABEL" if they are not specified by users
  # if LABEL is missing then replace with VAR_NAMES
  if (missing(label_col) && (LABEL %in% names(meta_data))) {
    label_col <- LABEL
  } else if (missing(label_col) && !(LABEL %in% names(meta_data))) {
    label_col <- VAR_NAMES
  }

  # fix to rename columns from old metadata to new
  # (e.g., KEY_STUDY_SEGMENT > STUDY_SEGMENT)
  # even if cause_label_df is missing
  if (missing(cause_label_df)) {
    try(meta_data <- do.call(
      prep_meta_data_v1_to_item_level_meta_data,  #TODO: check very slow
      list(
        meta_data = meta_data,
        label_col = label_col,
        verbose = FALSE
      )
    ),
    silent = TRUE)
  } else {
    try(meta_data <- do.call(
      prep_meta_data_v1_to_item_level_meta_data,
      list(
        meta_data = meta_data,
        label_col = label_col,
        cause_label_df = cause_label_df,
        verbose = FALSE
      )
    ),
    silent = TRUE)
  }

  label_col_provided <- label_col
  mod_label <- util_ensure_label(meta_data = meta_data,  #here it creates VAR_NAMES_1
                                 label_col = label_col)
  if (!is.null(mod_label$label_modification_text)) {
    # There were changes in the metadata.
    meta_data <- mod_label$meta_data
    label_col <- mod_label$label_col
  }

  attr(meta_data, "normalized") <- TRUE
  attr(meta_data, "version") <- 2

  rm(my_args)

  # check if a column VARIABLE_ROLE is present in the metadata.
  # if not create one with always "primary" as role
  if (!(VARIABLE_ROLE %in% colnames(meta_data))) {
    util_message(
      "No %s assigned in item level metadata. Defaulting to %s.",
      sQuote(VARIABLE_ROLE),
      dQuote(VARIABLE_ROLES$PRIMARY),
      applicability_problem = TRUE)
    meta_data$VARIABLE_ROLE <- VARIABLE_ROLES$PRIMARY
  }

  util_stop_if_not(
    `Internal error, sorry: meta_data should be a data frame in dq_report_by. Please report` =
      is.data.frame(meta_data))

  # if DATAFRAMES is present in the item_level metadata
  # check if it is a character vector.
  if (DATAFRAMES %in% colnames(meta_data)) {
    util_expect_data_frame(meta_data,
                           col_names = list(DATAFRAMES = is.character))
  }

  # Check if id_vars is a string vector, and is present in item_level md----
  if (!is.null(id_vars)) {
    util_expect_scalar(
      arg_name = id_vars,
      allow_more_than_one = TRUE,
      check_type = is.character
    )

    #Check if id_vars is written with pipe, in case fix it
    if (length(id_vars) == 1) {
      id_vars <-
        unname(unlist(util_parse_assignments(id_vars,
                                             split_char = SPLIT_CHAR)))
    }
    # check if the id_vars is present in the item_level md
    id_vars <- vapply(id_vars, FUN = function(x){
      if (!x %in% meta_data$VAR_NAMES) {
        var_x <- x
        x <- try(util_map_labels(x,
                             meta_data = meta_data,
                             from = label_col,
                             to = VAR_NAMES), silent = TRUE)
        if (util_is_try_error(x)) {
          util_error(c("The id_vars %s is not present",
                       " in the item_level metadata"),
                     dQuote(var_x))
        } else {
          names(x) <- NULL
        }
        rm(var_x)
        return(x)
      } else {x}
    }, FUN.VALUE = character(1) )
    names(id_vars) <- NULL

  } else {
    #if not specified as argument by the user
    id_vars <- character(0)
  }


  # Check the arguments segment_select and segment_exclude----
  # check if both segment_column and segment_select are present
  if (!is.null(segment_select) && is.null(segment_column)) {

    if(STUDY_SEGMENT %in% colnames(meta_data)) {
      segment_column <- STUDY_SEGMENT
      possible_segments <- unique(meta_data$STUDY_SEGMENT)
      if(!segment_select %in% possible_segments) {
        util_error(c("segment_select values are not present in the ",
                     "column 'STUDY_SEGMENT' that is assumed to be the ",
                     "segment_column when this is not assigned."))
      }
    } else {
      util_error(c("No segment_column provided and no STUDY_SEGMENT ",
                   "available in the metadata"))
    }
  }

  #check if both segment_column and segment_exclude are present
  if (!is.null(segment_exclude) && is.null(segment_column)) {
    if(STUDY_SEGMENT %in% colnames(meta_data)) {
      segment_column <- STUDY_SEGMENT
      possible_segments <- unique(meta_data$STUDY_SEGMENT)
      if(!segment_exclude %in% possible_segments) {
        util_error(c("segment_exclude values are not present in the ",
                     "column 'STUDY_SEGMENT' that is assumed to be the ",
                     "segment_column when this is not assigned."))
      }
    } else {
      util_error(c("No segment_column provided and no STUDY_SEGMENT",
                   " available in the metadata"))
      }
  }

  # Load cross-item_level metadata and normalize it ----
  # check if there is a cross item metadata and if it is a data frame
  # in case is not present, create an empty data frame for cross item metadata
  try(util_expect_data_frame(meta_data_cross_item), silent = TRUE)
  if (!is.data.frame(meta_data_cross_item)) {
    util_message(sprintf(
      "No cross-item level metadata %s found",
      dQuote(meta_data_cross_item)))
    meta_data_cross_item <- data.frame(VARIABLE_LIST = character(0),
                                       CHECK_LABEL = character(0))
  }

  # First normalize input for meta_data_cross_item from the user
  meta_data_cross_item <- util_normalize_cross_item(
    meta_data = meta_data,
    meta_data_cross_item = meta_data_cross_item,
    label_col = label_col
  )

  # Load segment_level metadata ----
  try(util_expect_data_frame(meta_data_segment,
                             col_names = list(STUDY_SEGMENT = is.character)),
      silent = TRUE)
  # if not present, create an empty one
  if (!is.data.frame(meta_data_segment)) {
    util_message("No segment level metadata %s found",
                 dQuote(meta_data_segment))
    if(!is.null(meta_data$STUDY_SEGMENT)) {  #Add study segments if present
      meta_data_segment <- data.frame(STUDY_SEGMENT =
                                        unique(meta_data$STUDY_SEGMENT),
                                      SEGMENT_ID_VARS = NA_character_)
    } else {
      meta_data_segment <- data.frame(STUDY_SEGMENT =
                                        character(0),
                                      SEGMENT_ID_VARS = character(0))
    }
  }

  if(!SEGMENT_ID_VARS %in% colnames(meta_data_segment)){
    meta_data_segment$SEGMENT_ID_VARS <- NA_character_
  }


  # Load data frame_level metadata ----
  # check if there is a dataframe level metadata or provide message
  try(util_expect_data_frame(meta_data_dataframe), silent = TRUE)
  if (!is.data.frame(meta_data_dataframe)) {
    util_message(sprintf(
      "No dataframe level metadata %s found",
      dQuote(meta_data_dataframe)))
    meta_data_dataframe <- data.frame(DF_NAME = character(0),
                                      DF_CODE = character(0),
                                      DF_ID_VARS = character(0))
  } else if (is.data.frame(meta_data_dataframe) &&
             (!missing(study_data) && !is.data.frame(study_data) &&
              length(study_data) > 1)) {
    # two or more study data indicated as vector by the user
    util_message(c("When multiple data frames are provided for the",
                   " argument 'study data', ",
                   "the dataframe_level_metadata information will be ignored"))
    meta_data_dataframe <- data.frame(DF_NAME = character(0),
                                      DF_CODE = character(0),
                                      DF_ID_VARS = character(0))
  } else {
    # check presence of expected columns in the dataframe_level metadata
    util_expect_data_frame(meta_data_dataframe,
                           col_names = list(DF_NAME = is.character))

    if (DF_ID_VARS %in% colnames(meta_data_dataframe)) {
      util_expect_data_frame(meta_data_dataframe,
                             col_names = list(DF_ID_VARS = is.character))
    } else {
      meta_data_dataframe$DF_ID_VARS <- NA_character_
    }

    if (DF_CODE %in% colnames(meta_data_dataframe)) {
      util_expect_data_frame(meta_data_dataframe,
                             col_names = list(DF_CODE = is.character))
    } else {
      meta_data_dataframe$DF_CODE <- NA_character_
    }

    if(nrow(meta_data_dataframe) > 0) {
      # Add the path to the dataframe name if not already present,
      # if input_dir is provided
      if (!is.null(input_dir)) {
        meta_data_dataframe$DF_NAME <-
          vapply(
            meta_data_dataframe$DF_NAME,
            FUN = function(x) {
              if (!grepl(.Platform$file.sep, x, fixed = TRUE)) {
                if (endsWith(input_dir, .Platform$file.sep)) {
                  input_dir <- substr(input_dir, 1, nchar(input_dir) - 1)
                }
                x <- file.path(input_dir, x)
                return(x)
              } else {
                return(x)
              }
            },
            FUN.VALUE = character(1)
          )}
    }
  }

  # Load "meta_data_item_computation" metadata and complete VARIABLE_LIST ----
  # check if there is a computed_items metadata and if it is a data frame
  # in case is not present, create an empty data frame for computed_items
  try(util_expect_data_frame(meta_data_item_computation),
      silent = TRUE)
  if (!is.data.frame(meta_data_item_computation)) {
    util_message(sprintf(
      "No meta_data_item_computation %s found",
      dQuote(meta_data_item_computation)))
    meta_data_item_computation <- data.frame(VAR_NAMES = character(0),
                                             COMPUTATION_RULE = character(0))
  }

  ### create VARIABLE_LIST entries from COMPUTATION_RULE entries
  needles_var_names <- unique(c(
    meta_data[[VAR_NAMES]],
    meta_data[[label_col]],
    meta_data[[LABEL]],
    meta_data[[LONG_LABEL]],
    meta_data[["ORIGINAL_VAR_NAMES"]],
    meta_data[["ORIGINAL_LABEL"]]
  ))
  needles <- paste0("[", needles_var_names, "]")
  x <- vapply(
    setNames(needles, nm = needles_var_names),
    grepl,
    setNames(nm = meta_data_item_computation[["COMPUTATION_RULE"]]),
    fixed = TRUE,
    FUN.VALUE = logical(length = nrow(meta_data_item_computation))
  )
  if (is.vector(x)) {
    x <- as.matrix(t(x))
  }
  variablelist <- unname(lapply(as.data.frame(t(x)), function(xx)
    unique(sort(colnames(
      x
    )[xx]))))
  variablelist <-
    lapply(variablelist, paste0, collapse = sprintf(" %s ", SPLIT_CHAR))

  if (!VARIABLE_LIST %in% colnames(meta_data_item_computation) &&
      nrow(meta_data_item_computation) > 0) {
    meta_data_item_computation[[VARIABLE_LIST]] <- NA_character_
  } else if (!VARIABLE_LIST %in% colnames(meta_data_item_computation) &&
             nrow(meta_data_item_computation) == 0) {
    meta_data_item_computation[[VARIABLE_LIST]] <- character(0)
  }
  vl_empty <- util_empty(meta_data_item_computation[[VARIABLE_LIST]])
  meta_data_item_computation[[VARIABLE_LIST]][vl_empty] <-
    variablelist[vl_empty]

  # define vars_in_subgroup----
  if(!is.null(subgroup)){
    needles_var_names <- unique(c(meta_data[[VAR_NAMES]],
                                  meta_data[[label_col]],
                                  meta_data[[LABEL]],
                                  meta_data[[LONG_LABEL]],
                                  meta_data[["ORIGINAL_VAR_NAMES"]],
                                  meta_data[["ORIGINAL_LABEL"]]))
    needles <- paste0("[", needles_var_names, "]")
    x <- vapply(setNames(needles, nm = needles_var_names),
                grepl,
                setNames(nm = subgroup),
                fixed = TRUE,
                FUN.VALUE = logical(length = length(subgroup)))
    if (is.vector(x)) {
      x <- as.matrix(t(x))
    }
    vars_in_subgroup <- unlist(unname(lapply(as.data.frame(t(x)), function(xx)
      unique(sort(colnames(x)[xx])))))
    rm(needles_var_names, needles, x)
  } else {
    vars_in_subgroup <- character(0)
  }

  vars_in_subgroup <- #if not in VAR_NAMES reduce it to VAR_NAMES
    vapply(vars_in_subgroup, FUN = function(x) {
      if(!x %in% meta_data$VAR_NAMES) {
        x <- util_map_labels(
          x,
          meta_data = meta_data,
          from = label_col,
          to = VAR_NAMES)
        names(x) <- NULL
        return(x)
      } else {
        x
      }
    }, FUN.VALUE = character(1))

  # Define needed objects for later use----
  # initialize split_segment to FALSE
  # it can be: FALSE if there is no splitting or TRUE otherwise
  split_segments <- FALSE


  # Prepare/filter item_level metadata when argument resp_vars is provided ----
  if(!missing(resp_vars)) {
    #if resp_vars is not VAR_NAMES but LABELS, first turn them to VAR_NAMES
    resp_vars <- vapply(resp_vars, FUN = function(x) {
      if(!x %in% meta_data$VAR_NAMES) {
        x <- util_map_labels(
          x,
          meta_data = meta_data,
          from = label_col,
          to = VAR_NAMES)
        names(x) <- NULL
        return(x)
      } else {
        x
      }
    }, FUN.VALUE = character(1))

    # 1st: extract all rules from cross-item containing a var from resp_vars
    #combine var_names with labels
    vars <- util_map_labels(resp_vars,
                            meta_data = meta_data,
                            to = label_col,
                            from = VAR_NAMES)
    # create a list with the vars from the column
    # variable_list in cross-item metadata
    rules_vars <-
      util_parse_assignments(meta_data_cross_item$VARIABLE_LIST,
                             multi_variate_text =
                               TRUE)

    # intersect vars in the rules with vars
    # in the segment to see which rules affect the
    # current segment and discard rules not affected by the resp_vars
    rules_to_use_cil <-
      vapply(lapply(rules_vars, intersect, vars),
             length,
             FUN.VALUE = integer(1)) > 0
    cil_in_resp_vars <- meta_data_cross_item[rules_to_use_cil, , FALSE]

    rm(rules_to_use_cil, rules_vars)


    # 2nd: extract all rules containing a resp_vars from item_computation_level
    # extracting the variable names from the column
    # VAR_NAMES in meta_data_item_computation
    comp_vars <- util_map_labels(
      meta_data_item_computation$VAR_NAMES,
      meta_data = meta_data,
      to = label_col,
      from = VAR_NAMES)
    # intersect vars in the rules with resp_vars
    computed_to_use <-
      vapply(lapply(comp_vars, intersect, vars),
             length,
             FUN.VALUE = integer(1)) > 0

    # discard rules not affected by resp_vars
    computed_in_resp_vars <- meta_data_item_computation[computed_to_use, ,
                                                        FALSE]

    rm(computed_to_use, comp_vars, vars)

    # Obtain all the original plus referred variables and
    # the filtered item_level metadata.
    # All the referred variables added has a variable role = 'suppress'
    overview_referred <- util_referred_vars(resp_vars = resp_vars,
                                            id_vars = id_vars,
                                            vars_in_subgroup = vars_in_subgroup,
                                            meta_data = meta_data,
                                            meta_data_segment =
                                              meta_data_segment,
                                            meta_data_dataframe =
                                              meta_data_dataframe,
                                            meta_data_cross_item =
                                              cil_in_resp_vars,
                                            meta_data_item_computation =
                                              computed_in_resp_vars,
                                            strata_column = strata_column)
    resp_vars_complete <- overview_referred$vars_complete
    meta_data <- overview_referred$md_complete
    attr(meta_data, "normalized") <- TRUE
    attr(meta_data, "version") <- 2

    rm(overview_referred)

    #Modify also meta_data_item_computation to remove non necessary rows
    meta_data_item_computation <-
      meta_data_item_computation[meta_data_item_computation$VAR_NAMES %in%
                                   resp_vars_complete, , drop = FALSE]
  }


  # Define the split ----
  # if nothing specified, it tries to separate data by STUDY SEGMENT
  # (unless there is a subgroup, or strata_column, or
  # specifically stated by the user "segment_column = NULL"
  # if there is "segment_column = NULL" it is nowhere in the if statement,
  # and it does not create any split
  if ((missing(segment_column) && is.null(segment_column)) &&
      is.null(strata_column) && is.null(subgroup)) {
    if (STUDY_SEGMENT %in% colnames(meta_data)) {
      segment_column <- STUDY_SEGMENT
    } else {
      util_error(
        paste0(
          "No information for split provided. ",
          "Missing both strata_column, segment_column, and subgroup.",
          " To have an unsplit report please set the ",
          "argument 'segment_column = NULL'"
        ),
        applicability_problem = TRUE
      )
    }
  } else if (is.null(segment_column) &&
             !is.null(strata_column)) {
    # if strata_column specified, check if the variable is in VAR_NAMES and
    # set a new object with the label if possible
    if (label_col_provided != VAR_NAMES && !is.null(strata_column)) {
      strata_column1 <- strata_column
      strata_column <- try(util_map_labels(strata_column,
                                           meta_data, VAR_NAMES,
                                           label_col),
                           silent = TRUE)
      # if strata_column is a VAR_NAME then there is an error instead of a
      # vector in strata_column, so replace with the original value
      if (!is.vector(strata_column)) {
        strata_column <- strata_column1
      }
      rm(strata_column1)
      if (!strata_column %in% meta_data[[VAR_NAMES]]) {
        util_error(
          paste0(
            "The strata_column provided does not correpond ",
            "to any variable in the item_level_metadata"
          ),
          applicability_problem = TRUE
        )
      }
      strata_column_label <- util_map_labels(strata_column,
                                             meta_data,
                                             label_col,
                                             VAR_NAMES)
    } else if (label_col_provided == VAR_NAMES && !is.null(strata_column)) {
      if (!strata_column %in% meta_data[VAR_NAMES]) {
        util_error(
          paste0(
            "The strata_column provided does not correpond ",
            "to any variable in the item_level_metadata"
          ),
          applicability_problem = TRUE
        )
      }
      strata_column_label <- util_map_labels(strata_column,
                                             meta_data, label_col,
                                             VAR_NAMES)
    }
    # if only segment_column is provided, check if the value corresponds to
    # a column in the item_level_metadata
  } else if (!is.null(segment_column) &&
             is.null(strata_column)) {
    if (!(segment_column %in% colnames(meta_data))) {
      util_error(
        "No metadata attribute %s found for segmenting DQ report.",
        dQuote(segment_column))
    }
    # if both segment_column and strata_column are provided
    # check if the segment_column provided corresponds to a column
    # in the item_level_metadata and
    # check if the variable of strata_column is in VAR_NAMES and
    # set a new object with the label if possible
  } else if (!is.null(segment_column) &&
             !is.null(strata_column)) {
    if (!(segment_column %in% colnames(meta_data))) {
      util_error(
        "No metadata attribute %s found for segmenting DQ report.",
        dQuote(segment_column))
    }
    if (label_col_provided != VAR_NAMES && !is.null(strata_column)) {
      strata_column1 <- strata_column
      strata_column <- try(util_map_labels(strata_column,
                                           meta_data, VAR_NAMES,
                                           label_col),
                           silent = TRUE)
      # if strata_column is a VAR_NAME then there is an error instead of a
      # vector in strata_column, so replace with the original value

      if (!is.vector(strata_column)) {
        strata_column <- strata_column1
      }
      rm(strata_column1)
      if (!strata_column %in% meta_data[, VAR_NAMES, drop = TRUE]) {
        util_error(
          paste0(
            "The strata_column provided does not correpond ",
            "to any variable in the item_level_metadata"
          ),
          applicability_problem = TRUE
        )
      }
      strata_column_label <- util_map_labels(strata_column,
                                             meta_data, label_col,
                                             VAR_NAMES)
    } else if (label_col_provided == VAR_NAMES && !is.null(strata_column)) {
      if (!strata_column %in% meta_data[, VAR_NAMES, drop = TRUE]) {
        util_error(
          paste0(
            "The strata_column provided does not correpond ",
            "to any variable in the item_level_metadata"
          ),
          applicability_problem = TRUE
        )
      }
      strata_column_label <- util_map_labels(strata_column,
                                             meta_data, label_col,
                                             VAR_NAMES)
    }
  }

  # Define the study data----
  #if the study_data argument is specified by the user
  if (!missing(study_data)) {
    ### case 1: only one study data indicated----
    #the data frame is an object in the environment
    if (is.data.frame(study_data)) {
      # if it is a data frame, look up the call of the function
      # to get the name of the study data from the call
      name_of_study_data <-
        head(as.character(substitute(study_data)), 1)
      # add the study data to the cache
      prep_add_data_frames(data_frame_list = setNames(list(study_data),
                                                      nm = name_of_study_data))
      # create also here for compatibility a list_sd_columns,
      # a list containing one element in this case,
      # containing the names of the study data variables
      dataframe_names <- name_of_study_data
      list_sd_columns <- colnames(study_data)
      list_sd_columns <- list(list_sd_columns)
      names(list_sd_columns) <- dataframe_names
    } else if (length(study_data) == 1 &&
               is.character(study_data)) {
      # if the user indicate a name of a data frame,
      # use that name (not loaded yet)

      # add the path before the name of the study data
      if (!is.null(input_dir)) {
        if (!grepl(.Platform$file.sep, study_data, fixed = TRUE)) {
          if (endsWith(input_dir, .Platform$file.sep)) {
            input_dir <- substr(input_dir, 1, nchar(input_dir) - 1)
          }
          study_data <- file.path(input_dir, study_data)
        }
      }
      name_of_study_data <- study_data

      # create also here for compatibility a list_sd_columns,
      # a list containing one element in this case,
      # containing the names of the study data variables
      dataframe_names <- name_of_study_data
      list_sd_columns <- colnames(prep_get_data_frame(dataframe_names))
      list_sd_columns <- list(list_sd_columns)
      names(list_sd_columns) <- dataframe_names
    } else if (!is.data.frame(study_data) && length(study_data) > 1) {
      ## case 2: two or more study data indicated as vector by the user----
      # if the study names are a vector of names without path and they are
      # not URL, add the input_dir before the names of each study data

      # create a list containing all names of the study data
      if (!is.null(input_dir)) {
        dataframe_names <- vapply(study_data, function(x) {
          res <- x
          if (!grepl(.Platform$file.sep, x, fixed = TRUE)) {
            if (endsWith(input_dir, .Platform$file.sep)) {
              input_dir <- substr(input_dir, 1, nchar(input_dir) - 1)
            }
            res <- file.path(input_dir, x)
          }
          return(res)
        }, FUN.VALUE = character(1))
        names(dataframe_names) <- NULL
      } else {
        dataframe_names<- study_data
      }



      # import the name of the variables in the study data
      list_sd_columns <- lapply(setNames(nm = dataframe_names), function (nm) {
        columns_df <-
          try(prep_get_data_frame(nm,
                                  column_names_only = TRUE,
                                  keep_types = TRUE),
              silent = TRUE)
        if (util_is_try_error(columns_df)) {
          columns_df <- NULL
        }
        columns_df
      })
      #import works also with URLs, in combination with n_max and nrows = 0
      list_sd_columns <- lapply(list_sd_columns, colnames)
      names(list_sd_columns) <- dataframe_names
    } else {
      util_error(c("Internal error, sorry. Please report. The provided ",
                   "study_data argument is not supported"))
    }
  } else if (missing(study_data) &&
             is.data.frame(meta_data_dataframe)) {
    ### case 3: no study data name provided by the user, use meta_data_dataframe----
    # import the study_data names from meta_data_dataframe

     #Following 4 rowsNot needed, It is not possible to arrive here without a valid DF_NAME
     # check if the column DF_NAME is not empty
#    if (all(is.na(meta_data_dataframe$DF_NAME))) {
#      util_error("Column %s in dataframe_level metadata can not be empty.",
#                 sQuote("DF_NAME"))
#    }

    # vector of the names present in the dataframe_level metadata
    dataframe_names <- meta_data_dataframe[, DF_NAME, drop = TRUE]

    #if there are information about the DATAFRAMES and DF_CODE
    if ((DF_CODE) %in% colnames(meta_data_dataframe) &&
        DATAFRAMES %in% colnames(meta_data)) {
      # Create a vector with the name of the study_data as before with
      # the actual path but also with the relative DF_CODE
      study_data_withcode <- meta_data_dataframe[, c(DF_NAME, DF_CODE),
                                                 drop = FALSE]
      list_sd_columns <- NULL

    } else {
      #if there are NO information about the DATAFRAMES and DF_CODE
      # import only headers of study data files
      list_sd_columns <- lapply(setNames(nm = dataframe_names), function (nm) {
        columns_df <-
          try(prep_get_data_frame(nm,
                                  column_names_only = TRUE,
                                  keep_types = TRUE),
              silent = TRUE)
        if (util_is_try_error(columns_df)) {
          columns_df <- NULL
        }
        columns_df
      })
      #import works also with URLs in combination with n_max = 0 and nrows = 0
      list_sd_columns <- lapply(list_sd_columns, colnames)
      names(list_sd_columns) <- dataframe_names
    }
    # stop if no study data provided and no dataframe_level metadata available
    # or dataframe_level metadata is empty
  } else if (missing(study_data) &&
             (!is.data.frame(meta_data_dataframe) ||
              nrow(meta_data_dataframe) == 0 )) {
    util_error(
      c("Not possible to create reports as no study data and no",
        "dataframe level metadata %s with study",
        "data names are available"),
      dQuote(meta_data_dataframe))
  }

  # Define name_of_study_data to NULL if it does not exist yet----
  if (!exists("name_of_study_data")) {
    name_of_study_data <- NULL
  }



  # Prepare the metadata depending on the presence of a segment_column or not----
  ### 1st case: the segment_column is present----
  if (!is.null(segment_column)) {
    # if there are empty entries in the column defined for the split, set
    # a new non-used segment name (e.g., na1) and use it for empty rows
    split_segments <- TRUE
    .md <- meta_data[[segment_column]]
    i <- ""
    while (any(.md == paste0("na", i), na.rm = TRUE)) {
      if (i == "") {
        i <- 0
      }
      i <- i + 1
    }
    .md[util_empty(.md)] <- paste0("na", i)
    meta_data[[segment_column]] <- .md
    # define an object containing the segments to create
    segments <- unique(meta_data[[segment_column]])

    # backwards compatibility-replace var_names with label in segments to create
    if (label_col_provided != VAR_NAMES &&
        all(segments %in% meta_data[[VAR_NAMES]])) {
      segmentNames <- util_map_labels(segments, meta_data, label_col)
    } else {
      segmentNames <- segments
    }
    rm(segments)

    # Add selection of 1 or multiple levels of segment_column or select using a
    # regex
    if (!is.null(segment_select)) {
      #Fix segment_select in case is written as "A | B" instead of c("A", "B")
      if (length(segment_select) == 1) {
        segment_select <-  unname(unlist(util_parse_assignments(segment_select,
                                                                split_char = SPLIT_CHAR)))
      }
      # check if the typed levels exist in the segment_column possible levels
      # before to select it
      if (!any(segment_select %in% segmentNames)) {
        #if there is more than one element it means it is not a pattern, but
        # a list of names
        if (length(segment_select) > 1) {
          # stop if selection does not match any level
          util_error(
            "No segment_column level matches the provided names: %s",
            dQuote(segment_select) )
        } else {
          # if the argument is not present in the level names, check if it is
          # a pattern, and if it also does not match any level then provide a
          # warning and stop
          all_segmentNames <- segmentNames
          segmentNames <- segmentNames[grepl(segment_select, segmentNames)]
          if (length(segmentNames) == 0) {
            # stop if selection (name or pattern) does
            # not match any level
            util_error(
              c("No segment_column level matches the provided name or",
                "pattern: %s"),
              dQuote(segment_select))
            segmentNames <- all_segmentNames
          }
          rm(all_segmentNames)
        }
      } else {
        segmentNames <- segmentNames[segmentNames %in% segment_select]
      }
    }

    ### remove segments to exclude
    if (!is.null(segment_exclude)) {
      #Fix segment_exclude in case is written as "A | B" instead of c("A", "B")
      if (length(segment_exclude) == 1) {
        segment_exclude <-
          unname(unlist(util_parse_assignments(segment_exclude,
                                               split_char = SPLIT_CHAR)))
      }

      # check if the typed levels exist in the segment_column possible levels
      # before to select it
      if (!any(segment_exclude %in% segmentNames)) {
        #if there is more than one element it means it is not a pattern, but
        # a list of names
        if (length(segment_exclude) > 1) {
          # stop if selection does not match any level
          util_error(
            "No segment_column level matches the provided names to exclude: %s",
            dQuote(segment_exclude))
        } else {
          # if the argument is not present in the level names, check if it is
          # a pattern, and if it also does not match any level then provide a
          # warning and stop
          all_segmentNames <- segmentNames
          unwanted_seg <- segmentNames[grepl(segment_exclude, segmentNames)]
          segmentNames <- setdiff(segmentNames, unwanted_seg)
          if (length(segmentNames) == 0) {
            # stop if selection (name or pattern) has no segment left
            util_error(
              c("No segment_column level left after removing ",
                "unwanted segments: %s"),
              dQuote(segment_exclude))
          }
          rm(all_segmentNames)
        }
      } else {
        #exclude unwanted segments
        segmentNames <- segmentNames[!(segmentNames %in% segment_exclude)]
        if (length(segmentNames) == 0) {
          # stop if selection has no segment left
          util_error(
            c("No segment_column level left after removing ",
              "unwanted segments: %s"),
            dQuote(segment_exclude))
        }
      }
    }


    # Filter cross-item level for the rules that can contain variables
    # in the segment (cross-item_level already normalized)
    # extract all rules containing a variable from the current evaluated segment
    cil_in_segment <- lapply(setNames(segmentNames, nm = segmentNames),
                             function(segment) {
                               vars <-
                                 meta_data[meta_data[[segment_column]] ==
                                             segment, VAR_NAMES]
                               #replace var_names with labels
                               vars <- util_map_labels(vars,
                                                       meta_data = meta_data,
                                                       to = label_col,
                                                       from = VAR_NAMES)
                               # extracting the variable names from the column
                               # variable_list in cross-item metadata to have
                               # a vector of variable names for each rule in the
                               # list rules_vars
                               rules_vars <-
                                 util_parse_assignments(
                                   meta_data_cross_item$VARIABLE_LIST,
                                   multi_variate_text = TRUE)
                               # intersect vars in the rules with vars
                               # in the segment to discard rules not
                               # affecting the current segment
                               rules_to_use <-
                                 vapply(lapply(rules_vars, intersect, vars),
                                        length,
                                        FUN.VALUE = integer(1)) > 0
                               meta_data_cross_item[rules_to_use, , FALSE]
                             })

    # Filter computed items to extract all rules to compute
    # containing a VAR_NAMES from the current evaluated segment
    computed_in_segment <-
      lapply(setNames(segmentNames, nm = segmentNames),
             function(segment) {
               vars <-
                 meta_data[meta_data[[segment_column]] ==
                             segment, VAR_NAMES]
               #replace var_names with labels
               vars <- util_map_labels(vars,
                                       meta_data = meta_data,
                                       to = label_col,
                                       from = VAR_NAMES)
               # extracting the variable names from the column
               # VAR_NAMES in meta_data_item_computation
               comp_vars <- util_map_labels(
                 meta_data_item_computation$VAR_NAMES,
                 meta_data = meta_data,
                 to = label_col,
                 from = VAR_NAMES
               )
               # intersect vars in the rules with vars
               # in the segment to see which VAR_NAMES affect the
               # current segment
               computed_to_use <-
                 vapply(lapply(comp_vars, intersect, vars),
                        length,
                        FUN.VALUE = integer(1)) > 0
               # discard rules not affected by the
               # current segment
               meta_data_item_computation[computed_to_use, , FALSE]
             })

    # Select only the data frames interested by the current segment
    ### if there is a dataframe level metadata----
    if (is.data.frame(meta_data_dataframe) &&
        nrow(meta_data_dataframe) > 0) { #this is introduced in the case of
      # study_data argument filled and so the dataframe level will be ignored

      # check if the column DF_NAME is not empty
      if (all(is.na(meta_data_dataframe$DF_NAME))) {
        util_error("Column %s in dataframe_level metadata can not be empty.",
                   sQuote("DF_NAME"))
      }

      # extract the data frame per segment
      #### caseA: there are DF_CODE and DATAFRAMES: select the dataframes ----
      # based on the DF_CODE of the variables in the segment
      if ((DF_CODE) %in% colnames(meta_data_dataframe) &&
          DATAFRAMES %in% colnames(meta_data)) {
        dfr_in_segment <-
          lapply(setNames(segmentNames, nm = segmentNames), function(segment) {
            vars <-
              meta_data[meta_data[[segment_column]] ==
                          segment, c(VAR_NAMES, DATAFRAMES), drop = FALSE]

            dfr_CODE_from_item_level <- unique(unname(unlist(
              util_parse_assignments(
                vars$DATAFRAMES,
                split_char = SPLIT_CHAR,
                multi_variate_text = TRUE
              )
            )))

            dfr_to_use <-
              meta_data_dataframe[meta_data_dataframe[[DF_CODE]] %in%
                                    dfr_CODE_from_item_level, , FALSE]
          })
      } else {
        #### caseB: there are no DF_CODE and DATAFRAMES ----
        dfr_in_segment <-
          lapply(setNames(segmentNames, nm = segmentNames), function(segment) {
            vars <-
              meta_data[meta_data[[segment_column]] ==
                          segment, VAR_NAMES]
            dfr_to_use <-
              vapply(lapply(list_sd_columns, intersect, vars),
                     length,
                     FUN.VALUE = integer(1)) > 0
            # convert to data frame with row.names = file names
            # and column with TRUE or FALSE
            dfr_to_use <- do.call(rbind, lapply(dfr_to_use, as.data.frame))
            colnames(dfr_to_use) <- "intersection"
            dfr_to_use <-
              dfr_to_use[dfr_to_use$intersection == TRUE, , drop = FALSE]
            dfr_to_use <- row.names(dfr_to_use)
            #discard data frames not containing the vars of this segment)
            dfr_to_use <-
              dataframe_names[dataframe_names %in% dfr_to_use]

            meta_data_dataframe[meta_data_dataframe[[DF_NAME]] %in%
                                  dfr_to_use, , FALSE]
          })
      }

    } else if (!is.data.frame(meta_data_dataframe) ||
               (is.data.frame(meta_data_dataframe) &&
                nrow(meta_data_dataframe) == 0)) {
      ### in case there is no dataframe metadata or it is empty----
      if (length(study_data) == 1 || is.data.frame(study_data)) {
        meta_data_dataframe <- data.frame(DF_NAME =
                                            name_of_study_data)
        dfr_in_segment <-
          lapply(setNames(segmentNames, nm = segmentNames), function(segment) {
            data.frame(DF_NAME = name_of_study_data,
                       DF_CODE = NA_character_,
                       DF_ID_VARS = NA_character_)
          })
      } else if (!is.data.frame(study_data) &&
                 length(study_data) > 1) {
        meta_data_dataframe <- data.frame(DF_NAME = dataframe_names,
                                          DF_CODE = NA_character_,
                                          DF_ID_VARS = NA_character_)

        dfr_in_segment <-
          lapply(setNames(segmentNames, nm = segmentNames), function(segment) {
            vars <-
              meta_data[meta_data[[segment_column]] ==
                          segment, VAR_NAMES]
            dfr_to_use <-
              vapply(lapply(list_sd_columns, intersect, vars),
                     length,
                     FUN.VALUE = integer(1)) > 0
            #convert to data frame with row.names = file names and
            # column with TRUE or FALSE
            dfr_to_use <- do.call(rbind, lapply(dfr_to_use, as.data.frame))
            colnames(dfr_to_use) <- "intersection"
            dfr_to_use <-
              dfr_to_use[dfr_to_use$intersection == TRUE, , drop = FALSE]
            dfr_to_use <- row.names(dfr_to_use)
            #discard data frames not containing the vars of this segment
            dfr_to_use <-
              dataframe_names[dataframe_names %in% dfr_to_use]
            meta_data_dataframe <-
              meta_data_dataframe[meta_data_dataframe$DF_NAME %in% dfr_to_use,
                                  , drop = FALSE]
          })

      }
    }

    # Prepare segment level meta_data, selecting the row matching
    # the current segment
    # if the segment_column is study_segment, separate it by segment
    if (segment_column == STUDY_SEGMENT) {
      seg_in_segment <-
        lapply(setNames(segmentNames, nm = segmentNames), function(segment) {
          meta_data_segment[meta_data_segment[[STUDY_SEGMENT]] ==
                              segment, , FALSE]
        })
    } else {
      # in case the segment_column is not the segment, need to first
      # create a list with the corresponding segments for each segment_column
      if (!STUDY_SEGMENT %in% colnames(meta_data)) {
        #In case there is no STUDY_SEGMENT
        meta_data$STUDY_SEGMENT <- "all"
      }
      segments_list <- meta_data[, c(segment_column, STUDY_SEGMENT), #TODO: need back compatibility with KEY_STUDY_SEGMENT using prep_meta_data_v1_to_item_level_meta_data?
                                 drop = FALSE]
      segments_list <- unique(segments_list)
      segments_list <-
        lapply(setNames(segmentNames, nm = segmentNames), function(piece) {
          segments_list[segments_list[segment_column] ==
                          piece, STUDY_SEGMENT, TRUE]
        })
      seg_in_segment <-
        lapply(setNames(segmentNames, nm = segmentNames), function(x) {
          meta_data_segment[meta_data_segment[[STUDY_SEGMENT]] %in%
                              c(segments_list[[x]]), , FALSE]
        })
      # in case of segment_select not present in the data, there will be
      # elements $<NA> in the list. Remove them
      seg_in_segment <- seg_in_segment[!is.na(names(seg_in_segment))]
    }


    #For each segment define which variables to include (using a repeat-loop)
    if(!missing(resp_vars)) {
      # if there is the argument resp_vars
      vars_in_segment <-
        lapply(setNames(segmentNames, nm = segmentNames),
               function(segment) {

                 # First select the variables that are indicated
                 # as part of the segment in the item-level metadata
                 # (in this case the metadata was filtered and contains
                 # original and referred variables)
                 vars <-
                   meta_data[meta_data[[segment_column]] ==
                               segment, VAR_NAMES]
                 #include variables that are in resp_vars and in this segment
                 vars<- vars[vars %in% resp_vars]
                 overview_vars_md <-
                   util_referred_vars(resp_vars = vars,
                                      id_vars = id_vars,
                                      vars_in_subgroup = vars_in_subgroup,
                                      meta_data = meta_data,
                                      meta_data_segment =
                                        seg_in_segment[[segment]],
                                      meta_data_dataframe =
                                        dfr_in_segment[[segment]],
                                      meta_data_cross_item =
                                        cil_in_segment[[segment]],
                                      meta_data_item_computation =
                                        computed_in_segment[[segment]],
                                      strata_column = strata_column)

                 vars <- overview_vars_md$vars_complete
               })

      # Subset the item_level metadata per each segment
      md_in_segment <-
        lapply(setNames(segmentNames, nm = segmentNames),
               function(segment) {
                 # First select the variables that are indicated
                 # as part of the segment in the item-level metadata
                 # (in this case the metadata was filtered and contains
                 # original and referred variables
                 vars <-
                   meta_data[meta_data[[segment_column]] ==
                               segment, VAR_NAMES]
                 #include variables that are in resp_vars and in this segment
                 vars<- vars[vars %in% resp_vars]
                 overview_vars_md <-
                   util_referred_vars(resp_vars = vars,
                                      id_vars = id_vars,
                                      vars_in_subgroup = vars_in_subgroup,
                                      meta_data = meta_data,
                                      meta_data_segment =
                                        seg_in_segment[[segment]],
                                      meta_data_dataframe =
                                        dfr_in_segment[[segment]],
                                      meta_data_cross_item =
                                        cil_in_segment[[segment]],
                                      meta_data_item_computation =
                                        computed_in_segment[[segment]],
                                      strata_column = strata_column)

                 md_seg <- overview_vars_md$md_complete
                 attr(md_seg, "normalized") <- TRUE
                 attr(md_seg, "version") <- 2
                 return(md_seg)
               })





      # create resp_vars_in_segment
      resp_vars_in_segment <- lapply(setNames(segmentNames, nm = segmentNames),
                                     function(segment) {
                                       vars_in_segment <-
                                         vars_in_segment[[segment]][
                                           vars_in_segment[[segment]] %in%
                                                                      resp_vars]
                                     })
    } else {
      # if there is no argument resp_vars
      vars_in_segment <-
        lapply(setNames(segmentNames, nm = segmentNames),
               function(segment) {
                 # First select the variables that are indicated
                 # as part of the segment in the item-level metadata
                 vars <-
                   meta_data[meta_data[[segment_column]] ==
                               segment, VAR_NAMES]
                 overview_vars_md <-
                   util_referred_vars(resp_vars = vars,
                                      id_vars = id_vars,
                                      vars_in_subgroup = vars_in_subgroup,
                                      meta_data = meta_data,
                                      meta_data_segment =
                                        seg_in_segment[[segment]],
                                      meta_data_dataframe =
                                        dfr_in_segment[[segment]],
                                      meta_data_cross_item =
                                        cil_in_segment[[segment]],
                                      meta_data_item_computation =
                                        computed_in_segment[[segment]],
                                      strata_column = strata_column)
                 vars <- overview_vars_md$vars_complete
               })

      # Define the item_level metadata for each segment
      md_in_segment <-
        lapply(setNames(segmentNames, nm = segmentNames),
               function(segment) {
                 # First select the variables that are indicated
                 # as part of the segment in the item-level metadata
                 vars <-
                   meta_data[meta_data[[segment_column]] ==
                               segment, VAR_NAMES]
                 overview_vars_md <-
                   util_referred_vars(resp_vars = vars,
                                      id_vars = id_vars,
                                      vars_in_subgroup = vars_in_subgroup,
                                      meta_data = meta_data,
                                      meta_data_segment =
                                        seg_in_segment[[segment]],
                                      meta_data_dataframe =
                                        dfr_in_segment[[segment]],
                                      meta_data_cross_item =
                                        cil_in_segment[[segment]],
                                      meta_data_item_computation =
                                        computed_in_segment[[segment]],
                                      strata_column = strata_column)

                 md_seg <- overview_vars_md$md_complete
                 attr(md_seg, "normalized") <- TRUE
                 attr(md_seg, "version") <- 2
                 return(md_seg)
               })


      #create resp_vars_in_segment = character(0)
      resp_vars_in_segment <-
        lapply(setNames(segmentNames, nm = segmentNames),
               function(segment) {
                 x <- character(0)
               })
    }



  } else {
    #### 2nd case: the segment_column is not present (NULL),----
    # include all variables and all metadata in category all_variables
    # create a list anyways but only with 1 element for all cross-item metadata
    cil_in_segment <- list(all_variables = meta_data_cross_item)

    # create a list anyways but only with 1 element for all computed vars md
    computed_in_segment <- list(all_variables = meta_data_item_computation)

    # Prepare metadata at the segment level
    try(util_expect_data_frame(meta_data_segment,
                               col_names = list(STUDY_SEGMENT = is.character)),
        silent = TRUE)
    # if not present, create an empty one
    if (!is.data.frame(meta_data_segment)) {
      util_message("No segment level metadata %s found",
                   dQuote(meta_data_segment))
      meta_data_segment <- data.frame(STUDY_SEGMENT =
                                        unique(meta_data$STUDY_SEGMENT))
    }
    seg_in_segment <- list(all_variables = meta_data_segment)

    # prepare the dataframe_level metadata, put all in a list element
    # names "all_variables" if present, if not create an empty one
    if (is.data.frame(meta_data_dataframe)) {
      dfr_in_segment <- list(all_variables = meta_data_dataframe)
    } else {
      meta_data_dataframe <- data.frame(DF_NAME = dataframe_names,
                                        DF_CODE = NA_character_,
                                        DF_ID_VARS = NA_character_)
      dfr_in_segment <- list(all_variables = meta_data_dataframe)
    }

    # prepare the list of variables in a list containing all the variables
    # in the item_level metadata
    vars_in_segment <- list(all_variables = meta_data[[VAR_NAMES]])

    # create resp_vars_in_segment <- character(0)
    if(!missing(resp_vars)) {
      resp_vars_in_segment <- list(all_variables = resp_vars)
    } else {
      resp_vars_in_segment <- list(all_variables = character(0))
    }






    #Add all the meta_data (item_level)
    md_in_segment <- list(all_variables = meta_data)

    #Addition for progressing bar
    segmentNames <- "all_variables"
  }


  # if no study data was specified by the user, set it to NULL -----
  # this fixes an issue in the following overall loop when it is missing
  if (missing(study_data)) {
    study_data <- NULL
  }




  # Calculate no. strata for the job progress bar----
  if (is.null(strata_column)) {
    n_strata <- 1
  } else {
    # Value labels from VALUE_LABEL_TABLE and CODE_LIST_TABLE
    if (is.na(meta_data[meta_data[VAR_NAMES] == strata_column,
                        VALUE_LABELS, drop = TRUE]) ||
        is.null(meta_data[meta_data[VAR_NAMES] == strata_column,
                          VALUE_LABELS, drop = TRUE])) {
      value_label_table_name <-
        meta_data[meta_data[VAR_NAMES] == strata_column,
                  VALUE_LABEL_TABLE, drop = TRUE]

      try(util_expect_data_frame(value_label_table_name),
          silent = TRUE)
      if (!is.data.frame(value_label_table_name)) {
        try(util_expect_data_frame("CODE_LIST_TABLE"), silent = TRUE)
        if (!is.data.frame(CODE_LIST_TABLE)) {
          util_message(sprintf(
            "No value_label_table_name %s found",
            dQuote(value_label_table_name)))
          value_label_table_name <- data.frame(CODE_VALUE = character(0),
                                               CODE_LABEL = character(0))
        } else {
          #select in CODE_LIST_TABLE$VALUE_LABEL_TABLE only the
          # value_label_table_name
          value_label_table_name <-
            CODE_LIST_TABLE[CODE_LIST_TABLE$VALUE_LABEL_TABLE ==
                              value_label_table_name, , drop = FALSE]
        }
      }
      expected_strata <- setNames(value_label_table_name$CODE_LABEL,
                                  nm = value_label_table_name$CODE_VALUE)
    } else {
      # if value labels are in a column in item_level metadata
      expected_strata <- unlist(
        util_parse_assignments(
          meta_data[meta_data[VAR_NAMES] == strata_column, VALUE_LABELS,
                    drop = TRUE],
          split_char = SPLIT_CHAR,
          split_on_any_split_char = TRUE,
          multi_variate_text = TRUE
        )
      )
    }



    # in case there is only a data frame as study_data
    if (length(list_sd_columns) == 1) {
      # if the dataframe already exists in the current environment assign it
      # otherwise import it
      tab1 <- util_expect_data_frame(names(list_sd_columns), dont_assign = TRUE)
      original_strata <- unique(tab1[[strata_column]])
      # check if all expected strata are present in the study data
      if (length(setdiff(names(expected_strata), original_strata)) > 0 ) {
        util_warning(c("The stratum/strata %s is/are ",
                       "not present in the study data"),
                     dQuote(names(expected_strata)[!(names(expected_strata) %in%
                                                       original_strata)]))
      }
      # check if in the study data there are strata not expected
      #removed NA from check
      if (length(setdiff(original_strata[!is.na(original_strata)],
                         names(expected_strata)) > 0 )) {
        util_warning(
          "The stratum/strata %s is/are not present in the metadata",
          dQuote(original_strata[!(original_strata %in%
                                     names(expected_strata))]))
      }
      n_strata <- length(original_strata)
      # in case the there are more dataframes as study_data
    } else if (length(list_sd_columns) > 1) {
      # obtain the names of the needed study data and select
      # only dataframes containing variables of this segment
      tab_to_import <-
        vapply(lapply(list_sd_columns, intersect, strata_column),
               length,
               FUN.VALUE = integer(1)) > 0
      tab_to_import <- names(tab_to_import[tab_to_import %in% TRUE])

      if (length(tab_to_import) == 1) {
        tab1 <- util_expect_data_frame(tab_to_import, dont_assign = TRUE)
        original_strata <- unique(tab1[[strata_column]])
      } else if (length(tab_to_import) > 1) {
        tabs <- lapply(setNames(nm = tab_to_import),
                       util_expect_data_frame,
                       dont_assign = TRUE)
        tab1 <- Reduce(function(x, y) {
          merge(x, y, all = TRUE)
        }, tabs)
        original_strata <- unique(tab1[[strata_column]])
      }
      n_strata <- length(original_strata)
    } else if (is.null(list_sd_columns)) {
      # In case in which the DF_CODE and DATAFRAME are present
      # obtain the names of the needed study data and select
      # only dataframes containing variables of strata_column
      dtf_CODE_of_strata_col <- meta_data[meta_data[[VAR_NAMES]] %in%
                                            strata_column,
                                          c(VAR_NAMES, DATAFRAMES),
                                          drop = FALSE]


      dfr_CODE_of_strata_col <- unique(unname(unlist(
        util_parse_assignments(
          dtf_CODE_of_strata_col$DATAFRAMES,
          split_char = SPLIT_CHAR,
          multi_variate_text = TRUE
        )
      )))

      tab_to_import <-
        study_data_withcode[study_data_withcode[[DF_CODE]] %in%
                              dfr_CODE_of_strata_col, DF_NAME, drop = TRUE]


      if (length(tab_to_import) == 1) {
        tab1 <- util_expect_data_frame(tab_to_import, dont_assign = TRUE)
        original_strata <- unique(tab1[[strata_column]])
      } else if (length(tab_to_import) > 1) {
        tabs <- lapply(setNames(nm = tab_to_import),
                       util_expect_data_frame,
                       dont_assign = TRUE)
        tab1 <- Reduce(function(x, y) {
          merge(x, y, all = TRUE)
        }, tabs)
        original_strata <- unique(tab1[[strata_column]])
      }
      n_strata <- length(original_strata)
    }

    # Define the strata in case strata_select is used
    if (!is.null(strata_select)) {
      #Fix strata_select in case is written as "A | B" instead of c("A", "B")
      if (length(strata_select) == 1) {
        strata_select <-
          unname(unlist(util_parse_assignments(strata_select,
                                               split_char = SPLIT_CHAR)))
      }


      # if there is a strata_select argument
      if (!is.null(selection_type)) {
        if (selection_type == "value") {
          util_stop_if_not(any(strata_select %in% original_strata),
                           label =
                             paste0("No values in the variable correspond ",
                                    "to the strata_select"))
          original_strata <-
            original_strata[original_strata %in% strata_select]
          n_strata <- length(original_strata)

        } else if (selection_type == "v_label") {
          util_stop_if_not(any(strata_select %in% expected_strata), label =
                             paste0("No value label in the variable ",
                                    "correspond to the strata_select"))
          expected_strata <-
            expected_strata[expected_strata %in% strata_select]
          original_strata <-
            original_strata[original_strata %in% names(expected_strata)]
          n_strata <- length(original_strata)
        } else if (selection_type == "regex") {
          name_pattern_strata <-
            original_strata[grepl(strata_select, original_strata)]
          name_pattern_strata2 <- expected_strata[grepl(strata_select,
                                                        expected_strata)]

          name_pattern_strata <- unique(c(name_pattern_strata,
                                          names(name_pattern_strata2)))

          util_stop_if_not(length(name_pattern_strata) > 0, label =
                             paste0("No value or value label in the variable",
                                    " correspond to the strata_select"))
          n_strata <- length(name_pattern_strata)
        }
      } else {
        #if selection_type is null, try to guess the typed strata
        if (!any(strata_select %in% original_strata)) {
          #if the strata does not match the list from data
          if (!any(strata_select %in% expected_strata)) {
            #if the strata does not match the value list labels
            # if the argument is not present in the level names, check if it is
            # a pattern
            name_pattern_strata <-
              original_strata[grepl(strata_select, original_strata)]
            name_pattern_strata <- c(name_pattern_strata,
                                     expected_strata[grepl(strata_select,
                                                           expected_strata)])
            if (length(name_pattern_strata) == 0) {
              util_error("%s does not corresponds to any strata",
                         dQuote(strata_select))
            } else {
              n_strata <- length(name_pattern_strata)
            }
          } else {
            # if the strata matches a value label
            expected_strata <-
              expected_strata[expected_strata %in% strata_select]
            original_strata <-
              original_strata[original_strata %in% names(expected_strata)]
            n_strata <- length(original_strata)

          }
        } else {
          # if the strata matches a value of the variable in the data
          original_strata <-
            original_strata[original_strata %in% strata_select]
          n_strata <- length(original_strata)
        }
      }
    }

    ## Define the strata in case strata_exclude is used
    if (!is.null(strata_exclude)) {
      if (length(strata_exclude) == 1) {
        strata_exclude <-
          unname(unlist(util_parse_assignments(strata_exclude,
                                               split_char = SPLIT_CHAR)))
      }
      # if there is a strata_exclude argument
      if (!is.null(selection_type)) {
        if (selection_type == "value") {
          util_stop_if_not(any(strata_exclude %in% original_strata), label =
                             paste0("No values in the variable correspond",
                                    " to the strata_exclude"))
          original_strata <-
            original_strata[!(original_strata %in% strata_exclude)]
          n_strata <- length(original_strata)

        } else if (selection_type == "v_label") {
          util_stop_if_not(any(strata_exclude %in% expected_strata), label =
                             paste0("No value label in the variable ",
                                    "correspond to the strata_exclude"))
          expected_strata <-
            expected_strata[!(expected_strata %in% strata_exclude)]
          original_strata <-
            original_strata[original_strata %in% names(expected_strata)]
          n_strata <- length(original_strata)
        } else if (selection_type == "regex") {
          unwanted_seg_orig <-
            original_strata[grepl(strata_exclude, original_strata)]
          unwanted_seg_label <- expected_strata[grepl(strata_exclude,
                                                      expected_strata)]
          unwanted_seg_label <- original_strata[original_strata %in%
                                                  names(unwanted_seg_label)]
          unwanted_seg <- unique(c(unwanted_seg_orig, unwanted_seg_label))
          rm(unwanted_seg_orig, unwanted_seg_label)

          name_pattern_strata	<- setdiff(original_strata, unwanted_seg)
          util_stop_if_not(length(unwanted_seg) > 0, label =
                             paste0("No value or value label in the variable",
                                    " correspond to the strata_exclude"))
          n_strata <- length(name_pattern_strata)
        }

      } else {
        #if selection_type is null, try to guess the typed strata
        if (!any(strata_exclude %in% original_strata)) {
          #if the strata does not match the list from data
          if (!any(strata_exclude %in% expected_strata)) {
            #if the strata does not match the value list labels
            # if the argument is not present in the level names, check if it is
            # a pattern
            unwanted_seg_orig <-
              original_strata[grepl(strata_exclude, original_strata)]
            unwanted_seg_label <- expected_strata[grepl(strata_exclude,
                                                        expected_strata)]
            unwanted_seg_label <- original_strata[original_strata %in%
                                                    names(unwanted_seg_label)]
            unwanted_seg <- unique(c(unwanted_seg_orig, unwanted_seg_label))
            rm(unwanted_seg_orig, unwanted_seg_label)

            name_pattern_strata	<- setdiff(original_strata, unwanted_seg)
            if (length(unwanted_seg) == 0) {
              util_error("%s does not corresponds to any strata",
                         dQuote(strata_exclude))
            } else {
              n_strata <- length(name_pattern_strata)
            }
          } else {
            # if the strata matches a value label
            expected_strata <-
              expected_strata[!(expected_strata %in% strata_exclude)]
            original_strata <-
              original_strata[original_strata %in% names(expected_strata)]
            n_strata <- length(original_strata)

          }
        } else {
          # if the strata matches a value of the variable in the data
          original_strata <-
            original_strata[!(original_strata %in% strata_exclude)]
          n_strata <- length(original_strata)
        }
      }
    }
  }

  # Create a job to report progress and give it a name ----
  util_setup_rstudio_job("dq_report_by")
  # create an extra environment for the progress
  p <- new.env(parent = emptyenv())
  p$i <- 0
  p$N <- length(segmentNames) * length(n_strata)

  # OUTER list: split base on segment----
  # Return a list of lists of results. The outer list is for the split based on
  # segment_column e.g., study segments. The inner list is for the strata
  # (split based on the strata_column)
  overall_res <- mapply(
    #here starts the outer loop by segment_column (by segment or another column)
    vars_in_segment = vars_in_segment[order(names(vars_in_segment))],
    cur_seg = sort(names(vars_in_segment)),
    meta_data = md_in_segment[order(names(md_in_segment))],
    meta_data_dataframe = dfr_in_segment[order(names(dfr_in_segment))] ,
    seg_in_segment = seg_in_segment[order(names(seg_in_segment))] ,
    MoreArgs = list(
      list_sd_columns = list_sd_columns,
      name_of_study_data = name_of_study_data,
      call_report_by = call_report_by,
      subgroup = subgroup,
      resp_vars_in_segment = resp_vars_in_segment,
      id_vars = id_vars,
      vars_in_subgroup = vars_in_subgroup
    ),
    SIMPLIFY = FALSE,
    FUN = function(vars_in_segment,
                   cur_seg,
                   meta_data,
                   meta_data_dataframe,
                   list_sd_columns,
                   seg_in_segment,
                   name_of_study_data,
                   call_report_by,
                   subgroup,
                   resp_vars_in_segment,
                   id_vars,
                   vars_in_subgroup) {

      attr(meta_data, "normalized") <- TRUE
      attr(meta_data, "version") <- 2

      # filter resp_vars_in_segment for the right segment if present
      # in case there is only a data frame as study_data
      if (length(list_sd_columns) == 1) {
        dataframe_names_cur_seg <- names(list_sd_columns)
        # if the study_data = object
        if (is.data.frame(study_data)) {
          dataframes_cur_seg <- study_data
          name_files <-
            head(as.character(substitute(study_data)), 1)
        } else {
          # import the study data files matching the name
          dataframes_cur_seg <-
            util_expect_data_frame(names(list_sd_columns), dont_assign = TRUE)
          name_files <- paste(names(list_sd_columns))
        }

        # in case there is only one study data in the data frame level
        if (is.null(name_of_study_data)) {
          name_of_study_data <- name_files
        }
        # in case the there are more data frames as study_data
      } else if (length(list_sd_columns) > 1 ||
                 is.null(list_sd_columns)) {


        #if there is CODE_DF the list_sd_columns is set to NULL
        if (is.null(list_sd_columns)) {
          # obtain the names of the needed study data and select
          # only data frames containing variables of this segment
          vars_in_seg <-
            meta_data[meta_data[[VAR_NAMES]] %in% vars_in_segment,
                      c(VAR_NAMES, DATAFRAMES), drop = FALSE]

          dfr_CODE_of_vars_in_seg <- unique(unname(unlist(
            util_parse_assignments(
              vars_in_seg$DATAFRAMES,
              split_char = SPLIT_CHAR,
              multi_variate_text = TRUE
            )
          )))

          dataframe_names_cur_seg <-
            study_data_withcode[study_data_withcode[[DF_CODE]] %in%
                                  dfr_CODE_of_vars_in_seg, DF_NAME,
                                drop = TRUE]

          rm(vars_in_seg, dfr_CODE_of_vars_in_seg)
        } else {
          # obtain the names of the needed study data and select
          # only dataframes containing variables of this segment
          dataframe_names_cur_seg <-
            vapply(lapply(list_sd_columns, intersect, vars_in_segment),
                   length,
                   FUN.VALUE = integer(1)) > 0
          dataframe_names_cur_seg <-
            dataframe_names_cur_seg[dataframe_names_cur_seg %in% TRUE]
          dataframe_names_cur_seg <-
            names(dataframe_names_cur_seg)
        }


        # import the study data files matching the names
        # in the dataframe_level metadata
        dataframes_cur_seg <-
          lapply(setNames(nm = dataframe_names_cur_seg),
                 util_expect_data_frame,
                 dont_assign = TRUE)



        # create a name for the study data merging all the
        # data frames names, after ordering and making them unique
        dataframe_names_cur_seg <- sort(unique(dataframe_names_cur_seg))
        name_of_study_data <- paste(dataframe_names_cur_seg, collapse = ", ")
      }


      #THIS IS A REPETITION BUT IT IS NEEDED BECAUSE there may be other
      # referred variables
      # Look to the data frame interested by the current segment
      dfr_in_segment <-
        lapply(setNames(dataframe_names_cur_seg, nm = dataframe_names_cur_seg),
               function(xx) {
                 meta_data_dataframe[meta_data_dataframe[[DF_NAME]] == xx, ,
                                     FALSE]
               })
      dfr_in_segment <-
        list(dfr = meta_data_dataframe[meta_data_dataframe[[DF_NAME]] %in%
                                         dataframe_names_cur_seg, , FALSE])
      names(dfr_in_segment) <- cur_seg

      # Create sd_merge and filter for var in segment if needed----
      #if there is no need to merge files
      if (length(list_sd_columns) == 1) {
        sd_merged <- dataframes_cur_seg
        #Filter for vars_in_segment
        sd_merged <- sd_merged[ , colnames(sd_merged) %in% vars_in_segment,
                                drop = FALSE]


      } else if (length(list_sd_columns) > 1 ||
                 is.null(list_sd_columns)) {
        # there is need to merge files
        #if the files are specified from user (length(study_data) > 1) or
        # there is no argument study_data
        if (is.null(list_sd_columns)) {
          #if there is no argument study_data
          # get id vars of the dataframe level metadata for merging
          # (only from dataframe and NOT from segment)
          dfr_in_segment <- dfr_in_segment[[1]]
          if (all(is.na(dfr_in_segment$DF_ID_VARS))) {
            # stop if all column is empty
            util_warning(
              paste0("Column %s in dataframe_level ",
                     "metadata is empty."),
              sQuote("DF_ID_VARS"))
          }

          id_variable <- unique(dfr_in_segment[, DF_ID_VARS, drop = TRUE])

          # Add the argument id_vars (works even if null or character(0))
          id_variable <- unique(c(id_variable, id_vars))

          util_expect_scalar(
            id_variable,
            allow_more_than_one = TRUE,
            allow_null = TRUE,     #It can be an empty id
            allow_na = TRUE)

          id_variable <-
            unlist(sapply(id_variable,
                          function(x) {
                            util_parse_assignments(
                              x,
                              split_char =
                                c(SPLIT_CHAR),
                              multi_variate_text =
                                TRUE,
                              split_on_any_split_char =
                                TRUE
                            )
                          })) #it also get rid of eventual NA in the id_variable
          names(id_variable) <- NULL
          id_variable <- unique(id_variable)


          if((DF_CODE) %in% colnames(dfr_in_segment) &&
             DATAFRAMES %in% colnames(meta_data)) {
            # In case there is a DATAFRAMES and DF_CODE, filter the data frames
            # imported in dataframe_cur_seg,
            # so that the var_names comes from the right data frame

            # First create a list with data frame names and list of variables
            # from metadata
            # corresponding to that data frame
            # (needed variables are in vars_in_segment)
            df1 <-
              meta_data[meta_data[[VAR_NAMES]] %in%
                          vars_in_segment, c(VAR_NAMES, DATAFRAMES),
                        drop = FALSE]
            dtf_with_expected_vars_from_itemlv <- lapply(
              setNames(nm = dataframe_names_cur_seg),
              FUN = function(x) {
                code_df <-
                  study_data_withcode[study_data_withcode[[DF_NAME]] %in%
                                        x, DF_CODE, drop = TRUE]
                df2 <- df1[grepl(sprintf("\\b(%s)\\b", code_df),
                                 df1$DATAFRAMES), ]
                df2[[VAR_NAMES]]
              }
            )

            # Filter each data frame imported in dataframe_cur_seg so that the
            # variables are matching the relative data frame in DATAFRAMES
            # in item_level md.
            # This way if a there are variables with the same name
            # in different data frames,they will be imported only
            # from the indicated DATAFRAMES in item_level and
            # not all of them.

            # Add always the id_vars to any data frame
            # (inside list_vars_DATAFRAMES) more specifically only import
            # id variables indicate in DF_ID_VARS of data frames and
            # the argument id_vars

            # Replace the dataframe_cur_seg with the one filtered
            dataframes_cur_seg <- lapply(
              setNames(nm = names(dtf_with_expected_vars_from_itemlv)),
              FUN = function(x) {
                # get a list of vars for each data frame based on the
                # DATAFRAMES col in item_level md
                list_vars_DATAFRAMES <-
                  dtf_with_expected_vars_from_itemlv[[x]]
                # get a list of id_vars from data
                my_id_vars <- dfr_in_segment[dfr_in_segment$DF_NAME == x,
                                             DF_ID_VARS, drop = TRUE]
                util_expect_scalar(
                  my_id_vars,
                  allow_more_than_one = FALSE,
                  allow_null = TRUE,
                  allow_na = TRUE
                )
                my_id_vars <- unlist(
                  util_parse_assignments(
                    my_id_vars,
                    split_char =
                      c(SPLIT_CHAR),
                    multi_variate_text =
                      TRUE,
                    split_on_any_split_char =
                      TRUE
                  )
                )

                names(my_id_vars) <- NULL
                my_id_vars <- unique(my_id_vars)
                # if a id_vars is provided as argument, this is added to
                # all segments
                my_id_vars <- unique(c(my_id_vars, id_vars))

                #add the id_vars to the list
                list_vars_DATAFRAMES <- c(list_vars_DATAFRAMES, my_id_vars)

                #get a list of vars for each data frame in dataframes_cur_seg
                list_vars_curseg <- names(dataframes_cur_seg[[x]])
                dataframes_cur_seg[[x]] <-
                  dataframes_cur_seg[[x]][,
                                          colnames(dataframes_cur_seg[[x]]) %in%
                                            list_vars_DATAFRAMES, drop = FALSE]
                dataframes_cur_seg[[x]]
              }
            )
          }

        } else {
          #TODO: check I think this can never happen
          #if the files are specified from user (length(list_sd_columns) > 1)
          id_variable <- id_vars
        }

        # If a segment is empty, do not create a report
        if (length(dataframes_cur_seg) == 0 ) {
          util_warning(sprintf("No data available for the segment %s",
                               sQuote(cur_seg)))
          return(NULL)
        }


        #Before merging select only variables in the current segment
        dataframes_cur_seg <-
          mapply(dataframes_cur_seg,
                 MoreArgs = list(vars_in_segment =
                                   vars_in_segment),
                 SIMPLIFY = FALSE,
                 FUN = function(x,
                                vars_in_segment){
                   x <- x[, colnames(x) %in% vars_in_segment, drop = FALSE]
                   return(x)
                   })

        #Before merging obtain the list of all variables
        vars_before <- unique(unlist(lapply(names(dataframes_cur_seg),
                                            FUN = function(x) {
                                              colnames(dataframes_cur_seg[[x]])
                                            })))


        #if there is no id_vars
        if(is.null(id_variable) || length(id_variable) == 0 ) {
          util_warning(c("Because no id variable is available, merging ",
                         "data frames could have created duplicated rows."))
          # marge data frame pairwise, without duplicating columns in common
          sd_merged <- Reduce(function(x, y) {
            merge(x, y, all = TRUE)
          }, dataframes_cur_seg)
        } else {
          # merge data frame pairwise, without duplicating columns in common
          sd_merged <- Reduce(function(x, y) {
            partial_merge <-
              suppressWarnings(merge(x, y, all = TRUE,
                                     by = intersect(
                                       intersect(colnames(x), colnames(y)),
                                       id_variable),
                                     suffixes = c("", "")
              ))
            partial_merge <-
              suppressWarnings(util_fix_merge_dups(partial_merge,
                                                   FALSE))
            return(partial_merge)
          }, dataframes_cur_seg)
        }

        #Check what happens to the variables during the merge
        if (length(vars_before) > length(colnames(sd_merged))) {
          util_warning(
            "Lost %d variables due to mapping problems. %d variables left.",
            length(vars_before) - length(colnames(sd_merged)),
            setdiff(vars_before, colnames(sd_merged)),
            applicability_problem = TRUE
          )
        } else if (length(vars_before) < length(colnames(sd_merged))) {
          util_warning(
            sprintf("There are duplicated variables due to merging: %s",
                    dQuote(setdiff(colnames(sd_merged),
                                   vars_before))),
            applicability_problem = TRUE
          )
        }
      }

      # Filter sd_merged by subgroup keeping only needed records----
      if (!is.null(subgroup)) {
        nrow_df <- nrow(sd_merged)
        # parse redcap rules to obtain contradiction in an usable way
        rule <- util_parse_redcap_rule(subgroup)

        sd_merged <- try(sd_merged[util_eval_rule(rule, ds1 = sd_merged,
                                                  meta_data = meta_data), ])
        if (inherits(sd_merged, "try-error")) {
          # if the subgroup selection did not work
          util_error("The subgroup rule %s was not acceptable.",
                     dQuote(subgroup))
        }
        if (nrow(sd_merged) == nrow_df) {
          util_warning(
            c("The number of cases did not change after applying the",
              "subgroup filter %s"),
            dQuote(subgroup))
        }
        rm(nrow_df, rule)
      }



      # Define the strata_column ----
      # if no argument to split present,
      # create a list anyways with all_observations
      if (is.null(strata_column) ||
          !strata_column %in% colnames(sd_merged)) {
        .sd_list <- list(all_observations = sd_merged)
      } else if (!is.null(strata_column) &&
                 strata_column %in% colnames(sd_merged)) {
        # split the data
        .sd_list <- split(sd_merged, sd_merged[[strata_column]])


        # provide a warning message in case there are NAs in the study data in
        # the column selected as strata_column
        if (anyNA(sd_merged[[strata_column]])) {
          # util_warning(m = sprintf(c("There are %d missing values inside the",
          #                           " column selected as strata_column"),
          #                         sum(is.na(sd_merged[[strata_column]])) )
          # )
          new_group_NA <- sd_merged[is.na(sd_merged[[strata_column]]), ]
          list_new_group_NA <- setNames(list(new_group_NA), nm = "NAs_group")
          .sd_list <- c(.sd_list, list_new_group_NA)
          rm(new_group_NA, list_new_group_NA)
        }

        ## filtering for the study_data_strata (vector of names or regexp)
        if (!is.null(strata_select)) {
          # Value labels from VALUE_LABEL_TABLE and CODE_LIST_TABLE
          if (is.na(meta_data[meta_data[VAR_NAMES] == strata_column,
                              VALUE_LABELS, drop = TRUE]) ||
              is.null(meta_data[meta_data[VAR_NAMES] == strata_column,
                                VALUE_LABELS, drop = TRUE])) {
            value_label_table_name <- meta_data[meta_data[VAR_NAMES] ==
                                                  strata_column,
                                                VALUE_LABEL_TABLE, drop = TRUE]

            try(util_expect_data_frame(value_label_table_name),
                silent = TRUE)
            if (!is.data.frame(value_label_table_name)) {
              try(util_expect_data_frame("CODE_LIST_TABLE"),
                  silent = TRUE)
              if (!is.data.frame(CODE_LIST_TABLE)) {
                util_message(sprintf(
                  "No value_label_table_name %s found",
                  dQuote(value_label_table_name)
                ))
                value_label_table_name <- data.frame(CODE_VALUE = character(0),
                                                     CODE_LABEL = character(0))
              } else {
                #select in CODE_LIST_TABLE$VALUE_LABEL_TABLE only the value_label_table_name
                value_label_table_name <-
                  CODE_LIST_TABLE[CODE_LIST_TABLE$VALUE_LABEL_TABLE ==
                                    value_label_table_name, , drop = FALSE]
              }
            }
            expected_strata <- setNames(value_label_table_name$CODE_LABEL,
                                        nm = value_label_table_name$CODE_VALUE)
          } else {
            expected_strata <- unlist(
              util_parse_assignments(
                meta_data[meta_data[VAR_NAMES] == strata_column,
                          VALUE_LABELS, drop = TRUE],
                split_char = SPLIT_CHAR,
                split_on_any_split_char = TRUE,
                multi_variate_text = TRUE
              )
            )
          }


          ## definition of selection_type and strata_select
          if (!is.null(selection_type)) {
            if (selection_type == "value") {
              # check if the typed levels exist in the segment_column possible
              # levels before to select it
              if (!any(strata_select %in% names(.sd_list))) {
                # stop if selection does not match any strata
                util_error(
                  c("No strata_column stratum matches the",
                    "provided names: %s"),
                  dQuote(strata_select))
              } else {
                .sd_list <- .sd_list[strata_select]
                # in case empty, remove empty from list
                # and give a warning
                .sd_list <- .sd_list[!is.na(names(.sd_list))]
                if (length(strata_select) > length(.sd_list)) {
                  util_warning(c(
                    "One or more strata were not",
                    "present in the study_data"
                  ))
                }
              }
            } else if (selection_type == "v_label") {
              value_of_strata_select <-
                expected_strata[expected_strata %in% strata_select]
              value_of_strata_select <- names(value_of_strata_select)
              # check if the typed levels exist in the segment_column possible
              # levels before to select it
              if (length(value_of_strata_select) == 0 &&
                  !any(value_of_strata_select %in% names(.sd_list))) {
                # stop if selection does not match any strata
                util_error(
                  c("No strata_column stratum matches the",
                    "provided names: %s"),
                  dQuote(strata_select))
              } else {
                .sd_list <- .sd_list[value_of_strata_select]
                # in case a strata is not present, remove empty from list
                # and give a warning
                .sd_list <- .sd_list[!is.na(names(.sd_list))]
                if (length(value_of_strata_select) > length(.sd_list)) {
                  util_warning(c(
                    "One or more strata were not",
                    "present in the study_data"
                  ))
                }
              }
            } else if (selection_type == "regex") {
              all_labels_in_sd_list <-
                expected_strata[names(expected_strata) %in% names(.sd_list)]
              value_of_strata_select <-
                all_labels_in_sd_list[grepl(strata_select, all_labels_in_sd_list)]
              if (length(names(.sd_list)[grepl(strata_select,
                                               names(.sd_list))]) > 0) {
                #regex match at least one strata
                .sd_list <- .sd_list[grepl(strata_select, names(.sd_list))]
                .sd_list <- .sd_list[!is.na(names(.sd_list))]
              } else if (length(names(.sd_list)[names(.sd_list) %in%
                                                names(value_of_strata_select)]) > 0) {
                .sd_list <- .sd_list[names(.sd_list)[names(.sd_list) %in%
                                                       names(value_of_strata_select)]]
                .sd_list <- .sd_list[!is.na(names(.sd_list))]
              } else {
                util_error(
                  c("No strata_column stratum matches the",
                    "provided regular expression: %s"),
                  dQuote(strata_select))
              }
            }
          } else {
            # if  selection_type is null
            # give a warning and then try to find matches
            util_message(
              c(
                "The argument selection_type is not provided, ",
                "the function will try to find any matches with value, ",
                "value labels or possible regular expressions"
              )
            )
            #check if the strata_select matches a names(.sd_list)
            if (any(strata_select %in% names(.sd_list))) {
              .sd_list <- .sd_list[strata_select]
              # in case present, remove empty from list
              .sd_list <- .sd_list[!is.na(names(.sd_list))]
            } else if (any(strata_select %in% expected_strata)) {
              #in case the strata_select matches a label of names(.sd_list)
              labels_strata_select <- expected_strata[expected_strata %in%
                                                        strata_select]
              value_of_strata_select <- names(labels_strata_select)
              .sd_list <- .sd_list[value_of_strata_select]
              # in case present, remove empty from list
              .sd_list <- .sd_list[!is.na(names(.sd_list))]
            } else if (length(names(.sd_list)[grepl(strata_select,
                                                    names(.sd_list))]) > 0) {
              #regex matches a value
              .sd_list <- .sd_list[names(.sd_list)[grepl(strata_select,
                                                         names(.sd_list))]]
            } else {
              labels_strata_select <-
                expected_strata[grepl(strata_select, expected_strata)]

              if (length(labels_strata_select) > 0) {
                value_of_strata_select <- names(labels_strata_select)
                .sd_list <- .sd_list[value_of_strata_select]
                # in case present, remove empty from list
                .sd_list <- .sd_list[!is.na(names(.sd_list))]
              } else {
                #it does not maches any labels or anything else
                util_error(
                  c("No strata_column stratum matches the",
                    "provided names: %s" ),
                  dQuote(strata_select))
              }
            }
          }
        }

        ## remove the strata to exclude
        if (!is.null(strata_exclude)) {
          # Value labels from VALUE_LABEL_TABLE and CODE_LIST_TABLE
          if (is.na(meta_data[meta_data[VAR_NAMES] == strata_column,
                              VALUE_LABELS, drop = TRUE]) ||
              is.null(meta_data[meta_data[VAR_NAMES] == strata_column,
                                VALUE_LABELS, drop = TRUE])) {
            value_label_table_name <- meta_data[meta_data[VAR_NAMES] ==
                                                  strata_column,
                                                VALUE_LABEL_TABLE, drop = TRUE]

            try(util_expect_data_frame(value_label_table_name),
                silent = TRUE)
            if (!is.data.frame(value_label_table_name)) {
              try(util_expect_data_frame("CODE_LIST_TABLE"),
                  silent = TRUE)
              if (!is.data.frame(CODE_LIST_TABLE)) {
                util_message(sprintf(
                  "No value_label_table_name %s found",
                  dQuote(value_label_table_name)))
                value_label_table_name <- data.frame(CODE_VALUE = character(0),
                                                     CODE_LABEL = character(0))
              } else {
                #select in CODE_LIST_TABLE$VALUE_LABEL_TABLE only the value_label_table_name
                value_label_table_name <-
                  CODE_LIST_TABLE[CODE_LIST_TABLE$VALUE_LABEL_TABLE ==
                                    value_label_table_name, , drop = FALSE]
              }
            }
            expected_strata <- setNames(value_label_table_name$CODE_LABEL,
                                        nm = value_label_table_name$CODE_VALUE)
          } else {
            expected_strata <- unlist(
              util_parse_assignments(
                meta_data[meta_data[VAR_NAMES] == strata_column,
                          VALUE_LABELS, drop = TRUE],
                split_char = SPLIT_CHAR,
                split_on_any_split_char = TRUE,
                multi_variate_text = TRUE
              )
            )
          }

          if (!is.null(selection_type)) {
            if (selection_type == "value") {
              # check if the typed levels exist in the segment_column possible
              # levels before to select it
              if (!any(strata_exclude %in% names(.sd_list))) {
                # stop if selection does not match any strata
                util_error(
                  c("No strata_column stratum matches the",
                    "strata to exclude: %s"),
                  dQuote(strata_exclude))
              } else {
                #remove strata_exclude elements from list
                .sd_list <- .sd_list[names(.sd_list) %in% strata_exclude == FALSE]
                # stop if removing all strata
                util_stop_if_not(length(.sd_list) > 0,
                                 label = paste0("No strata remain after",
                                                " excluding selected ones"))
              }
            } else if (selection_type == "v_label") {
              value_of_strata_select <-
                expected_strata[expected_strata %in% strata_exclude]
              value_of_strata_select <- names(value_of_strata_select)
              # check if the typed levels exist in the segment_column possible
              # levels before to select it
              if (length(value_of_strata_select) == 0 &&
                  !any(value_of_strata_select %in% names(.sd_list))) {
                # stop if selection does not match any strata
                util_error(
                  c("No strata_column stratum matches the",
                    "strata_exclude: %s"),
                  dQuote(strata_exclude))
              } else {
                .sd_list <- .sd_list[names(.sd_list) %in%
                                       value_of_strata_select == FALSE]
                #stop if no strata remain
                if (length(.sd_list) == 0) {
                  util_error(
                    c("No strata_column stratum remains",
                      "after removing strata_exclude: %s"),
                    dQuote(strata_exclude))
                }
              }
            } else if (selection_type == "regex") {
              all_labels_in_sd_list <-
                expected_strata[names(expected_strata) %in% names(.sd_list)]
              value_of_strata_select <-
                all_labels_in_sd_list[grepl(strata_exclude,
                                            all_labels_in_sd_list)]
              if (length(names(.sd_list)[grepl(strata_exclude,
                                               names(.sd_list))]) > 0) {
                #regex match at least one strata - remove the strata
                .sd_list <- .sd_list[!grepl(strata_exclude, names(.sd_list))]
                #stop if no strata remain
                if (length(.sd_list) == 0) {
                  util_error(
                    c("No strata_column stratum remains",
                      "after removing strata_exclude: %s"),
                    dQuote(strata_exclude))
                }
              } else if (length(names(.sd_list)[names(.sd_list) %in%
                                                names(value_of_strata_select)]) > 0) {
                .sd_list <- .sd_list[names(.sd_list) %in%
                                       names(value_of_strata_select) == FALSE]
              } else {
                util_error(
                  c("No strata_column stratum matches the",
                    "provided regular expression: %s"),
                  dQuote(strata_exclude))
              }
            }
          } else {
            # if  selection_type is null
            # give a warning and then try to find matches
            util_warning(
              c(
                "The argument selection_type not provided, ",
                "the function will try to find any matches with value, ",
                "value labels or possible regular expressions"
              )
            )
            #check if the strata_exclude matches a names(.sd_list)
            if (any(strata_exclude %in% names(.sd_list))) {
              #remove strata_exclude elements from list
              .sd_list <- .sd_list[names(.sd_list) %in% strata_exclude == FALSE]
              # stop if removing all strata
              util_stop_if_not(length(.sd_list) > 0,
                               label = "No strata remain after excluding selected ones")
            } else if (any(strata_exclude %in% expected_strata)) {
              #in case the strata_exclude matches a label of names(.sd_list)
              value_of_strata_select <-
                expected_strata[expected_strata %in% strata_exclude]
              value_of_strata_select <- names(value_of_strata_select)
              # check if the typed levels exist in the segment_column possible
              # levels before to select it
              if (length(value_of_strata_select) == 0 &&
                  !any(value_of_strata_select %in% names(.sd_list))) {
                # stop if selection does not match any strata
                util_error(
                  c("No strata_column stratum matches the",
                    "strata_exclude: %s"),
                  dQuote(strata_exclude))
              } else {
                .sd_list <- .sd_list[names(.sd_list) %in%
                                       value_of_strata_select == FALSE]
                #stop if no strata remain
                if (length(.sd_list) == 0) {
                  util_error(
                    c("No strata_column stratum remains",
                      "after removing strata_exclude: %s"),
                    dQuote(strata_exclude))
                }
              }
            } else {
              #in case no matches found for value or v_label try regex
              all_labels_in_sd_list <-
                expected_strata[names(expected_strata) %in% names(.sd_list)]
              value_of_strata_select <-
                all_labels_in_sd_list[grepl(strata_exclude,
                                            all_labels_in_sd_list)]
              #if the regex matches a value
              if (length(names(.sd_list)[grepl(strata_exclude,
                                               names(.sd_list))]) > 0) {
                #regex match at least one strata - remove the strata
                .sd_list <- .sd_list[!grepl(strata_exclude, names(.sd_list))]
                #stop if no strata remain
                if (length(.sd_list) == 0) {
                  util_error(
                    c("No strata_column stratum remains",
                      "after removing strata_exclude: %s"),
                    dQuote(strata_exclude))
                }
                #if the regex matches a v_label
              } else if (length(names(.sd_list)[names(.sd_list) %in%
                                                names(value_of_strata_select)]) > 0) {
                .sd_list <- .sd_list[names(.sd_list) %in%
                                       names(value_of_strata_select) == FALSE]
              } else {
                #if the regex also has no matches
                util_error(
                  c("No strata_column stratum matches the",
                    "provided strata_exclude: %s"),
                  dQuote(strata_exclude))
              }
            }
          }
        }

        # convert names of the groups
        # to VAR_NAMES_actualGroupValues
        names(.sd_list) <- paste0(strata_column_label, "_", names(.sd_list))
      }

      # INNER loop starts here: split by strata_column ----
      mapply(
        sd = .sd_list,
        sdn = names(.sd_list),
        MoreArgs = list(
          md = meta_data,
          sd_merged = sd_merged,
          name_of_study_data = name_of_study_data,
          call_report_by = call_report_by,
          resp_vars_in_segment = resp_vars_in_segment
        ),
        SIMPLIFY = FALSE,
        FUN = function(sd,
                       sdn,
                       md,
                       sd_merged,
                       name_of_study_data,
                       call_report_by,
                       resp_vars_in_segment) {

          # import a list containing original study data (too long), and
          # new assigned names, in case it was created in a previous apply,
          # otherwise create an empty one
          if (exists("..INFO_SD_NAME_FOR_REPORT", .dataframe_environment())) {
            info_sd_name <- prep_get_data_frame("..INFO_SD_NAME_FOR_REPORT")
            info_sd_name <- as.list(info_sd_name)
          } else {
            info_sd_name <- list()
          }
          # Define a new name composed of study_data_current
          # stratum_current segment
          level_name <- sdn

          # message on progresses
          util_message(sprintf(
            "Segment %s, Stratum %s..",
            sQuote(cur_seg),
            sQuote(level_name)))
          progress_msg(sprintf(
            "Segment %s, Stratum %s...",
            sQuote(cur_seg),
            sQuote(level_name)))
          # update the progress bar
          p$i <- p$i + 1
          progress(100 * p$i / p$N)

          # add the study data name that are too long to a list
          # containing original study data names and
          # new assigned names
          if (!is.null(name_of_study_data) && (
            startsWith(name_of_study_data, "https://") ||
            startsWith(name_of_study_data, "http://") ||
            startsWith(name_of_study_data, "ftp://") ||
            startsWith(name_of_study_data, "ftps://") ||
            startsWith(name_of_study_data, "dbx://") ||
            nchar(name_of_study_data) > 20
          )) {
            # if not already present in the list,
            # add the name to a general list for the overview
            if (!name_of_study_data %in%
                unlist(info_sd_name, use.names = FALSE)) {
              info_sd_name <-
                c(info_sd_name, setNames(as.list(name_of_study_data),
                                         nm = paste0("SD", length(
                                           info_sd_name
                                         ) + 1)))
            }

            # add the name to a local list for the report
            info_sd_name_vector <- setNames(names(info_sd_name), info_sd_name)
            info_sd_name_vector <-
              info_sd_name_vector[names(info_sd_name_vector) ==
                                    name_of_study_data]
            # turn it to a list to be used as arg. in dq_report2
            info_sd_name_per_report <-
              setNames(as.list(names(info_sd_name_vector)), info_sd_name_vector)
          } else {
            info_sd_name_vector <- NULL
            info_sd_name_per_report <- NULL
          }
          #use abbreviation SD# instead of study data file names in folder names
          if (!is.null(info_sd_name_vector)) {
            name_of_study_data <- info_sd_name_vector[name_of_study_data]
          }

          # Define a new name composed of study_data_current
          # stratum_current segment
          sdn <- paste0(name_of_study_data, "_", sdn, "_", cur_seg)

          subtitle_report <- paste0("Study data: ", name_of_study_data)

          if (!is.null(subgroup)) {
            subtitle_report <- paste0(subtitle_report, " Subgroup: ", subgroup)
          }


          #     }
          # update the cache with the new list info_sd_name for the overview
          if (length(info_sd_name) != 0) {
            ..INFO_SD_NAME_FOR_REPORT <- data.frame(info_sd_name)
            prep_add_data_frames("..INFO_SD_NAME_FOR_REPORT" =
                                   ..INFO_SD_NAME_FOR_REPORT)
          }
          title_report <-
            paste0("Data quality report on ", level_name, ".", cur_seg)

          # Created to later clean the cache to improve speed using function
          # prep_remove_from_cache
          old_dataframes <- prep_list_dataframes()

          # add missing tables specified as arguments
          if (!is.null(missing_tables)) {
            suppressMessages(lapply(
              missing_tables,
              FUN = function(x) {
                assign(x, get(x), envir = .dataframe_environment())
                invisible(.dataframe_environment())
              }
            ))
          }

          #in case of study_data being used as argument indicating the name of
          # the data frame, saved it here to prevent its change in the cache
          # during running the function dq_report2, and restored after
          if ("study_data" %in% names(.dataframe_environment())) {
            saved_study_data <- prep_get_data_frame("study_data")
          } else {
            saved_study_data <- NULL
          }

          if (!is.null(storr_factory)) {
            storr_factory_clone <- rlang::duplicate(storr_factory)
            environment(storr_factory_clone) <-
              rlang::env_clone(environment(storr_factory_clone))
            ns <- get("namespace", environment(storr_factory))
            ns <- paste0(ns, "/", sdn)
            assign("namespace", ns, environment(storr_factory_clone))
          } else {
            storr_factory_clone <- NULL
          }


          # If a segment is empty, do not create a report
          if (nrow(sd) == 0 || is.null(sd)) {
            util_warning(sprintf("No data available to create the report %s",
                                 sQuote(title_report)))
            return(NULL)
          }

          # create the different sub-reports ----
          r <- try(dq_report2(
            study_data = sd,
            meta_data = md,
            resp_vars = resp_vars_in_segment[[cur_seg]],
            meta_data_cross_item = cil_in_segment[[cur_seg]],
            meta_data_segment = seg_in_segment[[cur_seg]],
            meta_data_dataframe = dfr_in_segment[[1]],
            meta_data_item_computation = computed_in_segment[[cur_seg]],
            label_col = label_col,
            split_segments = split_segments,
            user_info = info_sd_name_per_report,
            title = title_report,
            subtitle = subtitle_report,
            storr_factory = storr_factory_clone,
            amend = amend,
            ...
          ))

          ###Check here if it is an error, put r <- NULL, empty report()
          attr(r, "label_modification_text") <- trimws(paste(
            attr(r, "label_modification_text"),
            mod_label$label_modification_text
          ))

          attr(r, "label_modification_table") <-
            rbind(attr(r, "label_modification_table"),
                  mod_label$label_modification_table)


          # Define subgroup for technical information
          if (!is.null(subgroup)) {
            Subgroup_info <- subgroup
          } else {
            Subgroup_info <- "Not specified"
          }

          # Add information in the "Technical information" part of the report
          attr(r, "properties") <- c(
            attr(r, "properties"),
            list(
              Study_data = name_of_study_data,
              Segment = cur_seg,
              Stratum = level_name,
              Subgroup = Subgroup_info
            )
          )

          rm(sd_merged, sd, md, Subgroup_info)
          #remove the files creating by the dq_report function from the cache
          new_dataframes <- prep_list_dataframes()
          names_to_remove <- setdiff(new_dataframes, old_dataframes)
          suppressMessages(prep_remove_from_cache(names_to_remove))
          # restore the study_data as mentioned before running the report
          suppressMessages(prep_remove_from_cache("study_data"))
          if (!is.null(saved_study_data)) {
            prep_add_data_frames(study_data = saved_study_data)
          }
          gc()

          # in case an output directory is defined, save reports and delete them ----
          if (length(output_dir) == 1) {
            if (inherits(r, "try-error")) {
              #check if instead of the report there is an error
              util_warning(
                "Could not compute report for %s: %s.",
                dQuote(sdn),
                sQuote(conditionMessage(attr(
                  r, "condition")))
              )
            } else {
              # Save the report as R object
              s_res <-
                try(prep_save_report(r, file.path(output_dir, gsub(
                  "[^a-zA-Z0-9_\\.]",
                  "",
                  sprintf("report_%s.dq2", sdn)
                ))))
              if (inherits(s_res, "try-error")) {
                # if the report could not be saved
                # gives a warning
                util_warning(
                  "Could not save report for %s: %s.",
                  dQuote(sdn),
                  sQuote(conditionMessage(attr(
                    s_res, "condition")))
                )
              }
              # remove object to reduce memory use
              rm(s_res)

              # Save the summaries for a quick overview of all the reports ----
              obj <- summary(r)
              this <- attr(obj, "this")
              this$stratum <- level_name
              this$used_data_file <- name_of_study_data
              this$segment <- cur_seg #not for combining
              this$sdn <- sdn
              attr(obj, "this") <- this

              s_res <-
                try(saveRDS(obj, file.path(output_dir, gsub(
                  "[^a-zA-Z0-9_\\.]",
                  "",
                  sprintf("report_summary_%s.RDS", sdn)
                ))))
              if (inherits(s_res, "try-error")) {
                # if the summary could not be saved
                util_warning(
                  "Could not save report summary for %s: %s.",
                  dQuote(sdn),
                  sQuote(conditionMessage(attr(
                    s_res, "condition")))
                )
              }
              # remove object to reduce memory use
              rm(s_res)
              # prepare the folder for saving the rendered report html file
              if (also_print) {
                .dir <- file.path(output_dir,
                                  gsub("[^a-zA-Z0-9_\\.]", "",
                                       sprintf("report_%s", sdn)))
                dir.create(.dir,
                           showWarnings = FALSE,
                           recursive = TRUE)
                if (!dir.exists(.dir)) {
                  # warning if the directory could not be created
                  util_warning(
                    paste0("Could not create directory %s. ",
                           "No HTML output for this segment"),
                    dQuote(.dir))
                } else {
                  # save the rendered html files in the directory
                  p_res <-
                    try(print.dataquieR_resultset2(
                      r,
                      dir = .dir,
                      view = FALSE,
                      disable_plotly = disable_plotly,
                      by_report = TRUE
                    ))
                  #creating the back link
                  if (inherits(p_res, "try-error")) {
                    # gives a warning if the report could not be rendered
                    util_warning(
                      "Could not create HTML report for %s: %s.",
                      dQuote(.dir),
                      sQuote(conditionMessage(
                        attr(p_res, "condition")))
                    )
                  }
                  rm(p_res)
                }
              }
            }
            # remove the reports to reduce memory use
            rm(r)
            gc()
            return(invisible(NULL))
          } else {
            #if no output directory is indicated, return the report
            return(r)
          }
          gc()
        }
      )
    }
  )

  # create the html overview page that links all sub-reports created -----
  if (!missing(output_dir) && also_print) {
    util_create_report_by_overview(
      output_dir = output_dir,
      strata_column = strata_column,
      segment_column = segment_column,
      strata_column_label = strata_column_label,
      subgroup = subgroup,
      mod_label = mod_label
    )
  }
  prep_purge_data_frame_cache()
  if (missing(output_dir)) {
    if (view) {
      return(overall_res)
    } else {
      return(invisible(overall_res))
    }
  } else {
    if (view && also_print) {
      util_view_file(file.path(output_dir, "index.html"))
      return(overall_res)
    } else {
      return(invisible(overall_res))
    }
  }

}
