#' Support function to augment metadata during data quality reporting
#'
#' @description
#' adds an annotation to static metadata
#'
#' @details
#'
#' Add metadata e.g. of transformed/new variable
#' This function is not yet considered stable, but we already export it,
#' because it could help. Therefore, we have some inconsistencies in the
#' formals still.
#'
#'
#'
#' @param VAR_NAMES [character] Names of the Variables to add
#' @param DATA_TYPE [character] Data type for the added variables
#' @param LABEL [character] Labels for these variables
#' @param VALUE_LABELS [character] Value labels for the values of the variables
#'                                 as usually pipe separated and assigned with
#'                                 `=`: `1 = male | 2 = female`
#' @param ... Further defined variable attributes, see
#'            [dataquieR::prep_create_meta]
#' @param item_level [data.frame] the metadata to extend
#' @param meta_data [data.frame] old name for `item_level`
#' @param meta_data_v2 [character] path to workbook like metadata file, see
#'                                 [`prep_load_workbook_like_file`] for details.
#'                                 **ALL LOADED DATAFRAMES WILL BE PURGED**,
#'                                 using [`prep_purge_data_frame_cache`],
#'                                 if you specify `meta_data_v2`.
#'
#' @return a data frame with amended metadata.
#'
#' @export
#'
prep_add_to_meta <- function(VAR_NAMES, DATA_TYPE, LABEL, VALUE_LABELS,
                             item_level = "item_level",
                             meta_data = item_level,
                             meta_data_v2,
                             ...) {

  util_maybe_load_meta_data_v2()

  util_expect_data_frame(meta_data)

  mini_md <- prep_create_meta(
    level = NULL,
    "VAR_NAMES" = VAR_NAMES,
    "DATA_TYPE" = DATA_TYPE,
    "LABEL" = LABEL,
    "VALUE_LABELS" = VALUE_LABELS,
    ...
  )

  mini_md <- mini_md[, names(mini_md) %in% names(meta_data)]
  new_names <- c(names(mini_md), names(meta_data)[!(names(meta_data) %in%
                                                      names(mini_md))])
  # new_names <- unique(c(names(mini_md), names(meta_data)))

  mini_md[, (dim(mini_md)[2] + 1):dim(meta_data)[2]] <- NA

  colnames(mini_md) <- new_names

  partvar_vector <-
    mini_md[[PART_VAR]]

  partvar_vector <-
    partvar_vector[!util_empty(partvar_vector)]

  if (PART_VAR %in% names(mini_md) &&
      any(is.na(util_find_var_by_meta(resp_vars = as.character(
        partvar_vector))))) {
    miss <-
      partvar_vector[
        is.na(util_find_var_by_meta(resp_vars = as.character(
          partvar_vector)))]
    util_error(c("In the existing %s, in the column %s, at least one of the",
                  "referred variables does not exist: %s."),
               sQuote("meta_data"),
               dQuote(PART_VAR),
               util_pretty_vector_string(miss))
  }

  meta_data <- dplyr::bind_rows(meta_data, mini_md)
  # Column order is auto-repaired for 2 DFs in R >= 3.3.0 by rbind too,
  # but I did not find an offical source for that. Only:
  # https://stackoverflow.com/q/16962576

  return(meta_data)
}
