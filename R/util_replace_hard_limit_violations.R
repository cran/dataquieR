#' Replace limit violations (HARD_LIMITS) by NAs
#'
#' @param study_data [study_data]
#' @param meta_data [study_data]
#' @param label_col [variable attribute] the name of the column in the metadata
#'                                       with labels of variables
#'
#' @return modified `study_data`
#'
#' @family data_management
#' @concept metadata_management
#' @noRd
 util_replace_hard_limit_violations <- function(study_data,
                                                meta_data,
                                                label_col) {

   if (!isTRUE(attr(study_data, "Codes_to_NA"))) {
     util_error(
       c("Missing codes have to have been replaced already by NA, before %s",
         "violations can be replaced by NA. This is an internal error.",
         "As a dataquieR developer, please ensure, that you only pass",
         "%s that have the %s attribute (set, e.g., by %s or %s) as %s to %s."),
       sQuote(HARD_LIMITS),
       sQuote("study_data"),
       dQuote("Codes_to_NA"),
       sQuote("prep_prepare_dataframes"),
       sQuote("util_replace_codes_by_NA"),
       sQuote("study_data"),
       sQuote("util_replace_hard_limit_violations")
       )
   }
   if (!isTRUE(attr(study_data, "MAPPED", exact = TRUE))) {
     util_error(
       c("%s must have been mapped with %s, before %s",
         "violations can be replaced by NA. This is an internal error.",
         "As a dataquieR developer, please ensure, that you only pass",
         "%s that have the %s attribute (set, e.g., by %s) as %s to %s."),
       sQuote("study_data"),
       sQuote("meta_data"),
       sQuote(HARD_LIMITS),
       sQuote("study_data"),
       dQuote("MAPPED"),
       sQuote("prep_prepare_dataframes"),
       sQuote("study_data"),
       sQuote("util_replace_hard_limit_violations")
     )
   }

   if (!HARD_LIMITS %in% names(meta_data)) {
     util_message("Cannot replace %s violations, because %s do not provide %s.",
                  sQuote(HARD_LIMITS),
                  dQuote("meta_data"),
                  sQuote(HARD_LIMITS),
                  applicability_problem = TRUE)
     attr(study_data, "HL_viol_to_NA") <- TRUE
     return(study_data)
   } else {
     hl <- setNames(meta_data[[HARD_LIMITS]], nm = meta_data[[label_col]])
     hl[util_empty(hl)] <- NA
   }

   i <- lapply(hl, util_parse_interval)

   if (length(meta_data[[DATA_TYPE]]) == 0) {
     meta_data[[DATA_TYPE]] <- "n/a in metadata"
   }

   study_data[] <- mapply(study_data, i, names(i), meta_data[[DATA_TYPE]],
                          SIMPLIFY = FALSE,
                          FUN = function(col, int, nm, dt) {
     if (inherits(int, "interval") && (prep_dq_data_type_of(col) %in%
                                        c(DATA_TYPES$INTEGER,
                                          DATA_TYPES$FLOAT,
                                          DATA_TYPES$DATETIME,
                                          DATA_TYPES$TIME))) {
       outside <- !redcap_env$`in`(col, int)
       outside[is.na(outside)] <- FALSE
       col[outside] <- NA
       col
     } else {
       if (inherits(int, "interval")) {
         util_warning(c("For %s, I have %s but the column is of type %s",
                        "(metadata say %s)"),
                      dQuote(nm), sQuote(HARD_LIMITS),
                      sQuote(prep_dq_data_type_of(col)),
                      sQuote(dt), applicability_problem = TRUE)
       }
       col
     }
   })

   attr(study_data, "HL_viol_to_NA") <- TRUE
   study_data
 }
