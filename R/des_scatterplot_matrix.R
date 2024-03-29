#' Compute Pairwise Correlations
#'
#' works on variable groups (`cross-item_level`), which are expected to show
#' a Pearson correlation
#'
#' [Descriptor] # TODO: This can be an indicator
#'
#' @param study_data [data.frame] the data frame that contains the measurements
#' @param meta_data [data.frame] the data frame that contains metadata
#'                               attributes of study data
#' @param label_col [variable attribute] the name of the column in the metadata
#'                                       with labels of variables
#' @param meta_data_cross_item [meta_data_cross]
#'
#' @return a `list` with the slots:
#'   - `SummaryPlotList`: for each variable group a [ggplot] object with
#'                        pairwise correlation plots
#'   - `SummaryData`: table with columns `VARIABLE_LIST`, `cors`,
#'                    `max_cor`, `min_cor`
#'   - `SummaryTable`: like `SummaryData`, but machine readable and with
#'                     stable column names.
#' @export
#'
#' @importFrom stats cor
#'
#' @examples
#' \dontrun{
#' devtools::load_all()
#' prep_load_workbook_like_file("meta_data_v2")
#' des_scatterplot_matrix("study_data")
#' }
des_scatterplot_matrix <- function(study_data, meta_data,
                                   label_col = LABEL,
                                   meta_data_cross_item = "cross-item_level") {
  util_ensure_suggested(c("GGally"),
                        goal = "create scatter plot matrices")
  prep_prepare_dataframes()
  meta_data_cross_item <- util_normalize_cross_item(
    meta_data = meta_data,
    meta_data_cross_item = meta_data_cross_item,
    label_col = label_col)
  VariableGroupPlotList <- lapply(which(
    useNames = TRUE,
    trimws(tolower(setNames(meta_data_cross_item$ASSOCIATION_METRIC,
                            nm = meta_data_cross_item$CHECK_LABEL))) ==
      "pearson"), function(vg) {
        rvs <-
          vapply(
            util_parse_assignments(meta_data_cross_item[12, "VARIABLE_LIST"]),
            identity, FUN.VALUE = character(1))
        GGally::ggpairs(ds1[, rvs], progress = FALSE)
      })
  VariableGroupTable <-
    util_rbind(data_frames_list = lapply(which(
      useNames = TRUE,
      trimws(tolower(setNames(meta_data_cross_item$ASSOCIATION_METRIC,
                              nm = meta_data_cross_item$CHECK_LABEL))) ==
        "pearson"), function(vg) {

          vl <- meta_data_cross_item[12, "VARIABLE_LIST"]

          rvs <-
            vapply(
              util_parse_assignments(vl),
              identity, FUN.VALUE = character(1))

          cors <- cor(ds1[, rvs, FALSE], use = "pairwise.complete.obs")

          cors_combs <-
            t(apply(expand.grid(rvs, rvs), 1, sort))

          cors_combs <-
            cors_combs[!duplicated(cors_combs), , drop = FALSE]

          cors_combs <-
            cors_combs[
              apply(cors_combs, 1, function(r) length(unique(r)) > 1), ,
              drop = FALSE
            ]

          cors_of_combs <- setNames(apply(cors_combs, 1, function(r) {
            cors[r[[1]], r[[2]]]
          }), nm = apply(cors_combs, 1, function(r) {
            sprintf("cor(%s, %s)", r[[1]], r[[2]])
          }))

          data.frame(VARIABLE_LIST = vl,
                     pretty_cors = prep_deparse_assignments(labels = format(cors_of_combs),
                                                     codes = names(cors_of_combs),
                                                     mode = "string_codes"),
                     pretty_max_cor = format(max(cors_of_combs, na.rm = TRUE)),
                     cors = prep_deparse_assignments(labels = cors_of_combs,
                                                    codes = names(cors_of_combs),
                                                    mode = "string_codes"),
                    max_cor = max(cors_of_combs, na.rm = TRUE)
          )
          #            # max(cor()))
        }))
  VariableGroupData <- VariableGroupTable[, c(VARIABLE_LIST, grep("^pretty_.*$",
                                                                  names(VariableGroupTable), value = TRUE))]
  colnames(VariableGroupData) <- gsub("^pretty_", "", colnames(VariableGroupData))
  list(
    VariableGroupPlotList = VariableGroupPlotList,
    VariableGroupTable = VariableGroupTable[, !startsWith(colnames(VariableGroupTable), "pretty_")],
    VariableGroupData = VariableGroupData
  )
}
# des_scatterplot_matrix
# https://plotly.com/r/splom/
# https://r-charts.com/correlation/ggpairs/
# GGally
