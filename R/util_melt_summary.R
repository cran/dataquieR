#' Melt a summary of a report table to long-format
#'
#' @param r [dq_report2] a report
#' @param ... arguments for [summary()]
#'
#' @return [data.frame] long format, compatible with [prep_summary_to_classes()]
#'
#' @family summary_functions
#' @keywords internal
util_melt_summary <- function(r, ...) {

  sum_old_style_intermediate <-
    as.data.frame(summary(r, ...), stringsAsFactor = FALSE)

  n_cols <- ncol(sum_old_style_intermediate)

  if (nrow(sum_old_style_intermediate) > 0) {
    sum_old_style_intermediate[[VAR_NAMES]] <-
      util_map_labels(rownames(sum_old_style_intermediate),
                      attr(r, "meta_data"),
                      to = VAR_NAMES,
                      from = attr(r, "label_col"))
    try(sum_old_style_intermediate[[STUDY_SEGMENT]] <-
      util_map_labels(rownames(sum_old_style_intermediate),
                      attr(r, "meta_data"),
                      to = STUDY_SEGMENT,
                      from = attr(r, "label_col")), silent = TRUE) # dont die with "Missing columns “STUDY_SEGMENT” from “meta_data”"
    if (!STUDY_SEGMENT %in% colnames(sum_old_style_intermediate)) {
      sum_old_style_intermediate[[STUDY_SEGMENT]] <- "Study"
    }
  } else {
    sum_old_style_intermediate[["class"]] <- util_as_cat(character(0))
    sum_old_style_intermediate <- rbind.data.frame(data.frame(VAR_NAMES = NA_character_,
                                                  STUDY_SEGMENT = NA_character_,
                                                   class = util_as_cat(NA),
                                                  stringsAsFactors = FALSE),
                                                  sum_old_style_intermediate )
  }

#  reshape::melt(sum_old_style_intermediate,
#                na.rm = TRUE,
#                id.vars = c(VAR_NAMES, STUDY_SEGMENT),
#                variable_name = "call_names") %>%
#    dplyr::rename(class = value) -> res

# Added an if() because using stats:reshape instead of reshape::melt
# create issues if the dataframe is empty
  if (is.null(sum_old_style_intermediate$STUDY_SEGMENT)) {
    sum_old_style_intermediate$STUDY_SEGMENT <- NA
  }
  sum_old_style_intermediate$STUDY_SEGMENT[
    is.na(sum_old_style_intermediate$STUDY_SEGMENT)] <- ""


  if (!all(is.na(sum_old_style_intermediate$VAR_NAMES))) {
    res <-
      stats::reshape(data = sum_old_style_intermediate,
                     idvar = c("VAR_NAMES", "STUDY_SEGMENT"),
                     varying = colnames(sum_old_style_intermediate)[1:n_cols],
                     v.names = "class",
                     times = colnames(sum_old_style_intermediate)[1:n_cols],
                     direction = "long")

    names(res)[names(res) == "time"] <- "call_names"

    levs <- unique(res$call_names)
    #sort levels
    #levs <- sort(levs)
    levs_int <- levs[grep("int_", levs)]
    levs_com <- levs[grep("com_", levs)]
    levs_con <- levs[grep("con_", levs)]
    levs_acc <- levs[grep("acc_", levs)]

    #order the factor levels
    res$call_names <- factor(res$call_names, levels = c(levs_int,levs_com,
                                            levs_con, levs_acc))

    rownames(res) <- NULL

    #Order rows by int, com, con, and acc
    res_int <- res[grep("int_",res$call_names), , drop = FALSE]
    #res_int <- res_int[order(res_int$call_names), , drop = FALSE]

    res_com <- res[grep("com_",res$call_names), ,drop = FALSE]
    #res_com <- res_com[order(res_com$call_names), , drop = FALSE]

    res_con <- res[grep("con_",res$call_names), ,drop = FALSE]
    #res_con <- res_con[order(res_con$call_names), , drop = FALSE]

    res_acc <- res[grep("acc_", res$call_names), ,drop = FALSE]
    #res_acc <- res_acc[order(res_acc$call_names), , drop = FALSE]

    res <- rbind(res_int,res_com,res_con,res_acc)


    res <- res[!is.na(res$class), , drop = FALSE]
    } else {
      res <- data.frame(VAR_NAMES = NA_character_,
                        STUDY_SEGMENT = NA_character_,
                        call_names = factor(NA, levels = "class"),
                        class = util_as_cat(NA),
                        stringsAsFactors = FALSE)
      res <- res[0, , drop = FALSE]
      }

  rm(n_cols)

  res$function_name <-
    vapply(FUN.VALUE = character(1),
           setNames(nm = res$call_names),
           util_cll_nm2fkt_nm,
           report = r)

  res
}
