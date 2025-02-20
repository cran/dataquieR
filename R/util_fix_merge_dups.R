#' Fix results from merge
#'
#' this function handles the result of [merge()]-calls, if `no.dups = TRUE` and
#' `suffixes = c("", "")`
#'
#' @param dfr data frame to fix
#' @param stop_if_incompatible [logical] stop if data frame can not be fixed
#'
#' @family data_management
#' @concept summary
#' @keywords internal
util_fix_merge_dups <- function(dfr,
                                stop_if_incompatible = TRUE) {
  dupcols <- unique(colnames(dfr)[duplicated(colnames(dfr))])
  for (dc in dupcols) {
    cur <- which(colnames(dfr) == dc)
    fix_nas <- dfr[, cur, drop = FALSE]
    xx <- apply(simplify = FALSE, fix_nas, 1, function(rw) {
      urw <- unique(rw)
      urw <- urw[!is.na(urw)]
      if (length(urw) > 1) {
        util_warning(c("Internal error: could not fix merge result, results are",
                       "incompatible. Sorry, please report"))
        return(rw)
      }
      if (length(urw) == 0) {
        # no action needed, all NA
        return(rw)
      }
      val <- urw[[1]]
      rw[] <- val
      return(rw)
    })
    fix_nas[] <- do.call(rbind, xx)
    # all(fix_nas[[1]] == fix_nas[[2]])
    fix_nas <- t(unique(t(fix_nas)))
    if(stop_if_incompatible) {
      util_stop_if_not(
        `Internal error, sorry, please report: fix_merge_dups failed` =
          ncol(fix_nas) == 1)
    } else {

      if (ncol(fix_nas) > 1) {
        colnames(dfr)[tail(cur, -1)] <-
          paste0(colnames(dfr)[tail(cur, -1)], ".", seq_len(length(cur)-1))  #TODO: check if there is not already a column with that name in all data, not just dfr
      } else {
        dfr[, head(cur, 1)] <- as.vector(fix_nas)
        dfr[, tail(cur, -1)] <- NULL

      }
    }

  }
  dfr
}
