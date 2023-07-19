# collect all errors, warnings, or messages so that they are combined for a combined result
util_collapse_msgs <- function(class, all_of_f) { # class is either error, warning or message
  # extract and create a list of all the messages by class
  allmsgsofclass <- lapply(all_of_f, attr, class)
  # TODO: This is maybe not very general, but the function is used for result attached messages, not for the general overview
  allmsgsofclass <- lapply(allmsgsofclass, function(msgofclass) {
    msgofclass[!vapply( # do not show conditions about calls, that would not be possible at all (e.g., loess for only categorical scales)
      lapply(msgofclass, attr, "intrinsic_applicability_problem"),
      identical,
      TRUE,
      FUN.VALUE = logical(1)
    )]
  })
  # /TODO
  msgs <- lapply(allmsgsofclass, vapply, conditionMessage, FUN.VALUE = character(1)); # extract and create a list of all the messages by class
  # the messages are grouped to avoid repetitions, so the messages are amended
  nms <- gsub("^.*?\\.", "", names(msgs), perl = TRUE)
  # remove variable names
  dfr <- do.call(rbind.data.frame, mapply(SIMPLIFY = FALSE, msgs, nms, FUN = function(msg_lst, varname) {
    msg_lst <- gsub(sprintf("(\\W|^)\\Q%s\\E(\\W|$)", varname), "<VARIABLE>", msg_lst, perl = TRUE)
    if (length(msg_lst) > 0) {
      r <- data.frame(varname = varname, message = msg_lst)
    } else {
      r <- data.frame(varname = character(0), message = character(0))
    }
    r
  }))
  # remove numbers
  dfr$message <- gsub("(\\W|^)\\d+(\\W|$)", " <NUMBER> ", dfr$message, perl = TRUE)
  dfr$message <- trimws(gsub(" +", " ", dfr$message, perl = TRUE))
  if (!prod(dim(dfr))) {
    return(character(0))
  }
  # group messages
  dfr <- dplyr::summarize(dplyr::group_by(dfr, message), varname =
                            paste(get("varname"), collapse = ", "))
  msgs <-
    mapply(dfr$varname, dfr$message,
           FUN = function(name, msg) {
             nm <- dQuote(strsplit(name, ", ", fixed = TRUE)[[1]])
             if (length(nm) > 5) { # truncate variable names for the current warning message group
               nm <- c(head(nm, 4), "...")
             } else if (length(nm) == 1 && nm == dQuote("[ALL]")) {
               nm <- "all variables"
             }
             paste0("For ", paste(nm, collapse = ", "), ": ", msg)
           })
  unname(msgs)
}
