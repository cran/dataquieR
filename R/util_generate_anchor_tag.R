#' Generate a tag for a specific result
#'
#' for `dq_report2`
#'
#' @param varname variable to create an anchor for
#' @param callname function call to create an anchor for
#' @param order_context anchor created on variable overview or indicator
#'                      overview page
#' @param name replaces `varname` and `callname`, must contain the `.`
#'             separator, then
#'
#' @return the `htmltools` tag
#'
#' @family reporting_functions
#' @concept process
#' @noRd
util_generate_anchor_tag <- function(
    varname,
    callname,
    order_context = c("variable",
                      "indicator"),
    name) {
  util_ensure_suggested("htmltools")
  if (!missing(name)) {
    util_stop_if_not(missing(varname) && missing(callname))
    varname <- sub("^.*?\\.", "", name)
    callname <- sub("^([^\\.]*).*$", "\\1", name)
    if (varname == callname && callname == name) {
      if (order_context == "variable") {
        varname <- ""
      } else {
        callname <- ""
      }
    }
    if (varname == "[ALL]") varname <- ""
    if (callname == "[ALL]") callname <- ""
  }
  dot <- ""
  if (nzchar(varname)) {
    varname <- prep_link_escape(varname, html = TRUE)
    if (nzchar(callname)) {
      dot <- "."
    }
  }
  if (util_match_arg(order_context) == "variable") {
    id <- sprintf("%s%s%s", varname, dot, callname)
  } else {
    id <- sprintf("%s%s%s", callname, dot, varname)
  }
  htmltools::a(id = id)
}

#' Generate a link to a specific result
#'
#' for `dq_report2`
#'
#' @param varname variable to create a link to
#' @param callname function call to create a link to
#' @param order_context link created to variable overview or indicator
#'                      overview page
#' @param name replaces `varname` and `callname`, must contain the `.`
#'             separator, then
#' @param title optional, replaces auto-generated link title
#'
#' @return the `htmltools` tag
#'
#' @family reporting_functions
#' @concept process
#' @noRd
util_generate_anchor_link <- function(
    varname,
    callname,
    order_context = c("variable",
                      "indicator"),
    name,
    title) {
  util_ensure_suggested("htmltools")
  if (!missing(name)) {
    util_stop_if_not(missing(varname) && missing(callname))
    varname <- sub("^.*?\\.", "", name)
    callname <- sub("^([^\\.]*).*$", "\\1", name)
    if (varname == callname && callname == name) {
      if (order_context == "variable") {
        util_error("For links to an variable page, I need to know the variable")
      } else {
        util_error(
          "For links to an indicator page, I need to know the indicator")
      }
    }
    if (varname == "[ALL]") varname <- ""
    if (callname == "[ALL]") callname <- ""
  }
  orig_varname <- varname
  dot <- ""
  if (nzchar(varname)) {
    varname <- prep_link_escape(varname, html = TRUE)
    if (nzchar(as.character(callname))) {
      dot <- "."
    }
  }
  if (util_match_arg(order_context) == "variable") {
    if (!missing(title)) {
      .title <- title
    } else {
      .title <- prep_title_escape(util_alias2caption(callname, long = TRUE),
                                  html = TRUE)
    }
    href <- sprintf("VAR_%s.html#%s%s%s", varname, varname, dot, callname)
  } else {
    .title <- prep_title_escape(orig_varname, html = TRUE)
    if (startsWith(callname, "acc_")) {
      fil <- paste0("dim_", "acc_", callname, ".html")
      # the "dim_" prefix is required because otherwise windows will ignore a file called con.html confusing it with a special device con:
    } else {
      dim <- sub("_.*$", "", callname)
      fil <- paste0("dim_", dim, ".html")
    }
    href <- sprintf("%s#%s%s%s", fil, callname, dot, varname)
  }
  if (!missing(title)) {
    .title <- title
  }
  if (!nzchar(.title) && any(grepl(".[ALL]", fixed = TRUE, name))) {
    .title <- gsub(".[ALL]", "", fixed = TRUE, name)
  }
  htmltools::a(href = href, .title)
}
