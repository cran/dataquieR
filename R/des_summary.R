#' Compute Descriptive Statistics - categorical variables
#'
#' @description
#'
#' generates a descriptive overview of the categorical variables (nominal and
#' ordinal) in `resp_vars`.
#'
#' [Descriptor]
#'
#' @details
#' TODO
#'
#' @inheritParams .template_function_indicator
#'
#' @param resp_vars [variable] the name of the categorical measurement variable
#' @param hard_limits_removal [logical] if TRUE values outside hard limits are
#'                                      removed from the data before calculating
#'                                      descriptive statistics. The default is FALSE
#' @param ... arguments to be passed to all called indicator functions if
#'            applicable.
#'
#' @return a [list] with:
#'   - `SummaryTable`: [data.frame]
#'   - `SummaryData`: [data.frame]
#' @export
#'
#' @examples
#' \dontrun{
#' prep_load_workbook_like_file("meta_data_v2")
#' xx <- des_summary_categorical(study_data = "study_data", meta_data =
#'                               prep_get_data_frame("item_level"))
#' util_html_table(xx$SummaryData)
#' util_html_table(des_summary_categorical(study_data = prep_get_data_frame("study_data"),
#'                    meta_data = prep_get_data_frame("item_level"))$SummaryData)
#' }
#' @seealso
#' [Online Documentation](
#' https://dataquality.qihs.uni-greifswald.de/VIN_des_impl_summary.html
#' )
#' @importFrom graphics barplot hist
#' @importFrom stats mad
#'
des_summary_categorical <- function(resp_vars = NULL, # IDEA: group_vars = NULL, co_vars = NULL,
                                    study_data,
                                    label_col,
                                    item_level = "item_level",
                                    meta_data = item_level,
                                    meta_data_v2,
                                    hard_limits_removal =
                                      getOption("dataquieR.des_summary_hard_lim_remove",
                                                dataquieR.des_summary_hard_lim_remove_default),
                                    ...) { # TODO: Describe @return slots

# TODO: add figures also to Excel Exports and prevent column type guessing
# by datatables export correctly (Java Script)

# TODO: add function util_first_arg_study_data_or_resp_vars

  # preps ----------------------------------------------------------------------
  util_maybe_load_meta_data_v2()
  # map metadata to study data
  util_expect_data_frame(study_data)
  if (inherits(
    try(prep_prepare_dataframes(.replace_hard_limits = hard_limits_removal), silent = TRUE),
    "try-error")) {
    w <- "not all %s found, estimating %s"
    if (suppressWarnings(util_ensure_suggested("cli", err = FALSE))) {
      w <- cli::bg_black(cli::col_yellow(w))
    }
    util_warning(w, sQuote("meta_data"), sQuote("meta_data"),
                 immediate = TRUE)
    meta_data <-
      prep_study2meta(study_data, level = VARATT_REQUIRE_LEVELS$RECOMMENDED)
    prep_prepare_dataframes(.meta_data = meta_data, .replace_hard_limits = hard_limits_removal)
  }

  ds1_factor <-
    prep_prepare_dataframes(.study_data = study_data,
                            .replace_hard_limits = FALSE,
                            .apply_factor_metadata = TRUE,
                            .apply_factor_metadata_inadm = TRUE,
                            .internal = FALSE)

  # define resp_vars
  if (length(resp_vars) == 0) {
    suppress <- function(...) suppressWarnings(suppressMessages(...))
    resp_vars <- colnames(ds1)
  } else {
    suppress <- eval
  }

  #If resp_vars is a var_name and not a LABEL, convert it to LABEL to check intersect
  if (length(intersect(colnames(ds1), resp_vars)) == 0) {
    resp_vars<- unname(prep_get_labels(resp_vars,
                                       label_col = label_col,
                                       meta_data = meta_data,
                                       resp_vars_are_var_names_only = TRUE))
  }

  resp_vars <- intersect(colnames(ds1), resp_vars)

  suppress(util_correct_variable_use(resp_vars,
                                     allow_more_than_one = TRUE,
                                     #             need_type = "integer|float|datetime",
                                     do_not_stop = TRUE,
                                     need_scale = "!na"))

  # create a named vector of VAR_NAMES with names = LABEL
  study_data_lc <- attr(study_data, "label_col")
  if (is.null(study_data_lc)) {
    study_data_lc <- VAR_NAMES
  }
  varnames <- setNames(nm =
                         prep_map_labels(colnames(study_data),
                                         to = label_col,
                                         from = study_data_lc,
                                         meta_data = meta_data,
                                         ifnotfound = colnames(study_data)),
                       object = colnames(study_data))
  # create a named vector of SEGMENTS with names = VAR_NAMES
  if (STUDY_SEGMENT %in% colnames(meta_data)) {
    segments <- prep_map_labels(varnames,
                                meta_data = meta_data,
                                from = study_data_lc,
                                to = STUDY_SEGMENT,
                                ifnotfound = NA_character_,
                                warn_ambiguous = FALSE)
  } else {
    segments <- NULL
  }
  ## Select only resp_vars that are categorical
  df_category <- meta_data[meta_data[, label_col, drop = TRUE] %in% resp_vars, ]
  df_category <- df_category[df_category$SCALE_LEVEL %in% c("nominal","ordinal"), ]
  resp_vars <- df_category[,label_col, drop = TRUE]
  rm(df_category)

  #defining the segments of the resp_vars
  if (!is.null(segments)) {
    sseg <- segments[varnames[resp_vars]]
  } else {
    sseg <- setNames(object = rep("All", length(varnames[resp_vars])),
                     nm = varnames[resp_vars])
  }


  # Functions
  # create a function to obtain a table for ordinal and nominal variables
  frequency_table <- function(lb) {
    rv <- varnames[[lb]]
    r <- ""
    if (scale_levels[[rv]] %in% c(SCALE_LEVELS$`NA`,
                                  NA_character_)) {
      #empty result
      r <- ""

    } else if (scale_levels[[rv]] %in% c(SCALE_LEVELS$ORDINAL,SCALE_LEVELS$NOMINAL)) {
      # create a vector with the values of the variable
      dt <- ds1[[lb]]

      if (scale_levels[[rv]] %in% c(SCALE_LEVELS$ORDINAL,
                                    SCALE_LEVELS$NOMINAL)) {
        dt <- ds1_factor[[lb]]
      }

      # No. unique values included NA
      r <- paste0(r, "No. unique values (incl. NA): ",
                  format(length(unique(dt))))


      # create a dataframe from the vector calculating the frequency of each value
      if(nrow(table(dt))>1) {
        cont_tab <- as.data.frame(sort(table(dt),
                                       decreasing = TRUE, na.last = FALSE))

      } else {
        cont_tab <- as.data.frame(table(dt), na.last = FALSE)
      }

      cont_tab[[1]] <- sQuote(cont_tab[[1]]) # to prevent values from being treated as numeric
      # rename the table
      colnames(cont_tab) <- c("Value", "Freq")
      # in case of tables with more than 5 rows, sum last values (less frequent)
      if (nrow(cont_tab) > 5) {
        foot <- tail(cont_tab, -5)
        foot <- c("Others", sum(foot[[2]], na.rm = TRUE))
        #      foot <- c(paste0(foot[[1]], collapse = ", "), sum(foot[[2]],
        #                                                        na.rm = TRUE))
        cont_tab <- rbind(head(cont_tab, 5), foot)
      }
      # save the formatted table in r
      r <- paste0(r, "",
                  util_formattable(util_first_row_to_colnames(
                    as.data.frame(t(cont_tab))),
                    min_color = c(235, 235, 235),
                    max_color = c(20, 20, 235)))
      rm(cont_tab, dt)
    } else {
      util_error(
        c("Internal error unexpected scale level %s for %s in %s, sorry.",
          "Please file a bug report."),
        dQuote(scale_levels[[rv]]),
        dQuote(rv),
        sQuote("des_summary_categorical")
      )
    }

    r
  }

  #function for Median - based on conditions
  c_median <- function(lb) {
    rv <- varnames[[lb]]
    r <- ""
    if (scale_levels[[rv]] %in% c( SCALE_LEVELS$ORDINAL)) {
      dt <- ds1_factor[[lb]]
      m <- median.ordered(dt)
      r <- paste0(r, format(m))
    } else if (scale_levels[[rv]] %in% c(SCALE_LEVELS$NOMINAL)) {
      r <- ""
    }
    r
  }

  #function for Mode - based on conditions
  c_mode <- function(lb) {
    rv <- varnames[[lb]]
    r <- ""
    if (scale_levels[[rv]] %in% c(SCALE_LEVELS$ORDINAL)) {
      dt <- ds1_factor[[lb]]
      dt <- dt[!is.na(dt)]
      mode_value <-  unique(dt)[tabulate(match(dt, unique(dt))) ==
                                  max(tabulate(match(dt, unique(dt))))]
      if (length(mode_value) > 5) {
        mode_value <- paste(c(head(mode_value, 5), ...), collapse = " ")
      } else if (length(mode_value) > 1) {
        mode_value <- paste(mode_value,collapse = " ")
      }
      r <- paste0(r, mode_value)
    } else if (scale_levels[[rv]] %in% c(SCALE_LEVELS$NOMINAL)) {
      dt <- ds1_factor[[lb]]
      dt <- dt[!is.na(dt)]
      mode_value <-  unique(dt)[tabulate(match(dt, unique(dt))) ==
                                  max(tabulate(match(dt, unique(dt))))]
      if (length(mode_value) > 5) {
        mode_value <- paste(c(head(mode_value, 5), ...), collapse = " ")
      } else if (length(mode_value) > 1) {
        mode_value <- paste(mode_value,collapse = " ")
      }
      r <- paste0(r, mode_value)
    }
    r
  }

  #function for quantiles and IQR - based on conditions
  c_quantiles <- function(lb) {

    rv <- varnames[[lb]]
    r <- ""
    if (scale_levels[[rv]] %in% c(SCALE_LEVELS$ORDINAL)) {
      dt <- ds1_factor[[lb]]
      q <- quantile(dt, na.rm = TRUE, names = TRUE, type = 1)  #TODO: check the TYPE
      names(q) <- paste0("Q", seq_len(length(q)) - 1)
      q <- q[c("Q1", "Q3")]
      r <- prep_deparse_assignments(labels = as.character(q),
                                    codes = names(q),
                                    mode = "string_codes")
      r <- paste0(format(IQR(na.rm = TRUE, x = dt, type = 1)), " (", r, ")")  #TODO: check the TYPE
    } else if (scale_levels[[rv]] %in% c(SCALE_LEVELS$NOMINAL)) {
      r <- ""
    }
    r
  }

  #function for Min - Max - based on conditions
  c_min_max <- function(lb) {
    rv <- varnames[[lb]]
    r <- ""
    if(scale_levels[[rv]] %in% c(SCALE_LEVELS$ORDINAL)) {
      dt <- ds1_factor[[lb]]
      r <- paste0(" (",
                  format(min(dt, na.rm = TRUE)), " - ",
                  format(max(dt, na.rm = TRUE), na.rm = TRUE), ")")
    } else if (scale_levels[[rv]] %in% c(SCALE_LEVELS$NOMINAL)) {
      r <- ""
    }
    r
  }


  # create a named vector of LONG LABELS (or LABELS or VAR_NAMES)
  # with names = VAR_NAMES
  labs <-
    prep_get_labels(varnames,
                    label_col = label_col,
                    meta_data = meta_data,
                    resp_vars_are_var_names_only = TRUE,
                    label_class = "LONG")

  # create a named vector of DATA_TYPEs with names = VAR_NAMES
  data_types <-
    prep_map_labels(varnames,
                    meta_data = meta_data,
                    to = DATA_TYPE,
                    ifnotfound = NA_character_,
                    warn_ambiguous = FALSE)

  # create a named vector of SCALE_LEVELs with names = VAR_NAMES
  scale_levels <-
    prep_map_labels(varnames,
                    meta_data = meta_data,
                    to = SCALE_LEVEL,
                    from = study_data_lc,
                    ifnotfound = NA_character_,
                    warn_ambiguous = FALSE)

  #Define content of col "Variables" with long_label, label, and var_names when different
  #  if (!.called_in_pipeline) {
  if (identical(resp_vars, unname(labs[resp_vars]))) {
    var_name_firstcol <- resp_vars
  } else if (identical(unname(labs[varnames[resp_vars]]),  resp_vars)){
    var_name_firstcol <- paste0(resp_vars,
                                "<br />", varnames[resp_vars])
  } else {
    if (identical(resp_vars, unname(varnames[resp_vars]))){
      var_name_firstcol <- paste0(labs[varnames[resp_vars]],
                                  "<br />", varnames[resp_vars])

    } else {
      var_name_firstcol <- paste0(labs[varnames[resp_vars]], "<br />", resp_vars,
                                  "<br />", varnames[resp_vars])
    }

  }


  if (length(resp_vars) == 0) {
    variable_names_labs <- character(0)
  } else {
    variable_names_labs <-  paste0(scale_levels[varnames[resp_vars]],
                                   ", ",
                                   data_types[varnames[resp_vars]])
  }
  #  } else {
  #    var_name_firstcol <- resp_vars
  #
  #    if (identical(resp_vars, unname(labs[resp_vars]))) {
  #     variable_names_labs <- paste0(scale_levels[varnames[resp_vars]],
  #                                   ", ",
  #                                    data_types[varnames[resp_vars]])
  #   } else if (identical(unname(labs[varnames[resp_vars]]),  resp_vars)){
  #     variable_names_labs <- paste0(varnames[resp_vars], "<br />",
  #                                   scale_levels[varnames[resp_vars]],
  #                                    ", ",
  #                                   data_types[varnames[resp_vars]])
  #    } else {
  #      variable_names_labs <- paste0(resp_vars,
  ##                                    "<br />", varnames[resp_vars],"<br />",
  #                                    scale_levels[varnames[resp_vars]],
  #                                    ", ",
  #                                    data_types[varnames[resp_vars]])
  #   }
  # }

  plain_label <- resp_vars

  SummaryData <- data.frame(`Variables`  = util_attach_attr(
    var_name_firstcol,
    plain_label = plain_label),
    `Type` =  variable_names_labs,
    STUDY_SEGMENT = sseg,
    `Median` =
      vapply(resp_vars, c_median, character(1)),
    `Mode` =
      vapply(resp_vars, c_mode, character(1)),
    `IQR (Quartiles)` =
      vapply(resp_vars, c_quantiles, character(1)),
    `Range (Min - Max)` =
      vapply(resp_vars, c_min_max, character(1)),
    `No. categories/Freq. table` =
      vapply(resp_vars, frequency_table, character(1)),
    Valid = (if (length(resp_vars) == 0) character(0) else paste0(vapply(resp_vars,
                                                                         function(rv) sum(is.finite(ds1[[rv]])),
                                                                         FUN.VALUE = numeric(1)),
                                                                  "<br />",
                                                                  scales::percent(vapply(resp_vars,
                                                                                         function(rv) sum(is.finite(ds1[[rv]])),
                                                                                         FUN.VALUE =
                                                                                           numeric(1)) / nrow(ds1)))),
    Missing = (if (length(resp_vars) == 0) character(0) else paste0(vapply(resp_vars,
                                                                           function(rv) sum(util_empty(ds1[[rv]])),
                                                                           FUN.VALUE =
                                                                             numeric(1)), "<br />",
                                                                    scales::percent(vapply(resp_vars,
                                                                                           function(rv) sum(util_empty(ds1[[rv]])),
                                                                                           FUN.VALUE =
                                                                                             numeric(1)) / nrow(ds1)))),
    stringsAsFactors = FALSE, check.names = FALSE)

  # if (.called_in_pipeline) {
  #   tb <- meta_data
  #   tb <- tb[tb$VAR_NAMES %in% varnames[resp_vars], ]
  #
  #   list_var_names <- vapply(X =   SummaryData[["Variables"]],
  #                                            FUN = function(x){
  #                                              x <- gsub("<br />", " ", x)
  #                                            },
  #                                            FUN.VALUE =  character(1))
  #
  #
  #
  #   if(label_col == VAR_NAMES || !(LABEL %in% names(tb))){
  #     href <- tb$VAR_NAMES
  #     coltitle <- setNames(nm = tb$VAR_NAMES)
  #     data<-setNames(list_var_names, nm = tb$VAR_NAMES)
  #   } else {
  #     href <- tb$LABEL
  #     coltitle <- setNames(tb$VAR_NAMES, nm = tb$LABEL)
  #     data<-setNames(list_var_names, nm = tb$LABEL)
  #   }
  #   coltitle <- prep_title_escape(coltitle,
  #                                 html = TRUE)
  #
  #   data <- prep_title_escape(data,
  #                             html = TRUE)
  #   href <- paste0("VAR_", prep_link_escape(href,
  #                                           html = TRUE),
  #                  ".html#",
  #                  htmltools::urlEncodePath(as.character(href)))
  #
  #   onclick <- sprintf(
  #     'if (window.hasOwnProperty("dq_report2") && window.dq_report2 && window.location != "%s") { if (all_ids.all_ids.includes("%s")) { window.location = "%s" } else { window.alert("No result avaialable"); } }',
  #     href,
  #     href,
  #     href)
  #
  #   links <- mapply(
  #     href = sprintf('javascript:console.log("%s")', href),
  #     onclick = onclick,
  #     title = coltitle,
  #     filter = data,
  #     data ,
  #     SIMPLIFY = FALSE,
  #     FUN = htmltools::a
  #   )
  #
  #   SummaryData[["Variables"]] <- vapply(links,
  #                                            FUN = as.character,
  #                                            FUN.VALUE = character(1))
  #   rm(tb)
  # }

  mrg <- 2

  if (suppressWarnings(util_ensure_suggested("htmltools",
                                             err = FALSE,
                                             goal = "Distribution Plots in Descriptive Summary"))) {
    SummaryData$Graph <- lapply(resp_vars, function(lb) {
      rv <- varnames[[lb]]

      if (scale_levels[[rv]] %in% c(SCALE_LEVELS$NOMINAL,
                                    SCALE_LEVELS$ORDINAL)) {
        dt <- ds1_factor[[lb]]
        r <- htmltools::plotTag({
          withr::local_par(list(
            mar = rep(mrg, 4),
            oma = rep(0, 4)
          ))
          barplot(table(dt), main = NULL,
                  xlab = NULL, # TODO: use acc_distributions
                  ylab = NULL)},
          width = 250,
          height = 200,
          alt = paste("Histogram of",
                      labs[[rv]]))

        r <- as.character(r)
      } else {
        r <- ""
      }
      r <- paste0('<div style="min-width: 300px">', r, "</div>")
      r
    })
  } else {
    SummaryData$Graph <- ""
  }

  rownames(SummaryData) <- NULL

  SummaryTable <- SummaryData # TODO: machine readable

  list(SummaryData = util_attach_attr(SummaryData,
                                      is_html_escaped = TRUE), # TODO: plotlist
       SummaryTable = util_attach_attr(SummaryTable,
                                       is_html_escaped = TRUE)) # REVIEW: be sure, that content is html escaped!
}

#' @seealso https://stackoverflow.com/a/7925162
#' @author Richie Cotton
median.ordered <- function(x, na.rm = TRUE, ...) {
  levs <- levels(x)
  m <- median(as.integer(x), na.rm = na.rm, ...)
  if(floor(m) != m)
  {
    message("Median is between two values; using the first one")
    m <- floor(m)
  }
  ordered(m, labels = levs, levels = seq_along(levs))
}

# TODO: do not compute the sd and mean so frequently, do as for SE.skewness and skewness.

#' Compute the Skewness
#'
#' @param x data
#'
#' @return the Skewness
util_compute_skewness <- function(x) { # TODO: Verify, generated code
  n <- length(x)
  mean_x <- mean(x, na.rm = TRUE)
  sd_x <- sd(x, na.rm = TRUE)

  skewness <- (sum((x - mean_x)^3, na.rm = TRUE) / (n * sd_x^3))
  return(skewness)
}

#' Compute SE.Skewness
#'
#' @param x data
#' @param skewness if already known
#'
#' @return the standard error of skewness
util_compute_SE_skewness <- function(x,
                                     skewness = util_compute_skewness(x)) {
  n <- length(x)

  se_skewness <- sqrt((6 * n * (n - 1)) / ((n - 2) * (n + 1) * (n + 3)))

  return(se_skewness)
}

#' Compute Kurtosis
#'
#' @param x data
#'
#' @return the Kurtosis
util_compute_kurtosis <- function(x) {
  # TODO: Verify, generated code
  n <- length(x)
  mean_x <- mean(x, na.rm = TRUE)
  sd_x <- sd(x, na.rm = TRUE)

  kurtosis <- sum((x - mean_x)^4, na.rm = TRUE) / (n * sd_x^4) - 3

  return(kurtosis)
}

#' Move the first row of a data frame to its column names
#'
#' @param dfr [data.frame]
#'
#' @return [data.frame] with first row as column names
util_first_row_to_colnames <- function(dfr) {
  colnames(dfr) <- dfr[1, , TRUE]
  dfr <- tail(dfr, -1)
  dfr
}

# TODO: Make Excel export contain plain numeric columns instead of a combined Stats/Values column.
# TODO: Remove warnings

# IDEA: des_pairs for variable groups










#' Compute Descriptive Statistics - continuous variables
#'
#' @description
#'
#' generates a descriptive overview of continuous variables (ratio and interval) in `resp_vars`.
#'
#' [Descriptor]
#'
#' @details
#' TODO
#'
#' @inheritParams .template_function_indicator
#'
#' @param resp_vars [variable] the name of the continuous measurement variable
#' @param hard_limits_removal [logical] if TRUE values outside hard limits are
#'                                      removed from the data before calculating
#'                                      descriptive statistics. The default is FALSE
#' @param ... arguments to be passed to all called indicator functions if
#'            applicable.
#'
#' @return a [list] with:
#'   - `SummaryTable`: [data.frame]
#'   - `SummaryData`: [data.frame]
#' @export
#'
#' @examples
#' \dontrun{
#' prep_load_workbook_like_file("meta_data_v2")
#' xx <- des_summary_continuous(study_data = "study_data", meta_data =
#'                               prep_get_data_frame("item_level"))
#' util_html_table(xx$SummaryData)
#' util_html_table(des_summary_continuous(study_data = prep_get_data_frame("study_data"),
#'                    meta_data = prep_get_data_frame("item_level"))$SummaryData)
#' }
#' @seealso
#' [Online Documentation](
#' https://dataquality.qihs.uni-greifswald.de/VIN_des_impl_summary.html
#' )
#' @importFrom graphics barplot hist
#' @importFrom stats mad
#'
des_summary_continuous <- function(resp_vars = NULL, # IDEA: group_vars = NULL, co_vars = NULL,
                                   study_data,
                                   label_col,
                                   item_level = "item_level",
                                   meta_data = item_level,
                                   meta_data_v2,
                                   hard_limits_removal =
                                     getOption("dataquieR.des_summary_hard_lim_remove",
                                               dataquieR.des_summary_hard_lim_remove_default),
                                   ...) {
  # TODO: add figures also to Excel Exports and prevent column type guessing
  # by datatables export correctly
  # TODO: Write vignette

  # preps ----------------------------------------------------------------------
  util_maybe_load_meta_data_v2()
  # map metadata to study data
  util_expect_data_frame(study_data)
  if (inherits(
    try(prep_prepare_dataframes(.replace_hard_limits = hard_limits_removal), silent = TRUE),
    "try-error")) {
    w <- "not all %s found, estimating %s"
    if (suppressWarnings(util_ensure_suggested("cli", err = FALSE))) {
      w <- cli::bg_black(cli::col_yellow(w))
    }
    util_warning(w, sQuote("meta_data"), sQuote("meta_data"),
                 immediate = TRUE)
    meta_data <-
      prep_study2meta(study_data, level = VARATT_REQUIRE_LEVELS$RECOMMENDED)
    prep_prepare_dataframes(.meta_data = meta_data, .replace_hard_limits = hard_limits_removal)
  }

  ds1_factor <-
    prep_prepare_dataframes(.study_data = study_data,
                            .replace_hard_limits = FALSE,
                            .apply_factor_metadata = TRUE,
                            .apply_factor_metadata_inadm = TRUE,
                            .internal = FALSE)

  # define resp_vars
  if (length(resp_vars) == 0) {
    suppress <- function(...) suppressWarnings(suppressMessages(...))
    resp_vars <- colnames(ds1)
  } else {
    suppress <- eval
  }

  #If resp_vars is a var_name and not a LABEL, convert it to LABEL to check intersect
  if(length(intersect(colnames(ds1), resp_vars))==0) {
    resp_vars <- unname(prep_get_labels(resp_vars,
                                       label_col = label_col,
                                       meta_data = meta_data,
                                       resp_vars_are_var_names_only = TRUE))
  }

  resp_vars <- intersect(colnames(ds1), resp_vars)

  suppress(util_correct_variable_use(resp_vars,
                                     allow_more_than_one = TRUE,
                                     #             need_type = "integer|float|datetime",
                                     do_not_stop = TRUE,
                                     need_scale = "!na"))

  # create a named vector of VAR_NAMES with names = LABEL
  study_data_lc <- attr(study_data, "label_col")
  if (is.null(study_data_lc)) {
    study_data_lc <- VAR_NAMES
  }
  varnames <- setNames(nm =
                         prep_map_labels(colnames(study_data),
                                         to = label_col,
                                         from = study_data_lc,
                                         meta_data = meta_data,
                                         ifnotfound = colnames(study_data)),
                       object = colnames(study_data))
  # create a named vector of SEGMENTS with names = VAR_NAMES
  if (STUDY_SEGMENT %in% colnames(meta_data)) {
    segments <- prep_map_labels(varnames,
                                meta_data = meta_data,
                                from = study_data_lc,
                                to = STUDY_SEGMENT,
                                ifnotfound = NA_character_,
                                warn_ambiguous = FALSE)
  } else {
    segments <- NULL
  }


  ## Select only resp_vars that are continuous
  df_continuous <- meta_data[meta_data[, label_col, drop = TRUE] %in%
                               resp_vars, ]
  df_continuous <- df_continuous[df_continuous$SCALE_LEVEL %in%
                                   c("ratio","interval"), ]
  resp_vars <- df_continuous[,label_col, drop = TRUE]
  rm(df_continuous)


  #defining the segments of the resp_vars
  if (!is.null(segments)) {
    sseg <- segments[varnames[resp_vars]]
  } else {
    sseg <- setNames(object = rep("All", length(varnames[resp_vars])),
                     nm = varnames[resp_vars])
  }

  # create a named vector of LONG LABELS (or LABELS or VAR_NAMES)
  # with names = VAR_NAMES
  labs <-
    prep_get_labels(varnames,
                    label_col = label_col,
                    meta_data = meta_data,
                    resp_vars_are_var_names_only = TRUE,
                    label_class = "LONG")

  # create a named vector of DATA_TYPEs with names = VAR_NAMES
  data_types <-
    prep_map_labels(varnames,
                    meta_data = meta_data,
                    to = DATA_TYPE,
                    ifnotfound = NA_character_,
                    warn_ambiguous = FALSE)

  # create a named vector of SCALE_LEVELs with names = VAR_NAMES
  scale_levels <-
    prep_map_labels(varnames,
                    meta_data = meta_data,
                    to = SCALE_LEVEL,
                    from = study_data_lc,
                    ifnotfound = NA_character_,
                    warn_ambiguous = FALSE)


  ## Functions
  ### Measure of central tendency or location parameters
  #function for mean - based on conditions
  c_mean <- function(lb) {
    rv <- varnames[[lb]]
    r <- ""
    mean_value <- mean(ds1[[lb]], na.rm = TRUE)
    if(data_types[[rv]] == "datetime"){

      r <- format(mean_value, usetz = TRUE)
    } else {
      mean_value <- round(mean_value, digits = 3)
      r <- format(mean_value)
    }
    rm(mean_value)
    r
  }


  #function for Median - based on conditions
  c_median <- function(lb) {
    rv <- varnames[[lb]]
    r <- ""
    dt <- ds1[[lb]]
    m <- median(dt, na.rm = TRUE)

    if(data_types[[rv]] == "datetime"){
      r <- paste0(r, format(m, usetz = TRUE))
    } else {
      r <- paste0(r, format(m))
    }
    r
  }



  #function for Mode - based on conditions
  c_mode <- function(lb) {
    rv <- varnames[[lb]]
    r <- ""
    dt <- ds1[[lb]]
    dt <- dt[!is.na(dt)]
    mode_value <-  unique(dt)[tabulate(match(dt, unique(dt))) ==
                                max(tabulate(match(dt, unique(dt))))]
    if(data_types[[rv]] == "datetime"){
      if(length(mode_value)==1) {
        r <- format(mode_value, usetz = TRUE)
      } else if(length(mode_value)==2) {
        mode_value <- format(mode_value, usetz = TRUE)
        mode_value <- paste(mode_value,collapse = " ")
        r <- paste0(r, format(mode_value, usetz = TRUE))
      } else if (length(mode_value)>2) {
        mode_value <- format(mode_value, usetz = TRUE)
        n_mode <- length(mode_value)
        mode_value<- mode_value[1:2]
        n_mode_removed <- n_mode - length(mode_value)
        mode_value <- paste(mode_value,collapse = " ")
        r <- paste0(format(mode_value, usetz = TRUE), " and other ",
                    n_mode_removed ," dates")
      }

    } else {
      if(length(mode_value)==1) {
        r <- format(mode_value)
      } else  if(length(mode_value)==2) {
        mode_value <- paste(mode_value,collapse = " ")
        r <- paste0(r, format(mode_value))
      } else if (length(mode_value)>2){
        n_mode <- length(mode_value)
        mode_value<- mode_value[1:2]
        n_mode_removed <- n_mode - length(mode_value)
        mode_value <- paste(mode_value,collapse = " ")
        r <- paste0(format(mode_value, usetz = TRUE), " and other ",
                    n_mode_removed ," values")
      }

    }
    rm(mode_value)

    r
  }


  #function for SD - based on conditions
  c_SD <- function(lb) {
    rv <- varnames[[lb]]
    r <- ""
    if(data_types[[rv]] == "datetime") {

      r <- format(as.difftime(sd(ds1[[lb]], na.rm = TRUE), #TODO: write an algorithm values > No.secs in year--> yrs..
                              units = 'secs'),usetz = TRUE )

    } else {
      sd_value <- round(sd(na.rm = TRUE, ds1[[lb]]), digits = 4)
      r <- format(sd_value)
      rm(sd_value)
    }
    r
  }




  #function for MAD - based on conditions
  c_MAD <- function(lb) {
    rv <- varnames[[lb]]
    r <- ""

    if(data_types[[rv]] == "datetime"){
      r <- format(mad(ds1[[lb]], na.rm = TRUE), usetz = TRUE)
    } else{
      MAD_value <- round(mad(ds1[[lb]], na.rm = TRUE), digits = 3)
      r <- format(MAD_value)
    }

    r
  }

  #function for Min - Max - based on conditions
  c_min_max <- function(lb) {
    rv <- varnames[[lb]]
    r <- ""
    if(data_types[[rv]] == "datetime"){
      r <- paste0(format(max(ds1[[lb]], na.rm = TRUE)-min(ds1[[lb]],
                                                          na.rm = TRUE),
                         usetz = TRUE),
                  " (", format(min(ds1[[lb]], na.rm = TRUE), usetz = TRUE),
                  " - ",
                  format(max(ds1[[lb]], na.rm = TRUE), usetz = TRUE), ")")
    } else {
      r <- paste0(format(max(ds1[[lb]], na.rm = TRUE)-min(ds1[[lb]],
                                                          na.rm = TRUE)),
                  " (", format(min(ds1[[lb]], na.rm = TRUE)), " - ",
                  format(max(ds1[[lb]], na.rm = TRUE)), ")")
    }
    r
  }

  #function for quantiles and IQR - based on conditions
  c_quantiles <- function(lb) {

    rv <- varnames[[lb]]
    r <- ""

    if(data_types[[rv]] == "datetime"){
      q <- quantile(ds1[[lb]], na.rm = TRUE, names = FALSE)
      names(q) <- paste0("Q", seq_len(length(q)) - 1)
      q <- q[c("Q1", "Q3")]
      r <- prep_deparse_assignments(labels = format(q, usetz = TRUE),
                                    codes = names(q),
                                    mode = "string_codes")
      r<- paste0(format(as.difftime(IQR(na.rm = TRUE, x = ds1[[lb]],
                                        type = 7), units = "secs"), #TODO: write an algorithm values > No.secs in year--> yrs..
                        usetz = TRUE), " (", r, ")")
      rm(q)
    } else {
      q <- quantile(ds1[[lb]], na.rm = TRUE, names = FALSE)  #TODO: check the TYPE
      names(q) <- paste0("Q", seq_len(length(q)) - 1)
      q <- q[c("Q1", "Q3")]
      r <- prep_deparse_assignments(labels = as.character(q),
                                    codes = names(q),
                                    mode = "string_codes")
      r<- paste0(format(IQR(na.rm = TRUE, x = ds1[[lb]], type = 7)),
                 " (", r, ")")  #TODO: check the TYPE
      rm(q)
    }
    r
  }

  #function for CV - based on conditions
  c_CV <- function(lb) {
    rv <- varnames[[lb]]
    r <- ""
    if (data_types[[rv]] %in% c(DATA_TYPES$INTEGER, DATA_TYPES$FLOAT)) {
      CV_value <- sd(ds1[[lb]], na.rm = TRUE) /
        mean(ds1[[lb]], na.rm = TRUE) * 100
      CV_value <- round(CV_value, digits = 4)
      r <- format(CV_value)
      rm(CV_value)
    } else {
      r <- ""
    }

    r
  }


  #function for skewness and SE.skewness - based on conditions
  c_skewness <- function(lb) {
    rv <- varnames[[lb]]
    r <- ""
    if (data_types[[rv]] %in% c(DATA_TYPES$INTEGER, DATA_TYPES$FLOAT)) {
      skewness <- util_compute_skewness(ds1[[lb]])
      SE_skew <- util_compute_SE_skewness(ds1[[lb]], skewness = skewness)
      skewness<- round(skewness, digits = 4)
      SE_skew<- round(SE_skew, digits = 4)
      r <- paste0(format(skewness),
                  " (", format(SE_skew), ")")
      rm(skewness, SE_skew)
    } else {
      r <- ""
    }
    r
  }


  #function for Kurtosis - based on conditions
  c_kurtosis <- function(lb) {
    rv <- varnames[[lb]]
    r <- ""
    if (data_types[[rv]] %in% c(DATA_TYPES$INTEGER, DATA_TYPES$FLOAT)) {
      kurtosis_value <- util_compute_kurtosis(ds1[[lb]])
      kurtosis_value <- round(kurtosis_value, digits = 4)
      r <- format(kurtosis_value)
      rm(kurtosis_value)
    } else {
      r <- ""
    }
    r
  }

  #Define content of col "Variables" with long_label, label, and var_names when different
  if (identical(resp_vars, unname(labs[resp_vars]))) {
    var_name_firstcol <- resp_vars
  } else if (identical(unname(labs[varnames[resp_vars]]),  resp_vars)){
    var_name_firstcol <- paste0(resp_vars,
                                "<br />", varnames[resp_vars])
  } else {
    if (identical(resp_vars, unname(varnames[resp_vars]))){
      var_name_firstcol <- paste0(labs[varnames[resp_vars]],
                                  "<br />", varnames[resp_vars])

    } else {
      var_name_firstcol <- paste0(labs[varnames[resp_vars]], "<br />", resp_vars,
                                  "<br />", varnames[resp_vars])
    }

  }

  if (length(resp_vars) == 0) {
    variable_names_labs <- character(0)
  } else {
    variable_names_labs <-  paste0(scale_levels[varnames[resp_vars]],
                                   ", ",
                                   data_types[varnames[resp_vars]])
  }

  plain_label <- resp_vars

  SummaryData <- data.frame(`Variables`  = util_attach_attr(
    var_name_firstcol,
    plain_label = plain_label),
    `Type` =  variable_names_labs,
    STUDY_SEGMENT = sseg,
    `Mean` =
      vapply(resp_vars, c_mean, character(1)),
    `Median` =
      vapply(resp_vars, c_median, character(1)),
    `Mode` =
      vapply(resp_vars, c_mode, character(1)),
    `SD` =
      vapply(resp_vars, c_SD, character(1)),
    `IQR (Quartiles)` =
      vapply(resp_vars, c_quantiles, character(1)),
    `MAD` =
      vapply(resp_vars, c_MAD, character(1)),
    `Range (Min - Max)` =
      vapply(resp_vars, c_min_max, character(1)),
    `CV` =
      vapply(resp_vars, c_CV, character(1)),
    `Skewness` =
      vapply(resp_vars, c_skewness, character(1)),
    `Kurtosis` =
      vapply(resp_vars, c_kurtosis, character(1)),
    Valid = (if (length(resp_vars) == 0) character(0) else paste0(vapply(resp_vars,
                                                                         function(rv) sum(is.finite(ds1[[rv]])),
                                                                         FUN.VALUE = numeric(1)),
                                                                  "<br />",
                                                                  scales::percent(vapply(resp_vars,
                                                                                         function(rv) sum(is.finite(ds1[[rv]])),
                                                                                         FUN.VALUE =
                                                                                           numeric(1)) / nrow(ds1)))),
    Missing = (if (length(resp_vars) == 0) character(0) else paste0(vapply(resp_vars,
                                                                           function(rv) sum(util_empty(ds1[[rv]])),
                                                                           FUN.VALUE =
                                                                             numeric(1)), "<br />",
                                                                    scales::percent(vapply(resp_vars,
                                                                                           function(rv) sum(util_empty(ds1[[rv]])),
                                                                                           FUN.VALUE =
                                                                                             numeric(1)) / nrow(ds1)))),
    stringsAsFactors = FALSE, check.names = FALSE)

  # if (.called_in_pipeline) {
  #   tb <- meta_data
  #   tb <- tb[tb$VAR_NAMES %in% varnames[resp_vars], ]
  #
  #   list_var_names <- vapply(X =   SummaryData[["Variables"]],
  #                                            FUN = function(x){
  #                                              x <- gsub("<br />", " <br> ", x)
  #                                            },
  #                                            FUN.VALUE =  character(1))
  #
  #
  #
  #   if(label_col == VAR_NAMES || !(LABEL %in% names(tb))){
  #     href <- tb$VAR_NAMES
  #     coltitle <- setNames(nm = tb$VAR_NAMES)
  #     data<-setNames(list_var_names, nm = tb$VAR_NAMES)
  #   } else {
  #     href <- tb$LABEL
  #     coltitle <- setNames(tb$VAR_NAMES, nm = tb$LABEL)
  #     data<-setNames(list_var_names, nm = tb$LABEL)
  #   }
  #   coltitle <- prep_title_escape(coltitle,
  #                                 html = TRUE)
  #
  #  data <- prep_title_escape(data,
  #                             html = TRUE)
  #
  #   href <- paste0("VAR_", prep_link_escape(href,
  #                                           html = TRUE),
  #                  ".html#",
  #                  htmltools::urlEncodePath(as.character(href)))
  #
  #   onclick <- sprintf(
  #     'if (window.hasOwnProperty("dq_report2") && window.dq_report2 && window.location != "%s") { if (all_ids.all_ids.includes("%s")) { window.location = "%s" } else { window.alert("No result avaialable"); } }',
  #     href,
  #     href,
  #     href)
  #
  #   links <- mapply(
  #     href = sprintf('javascript:console.log("%s")', href),
  #     onclick = onclick,
  #     title = coltitle,
  #     filter = data,
  #     data,
  #     SIMPLIFY = FALSE,
  #     FUN = htmltools::a
  #   )
  #
  #
  #
  #
  #
  #   SummaryData[["Variables"]] <- vapply(links,
  #                                            FUN = as.character,
  #                                            FUN.VALUE = character(1))
  #   rm(tb)
  # }

  mrg <- 2

  if (suppressWarnings(util_ensure_suggested("htmltools",
                                             err = FALSE,
                                             goal = "Distribution Plots in Descriptive Summary"))) {
    SummaryData$Graph <- lapply(resp_vars, function(lb) {
      rv <- varnames[[lb]]

      if (data_types[[rv]] %in% c(DATA_TYPES$INTEGER,
                                  DATA_TYPES$FLOAT,
                                  DATA_TYPES$DATETIME)) {
        if (data_types[[rv]] == DATA_TYPES$DATETIME) {
          breaks <- unlist(suppressMessages(util_optimize_histogram_bins(
            x = ds1[[lb]],
            # iqr_bw = IQR(ds1[[lb]], na.rm = TRUE),
            # n_bw = length(ds1[[lb]]),
            # min_plot = min(ds1[[lb]], na.rm = TRUE),
            # max_plot = max(ds1[[lb]], na.rm = TRUE),
            nbins_max = 100
          )))

          breaks <- as.POSIXct(breaks, origin = min(Sys.time(), 0))
          breaks <- unique(sort(breaks))
          breaks <- scales::pretty_breaks()(breaks)
          r <- htmltools::plotTag({
            withr::local_par(list(
              mar = rep(mrg, 4),
              oma = rep(0, 4)
            ))

            hist(ds1[[lb]], main = NULL,
                 xlab = NULL, # TODO: use acc_distributions
                 ylab = NULL, breaks = breaks,
                 freq = TRUE)},
            width = 250,
            height = 200,
            alt = paste("Histogram of", labs[[rv]]))
        } else {
          r <- htmltools::plotTag({
            withr::local_par(list(
              mar = rep(mrg, 4),
              oma = rep(0, 4)
            ))
            hist(ds1[[lb]], main = NULL, xlab = NULL, # TODO: use acc_distributions
                 ylab = NULL)},
            width = 250,
            height = 200,
            alt = paste("Histogram of", labs[[rv]]))
        }
        r <- as.character(r)
      } else {
        r <- ""
      }
      r <- paste0('<div style="min-width: 300px">', r, "</div>")
      r
    })
  } else {
    SummaryData$Graph <- ""
  }

  rownames(SummaryData) <- NULL

  SummaryTable <- SummaryData # TODO: machine readable

  list(SummaryData = util_attach_attr(SummaryData,
                                      is_html_escaped = TRUE), # TODO: plotlist
       SummaryTable = util_attach_attr(SummaryTable,
                                       is_html_escaped = TRUE)) # REVIEW: be sure, that content is html escaped!
}



# des_summary <- function(...) { # TODO: for convenience, offer this function to end users, but do not run in the pipeline
#   util_rbind()
# }






#' Compute Descriptive Statistics
#' @description
#'
#' generates a descriptive overview of the variables in `resp_vars`.
#'
#' [Descriptor]
#'
#' @details
#' TODO
#'
#' @inheritParams .template_function_indicator
#'
#' @param resp_vars [variable] the name of the measurement variable
#' @param hard_limits_removal [logical] if TRUE values outside hard limits are
#'                                      removed from the data before calculating
#'                                      descriptive statistics. The default is FALSE
#' @param ... arguments to be passed to all called indicator functions if
#'            applicable.
#'
#' @return a [list] with:
#'   - `SummaryTable`: [data.frame]
#'   - `SummaryData`: [data.frame]
#' @export
#'
#' @examples
#' \dontrun{
#' prep_load_workbook_like_file("meta_data_v2")
#' xx <- des_summary(study_data = "study_data", meta_data =
#'                    prep_get_data_frame("item_level"))
#' util_html_table(xx$SummaryData)
#' util_html_table(des_summary(study_data = prep_get_data_frame("study_data"),
#'                    meta_data = prep_get_data_frame("item_level"))$SummaryData)
#'
#' }
#' @seealso
#' [Online Documentation](
#' https://dataquality.qihs.uni-greifswald.de/VIN_des_impl_summary.html
#' )
#' @importFrom graphics barplot hist
#' @importFrom stats mad
#'
des_summary <- function(resp_vars = NULL, # IDEA: group_vars = NULL, co_vars = NULL,
                        study_data,
                        label_col,
                        item_level = "item_level",
                        meta_data = item_level,
                        meta_data_v2,
                        hard_limits_removal =
                          getOption("dataquieR.des_summary_hard_lim_remove",
                                    dataquieR.des_summary_hard_lim_remove_default),
                        ...) {
  # preps ----------------------------------------------------------------------
  util_maybe_load_meta_data_v2()
  # map metadata to study data
  util_expect_data_frame(study_data)
  if (inherits(
    try(prep_prepare_dataframes(.replace_hard_limits = hard_limits_removal), silent = TRUE),
    "try-error")) {
    w <- "not all %s found, estimating %s"
    if (suppressWarnings(util_ensure_suggested("cli", err = FALSE))) {
      w <- cli::bg_black(cli::col_yellow(w))
    }
    util_warning(w, sQuote("meta_data"), sQuote("meta_data"),
                 immediate = TRUE)
    meta_data <-
      prep_study2meta(study_data, level = VARATT_REQUIRE_LEVELS$RECOMMENDED)
    prep_prepare_dataframes(.meta_data = meta_data, .replace_hard_limits = hard_limits_removal)
  }

  # if there is VARIABLE_ORDER in metadata use that order, otherwise use appearance order
  if(VARIABLE_ORDER %in% colnames(meta_data)){ #if there is VARIABLE_ORDER
    if (!(length(resp_vars) == 0)) {
      if (all(resp_vars %in% meta_data$VAR_NAMES)){
        resp_vars_original_order <-
          meta_data[meta_data$VAR_NAMES %in% resp_vars,
                    c(VAR_NAMES, "VARIABLE_ORDER")]
      } else {
        resp_vars <- util_map_labels(resp_vars, to = VAR_NAMES, from = label_col)
        resp_vars_original_order <-
          meta_data[meta_data$VAR_NAMES %in% resp_vars ,
                    c(VAR_NAMES, "VARIABLE_ORDER")]
      }
    } else {
      resp_vars_original_order <-
        meta_data[meta_data$VAR_NAMES %in% names(study_data),
                  c(VAR_NAMES, "VARIABLE_ORDER")]
    }
  } else { #if there is no VARIABLE_ORDER
    # assign a progressive number to VARIABLE_ORDER the variables (as listed in the argument
    # or as present in the study_data)
    if (!(length(resp_vars) == 0)) {
      resp_vars_original_order <- resp_vars
      resp_vars_original_order <- data.frame(VAR_NAMES = resp_vars_original_order,
                                             VARIABLE_ORDER = seq_len(length(resp_vars_original_order)))
    } else {
      util_expect_data_frame(study_data)
      resp_vars_original_order <- names(study_data)
      resp_vars_original_order <- data.frame(VAR_NAMES = resp_vars_original_order,
                                             VARIABLE_ORDER = seq_len(length(resp_vars_original_order)))
    }
  }

  with_pipeline({
      # Calculate descriptive statistics for categorical variables
      categ_res <- des_summary_categorical(resp_vars = resp_vars,
                                           item_level = meta_data,
                                           label_col = label_col,
                                           study_data = study_data,
                                           meta_data_v2 = meta_data_v2, ...)

      # Calculate descriptive statistics for continuous variables
      contin_res <- des_summary_continuous(resp_vars = resp_vars,
                                           item_level = meta_data,
                                           label_col = label_col,
                                           study_data = study_data,
                                           meta_data_v2 = meta_data_v2, ...)
    }
  )

  # If one of the 2 descriptive statistics is empty
  if (length(categ_res) == 0) {
    SummaryData <- contin_res$SummaryData
    SummaryTable <- contin_res$SummaryTable
  } else if (length(contin_res) == 0) {
    SummaryData <- categ_res$SummaryData
    SummaryTable <- categ_res$SummaryTable
  } else {  #If both descriptive statistics contain results
    # merge results of categorical and continuous variables
    SummaryData_1 <- merge(categ_res$SummaryData,
                           contin_res$SummaryData,
                           by = (intersect(colnames(categ_res$SummaryData),
                                           colnames(contin_res$SummaryData))),
                           all = TRUE)

    # Obtain var_names from Variables columns and merge with vectors of original
    # variables with order
    #From https://stackoverflow.com/questions/57908854/can-i-extract-strings-that-contain-any-of-a-vector-of-strings-in-r
    ind <- which(outer(resp_vars_original_order$VAR_NAMES,
                       SummaryData_1$Variables,
                       Vectorize(grepl)),
                 arr.ind = T)

    SummaryData_1$VAR_NAMES <- resp_vars_original_order$VAR_NAMES[ind[,1]]
    SummaryData_1 <- merge(SummaryData_1,
                           resp_vars_original_order,
                           by = "VAR_NAMES",
                           all.x = TRUE)
    SummaryData_1$VARIABLE_ORDER <- as.numeric(SummaryData_1$VARIABLE_ORDER)
    SummaryData_1 <- SummaryData_1[order(SummaryData_1$VARIABLE_ORDER),]

    #Reorganize columns in SummaryData
    SummaryData <- SummaryData_1[, c("Variables", "Type")]
    if("STUDY_SEGMENT" %in% colnames(SummaryData_1)){
      SummaryData$STUDY_SEGMENT <- SummaryData_1$STUDY_SEGMENT
    }
    SummaryData <- cbind(SummaryData, SummaryData_1[, c("Mean", "Median", "Mode",
                                                        "SD", "IQR (Quartiles)", "MAD",
                                                        "Range (Min - Max)", "CV",
                                                        "Skewness" ,"Kurtosis",
                                                        "No. categories/Freq. table",
                                                        "Valid", "Missing",
                                                        "Graph"    )])


    # merge results of categorical and continuous variables
    SummaryTable <- merge(categ_res$SummaryTable,
                          contin_res$SummaryTable,
                          by = (intersect(colnames(categ_res$SummaryTable),
                                          colnames(contin_res$SummaryTable))),
                          all = TRUE)

    # Obtain var_names from Variables columns and merge with vectors of original
    # variables with order
    #From https://stackoverflow.com/questions/57908854/can-i-extract-strings-that-contain-any-of-a-vector-of-strings-in-r
    ind <- which(outer(resp_vars_original_order$VAR_NAMES,
                       SummaryTable$Variables,
                       Vectorize(grepl)),
                 arr.ind = T)

    SummaryTable$VAR_NAMES <- resp_vars_original_order$VAR_NAMES[ind[,1]]
    SummaryTable <- merge(SummaryTable,
                          resp_vars_original_order,
                          by = "VAR_NAMES",
                          all.x = TRUE)
    SummaryTable$VARIABLE_ORDER <- as.numeric(SummaryTable$VARIABLE_ORDER)
    SummaryTable <- SummaryTable[order(SummaryTable$VARIABLE_ORDER),]

    # clean environment
    rm(ind, SummaryData_1, categ_res, contin_res, resp_vars_original_order)
  }


  list(SummaryData = util_attach_attr(SummaryData,
                                      is_html_escaped = TRUE), # TODO: plotlist
       SummaryTable = util_attach_attr(SummaryTable,
                                       is_html_escaped = TRUE)) # REVIEW: be sure, that content is html escaped!

}










