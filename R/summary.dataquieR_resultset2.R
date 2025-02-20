#' Generate a report summary table
#'
#' @param object a square result set
#' @param aspect an aspect/problem category of results
#' @param FUN function to apply to the cells of the result table
#' @param ... not used
#' @param collapse passed to `FUN`
#'
#' @return a summary of a `dataquieR` report
#' @export
#' @examples
#' \dontrun{
#'   util_html_table(summary(report),
#'        filter = "top", options = list(scrollCollapse = TRUE, scrollY = "75vh"),
#'        is_matrix_table = TRUE, rotate_headers = TRUE, output_format = "HTML"
#'   )
#' }
summary.dataquieR_resultset2 <- function(object, aspect = c("applicability", "error", "anamat", "indicator_or_descriptor"),
                                   FUN,
                                   collapse = "\n<br />\n",
                                   ...) {
  my_storr_object <- util_get_storr_object_from_report(object)
  # FIXME: Go only once ofer all results
  # TODO: depending on the back-end, do not access the single results but load from the summary namespace
  if (!is.null(attr(object, "repsum")) &&
      identical(rlang::call_args_names(rlang::call_match()), "object") &&
      identical(attr(attr(object, "repsum"), "rule_digest"),
                rlang::hash(list(util_get_rule_sets(),
                                 util_get_ruleset_formats())))
      ) {
    return(attr(object, "repsum"))
  }
  if (missing(FUN)) {# TODO: I need this long-format overall dq summary data frame also elsewhere
    util_stop_if_not("aspect is only supported for specific FUN values" =
                       missing(aspect))

    labels_of_var_names_in_report <- setNames(
      attr(object, "meta_data")[[attr(object, "label_col")]],
      nm = attr(object, "meta_data")[[VAR_NAMES]]
    )

    var_names_of_labels_in_report <- setNames(
      nm = attr(object, "meta_data")[[attr(object, "label_col")]],
      attr(object, "meta_data")[[VAR_NAMES]]
    )

    labels <- util_get_labels_grading_class()
    colors <- util_get_colors()
    names(labels) <- paste0("cat", names(labels))
    names(colors) <- paste0("cat", names(colors))
    if (!identical(names(labels), names(colors)) ||
        !all(paste0("cat", seq_len(5)) %in% names(colors))) {
      util_warning(
        c("Could not find enough categories in custom format file,",
          "falling back to default"), applicability_problem = TRUE,
        intrinsic_applicability_problem = FALSE)
      old_o <- options(
        dataquieR.grading_formats =
          system.file("grading_formats.xlsx",
                      package = "dataquieR"),
        dataquieR.grading_rulesets =
          system.file("grading_rulesets.xlsx",
                      package = "dataquieR"))
      on.exit(options(old_o))
      labels <- util_get_labels_grading_class()
      colors <- util_get_colors()
      names(labels) <- paste0("cat", names(labels))
      names(colors) <- paste0("cat", names(colors))
    }

    labels <- as.list(labels)
    colors <- as.list(colors)

    labels[["catNA"]] <- htmltools::HTML("\u00A0") # this is nbsp, but it reads " ", even in the HTML source code
    labels[["NA"]] <- htmltools::HTML("\u00A0")
    colors[["catNA"]] <- "#ffffff00" # transparent / white
    colors[["NA"]] <- "#ffffff00" # transparent / white

    icons <- lapply(labels, function(x) htmltools::HTML("\u00A0")) # not available, yet

    order_of <- suppressWarnings(setNames(as.integer(gsub("^cat", "",
                              util_as_cat(names(colors)))), nm = names(colors)))

    filter_of <- labels

    if (!is.null(my_storr_object)) {
      namespace = util_get_storr_summ_namespace(my_storr_object)
      all_sums <- my_storr_object$mget(my_storr_object$list(
        namespace = namespace),
                          namespace = namespace)
    } else {
      all_sums <- lapply(object, attr, "r_summary")
    }

    result <- util_rbind(data_frames_list = all_sums)

    if (any(is.na(result$n_classes))) {
      result[is.na(result$n_classes), "n_classes"] <- 5
    }

    ordered_call_names <- colnames(object)
    ordered_var_names <- var_names_of_labels_in_report[rownames(object)]


    # spreaded_result <- lapply(
    #   setNames(nm = intersect(ordered_call_names, result$call_names)),
    #   function(cn) vapply(
    #     FUN.VALUE = character(1),
    #     setNames(nm = intersect(ordered_var_names, result$VAR_NAMES)),
    #     util_get_html_cell_for_result,
    #     cn = cn,
    #     result = result,
    #     icons = icons,
    #     labels = labels,
    #     colors = colors,
    #     order_of = order_of,
    #     filter_of = filter_of,
    #     labels_of_var_names_in_report = labels_of_var_names_in_report,
    #     var_names_of_labels_in_report = var_names_of_labels_in_report
    #   )
    # )
    # spreaded_result_df <- do.call(data.frame, spreaded_result)
    # Variables <-
    #   labels_of_var_names_in_report[rownames(spreaded_result_df)]
    # spreaded_result_df <- cbind(
    #   data.frame(Variables = Variables), spreaded_result_df)
    # rownames(spreaded_result_df) <- NULL

    indicator_metric <- NULL

    if (!prod(dim(result))) {
      cls <-
        c("VAR_NAMES", "class", "indicator_metric", "value", "values_raw",
          "n_classes", "STUDY_SEGMENT", "call_names", "function_name")
      result <-
        as.data.frame(setNames(rep(list(character(0)), length(cls)), nm = cls))
    }

    if (!"class" %in% colnames(result)) {
      result$class <- rep(NA, nrow(result))
    }

#    result$class <- 1 # fixme: this needs to be replaced by the interval code R/prep_summary_to_classes.R
#    result$class[is.na(result$class)] <- "NA"
    result$class <- util_as_cat(result$class)

    result <- result[!is.na(result[[VAR_NAMES]]), , FALSE] # remove all non-variable-related stuff, not yet supported, here.

    result %>% dplyr::filter(!startsWith(as.character(indicator_metric), "CAT_") &
                               !startsWith(as.character(indicator_metric), "MSG_")) %>%
      dplyr::group_by(VAR_NAMES) %>%
      dplyr::summarise(rowmax = suppressWarnings(
        max(util_as_cat(class), na.rm = TRUE))) ->
      rowmaxes

    rowmax <- paste(rowmaxes$rowmax) # also make NA -> "NA"

    rowmaxes$label <- vapply(labels[rowmax], identity,
                             FUN.VALUE = character(1))

    rowmaxes$color <- vapply(colors[rowmax], identity,
                             FUN.VALUE = character(1))

    rowmaxes$order <- vapply(order_of[rowmax], identity,
                             FUN.VALUE = integer(1))

    rowmaxes$filter <- vapply(filter_of[rowmax], identity,
                             FUN.VALUE = character(1))

    get_cell_text <- function(lb, cl, o, f, vn) {
      # util_message("lb = %s, cl = %s, o = %s, f = %s, vn = %s",
      #              sQuote(lb), sQuote(cl), sQuote(o), sQuote(f), sQuote(vn))
      link <- util_generate_anchor_link(labels_of_var_names_in_report[[vn]],
                                        "",
                                        title =
                                          htmltools::HTML(as.character(lb)))
      link$attribs$style <- c(link$attribs$style,
                              "text-decoration:none;display:block;")
      fg_color <- util_get_fg_color(cl)
      link$attribs$style <- c(link$attribs$style, sprintf("color:%s;", fg_color))

      paste0(
        htmltools::tagList(
        # htmltools::p(cn),
        # htmltools::p(vn)
        htmltools::pre(
          onclick = "",
          style = sprintf("height: 100%%; min-height: 2em; margin: 0em; padding: 0em; background: %s; cursor: pointer; text-align: center;", cl),
          sort = o,
          filter = f,
          title = "",
          link)
      ), collapse = "\n")
    }

    rowmaxes$cell_text <- setNames(vapply(mapply(
      SIMPLIFY = FALSE,
      FUN = get_cell_text,
      lb = rowmaxes$label,
      cl = rowmaxes$color,
      o = rowmaxes$order,
      f = rowmaxes$filter,
      vn = rowmaxes$VAR_NAMES
    ),
    FUN = identity,
    FUN.VALUE = character(1)),
    nm = labels_of_var_names_in_report[rowmaxes$VAR_NAMES])


    # compute stopped_functions
    stopped_functions <- vapply(object,
                                inherits, what = "dataquieR_NULL",
                                FUN.VALUE = logical(1) )





    # spreaded_result_df$Total <- rowmaxes$cell_text[
    #   spreaded_result_df$Variables]
    #
    # class(spreaded_result_df) <- c("dataquieR_summary", "data.frame")

    spreaded_result_df <- NA
    class(spreaded_result_df) <- c("dataquieR_summary")


    matrix_list <- attr(object, "matrix_list")
    function_alias_map <- attr(matrix_list, "function_alias_map")
    function_alias_map <- unique(function_alias_map[ , c("name", "alias"), drop = FALSE])
    alias_names <- setNames(function_alias_map$name, nm = function_alias_map$alias)

    # FIXME: avoid to do this already once, before
    result <- util_metrics_to_classes(result,
                                      attr(object, "meta_data")) # re-classify

    this <- new.env(parent = emptyenv())

    for (prop in c("result", # long format of the summary, column indicator_metric contains CAT_ and MSG_, which are special process classes
                   "rowmaxes", # maximum category for each row in the summary table
                   # and all the ellipsis_arguments from
                   # util_get_html_cell_for_result, above:
                   "labels_of_var_names_in_report",
                   "alias_names",
                   "stopped_functions",
                   "colors",
                   "labels",
                   "order_of",
                   "filter_of",
                   "ordered_call_names",
                   "ordered_var_names"
    )) {
      this[[prop]] <- get(prop)
    }

    # needed for the summary downlaod button
    this[["title"]] <- attr(object, "title")
    this[["subtitle"]] <- attr(object, "subtitle")

    # needed to pretty print
    this[["meta_data"]] <- attr(object, "meta_data")

    # "label_col", # needed to pretty print
    this[["label_col"]] <- attr(object, "label_col")

    # needed for pretty plot
    this[["rownames_of_report"]] <- rownames(object)
    this[["colnames_of_report"]] <- colnames(object)

    attr(spreaded_result_df, "this") <- this
    attr(spreaded_result_df, "rule_digest") <-
              rlang::hash(list(util_get_rule_sets(),
                               util_get_ruleset_formats()))
    spreaded_result_df

  } else {
    f <- substitute(FUN)
    FUN <- force(eval(f, enclos = parent.frame(), envir = environment()))
    util_ensure_suggested("htmltools", "Generating nice tables")
    util_stop_if_not(inherits(object, "dataquieR_resultset2"))
    aspect <- util_match_arg(aspect, several_ok = FALSE)

    rn_obj <- rownames(object)
    cn_obj <- colnames(object)

    rn_to_use <- vapply(rn_obj, function(rn) {
      any(vapply(object[rn, , drop = TRUE],
                 inherits,
                 "dataquieR_result",
                 FUN.VALUE = logical(1)))
    }, FUN.VALUE = logical(1))

    cn_to_use <-
      vapply(cn_obj, function(cn) {
        # TODO: check entity attribute instead and omit outputs with entity != item
        x <- object[, cn, drop = FALSE]
        length(x) > 0 &&
          any(!endsWith(names(x), ".[ALL]")) &&
          all(vapply(names(x), function(listname) {
            any(endsWith(listname, paste0(".", rownames(object))))
          }, FUN.VALUE = logical(1)))
      }, FUN.VALUE = logical(1))

    rn_obj <- rn_obj[rn_to_use]
    cn_obj <- cn_obj[cn_to_use]

    if (!any(rn_to_use) || !any(cn_to_use)) {
      return(
        matrix(nrow = 0, ncol = 2,
               dimnames = list(c(), c(VAR_NAMES, STUDY_SEGMENT)))
      )
    }

    do.call(rbind, lapply(setNames(nm = rn_obj),
                          FUN = function(rn) {
                            vapply(setNames(nm = cn_obj),
                                   FUN.VALUE = character(1),
                                   FUN = function(cn, aspect, collapse) {
                                     r <- FUN(object[rn, cn, drop = TRUE],
                                              aspect = aspect,
                                              collapse = collapse,
                                              rn = rn,
                                              cn = cn)
                                     # if (inherits(r, "try-error")) {
                                     #   debug(FUN)
                                     #   r <- FUN(object[rn, cn, drop = TRUE],
                                     #            aspect = aspect,
                                     #            collapse = collapse,
                                     #            rn = rn,
                                     #            cn = cn)
                                     # }
                                     # if (!is.character(r) || length(r) != 1)
                                     r
                                   },
                                   aspect = aspect,
                                   collapse = collapse)
                          }
    )) # TOOD: "Any-Issue" Column
  }
}


util_reclassify_dataquieR_summary <- function(x) {
  util_stop_if_not(inherits(x, "dataquieR_summary"))

  if (identical(attr(x, "rule_digest"),
            rlang::hash(list(util_get_rule_sets(),
                             util_get_ruleset_formats()))))
    return(x)

  rs_table_long <- attr(x, "this")$result
  meta_data <- attr(x, "this")$meta_data

  attr(x, "this")$result <-
    util_metrics_to_classes(rs_table_long, meta_data)

  attr(x, "rule_digest") <-
    rlang::hash(list(util_get_rule_sets(),
                     util_get_ruleset_formats()))

  x
}

