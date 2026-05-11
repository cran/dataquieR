#' Display a sunburst chart
#'
#' @param repsum a report summary
#' @param remove_lines display lines?
#' @param ex exponent to steepen color gradient
#' @param folder_of_report a named vector with the location of variable and
#'                         `call_names`
#' @param var_uniquenames a data frame with the original variable names and the
#'                        unique names in case of reports created with dq_report_by
#'                        containing the same variable in several reports
#'                        (e.g., creation of reports by sex)
#'
#' @returns a `plotly` object
#' @noRd
util_render_sunburst_from_summary_classes <- function(repsum,
                                                      remove_lines,
                                                      ex = 5,
                                                      folder_of_report = NULL,
                                                      var_uniquenames = NULL) {

  util_ensure_suggested(pkg = "plotly", goal = "generate sunburst plot")
  util_ensure_suggested(pkg = "htmlwidgets", goal = "generate sunburst plot")
  util_ensure_suggested(pkg = "rmarkdown", goal = "generate sunburst plot")

  # NSE variables (for R CMD check): avoid "no visible binding" notes
  Dimension <- Domain <- Name <- Parent_Element_ID <- IndicatorID <-
    abbreviation <- Level <- Indicator <- class <- LABEL_use <- max.class <-
    severity_num <- mean_class_within_name <- mean_class_within_dom <-
    mean_class_within_dim <- color <- id <- parent <- path <- L1 <-
    L2 <- L3 <- L4 <- severity_mix <- n <- font_size <- id_match <-
    children <- child_vals <- n_leaves_dim <- n_leaves_dom <-
    href <- popup_href <- title <- n_leaves_name <- NULL


  this <- attr(repsum, "this")

  if (missing(remove_lines)) {
    remove_lines <- length(unique(this$colnames_of_report)) *
      length(unique(this$rownames_of_report)) > 1000 # TODO: find empirically a reasonable default.
  }

  repsum2 <- util_dashboard_table(repsum, folder_of_report = folder_of_report)
  is_by <- !is.null(folder_of_report)

  repsum2$LABEL_use <-
    prep_get_labels(repsum2$VAR_NAMES,item_level = this$meta_data,
                    label_col = this$label_col, max_len = 40, label_class = "SHORT",
                    resp_vars_are_var_names_only = TRUE, resp_vars_match_label_col_only = FALSE)

  dqi <- util_get_concept_info("dqi")

  dqi$Indicator = dqi$Name
  dqi$Indicator[dqi$Level != 3] <- NA

  cols_dqi <- c("Dimension","Domain","Name","Parent_Element_ID","IndicatorID","abbreviation","Level","Indicator")

  dqi <-
    dqi %>%
    dplyr::select(dplyr::all_of(cols_dqi)) %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(cols_dqi)), .keep_all = FALSE)


  repsum2$indicator_metric <- sub("^[^_]+_", "", repsum2$indicator_metric)

  df <- dplyr::left_join(dqi, repsum2, by = c("abbreviation" = "indicator_metric"))

  sunburst_df <-
    df %>%
    dplyr::select("Parent_Element_ID",
                  "IndicatorID" ,
                  "LABEL_use" ,
                  "Dimension",
                  "Domain",
                  "Name",
                  "class",
                  "Level",
                  "href",
                  "popup_href",
                  "title")

  dqi_3 <-
    dqi %>%
    dplyr::filter(get("Level") == 3) %>%
    dplyr::select("Dimension",
                  "Domain",
                  "Name",
                  "Level") %>%
    dplyr::mutate("LABEL_use" = NA,
                  "max.class" = NA)

  cols_sunburst_df <- c("Dimension", "Domain", "Name", "LABEL_use", "Level", "href", "popup_href", "title")

  sunburst_df <-
    sunburst_df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(cols_sunburst_df))) %>%
    dplyr::mutate("Level" = dplyr::case_when(
      !is.na(get("LABEL_use")) ~ 4,
      TRUE ~ get("Level")
    )) %>%
    dplyr::summarize("max.class" = suppressWarnings(max(get("class"), na.rm = TRUE))) %>%
    dplyr::select("Dimension", "Domain", "Name", "LABEL_use", "Level", "max.class", "href", "popup_href", "title") %>%
    dplyr::filter(get("Level") != 3)

  sunburst_df <- dplyr::bind_rows(dqi_3, sunburst_df)

  # Define a severity → numeric scale
  sunburst_df$severity_num <- as.numeric(sunburst_df$max.class) # now, we have numbers according to the level order in the ordered class factor

  # Level 3 = Mean der Leaves
  sunburst_df <- sunburst_df %>%
    dplyr::group_by(Dimension, Domain, Name) %>%
    dplyr::mutate(
      mean_class_within_name = mean(severity_num[Level == 4], na.rm = TRUE),
      severity_num = ifelse(Level == 3, mean_class_within_name, severity_num)
    ) %>%
    dplyr::ungroup()

  # Level 2
  sunburst_df <- sunburst_df %>%
    dplyr::group_by(Dimension, Domain) %>%
    dplyr::mutate(
      mean_class_within_dom = mean(severity_num[Level == 4], na.rm = TRUE),
      severity_num = ifelse(Level == 2, mean_class_within_dom, severity_num)
    ) %>%
    dplyr::ungroup()

  # Level 1
  sunburst_df <- sunburst_df %>%
    dplyr::group_by(Dimension) %>%
    dplyr::mutate(
      mean_class_within_dim = mean(severity_num[Level == 4], na.rm = TRUE),
      severity_num = ifelse(Level == 1, mean_class_within_dim, severity_num)
    ) %>%
    dplyr::ungroup()

  sunburst_df$LABEL_use <-
    vapply(sunburst_df$LABEL_use, function(x) {
      if (is.null(x)) NA_character_ else as.character(x)
    }, FUN.VALUE = character(1))

  sunburst_df <-
    sunburst_df %>%
    dplyr::mutate("LABEL_use" = dplyr::case_when(
      get("Level") == 1 ~ get("Name"),
      get("Level") == 2 ~ get("Name"),
      get("Level") == 3 ~ get("Name"),
      TRUE ~ get("LABEL_use")
    ))

  # Create an ID and parent relationship for sunburst
  sunburst_full <- sunburst_df %>%
    dplyr::filter(!is.na(get("severity_num"))) %>%
    dplyr::mutate(
      "id" = dplyr::case_when( # ids must be allowed anchor names in a URL
        get("Level") == 1 ~ get("Dimension"),
        get("Level") == 2 ~ paste(get("Dimension"), get("Domain"), sep = " - "),
        get("Level") == 3 ~ paste(get("Dimension"), get("Domain"), get("Name"), sep = " - "),
        get("Level") == 4 ~ paste(get("Dimension"), get("Domain"), get("Name"), get("LABEL_use"), sep = " - ")
      ),
      "parent" = dplyr::case_when(
        get("Level") == 1 ~ "",
        get("Level") == 2 ~ get("Dimension"),
        get("Level") == 3 ~ paste(get("Dimension"), get("Domain"), sep = " - "),
        get("Level") == 4 ~ paste(get("Dimension"), get("Domain"), get("Name"), sep = " - ")

      )
    )  %>%
    # ---- leaf counts (descendant leaves); leaves are Level==4 ----
  dplyr::left_join(
    dplyr::filter(., get("Level") == 4) %>%
      dplyr::count(Dimension, name = "n_leaves_dim"),
    by = "Dimension"
  ) %>%
    dplyr::left_join(
      dplyr::filter(., get("Level") == 4) %>%
        dplyr::count(Dimension, Domain, name = "n_leaves_dom"),
      by = c("Dimension", "Domain")
    ) %>%
    dplyr::left_join(
      dplyr::filter(., get("Level") == 4) %>%
        dplyr::count(Dimension, Domain, Name, name = "n_leaves_name"),
      by = c("Dimension", "Domain", "Name")
    ) %>%
    dplyr::mutate(
      n_leaves = dplyr::case_when(
        get("Level") == 1 ~ as.integer(n_leaves_dim),
        get("Level") == 2 ~ as.integer(n_leaves_dom),
        get("Level") == 3 ~ as.integer(n_leaves_name),
        get("Level") == 4 ~ 1L,
        TRUE ~ NA_integer_
      )
    ) %>%
    dplyr::select(-n_leaves_dim, -n_leaves_dom, -n_leaves_name)

  line_cfg <- if (remove_lines) {
    list(width = 0, color = 'rgba(0,0,0,0)')
  } else {
    list(width = 1, color = 'white') # Standard: white seperator lines
  }

  min_font <- 12
  max_font <- 30

  sunburst_full <- sunburst_full %>%
    dplyr::mutate(
      # scale based on severity like in a word-cloud
      font_size = min_font + (severity_num - 1) / 4 * (max_font - min_font)
    )

  sunburst_full$font_size[is.na(sunburst_full$font_size)] <- min_font

  sunburst_full <- sunburst_full %>%
    dplyr::mutate(
      customdata = Map(
        function(href, popup_href, title, severity_num) {
          list(
            href = href,
            popup_href = popup_href,
            title = title,
            severity_num = severity_num
          )
        },
        href,
        popup_href,
        title,
        severity_num
      )
    )

  # ---------------------------
  # Plot
  # ---------------------------
  py <- plotly::plot_ly(
    data = sunburst_full,
    ids = ~ id,
    labels = ~ LABEL_use,
    parents = ~ parent,
    values = ~ severity_num * n_leaves,
    customdata = ~ customdata,
    type = 'sunburst',
    branchvalues = 'total',
    textinfo = "label",
    textfont = list(size = ~ font_size),
    marker = list(
      colors = ~ 1 - (( 1 - ((severity_num - 1)/4)) ^ ex),
      line = line_cfg,
      # colorscale = "Bluered",
      # colorscale = "RdBu",
      # colorscale = list(
      #   list(1.0, "#f2f2f2"),
      #   list(2.0, "#d9d9d9"),
      #   list(3.0, "#f4c7c3"),
      #   list(4.0, "#e57368"),
      #   list(5.0, "#c43c39")
      # ),
      colorscale = lapply(mapply(SIMPLIFY = FALSE, nm = 1.0 * (1:5-1)/4, cl = util_get_colors(), list), unname),
      cmin = 0,
      cmax = 1,
      showscale = FALSE
    )
  )

  py <- htmlwidgets::onRender(
    py,
    "sunburst_on_render",
    data = list(ex_init = ex)
  )

  py$height <- NULL
  py$width  <- NULL
  # py <- plotly::config(py, responsive = TRUE)

  py <- plotly::layout(py,
                       autosize = is_by
  )

  py <- plotly::config(py, displaylogo = FALSE)

  py <- util_decorate_plotly(py)

  py <- util_plotly_add_modebar_buttons(py,
                   list(
                     list(
                       name = "Back to root",
                       icon = htmlwidgets::JS("Plotly.Icons.home"),
                       click = htmlwidgets::JS("Plotly_dq_go_root")
                      )
                   )
  )

  py <- util_plotly_remove_modebar_button_added(py, "Restore initial Size")

  py <- util_plotly_modebar_right(py, 50)

  py <- plotly::layout(py,
                       margin = list(r = 40))

  py$sizingPolicy$defaultHeight <- "100%"

  if (.called_in_pipeline) {
    dummy <- htmltools::tagList()
  } else {
    dummy <-
      htmltools::tagList(htmltools::div(class = "navbar"),
                         htmltools::div(class = "content"),
                         htmltools::tags$script(type = "text/javascript",
                                                "window.dq_report2 = true")
      )
  }

  if (is_by) {
    py <- htmltools::tagList(dummy,
                             htmltools::div(
                               style="width:100vw;height:100vh",
                               `data-tippy-always-on` = "true",
                               py
                             )
    )
  } else {
    py <- htmltools::tagList(dummy,
                             htmltools::div(
                               style="width:100vw;height:95vh;overflow:hidden;",
                               `data-tippy-always-on` = "true",
                               py
                             )
    )
  }


  if (!.called_in_pipeline) {
    py <- htmltools::tagList(rmarkdown::html_dependency_jquery(),
                           html_dependency_clipboard(),
                           html_dependency_tippy(),
                           html_dependency_dataquieR(iframe = FALSE),
                           py)
  }

  py <- htmltools::browsable(py)

  py

}

#' Force `Plotly` `modebar` to the top-right (`htmlwidgets`)
#'
#' Some CSS setups or late resizes can make `Plotly` place the `modebar` (buttons)
#' on the left. This helper pins it to the right via a widget-local JS hook.
#'
#' @param p A `plotly` `htmlwidget`.
#' @param right_px Inset from the right edge (CSS px).

#' @return The modified widget.
#' @noRd
util_plotly_modebar_right <- function(p, right_px = 16) {
  stopifnot(is.numeric(right_px), length(right_px) == 1L, is.finite(right_px))

  htmlwidgets::onRender(
    p,
    sprintf("
function(el, x) {
  function pin() {
    var mbs = el.querySelectorAll('.modebar, .modebar-container');
    for (var i = 0; i < mbs.length; i++) {
      mbs[i].style.left  = 'auto';
      mbs[i].style.right = '%spx';
    }
  }

  pin();

  if (typeof Plotly !== 'undefined' && el && typeof el.on === 'function') {
    el.on('plotly_afterplot', pin);
    el.on('plotly_relayout',  pin);
  }

  requestAnimationFrame(pin);
  setTimeout(pin, 0);
  setTimeout(pin, 50);
}
", as.integer(round(right_px)))
  )
}

util_plotly_remove_modebar_button_added <- function(p, name) {
  cfg <- p$x$config
  mba <- cfg$modeBarButtonsToAdd
  if (is.null(mba) || !length(mba)) return(p)

  cfg$modeBarButtonsToAdd <- Filter(
    function(btn) {
      # keep unknown shapes by default
      if (is.list(btn)) {
        # custom buttons are lists with $name
        return(is.null(btn$name) || !identical(btn$name, name))
      }
      # atomic entries (sometimes characters) -> drop only if equal
      if (is.atomic(btn) && length(btn) == 1L) {
        return(!identical(as.character(btn), name))
      }
      TRUE
    },
    mba
  )

  p$x$config <- cfg
  p
}
# Nur AGE_0 wird organge?
