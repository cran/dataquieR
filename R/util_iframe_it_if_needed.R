#' Make `it` scalable, if it is a figure (parallel-safe version)
#'
#' This version avoids writing to disk in parallel execution.
#' Instead, it attaches metadata for later serialization.
#'
#' @param it [htmltools::tag()] compatible object
#' @param dir [character] output directory for potential `iframe`.
#' @param nm [character] name for the `iframe`d file, if one is created
#' @param fkt [character] function name of the indicator function that created `it`.
#' @param sizing_hints `object` additional metadata about the natural figure size
#' @param ggthumb [ggplot2::ggplot()] optional preview plot
#'
#' @return [htmltools::tagList()] tagged object with metadata for later rendering
#' @noRd
util_iframe_it_if_needed <- function(it, dir, nm, fkt, sizing_hints, ggthumb) {
  functionName <- fkt

  if (!isTRUE(getOption('knitr.in.progress')) &&
      (inherits(it, "plotly") ||
       (inherits(it, "shiny.tag") && it$name == "img"))) {

    util_expect_scalar(dir, check_type = is.character)
    util_expect_scalar(nm, check_type = is.character)
    util_stop_if_not(dir.exists(dir))

    if (!nzchar(paste0("", nm))) {
      fig_framed_file <- tempfile(pattern = "FIG_", fileext = ".html", tmpdir = dir)
      fig_framed_file <- basename(fig_framed_file)
    } else {
      fig_framed_file <- paste0("FIG_", prep_link_escape(nm, html = TRUE), ".html")
    }

    full_fig_file <- file.path(dir, fig_framed_file)
    fig_framed_thumb_file <- gsub("\\.html$", ".png", fig_framed_file)
    thumb_file <- file.path(dir, fig_framed_thumb_file)

    sizing_hints <- util_finalize_sizing_hints(sizing_hints = sizing_hints)

    if (util_is_gg(ggthumb)) {
      optimized_args <- util_get_restricted_size_args_for_figure(
        MAX_SIZE = 5 * 1024 * 1024,
        max_w_in_cm = 53,
        max_h_in_cm = 30,
        sizing_hints = sizing_hints
      )

      sizing_hints$w_in_cm <- optimized_args$sizing_hints_updated$w_in_cm
      sizing_hints$h_in_cm <- optimized_args$sizing_hints_updated$h_in_cm
    } else {
      optimized_args <- NULL
    }

    if (suppressWarnings(util_ensure_suggested("jsonlite", goal = "generate figures in reports", err = FALSE))) {
      sizing_hint_script <- paste0(
        'window.sizing_hints = ',
        jsonlite::toJSON(sizing_hints, auto_unbox = FALSE)
      )
    } else {
      sizing_hint_script <- 'window.sizing_hints = "No figure size hints, need R package jsonlite for this."'
    }

    util_ensure_suggested("rmarkdown",
                          goal = "Creating the Print Dialog for Plotly")

    jqui <- rmarkdown::html_dependency_jqueryui()
    jqui$stylesheet <- "jquery-ui.min.css"

    html_inner <- htmltools::tagList(
      rmarkdown::html_dependency_jquery(),
      jqui,
      htmltools::tags$script(type = "text/javascript", sizing_hint_script),
      htmltools::tags$span(`data-nm` = nm, id = "nm"),
      htmltools::tags$span(`data-functionName` = functionName, id = "functionName"),
      html_dependency_dataquieR(iframe = TRUE),
      html_dependency_jspdf(),
      it
    )

    frameTag <- htmltools::tags$iframe(
      src = fig_framed_file,
      style = htmltools::css(border = "0", width = "100%", height = "calc(100% - 5px)")
    )

    if (util_is_gg(ggthumb)) {
      my_style <- "margin:-10px;width:100%;height:100%;object-fit:fill;cursor:zoom-in;"
      imgTag <- htmltools::tagList(htmltools::img(
        src = basename(thumb_file),
        style = my_style,
        onmouseup = "tglePyHandler()",
        `data-iframe` = frameTag
      ))
    } else {
      imgTag <- frameTag
    }

    final_div <- htmltools::div(
      class = "scaler",
      `data-initialw` = sizing_hints$w,
      `data-initialh` = sizing_hints$h,
      style = htmltools::css(
        width = sizing_hints$w,
        height = sizing_hints$h,
        min.width = "320px",
        min.height = "180px",
        resize = "both",
        overflow = "auto",
        border = "1px"
      ),
      imgTag
    )

    attr(final_div, "html_file") <- full_fig_file
    attr(final_div, "html_inner") <- html_inner
    # attr(final_div, "dependencies") <- htmltools::htmlDependencies(html_inner))
    if (util_is_gg(ggthumb)) {
      attr(final_div, "thumbnail_path") <- thumb_file
      attr(final_div, "ggthumb") <- util_compress(ggthumb)
      attr(final_div, "thumbnail_args") <- list(
        width = optimized_args$width,
        height = optimized_args$height,
        dpi = optimized_args$dpi,
        figure_type_id = sizing_hints$figure_type_id,
        rotated = sizing_hints$rotated
      )
    }

    return(final_div)
  }

  return(htmltools::div(it))
}

util_get_restricted_size_args_for_figure <- function(MAX_SIZE, max_w_in_cm, max_h_in_cm, sizing_hints) {
  # 32 bit color-space (4 bytes per pixel)
  BYTES_PER_PIXEL <- 4

  # original proportions
  original_w_cm <- sizing_hints$w_in_cm
  original_h_cm <- sizing_hints$h_in_cm
  aspect_ratio <- original_w_cm / original_h_cm

  # resize if too big keeping aspect ratio
  if (original_w_cm > max_w_in_cm || original_h_cm > max_h_in_cm) {
    scale_factor_w <- max_w_in_cm / original_w_cm
    scale_factor_h <- max_h_in_cm / original_h_cm

    # preserve aspect ratio
    scale_factor <- min(scale_factor_w, scale_factor_h)

    sizing_hints_updated_w_cm <- original_w_cm * scale_factor
    sizing_hints_updated_h_cm <- original_h_cm * scale_factor
  } else {
    sizing_hints_updated_w_cm <- original_w_cm
    sizing_hints_updated_h_cm <- original_h_cm
  }

  # convert to inches for ggsave()
  width_in_inches <- sizing_hints_updated_w_cm / 2.54
  height_in_inches <- sizing_hints_updated_h_cm / 2.54

  # compute number of pixels given the color space and the maximum file size
  max_pixels <- floor(MAX_SIZE / BYTES_PER_PIXEL)

  # compute max. dpi based on physical dimensions and max_pixels
  # we know: max_pixels = (width_in_inches * dpi) * (height_in_inches * dpi)
  # max_pixels = width_in_inches * height_in_inches * dpi^2
  # dpi^2 = max_pixels / (width_in_inches * height_in_inches)
  # dpi = sqrt(max_pixels / (width_in_inches * height_in_inches))

  # reboustness
  if (width_in_inches <= 0 || height_in_inches <= 0) {
    util_warning(c("Internal error, sorry. Please report: Invalid physical",
                   "dimensions (width or height is zero or negative).",
                   "Cannot calculate DPI."))
    return(list(sizing_hints_updated =
                  list(w_in_cm = sizing_hints_updated_w_cm,
                       h_in_cm = sizing_hints_updated_h_cm),
                dpi = 72,
                width = width_in_inches,
                height = height_in_inches))
  }

  max_dpi_from_size <- sqrt(max_pixels / (width_in_inches * height_in_inches))

  # rounding (should not be larger than max_dpi_from_size, so floor())
  # but limit resultion to at least 72 dpi for typical screens
  dpi <- max(72, floor(max_dpi_from_size))

  # Aktualisiere die sizing_hints_updated für die Rückgabe
  sizing_hints_updated <- list(w_in_cm = sizing_hints_updated_w_cm,
                               h_in_cm = sizing_hints_updated_h_cm)

  return(list(sizing_hints_updated = sizing_hints_updated,
              dpi = dpi,
              width = width_in_inches,
              height = height_in_inches))
}

#' Get the size of the currently set cluster
#'
#' or 1, if no cluster has been set
#'
#' @returns [integer] size of the default cluster
#' @noRd
util_get_cores_safe <- function() {
  cl <- parallel::getDefaultCluster()
  if (is.null(cl)) {
    return(1)
  } else {
    return(length(cl))
  }
}

