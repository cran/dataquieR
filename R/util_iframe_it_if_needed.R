#' Make `it` scalable, if it is a figure
#'
#' this function writes figures to helper files and embeds these in a returned
#' object which is a scalable `iframe`. it does not change other objects in `it`.
#'
#' @param it [htmltools::tag()] compatible object
#' @param dir [character] output directory for potential `iframe`.
#' @param nm [character] name for the `iframe`d file, if one is created
#' @param fkt [character] function name of the indicator function that created
#'                        `ìt`.
#' @param sizing_hints `object` additional metadata about the natural figure size
#' @param ggthumb [ggplot2::ggplot()] optional, underlying `ggplot2` object
#'                                    for a preview
#'
#' @return [htmltools::tag()] compatible object, maybe now in an `iframe`
#' @keywords internal
util_iframe_it_if_needed <- function(it, dir, nm, fkt, sizing_hints, ggthumb) {
  functionName <- fkt
  if ((inherits(it, "plotly") ||
      (inherits(it, "shiny.tag") && it$name == "img")) &&
      !isTRUE(getOption('knitr.in.progress'))) {
    util_expect_scalar(dir, check_type = is.character)
    util_expect_scalar(nm, check_type = is.character)
    util_stop_if_not(dir.exists(dir))

    if (!dir.exists(file.path(dir, "lib"))) {
      dir.create(file.path(dir, "lib"), showWarnings = FALSE, recursive = TRUE)
    }

#    if (inherits(it, "plotly")) {
#      it$sizingPolicy$defaultHeight <- "100%" # seems sometimes not to work?! now done by an !important style
#    }

    if (!nzchar(paste0("", nm))) { # no name --> not a dq_report2, will create a temporary name
      fig_framed_file <- tempfile(pattern = "FIG_",
                                  fileext = ".html",
                                  tmpdir = getwd())
      util_stop_if_not(dirname(fig_framed_file) == getwd())
      fig_framed_file <- basename(fig_framed_file)
    } else {
      fig_framed_file <- paste0("FIG_", prep_link_escape(nm, html = TRUE),
                              ".html")
    }

    # print(nm)
    sizing_hints <- util_finalize_sizing_hints(sizing_hints = sizing_hints)

    if (!file.exists(fig_framed_file)) {

      if (util_is_gg(ggthumb)) {
        # limit to 5 meters based on limits like limitsize below and
        #   options(ragg.max_dim = ...)
        # (50000px).

        #Fix to keep the aspect ratio
        if (sizing_hints$w_in_cm > 40 ) {
          aspect_ratio_1 <- sizing_hints$h_in_cm/sizing_hints$w_in_cm
          sizing_hints$w_in_cm <- 40
          sizing_hints$h_in_cm <- aspect_ratio_1 *  sizing_hints$w_in_cm
        }
        if ( sizing_hints$h_in_cm > 40) {
          aspect_ratio_2 <- sizing_hints$w_in_cm/sizing_hints$h_in_cm
          sizing_hints$h_in_cm <- 40
          sizing_hints$w_in_cm <- aspect_ratio_2 * sizing_hints$h_in_cm
        }
        .w <- sizing_hints$w_in_cm
        .h <- sizing_hints$h_in_cm
       # .w <- min(sizing_hints$w_in_cm, 40)
       # .h <- min(sizing_hints$h_in_cm, 40)
        fig_framed_thumb_file <- gsub("\\.html$", ".png", fig_framed_file)
        tfn <- file.path(dir, fig_framed_thumb_file)
        suppressWarnings(suppressMessages(ggplot2::ggsave(
          plot = ggthumb + ggplot2::theme(
            plot.margin = ggplot2::unit(rep(4, 4), "mm"),
            aspect.ratio = NULL),
          filename = tfn,
          width = .w,
          height = .h,
          units = "cm",
          limitsize = FALSE
          )));
      } else {
        tfn <- NULL
      }

      if (
        suppressWarnings(
          util_ensure_suggested("jsonlite",
                                goal = "generate figures in reports",
                                err = FALSE))) {
        sizing_hint_script <- paste0(c(
          'window.sizing_hints = ', jsonlite::toJSON(sizing_hints,
                                                    auto_unbox = FALSE) # TODO: Use jsonlite also for renderInfo
        ), collapse = "\n")
      } else {
        sizing_hint_script <- paste0(c(
          'window.sizing_hints = ',
            '"No figure size hints, need R package jsonlite for this."'
        ), collapse = " ")
      }
      it <- htmltools::tagList(
        rmarkdown::html_dependency_jquery(),
        htmltools::tags$script(type = "text/javascript", sizing_hint_script),
        htmltools::tags$span(`data-nm` = nm, id = "nm"),
        htmltools::tags$span(`data-functionName` = functionName,
                             id = "functionName"),
        html_dependency_dataquieR(iframe = TRUE),
        it
      )
      suppressWarnings(htmltools::save_html(html = it,
                           file = fig_framed_file,
                           libdir = file.path(dir, "lib")))
    } else {
      fig_framed_thumb_file <- gsub("\\.html$", ".png", fig_framed_file)
      tfn <- file.path(dir, fig_framed_thumb_file)
    }


    frameTag <-
      htmltools::tags$iframe(
        src = fig_framed_file,
        style = htmltools::css(
          border = "0",
          width = "100%",
          height = "calc(100% - 5px)"
        )
      )

    if (!is.null(tfn) && file.exists(tfn)) {
      imgTag <- htmltools::tagList(htmltools::img(src = basename(tfn),
                            style =
                              "margin:-10px;width:100%;height:100%;object-fit:fill;cursor:zoom-in;",
                            onmouseup = "tglePyHandler()",
                            `data-iframe`= frameTag)
      )
    } else {
      imgTag <- frameTag
    }

    it <- htmltools::div(
      class = "scaler",
      `data-initialw` = sizing_hints$w,
      `data-initialh` = sizing_hints$h,
      style = htmltools::css(
        width = sizing_hints$w,
        height = sizing_hints$h,
        # width = "480px",
        # height = "270px",
        min.width = "320px",
        min.height = "180px",
        resize = "both",
        overflow = "auto", # hidden does not work with Safari, which does not show the resize handle, then.
        border = "1px"
      ),
      imgTag
    )
    return(it)
  } else {
    return(htmltools::div(it))
  }
}
