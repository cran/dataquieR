#' print implementation for the class `ReportSummaryTable`
#'
#' @description
#' Use this function to print results objects of the class
#' `ReportSummaryTable`.
#'
#' @param relative [logical] normalize the values in each column by
#'                           division by the `N` column.
#' @param dt [logical] use `DT::datatables`, if installed
#' @param fillContainer [logical] if `dt` is `TRUE`, control table size,
#'        see `DT::datatables`.
#' @param displayValues [logical] if `dt` is `TRUE`, also display the actual
#'                                values
#' @param view [logical] if `view` is `FALSE`, do not print but return the
#'                       output, only
#' @param x `ReportSummaryTable` objects to print
#' @inheritParams acc_distributions
#' @param ... not used, yet
#'
#' @seealso base::print
#' @importFrom grDevices colorRamp rgb col2rgb
#' @importFrom ggplot2 expansion waiver scale_color_gradientn
#' @export
#' @return the printed object
print.ReportSummaryTable <- function(x, relative, dt = FALSE,
                                     fillContainer = FALSE,
                                     displayValues = FALSE,
                                     view = TRUE, ...,
                                     flip_mode = "auto") {
  empty <-
    (!length(setdiff(colnames(x), c("Variables", "N")))) ||
    (!c("Variables") %in% colnames(x)) ||
    (!c("N") %in% colnames(x))
  if (empty) {
    if (!dt) {
      x <- ggplot() +
        annotate("text", x = 0, y = 0, label = "Empty result.") +
        theme(
          axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "none",
          panel.background = element_blank(),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_blank()
        )
      x <- util_set_size(x)
    } else {
      x <- htmltools::HTML("")
    }
    attr(x, "warning") <- "Empty result"
    class(x) <- union("dataquieR_result", class(x))
    if (view) {
      print(x)
    }
    attr(x, "from_ReportSummaryTable") <- TRUE
    return(invisible(x))
  }
  higher_means <- attr(x, "higher_means")
  if (is.null(higher_means)) higher_means <- "worse"
  continuous <- attr(x, "continuous")
  if (is.null(continuous)) continuous <- TRUE
  colcode <- attr(x, "colcode")
  if (is.null(colcode)) {
    continuous <- TRUE
  }
  level_names <- attr(x, "level_names")

  if (missing(relative)) relative <- attr(x, "relative")
  if (is.null(relative)) relative <- continuous

  hm <- x
  if (relative) {
    hm <- cbind.data.frame(Variables = hm$Variables,
                           hm[, setdiff(colnames(hm), c("Variables", "N")),
                              drop = FALSE] /
                             hm$N)
  } else {
    hm <- cbind.data.frame(Variables = hm$Variables,
                           hm[, setdiff(colnames(hm), c("Variables", "N")),
                              drop = FALSE])
  }

  # plot usual ggplot
  # ggplot(ds2, aes(x=value)) + facet_wrap(variable ~ ., ncol = 1) +
  # geom_bar() + theme_minimal() +
  #  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

  my_cols <- c("#7f0000", "#b30000", "#d7301f", "#ef6548", "#fc8d59",
               "#fdbb84", "#fdd49e", "#fee8c8", "#2166AC")

  colscale <- attr(x, "colscale")
  if (is.null(colscale)) {
    colscale <- my_cols
  }

  if (higher_means != "worse")
    colscale <- rev(colscale)

  #p <- util_set_size(p,
  #                   width_em  = length(unique(ctab1$Var2)) + 20,
  #                   height_em = length(unique(ctab1$Var1)) + 5)


  if (prod(dim(hm)) == 0) { # no output, # TODO: is this still possible after empty checks above?
    if (dt) {
      util_ensure_suggested("DT", "the option dt = TRUE")
      w <- DT::datatable(data.frame())
      if (view) print(w)
      attr(w, "from_ReportSummaryTable") <- TRUE
      return(w)
    } else {
      p <- ggplot()
      if (view) print(p)
      attr(p, "from_ReportSummaryTable") <- TRUE
      return(p)
    }
  }

  #tb <- reshape::melt(hm, id.vars = "Variables")
  tb <- stats::reshape(data = hm, idvar = "Variables",
                       varying = colnames(hm)[2:ncol(hm)],
                       v.names = "value",
                       times = colnames(hm)[2:ncol(hm)],
                       direction = "long")
  rownames(tb) <- NULL
  names(tb) <- c("Variables", "variable", "value")


  levs <- unique(tb$variable)

  if(length(levs[grep("int_", levs)]) == 0  &&
     length(levs[grep("com_", levs)]) == 0  &&
     length(levs[grep("con_", levs)]) == 0  &&
     length(levs[grep("acc_", levs)]) == 0 ) {

    tb$variable <- factor(tb$variable,
                          levels = levs)
  }else {

  #sort levels
  #levs <- sort(levs)
  levs_int <- levs[grep("int_", levs)]
  levs_com <- levs[grep("com_", levs)]
  levs_con <- levs[grep("con_", levs)]
  levs_acc <- levs[grep("acc_", levs)]

  #order the factor levels
  tb$variable <- factor(tb$variable,
                                 levels = c(levs_int,levs_com,
                                            levs_con, levs_acc))
  }

  if (dt) {
    # https://stackoverflow.com/a/50406895
    util_ensure_suggested("DT", "the option dt = TRUE")

    # if (!relative) {
    #   hm[, setdiff(colnames(hm), c("Variables", "N"))] <-
    #     hm[, setdiff(colnames(hm), c("Variables", "N")),
    #            drop = FALSE] / max(as.matrix(
    #              hm[, setdiff(colnames(hm), c("Variables", "N")),
    #                 drop = FALSE]
    #            ), na.rm = TRUE)
    # }
    mx <- max(as.matrix(
                    hm[, setdiff(colnames(hm), c("Variables", "N")),
                       drop = FALSE]
                  ), na.rm = TRUE)
    colr <- colorRamp(colors = rev(colscale))
    colors_of_hm <- lapply(hm, function(values) {
      if (!all(is.numeric(values))) { return(values) }
      if (any(is.na(values))) { return(values) }
      if (continuous) {
        if (!relative) {
          v <- colr(values / mx)
        } else {
          v <- colr(values)
        }
      } else {
        if (length(level_names) > 0) {
          cc <- setNames(colcode, nm = level_names)
          v <- t(col2rgb(cc[level_names[as.character(values)]], alpha = TRUE))
        } else {
          v <- t(col2rgb(colcode[as.character(values)], alpha = TRUE))
        }
      }
      a <- apply(v, 1, function(cl) {
        paste0(
          "<span style=\"width:100%;display:block;text-align:center;",
          "color:",
          rgb(255 - cl[[1]],
              255 - cl[[2]],
              255 - cl[[3]], maxColorValue = 255.0),
          ";",
          "overflow:hidden;background:",
          rgb(cl[[1]],
            cl[[2]],
            cl[[3]], maxColorValue = 255.0),
          "\" title=\""
        )
      })
      b <- apply(v, 1, function(cl) {
        paste0(
          "\" sort=\""
        )
      })
      cc <- apply(v, 1, function(cl) {
        paste0(
          "\">"
        )
      })
      d <- apply(v, 1, function(cl) {
        paste0(
          "</span>"
        )
      })
      if (displayValues) {
        if (relative) {
          dv <- paste0(round(100 * values, 0), "%")
        } else {
          dv <- values
        }
      } else {
        dv <- "&nbsp;"
      }
      if (relative) {
        paste0(a, round(100 * values, 1), "%", b, values, cc, dv, d)
      } else {
        if (length(level_names) > 0)
          hover <- level_names[as.character(values)]
        else
          hover <- round(values, 1)
        paste0(a, hover, b, values, cc, dv, d)
      }
    })
    x[, names(colors_of_hm)] <- colors_of_hm

    # https://www.pierrerebours.com/2017/09/custom-sorting-with-dt.html
    # https://datatables.net/manual/data/orthogonal-data
    # filter = "top" is not helpful
    w <- DT::datatable(x, # TODO: Add Buttons Extension
                       fillContainer = fillContainer,
                       rownames = FALSE,
                       options = list(
                         pageLength = nrow(x),
                         columnDefs = list(
                             list(
                               targets = seq_len(ncol(x)-2),
                               render = DT::JS("sort_heatmap_dt")
                             )
                         )
                         ),
                       class = "ReportSummaryTable",
                  colnames = paste("<div class=\"colheader\">",
                                   vapply(
                                     strsplit(
                                       colnames(x),
                                       "", fixed = TRUE),
                                     function(letters) {
                                       paste0("<span>",
                                              paste0(letters,
                                                     collapse = ""),
                                              "</span>")
                                      },
                                     FUN.VALUE = character(1)),
                                   "</div>"),
                  escape = FALSE
    )


    # https://stackoverflow.com/a/35775262
    w$dependencies <- c(
      w$dependencies,
      list(html_dependency_vert_dt())
    )

    if (view) print(w)

    attr(w, "from_ReportSummaryTable") <- TRUE

    return(w)

    # https://stackoverflow.com/a/46043032
  } else {# https://stackoverflow.com/a/64112567
    if (continuous &&
        (length(unique(tb$variable)) == 1 ||
        length(unique(tb$Variables)) == 1)) { # if only one dimension and real numbers, not categories, collapse the heatmap to a barchart
      if (length(unique(tb$Variables)) == 1) { # only one value in y direction
        x <- "variable"
        y <- "value"
        fill <- "value"
        if (relative) {
          if (max(tb$value) > 0.1)
            add_amount <- 0.04
          else
            add_amount <- 0.0004
          rel_ax <- scale_y_continuous(labels = scales::percent,
                                       expand = expansion(add = c(0,
                                                                  add_amount)))
        } else {
          rel_ax <- NULL
        }
      } else {
        x <- "Variables"
        y <- "value"
        fill <- "value"
        if (relative) {
          if (max(tb$value) > 0.1)
            add_amount <- 0.04
          else
            add_amount <- 0.0004
          rel_ax <- scale_y_continuous(labels = scales::percent,
                                       expand = expansion(add = c(0,
                                                                  add_amount)))
        } else {
          rel_ax <- NULL
        }
      }

      if (missing(flip_mode) && getOption("dataquieR.flip_mode",
                                          dataquieR.flip_mode_default) ==
          dataquieR.flip_mode_default) {
        # for bar charts, flip mode defaults to default (noflip)
        # fli <- coord_cartesian();
        fli <- coord_flip()
      } else {
        fli <- util_coord_flip(w = length(unique(tb[[x]])),
                               h = length(unique(tb[[y]])))
      }

      is_flipped <- inherits(fli, "CoordFlip")

      if (is_flipped) {
        hjust <- 0
        vjust <- 0
      } else {
        hjust <- 0.5
        vjust <- 0
      }

      if (relative) {
        scale_fill <- scale_fill_gradientn(colors = rev(colscale),
                             labels = scales::percent)
        gtlb <- paste0(" ", round(tb$value * 100, digits = 2), "%")
        texts <-
          geom_text(label = gtlb,
                    hjust = hjust, vjust = vjust)
      } else {
        scale_fill <- scale_fill_gradientn(colors = rev(colscale))
        gtlb <- paste0(" ", round(tb$value, digits = 2))
        texts <-
          geom_text(label = gtlb,
                    hjust = hjust, vjust = vjust)
      }

      p <- ggplot(tb, aes(
        x = .data[[x]], y = .data[[y]],
        fill = .data[[fill]]
      )) + geom_bar(stat = "identity", na.rm = TRUE,
                    colour = "white", linewidth = 0.8) + # https://github.com/tidyverse/ggplot2/issues/5051
        theme_minimal() +
        texts +
        fli +
        scale_fill +
        xlab("") +
        guides(fill = guide_legend(
          title = ""
        #   ncol = 1, nrow = length(colcode),
        #   byrow = TRUE
        )) +
        rel_ax +
        theme(
          legend.position = "bottom",
          axis.text.x = element_text(angle = 90, hjust = 0),
          axis.text.y = element_text(size = 10)
        ) + xlab("") + ylab("")

      if (suppressWarnings(util_ensure_suggested("plotly",
                                      goal = "generate interactive plots",
                                      err = FALSE))) {
        attr(p, "py") <- local({
          py <- plotly::plot_ly(tb,
                  x = tb[[ifelse(is_flipped, y, x)]],
                  y = tb[[ifelse(is_flipped, x, y)]],
                  type = 'bar',
                  marker = list(color = tb[[fill]],
                                autocolorscale = FALSE,
                                colorscale = colscale))
          py <- plotly::style(py,
                              text = gtlb,
                              textposition = "auto")

          if (relative) {
            if (is_flipped) {
              py <- plotly::layout(py, xaxis = list(tickformat = ".2%",
                                                    rangemode="tozero"))
            } else {
              py <- plotly::layout(py, yaxis = list(tickformat = ".2%",
                                                    rangemode="tozero"))
            }
          }

          # %>%
          #   layout(xaxis = list(title = 'Parameter Name (Unit)',
          #                       range = c(0, 1), tickvals = seq(0, 1, 0.2)),
          #          yaxis = list(title = 'Sample ID', categoryorder = "total ascending"))
          #
          #
          # p <- ggplot(tb, aes(
          #   x = .data[[x]], y = .data[[y]],
          #   #fill = .data[[fill]]
          # )) + geom_bar(stat = "identity", na.rm = TRUE,
          #               colour = "white", linewidth = 0.8) + # https://github.com/tidyverse/ggplot2/issues/5051
          #   theme_minimal() +
          #   fli +
          #   #scale_fill +
          #   xlab("") +
          #   guides(fill = guide_legend(
          #     title = ""
          #   )) +
          #   rel_ax +
          #   theme(
          #     legend.position = "bottom",
          #     axis.text.x = element_text(angle = 90, hjust = 0),
          #     axis.text.y = element_text(size = 10)
          #   ) + xlab("") + ylab("")
          # py <- util_plot_figure_plotly(p)
          # for (i in seq_along(gtlb)) {
          #   py <- plotly::style(py,
          #                       traces = i,
          #                       text = gtlb[[i]],
          #                       textposition = "auto")
          # }
          py
        })
      }

      if (view) print(p)
      attr(p, "from_ReportSummaryTable") <- TRUE
      return(p)

    } else {

      if (continuous) {
        xlim <- as.character(tb$Variables)
        xlim <- xlim[!duplicated(xlim)]
        ylim <- as.character(tb$variable)
        xsize <- util_plotly_font_size(ncol(hm) - 1)
        ysize <- util_plotly_font_size(nrow(hm), space = 150)
        p <- ggplot(tb, aes(x = Variables, y = variable, colour = value, size = value)) +
          geom_point()  + #scale_size_continuous(range = c(-1, 10)) + scale_x_discrete() +# breaks = 50 * seq_len(length(unique(tb$Variables)))) +
          #scale_x_discrete(guide = guide_axis(n.dodge = 5)) +
          labs(title = waiver(),
               subtitle = waiver(),
               x = "",
               y = "") +
          scale_x_discrete(limits = xlim) +
          # (if (nrow(hm) < ncol(hm)) coord_flip()) + # 7x5 standard of rmarkdown, not din a4
          util_coord_flip(w = nrow(hm), h = ncol(hm)) +
          scale_color_gradientn(colors = rev(colscale)) +
          theme_minimal() +
          theme(aspect.ratio=5*length(unique(tb$Variables))/7/length(unique(tb$Variables)),
                axis.text.x = element_text(angle = 35, hjust = 1,
                                           size = xsize),
                axis.text.y = element_text(size = ysize))

        p <- util_set_size(p, 500, 300)

        if (view) print(p)

        attr(p, "from_ReportSummaryTable") <- TRUE

        return(p)

      } else {
        tb$value <- as.factor(tb$value)

        if (length(level_names) > 0) {
          levels(tb$value) <- level_names[levels(tb$value)]
          cc <- setNames(colcode, nm = level_names)
        } else {
          cc <- colcode
        }

        #    colcode <- c("#B2182B", "#ef6548", "#92C5DE", "#2166AC", "#B0B0B0")
        #    names(colcode) <- levels(tb$value)

        p <- ggplot(tb, aes(
          x = variable, y = Variables,
          fill = value
        )) + geom_tile(colour = "white", linewidth = 0.8) + # https://github.com/tidyverse/ggplot2/issues/5051
          theme_minimal() +
          # (if (nrow(hm) > ncol(hm)) coord_flip()) +
          util_coord_flip(w = nrow(hm), h = ncol(hm)) +
          scale_fill_manual(values = cc, name = " ") +
          #      scale_fill_gradientn(colors = rev(colscale)) +
          #      scale_x_discrete(position = "top") +
          xlab("") +
          ylab("") +
          guides(fill = guide_legend(
            ncol = 1, nrow = length(colcode),
            byrow = TRUE
          )) +
          theme(
            legend.position = "bottom",
            axis.text.x = element_text(angle = 90, hjust = 0),
            axis.text.y = element_text(size = 10)
          )
        if (view) print(p)

        attr(p, "from_ReportSummaryTable") <- TRUE

        return(p)
      }
    }
  }
}
