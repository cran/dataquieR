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
#'
#' @inherit base::print
#' @importFrom grDevices colorRamp rgb col2rgb
#' @export
#' @return the printed object
print.ReportSummaryTable <- function(x, relative, dt = FALSE,
                                     fillContainer = FALSE, ...) {

  higher_means <- attr(x, "higher_means")
  if (is.null(higher_means)) higher_means <- "worse"
  continuous <- attr(x, "continuous")
  if (is.null(continuous)) continuous <- TRUE
  colcode <- attr(x, "colcode")
  if (is.null(colcode)) {
    continuous <- TRUE
  }
  level_names <- attr(x, "level_names")

  if (missing(relative)) relative <- continuous

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

  if (higher_means != "worse")
    my_cols <- rev(my_cols)

  #p <- util_set_size(p,
  #                   width_em  = length(unique(ctab1$Var2)) + 20,
  #                   height_em = length(unique(ctab1$Var1)) + 5)


  if (prod(dim(hm)) == 0) { # no output
    if (dt) {
      util_ensure_suggested("DT", "the option dt = TRUE")
      w <- DT::datatable(data.frame())
      print(w)
      return(w)
    } else {
      return(print(ggplot()))
    }
  }
  tb <- reshape::melt(hm, id.vars = "Variables")


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
    colr <- colorRamp(colors = rev(my_cols))
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
        v <- t(col2rgb(colcode[as.character(values)]))
      }
      a <- apply(v, 1, function(cl) {
        paste0(
          "<span style=\"width:100%;display:block;",
          "overflow:hidden;background: ",
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
          "\">&nbsp;</span>"
        )
      })
      if (relative) {
        paste0(a, round(100 * values, 1), "%", b, values, cc)
      } else {
        paste0(a, round(values, 1), b, values, cc)
      }
    })
    x[, names(colors_of_hm)] <- colors_of_hm

    # https://www.pierrerebours.com/2017/09/custom-sorting-with-dt.html
    # https://datatables.net/manual/data/orthogonal-data
    w <- DT::datatable(x,
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
      list(htmltools::htmlDependency(
        name = "vertical-dt-style"
        ,version = "0.0.1"
        ,src = c(file = system.file("", package = "dataquieR"))
        ,stylesheet = "vertical-dt-style.css"
        ,script = "sort_heatmap_dt.js"
      )
    ))

    print(w)

    return(w)

    # https://stackoverflow.com/a/46043032
  } else {
    if (continuous) {
      print(ggballoonplot(tb,
                          fill = "value"),
            ggtheme = theme_minimal()) +
        (if (nrow(hm) > ncol(hm)) coord_flip()) +
        scale_fill_gradientn(colors = rev(my_cols))

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

      print(ggplot(tb, aes_(
        x = ~ variable, y = ~ Variables,
        fill = ~ value
      )) + geom_tile(colour = "white", lwd = 0.8) +
        theme_minimal() +
        (if (nrow(hm) > ncol(hm)) coord_flip()) +
        scale_fill_manual(values = cc, name = " ") +
        #      scale_fill_gradientn(colors = rev(my_cols)) +
        #      scale_x_discrete(position = "top") +
        xlab("") +
        guides(fill = guide_legend(
          ncol = 1, nrow = length(colcode),
          byrow = TRUE
        )) +
        theme(
          legend.position = "bottom",
          axis.text.x = element_text(angle = 90, hjust = 0),
          axis.text.y = element_text(size = 10)
        )
      )
    }
  }

}
