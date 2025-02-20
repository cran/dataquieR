statichtmlWidget <- function(html, width = NULL, height = NULL,
                             elementId = NULL, sizingPolicy,
                             js = list()) {
  util_ensure_suggested("htmlwidgets", "Render results in RMarkdown")
  if (missing(sizingPolicy)) sizingPolicy <- htmlwidgets::sizingPolicy(fill = TRUE)
  util_ensure_suggested("htmltools", "Render results in RMarkdown")
  #o <- htmltools::knit_print.shiny.tag(html)

  #deps <- c(attr(o, "knit_meta"))
  #  deps <- c(list(rmarkdown::html_dependency_jquery()), attr(o, "knit_meta")) # order matters, jquery before potential datatable.js
  #  deps <- c(attr(o, "knit_meta"), list(rmarkdown::html_dependency_jquery())) # order matters, jquery after potential plotly

  o <- htmltools::as.tags(html)

  deps <- htmltools::findDependencies(o)
  deps <- c(list(rmarkdown::html_dependency_jquery()), deps) # order matters, jquery before potential datatable.js
  # forward options using x
  deps <- deps[!duplicated(
    vapply(deps, "[[", "name", FUN.VALUE = character(1)))]
  x = list(
    html = as.character(htmltools::div(class = "htmlwidget_container",
                                       o))#htmltools::HTML(o)))
    , js = htmlwidgets::JS(js)

  )

  # create widget
  w <- htmlwidgets::createWidget(
    name = 'statichtmlWidget',
    dependencies = deps,
    x,
    width = width,
    height = height,
    package = 'dataquieR',
    elementId = elementId,
    sizingPolicy = sizingPolicy
  )

  #if (any(grepl("plotly", o))) browser()

  w
}
