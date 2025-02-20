# This file creates an environment with functions, that generate an
# html menu
# used by [print.dataquieR_resultset2]

#' `.menu_env` -- an environment for HTML menu creation
#'
#' used by the dq_report2-pipeline
#' @name menu_env
#' @keywords internal
.menu_env <- new.env(parent = environment())

#' Create a single menu entry
#'
#' @param title of the entry
#' @param id linked `href`, defaults to modified title. can be a word, then
#'        a single-page-link with an anchor tag is created.
#' @param ... additional arguments for the menu link
#' @return html-a-tag object
#' @name menu_env_menu_entry
#' @keywords internal
.menu_env$menu_entry <- function(title,
                                 id = title,
                       ...) {
  if (!grepl("#", id, fixed = TRUE) &&
      !startsWith(id, "http://") &&
      !startsWith(id, "https://") &&
      !startsWith(id, "file:") &&
      !startsWith(id, "ftp://") &&
      !startsWith(id, "ftps://") &&
      !startsWith(id, "sftp://") &&
      !startsWith(id, "ssh:") &&
      !startsWith(id, "gopher:") &&
      !startsWith(id, "scp:") &&
      !startsWith(id, "rsync:")) {
    hash_if_needed <- "#"
    id <- prep_link_escape(id,
                           html = TRUE)
  } else {
    hash_if_needed <- ""
    link <- sub("#.*$", "", id)
    hash <- sub("^.*?#", "", id)
    hash <- htmltools::urlEncodePath(as.character(hash))
    id <- paste0(link, "#", hash)
  }
  htmltools::a(href=sprintf(
    "%s%s",
    hash_if_needed,
    id),
    title, # htmltools::htmlEscape(title),
    ...)
}

#' Creates a drop-down menu
#'
#' @param title name of the entry in the main menu
#' @param menu_description description, displayed, if the main
#'                         menu entry itself is clicked
#' @param ... the sub-menu-entries
#' @param id id for the entry, defaults to modified title
#'
#' @return html div object
#' @name menu_env_drop_down
#' @keywords internal
.menu_env$drop_down <- function(title,
                                menu_description,
                      ..., id = prep_link_escape(title)) {
  htmltools::div(class = "dropdown",
                 id = id,
                 onclick = sprintf("showDescription('%s', '%s'); event.stopPropagation()",
                                   id,
                                   htmltools::htmlEscape(menu_description,
                                                         TRUE)),
                 htmltools::tags$button(class="dropbtn",
                                        htmltools::tagList(
                                          htmltools::tags$p(htmltools::htmlEscape(title)),
                                          htmltools::tags$i(
                                            class="fa fa-caret-down"))),
                 htmltools::div(class="dropdown-content",
                                # https://stackoverflow.com/a/33225276
                                ...
                 )
  )
}

#' Generate the menu for a report
#'
#' @param pages encapsulated `list` with report pages as `tagList` objects,
#'              its names are the desired file names
#'
#' @return the html-`taglist` for the menu
#' @name menu_env-menu
#' @keywords internal
.menu_env$menu <- function(pages) { # TODO: implement
  # htmltools::tagList(
  #   menu_entry("Home"),
  #   menu_entry("News"),
  #   drop_down("Dropdown",
  #             menu_entry("Link 1", "report.html#x"),
  #             menu_entry("Link 2", "report.html#y"),
  #             menu_entry("Link 3", "https://google.de", target="_blank"),
  #             menu_entry("Link 4", "z.html#z")
  #   )
  # )
  entry_env <- environment()
  entries_of_dd <- lapply(names(pages), function(fn) {
    dd <- lapply(names(pages[[fn]]), function(sp) {
      r <- attr(pages[[fn]][[sp]], "dropdown")
      if (length(r) != 1) {
        r <- "Dropdown"
        attr(entry_env$pages[[fn]][[sp]], "dropdown") <- r
      }
      r
    })
    dd <- unique(dd)
    lapply(setNames(nm = dd), function(ddn) {
      eoddn <- lapply(names(pages[[fn]]), function(sp) {
        if (attr(pages[[fn]][[sp]], "dropdown") == ddn) {
          util_attach_attr(sp, fn = fn)
        } else {
          NULL
        }
      })
      eoddn
    })
  })
  all_dd <- unique(unlist(lapply(entries_of_dd, names)))

  menu_from_pages <- lapply(all_dd, function(ddn) {
    util_ensure_suggested("markdown")
    ddmen <- lapply(unlist(lapply(entries_of_dd, `[[`, ddn), recursive = FALSE),
                    function(me) {
                      if (is.null(me)) {
                        NULL
                      } else {
                        do.call("call", args = c(list("menu_entry",
                                                      me,
                                                      sprintf("%s#%s", attr(me, "fn"), me))),
                                quote = TRUE)
                      }
    })
    concept_info <- subset(util_get_concept_info("dqi"),
           get("Dimension") == ddn & get("Level") == 1 &
             get("dataquierR pipeline include") == 1,
           select = c("Definition", "Explanation", "Guidance", "abbreviation",
                      "IndicatorID"),
           drop = FALSE)
    if (nrow(concept_info) == 1) { # https://dataquality.qihs.uni-greifswald.de/PDQC_DQ_1_0_0_0.html
      menu_description <-
        htmltools::tagList(
          htmltools::h2(ddn),
          htmltools::tags$p(
            htmltools::a(
              href=
                sprintf(
          "https://dataquality.qihs.uni-greifswald.de/PDQC_%s.html",
                  concept_info$IndicatorID),
              target="_blank",
              title="Online reference",
              ddn
            ),
            sprintf(
              " -- related indicator function names are prefixed with %s",
              dQuote(concept_info$abbreviation))
          ),
          htmltools::h3("Definition"),
          htmltools::tags$p(
            htmltools::HTML(markdown::markdownToHTML(
              text = concept_info$Definition, fragment.only = TRUE))
          ),
          htmltools::h3("Explanation"),
          htmltools::tags$p(
            htmltools::HTML(markdown::markdownToHTML(
              text = concept_info$Explanation, fragment.only = TRUE))
          ),
          htmltools::h3("Guidance"),
          htmltools::tags$p(
            htmltools::HTML(markdown::markdownToHTML(
              text = concept_info$Guidance, fragment.only = TRUE))
          )
        )
    } else {
      menu_description <-
        htmltools::tagList(
          htmltools::h2(ddn),
          htmltools::tags$p(
            sprintf("No unique description for %s in DQ concept", dQuote(ddn)),
          )
        )
    }
    do.call("call", args = c(list("drop_down",
                                  ddn,
                                  menu_description =
                                    as.character(menu_description)),
                             ddmen), quote = TRUE)
  })
  do.call(htmltools::tagList,
          lapply(unlist(menu_from_pages), eval, envir = entry_env),
          quote = FALSE)
}

# make all the functions in the environment enclosed by this environment, too,
# so that they can look up this environment for metadata
for (f in ls(.menu_env)) {
  if (is.function(.menu_env[[f]])) {
    environment(.menu_env[[f]]) <- .menu_env
  }
}
