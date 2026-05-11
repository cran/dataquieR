util_fix_columns_in_dashboard_for_overview <- function(DashboardT, image_dir) {
  util_ensure_suggested("jsonlite",
                        goal = "dashboard views in overall-overviews",
                        err = TRUE)

  if (nrow(DashboardT) == 0) return(DashboardT)

  DashboardT <-
    util_extract_datauri_pngs(DashboardT, image_dir = image_dir)

  if (nrow(DashboardT) == 0) return(DashboardT)

  orig_cn <- colnames(DashboardT)

  trnsl <- util_translate(c("href", "value", "popup_href", "title"),
                          as_this_translation = colnames(DashboardT))

  make_vnlb_link <- function(to_show) {
    if (any(.cempt <- is.na(DashboardT[[to_show]])))
      DashboardT[[to_show]][.cempt] <- ""

    vapply(apply(DashboardT, 1, function(x) {
      res <- x
      try({
        cnt <- x[[to_show]]
        res <- htmltools::a(href = x[[trnsl[["href"]]]],
                            title = htmltools::HTML(as.character(
                              x[[trnsl[["value"]]]])),
                            onclick = htmltools::htmlTemplate(text_ = "(function(e) {
                                                        e.preventDefault();
                                                        showDataquieRResult({{url}}, {{link_url}}, {{title}});
                                                      })(event);",
                                                              url =
                                                                jsonlite::toJSON(paste0(x[[trnsl[["popup_href"]]]]), auto_unbox = TRUE),
                                                              link_url = jsonlite::toJSON(paste0(x[[trnsl[["href"]]]]), auto_unbox = TRUE),
                                                              title = jsonlite::toJSON(x[[trnsl[["title"]]]], auto_unbox = TRUE)
                            ),
                            htmltools::HTML(as.character(cnt)))
      })
      res
    }), as.character, FUN.VALUE = character(1))
  }

  nm <- util_translate(VAR_NAMES, as_this_translation = colnames(DashboardT))
  lb <- util_translate(LABEL, as_this_translation = colnames(DashboardT))
  fig <- util_translate("Figure", as_this_translation = colnames(DashboardT))
  gra <- util_translate("Graph", as_this_translation = colnames(DashboardT))
  fqvn <- util_translate("fq_VARNAME", as_this_translation = colnames(DashboardT))

  DashboardT[[lb]] <- make_vnlb_link(lb)
  DashboardT[[nm]] <- make_vnlb_link(fqvn)
  DashboardT[[fig]] <- make_vnlb_link(fig)
  DashboardT[[gra]] <- make_vnlb_link(gra)

  # ok <- head(levels(DashboardT$Classification), 1)
  #
  # if (length(ok) > 0) {
  #   rows_to_take <- DashboardT$Classification > ok
  # } else {
    rows_to_take <- rep(TRUE, nrow(DashboardT))
  # }

  rows_to_take[is.na(rows_to_take)] <- FALSE

  # columns_to_shorten <-
  #   vapply(DashboardT,
  #          function(x)
  #            suppressWarnings(max(nchar(as.character(x)), na.rm = TRUE)),
  #          FUN.VALUE = numeric(1)) > 2000
  columns_to_shorten <- rep(FALSE, ncol(DashboardT))


  if (any(columns_to_shorten)) {
    util_warning("Removed long columns from overall dashboard") # TODO: Better warning message
    DashboardT[, columns_to_shorten] <-
      NA_character_
  }


  DashboardT <-
    DashboardT[rows_to_take, , FALSE]

  DashboardT$title <- NULL
  DashboardT$href <- NULL
  DashboardT$popup_href <- NULL

  util_translated_colnames(DashboardT) <-
    util_translate(util_translate(colnames(DashboardT),
                                  as_this_translation = orig_cn,
                                  reverse = TRUE),
                   as_this_translation = orig_cn) # TODO: avoid reverse

  DashboardT
}

util_extract_datauri_pngs <- function(DashboardT, image_dir) {
  util_stop_if_not(is.data.frame(DashboardT))
  util_stop_if_not(length(image_dir) == 1L, is.character(image_dir), nzchar(image_dir))

  util_ensure_suggested(
    "jsonlite",
    goal = "dashboard views in overall-overviews",
    err = TRUE
  )

  image_dir_fs <- path.expand(image_dir)

  if (!dir.exists(image_dir_fs)) {
    dir.create(image_dir_fs, recursive = TRUE, showWarnings = FALSE)
  }

  image_dir_href <- basename(normalizePath(image_dir_fs, winslash = "/", mustWork = FALSE))

  normalize_data_uri <- function(uri) {
    paste0(
      "data:image/png;base64,",
      gsub(
        "\\s+",
        "",
        sub("^data:image/png;base64,", "", uri, perl = TRUE),
        perl = TRUE
      )
    )
  }

  replace_src_attr <- function(attr_text, new_path) {
    quote_chr <- sub(
      '^src\\s*=\\s*(["\\\']).*$',
      "\\1",
      attr_text,
      perl = TRUE
    )
    paste0("src=", quote_chr, new_path, quote_chr)
  }

  process_cell <- function(x, dedup_env, file_map_env, next_id_env) {
    if (is.na(x) || !grepl("data:image/png;base64", x, fixed = TRUE)) {
      return(x)
    }

    m <- gregexpr(
      'src\\s*=\\s*["\\\']data:image/png;base64,[A-Za-z0-9+/=[:space:]]+["\\\']',
      x,
      perl = TRUE
    )[[1L]]

    if (identical(m, -1L)) {
      return(x)
    }

    attrs <- regmatches(x, list(m))[[1L]]
    new_attrs <- attrs

    for (i in seq_along(attrs)) {
      attr_text <- attrs[[i]]

      uri <- sub(
        '^src\\s*=\\s*["\\\'](data:image/png;base64,[A-Za-z0-9+/=[:space:]]+)["\\\']$',
        "\\1",
        attr_text,
        perl = TRUE
      )

      uri_norm <- normalize_data_uri(uri)
      payload <- sub("^data:image/png;base64,", "", uri_norm, perl = TRUE)
      dedup_key <- paste0("k_", rlang::hash(payload))

      if (exists(dedup_key, envir = dedup_env, inherits = FALSE)) {
        rel_file <- get(dedup_key, envir = dedup_env, inherits = FALSE)
      } else {
        img_raw <- jsonlite::base64_dec(payload)

        rel_file <- sprintf("img_%04d.png", get("next_id", envir = next_id_env))
        assign("next_id", get("next_id", envir = next_id_env) + 1L, envir = next_id_env)

        writeBin(img_raw, file.path(image_dir_fs, rel_file))
        assign(dedup_key, rel_file, envir = dedup_env)

        file_map <- get("file_map", envir = file_map_env)
        file_map[[length(file_map) + 1L]] <- list(
          file = file.path(image_dir_fs, rel_file),
          uri = uri_norm
        )
        assign("file_map", file_map, envir = file_map_env)
      }

      new_attrs[[i]] <- replace_src_attr(
        attr_text,
        file.path(image_dir_href, rel_file)
      )
    }

    regmatches(x, list(m)) <- list(new_attrs)
    x
  }

  out <- DashboardT
  dedup_env <- new.env(parent = emptyenv())
  file_map_env <- new.env(parent = emptyenv())
  next_id_env <- new.env(parent = emptyenv())

  assign("file_map", list(), envir = file_map_env)
  assign("next_id", 1L, envir = next_id_env)

  hits <- matrix(
    FALSE,
    nrow = nrow(out),
    ncol = ncol(out),
    dimnames = list(rownames(out), names(out))
  )

  for (j in seq_along(out)) {
    if (is.factor(out[[j]])) {
      out[[j]] <- as.character(out[[j]])
    }

    if (!is.character(out[[j]])) {
      next
    }

    col_hits <- !is.na(out[[j]]) & grepl("data:image/png;base64", out[[j]], fixed = TRUE)
    hits[, j] <- col_hits

    if (any(col_hits)) {
      out[[j]][col_hits] <- vapply(
        out[[j]][col_hits],
        process_cell,
        character(1L),
        dedup_env = dedup_env,
        file_map_env = file_map_env,
        next_id_env = next_id_env
      )
    }
  }

  file_map_list <- get("file_map", envir = file_map_env)
  file_map <- if (length(file_map_list)) {
    do.call(
      rbind,
      lapply(file_map_list, function(x) {
        data.frame(
          file = x$file,
          uri = x$uri,
          stringsAsFactors = FALSE
        )
      })
    )
  } else {
    data.frame(
      file = character(),
      uri = character(),
      stringsAsFactors = FALSE
    )
  }

  out
}
