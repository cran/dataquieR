odm_installed <- FALSE

odm_cache <- new.env(parent = emptyenv())

util_prune_odm_cache <- function() {
  rm(envir = odm_cache, list = ls(odm_cache, all.names = TRUE))
}

util_odm2dataquieR <- function(...) {
  key <- rlang::hash(list(...))
  if (exists(key, odm_cache)) {
    return(get(key, odm_cache))
  }
  odm2dataquieR <- function(...) {
    util_message(
      "Optional feature requires 'dataquieR2odm'. Get it from %s",
      sQuote("https://dataquality.qihs.uni-greifswald.de/DownloadR.html"));
    invisible(NULL)
  }
  if (odm_installed) {
    suppressWarnings(
      util_ensure_suggested("dataquieR2odm",
                            goal = "Read ODM files",
                            err = FALSE,
                            and_import = "odm2dataquieR"))
  }
  r <- odm2dataquieR(...)
  assign(key, r, odm_cache)
  r
}

util_is_odm_xml <- function(path, bytes = 65536L) {
  con <- file(path, open = "rb")
  on.exit(close(con), add = TRUE)

  x <- readChar(con, nchars = bytes, useBytes = TRUE)

  # UTF-8 BOM removal
  x <- sub("^\ufeff", "", x, useBytes = TRUE)

  # root element muss ODM sein
  if (!grepl("<\\s*([[:alnum:]_\\-]+:)?ODM\\b", x, perl = TRUE))
    return(FALSE)

  # any ODM namespace version is ok
  grepl("xmlns(:[[:alnum:]_\\-]+)?\\s*=\\s*['\"]http://www\\.cdisc\\.org/ns/odm/",
        x, perl = TRUE)
}
