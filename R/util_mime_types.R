util_fetch_ext <- function(url) {
  #ask the server for headers
  headers <- try(base::curlGetHeaders(url), silent = TRUE)
  if (inherits(headers, "try-error")) {
    return(attr(headers, "condition"))
  } else {
    # TODO: content-disposition
    #search for the content type
    mt <- grep("^content-type:", headers, value = TRUE, ignore.case = TRUE)
    if (length(mt) != 1) {
      return(errorCondition("No content-type header found"))
    }
  }
  #remove the content_type at the beginning of mime
  mt <- trimws(gsub("^content-type:", "", trimws(mt), ignore.case = TRUE))
  #delete everything after the ; including the ; (extra information)
  mt <- gsub(";.*$", "", mt)
  #look to the corresponding file extension from a table delivered with the
  # package (if more than 1 extension, takes the first)
  r <- head(unlist(strsplit(
    subset(.mime_chache$tb, get("Media Type") == mt, "Extensions", drop = TRUE),
    "\\s",
  )), 1)
  #get the file name from the header content-disposition
  file_name <- grep("^content-disposition", headers, value = TRUE,
                    ignore.case = TRUE)
  if (length(file_name) == 1 && is.character(file_name)) {
    # see https://httpwg.org/specs/rfc6266.html#n-grammar
    # and https://www.rfc-editor.org/rfc/rfc5987.html#section-3.2
    if (grepl("filename*=", tolower(file_name), fixed = TRUE)) {
      file_name <- utils::URLdecode(
        gsub(".*filename\\*\\s*=\\s*.*[^']*'[^']*'([^;]*).*$", "\\1",
             file_name, ignore.case = TRUE)) # TODO:  we ignore charset and language
      attr(r, "file-name") <- file_name
    } else if (grepl("filename=", tolower(file_name), fixed = TRUE)) {
      file_name <-
        gsub("^.*filename\\s*=\\s*\"(.*)\".*$", "\\1", file_name,
             ignore.case = TRUE)
      attr(r, "file-name") <- file_name
    }
  }
  r
}

.mime_chache <- new.env(parent = emptyenv())

local({
  f <- system.file("mimetypes", "media-types.RDS", package = "dataquieR")
  if (0 != file.access(f, 4)) {
    util_error("Cannot read file %s. Internal error.", dQuote(f))
  }
  tb <- readRDS(f)
  assign("tb", tb, envir = .mime_chache)
})
