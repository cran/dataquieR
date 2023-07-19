#' Save a `dq_report2`
#'
#' @param report [dataquieR_resultset2] the report
#' @param file [character] the file name to write to
#' @param compression_level [integer]  from=0 to=9. Compression level.
#'        9 is very slow.
#'
#' @return `invisible(NULL)`
#' @export
prep_save_report <- function(report, file, compression_level = 3) {
  util_expect_scalar(file, check_type = is.character)
  if (!inherits(report, "dataquieR_resultset2")) {
    util_error("Can only save %s reports",
               sQuote("dq_report2"))
  }
  util_expect_scalar(compression_level,
                     check_type = util_is_integer)
  if (compression_level < 0 || compression_level > 9) {
    util_error("%s msut be an integer between 0 and 9, not %d",
               sQuote("compression_level"),
               compression_level)
  }
  zz <- gzcon(file(file, "wb"), level = compression_level)
  serialize(object = report, connection = zz, version = 3)
  close(zz)
  invisible(NULL)
}

#' Load a `dq_report2`
#'
#' @param file [character] the file name to load from
#'
#' @return [dataquieR_resultset2] the report
#' @export
prep_load_report <- function(file) {
  util_expect_scalar(file, check_type = is.character)
  util_stop_if_not(file.exists(file))
  zz <- gzcon(file(file, "rb"))
  report <- unserialize(zz)
  close(zz)
  report
}
