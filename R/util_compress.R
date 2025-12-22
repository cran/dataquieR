#' Compress an R object using available algorithms
#'
#' Serializes and compresses an R object using one of the available compression
#' algorithms supported by `memCompress()`. If no algorithm is specified, the
#' function selects the first available one from a predefined priority list.
#'
#' @param x Any R object to be serialized and compressed.
#' @param algo Character vector of candidate algorithms (e.g., `"xz"`, `"gzip"`,
#'   `"zstd"`, `"bzip2"`, `"none"`). If missing, defaults are chosen depending
#'   on the R version.
#'
#' @returns A raw vector of class `"compressed"` with attributes describing the
#'   method and algorithm used.
#'
#' @noRd
util_compress <- function(x, algo)  {
  #structure(memCompress(serialize(x, NULL, xdr = FALSE), "xz"),
  if (missing(algo)) {
    if (R.version$major > 4 || (R.version$major == 4 && R.version$minor >= 5))
      algo <- c("zstd", "gzip", "bzip2", "xz", "none")
    else
      algo <- c("gzip", "bzip2", "xz", "none")
  }
  algo <- head(intersect(
    algo,
    MEM_COMPRESS_CAPABILITIES), 1)
  structure(memCompress(serialize(x, NULL, xdr = FALSE), type = algo),
      class = "compressed",
      method = "memCompress",
      algo = algo)
  # qs::qserialize(x, preset = "high")
}

#' Decompress an object created by `util_compress()`
#'
#' Decompresses and un-serializes an object previously compressed with
#' `util_compress()`. Only objects of class `"compressed"` are supported.
#'
#' @param x A raw vector of class `"compressed"` produced by
#'   `util_compress()`.
#'
#' @returns The original R object restored after decompression.
#'
#' @noRd
util_decompress <- function(x)  {
  if (inherits(x, "compressed")) {
    algo <- attr(x, "algo")
    if (is.null(algo)) {
      algo <- head(MEM_COMPRESS_CAPABILITIES, 1)
    }
    unserialize(memDecompress(x, type = algo))
  } else {
    util_error("Decompression of objects of class %s unsupported",
               util_pretty_vector_string(class(x), quote = sQuote))
  }
  # qs::qdeserialize(r)
}

.util_mem_compress_capabilities <- function(types = c("gzip", "bzip2", "xz",
                                                      "zstd", "none")) {
  test_raw <- charToRaw("test")
  setNames(vapply(types, function(t) {
    !inherits(try(memCompress(test_raw, type = t), silent = TRUE), "try-error")
  }, logical(1)), types)
}

