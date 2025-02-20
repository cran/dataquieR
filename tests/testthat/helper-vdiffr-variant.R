if (requireNamespace("vdiffr", quietly = TRUE) &&
    packageVersion("vdiffr") >= "1.0.8" &&
    "variant" %in% names(formals(vdiffr::expect_doppelganger))) {
  expect_doppelganger2 <- vdiffr::expect_doppelganger
  formals(expect_doppelganger2)$variant <- prep_get_variant()
} else { # here, the variant formal was missing.
  expect_doppelganger2 <- function (title, fig, path = deprecated(), ...,
                                    writer = write_svg,
                                    cran = FALSE, variant =
                                      prep_get_variant())
  { # see vdiffr::expect_doppelganger, ugly patch to avoid https://github.com/r-lib/vdiffr/issues/125
    # this file is a patched version from vdiffr (MIT License, original authors: see packageDescription("vdiffr"))
    testthat::local_edition(3)
    fig_name <- str_standardise(title)
    file <- paste0(fig_name, ".svg")
    testthat::announce_snapshot_file(name = file)
    testcase <- make_testcase_file(fig_name)
    writer(fig, testcase, title)
    if (!missing(...)) {
      lifecycle::deprecate_soft("1.0.0", "vdiffr::expect_doppelganger(... = )",
      )
    }
    if (lifecycle::is_present(path)) {
      lifecycle::deprecate_soft("1.0.0", "vdiffr::expect_doppelganger(path = )",
      )
    }
    if (is_graphics_engine_stale()) {
      testthat::skip(paste_line("The R graphics engine is too old.",
                                "Please update to R 4.1.0 and regenerate the vdiffr snapshots."))
    }
    withCallingHandlers(testthat::expect_snapshot_file(variant = variant,
                                                       testcase, name = file, cran = cran, compare = testthat::compare_file_text),
                        expectation_failure = function(cnd) {
                          if (is_snapshot_stale(title, testcase)) {
                            testthat::skip(paste_line("SVG snapshot generated under a different vdiffr version.",
                                                      i = "Please update your snapshots."))
                          }
                          if (!is_null(snapshotter <- get_snapshotter())) {
                            path_old <- snapshot_path(snapshotter, file)
                            path_new <- snapshot_path(snapshotter, paste0(fig_name,
                                                                          ".new.svg"))
                            if (all(file.exists(path_old, path_new))) {
                              push_log(fig_name, path_old, path_new)
                            }
                          }
                        })
  }
  if (requireNamespace("vdiffr", quietly = TRUE)) {
    environment(expect_doppelganger2) <- asNamespace("vdiffr")
  }
}
