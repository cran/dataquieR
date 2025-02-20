test_that("util_finalize_sizing_hints works", {
  skip_on_cran()
  skip_if_not_installed("jsonlite")

  sh <- jsonlite::parse_json(
    '{"figure_type_id": "scatt_plot",
      "range": 63,
      "number_of_vars": 1,
      "no_char_y":3
    }')

  sh <- util_finalize_sizing_hints(sh)

  expect_equal(sh$w_in_cm, 21.5, tolerance = 0.1)
  expect_equal(sh$h_in_cm, 9.3, tolerance = 0.1)

  expect_true(any(
    grepl("atomic vectors",
          capture.output(sh <- util_finalize_sizing_hints("x"),
                         type = "message")
    ))
  )

  sh <- jsonlite::parse_json(
    '{"figure_type_id": "pass_through",
      "w": "10px"
    }')

  expect_message(sh <- util_finalize_sizing_hints(sh),
                         regexp = "sizes in pixels")

  expect_equal(sh$w_in_cm, 0.26, tolerance = 0.1)

  sh <- jsonlite::parse_json(
    '{"figure_type_id": "pass_through",
      "w": "10pt"
    }')

  sh <- util_finalize_sizing_hints(sh)

  expect_equal(sh$w_in_cm, 0.35, tolerance = 0.1)

  sh <- jsonlite::parse_json(
    '{"figure_type_id": "pass_through",
      "w": "10pc"
    }')

  sh <- util_finalize_sizing_hints(sh)

  expect_equal(sh$w_in_cm, 4.2, tolerance = 0.1)

  sh <- jsonlite::parse_json(
    '{"figure_type_id": "pass_through",
      "w": "1em"
    }')

  sh <- util_finalize_sizing_hints(sh)

  expect_equal(sh$w_in_cm, 0.4, tolerance = 0.1)

  sh <- jsonlite::parse_json(
    '{"figure_type_id": "pass_through",
      "w": "1ex"
    }')

  sh <- util_finalize_sizing_hints(sh)

  expect_equal(sh$w_in_cm, 0.4, tolerance = 0.1)

  sh <- jsonlite::parse_json(
    '{"figure_type_id": "pass_through",
      "w": "1ch"
    }')

  sh <- util_finalize_sizing_hints(sh)

  expect_equal(sh$w_in_cm, 0.4, tolerance = 0.1)

  sh <- jsonlite::parse_json(
    '{"figure_type_id": "pass_through",
      "w": "1rem"
    }')

  sh <- util_finalize_sizing_hints(sh)

  expect_equal(sh$w_in_cm, 0.4, tolerance = 0.1)

  sh <- jsonlite::parse_json(
    '{"figure_type_id": "pass_through",
      "w": "1vw"
    }')

  sh <- util_finalize_sizing_hints(sh)

  expect_equal(sh$w_in_cm, 48.8, tolerance = 0.1)

  sh <- jsonlite::parse_json(
    '{"figure_type_id": "pass_through",
      "w": "1vh"
    }')

  sh <- util_finalize_sizing_hints(sh)

  expect_equal(sh$w_in_cm, 27.4, tolerance = 0.1)

  sh <- jsonlite::parse_json(
    '{"figure_type_id": "pass_through",
      "w": "1vmin"
    }')

  sh <- util_finalize_sizing_hints(sh)

  expect_equal(sh$w_in_cm, 27.4, tolerance = 0.1)

  sh <- jsonlite::parse_json(
    '{"figure_type_id": "pass_through",
      "w": "1vmax"
    }')

  sh <- util_finalize_sizing_hints(sh)

  expect_equal(sh$w_in_cm, 48.8, tolerance = 0.1)

  sh <- jsonlite::parse_json(
    '{"figure_type_id": "pass_through",
      "w": "1%"
    }')

  sh <- util_finalize_sizing_hints(sh)

  expect_equal(sh$w_in_cm, 48.8, tolerance = 0.1)

  sh <- jsonlite::parse_json(
    '{"figure_type_id":"dot_loess",
      "range":28.2342,
      "no_char_y":6,
      "n_groups":14
    }')

  sh <- util_finalize_sizing_hints(sh)

  expect_equal(sh$w_in_cm, 25.4, tolerance = 0.1)

})
