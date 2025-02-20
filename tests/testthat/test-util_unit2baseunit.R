test_that("util_unit2baseunit works", {
  skip_on_cran() # OS dependent

  suppressMessages(util_unit2baseunit("%"))

  expect_equal(util_unit2baseunit("%"), "%")
  expect_equal(util_unit2baseunit("d%"), "%")

  # Invalid unit
  expect_length(util_unit2baseunit("aa%"), 0)
  expect_length(util_unit2baseunit("aa%", unique = FALSE), 0)

  expect_equal(util_unit2baseunit("a%"), "%")

  # Invalid unit
  expect_length(util_unit2baseunit("e%"), 0)
  expect_length(util_unit2baseunit("e%", unique = FALSE), 0)

  expect_equal(util_unit2baseunit("E%"), "%")
  expect_equal(util_unit2baseunit("Eg"), "g")

  # Invalid unit
  expect_length(util_unit2baseunit("E"), 0)
  expect_length(util_unit2baseunit("E", unique = FALSE), 0)

  expect_equal(util_unit2baseunit("EC"), "C")
  expect_equal(util_unit2baseunit("EK"), "K")
  expect_equal(util_unit2baseunit("µg"), "g")
  expect_equal(util_unit2baseunit("mg"), "g")
  expect_equal(util_unit2baseunit("°C"), "°C")
  expect_equal(util_unit2baseunit("k°C"), "°C")
  expect_equal(util_unit2baseunit("kK"), "K")
  expect_equal(util_unit2baseunit("nK"), "K")

  # Ambiguous units, if used with unique = FALSE
  expect_equal(util_unit2baseunit("kg"), "kg")
  expect_equal(util_unit2baseunit("cd"), "cd")
  expect_equal(util_unit2baseunit("Pa"), "Pa")
  expect_equal(util_unit2baseunit("kat"), "kat")
  expect_equal(util_unit2baseunit("min"), "min")

  # atto atom units or astronomical units, both in state "accepted"
  expect_length(util_unit2baseunit("au"), 0)
  expect_equal(util_unit2baseunit("au", unique = FALSE), c("u", "au"))

  # astronomical units or micro are, both in state "accepted"
  expect_length(util_unit2baseunit("ua"), 0)
  expect_equal(util_unit2baseunit("ua", unique = FALSE), c("ua", "a"))

  expect_equal(util_unit2baseunit("kt"), "t")

  # parts per trillion or pico US_liquid_pint, both in state "common",
  # but in this case, plain count units will be preferred
  expect_equal(util_unit2baseunit("ppt"), "ppt")
  expect_equal(util_unit2baseunit("ppt", unique = FALSE), c("ppt", "pt"))

  expect_equal(  util_unit2baseunit("ft"), "t")
  expect_equal(util_unit2baseunit("yd"), "d")
  expect_equal(util_unit2baseunit("pt"), "t")

  # actually the same, but both only common, and to my knowledge not-so-common
  # gram-force vs. kilogram-force (kilo pond)
  expect_length(util_unit2baseunit("kgf"), 0)
  expect_equal(util_unit2baseunit("kgf", unique = FALSE), c("kgf", "gf"))

  expect_equal(util_unit2baseunit("at"), "t")
  expect_equal(util_unit2baseunit("ph"), "h")
  expect_equal(util_unit2baseunit("nt"), "t")
})
