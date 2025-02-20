test_that("util_queue_cluster_setup works", {
  skip_on_cran() # runs parallel
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  q <- util_queue_cluster_setup(1, function(...) {}, FALSE, NULL)
  r1 <- q$compute_report(all_calls = list(a = call("ls"),
                                          b = call("search"),
                                          c = call("class", as.symbol("dt0"))),
                         worker = function(exp, env, nm, my_storr_object) {
                           eval(exp, env)
                         },
                         step = 1)
  dt0 <- cars
  q$export("dt0")
  q$workerEval(function(lib) {
    require(lib, character.only = TRUE)
  }, list(lib = "ggplot2"))
  r2 <- q$compute_report(all_calls = list(a = call("ls"),
                                         b = call("search"),
                                         c = call("class", as.symbol("dt0"))),
                        worker = function(exp, env, nm, my_storr_object) {
                          eval(exp, env)
                        },
                        step = 1)
  q <- NULL
  gc()
  expect_equal(setdiff(r2$b, r1$b), "package:ggplot2")
  expect_s3_class(r1$c, "try-error")
  expect_equal(conditionMessage(attr(r1$c, "condition")),
               "object 'dt0' not found")
  expect_equal(r2$c, "data.frame")
  expect_equal(r1$a, r2$a)
  expect_equal(r1$a, c("e", "nm"))
})
