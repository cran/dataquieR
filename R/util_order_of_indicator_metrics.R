util_order_of_indicator_metrics <- function(indicator_metrics) {

  abbreviationMetrics <- util_get_concept_info("abbreviationMetrics")
  abbreviationMetrics$order <-
    rank(abbreviationMetrics$order)
  dqi <- util_get_concept_info("dqi")
  dqi$order_nr <-
    rank(dqi$order_nr)
  order_value <-
    vapply(indicator_metrics, FUN.VALUE = integer(1), FUN = function(x) {
      util_stop_if_not(length(x) == 1)
      nm <- strsplit(x, "_", fixed = TRUE)[[1]]
      if (length(nm) >= 2) {

        m <- head(subset(abbreviationMetrics, get("Abbreviation") == nm[[1]],
                         "order", drop = TRUE), 1)

        d <- head(subset(dqi, get("abbreviation") == paste(tail(nm, -1),
                                                           collapse = "_"),
                         "order_nr", drop = TRUE), 1)

        if (length(m) == length(d) && length(d) == 1 &&
            !util_empty(m) && !util_empty(d)) {
          as.integer(d * 100 + m)
        } else {
          NA_integer_
        }
      } else {
        NA_integer_
      }
    })

  order_value[is.na(order_value)] <-
    99999999

  rank(order_value)

}
