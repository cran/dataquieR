.redcap_cache <- new.env(parent = emptyenv())
#' Interpret a `REDcap`-style rule and create an expression, that represents this rule
#'
#' @param rule [character] `REDcap` style rule
#' @param debug [integer] debug level (0 = off, 1 = log, 2 = breakpoints)
#' @param entry_pred [character] for debugging reasons: The production
#'        rule used entry point for the parser
#' @param must_eof [logical] if `TRUE`, expect the input to be `eof`, when the
#'        parser succeeded, fail, if not.
#'
#' @return [expression] the interpreted rule
#'
#' [`REDcap` rules 1](https://help.redcap.ualberta.ca/help-and-faq/project-best-practices/data-quality/example-data-quality-rules)
#' [`REDcap` rules 2](https://www.ctsi.ufl.edu/files/2017/06/Calculated-Fields-%E2%80%93-REDCap-How.pdf)
#' [`REDcap` rules 3](https://www.iths.org/wp-content/uploads/REDCap-Branching-Logic-2017-202.pdf)
#'
#'
#' For resolving left-recursive rules,
#' [StackOverflow](https://stackoverflow.com/a/9934631)
#' helps understanding the grammar below, just in case, theoretical computer
#' science is not right in your mind currently.
#'
#' @import qmrparser
#'
#' @examples
#' \dontrun{
#' #  rules:
#' # pregnancies <- 9999 ~ SEX == 'm' |  is.na(SEX)
#' # pregnancies <- 9998 ~ AGE < 12 |  is.na(AGE)
#' # pregnancies = 9999 ~ dist > 2 |  speed == 0
#'
#' data.frame(target = "SEX_0",
#'   rule = '[speed] > 5 and [dist] > 42 or 1 = "2"',
#'   CODE = 99999, LABEL = "PREGNANCIES_NOT_ASSESSED FOR MALES",
#'   class = "JUMP")
#' ModifyiedStudyData <- replace in SEX_0 where SEX_0 is empty, if rule fits
#' ModifyedMetaData <- add missing codes with labels and class here
#'
#' subset(study_data, eval(pregnancies[[3]]))
#'
#' rule <-
#'  paste0('[con_consentdt] <> "" and [sda_osd1dt] <> "" and',
#'  ' datediff([con_consentdt],[sda_osd1dt],"d",true) < 0')
#'
#' x <- data.frame(con_consentdt = c(as.POSIXct("2020-01-01"),
#'                 as.POSIXct("2020-10-20")),
#'                 sda_osd1dt = c(as.POSIXct("2020-01-20"),
#'                 as.POSIXct("2020-10-01")))
#' eval(util_parse_redcap_rule(paste0(
#'   '[con_consentdt] <> "" and [sda_osd1dt] <> "" and ',
#'   'datediff([con_consentdt],[sda_osd1dt],"d", "Y-M-D",true) < 10')),
#'   x, util_get_redcap_rule_env())
#'
#' util_parse_redcap_rule("[a] = 12 or [b] = 13")
#' cars[eval(util_parse_redcap_rule(
#'   rule = '[speed] > 5 and [dist] > 42 or 1 = "2"'), cars,
#'   util_get_redcap_rule_env()), ]
#' cars[eval(util_parse_redcap_rule(
#'   rule = '[speed] > 5 and [dist] > 42 or 2 = "2"'), cars,
#'   util_get_redcap_rule_env()), ]
#' cars[eval(util_parse_redcap_rule(
#'   rule = '[speed] > 5 or [dist] > 42 and 1 = "2"'), cars,
#'   util_get_redcap_rule_env()), ]
#' cars[eval(util_parse_redcap_rule(
#'   rule = '[speed] > 5 or [dist] > 42 and 2 = "2"'), cars,
#'   util_get_redcap_rule_env()), ]
#' util_parse_redcap_rule(rule = '(1 = "2" or true) and (false)')
#' eval(util_parse_redcap_rule(rule =
#'   '[dist] > sum(1, +(2, [dist] + 5), [speed]) + 3 + [dist]'),
#' cars, util_get_redcap_rule_env())
#' }
#'
#' @family parser_functions
#' @concept metadata_management
#' @keywords internal

util_parse_redcap_rule <- function(rule, debug = 0, entry_pred = "REDcapPred",
                                   must_eof = FALSE) {
  util_expect_scalar(must_eof, check_type = is.logical)

  if (!exists("f", .redcap_cache)) {
    f <- function(rule, debug = 0, entry_pred = "REDcapPred",
                  must_eof = must_eof) {

      empty_part <- structure(list(), class = "NULL")
      empty_part_qmr <- list(type = "empty", value = "")
      separator <- structure(list(), class = "separator")

      remove_empty_part <- function(s) {
        if (is.symbol(s)) {
          return(s)
        }
        if (identical(s, empty_part)) return(NULL)
        res <- s[!vapply(s, identical, empty_part, FUN.VALUE = logical(1))]
        if (identical(res, empty_part_qmr)) return(NULL)
        res <- res[!vapply(res, identical, empty_part_qmr, FUN.VALUE = logical(1))]
        res[vapply(res, length, FUN.VALUE = integer(1)) > 1] <-
          lapply(res[vapply(res, length, FUN.VALUE = integer(1)) > 1],
                 remove_empty_part)
        res
      }

      shift <- function(x) {
        obj_name <- force(as.character(substitute(x)))
        obj <- get(obj_name, envir = parent.frame())
        if (length(obj) > 0)
          r <- obj[[1]]
        else
          r <- NULL
        obj <- tail(obj, -1)
        assign(obj_name, obj, envir = parent.frame())
        r
      }

      tag <- function(x) {
        ex <- substitute(x)
        if (debug > 0 && length(ex) == 3 && is.symbol(ex[[2]]) && ex[[1]] == as.symbol("<-")) {
          fname <- util_deparse1(ex[[2]])
          x <- eval.parent(ex)
          if (is.function(x)) {
            y <- function(...) {
              if (debug >= 2) message(sprintf("-- %s", fname))
              args <- list(...)
              if (length(body(x)) > 0 && "action" %in% names(body(x))) {
                tagged_action <- body(x)[["action"]]
                body(x)[["action"]] <- function(...) {
                  if (debug >= 1) message(sprintf(">> %s", fname))
                  if (debug >= 3) browser()
                  r <- eval(parent.env(environment())$tagged_action)(...)
                  if (debug >= 1) message(sprintf(">> %s", dQuote(util_deparse1(r))))
                  r
                }
              } else if (length(body(x)) > 0) {
                if (body(x)[[1]] == as.symbol("{")) { stop("FIXME") # FIXME: this does not work porperly
                  tagged_action <- formals(eval(body(x)[[2]][[1]]))$action
                  body(x)[[2]][["action"]] <- function(...) {
                    if (debug >= 1) message(sprintf(">> %s", fname))
                    if (debug >= 3) browser()
                    r <- eval(parent.env(environment())$tagged_action)(...)
                    if (debug >= 1) message(sprintf(">> %s", dQuote(util_deparse1(r))))
                    r
                  }
                } else {
                  tagged_action <- formals(eval(body(x)[[1]]))$action
                  body(x)[["action"]] <- function(...) {
                    if (debug >= 1) message(sprintf(">> %s", fname))
                    if (debug >= 3) browser()
                    r <- eval(parent.env(environment())$tagged_action)(...)
                    if (debug >= 1) message(sprintf(">> %s", dQuote(util_deparse1(r))))
                    r
                  }
                }
              }
              r <- do.call(x, args, quote = TRUE)
              r
            }
            assign(x = fname,
                   value = y,
                   envir = parent.frame())
          } else {
            y <- x
          }
        } else {
          y <- eval.parent(ex)
        }
        y
      }

      tag(boolean <- function()
        alternation(
          keyword("true"),
          keyword("false"),
          action = function(s) {
            if (debug >= 1) message("boolean")
            as.logical(s$value)
          }
        )
      )

      tag(dash <- function()
        keyword("-")
      )

      tag(digit <- function()
        alternation(
          keyword("1"),
          keyword("2"),
          keyword("3"),
          keyword("4"),
          keyword("5"),
          keyword("6"),
          keyword("7"),
          keyword("8"),
          keyword("9"),
          keyword("0"),
          action = function(s) {
            if (debug >= 1) message("digit")
            s$value
          })
      )

      tag(digit012345 <- function()
        alternation(
          keyword("1"),
          keyword("2"),
          keyword("3"),
          keyword("4"),
          keyword("5"),
          keyword("0"),
          action = function(s) {
            if (debug >= 1) message("digit012345")
            s$value
          })
      )

      tag(digit0123 <- function()
        alternation(
          keyword("1"),
          keyword("2"),
          keyword("3"),
          keyword("0"),
          action = function(s) {
            if (debug >= 1) message("digit0123")
            s$value
          })
      )

      tag(digit012 <- function()
        alternation(
          keyword("1"),
          keyword("2"),
          keyword("0"),
          action = function(s) {
            if (debug >= 1) message("digit012")
            s$value
          })
      )

      tag(digit01 <- function()
        alternation(
          keyword("1"),
          keyword("0"),
          action = function(s) {
            if (debug >= 1) message("digit01")
            s$value
          })

      )

      tz_parsers <- lapply(c(
        OlsonNames()
        ), keyword)

      tag(tz_parser <- function() do.call(alternation, c(
        tz_parsers,
        list(action = function(s) s$value))))

      # TODO: maybe, we need to downgrade this to string, since REDcap seems unaware of a datetime data type, it uses strings and converts them later, if needed
      # One way could be to write our dates w/o quotes or in single quotes, backticks or whatever, and to support then also strings in the datediff function -- maybe add
      # some converter function string to date. Then, also datediff has to be rewritten in util_get_redcap_rule_env. See also tag: 1893839
      dt_general <- function(no_brackets = FALSE) alternation(
        concatenation(
          if (!no_brackets) keyword("[", action = function(s) empty_part) else empty(action = function(s) {empty_part}),
          digit(), digit(), digit(), digit(),
          dash(),
          digit01(), digit(),
          dash(),
          digit0123(), digit(),
          alternation(concatenation(
            whitespace(),
            ignore_ws,
            digit012(),
            digit(),
            keyword(":"),
            digit012345(),
            digit(),
            keyword(":"),
            digit012345(),
            digit(),
            option(
              concatenation(
                whitespace(),
                ignore_ws,
                tz_parser()
              )
            )
          ), empty(action = function(s) empty_part)),
          if (!no_brackets) keyword("]", action = function(s) empty_part) else empty(action = function(s) {empty_part}),
          action = function(s) {
            s <- remove_empty_part(s)
            s_str <- unlist(s)[!grepl("type$", perl = TRUE, names(unlist(s)))]
            if (any(unlist(s) == "option")) {
              tz0 <- unname(tail(unlist(s), 1))
              if (tz0 != "option") {
                tz <- tz0
                s_str <- head(s_str, -2)
              } else {
                tz <- ""
              }
              rm("tz0")
            } else {
              tz <- ""
            }
            call("as.POSIXct", paste(s_str, collapse = ""), tz = tz)
          }
        ),
        keyword("\"today\"", action = function(s) {
          call("as.POSIXct", Sys.Date())
        }),
        action = function(s) {
          if (debug >= 1) message("datetime")
          remove_empty_part(s)
        }
      )

      dt_no_brackets <- dt_general

      formals(dt_no_brackets)$no_brackets <- TRUE

      tag(datetime <- dt_general)

      tag(datetime_no_brackets <- dt_no_brackets)


      tag(nan <- function() keyword('"NaN"', action = function(s) {
        if (debug >= 1) message("nan")
        ""
      }))

      tag(set <- function() concatenation(
                                          charParser("{"),
                                          ignore_ws,
                                          arg_part(),
                                          ignore_ws,
                                          charParser("}"),
                                          action = function(s0) {
                                            s <- s0
                                            s <- remove_empty_part(s)
                                            do.call(call, c(list(name = "set"), s[[2]]), quote = TRUE)
                                          }
      ))

      tag(interval <- function() concatenation(
        alternation(charParser("("), charParser("["), action = function(s0) {
          s <- s0
          s <- remove_empty_part(s)
          s$value
        }),
        ignore_ws,
        alternation(
                    datetime_no_brackets(),
                    keyword("Inf", action = function(s0) {Inf}),
                    keyword("+Inf", action = function(s0) {Inf}),
                    keyword("-Inf", action = function(s0) {-Inf}),
                    term_expression(),#TODO: need alternative datetime rule here (w/o square brackets)
                    empty(action = function(s) {-Inf}),
                    action = function(s0) {
                      s <- s0
                      s <- remove_empty_part(s)
                      s
                    }),
        ignore_ws,
        alternation(charParser(";"), charParser(","),
                    action = function(s0) empty_part),
        ignore_ws,
        alternation(
                    datetime_no_brackets(),
                    keyword("Inf", action = function(s0) {Inf}),
                    keyword("+Inf", action = function(s0) {Inf}),
                    keyword("-Inf", action = function(s0) {-Inf}),
                    term_expression(),
                    empty(action = function(s) {+Inf}),
                    action = function(s0) {
                      s <- s0
                      s <- remove_empty_part(s)
                      s
                    }),
        ignore_ws,
        alternation(charParser(")"), charParser("]"), action = function(s0) {
          s <- s0
          s <- remove_empty_part(s)
          s$value
        }),
        action = function(s0) {
          s <- s0
          s <- remove_empty_part(s)
          r <- unlist(s, recursive = FALSE, use.names = FALSE)
          do.call(call, c(list(name = "interval",
                               inc_l = r[[1]] == "[",
                               low = r[[2]],
                               upp = r[[3]],
                               inc_u = r[[4]] == "]")), quote = TRUE)
        }
      ))

      tag(literal <- function()
        alternation(
          set(),
          interval(),
          nan(),
          datetime(),
          string(),
          numberFloat(),
          numberInteger(),
          numberNatural(),
          numberScientific(),
          boolean(),
          action = function(s) {
            if (debug >= 1) message("literal")
            if (!is.list(s))
              return(s)

            s <- remove_empty_part(s)
            if (s$type == "datetime") {
              # as.POSIXct(s$value)
              call("as.POSIXct", s$value)
            } else if (s$type == "string") {
              as.character(s$value)
            } else if (s$type %in% c("numberInteger", "numberNatural")) {
              as.integer(s$value)
            } else if (s$type %in% c("numberFloat", "numberScientific")) {
              as.numeric(s$value)
              # } else if (s$type == "boolean") {
              #   as.logical(s$value)
            }

          }
        )
      )

      tag(symbol_expression <- function()
        concatenation(keyword("["), symbolic(charFirst = function(ch) isLetter(ch) || ch == "_" || ch == ".", charRest = function(ch) isLetter(ch) ||
                                               isDigit(ch) || ch == "-" || ch == "_" || ch == "."), keyword("]"),
                      action = function(s) {
                        if (debug >= 1) message("symbol_expression")
                        s <- remove_empty_part(s)
                        as.symbol(s[[2]]$value)
                      })
      )

      tag(REDcapPred <- function() concatenation( # TODO: simplify
        alternation(
          expression()
        ),
        eofMark(),
        action = function(s) {
          if (debug >= 1) message("REDcapPred")
          s <- remove_empty_part(s)
          s[[1]]$value
        }))

      keywordfunction <- function(fname) {
        kwf <- function(...) {
          if (debug >= 2) message(sprintf("-- %s", fname))
          rf <- keyword(fname, action = function(s) {
            if (debug >= 1) message(sprintf(">> %s", fname))
            if (debug >= 3) browser()
            r <- list(type = "keyword", value = s)
            if (debug >= 1) message(sprintf(">> %s", dQuote(util_deparse1(r))))
            r
          })
          r <- rf(...)
          r
        }
        kwf
      }

      redcap_rule_env <- util_get_redcap_rule_env()

      functions <- unique(setdiff(names(redcap_rule_env), # but only, if ** is in names
                                  c("(")))

      get_internal <- function(fname) {
        r <- as.logical(attr(redcap_rule_env[[fname]], "internal"))
        if (length(r) == 0) {
          r <- FALSE
        }
        r
      }

      internal <- vapply(setNames(nm = functions), get_internal,
                          FUN.VALUE = integer(1))

      functions <- functions[!internal]

      get_order <- function(fname) {
        r <- as.integer(attr(redcap_rule_env[[fname]], "order"))
        if (length(r) == 0) {
          r <- 0L
        }
        r
      }

      get_prio <- function(fname) {
        r <- as.integer(attr(redcap_rule_env[[fname]], "prio"))
        if (length(r) == 0) {
          r <- -3L
        }
        r
      }

      fkt_order <- vapply(setNames(nm = functions), get_order,
                         FUN.VALUE = integer(1))

      functions <- functions[order(fkt_order)]

      op_prio <- vapply(setNames(nm = functions), get_prio,
                        FUN.VALUE = integer(1))
      supported_fkt <- lapply(functions, keywordfunction)
      supported_fkt <- append(supported_fkt,
                              empty(action =
                                      function(s) {
                                        list(type = "empty", value = "identity")
                                      }))
      op_prio <- c(op_prio, -3L)

      fkt <- function() do.call(
        what = alternation,
        args = append(supported_fkt,
                      c(action = function(s) {
                        f <- s$value
                        if (debug >= 1) message(sprintf("fkt: %s", dQuote(f)))
                        if (f == "identity") {
                          return(empty_part)
                        } else {
                          as.symbol(f)
                        }
                      })
        )
      )

      op_far_below_lowest <- function() do.call( # TODO: rename these with a number in the name
        what = alternation,
        args = append(supported_fkt[op_prio == -2],
                      c(action = function(s) {
                        f <- s$value
                        if (debug >= 1) message(sprintf("fkt: %s", dQuote(f)))
                        if (f == "identity") {
                          return(empty_part)
                        } else {
                          as.symbol(f)
                        }
                      })
        )
      )

      op_below_lowest <- function() do.call(
        what = alternation,
        args = append(supported_fkt[op_prio == -1],
                      c(action = function(s) {
                        f <- s$value
                        if (debug >= 1) message(sprintf("fkt: %s", dQuote(f)))
                        if (f == "identity") {
                          return(empty_part)
                        } else {
                          as.symbol(f)
                        }
                      })
        )
      )


      op_lowest <- function() do.call(
        what = alternation,
        args = append(supported_fkt[op_prio == 0],
                      c(action = function(s) {
                        f <- s$value
                        if (debug >= 1) message(sprintf("fkt: %s", dQuote(f)))
                        if (f == "identity") {
                          return(empty_part)
                        } else {
                          as.symbol(f)
                        }
                      })
        )
      )

      op_low <- function() do.call(
        what = alternation,
        args = append(supported_fkt[op_prio == 1],
                      c(action = function(s) {
                        f <- s$value
                        if (debug >= 1) message(sprintf("fkt: %s", dQuote(f)))
                        if (f == "identity") {
                          return(empty_part)
                        } else {
                          as.symbol(f)
                        }
                      })
        )
      )

      op_high <- function() do.call(
        what = alternation,
        args = append(supported_fkt[op_prio == 2],
                      c(action = function(s) {
                        f <- s$value
                        if (debug >= 1) message(sprintf("fkt: %s", dQuote(f)))
                        if (f == "identity") {
                          return(empty_part)
                        } else {
                          as.symbol(f)
                        }
                      })
        )
      )


      op_highest <- function() do.call(
        what = alternation,
        args = append(supported_fkt[op_prio == 3],
                      c(action = function(s) {
                        f <- s$value
                        if (debug >= 1) message(sprintf("fkt: %s", dQuote(f)))
                        if (f == "identity") {
                          return(empty_part)
                        } else {
                          as.symbol(f)
                        }
                      })
        )
      )

      tag(arg <- function() alternation(function_expression(),
                                        literal(),
                                        symbol_expression(),
                                        action = function(s) {
                                          if (debug >= 1) message(sprintf("arg %s", dQuote(paste0(deparse(s), collapse = " "))))
                                          if (is.list(s)) {
                                            s <- remove_empty_part(s)
                                            s$value
                                          } else {
                                            s
                                          }
                                        }))

      tag(arg_part <- function() alternation(
        concatenation(
          repetition1N(
            concatenation(ignore_ws,
                          term_expression(),
                          ignore_ws,
                          alternation(charParser(","),
                                      charParser(";"), action = function(s) {
                                        separator
                                      }),
                          ignore_ws
            )
          ),
          ignore_ws,
          term_expression(),
          ignore_ws
        ),
        concatenation(
          ignore_ws,
          term_expression(),
          ignore_ws
        ),
        ignore_ws,
        action = function(s0) {
          if (debug >= 1) message("arg_part")
          s <- remove_empty_part(s0$value)
          if (length(s) > 1 &&
              length(s[[1]]) > 1 &&
              identical(s[[1]]$type,
                        "repetition1N")) {
            rep <- s[[1]]$value
            rep <- lapply(rep,
                          function(t)
                            remove_empty_part(
                              t$value))
            rep <- lapply(
              rep,
              function(r) {
                r[!vapply(r, identical,
                          separator,
                          FUN.VALUE =
                            logical(1))]
              })
            res <- append(unlist(rep, recursive = FALSE), s[[2]])
          } else if (length(s) > 0) {
            res <- s[[1]]
          } else {
            res <- empty_part
          }
          #res <- unlist(lapply(res, deparse))
          #paste(res, collapse = ", ")
          res
        }
      ))

      tag(function_expression <- function() concatenation(fkt(), ignore_ws,
                                                          charParser("(", action = function(s) as.symbol("(")),
                                                          ignore_ws,
                                                          arg_part(),
                                                          ignore_ws,
                                                          charParser(")", action = function(s) as.symbol(")")),
                                                          action = function(s0) {
                                                            s <- s0
                                                            s <- remove_empty_part(s)

                                                            fkt <- gsub("`", "", fixed = TRUE, as.character(shift(s))) # TODO: use a better ways
                                                            if (debug >= 1) message(sprintf("function_expression: %s", sQuote(fkt)))
                                                            lng <- vapply(s, function(s_i) {
                                                              (length(s_i) == 1 && is.language(s_i))
                                                            }, FUN.VALUE = logical(1))
                                                            # val <- vapply(s, function(s_i) {
                                                            #   (length(s_i) == 2)
                                                            # }, FUN.VALUE = logical(1))
                                                            # s1 <- s
                                                            # s1[val] <- lapply(s[val], `[[`, "value")
                                                            # s[lng] <- lapply(s[lng], deparse)
                                                            r <- unlist(s, recursive = FALSE, use.names = FALSE)
                                                            # r <- r[!vapply(r, function(x) !is.language(x) && util_empty(x), FUN.VALUE = logical(1))]
                                                            # if (length(r) == 3 && is.language(r[[2]])) {
                                                            #   call(gsub("`", "", as.character(r[[2]])), r[[1]], r[[3]])
                                                            # } else {
                                                            #
                                                            # }
                                                            r <- r[!(r %in% c(as.symbol(")"), as.symbol("(")))]
                                                            do.call(call, c(list(name = fkt), r), quote = TRUE)
                                                          }
      ))

      action_subterm = function(s) {
        if (debug > 0) message(util_deparse1(sys.call()))
        s <- remove_empty_part(s)
        if (length(s)) {
          part <- function(x) {
            if (length(x) < 3) {
              if (length(x) == 1)
                x[[1]]
              else
                stop("Syntax error")
            } else {
              call(util_deparse1(x[[length(x)-1]]),
                   Recall(head(x, -2)),
                   x[[length(x)]]
              )
            }
          }
          if (length(s) > 1) {
             if (inherits(s[[1]], "POSIXct")) {
               s[[1]] <- call("as.POSIXct", list(as.character(s[[1]])))
              #   s[[1]] <- as.numeric(s[[1]])
             }
            ensure_brackets(part(c(s[[1]], s[[2]])))
          } else
            ensure_brackets(part(c(s[[1]])))
        } else
          empty_part
      }

      action_op <- function(s) {
        if (debug > 0) message(util_deparse1(sys.call()))
        s <- remove_empty_part(s)
        if (inherits(s[[2]], "POSIXct")) {
          s[[2]] <- call("as.POSIXct", list(as.character(s[[2]])))
        }
        c(s[[1]], s[[2]])
      }

      action_factor <- function(s) {
        if (debug > 0) message(util_deparse1(sys.call()))
        s <- remove_empty_part(s)
        if (!is.null(s))
          do.call("c", s$value)
        else
          empty_part # base::expression()
      }

      ensure_brackets <- function(x) {
        if (length(x) < 2) {
          x
        } else {
          if (!is.call(x) || x[[1]] == as.symbol("(")) {
            x
          } else {
            call("(", x)
          }
        }
      }

      tag( expression <- function() concatenation(
        alternation(op_far_below_lowest(), empty(action = function(s) {empty_part}), action = function(s) {
          s <- remove_empty_part(s)
          if (length(s) == 0)
            empty_part
          else
            s
        }), ignore_ws,
        term(), ignore_ws,
        repetition0N(concatenation(op_far_below_lowest(),  ignore_ws, term()),  ignore_ws,
                     action = function(s) {
                       s <- remove_empty_part(s)
                       if (!is.null(s))
                         do.call("c", lapply(s$value, `[[`, "value"))
                       else
                         empty_part # base::expression()
                     }),
        action = action_subterm)
      )


      tag( term <- function() concatenation(factor1(),  ignore_ws, repetition0N(concatenation(op_below_lowest(),  ignore_ws, factor1(), ignore_ws,
                                                                                              action = action_op),
                                                                                action = action_factor),
                                            action = action_subterm)
      )

      tag( factor1 <- function() concatenation(factor2(),  ignore_ws, repetition0N(concatenation(op_lowest(),  ignore_ws, factor2(),  ignore_ws,
                                                                                                 action = action_op),
                                                                                   action = action_factor),
                                               action = action_subterm)
      )

      tag( factor2 <- function() concatenation(factor3(), ignore_ws, repetition0N(concatenation(op_low(), ignore_ws, factor3(), ignore_ws,
                                                                                                action = action_op),
                                                                                  action = action_factor),
                                               action = action_subterm)
      )

      tag( factor3 <- function() concatenation(factor4(), ignore_ws, repetition0N(concatenation(op_high(), ignore_ws, factor4(), ignore_ws,
                                                                                                action = action_op),
                                                                                  action = action_factor),
                                               action = action_subterm)
      )

      tag( factor4 <- function() concatenation(factor5(), ignore_ws, repetition0N(concatenation(op_highest(), ignore_ws, factor5(), ignore_ws,
                                                                                                action = action_op),
                                                                                  action = action_factor),
                                               action = action_subterm)
      )

      tag( factor5 <- function() alternation(arg(),
                                             concatenation(
                                               charParser("(", action = function(s) empty_part),
                                               ignore_ws,
                                               expression(),
                                               ignore_ws,
                                               charParser(")", action = function(s) empty_part),
                                               action = function(s) {
                                                 do.call(call, c(
                                                   list(name = "("),
                                                   remove_empty_part(s)),
                                                   quote = TRUE)
                                               }),
                                             action = function(s) {
                                               s <- remove_empty_part(s)
                                               s
                                             })
      )

      term_expression <- expression

      # tag(term_expression <- function() alternation(
      #   term_expression_1(),
      #   arg(),
      #   action = function(s) {
      #     if (debug >= 1) message("term_expression")
      #     s
      #   }
      # ))


      ignore_ws <- whitespace(action = function(s) empty_part)

      # tag(term_expression_1 <- function() concatenation( # TODO: remove?
      #   alternation(function_expression(), arg()),
      #   ignore_ws,
      #   #      alternation(op_low()),
      #   alternation(
      #     op_highest(), # ^ * is prefix of **
      #     op_lowest(), # compare ops
      #     op_low(), # +
      #     op_high(), # *
      #     action = function(s) {
      #       s
      #     }
      #   ),
      #   ignore_ws,
      #   alternation(term_expression(), function_expression(), arg()),
      #   action = function(s) {
      #     if (debug >= 1) message("term_expression_1")
      #     s <- remove_empty_part(s)
      #     if(length(s) < 3) {
      #       stop(sprintf("two terms combined without an operator: %s",
      #                    dQuote(paste(s[[1]]$value,
      #                                 s[[2]]$value))))
      #     }
      #     call(as.character(s[[2]]),
      #          s[[1]]$value,
      #          s[[3]]$value
      #     )
      #   }
      # ))

      errorFun <- function(strmPosition,h=NULL,type="") {
        if ( is.null(h) || type != "concatenation" ) {# TODO: XXX
          warning(sprintf("Error from line %d, character %d in %s (will ignore this rule): %s",
                          strmPosition$line,
                          strmPosition$linePos,
                          rule,
                          substring(rule, strmPosition$streamPos)))
        } else {
          errorFun(h$pos,h$h,h$type)
        }
        return(list(type=type,pos=strmPosition,h=h))
      }

      # cstream <- get(entry_pred)()(streamParserFromString(sprintf("(%s)", rule))) # <ADDBR>, see below
      cstream <- get(entry_pred)()(streamParserFromString(sprintf("%s", rule)))

      if ( cstream$status == "fail" ) {
        invisible(errorFun(cstream$node$pos,cstream$node$h,cstream$node$type))
        return(util_attach_attr(list(), src = rule))
      } else if ( cstream$status != "ok" ) {
        warning(sprintf("Unknown error parsing %s: %s (will ignore this rule)", rule, cstream$status))
        return(util_attach_attr(list(), src = rule))
      } else {
        # r <- cstream$node[[2]] # 2 to remove the brackets added in <ADDBR>
        iseof <- !(cstream$stream$pos < cstream$stream$lenchar)
        if (!iseof && must_eof) {
          strmPosition <- streamParserPosition(cstream$stream)
          warning(sprintf("Found extra characters in line %d, character %d in %s (will ignore this rule): %s",
                          strmPosition$line,
                          strmPosition$linePos,
                          rule,
                          substring(rule, strmPosition$streamPos)))
          return(list())
        }
        # if (cstream$) {
        #
        # }
        r <- cstream$node
        if (debug > 0) message(r)
        if (is.symbol(r)) r <- as.expression(r)
        attr(r, "src") <- rule
        return(r)
      }
    }

    # parent.env(environment(f)) <- environment(empty)

    assign("f", f, .redcap_cache)
  }

  get("f", .redcap_cache)(rule = rule, debug = debug, entry_pred = entry_pred,
                          must_eof = must_eof)

}
