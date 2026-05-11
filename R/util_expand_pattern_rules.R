# util_expand_pattern_rules.R
# This file contains a helper to expand pattern-based variable references
# inside cross-item rules. Only content inside square brackets [ ... ] is
# interpreted as a pattern. Everything else is left unchanged.

util_expand_pattern_rules <- function(pattern_rules, valid_names) {

  e <- environment()

  mismatches <- list()

  # Basic input checks
  util_stop_if_not(is.character(pattern_rules), is.character(valid_names))
  if (any(inv <- grepl("[][]", valid_names))) {
    util_warning("valid names must not contain '[' or ']'.",
                 applicability_problem = TRUE)
    valid_names <- valid_names[!inv]
  }

  # Escape regex meta characters to ensure user input cannot alter regex logic
  .rx_escape_chr <- function(x) {
    gsub("([][{}()+*?^$|\\\\.,])", "\\\\\\1", x, perl = TRUE)
  }

  # Read escaped character (e.g. \*, \?, \#)
  .read_escaped <- function(x, i) {
    n <- nchar(x)
    if (i < n && substr(x, i, i) == "\\") {
      list(value = substr(x, i + 1L, i + 1L), next_i = i + 2L)
    } else {
      list(value = substr(x, i, i), next_i = i + 1L)
    }
  }

  # Find closing brace for capture definitions { ... }
  .find_unescaped <- function(x, char, start) {
    n <- nchar(x)
    i <- start
    while (i <= n) {
      if (substr(x, i, i) == "\\") {
        i <- i + 2L
      } else if (substr(x, i, i) == char) {
        return(i)
      } else {
        i <- i + 1L
      }
    }
    NA_integer_
  }

  # Resolve capture references such as {W}, {W-1}, or {W+1}.
  # Arithmetic is only attempted for integer capture values.
  .resolve_capture_reference <- function(cap_expr, captures) {
    m <- regexec("^([A-Za-z][A-Za-z0-9_]*)([+-][0-9]+)?$", cap_expr, perl = TRUE)
    mm <- regmatches(cap_expr, m)[[1L]]

    if (!length(mm)) {
      util_error(sprintf("Invalid capture reference {%s}.", cap_expr), call. = FALSE)
    }

    ref_name <- mm[[2L]]
    offset <- if (length(mm) >= 3L && nzchar(mm[[3L]])) {
      as.integer(mm[[3L]])
    } else {
      0L
    }

    if (is.null(captures[[ref_name]])) {
      util_error("Capture {%s} used before being defined.", ref_name)
    }

    ref_value <- captures[[ref_name]]

    if (offset != 0L) {
      if (!grepl("^-?[0-9]+$", ref_value)) {
        return(NULL)
      }
      ref_value <- as.character(as.integer(ref_value) + offset)
    }

    ref_value
  }

  # Translate wildcard mini-language into regex
  .compile_fragment <- function(x) {
    rx <- character()
    i <- 1L
    n <- nchar(x)

    while (i <= n) {
      ch <- substr(x, i, i)
      ch2 <- if (i < n) substr(x, i, i + 1L) else ""

      if (ch == "\\") {
        esc <- .read_escaped(x, i)
        rx <- c(rx, .rx_escape_chr(esc$value))
        i <- esc$next_i
      } else if (ch2 == "#?") {
        rx <- c(rx, "[0-9]{1,2}")
        i <- i + 2L
      } else if (ch2 == "##") {
        rx <- c(rx, "[0-9]{2}")
        i <- i + 2L
      } else if (ch == "#") {
        rx <- c(rx, "[0-9]")
        i <- i + 1L
      } else if (ch == "*") {
        rx <- c(rx, ".*")
        i <- i + 1L
      } else if (ch == "?") {
        rx <- c(rx, ".")
        i <- i + 1L
      } else {
        rx <- c(rx, .rx_escape_chr(ch))
        i <- i + 1L
      }
    }

    paste0(rx, collapse = "")
  }

  # Compile full variable pattern with optional capture groups
  .compile_var_pattern <- function(pattern, captures = list()) {
    rx <- character()
    capture_names <- character()
    pattern_invalid <- FALSE
    i <- 1L
    n <- nchar(pattern)

    while (i <= n) {
      ch <- substr(pattern, i, i)

      if (ch == "\\") {
        esc <- .read_escaped(pattern, i)
        rx <- c(rx, .rx_escape_chr(esc$value))
        i <- esc$next_i
      } else if (ch == "{") {
        close <- .find_unescaped(pattern, "}", i + 1L)
        if (is.na(close)) util_error("Unclosed capture in pattern")

        inner <- substr(pattern, i + 1L, close - 1L)
        def <- strsplit(inner, ":", fixed = TRUE)[[1L]]
        cap_name <- def[[1L]]

        if (length(def) == 1L) {
          ref_value <- .resolve_capture_reference(cap_name, captures)

          if (is.null(ref_value)) {
            pattern_invalid <- TRUE
            rx <- c(rx, "(?!)")
          } else {
            rx <- c(rx, .rx_escape_chr(ref_value))
          }
        } else if (!is.null(captures[[cap_name]])) {
          rx <- c(rx, .rx_escape_chr(captures[[cap_name]]))
        } else {
          rx <- c(rx, "(", .compile_fragment(def[[2L]]), ")")
          capture_names <- c(capture_names, cap_name)
        }

        i <- close + 1L
      } else {
        next_special <- regexpr("[\\\\{]", substr(pattern, i, n), perl = TRUE)[[1L]]
        end <- if (next_special < 0L) n else i + next_special - 2L
        rx <- c(rx, .compile_fragment(substr(pattern, i, end)))
        i <- end + 1L
      }
    }

    list(
      regex = paste0("^", paste0(rx, collapse = ""), "$"),
      capture_names = capture_names,
      pattern_invalid = pattern_invalid
    )
  }

  # Expand a single token against valid names
  .expand_token <- function(token, captures = list()) {
    compiled <- .compile_var_pattern(token, captures)

    hits <- valid_names[grepl(compiled$regex, valid_names, perl = TRUE)]

    if (!length(hits)) {
      return(structure(list(), pattern_invalid = compiled$pattern_invalid))
    }

    lapply(hits, function(nm) {
      new_captures <- captures

      if (length(compiled$capture_names)) {
        m <- regexec(compiled$regex, nm, perl = TRUE)
        vals <- regmatches(nm, m)[[1L]][-1L]

        for (i in seq_along(compiled$capture_names)) {
          new_captures[[compiled$capture_names[[i]]]] <- vals[[i]]
        }
      }

      list(name = nm, captures = new_captures)
    })
  }

  # Expand full rule
  .extract_bracket_tokens <- function(rule) {
    m <- gregexpr("\\[[^][]+\\]", rule, perl = TRUE)
    tokens <- regmatches(rule, m)[[1L]]
    if (!length(tokens)) return(character(0))
    sub("^\\[", "", sub("\\]$", "", tokens))
  }

  .expand_rule <- function(rule) {
    tokens <- .extract_bracket_tokens(rule)
    states <- list(list(text = rule, captures = list()))

    for (token in tokens) {
      new_states <- list()
      token_had_hit <- FALSE

      for (state in states) {
        hits <- .expand_token(token, state$captures)

        if (length(hits) || isTRUE(attr(hits, "pattern_invalid"))) {
          token_had_hit <- TRUE
        }

        for (hit in hits) {
          new_states[[length(new_states) + 1L]] <- list(
            text = sub(
              paste0("[", token, "]"),
              paste0("[", hit$name, "]"),
              state$text,
              fixed = TRUE
            ),
            captures = hit$captures
          )
        }
      }

      if (!token_had_hit) {
        assign("mismatches",
               c(e$mismatches, token),
               envir = e)
      }

      states <- new_states
      if (!length(states)) break
    }

    unique(vapply(states, `[[`, character(1), "text"))
  }

  res <- force(unlist(lapply(pattern_rules, .expand_rule), use.names = FALSE))

  util_attach_attr(
    res,
    mismatches = unique(mismatches)
  )
}
