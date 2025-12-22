util_readr_cols_from_metadata <- function(meta_data) {
  util_expect_data_frame(meta_data, c(VAR_NAMES, DATA_TYPE))
  r <- setNames(readr_coltypes[trimws(tolower(meta_data$DATA_TYPE))],
           nm = meta_data$VAR_NAMES)
  unkown_dt <- vapply(r, is.null, FUN.VALUE = logical(1))
  if (any(unkown_dt)) {
    util_warning(
      c("For the following variables, the %s is not one of %s: %s\n",
        "--> falling back to %s"),
      sQuote(DATA_TYPE),
      util_pretty_vector_string(DATA_TYPES),
      prep_deparse_assignments(names(which(unkown_dt)),
                               meta_data$DATA_TYPE[unkown_dt],
                               mode = "string_codes"),
      dQuote(DATA_TYPES$STRING),
      applicability_problem = TRUE
    )
    r[unkown_dt] <- lapply(r[unkown_dt], function(x) readr::col_character())
  }
  r
}

readr_coltypes <-
  list(DATA_TYPES$INTEGER, readr::col_double(),
       DATA_TYPES$STRING, readr::col_character(),
       DATA_TYPES$FLOAT, readr::col_double(),
       DATA_TYPES$DATETIME, readr::col_datetime(),
       DATA_TYPES$TIME, readr::col_time())

readr_coltypes <-
  setNames(readr_coltypes[2*seq_len(length(readr_coltypes)/2)],
           nm = readr_coltypes[2*seq_len(length(readr_coltypes)/2) - 1])

util_adjust_data_type2 <- function(study_data,
                                   meta_data,
                                   relevant_vars_for_warnings = character(0),
                                   language_code =
                                     getOption("dataquieR.locale",
                                               dataquieR.locale_default)) {
  util_expect_data_frame(study_data, keep_types = TRUE)
  if (length(relevant_vars_for_warnings) == 0) {
    relevant_vars_for_warnings <- colnames(study_data)
  }
  if (isTRUE(attr(study_data, "Data_type_matches"))) {
    return(study_data)
  }
  if (.called_in_pipeline && rlang::is_integerish(dynGet("cores")) &&
      as.integer(dynGet("cores")) > 1 &&
      suppressWarnings(util_ensure_suggested("parallel", err = FALSE))) {
    mycl <- parallel::makePSOCKcluster(as.integer(dynGet("cores")))
    parallel::clusterCall(mycl, library, "dataquieR", character.only = TRUE)
    parallel::clusterCall(mycl, loadNamespace, "hms")
    on.exit(parallel::stopCluster(mycl))
  } else {
    mycl <- NULL
  }
  for (cn in colnames(study_data)) {
    if (!is.null(study_data[[cn]])) {
      attr(study_data[[cn]], "..cn") <-
        cn
    }
  }
  if (isTRUE(as.logical(getOption("dataquieR.old_factor_handling",
                dataquieR.old_factor_handling_default)))) {
    # This is for backwards compatibility, but it may be more user friendly to
    # omit this in both adjust_data_type functions
    study_data[, vapply(study_data, is.factor, FUN.VALUE = logical(1))] <-
      lapply(study_data[,
                        vapply(study_data, is.factor, FUN.VALUE = logical(1)),
                        drop = FALSE], as.integer)
  } else {
    study_data[, vapply(study_data, is.factor, FUN.VALUE = logical(1))] <-
      util_par_lapply_lb(cl = mycl, study_data[,
                        vapply(study_data, is.factor, FUN.VALUE = logical(1)),
                        drop = FALSE], function(x) util_as_character(x))
  }
  study_data[] <- util_par_lapply_lb(cl = mycl,
                                     study_data, util_as_character)
  to_warn <- character(0)
  my_env <- environment()
  old_names <- names(study_data)
  no_name <- !nzchar(names(study_data))
  if (any(no_name)) {
    # find unused prefix
    prefix <- "#"
    while (any(startsWith(names(study_data), prefix))) {
      prefix <- paste0("#", prefix)
    }
    names(study_data)[no_name] <-
      paste0(prefix, seq_len(sum(no_name)))
  }
  e <- l10n_info()$codeset
  if (is.null(e))
    e <- "UTF-8" # no reliable way of detecting the current encoding
  locale <- readr::locale(
    date_names = language_code,
#    date_format = , # keep the default
#    time_format = , # keep the default
    decimal_mark = Sys.localeconv()[["decimal_point"]],
    grouping_mark = Sys.localeconv()[["thousands_sep"]],
    tz = Sys.timezone(),
    encoding = e,
    asciify = FALSE)
  locale$tz <- Sys.timezone()
  col_types <- util_readr_cols_from_metadata(meta_data) # also checks for VAR_NAMES and DATA_TYPE
  num <- intersect(colnames(study_data),
                     meta_data[meta_data[[DATA_TYPE]] %in% c(
                       DATA_TYPES$FLOAT,
                       DATA_TYPES$INTEGER),
                               VAR_NAMES])
  intg <- intersect(colnames(study_data),
                            meta_data[meta_data[[DATA_TYPE]] ==
                                        DATA_TYPES$INTEGER,
                              VAR_NAMES])

  datim <- intersect(colnames(study_data),
                     meta_data[meta_data[[DATA_TYPE]] == DATA_TYPES$DATETIME,
                               VAR_NAMES])
  if (length(intg) > 0) {
    study_data[, intg] <- util_par_lapply_lb(cl = mycl,
      study_data[, intg, drop = FALSE],
      function(cl) {
        r <- cl
        r <- trimws(tolower(r))
        r[r %in% c("t", "true")] <- "1"
        r[r %in% c("f", "false")] <- "1"
        r
      }
    )
  }
  if (length(datim) > 0) {
    study_data[, datim] <- util_par_lapply_lb(cl = mycl,
      study_data[, datim, drop = FALSE],
      function(cl) {
        r <- cl
        r <- # prepend 000 to one-digit years
          gsub("^(\\d)\\-(\\d\\d)\\-(\\d\\d)(\\W.*|)$", "000\\1-\\2-\\3\\4",
               r)
        r <- # prepend 00 to two-digit years
          gsub("^(\\d\\d)\\-(\\d\\d)\\-(\\d\\d)(\\W.*|)$", "00\\1-\\2-\\3\\4",
             r)
        r <- # prepend 0 to three-digit years
          gsub("^(\\d\\d\\d)\\-(\\d\\d)\\-(\\d\\d)(\\W.*|)$", "0\\1-\\2-\\3\\4",
               r)
        r
      }
    )
  }

  if (isTRUE(as.logical(getOption("dataquieR.type_adjust_parallel",
                       dataquieR.type_adjust_parallel_default)))) {
    processed_list <- util_par_lapply_lb(
      cl = mycl,
      X  = seq_along(study_data),
      fun = function(i, study_data, col_types, locale, relevant_vars_for_warnings) {
        e <- environment()
        e$local_warns <- character()

        col_name <- names(study_data)[i]
        col_vec  <- study_data[[i]]

        df_one <- data.frame(col_vec, stringsAsFactors = FALSE)
        names(df_one) <- col_name

        if (!col_name %in% names(col_types)) {
          col_types[[col_name]] <- readr::col_guess()
        }
        converted_df <- withCallingHandlers(
          readr::type_convert(
            df_one,
            col_types     = col_types[col_name],
            trim_ws       = TRUE,
            guess_integer = FALSE,
            locale        = locale,
            na            = c("", "NA", "Inf", "+Inf", "-Inf", "NaN")
          ),
          warning = function(w) {
            # cat(conditionMessage(w), file = "/tmp/asdf", append = TRUE)
            # cat("\n", file = "/tmp/asdf", append = TRUE)
            cm <- util_extract_named_groups(
              "^\\[(?<row>\\d+), +(?<col>\\d+)\\]:.*",
              conditionMessage(w)
            )
            if (nrow(cm) > 0) {
              e$local_warns <- unique(c(e$local_warns, col_name))
              invokeRestart("muffleWarning")
            }
          }
        )

        list(
          data  = converted_df[[col_name]],
          warns = e$local_warns
        )
      },
      study_data                   = study_data,
      col_types                    = col_types,
      locale                       = locale,
      relevant_vars_for_warnings   = relevant_vars_for_warnings
    )

    data_list   <- lapply(processed_list, `[[`, "data")
    study_data0 <- as.data.frame(data_list, stringsAsFactors = FALSE)
    names(study_data0) <- names(study_data)

    warn_lists <- lapply(processed_list, `[[`, "warns")
    to_warn    <- unique(unlist(warn_lists))
  } else {
    study_data0 <- withCallingHandlers(readr::type_convert(
      # somehow, tehre is a wrning for TIME, currently (in parallel but hopefully, also here)
      # r <- dq_report2(study_data, meta_data = meta_data,
      #    meta_data_v2 = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx")
      #    and use meta_data and study_data according to test-con_limit_devaitions with time-var
      study_data,
      col_types = col_types,
      trim_ws = TRUE,
      guess_integer = FALSE,
      locale = locale,
      na = c("", "NA", "Inf", "+Inf", "-Inf", "NaN")
    ), warning = function(c) {
      cm <- util_extract_named_groups(
        "^\\[(?<row>\\d+), +(?<col>\\d+)\\]:.*",
        conditionMessage(c))
      if (nrow(cm) > 0) {
        my_env$to_warn <- c(
          my_env$to_warn,
          intersect(relevant_vars_for_warnings,
                    colnames(study_data)[as.integer(cm[, "col"])])
        )
        invokeRestart("muffleWarning")
      }
    })
  }

  if (length(num) > 0) {
    study_data0[, num] <- mapply(
      SIMPLIFY = FALSE,
      cl = study_data0[, num, drop = FALSE],
      orig = study_data[, num, drop = FALSE],
      FUN = function(cl, orig) {
        if (any(is.na(cl))) {
          cl[is.na(cl) & orig %in% c("Inf", "+Inf", "-Inf", "NaN")] <-
            as.vector(as.numeric(
              orig[is.na(cl) & orig %in% c("Inf", "+Inf", "-Inf", "NaN")]))
        }
        cl
      }
    )
  }
  study_data <- study_data0
  rm(study_data0)
  names(study_data) <- old_names
  dt_cols <- vapply(study_data, inherits, "POSIXt", FUN.VALUE = logical(1))
  study_data[, dt_cols] <- util_par_lapply_lb(cl = mycl, study_data[, dt_cols, drop = FALSE],
                                  function(cl) {
                                    cl <- as.POSIXct(round(as.POSIXct(cl), units = "secs"))
                                    cl <- lubridate::with_tz(lubridate::force_tz(cl))
                                  })
  study_data[, intg] <- util_par_lapply_lb(cl = mycl, study_data[, intg, drop = FALSE],
                               function(cl) { # TRUE -> 1, ...
                                 floor(cl)
                               })
  if (length(to_warn) > 0) {
    tb <- table(to_warn)
    invisible(apply(as.data.frame(tb), 1, function(rv_Freq) {
      rv <- rv_Freq["to_warn"]
      Freq <- rv_Freq["Freq"] # is this number also correct or lists?
      util_message(
        paste("Data type transformation of", dQuote(rv), "introduced",
              Freq,
              "additional missing values."),
        applicability_problem = TRUE)
    }))
  }
  attr(study_data, "Data_type_matches") <- TRUE
  study_data
}

# m <- prep_get_data_frame("meta_data")
# s <- prep_get_data_frame("study_data", keep_types = TRUE)
# s <- rbind(s, s, s, s, s, s)
# s <- rbind(s, s, s, s, s, s)
#
#
# microbenchmark::microbenchmark(
#   util_adjust_data_type(s, meta_data = m),
#   util_adjust_data_type2(s, meta_data = m)
# )
#
# Unit: seconds
# expr      min       lq     mean   median       uq       max neval
# util_adjust_data_type(s, meta_data = m) 3.910896 4.021739 4.163222 4.046629 4.070163 15.708896   100
# util_adjust_data_type2(s, meta_data = m) 2.797905 2.935524 3.025309 2.994379 3.035081  4.840821   100
# >
