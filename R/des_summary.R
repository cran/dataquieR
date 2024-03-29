#' Compute Descriptive Statistics
#'
#' @description
#'
#' generates a descriptive overview on the variables ins `resp_vars`.
#'
#' [Descriptor]
#'
#' @details
#' TODO
#'
#' @param resp_vars [variable] the name of the continuous measurement variable
#' @param study_data [data.frame] the data frame that contains the measurements
#' @param meta_data [data.frame] the data frame that contains metadata
#'                               attributes of study data
#' @param label_col [variable attribute] the name of the column in the metadata
#'                                       with labels of variables
#'
#' @return a list with:
#'   - SummaryTable: data frame
#'   - SummaryData: data frame
#' @export
#'
#' @examples
#' \dontrun{
#' prep_load_workbook_like_file("meta_data_v2")
#' xx <- des_summary("study_data", meta_data =
#'                               prep_get_data_frame("item_level"))
#' util_html_table(xx$SummaryData)
#' util_html_table(des_summary(study_data = prep_get_data_frame("study_data"),
#'                    meta_data = prep_get_data_frame("item_level"))$SummaryData)
#' }
#' @seealso
#' [Online Documentation](
#' https://dataquality.qihs.uni-greifswald.de/VIN_des_impl_summary.html
#' )
#' @importFrom graphics barplot hist
#' @importFrom stats mad
#'
des_summary <- function(study_data,
                        resp_vars = NULL, # IDEA: group_vars = NULL, co_vars = NULL,
                        meta_data = "item_level",
                        label_col = LABEL) {
  # TODO: add figures also to Excel Exports and prevent column type guessing
  # by datatables export correctly
  # TODO: Write vignette

  # preps ----------------------------------------------------------------------
  # map metadata to study data
  util_expect_data_frame(study_data)
  if (inherits(
      try(prep_prepare_dataframes(.replace_hard_limits = TRUE), silent = TRUE),
      "try-error")) {
    w <- "not all %s found, estimating %s"
    if (suppressWarnings(util_ensure_suggested("cli", err = FALSE))) {
      w <- cli::bg_black(cli::col_yellow(w))
    }
    util_warning(w, sQuote("meta_data"), sQuote("meta_data"),
                 immediate = TRUE)
    meta_data <-
      prep_study2meta(study_data, level = VARATT_REQUIRE_LEVELS$RECOMMENDED)
    prep_prepare_dataframes(.meta_data = meta_data, .replace_hard_limits = TRUE)
  }

   # define resp_vars
  if (length(resp_vars) == 0) {
    suppress <- function(...) suppressWarnings(suppressMessages(...))
    resp_vars <- colnames(ds1)
  } else {
    suppress <- eval
  }

  resp_vars <- intersect(colnames(ds1), resp_vars)

  suppress(util_correct_variable_use(resp_vars,
                                     allow_more_than_one = TRUE,
                                     #             need_type = "integer|float|datetime",
                                     do_not_stop = TRUE,
                                     need_scale = "!na"))

  # create a named vector of VAR_NAMES with names = LABEL
  study_data_lc <- attr(study_data, "label_col")
  if (is.null(study_data_lc)) {
    study_data_lc <- VAR_NAMES
  }
  varnames <- setNames(nm =
                         prep_map_labels(colnames(study_data),
                                         to = label_col,
                                         from = study_data_lc,
                                         meta_data = meta_data),
                       object = colnames(study_data))
  # create a named vector of SEGMENTS with names = VAR_NAMES
  if (STUDY_SEGMENT %in% colnames(meta_data)) {
    segments <- prep_map_labels(varnames,
                    meta_data = meta_data,
                    from = study_data_lc,
                    to = STUDY_SEGMENT,
                    ifnotfound = NA_character_,
                    warn_ambiguous = FALSE)
  } else {
    segments <- NULL
  }

  #defining the segments of the resp_vars
  if (!is.null(segments)) {
    sseg <- segments[varnames[resp_vars]]
  } else {
    sseg <- NULL
  }


  # create a named vector of LONG LABELS with names = VAR_NAMES
  labs <-
    prep_get_labels(varnames,
                    label_col = label_col,
                    meta_data = meta_data,
                    resp_vars_are_var_names_only = TRUE,
                    label_class = "LONG")
  # create a named vector of DATA_TYPEs with names = VAR_NAMES
  data_types <-
    prep_map_labels(varnames,
                    meta_data = meta_data,
                    to = DATA_TYPE,
                    ifnotfound = NA_character_,
                    warn_ambiguous = FALSE)
  # create a named vector of Value labels with names = VAR_NAMES
  val_labs <-
    prep_map_labels(varnames,
                    meta_data = meta_data,
                    to = VALUE_LABELS,
                    from = study_data_lc,
                    ifnotfound = NA_character_,
                    warn_ambiguous = FALSE)
  # create a named vector of SCALE_LEVELs with names = VAR_NAMES
  scale_levels <-
    prep_map_labels(varnames,
                    meta_data = meta_data,
                    to = SCALE_LEVEL,
                    from = study_data_lc,
                    ifnotfound = NA_character_,
                    warn_ambiguous = FALSE)


  ## Functions
  # create a function to obtain a table for ordinal and nominal variables
  frequency_table <- function(lb) {
    rv <- varnames[[lb]]
    r <- ""
    if (scale_levels[[rv]] %in% c(SCALE_LEVELS$INTERVAL,
                                  SCALE_LEVELS$RATIO,
                                  SCALE_LEVELS$`NA`,
                                  NA_character_)) {
      #empty result
      r <- ""

    } else if (scale_levels[[rv]] %in% c(SCALE_LEVELS$ORDINAL,SCALE_LEVELS$NOMINAL)) {
      # create a vector with the values of the variable
      dt <- ds1[[lb]]

      if (scale_levels[[rv]] %in% c(SCALE_LEVELS$ORDINAL)) {
        #in case there are VALUE_LABEL
        if (length(val_labs[[rv]]) > 0 && !util_empty(val_labs[[rv]])) {
          try({split_char <- attr(util_parse_assignments(val_labs[[rv]],
                                                         split_char =
                                                           c("<", SPLIT_CHAR))[[1]],
                                  "split_char")
          # Assign labels of the level to the vector
            dt <- suppressWarnings(util_assign_levlabs(warn_if_inadmissible = TRUE,
                                                   ds1[[lb]],
                                                   string_of_levlabs = val_labs[[rv]],
                                                   splitchar = split_char,
                                                   assignchar = "=",
                                                   ordered = FALSE))
            }, silent = TRUE)
        } else {
          #in case of VALUE_LABEL missing, the levels are obtained from data
          lev <- unique(ds1[[lb]])
#          lev <- lev[!is.na(lev)]
          lev<- sort(lev)
          dt <- factor(ds1[[lb]], levels = lev, labels = lev, ordered = FALSE)
          ##  m0 <- paste0(" = ", format(which.max(table(dt))))
        }

      } else if (scale_levels[[rv]] %in% c(SCALE_LEVELS$NOMINAL)) {
        # in case there are value labels
        if (length(val_labs[[rv]]) > 0 && !util_empty(val_labs[[rv]])) {
          # Assign labels of the level to the vector
          try({dt <-
            suppressWarnings(util_assign_levlabs(warn_if_inadmissible = TRUE,
                                                   ds1[[lb]],
                                                   string_of_levlabs = val_labs[[rv]],
                                                   splitchar = SPLIT_CHAR, assignchar = "=",
                                                   ordered = FALSE))
          }, silent = TRUE)

        # vector with levels
          dt <- factor(dt, ordered = FALSE)
          ##   m0 <- paste0(" = ", format(which.max(table(dtraw))))
        } else {
          #in case of VALUE_LABEL missing, the levels are obtained from data
          lev <- unique(ds1[[lb]])
         # lev <- lev[!is.na(lev)]
          lev<- sort(lev)
          dt <- factor(ds1[[lb]], levels = lev, labels = lev, ordered = FALSE)
          ##          m0 <- paste0(" = ", format(which.max(table(dt))))
        }

      }

      # No. unique values included NA
      r <- paste0(r, "No. unique values (incl. NA): ",
                  format(length(unique(dt))))


      # create a dataframe from the vector calculating the frequency of each value
      if(nrow(table(dt))>1) {
      cont_tab <- as.data.frame(sort(table(dt),
                                     decreasing = TRUE, na.last = FALSE))

      } else {
        cont_tab <- as.data.frame(table(dt), na.last = FALSE)
      }

      cont_tab[[1]] <- sQuote(cont_tab[[1]]) # to prevent values from being treated as numeric
      # rename the table
      colnames(cont_tab) <- c("Value", "Freq")
      # in case of tables with more than 5 rows, sum last values (less frequent)
      if (nrow(cont_tab) > 5) {
        foot <- tail(cont_tab, -5)
        foot <- c("Others", sum(foot[[2]], na.rm = TRUE))
 #      foot <- c(paste0(foot[[1]], collapse = ", "), sum(foot[[2]],
 #                                                        na.rm = TRUE))
        cont_tab <- rbind(head(cont_tab, 5), foot)
      }
      # save the formatted table in r
      r <- paste0(r, "",
                  util_formattable(util_first_row_to_colnames(
                    as.data.frame(t(cont_tab))),
                    min_color = c(235, 235, 235),
                    max_color = c(20, 20, 235)))
      rm(cont_tab, dt)
      } else {
        util_error(
          c("Internal error unexpected scale level %s for %s in %s, sorry.",
            "Please file a bug report."),
          dQuote(scale_levels[[rv]]),
          dQuote(rv),
          sQuote("des_summary")
        )
      }

    r
  }



  ### Measure of central tendency or location parameters
  #function for mean - based on conditions
  c_mean <- function(lb) {
    rv <- varnames[[lb]]
    r <- ""
    if (scale_levels[[rv]] %in% c(SCALE_LEVELS$INTERVAL,
                                  SCALE_LEVELS$RATIO)) {
      mean_value <- mean(ds1[[lb]], na.rm = TRUE)
      if(data_types[[rv]] == "datetime"){

        r <- format(mean_value, usetz = TRUE)
      } else {
        mean_value <- round(mean_value, digits = 3)
        r <- format(mean_value)
      }
      rm(mean_value)
    } else if (scale_levels[[rv]] %in% c(SCALE_LEVELS$ORDINAL,
                                         SCALE_LEVELS$NOMINAL)) {
      r <- ""
    }
    r
  }


  #function for Median - based on conditions
  c_median <- function(lb) {
    rv <- varnames[[lb]]
    r <- ""
    if (scale_levels[[rv]] %in% c(SCALE_LEVELS$INTERVAL,
                                  SCALE_LEVELS$RATIO)) {
      dt <- ds1[[lb]]
      m <- median(dt, na.rm = TRUE)

      if(data_types[[rv]] == "datetime"){
        r <- paste0(r, format(m, usetz = TRUE))
      } else {
        r <- paste0(r, format(m))
      }


    } else if (scale_levels[[rv]] %in% c( SCALE_LEVELS$ORDINAL)) {

      if(!is.na(val_labs[[rv]])) {
        try({
          split_char <-
            attr(util_parse_assignments(val_labs[[rv]],
                                        split_char = c("<", SPLIT_CHAR))[[1]],
                 "split_char")
        dt <-
            suppressWarnings(util_assign_levlabs(warn_if_inadmissible = TRUE,
                                                 ds1[[lb]],
                                                 string_of_levlabs = val_labs[[rv]],
                                                 splitchar = split_char,
                                                 assignchar = "=",
                                                 ordered = TRUE))

          m <- median.ordered(dt)

#          m0 <- paste0(" = ", format(m))
        }, silent = TRUE)

      } else {
        dt <- ds1[[lb]]
        #in case of VALUE_LABEL missing, the levels are obtained from data
        lev <- unique(dt)
 #       lev <- lev[!is.na(lev)]
        lev<- sort(lev)
        dt <- factor(dt, levels = lev, labels = lev)
        m <- median.ordered(dt)
      }
      r <- paste0(r, format(m) )

      } else if (scale_levels[[rv]] %in% c(SCALE_LEVELS$NOMINAL)) {
      r <- ""
    }
    r
  }



  #function for Mode - based on conditions
  c_mode <- function(lb) {
    rv <- varnames[[lb]]
    r <- ""
    if (scale_levels[[rv]] %in% c(SCALE_LEVELS$INTERVAL,
                                  SCALE_LEVELS$RATIO)) {
      dt <- ds1[[lb]]
      dt <- dt[!is.na(dt)]
      mode_value <-  unique(dt)[tabulate(match(dt, unique(dt))) ==
                                  max(tabulate(match(dt, unique(dt))))]
      if(data_types[[rv]] == "datetime"){
        if(length(mode_value)==1) {
          r <- format(mode_value, usetz = TRUE)
        } else if(length(mode_value)==2) {
          mode_value <- format(mode_value, usetz = TRUE)
          mode_value <- paste(mode_value,collapse = " ")
          r <- paste0(r, format(mode_value, usetz = TRUE))
        } else if (length(mode_value)>2) {
          mode_value <- format(mode_value, usetz = TRUE)
          n_mode <- length(mode_value)
          mode_value<- mode_value[1:2]
          n_mode_removed <- n_mode - length(mode_value)
          mode_value <- paste(mode_value,collapse = " ")
          r <- paste0(format(mode_value, usetz = TRUE), " and other ",
                      n_mode_removed ," dates")
        }

      } else {
        if(length(mode_value)==1) {
          r <- format(mode_value)
        } else  if(length(mode_value)==2) {
          mode_value <- paste(mode_value,collapse = " ")
          r <- paste0(r, format(mode_value))
        } else if (length(mode_value)>2){
          n_mode <- length(mode_value)
          mode_value<- mode_value[1:2]
          n_mode_removed <- n_mode - length(mode_value)
          mode_value <- paste(mode_value,collapse = " ")
          r <- paste0(format(mode_value, usetz = TRUE), " and other ",
                      n_mode_removed ," values")
        }

        }
      rm(mode_value)
    } else if (scale_levels[[rv]] %in% c(SCALE_LEVELS$ORDINAL)) {

      if(!is.na(val_labs[[rv]])) {
        try({
          split_char <-
            attr(util_parse_assignments(val_labs[[rv]],
                                        split_char = c("<", SPLIT_CHAR))[[1]],
                 "split_char")
          dt <-
            suppressWarnings(util_assign_levlabs(warn_if_inadmissible = TRUE,
                                                 ds1[[lb]],
                                                 string_of_levlabs = val_labs[[rv]],
                                                 splitchar = split_char,
                                                 assignchar = "=",
                                                 ordered = TRUE))

          dt <- dt[!is.na(dt)]

          mode_value <-  unique(dt)[tabulate(match(dt, unique(dt))) ==
                                      max(tabulate(match(dt, unique(dt))))]

        }, silent = TRUE)

      } else {
        dt <- ds1[[lb]]
        dt <- dt[!is.na(dt)]

        lev <- unique(dt)
        lev<- sort(lev)
        dt <- factor(dt, levels = lev, labels = lev)
        mode_value <-  unique(dt)[tabulate(match(dt, unique(dt))) ==
                                    max(tabulate(match(dt, unique(dt))))]



      }
      if(length(mode_value)>1) {
        mode_value <- paste(mode_value,collapse = " ")
      }
        r <- paste0(r, mode_value)


    } else if (scale_levels[[rv]] %in% c(SCALE_LEVELS$NOMINAL)) {

      if(!is.na(val_labs[[rv]])) {
        try({
          dt <-
            suppressWarnings(util_assign_levlabs(warn_if_inadmissible = TRUE,
                                                 ds1[[lb]],
                                                 string_of_levlabs = val_labs[[rv]],
                                                 splitchar = SPLIT_CHAR,
                                                 assignchar = "=",
                                                 ordered = TRUE))

          dt <- dt[!is.na(dt)]

          mode_value <-  unique(dt)[tabulate(match(dt, unique(dt))) ==
                                      max(tabulate(match(dt, unique(dt))))]
          if(length(mode_value)>1) {
            mode_value <- paste(mode_value,collapse = " ")
          }
          r <- paste0(r, mode_value)
        }, silent = TRUE)

      } else {


        dt <- ds1[[lb]]
        dt <- dt[!is.na(dt)]

        lev <- unique(dt)
        lev<- sort(lev)
        dt <- factor(dt, levels = lev, labels = lev)
        mode_value <-  unique(dt)[tabulate(match(dt, unique(dt))) ==
                                    max(tabulate(match(dt, unique(dt))))]


        if(length(mode_value)>1) {
          mode_value <- paste(mode_value,collapse = " ")
        }
        r <- paste0(r, mode_value)


      }



    }
   r
  }


  #function for SD - based on conditions
  c_SD <- function(lb) {
    rv <- varnames[[lb]]
    r <- ""
    if (scale_levels[[rv]] %in% c(SCALE_LEVELS$INTERVAL,
                                  SCALE_LEVELS$RATIO)) {
      if(data_types[[rv]] == "datetime") {

        r <- format(as.difftime(sd(ds1[[lb]], na.rm = TRUE), #TODO: write an algorithm values > No.secs in year--> yrs..
                      units = 'secs'),usetz = TRUE )

      } else {
        sd_value <- round(sd(na.rm = TRUE, ds1[[lb]]), digits = 4)
        r <- format(sd_value)
        rm(sd_value)
      }

    } else if (scale_levels[[rv]] %in% c(SCALE_LEVELS$ORDINAL,
                                         SCALE_LEVELS$NOMINAL)) {
      r <- ""
    }
    r
  }




  #function for MAD - based on conditions
  c_MAD <- function(lb) {
    rv <- varnames[[lb]]
    r <- ""
    if (scale_levels[[rv]] %in% c(SCALE_LEVELS$INTERVAL,
                                  SCALE_LEVELS$RATIO)) {

      if(data_types[[rv]] == "datetime"){
        r <- format(mad(ds1[[lb]], na.rm = TRUE), usetz = TRUE)
      } else{
        MAD_value <- round(mad(ds1[[lb]], na.rm = TRUE), digits = 3)
        r <- format(MAD_value)
      }

    } else if (scale_levels[[rv]] %in% c(SCALE_LEVELS$ORDINAL,
                                         SCALE_LEVELS$NOMINAL)) {
      r <- ""
    }
    r
  }

  #function for Min - Max - based on conditions
  c_min_max <- function(lb) {
    rv <- varnames[[lb]]
    r <- ""
    if (scale_levels[[rv]] %in% c(SCALE_LEVELS$INTERVAL,
                                  SCALE_LEVELS$RATIO)) {

      if(data_types[[rv]] == "datetime"){
        r <- paste0(format(max(ds1[[lb]], na.rm = TRUE)-min(ds1[[lb]], na.rm = TRUE), usetz = TRUE),
                    " (", format(min(ds1[[lb]], na.rm = TRUE), usetz = TRUE), " - ",
                    format(max(ds1[[lb]], na.rm = TRUE), usetz = TRUE), ")")
      } else {
      r <- paste0(format(max(ds1[[lb]], na.rm = TRUE)-min(ds1[[lb]], na.rm = TRUE)),
                  " (", format(min(ds1[[lb]], na.rm = TRUE)), " - ",
                  format(max(ds1[[lb]], na.rm = TRUE)), ")")
      }


    } else if (scale_levels[[rv]] %in% c(SCALE_LEVELS$ORDINAL)) {
      if(!is.na(val_labs[[rv]])) {
        try({
          split_char <-
            attr(util_parse_assignments(val_labs[[rv]],
                                        split_char = c("<", SPLIT_CHAR))[[1]],
                 "split_char")
          dt <-
            suppressWarnings(util_assign_levlabs(warn_if_inadmissible = TRUE,
                                                 ds1[[lb]],
                                                 string_of_levlabs = val_labs[[rv]],
                                                 splitchar = split_char,
                                                 assignchar = "=",
                                                 ordered = TRUE))


          r <- paste0(" (",
                      format(min(dt, na.rm = TRUE)), " - ",
                      format(max(dt, na.rm = TRUE), na.rm = TRUE), ")")
        }, silent = TRUE)

      } else {
        dt <- ds1[[lb]]
        dt <- dt[!is.na(dt)]
        lev <- unique(dt)
        lev<- sort(lev)
        dt <- factor(dt, levels = lev, labels = lev, ordered = TRUE)
        r <- paste0(format(min(dt, na.rm = TRUE)), " - ",
                   format(max(dt, na.rm = TRUE), na.rm = TRUE))
        r <- ""
      }

    }else if (scale_levels[[rv]] %in% c(SCALE_LEVELS$NOMINAL)) {
      r <-""
    }
    r
  }

  #function for quantiles and IQR - based on conditions
  c_quantiles <- function(lb) {

    rv <- varnames[[lb]]
    r <- ""
    if (scale_levels[[rv]] %in% c(SCALE_LEVELS$INTERVAL,
                                  SCALE_LEVELS$RATIO)) {

      if(data_types[[rv]] == "datetime"){
        q <- quantile(ds1[[lb]], na.rm = TRUE, names = FALSE)
        names(q) <- paste0("Q", seq_len(length(q)) - 1)
        q <- q[c("Q1", "Q3")]
        r <- prep_deparse_assignments(labels = format(q, usetz = TRUE),
                                      codes = names(q),
                                      mode = "string_codes")
        r<- paste0(format(as.difftime(IQR(na.rm = TRUE, x = ds1[[lb]], type = 7), units = "secs"), #TODO: write an algorithm values > No.secs in year--> yrs..
           usetz = TRUE), " (", r, ")")
        rm(q)
      } else {
        q <- quantile(ds1[[lb]], na.rm = TRUE, names = FALSE)  #TODO: check the TYPE
        names(q) <- paste0("Q", seq_len(length(q)) - 1)
        q <- q[c("Q1", "Q3")]
        r <- prep_deparse_assignments(labels = as.character(q),
                                      codes = names(q),
                                      mode = "string_codes")
        r<- paste0(format(IQR(na.rm = TRUE, x = ds1[[lb]], type = 7)), " (", r, ")")  #TODO: check the TYPE
        rm(q)
      }

    } else if (scale_levels[[rv]] %in% c(SCALE_LEVELS$ORDINAL)) {
      dt<- ds1[[lb]]
      if(!is.na(val_labs[[rv]])) {

        try({
          split_char <-
            attr(util_parse_assignments(val_labs[[rv]],
                                        split_char = c("<", SPLIT_CHAR))[[1]],
                 "split_char")
          dt <-
            suppressWarnings(util_assign_levlabs(warn_if_inadmissible = TRUE,
                                                 ds1[[lb]],
                                                 string_of_levlabs = val_labs[[rv]],
                                                 splitchar = split_char,
                                                 assignchar = "=",
                                                 ordered = TRUE)) }, silent = TRUE)
        q <- quantile(dt, na.rm = TRUE, names = TRUE, type = 1)  #TODO: check the TYPE
        names(q) <- paste0("Q", seq_len(length(q)) - 1)
        q <- q[c("Q1", "Q3")]
        r <- prep_deparse_assignments(labels = as.character(q),
                                      codes = names(q),
                                      mode = "string_codes")
        r<- paste0(format(IQR(na.rm = TRUE, x = dt, type = 1)), " (", r, ")")  #TODO: check the TYPE

        rm(q)
        } else {
       r <- ""

      }

    } else if (scale_levels[[rv]] %in% c(SCALE_LEVELS$NOMINAL)) {
      r <- ""
    }
    r
  }

  #function for CV - based on conditions
  c_CV <- function(lb) {
    rv <- varnames[[lb]]
    r <- ""
    if (scale_levels[[rv]] %in% c(SCALE_LEVELS$INTERVAL,
                                  SCALE_LEVELS$RATIO))  {
      if (data_types[[rv]] %in% c(DATA_TYPES$INTEGER, DATA_TYPES$FLOAT)) {
        CV_value <- sd(ds1[[lb]], na.rm = TRUE) /
          mean(ds1[[lb]], na.rm = TRUE) * 100
        CV_value <- round(CV_value, digits = 4)
        r <- format(CV_value)
        rm(CV_value)
      } else {
        r <- ""
      }
    } else if (scale_levels[[rv]] %in% c(SCALE_LEVELS$ORDINAL,
                                         SCALE_LEVELS$NOMINAL)) {
      r <- ""
    }
    r
  }


  #function for skewness and SE.skewness - based on conditions
  c_skewness <- function(lb) {
    rv <- varnames[[lb]]
    r <- ""
    if (scale_levels[[rv]] %in% c(SCALE_LEVELS$INTERVAL,
                                  SCALE_LEVELS$RATIO))  {
      if (data_types[[rv]] %in% c(DATA_TYPES$INTEGER, DATA_TYPES$FLOAT)) {
        skewness <- util_compute_skewness(ds1[[lb]])
        SE_skew <- util_compute_SE_skewness(ds1[[lb]], skewness = skewness)
        skewness<- round(skewness, digits = 4)
        SE_skew<- round(SE_skew, digits = 4)
        r <- paste0(format(skewness),
                    " (", format(SE_skew), ")")
        rm(skewness, SE_skew)
      } else {
        r <- ""
      }
    } else if (scale_levels[[rv]] %in% c(SCALE_LEVELS$ORDINAL,
                                         SCALE_LEVELS$NOMINAL)) {
      r <- ""
    }
    r
  }


  #function for Kurtosis - based on conditions
  c_kurtosis <- function(lb) {
    rv <- varnames[[lb]]
    r <- ""
    if (scale_levels[[rv]] %in% c(SCALE_LEVELS$INTERVAL,
                                  SCALE_LEVELS$RATIO))  {
      if (data_types[[rv]] %in% c(DATA_TYPES$INTEGER, DATA_TYPES$FLOAT)) {
        kurtosis_value <- util_compute_kurtosis(ds1[[lb]])
        kurtosis_value <- round(kurtosis_value, digits = 4)
        r <- format(kurtosis_value)
        rm(kurtosis_value)
      } else {
        r <- ""
      }
    } else if (scale_levels[[rv]] %in% c(SCALE_LEVELS$ORDINAL,
                                         SCALE_LEVELS$NOMINAL)) {
      r <- ""
    }
    r
  }









  SummaryData <- data.frame(Variables = resp_vars,
                            `Labels` = paste0(labs[varnames[resp_vars]], "<br />",
                                               resp_vars,
                                               "<br />",
                                               varnames[resp_vars],
                                               "<br />[",
                                               scale_levels[varnames[resp_vars]],
                                               ", ",
                                               data_types[varnames[resp_vars]],
                                               "]"),
                            STUDY_SEGMENT = sseg,
             `Mean` =
               vapply(resp_vars, c_mean, character(1)),
             `Median` =
               vapply(resp_vars, c_median, character(1)),
             `Mode` =
               vapply(resp_vars, c_mode, character(1)),
             `SD` =
               vapply(resp_vars, c_SD, character(1)),
             `IQR (Quartiles)` =
               vapply(resp_vars, c_quantiles, character(1)),
             `MAD` =
               vapply(resp_vars, c_MAD, character(1)),
             `Range (Min - Max)` =
               vapply(resp_vars, c_min_max, character(1)),
             `CV` =
               vapply(resp_vars, c_CV, character(1)),
             `Skewness` =
               vapply(resp_vars, c_skewness, character(1)),
             `Kurtosis` =
               vapply(resp_vars, c_kurtosis, character(1)),
             `No. categories/Freq. table` =
               vapply(resp_vars, frequency_table, character(1)),
             Valid = paste0(vapply(resp_vars, function(rv) sum(is.finite(ds1[[rv]])),
                            FUN.VALUE = numeric(1)), "<br />",
                            scales::percent(vapply(resp_vars,
                                                   function(rv) sum(is.finite(ds1[[rv]])),
                                                   FUN.VALUE = numeric(1)) / nrow(ds1))),
             Missing = paste0(vapply(resp_vars,
                                     function(rv) sum(util_empty(ds1[[rv]])),
                                     FUN.VALUE = numeric(1)), "<br />",
                              scales::percent(vapply(resp_vars,
                                                     function(rv) sum(util_empty(ds1[[rv]])),
                                                     FUN.VALUE = numeric(1)) / nrow(ds1))),

             stringsAsFactors = FALSE, check.names = FALSE)

  if (suppressWarnings(util_ensure_suggested("htmltools",
                                             err = FALSE,
                                             goal = "Distribution Plots in Descriptive Summary"))) {
    SummaryData$Graph <- lapply(resp_vars, function(lb) {
      rv <- varnames[[lb]]

      if (scale_levels[[rv]] %in% c(SCALE_LEVELS$INTERVAL,
                                    SCALE_LEVELS$RATIO) &&
          data_types[[rv]] %in% c(DATA_TYPES$INTEGER,
                                  DATA_TYPES$FLOAT,
                                  DATA_TYPES$DATETIME)) {
        if (data_types[[rv]] == DATA_TYPES$DATETIME) {
          breaks <- unlist(suppressMessages(util_optimize_histogram_bins(
            x = ds1[[lb]],
            # iqr_bw = IQR(ds1[[lb]], na.rm = TRUE),
            # n_bw = length(ds1[[lb]]),
            # min_plot = min(ds1[[lb]], na.rm = TRUE),
            # max_plot = max(ds1[[lb]], na.rm = TRUE),
            nbins_max = 100
          )))

          breaks <- as.POSIXct(breaks, origin = min(Sys.time(), 0))
          breaks <- unique(sort(breaks))
          breaks <- scales::pretty_breaks()(breaks)
          r <- htmltools::plotTag({hist(ds1[[lb]], main = NULL,
                                        xlab = NULL, # TOOD: use acc_distributions
                                        ylab = NULL, breaks = breaks,
                                        freq = TRUE)}, width = 250, height = 250,
                                  alt = paste("Histogram of", labs[[rv]]))
        } else {
          r <- htmltools::plotTag({hist(ds1[[lb]], main = NULL, xlab = NULL, # TOOD: use acc_distributions
                                        ylab = NULL)}, width = 250, height = 250,
                                  alt = paste("Histogram of", labs[[rv]]))
        }
        r <- as.character(r)
      } else if (scale_levels[[rv]] %in% c(SCALE_LEVELS$NOMINAL,
                                           SCALE_LEVELS$ORDINAL)) {

        dt <- factor(ds1[[lb]], ordered = scale_levels[[rv]] ==
                       c(SCALE_LEVELS$ORDINAL))
        try({
          split_char <- attr(util_parse_assignments(val_labs[[rv]],
                                                    split_char =
                                                      c("<", SPLIT_CHAR))[[1]],
                             "split_char")
          dt <-
            suppressWarnings(util_assign_levlabs(warn_if_inadmissible = TRUE,
                                                 ds1[[lb]],
                                                 string_of_levlabs = val_labs[[rv]],
                                                 splitchar = split_char,
                                                 assignchar = "=",
                                                 ordered = scale_levels[[rv]] ==
                                                   c(SCALE_LEVELS$ORDINAL)))
        }, silent = TRUE)


        r <- htmltools::plotTag({barplot(table(dt), main = NULL, xlab = NULL, # TOOD: use acc_distributions
                                      ylab = NULL)}, width = 250, height = 250,
                                alt = paste("Histogram of", labs[[rv]]))
        r <- as.character(r)
      } else {
        r <- ""
      }
      r
    })
  } else {
    SummaryData$Graph <- ""
  }

  rownames(SummaryData) <- NULL

  SummaryTable <- SummaryData # TODO: machine readable

  list(SummaryData = SummaryData, # TODO: plotlist
       SummaryTable = SummaryTable)
}

#' @seealso https://stackoverflow.com/a/7925162
#' @author Richie Cotton
median.ordered <- function(x, na.rm = TRUE, ...) {
  levs <- levels(x)
  m <- median(as.integer(x), na.rm = na.rm, ...)
  if(floor(m) != m)
  {
    message("Median is between two values; using the first one")
    m <- floor(m)
  }
  ordered(m, labels = levs, levels = seq_along(levs))
}

# TODO: do not compute the sd and mean so frequently, do as for SE.skewness and skewness.

#' Compute the Skewness
#'
#' @param x data
#'
#' @return the Skewness
util_compute_skewness <- function(x) { # TODO: Verify, generated code
  n <- length(x)
  mean_x <- mean(x, na.rm = TRUE)
  sd_x <- sd(x, na.rm = TRUE)

  skewness <- (sum((x - mean_x)^3, na.rm = TRUE) / (n * sd_x^3))
  return(skewness)
}

#' Compute SE.Skewness
#'
#' @param x data
#' @param skewness if already known
#'
#' @return the standard error of skewness
util_compute_SE_skewness <- function(x,
                                     skewness = util_compute_skewness(x)) {
  n <- length(x)

  se_skewness <- sqrt((6 * n * (n - 1)) / ((n - 2) * (n + 1) * (n + 3)))

  return(se_skewness)
}

#' Compute Kurtosis
#'
#' @param x data
#'
#' @return the Kurtosis
util_compute_kurtosis <- function(x) {
  # TODO: Verify, generated code
  n <- length(x)
  mean_x <- mean(x, na.rm = TRUE)
  sd_x <- sd(x, na.rm = TRUE)

  kurtosis <- sum((x - mean_x)^4, na.rm = TRUE) / (n * sd_x^4) - 3

  return(kurtosis)
}

#' Move the first row of a data frame to its column names
#'
#' @param dfr [data.frame]
#'
#' @return [data.frame] with first row as column names
util_first_row_to_colnames <- function(dfr) {
  colnames(dfr) <- dfr[1, , TRUE]
  dfr <- tail(dfr, -1)
  dfr
}

# TODO: Make Excel export contain plain numeric columns instead of a combined Stats/Values column.
# TODO: Remove warnings

# IDEA: des_pairs for variable groups

