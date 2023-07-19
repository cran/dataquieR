#' Generate a stratified full DQ report
#'
#' @param study_data [data.frame] the data frame that contains the measurements
#' @param meta_data [data.frame] the data frame that contains metadata
#'                               attributes of study data
#' @param meta_data_segment [data.frame] -- optional: Segment level metadata
#' @param meta_data_dataframe [data.frame] -- optional: Data frame level
#'                                                                 metadata
#' @param meta_data_cross_item [data.frame] -- optional: Cross-item level
#'                                                                 metadata
#' @param ... arguments to be passed through to [dq_report] or [dq_report2]
#' @param label_col [variable attribute] the name of the column in the
#'                                       metadata with labels of variables
#' @param meta_data_v2 [character] path to workbook like metadata file, see
#'                                 [`prep_load_workbook_like_file`] for details.
#'                                 **ALL LOADED DATAFRAMES WILL BE PURGED**,
#'                                 using [`prep_purge_data_frame_cache`],
#'                                 if you specify `meta_data_v2`.
#' @param meta_data_split [variable attribute] name of a metadata attribute to
#'                                             split the report in sections of
#'                                             variables, e.g. all blood-
#'                                             pressure. By default, reports are
#'                                             split by [STUDY_SEGMENT]
#'                                             if available.
#' @param study_data_split [variable] Name of a study variable to stratify the
#'                                    report by, e.g. the study centers.
#' @param v1.0 [logical] if `TRUE`, create old `dq_report` reports, use
#'                       `dq_report2`, otherwise
#' @param output_dir [character] if given, the output is not returned but
#' @param also_print [logical] if `output_dir` is not `NULL`, also create
#'                             `HTML` output for each segment using
#'                             [print.dataquieR_resultset2()].
#'                               written to the path `output_dir`
#' @return named [list] of named [list]s of [dq_report2] reports or,
#'         if `output_dir` has been specified, `invisible(NULL)`
#'
#' @seealso [dq_report]
#'
#' @export
#'
#' @examples
#' \dontrun{ # really long-running example.
#' prep_load_workbook_like_file("meta_data_v2")
#' rep <- dq_report_by("study_data", label_col =
#'   LABEL, study_data_split = "CENTER_0")
#' rep <- dq_report_by("study_data",
#'   label_col = LABEL, study_data_split = "CENTER_0",
#'   meta_data_split = NULL
#' )
#' unlink("/tmp/testRep/", force = TRUE, recursive = TRUE)
#' dq_report_by("study_data",
#'   label_col = LABEL, study_data_split = "CENTER_0",
#'   meta_data_split = STUDY_SEGMENT, output_dir = "/tmp/testRep"
#' )
#' unlink("/tmp/testRep/", force = TRUE, recursive = TRUE)
#' dq_report_by("study_data",
#'   label_col = LABEL, study_data_split = "CENTER_0",
#'   meta_data_split = NULL, output_dir = "/tmp/testRep"
#' )
#' dq_report_by("study_data",
#'   label_col = LABEL,
#'   meta_data_split = STUDY_SEGMENT, output_dir = "/tmp/testRep"
#' )
#' dq_report_by("study_data",
#'   label_col = LABEL,
#'   meta_data_split = STUDY_SEGMENT, output_dir = "/tmp/testRep",
#'   also_print = TRUE
#' )
#' }
dq_report_by <- function(study_data, meta_data = "item_level",
                         meta_data_segment = "segment_level",
                         meta_data_dataframe = "dataframe_level",
                         meta_data_cross_item = "cross-item_level",
                         label_col,
                         meta_data_v2,
                         meta_data_split =
                           STUDY_SEGMENT, study_data_split, ..., v1.0 = FALSE,
                         output_dir = NULL,
                         also_print = FALSE) {
  util_expect_scalar(v1.0, check_type = is.logical)

  if (!missing(meta_data_v2)) {
    util_message("Have %s set, so I'll remove all loaded data frames",
                 sQuote("meta_data_v2"))
    prep_purge_data_frame_cache()
    prep_load_workbook_like_file(meta_data_v2)
    if (!exists("item_level", .dataframe_environment)) {
      w <- paste("Did not find any sheet named %s in %s, is this",
                 "really dataquieR version 2 metadata?")
      if (requireNamespace("cli", quietly = TRUE)) {
        w <- cli::bg_red(cli::col_br_yellow(w))
      }
      util_warning(w, dQuote("item_level"), dQuote(meta_data_v2),
                   immediate = TRUE)
    }
  }

  util_expect_data_frame(meta_data)
  try(util_expect_data_frame(meta_data_cross_item), silent = TRUE)
  if (!is.data.frame(meta_data_cross_item)) {
    util_message("No cross-item level metadata %s found",
                 dQuote(meta_data_cross_item))
    meta_data_cross_item <- data.frame(VARIABLE_LIST = character(0),
                                       CHECK_LABEL = character(0))
  }
  suppressWarnings(util_ensure_in(VAR_NAMES, names(meta_data), error = TRUE,
                                  err_msg =
                                    sprintf("Did not find the mandatory column %%s in the %s.",
                                            sQuote("meta_data"))))

  if (!(VARIABLE_ROLE %in% colnames(meta_data))) {
    util_message("No %s assigned in item level metadata. Defaulting to %s.",
                 sQuote(VARIABLE_ROLE), dQuote(VARIABLE_ROLES$PRIMARY),
                 applicability_problem = TRUE)
    meta_data$VARIABLE_ROLE <- VARIABLE_ROLES$PRIMARY
  }

  if (!missing(output_dir)) {
    util_expect_scalar(output_dir, check_type = is.character)
    util_expect_scalar(also_print, check_type = is.logical)
    if (dir.exists(output_dir)) {
      util_error("%s already exists. Remove the %s first",
                 dQuote(output_dir),
                 sQuote("output_dir"))
    }
    if (!dir.create(output_dir, FALSE, TRUE)) {
      util_error("Could not create %s", dQuote(output_dir))
    }
  }
  if (missing(label_col)) {
    if (v1.0)
      label_col <- VAR_NAMES
    else
      label_col <- LABEL
  }

  # ensure that labels exist, are unique and not too long
  mod_label <- util_ensure_label(meta_data = meta_data,
                                 label_col = label_col)
  if (!is.null(mod_label$label_modification_text)) {
    # There were changes in the metadata.
    meta_data <- mod_label$meta_data
  }

  if (is.data.frame(study_data)) {
    name_of_study_data <- head(as.character(substitute(study_data)), 1)
  } else if (length(study_data) == 1 && is.character(study_data)) {
    name_of_study_data <- study_data
  } else {
    name_of_study_data <- "??No study data found??"
  }
  util_prepare_dataframes(.replace_missings = FALSE)
  if (!missing(study_data_split)) {
    .study_data_split <- study_data_split
  }
  if (label_col != VAR_NAMES && !missing(study_data_split)) {
    study_data_split <-
      util_map_labels(study_data_split, meta_data, VAR_NAMES, label_col)
  }
  if (missing(study_data_split) || is.null(study_data_split) ||
      !study_data_split %in% colnames(study_data)) {
    .sd_list <- list(all_observations = study_data)
  } else {
    .sd_list <- split(study_data, study_data[[study_data_split]])
    names(.sd_list) <- paste0(.study_data_split, "_", names(.sd_list))
  }
  if (missing(meta_data_split)) {
    if (STUDY_SEGMENT %in% colnames(meta_data)) {
      meta_data_split <- STUDY_SEGMENT
    } else {
      meta_data_split <- NULL
    }
  } else if (!is.null(meta_data_split)) {
    if (!(meta_data_split %in% colnames(meta_data))) {
      util_error("No metadata attribute %s found for segmenting DQ report.",
                 dQuote(meta_data_split))
    }
    if (meta_data_split != STUDY_SEGMENT) {
      util_error(
        "Only STUDY_SEGMENT is supported for meta_data_split up to now.")
    }
  }
  key_cols <- util_variable_references(meta_data)
  split_segments <- FALSE
  if (!is.null(meta_data_split)) {
    split_segments <- TRUE
    .md <- meta_data[[meta_data_split]]
    i <- ""
    while (any(.md == paste0("na", i), na.rm = TRUE)) {
      if (i == "") i <- 0
      i <- i + 1
    }
    .md[is.na(.md)] <- paste0("na", i)
    meta_data[[meta_data_split]] <- .md
    segments <- unique(meta_data[[meta_data_split]])
    if (label_col != VAR_NAMES && all(segments %in% meta_data[[VAR_NAMES]])) {
      segmentNames <- util_map_labels(segments, meta_data, label_col)
    } else {
      segmentNames <- segments
    }
    cil <- util_normalize_cross_item(meta_data = meta_data,
                                     meta_data_cross_item =
                                       meta_data_cross_item,
                                     label_col = label_col)
    cil_in_segment <- lapply(setNames(segments, nm = segmentNames),
                             function(segment) {
                               vars <-
                                 meta_data[meta_data[[meta_data_split]] ==
                                             segment, VAR_NAMES]
                               vars <- prep_map_labels(vars,
                                                       meta_data = meta_data,
                                                       to = label_col,
                                                       from = VAR_NAMES)
                               rules_vars <-
                                 util_parse_assignments(cil$VARIABLE_LIST,
                                                      multi_variate_text = TRUE)
                               rules_to_use <-
                                 vapply(lapply(rules_vars, intersect, vars),
                                        length, FUN.VALUE = integer(1)) > 0
                               cil[rules_to_use, , FALSE]

                      })
    try(util_expect_data_frame(meta_data_dataframe), silent = TRUE)
    if (!is.data.frame(meta_data_dataframe)) {
      util_message("No dataframe level metadata %s found",
                   dQuote(meta_data_dataframe))
      meta_data_dataframe <- data.frame(DF_NAME =
                                          name_of_study_data)
    }
    dfr_in_segment <- lapply(setNames(segments, nm = segmentNames),
                             function(segment) {
                               meta_data_dataframe
                             })
    try(util_expect_data_frame(meta_data_segment), silent = TRUE)
    if (!is.data.frame(meta_data_segment)) {
      util_message("No segment level metadata %s found",
                   dQuote(meta_data_segment))
      meta_data_segment <- data.frame(STUDY_SEGMENT =
                                        unique(meta_data$STUDY_SEGMENT))
    }
    seg_in_segment <- lapply(setNames(segments, nm = segmentNames),
                             function(segment) {
                               meta_data_segment[
                                 meta_data_segment[[STUDY_SEGMENT]] == segment,
                                 , FALSE
                                 ]
                             })
    vars_in_segment <- lapply(setNames(segments, nm = segmentNames),
                              function(segment) {
      vars <- meta_data[meta_data[[meta_data_split]] == segment, VAR_NAMES]
      repeat {
        referred_vars <-
          unique(unlist(meta_data[meta_data[[VAR_NAMES]] %in% vars, key_cols],
                        recursive = TRUE))
        referred_vars <-
          c(referred_vars, unlist(util_parse_assignments(
            cil_in_segment[[segment]][[VARIABLE_LIST]],
            multi_variate_text = TRUE
            )))
        referred_vars <-
          c(referred_vars, seg_in_segment[[segment]][[SEGMENT_ID_VARS]])
        referred_vars <-
          c(referred_vars, dfr_in_segment[[segment]][[DF_ID_VARS]])
        referred_vars <-
          unique(util_find_var_by_meta(unique(
            unlist(util_parse_assignments(referred_vars,
                                          multi_variate_text = TRUE))),
            meta_data = meta_data))
        referred_vars <- referred_vars[!is.na(referred_vars)]
        if (all(referred_vars %in% vars)) {
          break
        } else {
          vars <- union(vars, referred_vars)
        }
      }
      # add vars from rules

      vars
    })
  } else {
    cil <- util_normalize_cross_item(meta_data = meta_data,
                                     meta_data_cross_item =
                                       meta_data_cross_item,
                                     label_col = label_col)

    cil_in_segment <- list(all_variables = cil)
    seg_in_segment <- list(all_variables = meta_data_segment)
    dfr_in_segment <- list(all_variables = meta_data_dataframe)

    vars_in_segment <- list(all_variables = meta_data[[VAR_NAMES]])
  }

  util_setup_rstudio_job("dq_report_by")

  p <- new.env(parent = emptyenv())
  p$i <- 0
  p$N <- length(.sd_list) * length(vars_in_segment)

  mapply(sd = .sd_list, sdn = names(.sd_list), MoreArgs = list(md = meta_data),
         SIMPLIFY = FALSE, FUN = function(sd, sdn, md = meta_data) {
    util_message("Stratum %s...", sQuote(sdn))
    mapply(vars_in_segment = vars_in_segment,
           cur_seg <- names(vars_in_segment),
           SIMPLIFY = FALSE, FUN = function(vars_in_segment, cur_seg) {
      util_message("Segment %s...", sQuote(cur_seg))
      progress_msg(sprintf("Segment %s, Stratum %s...",
                           sQuote(cur_seg),
                           sQuote(sdn)))
      p$i <- p$i + 1
      progress(100 * p$i / p$N)
      before <- length(vars_in_segment)
      vars_in_segment <- intersect(vars_in_segment, colnames(sd))
      after <- length(vars_in_segment)
      if (after < before) {
        util_warning(
          "Lost %d variables due to mapping problems. %d variables left.",
          before - after, after, applicability_problem = TRUE)
      }
      sd <- sd[, vars_in_segment, FALSE]

      added_on_the_fly <- setdiff(vars_in_segment, md[md[[STUDY_SEGMENT]] ==
                                                        cur_seg, "VAR_NAMES"])

      md <- md[md[[VAR_NAMES]] %in% vars_in_segment, , FALSE]
      md[md[[VAR_NAMES]] %in% added_on_the_fly, VARIABLE_ROLE] <-
        VARIABLE_ROLES$SUPPRESS

      if (v1.0) {
        r <- dq_report(study_data = sd, meta_data = md, label_col = label_col,
                  split_segments = split_segments, ...) ## TODO: set the correct meta_data_segment, reduce the item level metadata (metadata is reduced, so, nothing is missing)
      } else {
        sdn <- paste0(name_of_study_data, "_", sdn, "_", cur_seg)
        prep_add_data_frames(data_frame_list = setNames(list(sd),
                                                        nm = sdn))
        # TODO: fix segment level metadata acccording to the segment level metadata (coutre)
        # TODO: fix dataframe level metadata to have the right study data frame name
        r <- try(dq_report2(study_data = sdn,
                   meta_data = md,
                   meta_data_cross_item = cil_in_segment[[cur_seg]],
                   meta_data_segment = seg_in_segment[[cur_seg]],
                   meta_data_dataframe = dfr_in_segment[[cur_seg]],
                   label_col = label_col,
                   notes_from_wrapper = list(mod_label$label_modification_text,
                                             mod_label$label_modification_table),
                  split_segments = split_segments, ...)) ## TODO: set the correct meta_data_segment
      }
      if (length(output_dir) == 1) {
        if (inherits(r, "try-error")) {
          util_warning(
            "Could not compute report for %s: %s.",
            dQuote(sdn),
            sQuote(conditionMessage(attr(r, "condition"))))
        } else {
          s_res <- try(prep_save_report(r, file.path(
            output_dir,
            gsub("[^a-zA-Z0-9_\\.]", "", sprintf("report_%s.dq2",
                                                 sdn))
          )))
          if (inherits(s_res, "try-error")) {
            util_warning(
              "Could not save report for %s: %s.",
              dQuote(sdn),
              sQuote(conditionMessage(attr(s_res, "condition"))))
          }
          rm(s_res)
          if (also_print) {
            .dir <- file.path(
              output_dir,
              gsub("[^a-zA-Z0-9_\\.]", "", sprintf("report_%s",
                                                   sdn))
            )
            dir.create(.dir, showWarnings = FALSE, recursive = TRUE)
            if (!dir.exists(.dir)) {
              util_warning(
                "Could not create directory %s. No HTML output for this segment",
                dQuote(.dir))
            } else {
              p_res <- try(print.dataquieR_resultset2(r, dir = .dir, view = FALSE))
              if (inherits(p_res, "try-error")) {
                util_warning(
                  "Could not create HTML report for %s: %s.",
                  dQuote(.dir),
                  sQuote(conditionMessage(attr(p_res, "condition"))))
              }
              rm(p_res)
            }
          }
        }
        rm(r)
        gc()
        return(invisible(NULL))
      } else {
        return(r)
      }
    })
  })
}
