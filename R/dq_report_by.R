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
#' @param output_dir [character] if given, the output is not returned but
#' @param also_print [logical] if `output_dir` is not `NULL`, also create
#'                             `HTML` output for each segment using
#'                             [print.dataquieR_resultset2()].
#'                               written to the path `output_dir`
#' @param disable_plotly [logical] do not use `plotly`, even if installed
#'
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
#' dq_report_by(study_data = "study_data", meta_data_v2 = "meta_data_v2",
#'   advanced_options = list(dataquieR.study_data_cache_max = 0,
#'   dataquieR.study_data_cache_metrics = TRUE,
#'   dataquieR.study_data_cache_metrics_env = environment()),
#'   cores = NULL, dimensions = "int")
#' dq_report_by(study_data = "study_data", meta_data_v2 = "meta_data_v2",
#'   advanced_options = list(dataquieR.study_data_cache_max = 0),
#'   cores = NULL, dimensions = "int")
#' }
dq_report_by <- function(study_data, meta_data = "item_level",
                         meta_data_segment = "segment_level",
                         meta_data_dataframe = "dataframe_level",
                         meta_data_cross_item = "cross-item_level",
                         label_col,
                         meta_data_v2,
                         meta_data_split =
                           STUDY_SEGMENT, study_data_split, ...,
                         output_dir = NULL,
                         also_print = FALSE,
                         disable_plotly = FALSE) {
  # creating a list "toc" to trace what will be created, and set an environment to
  # contain the list
  toc <- list()
  toc_env <- environment()
  add_to_toc <- function(sdn, cur_seg, dir) {
    if (!sdn %in% names(toc_env$toc)) {
      toc_env$toc[[sdn]] <- list()
    }
    toc_env$toc[[sdn]][[cur_seg]] <- dir
  }

  # look up the call of the function to get the name of the study data
  arg_study_data <- head(as.character(substitute(study_data)), 1)

  # in case of presence of meta_data_v2 purge the cache and load it
  if (!missing(meta_data_v2)) {
    util_message("Have %s set, so I'll remove all loaded data frames",
                 sQuote("meta_data_v2"))
    prep_purge_data_frame_cache()
    prep_load_workbook_like_file(meta_data_v2)
    # check if there is item level metadata in the cache and provide an error

    # if it is not present it predicts item-level from the data
    if (!is.data.frame(meta_data) &&
        (!is.character(meta_data) || length(meta_data) != 1) ||
        !exists(meta_data, .dataframe_environment)) {

      w <- paste("Did not find any sheet named %s in %s, is this",
                 "really dataquieR version 2 metadata?")
      if (requireNamespace("cli", quietly = TRUE)) {
        w <- cli::bg_red(cli::col_br_yellow(w))
      }
      util_warning(w, dQuote(meta_data), dQuote(meta_data_v2),
                   immediate = TRUE)
    }
  }

  # check if meta_data is a data frame, if not looks for it in the cache and in the file system
  util_expect_data_frame(meta_data)
  # check if there is a cross item metadata and it is a data frame
  # in case is not present, create an empty data frame for cross item metadata
  try(util_expect_data_frame(meta_data_cross_item), silent = TRUE)
  if (!is.data.frame(meta_data_cross_item)) {
    util_message("No cross-item level metadata %s found",
                 dQuote(meta_data_cross_item))
    meta_data_cross_item <- data.frame(VARIABLE_LIST = character(0),
                                       CHECK_LABEL = character(0))
  }
  # check if meta_data contains the column VAR_NAMES
  suppressWarnings(util_ensure_in(VAR_NAMES, names(meta_data), error = TRUE,
                                  err_msg =
                                    sprintf("Did not find the mandatory column %%s in the %s.",
                                            sQuote("meta_data"))))
  # check if a column VARIABLE_ROLE is present in the metadata.
  # If not create one with always "primary" as role
  if (!(VARIABLE_ROLE %in% colnames(meta_data))) {
    util_message("No %s assigned in item level metadata. Defaulting to %s.",
                 sQuote(VARIABLE_ROLE), dQuote(VARIABLE_ROLES$PRIMARY),
                 applicability_problem = TRUE)
    meta_data$VARIABLE_ROLE <- VARIABLE_ROLES$PRIMARY
  }

  # if users specify an output dir, check if it is a scalar and a character
  # string, check if also_print is a scalar and logical,
  # stop if the specified dir already exists
  # provide an error if the dir can not be created
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

  # define label_col as "LABEL" if they are not specified by users
  if (missing(label_col)) {
    label_col <- LABEL
  }

  # ensure that labels exist, are unique and not too long
  mod_label <- util_ensure_label(study_data = study_data,
                                 meta_data = meta_data,
                                 label_col = label_col)
  if (!is.null(mod_label$label_modification_text)) {
    # There were changes in the metadata.
    study_data <- mod_label$study_data
    meta_data <- mod_label$meta_data
  }

  # check if there is a study data
  # if it is a dataframe, use the name from the call,
  # if the user indicate a name of a data frame, use that name (not loaded yet)
  # otherwise use "no study data found"
  if (is.data.frame(study_data)) {
    name_of_study_data <- arg_study_data
  } else if (length(study_data) == 1 && is.character(study_data)) {
    name_of_study_data <- study_data
  } else {
    name_of_study_data <- "??No study data found??"
  }

  # Create the ds1 data frame, with variable names replaced by labels and
  # missing and jump codes are not replaced by NAs
  prep_prepare_dataframes(.replace_missings = FALSE)

  # Define the study_data_split
  # If present, set to use the variable indicate by user
  # to split the study data row-wise
  if (!missing(study_data_split)) {
    .study_data_split <- study_data_split
  }
  # if both split and label col refers to labels, match the labels to var_names
  # this means that replaces study_data_split from label to var_names
  if (label_col != VAR_NAMES && !missing(study_data_split)) {
    study_data_split <-
      util_map_labels(study_data_split, meta_data, VAR_NAMES, label_col)
  }
  # if no argument to split present, create a list anyways with all_observations
  if (missing(study_data_split) || is.null(study_data_split) ||
      !study_data_split %in% colnames(study_data)) {
    .sd_list <- list(all_observations = study_data)
  } else {   # split the data and convert names of the groups to VAR_NAMES_actualGroupValues
    .sd_list <- split(study_data, study_data[[study_data_split]])
    names(.sd_list) <- paste0(.study_data_split, "_", names(.sd_list))
  }

  # define the meta_data_split. It can only be by STUDY_SEGMENT, automatically
  # separate by STUDY SEGMENT unless set to NULL
  if (missing(meta_data_split)) {
    if (STUDY_SEGMENT %in% colnames(meta_data)) { #TODO: StS allow other metadata attributes, e.g., report name or variable group
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

  # take the name of all the columns from item level metadata that refers
  # to other variables
  key_cols <- util_variable_references(meta_data)
  # define an object to use later to either FALSE if there is no splitting or
  # TRUE otherwise
  split_segments <- FALSE
  # in case there is a column defined for the split (for example study segment),
  # if there are empty entries in the column defined for the split, set
  # a new non-used segment name (e.g., na1) and use it for rows with empty entries
  if (!is.null(meta_data_split)) {
    split_segments <- TRUE
    .md <- meta_data[[meta_data_split]]
    i <- ""
    while (any(.md == paste0("na", i), na.rm = TRUE)) {
      if (i == "") i <- 0
      i <- i + 1
    }
    .md[util_empty(.md)] <- paste0("na", i)
    meta_data[[meta_data_split]] <- .md
    # define an object containing the segments to create
    segments <- unique(meta_data[[meta_data_split]])
    # backwards compatibility - replace var_names with label in segments to create
    if (label_col != VAR_NAMES && all(segments %in% meta_data[[VAR_NAMES]])) {
      segmentNames <- util_map_labels(segments, meta_data, label_col)
    } else {
      segmentNames <- segments
    }
    # check cross-item level that can contain variables needed but not present
    # in the segment
    # First normalize input from the user
    cil <- util_normalize_cross_item(meta_data = meta_data,
                                     meta_data_cross_item =
                                       meta_data_cross_item,
                                     label_col = label_col)
    # extract all rules containing a variables from the current evaluated segment
    cil_in_segment <- lapply(setNames(segments, nm = segmentNames),
                             function(segment) {
                               vars <-
                                 meta_data[meta_data[[meta_data_split]] ==
                                             segment, VAR_NAMES]
                               #replace var_names with labels
                               vars <- prep_map_labels(vars,
                                                       meta_data = meta_data,
                                                       to = label_col,
                                                       from = VAR_NAMES)
                               # extracting the variable names from the column
                               # variable_list in cross-item metadata to have
                               # a vector of variable names for each rule in the
                               # list rules_vars
                               rules_vars <-
                                 util_parse_assignments(cil$VARIABLE_LIST,
                                                        multi_variate_text = TRUE)
                               # intersect vars in the rules with vars
                               # in the segment to see which rules affect the
                               # current segment
                               rules_to_use <-
                                 vapply(lapply(rules_vars, intersect, vars),
                                        length, FUN.VALUE = integer(1)) > 0
                               cil[rules_to_use, , FALSE] #discard rules not
                               # affected by the current segment
                             })
    # Look to the data frame interested by the current segment
    # this is a preparation for future implementation, for the moment takes
    # the same data frame for all segments
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
    # Prepare metadata at the segment level, selecting the row matching
    # the current segment
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
    #For each segment define which variables are to include (using a repeat loop)
    vars_in_segment <- lapply(setNames(segments, nm = segmentNames),
                              function(segment) {
                                # First select the variables that are indicated
                                # as part of the segment in the item-level metadata
                                vars <- meta_data[meta_data[[meta_data_split]] == segment, VAR_NAMES]
                                repeat { #The actual loop starts and looks for
                                  # all referred variables:
                                  # To do that extract all rows from the metadata
                                  # that describe variables already in vector vars,
                                  # and only column referring to other variables (key_cols)
                                  referred_vars <-
                                    unique(unlist(meta_data[meta_data[[VAR_NAMES]] %in% vars, key_cols],
                                                  recursive = TRUE))
                                  # then all variables from the rules in cross-item level metadata
                                  # that are related to the segment
                                   referred_vars <-
                                    c(referred_vars, unlist(util_parse_assignments(
                                      cil_in_segment[[segment]][[VARIABLE_LIST]],
                                      multi_variate_text = TRUE
                                    )))
                                  # then add id variables from segment-level metadata
                                  referred_vars <-
                                    c(referred_vars, seg_in_segment[[segment]][[SEGMENT_ID_VARS]])
                                  # then add id variables from dataframe-level metadata
                                  referred_vars <-
                                    c(referred_vars, dfr_in_segment[[segment]][[DF_ID_VARS]])
                                  # looks to all possible labels and return var_names
                                  referred_vars <-
                                    unique(util_find_var_by_meta(unique(
                                      unlist(util_parse_assignments(referred_vars,
                                                                    multi_variate_text = TRUE))),
                                      meta_data = meta_data))
                                  #remove na's
                                  referred_vars <- referred_vars[!is.na(referred_vars)]
                                  if (all(referred_vars %in% vars)) {
                                    break #if all variables are already present than it stops
                                  } else { #otherwise add the new variables and restart the loop
                                    vars <- union(vars, referred_vars)
                                  }
                                }
                                # return all variables obtained in the vector vars
                                # for the current segment
                                vars
                              })
  } else { #in case no split is set, it just includes all variables and all metadata
    cil <- util_normalize_cross_item(meta_data = meta_data,
                                     meta_data_cross_item =
                                       meta_data_cross_item,
                                     label_col = label_col)
    # create a list anyways but only with one element for all the metadata
    cil_in_segment <- list(all_variables = cil)
    seg_in_segment <- list(all_variables = meta_data_segment)
    dfr_in_segment <- list(all_variables = meta_data_dataframe)

    vars_in_segment <- list(all_variables = meta_data[[VAR_NAMES]])
  }
  # create a job to report progress and give it a name
  util_setup_rstudio_job("dq_report_by")
  # create an extra environment for the progress
  p <- new.env(parent = emptyenv())
  p$i <- 0
  p$N <- length(.sd_list) * length(vars_in_segment) #no. total iterations
                                             # (no. dataframes * no. segments)
  # Return a list of lists of results. The outer list is for each of the study data
  # strata and the inner list is for the meta data split, e.g., study segments
  overall_res <-
    #first it looks to the split in the study data and then the split in the metadata (by segment)
    mapply(sd = .sd_list, sdn = names(.sd_list), MoreArgs = list(md = meta_data),
           SIMPLIFY = FALSE, FUN = function(sd, sdn, md = meta_data) {
             util_message("Stratum %s...", sQuote(sdn))
             mapply(vars_in_segment = vars_in_segment,  #here start the inner list by segment
                    cur_seg = names(vars_in_segment),
                    SIMPLIFY = FALSE, FUN = function(vars_in_segment, cur_seg) {
                      util_message("Segment %s...", sQuote(cur_seg))
                      progress_msg(sprintf("Segment %s, Stratum %s...",
                                           sQuote(cur_seg),
                                           sQuote(sdn)))
                      #update the progress bar
                      p$i <- p$i + 1
                      progress(100 * p$i / p$N)
                      #check if variables are present in both study data and metadata
                      before <- length(vars_in_segment)
                      vars_in_segment <- intersect(vars_in_segment, colnames(sd))
                      after <- length(vars_in_segment)
                      if (after < before) {
                        util_warning(
                          "Lost %d variables due to mapping problems. %d variables left.",
                          before - after, after, applicability_problem = TRUE)
                      }
                      sd <- sd[, vars_in_segment, FALSE] # Filtering study data
                      # for the current segment
                      #to keep only the variables needed

                      #check which variables are not already in the study segment
                      added_on_the_fly <- setdiff(vars_in_segment, md[md[[STUDY_SEGMENT]] ==
                                                                        cur_seg, "VAR_NAMES"])
                      #Filtering metadata to keep only needed variables
                      md <- md[md[[VAR_NAMES]] %in% vars_in_segment, , FALSE]
                      # Set VARIABLE_ROLE to suppress for variables added and not
                      # already present in the study segment (added_on_the_fly)
                      md[md[[VAR_NAMES]] %in% added_on_the_fly, VARIABLE_ROLE] <-
                        VARIABLE_ROLES$SUPPRESS

                      # Define a new name composed of study_data_current stratum_current segment
                      sdn <- paste0(name_of_study_data, "_", sdn, "_", cur_seg)
                      # Add the subset of the study data need for the current segment
                      # as another dataframe to the cache
                      prep_add_data_frames(data_frame_list = setNames(list(sd),
                                                                      nm = sdn))
                      # TODO: fix segment level metadata according to the segment level metadata (coutre)
                      # TODO: fix dataframe level metadata to have the right study data frame name
                      # create the different sub-reports
                      r <- try(dq_report2(study_data = sdn,
                                          meta_data = md,
                                          meta_data_cross_item = cil_in_segment[[cur_seg]],
                                          meta_data_segment = seg_in_segment[[cur_seg]],
                                          meta_data_dataframe = dfr_in_segment[[cur_seg]],
                                          label_col = label_col,
                                          notes_from_wrapper = list(label_modification_text =
                                                                      mod_label$label_modification_text,
                                                                    label_modification_table =
                                                                      mod_label$label_modification_table),
                                          split_segments = split_segments, ...)) ## TODO: set the correct meta_data_segment
                      # in case an output directory is defined, save reports and
                      # delete them
                      if (length(output_dir) == 1) {
                        if (inherits(r, "try-error")) { #check if instead of the report there is an error
                          util_warning(
                            "Could not compute report for %s: %s.",
                            dQuote(sdn),
                            sQuote(conditionMessage(attr(r, "condition"))))
                        } else {
                          # Save the report as R object
                          s_res <- try(prep_save_report(r, file.path(
                            output_dir,
                            gsub("[^a-zA-Z0-9_\\.]", "", sprintf("report_%s.dq2",
                                                                 sdn))
                          )))
                          if (inherits(s_res, "try-error")) { # if the report could not be saved
                            # gives a warning
                            util_warning(
                              "Could not save report for %s: %s.",
                              dQuote(sdn),
                              sQuote(conditionMessage(attr(s_res, "condition"))))
                          }
                          # remove object to reduce memory use
                          rm(s_res)

                          # Save the summaries for a quick overview of all the reports
                          s_res <- try(saveRDS(summary(r), file.path(
                            output_dir,
                            gsub("[^a-zA-Z0-9_\\.]", "", sprintf("report_summary_%s.RDS",
                                                                 sdn))
                          )))
                          if (inherits(s_res, "try-error")) { # if the summary could not be saved
                            util_warning(
                              "Could not save report summary for %s: %s.",
                              dQuote(sdn),
                              sQuote(conditionMessage(attr(s_res, "condition"))))
                          }
                          # remove object to reduce memory use
                          rm(s_res)

                          # prepare the folder for saving the rendered report html file
                          if (also_print) {
                            .dir <- file.path(
                              output_dir,
                              gsub("[^a-zA-Z0-9_\\.]", "", sprintf("report_%s",
                                                                   sdn))
                            )
                            dir.create(.dir, showWarnings = FALSE, recursive = TRUE)
                            if (!dir.exists(.dir)) {
                              # warning if the directory could not be created
                              util_warning(
                                "Could not create directory %s. No HTML output for this segment",
                                dQuote(.dir))
                            } else {
                              # save the rendered html files in the directory
                              p_res <- try(print.dataquieR_resultset2(r, dir = .dir,
                                                                      view = FALSE,
                                                                      disable_plotly = disable_plotly,
                                                                      by_report = TRUE)) #creating the back link
                              if (inherits(p_res, "try-error")) {
                                # gives a warning if the report could not be rendered
                                util_warning(
                                  "Could not create HTML report for %s: %s.",
                                  dQuote(.dir),
                                  sQuote(conditionMessage(attr(p_res, "condition"))))
                              } else {
                                # update the table of contents
                                add_to_toc(sdn = sdn, cur_seg = cur_seg, dir = .dir)
                              }
                              # remove object to reduce memory use
                              rm(p_res)
                            }
                          }
                        }
                        # remove the reports to reduce memory use
                        rm(r)
                        gc()
                        return(invisible(NULL))
                      } else {
                        #if no output directory is indicated, return the report
                        return(r)
                      }
                    })
           })

  #creates the html overview page that links all sub-reports created
  if (!missing(output_dir) && also_print) {
    # extract the names of the segments for each strata (e.g.,  "PART_STUDY")
    segs <- vapply(toc, names, FUN.VALUE = character(1))

    nchar_segs <- nchar(segs) #counts the no. characters for each innerlist(segment names)
    #extract the names of the outer level strata (e.g., Studycenter_1)

    #extract the strata names, without the study name and the segment names
    # example: "CENTER_0_1"
    n <- substr(names(toc),
                nchar(name_of_study_data) + 2,  #start after the first underscore
                nchar(names(toc)) - nchar_segs - 1)

    #if there is no split by study data, there are no strata
    if (missing(study_data_split)) {
      if (!is.null(meta_data_split) && nzchar(meta_data_split))
        title <- paste(meta_data_split, "=", segs)
      else
        title <- segs
    } else {
      #extract only values of strata variables
      ns <- substr(n, nchar(.study_data_split) + 2, nchar(n))
      # makes a nice name for the report (e.g., "CENTER_0 = 1, STUDY_SEGMENT = PART_STUDY")
      title <- paste0(paste(.study_data_split, "=", ns), ", ",
                      paste(meta_data_split, "=", segs))
    }
    #computing the hyper-reference, the link to the files
    href <- substr(paste0(vapply(toc, `[[`, 1,
                                 FUN.VALUE = character(1)), "/index.html"),
                   nchar(output_dir) + 2, 10000)
    util_ensure_suggested("htmltools", "generate reports TOC")
    #create the full table of contents, the actual html file
    toc <- htmltools::tags$html(htmltools::tags$body(htmltools::tags$ul(
      htmltools::HTML(
        vapply(lapply(mapply(SIMPLIFY = FALSE, FUN = htmltools::a,
                             href = href, title),
                      htmltools::tags$li), as.character, FUN.VALUE = character(1))
      ))))
    #convert the html object to a character
    toc <- as.character(toc)
    # prepare a file in utf-8 mode
    f <- file(description = file.path(output_dir, "index.html"),
              open = "w", encoding = "utf-8")
    on.exit(close(f)) #when the function is finished close the file
    #write the table of contents inside the file
    cat(toc, file = f)
  }
  return(overall_res)
}
