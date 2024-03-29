#' Detect Expected Observations
#'
#' For each participant, check, if an observation was expected, given the
#' `PART_VARS` from item-level metadata
#'
#' [Descriptor]
#'
#' @param study_data [study_data] must have all relevant `PART_VARS` to avoid
#'                                false-positives on `PART_VARS` missing from
#'                                `study_data`
#' @param meta_data [meta_data] must be complete to avoid false positives on
#'                              non-existing `PART_VARS`
#' @param label_col [character] mapping attribute `colnames(study_data)` vs.
#'                              `meta_data[label_col]`
#' @param expected_observations [enum] HIERARCHY | SEGMENT. How should
#'                                     `PART_VARS` be handled:
#'                                     - `SEGMENT`: if `PART_VAR` is 1, an
#'                                       observation is expected
#'                                     - `HIERARCHY`: the default, if the
#'                                       `PART_VAR` is 1 for this variable and
#'                                       also for all `PART_VARS` of `PART_VARS`
#'                                       up in the hierarchy, an observation is
#'                                       expected.
#' @param disclose_problem_paprt_var_data [logical] show the problematic data
#'                                        (`PART_VAR` only)
#'
#' @return empty list, so far -- the function only warns.
#' @export
int_part_vars_structure <- # TODO: Support segment level metadata links to SEGMENT_PART_VARS
  function(study_data, meta_data, label_col = LABEL,
           expected_observations =
             c("HIERARCHY",
               "SEGMENT"),
           disclose_problem_paprt_var_data = FALSE) {
  util_expect_scalar(disclose_problem_paprt_var_data, check_type = is.logical)

  util_expect_scalar(expected_observations, allow_more_than_one = TRUE)
  expected_observations <- util_match_arg(expected_observations)
  util_expect_scalar(expected_observations)

  prep_prepare_dataframes()

  if (!PART_VAR %in% colnames(meta_data)) {
    meta_data[[PART_VAR]] <- NA_character_
  }

  pv <- meta_data[, PART_VAR]
  pv <- pv[!util_empty(pv)]
  if (length(pv) < nrow(meta_data)) {
    util_warning("Found %d variables w/o %s in %s",
                 nrow(meta_data) - length(pv),
                 sQuote("PART_VARS"),
                 sQuote("meta_data"))
  }

  if (!all(pv %in%
           c(meta_data[[label_col]],
             meta_data[[VAR_NAMES]],
             meta_data[[LABEL]]
           ))) {
    ms <- !(pv %in%
      c(meta_data[[label_col]],
        meta_data[[VAR_NAMES]],
        meta_data[[LABEL]]
      ))

    util_warning("Missing %d %s from the %s: %s",
                 sum(ms),
                 sQuote("PART_VARS"),
                 sQuote("meta_data"),
                 paste0(dQuote(pv[ms]), collapse = ", "))
  }

  if (!all(pv %in%
           c(colnames(ds1),
             meta_data[meta_data[[label_col]] %in% colnames(ds1),
                       VAR_NAMES, drop = TRUE],
             meta_data[meta_data[[label_col]] %in% colnames(ds1),
                       LABEL, drop = TRUE]
             )
           )) {
    ms <- !(pv %in%
              c(colnames(ds1),
                meta_data[meta_data[[label_col]] %in% colnames(ds1),
                          VAR_NAMES, drop = TRUE],
                meta_data[meta_data[[label_col]] %in% colnames(ds1),
                          LABEL, drop = TRUE]
              ))

    util_warning("Missing %d %s from the %s: %s",
                 sum(ms),
                 sQuote("PART_VARS"),
                 sQuote("study_data"),
                 paste0(dQuote(pv[ms]), collapse = ", "))
  }

  represening_rvs <- sort(unique(util_map_labels(pv,
                                     meta_data,
                                     to = label_col,
                                     from = PART_VAR)))


  r <- lapply(represening_rvs, function(rv) {

    all_need_to_be_1 <- # and the order is from root to leaf in the PART_VAR
                        # hierarchy
      util_all_intro_vars_for_rv(rv, ds1, meta_data, label_col,
                                 expected_observations = expected_observations)

    missing_vars <- setdiff(all_need_to_be_1, colnames(ds1))
    if (any(missing_vars)) {
      util_warning(c("Missing %s from %s, I fill it with NA.",
                     "This may cause inconsistencies, if below in the",
                     "hierarchy, something is expected"),
                   paste0(dQuote(missing_vars), collapse = ", "),
                   sQuote("meta_data"))
      ds1[, missing_vars] <- NA
    }
    ds1 <- ds1[, all_need_to_be_1, drop = FALSE]

## FIXME TOO SLOW
    # all part_bits must start with 1*(0|NA)* ("" contained implicitly.)
    # part_bits <- apply(ds1, 1, paste0, collapse = "")
    # structure_damaged <- !grepl("1*(0|NA)*", part_bits, perl = TRUE)
    # part_bits <- apply(ds1, 1, function(part_bits_vector) {
    #   # but handle NA, "" better and return always 1 character per var
    #   part_bits_vector[util_is_na_0_empty_or_false(part_bits_vector)] <- 0
    #   paste0(part_bits_vector, collapse = "")
    # })
    # structure_damaged <- !grepl("1*0*", part_bits, perl = TRUE)


    test_part_bits <- function(part_bits_vector) { # TODO: Not here, write an Integrity function
      # tst_vld <- c(1, 1, 1, 0, 0, NA, 0)
      # tst_ivl <- c(1, 1, 1, 0, 0, 1, 0)
      # part_bits_vector <- tst_vld
      # test_part_bits(part_bits_vector)
      # part_bits_vector <- tst_ivl
      # test_part_bits(part_bits_vector)
      part_bits_vector <- !util_is_na_0_empty_or_false(part_bits_vector)
      seqs <- rle(part_bits_vector)
      ( ((any(head(seqs$values, 1) == FALSE) && (length(seqs$values)) > 1)) ||
          any(tail(seqs$values, -1))
      )
    }
    structure_damaged <- apply(ds1, 1, test_part_bits)


    if (any(structure_damaged)) { # TODO: test this
      case_data <- case_data <- ds1[structure_damaged, , FALSE]
      case_data <- paste0(capture.output(print(case_data)), collapse = "\n")
      case_data <- paste0(":\n", case_data, "\n")
      if (!disclose_problem_paprt_var_data) {
        case_data <- ""
      }
      util_warning(c("In %s, I found %d inconsistencies in %s hiearchy, an",
                     "observation of a sub-segment cannot be expected, if the",
                     "segment where the sub-segment belongs to is not.",
                     "Found such cases, e.g., in lines %s%s of the %s%s"),
                   dQuote(util_map_labels(rv, meta_data, from = label_col,
                                          to = STUDY_SEGMENT)),
                   sum(structure_damaged),
                   dQuote(PART_VAR),
                   paste(head(which(structure_damaged), 5), collapse = ", "),
                   (if (sum(structure_damaged, na.rm = TRUE) > 5) ", ..."
                    else ""),
                   sQuote("study_data"),
                   case_data)
    }
  })
  list()
}
