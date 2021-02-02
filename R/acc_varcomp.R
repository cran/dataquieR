#' Estimates variance components
#'
#' @description
#' Variance based models and intraclass correlations (ICC) are approaches to
#' examine the impact of so-called process variables on the measurements. This
#' implementation is model-based.
#'
#' **NB:** The term ICC is frequently used to describe the agreement between
#' different observers, examiners or even devices. In respective settings a good
#' agreement is pursued. ICC-values can vary between `[-1;1]` and an ICC close
#' to 1 is desired (Koo and Li 2016, Müller and Büttner 1994).
#'
#' However, in multi-level analysis the ICC is interpreted differently. Please
#' see Snijders et al. (Sniders and Bosker 1999). In this context the proportion
#' of variance explained by respective group levels indicate an influence of (at
#' least one) level of the respective group_vars. An ICC close to 0 is desired.
#'
#' @details
#' # ALGORITHM OF THIS IMPLEMENTATION:
#'
#' - This implementation is yet restricted to data of type float.
#' - Missing codes are removed from resp_vars (if defined in the metadata)
#' - Deviations from limits, as defined in the metadata, are removed
#' - A linear mixed-effects model is estimated for resp_vars using co_vars and
#'   group_vars for adjustment.
#' - An output data frame is generated for group_vars indicating the ICC.
#'
#' @export
#' @importFrom stats as.formula na.omit median
#'
#' @param resp_vars [variable list] the names of the continuous measurement
#'                                  variables
#' @param group_vars [variable list] the names of the resp. observer, device or
#'                                   reader variables
#' @param co_vars [variable list] a vector of covariables, e.g. age and sex for
#'                                adjustment
#' @param min_obs_in_subgroup [integer] from=0. optional argument if a
#'                                   "group_var" is used. This argument
#'                                   specifies the minimum no. of observations
#'                                   that is required to include a subgroup
#'                                   (level) of the "group_var" in the analysis.
#'                                   Subgroups with less observations are
#'                                   excluded. The default is 30.
#' @param min_subgroups [integer] from=0. optional argument if a "group_var" is
#'                                        used. This argument specifies the
#'                                        minimum no. of subgroups (levels)
#'                                        included "group_var". If the variable
#'                                        defined in "group_var" has less
#'                                        subgroups it is not used for analysis.
#'                                        The default is 5.
#' @param threshold_value [numeric] from=0 to=1. a numerical value ranging
#'                                              from 0-1
#' @param study_data [data.frame] the data frame that contains the measurements
#' @param meta_data [data.frame] the data frame that contains metadata
#'                               attributes of study data
#' @param label_col [variable attribute] the name of the column in the metadata
#'                                       with labels of variables
#'
#' @return a list with:
#'   - `SummaryTable`: data frame with ICCs per `rvs`
#'   - `ScalarValue_max_icc`: maximum variance contribution value by group_vars
#'   - `ScalarValue_argmax_icc`: variable with maximum variance contribution by
#'                               group_vars
#'
#' @seealso
#' [Online Documentation](
#' https://dfg-qa.ship-med.uni-greifswald.de/VIN_acc_impl_varcomp.html
#' )
#'
#' @examples
#' \dontrun{
#' # runs spuriously slow on rhub
#' load(system.file("extdata/study_data.RData", package = "dataquieR"))
#' load(system.file("extdata/meta_data.RData", package = "dataquieR"))
#' co_vars <- c("SEX_0", "AGE_0")
#' min_obs_in_subgroup <- 30
#' min_subgroups <- 3
#' label_col <- LABEL
#' rvs <- c("DBP_0", "SBP_0")
#' group_vars <- prep_map_labels(rvs, meta_data = meta_data, from = label_col,
#'   to = VAR_NAMES)
#' group_vars <- prep_map_labels(group_vars, meta_data = meta_data,
#'   to = KEY_OBSERVER)
#' group_vars <- prep_map_labels(group_vars, meta_data = meta_data)
#' acc_varcomp(
#'   resp_vars = rvs, group_vars = group_vars, co_vars = co_vars,
#'   min_obs_in_subgroup = min_obs_in_subgroup,
#'   min_subgroups = min_subgroups, label_col = label_col,
#'   study_data = study_data, meta_data = meta_data
#' )
#' }
#'
acc_varcomp <-
  function(resp_vars = NULL, group_vars, co_vars = NULL,
           min_obs_in_subgroup = 30, min_subgroups = 5, label_col = NULL,
           threshold_value = 0.05, study_data, meta_data) {

  # map meta to study
  util_prepare_dataframes()

  .min_obs_in_subgroup <- suppressWarnings(as.integer(min_obs_in_subgroup))
  if (is.na(.min_obs_in_subgroup)) {
   util_warning(c(
    "Could not convert min_obs_in_subgroup %s to a number.",
    "Set to standard value."
    ),
     dQuote(as.character(min_obs_in_subgroup))
   )
    min_obs_in_subgroup <- 30
  } else {
    min_obs_in_subgroup <- .min_obs_in_subgroup
  }

  .min_subgroups <- suppressWarnings(as.integer(min_subgroups))
  if (is.na(.min_subgroups)) {
    util_warning(
      "Could not convert min_subgroups %s to a number. Set to standard value.",
      dQuote(as.character(min_subgroups))
    )
    min_subgroups <- 5
  } else {
    min_subgroups <- .min_subgroups
  }

  # correct variable use?
  util_correct_variable_use("resp_vars",
    allow_more_than_one = TRUE,
    allow_null = TRUE,
    need_type = "integer|float"
  )

  util_correct_variable_use("group_vars",
    allow_null = TRUE,
    allow_any_obs_na = TRUE,
    allow_more_than_one = TRUE,
    need_type = "!float"
  )

  util_correct_variable_use("co_vars",
    allow_na = TRUE,
    allow_more_than_one = TRUE,
    allow_null = TRUE
  )

  rvs <- resp_vars

  if (length(rvs) == 0) {
    rvs <- colnames(ds1[, vapply(ds1, is.numeric, FUN.VALUE = logical(1))])
    rvs <- setdiff(rvs, group_vars)
    rvs <- setdiff(rvs, co_vars)
    if (length(group_vars) != 1)
      util_error(
        "Need exactly 1 group_vars, if all applicable resp_vars are used")
  }

  if (length(group_vars) == 1 && length(rvs) > 1) {
    group_vars <- rep(group_vars, length(rvs))
    message(sprintf("using the same group var %s for all resp_vars",
                    dQuote(group_vars[[1]])
                    ))
  }

  if (length(group_vars) != length(rvs)) {
    util_error(
      c("acc_varcomp expects one group_var per resp_var. Here,",
        "it has been called with %d resp_vars but %d group_vars"),
      length(rvs), length(group_vars))
  }

  ds1[, unique(group_vars)] <- lapply(ds1[, unique(group_vars), drop = FALSE],
                                      factor)

  # (3) if no covariables are defined for adjustment only the intercept is
  #     modelled
  if (is.null(co_vars) || length(co_vars) == 0) {
    co_vars <- "1"
  }

  # missing checks
  # -> encoded missings or jumps
  # -> resp_variables
  # -> variable types (factor(), ...)


  icc_output <- apply(cbind(rvs, group_vars), 1, function(row) {
    rv <- row[[1]]
    group_var <- row[[2]]

    check_df <- data.frame(table(ds1[[group_var]]))

    # too few observations in >1 level of the random effect
    critical_levels <- levels(check_df$Var1)[check_df$Freq <
                                               min_obs_in_subgroup]
    if (length(critical_levels) > 0) {
      util_warning("Levels %s were excluded due to less than %d observations.",
                   paste0(vapply(critical_levels, dQuote, ""), collapse = ", "),
                   min_obs_in_subgroup)
      # exclude levels with too less observations
      ds1 <- ds1[!(ds1[[group_var]] %in% critical_levels), ]
      # dropping unused levels
      ds1[[group_var]] <- factor(ds1[[group_var]])
    }

    check_df <-
      data.frame(table(ds1[[group_var]])) # number of observers may have changed

    # less than min_subgroups levels of random effect
    if (length(check_df[, 1]) < min_subgroups) {
      util_warning("%d < %d levels in %s. Will not compute ICCs for %s.",
                   length(check_df[, 1]),
                   min_subgroups,
                   dQuote(group_var),
                   dQuote(rv))
      return(data.frame(
        Variables = NA,
        Object = NA,
        Model.Call = NA,
        ICC = NA,
        Class.Number = NA,
        Mean.Class.Size = NA,
        Median.Class.Size = NA,
        Min.Class.Size = NA,
        Max.Class.Size = NA,
        convergence.problem = FALSE
      )[FALSE, , FALSE])
    }

    ds1[, rv] <- lapply(ds1[, rv, drop = FALSE], util_as_numeric)

    # prepare data frame for output
    res_df <- data.frame(
      Variables = NA,
      Object = NA,
      Model.Call = NA,
      ICC = NA,
      Class.Number = nrow(check_df),
      Mean.Class.Size = mean(check_df$Freq),
      Median.Class.Size = median(check_df$Freq),
      Min.Class.Size = min(check_df$Freq),
      Max.Class.Size = max(check_df$Freq),
      convergence.problem = FALSE
    )

    # fill output df
    res_df[1, 1] <- rv
    res_df[1, 2] <- group_var
    # create model formula from function arguments
    fmla <- as.formula(paste0(paste0(rv, "~"),
                              paste0(c(co_vars, paste0("(1|", group_var, ")")),
                                     collapse = "+")))
    # save formula for results
    res_df[1, 3] <- Reduce(paste, deparse(fmla))

    # remove misisng in used variables
    subdf <- na.omit(ds1[, c(rv, co_vars[co_vars %in% colnames(ds1)],
                             group_var), drop = FALSE])
    # drop unused levels (https://stackoverflow.com/a/1197154)
    subdf[] <- lapply(subdf, function(x) if (is.factor(x)) factor(x) else x)

    env <- environment()

    convergence.problem <- FALSE

    suppressMessages(withCallingHandlers(
      # fit mixed effects model with formula
      fit_lmer <- try(lme4::lmer(fmla, data = subdf, REML = TRUE)),
      message = function(m) {
        if (inherits(m, "simpleMessage") && any(
            grepl("singular",
             trimws(conditionMessage(m))))) {
          env$convergence.problem <- TRUE
        } else {
          message(m) # nocov
          # This is to handle other possible problems with the model
          # Since it is for unexpected problems, I cannot write a test
        }
      },
      warning = function(w) {
        # nocov start
        # on travis, the try above prevents a stop on convergence problems
        # for some unclear reasons. therefore, on travis,
        # the following code cannot be reached. However, locally, this
        # is possible.
        haystack <- trimws(paste0(conditionMessage(w), collapse = "\n"))
        if (inherits(w, "simpleWarning") && any(vapply(
            c("unable to evaluate scaled gradient",
              "Problem with Hessian check",
              "Model is nearly unidentifiable: very large eigenvalue",
              "convergence code [0-9\\-].+ from nloptwrap",
              "Model failed to converge with"
              ), grepl, haystack, perl = TRUE, FUN.VALUE = logical(1)))) {
          env$convergence.problem <- TRUE
          invokeRestart("muffleWarning")
        } else {
          warning(w)
          # This is to handle other possible problems with the model
          # Since it is for unexpected problems, I cannot write a test
        }
        # nocov end
      }
    ))

    res_df[1, "convergence.problem"] <- convergence.problem

    if (inherits(fit_lmer, "try-error")) {
      res_df[1, "convergence.problem"] <- TRUE
    } else {
      # extract variance of random effects
      v_tab <- as.data.frame(lme4::VarCorr(fit_lmer))
      res_df$Class.Number[[1]] <- lme4::ngrps(fit_lmer)
      # calculate icc
      res_df[1, 4] <- round(v_tab$vcov[1] / (v_tab$vcov[1] + v_tab$vcov[2]), 3)
    }

    class_sizes <- as.data.frame(table(subdf[, group_var]))
    res_df <- within(res_df, {
      Mean.Class.Size[[1]] <- mean(class_sizes$Freq)
      Median.Class.Size[[1]] <- median(class_sizes$Freq)
      Min.Class.Size[[1]] <- min(class_sizes$Freq)
      Max.Class.Size[[1]] <- max(class_sizes$Freq)
    })

    # output
    return(res_df)
  })

  icc_output <- as.data.frame(do.call(dplyr::bind_rows, icc_output),
                              stringsAsFactors = FALSE)
  rownames(icc_output) <- NULL

  max_icc <- suppressWarnings(max(icc_output$ICC, na.rm = TRUE))
  argmax_icc <- icc_output$Variables[which.max(icc_output$ICC)]

  icc_output[["GRADING"]] <- as.numeric(threshold_value <= icc_output$ICC)

  ## Format

  rownames(icc_output) <- NULL

  return(list(SummaryTable = icc_output, ScalarValue_max_icc = max_icc,
              ScalarValue_argmax_icc = argmax_icc))
}
