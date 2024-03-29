# RECORD_MISSMATCH_CHECKTYPE
# none -> no check
# exact -> There must be a 1:1 match
# subset_u -> study data are a subset of metadata
# subset_m -> metadata are a subset of study data
options("dataquieR.ELEMENT_MISSMATCH_CHECKTYPE" = "exact")

# Wenn Anzahl Ausprägungen(Var) >"Grenzwert Metrisch" (argument frei festzulegen beim Programmaufruf default in Stata 25 Name metriclevels ) & min(Var)>=0 -> ratio
# Wenn Anzahl Ausprägungen(Var) >"Grenzwert Metrisch" (argument frei festzulegen beim Programmaufruf default in Stata 25 Name metriclevels ) & min(Var)<0 -> interval
# Wenn Anzahl Ausprägungen(Var) <="Grenzwert Metrisch" (argument frei festzulegen beim Programmaufruf default in Stata 25 Name metriclevels )   & Anzahl Ausprägungen(Var) >"Grenzwert Binär Recode" (argument frei festzulegen beim Programmaufruf default in Stata 8 Name binaryrecodelimit) -> ordinal
# Wenn Anzahl Ausprägungen(Var) <="Grenzwert Binär Recode" (argument frei festzulegen beim Programmaufruf default in Stata 8 Name binaryrecodelimit) -> nominal

dataquieR.scale_level_heuristics_control_metriclevels_default <- 25
dataquieR.scale_level_heuristics_control_binaryrecodelimit_default <- 8
options("dataquieR.scale_level_heuristics_control_metriclevels" =
          dataquieR.scale_level_heuristics_control_metriclevels_default,
        "dataquieR.scale_level_heuristics_control_binaryrecodelimit" =
          dataquieR.scale_level_heuristics_control_binaryrecodelimit_default)

dataquieR.ERRORS_WITH_CALLER_default <- TRUE
options("dataquieR.ERRORS_WITH_CALLER" =
          dataquieR.ERRORS_WITH_CALLER_default)
dataquieR.WARNINGS_WITH_CALLER_default <- TRUE
options("dataquieR.WARNINGS_WITH_CALLER" =
          dataquieR.WARNINGS_WITH_CALLER_default)
dataquieR.MESSAGES_WITH_CALLER_default <- FALSE
options("dataquieR.MESSAGES_WITH_CALLER" =
          dataquieR.MESSAGES_WITH_CALLER_default)
options("dataquieR.CONDITIONS_WITH_STACKTRACE" = FALSE)
dataquieR.CONDITIONS_LEVEL_TRHESHOLD_default <- 0
options("dataquieR.CONDITIONS_LEVEL_TRHESHOLD" =
          dataquieR.CONDITIONS_LEVEL_TRHESHOLD_default)
options("dataquieR.debug" = FALSE)

dataquieR.flip_mode_default <- "default" # flip, noflip, auto
options("dataquieR.flip_mode" = dataquieR.flip_mode_default)

dataquieR.force_item_specific_missing_codes_default <- FALSE
options("dataquieR.force_item_specific_missing_codes" =
          dataquieR.force_item_specific_missing_codes_default)

dataquieR.progress_fkt <- NULL
options("dataquieR.progress_fkt" =
          dataquieR.progress_fkt)

dataquieR.progress_msg_fkt <- NULL
options("dataquieR.progress_msg_fkt" =
          dataquieR.progress_msg_fkt)

dataquieR.grading_rulesets_default <- "grading_rulesets"
options("dataquieR.grading_rulesets" =
          dataquieR.grading_rulesets_default)

dataquieR.grading_formats_default <- "grading_formats"
options("dataquieR.grading_formats" =
          dataquieR.grading_formats_default)

dataquieR.VALUE_LABELS_htmlescaped_default <- FALSE
options(dataquieR.VALUE_LABELS_htmlescaped =
          dataquieR.VALUE_LABELS_htmlescaped_default)

dataquieR.acc_loess.mark_time_points_default <- FALSE
options("dataquieR.acc_loess.mark_time_points" =
          dataquieR.acc_loess.mark_time_points_default)

dataquieR.acc_loess.plot_observations_default <- TRUE
options("dataquieR.acc_loess.plot_observations" =
          dataquieR.acc_loess.plot_observations_default)

#TODO: document all options, how?
