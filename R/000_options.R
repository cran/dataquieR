# RECORD_MISSMATCH_CHECKTYPE
# none -> no check
# exact -> There must be a 1:1 match
# subset_u -> study data are a subset of metadata
# subset_m -> metadata are a subset of study data
options("dataquieR.RECORD_MISSMATCH_CHECKTYPE" = "exact")
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

#TODO: document all options, how?
