#' All available probability distributions for
#' [acc_shape_or_scale]
#'
#'  - `uniform` For uniform distribution
#'  - `normal` For Gaussian distribution
#'  - `gamma` For a gamma distribution
#'
#' @export
DISTRIBUTIONS <- list(
  UNIFORM = "uniform",
  NORMAL = "normal",
  GAMMA = "gamma"
)

#' @title Names of DQ dimensions
#' @name dimensions
#' @description
#'
#' a vector of data quality dimensions. The supported
#' dimensions are Completeness, Consistency and Accuracy.
#'
#' @seealso [Data Quality Concept](
#'   https://dataquality.qihs.uni-greifswald.de/DQconceptNew.html)
#'
#' @return Only a definition, not a function, so no return value
#'
dimensions = c("Completeness", "Consistency", "Accuracy")

#' Data Types
#'
#' ## Data Types of Study Data
#' In the metadata, the following entries are allowed for the
#' [variable attribute] [DATA_TYPE]:
#'
#'  - `integer` for integer numbers
#'  - `string` for text/string/character data
#'  - `float` for decimal/floating point numbers
#'  - `datetime` for timepoints
#'  - `time` for time of day
#'
#' ## Data Types of Function Arguments
#' As function arguments, [dataquieR] uses additional type specifications:
#'
#' - `numeric` is a numerical value ([float] or [integer]), but it is not an
#'   allowed `DATA_TYPE` in the metadata. However, some functions may accept
#'   `float` or `integer` for specific function arguments. This is, where we
#'   use the term `numeric`.
#' - `enum` allows one element out of a set of allowed options similar to
#'   [match.arg]
#' - `set` allows a subset out of a set of allowed options similar to
#'   [match.arg] with `several.ok = TRUE`.
#' - `variable` Function arguments of this type expect a character scalar that
#'              specifies one variable using the variable identifier given in
#'              the metadata attribute `VAR_NAMES` or, if `label_col` is set,
#'              given in the metadata attribute given in that argument.
#'              Labels can easily be translated using [prep_map_labels]
#' - `variable list` Function arguments of this type expect a character vector
#'                   that specifies variables using the variable identifiers
#'                   given in the metadata attribute `VAR_NAMES` or,
#'                   if `label_col` is set, given in the metadata attribute
#'                   given in that argument. Labels can easily be translated
#'                   using [prep_map_labels]
#'
#' @seealso [integer] [string]
#' @aliases integer string float datetime time numeric enum
#'          FLOAT INTEGER STRING DATETIME TIME variable set
#' @rawRd \alias{variable list}
#'
#' @export
DATA_TYPES <- list(
  INTEGER = "integer",
  STRING = "string",
  FLOAT = "float",
  DATETIME = "datetime",
  TIME = "time"
)

#' Scale Levels
#'
#' ## Scale Levels of Study Data according to `Stevens's` Typology
#' In the metadata, the following entries are allowed for the
#' [variable attribute] [SCALE_LEVEL]:
#'
#'  - `nominal` for categorical variables
#'  - `ordinal` for ordinal variables (i.e., comparison of values is possible)
#'  - `interval` for interval scales, i.e., distances are meaningful
#'  - `ratio` for ratio scales, i.e., ratios are meaningful
#'  - `na` for variables, that contain e.g. unstructured texts, `json`,
#'          `xml`, ... to distinguish them from variables, that still need to
#'          have the `SCALE_LEVEL` estimated by
#'          `prep_scalelevel_from_data_and_metadata()`
#'
#' ## Examples
#'
#' - sex, eye color -- `nominal`
#' - income group, education level -- `ordinal`
#' - temperature in degree Celsius -- `interval`
#' - body weight, temperature in Kelvin -- `ratio`
#'
#' @seealso [Wikipedia](https://en.wikipedia.org/wiki/Level_of_measurement)
#' @export
SCALE_LEVELS <-
  list(
    NOMINAL = "nominal",
    ORDINAL = "ordinal",
    INTERVAL = "interval",
    RATIO = "ratio",
    'NA' = "na"
  ) # TODO: validate item level metadata `SCALE_LEVEL` entries somewhere.

#' All available data types, mapped from their respective
#' R types
#' @seealso [`prep_dq_data_type_of`]
#' @export
DATA_TYPES_OF_R_TYPE <- list(
  # order important for prep_dq_data_type_of --
  # start with the most restrictive types
  logical = DATA_TYPES$INTEGER,
  integer = DATA_TYPES$INTEGER,
  ordered = DATA_TYPES$STRING,
  factor = DATA_TYPES$STRING,
  POSIXct = DATA_TYPES$DATETIME,
  POSIXlt = DATA_TYPES$DATETIME,
  POSIXt = DATA_TYPES$DATETIME,
  Date = DATA_TYPES$DATETIME,
  dates = DATA_TYPES$DATETIME,
  times = DATA_TYPES$TIME,
  hms = DATA_TYPES$TIME,
  times = DATA_TYPES$DATETIME,
  double = DATA_TYPES$FLOAT,
  numeric = DATA_TYPES$FLOAT,
  character = DATA_TYPES$STRING,
  ITime = DATA_TYPES$TIME,
  time_of_day = DATA_TYPES$TIME
)

#' Variable roles can be one of the following:
#'
#'   - `intro` a variable holding consent-data
#'   - `primary` a primary outcome variable
#'   - `secondary` a secondary outcome variable
#'   - `process` a variable describing the measurement process
#'   - `suppress` a variable added on the fly computing sub-reports, i.e., by
#'                [dq_report_by] to have all referred variables available,
#'                even if they are not part of the currently processed segment.
#'                But they will only be fully assessed in their real segment's
#'                report.
#'
#' @rawRd \alias{variable roles}
#' @export
VARIABLE_ROLES <- list(
  INTRO = "intro",
  PRIMARY = "primary",
  SECONDARY = "secondary",
  PROCESS = "process",
  SUPPRESS = "suppress"
)

#' @name MAXIMUM_LONG_STRING
#' @title Cross-item level metadata attribute name
#' @description
#'
#' TODO
#'
#' @seealso [meta_data_cross]
#' @family meta_data_cross
#' @family SSI
NULL

#' @name IRV
#' @title Cross-item level metadata attribute name
#' @description
#'
#' TODO
#'
#' @seealso [meta_data_cross]
#' @family meta_data_cross
#' @family SSI
NULL

#' @name TOTRESPT
#' @title Cross-item level metadata attribute name
#' @description
#'
#' TODO
#'
#' @seealso [meta_data_cross]
#' @family meta_data_cross
#' @family SSI
NULL

#' @name RELCOMPL_SPEED
#' @title Cross-item level metadata attribute name
#' @description
#'
#' TODO
#'
#' @seealso [meta_data_cross]
#' @family meta_data_cross
#' @family SSI
NULL

#' @name RESPT_PER_ITEM
#' @title Cross-item level metadata attribute name
#' @description
#'
#' TODO
#'
#' @seealso [meta_data_cross]
#' @family meta_data_cross
#' @family SSI
NULL

#' @name RELCOMPL_SPEED
#' @title Cross-item level metadata attribute name
#' @description
#'
#' TODO
#'
#' @seealso [meta_data_cross]
#' @family meta_data_cross
#' @family SSI
NULL

#' @name MISS_RESP
#' @title Cross-item level metadata attribute name
#' @description
#'
#' TODO
#'
#' @seealso [meta_data_cross]
#' @family meta_data_cross
#' @family SSI
NULL


#' @name COMPUTED_VARIABLE_ROLES
#' @title `SSI` related Cross-item level metadata attribute names
#' Computed Variable roles can be one of the following:
#' @description
#'
#'   - `MAXIMUM_LONG_STRING` Social Science: Computed Indicator Variable,
#'                           maximum long string
#'   - `IRV` Social Science: Computed Indicator Variable, `IRV`
#'   - `TOTRESPT` Social Science: Computed Indicator Variable, `TOTRESPT`
#'   - `RESPT_PER_ITEM` Social Science: Computed Indicator Variable, `RESPT_PER_ITEM`
#'   - `RELCOMPL_SPEED` Social Science: Computed Indicator Variable, `RELCOMPL_SPEED`
#'   - `MISS_RESP` Social Science: Computed Indicator Variable, `MISS_RESP`
#'   - `NA` Social Science: Computed Indicator Variable -- N/A
#'
#' @seealso [VARIABLE_ROLES]
#' @family meta_data_cross
#' @family SSI
NULL

#' Character used  by default as a separator in metadata such as
#'           missing codes
#'
#' This 1 character is according to our metadata concept `r dQuote(SPLIT_CHAR)`.
#' @export
SPLIT_CHAR <- "|"

# constants for the names of defined metadata attributes / meta reference
# attributes
REQUIREMENT_ATT <- "var_att_required"

#' Requirement levels of certain metadata columns
#'
#' These levels are cumulatively used by the function [prep_create_meta] and
#' related in the argument `level` therein.
#'
#' currently available:
#'
#' ```{r echo=FALSE,results='asis'}
#' cat(
#'   paste(" -", sQuote(unlist(names(VARATT_REQUIRE_LEVELS))), "=",
#'               dQuote(unlist(VARATT_REQUIRE_LEVELS)), collapse = "\n")
#' )
#' ```
#'
#' @export
#' @aliases COMPATIBILITY REQUIRED RECOMMENDED OPTIONAL TECHNICAL UNKNOWN
VARATT_REQUIRE_LEVELS <- list(
  COMPATIBILITY = "compatibility",
  REQUIRED = "required",
  RECOMMENDED = "recommended",
  OPTIONAL = "optional",
  TECHNICAL = "technical"
)

VARATT_REQUIRE_LEVELS_ORDER <- c(
  # for cumulative access from util_get_var_att_names_of_level
  VARATT_REQUIRE_LEVELS$REQUIRED,
  VARATT_REQUIRE_LEVELS$RECOMMENDED,
  VARATT_REQUIRE_LEVELS$OPTIONAL,
  VARATT_REQUIRE_LEVELS$COMPATIBILITY,
  VARATT_REQUIRE_LEVELS$TECHNICAL
)

#' Well-known metadata column names, names of metadata columns
#'
#' names of the variable attributes in the metadata frame holding
#' the names of the respective observers, devices, lower limits for plausible
#' values, upper limits for plausible values, lower limits for allowed values,
#' upper limits for allowed values, the variable name (column name, e.g.
#' v0020349) used in the study data,  the variable name used for processing
#' (readable name, e.g. RR_DIAST_1) and in parameters of the QA-Functions, the
#' variable label, variable long label, variable short label, variable data
#' type (see also [DATA_TYPES]), re-code for definition of lists of event
#' categories, missing lists and jump lists as CSV strings. For valid units
#' see [UNITS].
#'
#' all entries of this list will be mapped to the package's exported NAMESPACE
#' environment directly, i.e. they are available directly by their names too:
#'
#' `r paste0(' - [', names(WELL_KNOWN_META_VARIABLE_NAMES), ']', collapse = "\n")`
#'
#' @rawRd \alias{variable attribute}
#'
#' @eval c("@aliases", paste(names(WELL_KNOWN_META_VARIABLE_NAMES)))
#'
#' @seealso [meta_data_segment] for `STUDY_SEGMENT`
#' @seealso [online](https://dataquality.qihs.uni-greifswald.de/VIN_Item_Level_Metadata.html)
#' @family UNITS
#'
#' @examples
#' print(WELL_KNOWN_META_VARIABLE_NAMES$VAR_NAMES)
#' # print(VAR_NAMES) # should usually also work
#' @export
WELL_KNOWN_META_VARIABLE_NAMES <- list(
  VAR_NAMES = structure("VAR_NAMES", var_att_required =
                          VARATT_REQUIRE_LEVELS$REQUIRED),
  LABEL = structure("LABEL", var_att_required =
                      VARATT_REQUIRE_LEVELS$RECOMMENDED),
  DATA_TYPE = structure("DATA_TYPE", var_att_required =
                          VARATT_REQUIRE_LEVELS$REQUIRED),
  SCALE_LEVEL = structure("SCALE_LEVEL", var_att_required =
                            VARATT_REQUIRE_LEVELS$RECOMMENDED),
  UNIT = structure("UNIT", var_att_required =
                            VARATT_REQUIRE_LEVELS$RECOMMENDED),
  VALUE_LABELS = structure("VALUE_LABELS", var_att_required =
                             VARATT_REQUIRE_LEVELS$RECOMMENDED),
  VALUE_LABEL_TABLE = structure("VALUE_LABEL_TABLE", var_att_required =
                             VARATT_REQUIRE_LEVELS$OPTIONAL),
  MISSING_LIST = structure("MISSING_LIST", var_att_required =
                             VARATT_REQUIRE_LEVELS$COMPATIBILITY),
  JUMP_LIST = structure("JUMP_LIST", var_att_required =
                          VARATT_REQUIRE_LEVELS$COMPATIBILITY),
  MISSING_LIST_TABLE = structure("MISSING_LIST_TABLE", var_att_required =
                                   VARATT_REQUIRE_LEVELS$REQUIRED),
  HARD_LIMITS = structure("HARD_LIMITS", var_att_required =
                            VARATT_REQUIRE_LEVELS$RECOMMENDED),
  DETECTION_LIMITS = structure("DETECTION_LIMITS", var_att_required =
                                 VARATT_REQUIRE_LEVELS$OPTIONAL),
  SOFT_LIMITS = structure("SOFT_LIMITS", var_att_required =
                            VARATT_REQUIRE_LEVELS$OPTIONAL),
  CONTRADICTIONS = structure("CONTRADICTIONS", var_att_required =
                               VARATT_REQUIRE_LEVELS$OPTIONAL),
  DISTRIBUTION = structure("DISTRIBUTION", var_att_required =
                             VARATT_REQUIRE_LEVELS$OPTIONAL),
  DECIMALS = structure("DECIMALS", var_att_required =
                         VARATT_REQUIRE_LEVELS$OPTIONAL),
  DATA_ENTRY_TYPE = structure("DATA_ENTRY_TYPE", var_att_required =
                                VARATT_REQUIRE_LEVELS$OPTIONAL),
  END_DIGIT_CHECK = structure("END_DIGIT_CHECK", var_att_required =
                                VARATT_REQUIRE_LEVELS$OPTIONAL),
  CO_VARS = structure("CO_VARS", var_att_required =
                        VARATT_REQUIRE_LEVELS$RECOMMENDED),
  GROUP_VAR_OBSERVER = structure("GROUP_VAR_OBSERVER", var_att_required =
                             VARATT_REQUIRE_LEVELS$RECOMMENDED),
  GROUP_VAR_DEVICE = structure("GROUP_VAR_DEVICE", var_att_required =
                             VARATT_REQUIRE_LEVELS$RECOMMENDED),
  KEY_OBSERVER = structure("KEY_OBSERVER", var_att_required =
                             VARATT_REQUIRE_LEVELS$COMPATIBILITY),
  KEY_DEVICE = structure("KEY_DEVICE", var_att_required =
                           VARATT_REQUIRE_LEVELS$COMPATIBILITY),
  TIME_VAR = structure("TIME_VAR", var_att_required =
                             VARATT_REQUIRE_LEVELS$RECOMMENDED),
  TIME_VAR_END = structure("TIME_VAR_END", var_att_required =
                         VARATT_REQUIRE_LEVELS$OPTIONAL),
  KEY_DATETIME = structure("KEY_DATETIME", var_att_required =
                             VARATT_REQUIRE_LEVELS$COMPATIBILITY),
  PART_VAR = structure("PART_VAR", var_att_required =
                         VARATT_REQUIRE_LEVELS$RECOMMENDED),
  STUDY_SEGMENT = structure("STUDY_SEGMENT", var_att_required =
                              VARATT_REQUIRE_LEVELS$RECOMMENDED),
  KEY_STUDY_SEGMENT = structure("KEY_STUDY_SEGMENT", var_att_required =
                                  VARATT_REQUIRE_LEVELS$COMPATIBILITY),
  VARIABLE_ROLE = structure("VARIABLE_ROLE", var_att_required =
                              VARATT_REQUIRE_LEVELS$OPTIONAL),
  VARIABLE_ORDER = structure("VARIABLE_ORDER", var_att_required =
                               VARATT_REQUIRE_LEVELS$OPTIONAL),
  LONG_LABEL = structure("LONG_LABEL", var_att_required =
                           VARATT_REQUIRE_LEVELS$RECOMMENDED),
  SOFT_LIMIT_LOW = structure("SOFT_LIMIT_LOW", var_att_required =
                               VARATT_REQUIRE_LEVELS$TECHNICAL),
  SOFT_LIMIT_UP = structure("SOFT_LIMIT_UP", var_att_required =
                              VARATT_REQUIRE_LEVELS$TECHNICAL),
  HARD_LIMIT_LOW = structure("HARD_LIMIT_LOW", var_att_required =
                               VARATT_REQUIRE_LEVELS$TECHNICAL),
  HARD_LIMIT_UP = structure("HARD_LIMIT_UP", var_att_required =
                              VARATT_REQUIRE_LEVELS$TECHNICAL),
  DETECTION_LIMIT_LOW = structure("DETECTION_LIMIT_LOW", var_att_required =
                                    VARATT_REQUIRE_LEVELS$TECHNICAL),
  DETECTION_LIMIT_UP = structure("DETECTION_LIMIT_UP", var_att_required =
                                   VARATT_REQUIRE_LEVELS$TECHNICAL),
  INCL_SOFT_LIMIT_LOW = structure("INCL_SOFT_LIMIT_LOW", var_att_required =
                                    VARATT_REQUIRE_LEVELS$TECHNICAL),
  INCL_SOFT_LIMIT_UP = structure("INCL_SOFT_LIMIT_UP", var_att_required =
                                   VARATT_REQUIRE_LEVELS$TECHNICAL),
  INCL_HARD_LIMIT_LOW = structure("INCL_HARD_LIMIT_LOW", var_att_required =
                                    VARATT_REQUIRE_LEVELS$TECHNICAL),
  INCL_HARD_LIMIT_UP = structure("INCL_HARD_LIMIT_UP", var_att_required =
                                   VARATT_REQUIRE_LEVELS$TECHNICAL),
  LOCATION_RANGE = structure("LOCATION_RANGE", var_att_required =
                               VARATT_REQUIRE_LEVELS$RECOMMENDED),
  LOCATION_METRIC = structure("LOCATION_METRIC", var_att_required =
                                VARATT_REQUIRE_LEVELS$RECOMMENDED),
  PROPORTION_RANGE = structure("PROPORTION_RANGE", var_att_required =
                                 VARATT_REQUIRE_LEVELS$RECOMMENDED),
  LOCATION_LIMIT_LOW = structure("LOCATION_LIMIT_LOW", var_att_required =
                                   VARATT_REQUIRE_LEVELS$TECHNICAL),
  LOCATION_LIMIT_UP = structure("LOCATION_LIMIT_UP", var_att_required =
                                  VARATT_REQUIRE_LEVELS$TECHNICAL),
  INCL_LOCATION_LIMIT_LOW = structure("INCL_LOCATION_LIMIT_LOW", var_att_required =
                                        VARATT_REQUIRE_LEVELS$TECHNICAL),
  INCL_LOCATION_LIMIT_UP = structure("INCL_LOCATION_LIMIT_UP", var_att_required =
                                       VARATT_REQUIRE_LEVELS$TECHNICAL),
  PROPORTION_LIMIT_LOW = structure("PROPORTION_LIMIT_LOW", var_att_required =
                                     VARATT_REQUIRE_LEVELS$TECHNICAL),
  PROPORTION_LIMIT_UP = structure("PROPORTION_LIMIT_UP", var_att_required =
                                    VARATT_REQUIRE_LEVELS$TECHNICAL),
  INCL_PROPORTION_LIMIT_LOW = structure("INCL_PROPORTION_LIMIT_LOW", var_att_required =
                                          VARATT_REQUIRE_LEVELS$TECHNICAL),
  INCL_PROPORTION_LIMIT_UP = structure("INCL_PROPORTION_LIMIT_UP", var_att_required =
                                         VARATT_REQUIRE_LEVELS$TECHNICAL),
  #  VARSHORTLABEL = structure("varshortlabel", var_att_required =
  #                                     VARATT_REQUIRE_LEVELS$OPTIONAL),
  RECODE_CASES = structure("RECODE_CASES", var_att_required =
                       VARATT_REQUIRE_LEVELS$OPTIONAL),
  RECODE_CONTROL = structure("RECODE_CONTROL", var_att_required =
                             VARATT_REQUIRE_LEVELS$OPTIONAL),
  GRADING_RULESET = structure("GRADING_RULESET", var_att_required =
                                VARATT_REQUIRE_LEVELS$OPTIONAL),
  STANDARDIZED_VOCABULARY_TABLE = structure("STANDARDIZED_VOCABULARY_TABLE",
                                            var_att_required =
                                              VARATT_REQUIRE_LEVELS$OPTIONAL),
  DATAFRAMES = structure("DATAFRAMES",
                         var_att_required =
                           VARATT_REQUIRE_LEVELS$OPTIONAL),
  ENCODING = structure("ENCODING",
                         var_att_required =
                           VARATT_REQUIRE_LEVELS$OPTIONAL),
  UNIVARIATE_OUTLIER_CHECKTYPE = structure("UNIVARIATE_OUTLIER_CHECKTYPE",
                                           var_att_required =
                                             VARATT_REQUIRE_LEVELS$RECOMMENDED),
  N_RULES = structure("N_RULES",
                       var_att_required =
                         VARATT_REQUIRE_LEVELS$RECOMMENDED),
  EXTENDED_DATA_TYPE = structure("EXTENDED_DATA_TYPE", var_att_required =
                                   VARATT_REQUIRE_LEVELS$OPTIONAL),
  COMPUTED_VARIABLE_ROLE = structure("COMPUTED_VARIABLE_ROLE",
                                     var_att_required =
                                       VARATT_REQUIRE_LEVELS$TECHNICAL
                                     )
)

.onAttach <- function(...) { # nocov start
  # if (packageVersion("ggplot2") >= as.package_version("4.0.0")) {
  #   packageStartupMessage(paste(
  #     "ggplot2 >= 4.0.0 uses S7 classes, which can cause performance issues.",
  #     "We are currently trying to address these, in doubt, try to downgrade",
  #     "ggplot2, if you consider trouble after updating ggplot2"))
  # }

  if (length(user_hints$l) > 0) {
    packageStartupMessage(paste(user_hints$l, collapse = "\n"))
  }
}
# nocov end

.onLoad <- function(...) { # nocov start

  to_set <-
    grep("^dataquieR.*_default$", ls(asNamespace("dataquieR")), value = TRUE)

  for (o in to_set) {
    nm <- sub("_default$", "", o)
    if (is.null(getOption(nm))) {
      do.call("options", setNames(list(get(o, asNamespace("dataquieR"))),
                                  nm = nm))
    }
  }

  ## cowplot integration (only if installed)
  if ((suppressWarnings(util_ensure_suggested("cowplot", err = FALSE)))) {
    util_s3_register("cowplot::as_grob", "dq_lazy_ggplot_s7", as_grob.dq_lazy_ggplot_s7)
    util_s3_register("cowplot::as_grob", "dq_lazy_ggplot",    as_grob.dq_lazy_ggplot)
  }

  ## plotly integration (only if installed)

  if ((suppressWarnings(util_ensure_suggested("plotly", err = FALSE)))) {
    util_s3_register("plotly::ggplotly",     "dq_lazy_ggplot", ggplotly.dq_lazy_ggplot)
    util_s3_register("plotly::plotly_build", "dq_lazy_ggplot", plotly_build.dq_lazy_ggplot)
    util_s3_register("plotly::ggplotly",     "dq_lazy_ggplot_s7", ggplotly.dq_lazy_ggplot_s7)
    util_s3_register("plotly::plotly_build", "dq_lazy_ggplot_s7", plotly_build.dq_lazy_ggplot_s7)
  }

  if (packageVersion("ggplot2") >= as.package_version("3.5.2")) {
    f <- get("is_ggplot",
             envir = environment(ggplot2::ggplot))
  } else {
    f <- get("is.ggplot",
             envir = environment(ggplot2::ggplot))

  }
  assign("util_is_gg_plot",
         f,
         environment(util_is_gg_plot))

  if (getRversion() >= "2.15.1") {
    utils::globalVariables(
      c(
        names(WELL_KNOWN_META_VARIABLE_NAMES),
        "....alt_call_res",
        "colcode",
        "colscale",
        "continuous",
        "level_names",
        "APP_SCORE",
        "FITTED_VALUE",
        "GRADING",
        "IMPLEMENTATION",
        "INTERVALS",
        "LCL",
        "LOWER_CL",
        "PROB",
        "TIME",
        "UCL",
        "UPPER_CL",
        "VARIABLES",
        "PCT_con_con",
        "Variables",
        "category",
        "lwl",
        "margins",
        "percent",
        "upl",
        "x",
        "y",
        "z2",
        "ds1",
        "meta_data",
        "variable",
        "value",
        "Rules",
        "progress",
        "progress_msg",
        "REQUIRED", # a var att requirement level, using NSE
        "label_col", # generated by prep_prepare_dataframes like ds1
        "tr", # htmltools withTags
        "td", # htmltools withTags
        "th", # htmltools withTags
        "table", # htmltools withTags
        "." # dplyr
      )
    )
  }

  if (requireNamespace("pkgload", quietly = TRUE)) {
    is_dev_package <- pkgload::is_dev_package
  } else {
    is_dev_package <- function(...) { return(FALSE) }
  }

  for (name in names(WELL_KNOWN_META_VARIABLE_NAMES)) {
    if (exists(name, asNamespace("dataquieR")) &&
        !is_dev_package("dataquieR")) {
      util_warning("Variable %s is in dataquieR too!", name,
                   applicability_problem = FALSE)
    }
    assign(name, WELL_KNOWN_META_VARIABLE_NAMES[[name]],
           asNamespace("dataquieR"))
  }
  if (!is_dev_package("dataquieR")) {
    namespaceExport(asNamespace("dataquieR"),
                    names(WELL_KNOWN_META_VARIABLE_NAMES))
  }

  if (file.exists(system.file("ssi.rds", package = "dataquieR"))) {
    local({
      .ssi <- readRDS(system.file("ssi.rds", package = "dataquieR"))
      COMPUTED_VARIABLE_ROLES <-
        as.list(setNames(nm = .ssi$SSI_METRICS))
      COMPUTED_VARIABLE_ROLES$`NA` <- "na"
      assign("COMPUTED_VARIABLE_ROLES", COMPUTED_VARIABLE_ROLES,
             asNamespace("dataquieR"))
      for (name in names(COMPUTED_VARIABLE_ROLES)) {
        if (exists(name, asNamespace("dataquieR")) &&
            !is_dev_package("dataquieR")) {
          util_warning("Variable %s is in dataquieR too!", name,
                       applicability_problem = FALSE)
        }
        assign(name, COMPUTED_VARIABLE_ROLES[[name]],
               asNamespace("dataquieR"))
      }
      if (!is_dev_package("dataquieR")) {
        namespaceExport(asNamespace("dataquieR"),
                        "COMPUTED_VARIABLE_ROLES")
        namespaceExport(asNamespace("dataquieR"),
                        setdiff(names(COMPUTED_VARIABLE_ROLES), "NA"))
      }
    })
  }

  if (is_dev_package("dataquieR") &&
      exists("util_load_helpers") &&
      is.function(get("util_load_helpers"))) {
    get("util_load_helpers")()
  }

  makeActiveBinding(".manual", function() {
    if (!length(ls(..manual))) {
      util_load_manual()
    }
    if (!length(..manual$titles)) {
      wrnPat <- paste("Did not find the reference manual for %s.",
                      "This can cause all-white summmaries")
      if (suppressWarnings(util_ensure_suggested("cli", err = FALSE))) {
        wrnPat <- cli::cli_alert_warning(
          cli::bg_black(cli::col_yellow(wrnPat)))
      }
      rlang::warn(sprintf(
        wrnPat,
        dQuote(rlang::ns_env_name())),
        .frequency = "regularly", .frequency_id = "dataquieRManual")
    }
    return(..manual)
  }, environment(util_load_manual))

  makeActiveBinding(".indicator_or_descriptor", function() {
    if (!length(ls(..indicator_or_descriptor))) {
      util_load_manual()
    }
    return(..indicator_or_descriptor)
  }, environment(util_load_manual))

  makeActiveBinding(".called_in_pipeline", function() {
    .dq2_globs$.called_in_pipeline
  }, environment(.onLoad))

  makeActiveBinding("MAX_LONG_LABEL_LEN", function() {
    min(.MAX_LABEL_LEN, getOption("dataquieR.MAX_LONG_LABEL_LEN",
                                  dataquieR.MAX_LONG_LABEL_LEN_default),
        na.rm = TRUE)
  }, env = environment(.onLoad))

  makeActiveBinding("MAX_LABEL_LEN", function() {
    min(.MAX_LABEL_LEN, getOption("dataquieR.MAX_LABEL_LEN",
                                  dataquieR.MAX_LABEL_LEN_default),
        na.rm = TRUE)
  }, env = environment(.onLoad))

  util_fix_rstudio_bugs()

  if (!l10n_info()[["UTF-8"]]) {
    update_recommended <-
      (.Platform$OS.type == "windows" &&
       (R.version$major < 4 || (R.version$major >= 4 && R.version$minor < 2)))
    util_user_hint(sprintf(
      paste("Your R session is not using a UTF-8 character set,",
      "be prepared to encoding problems. Also use %s. %s"),
      "Sys.setlocale() to select a UTF-8 locale in your .Rprofile",
      ifelse(update_recommended,
        paste(
          "\nOn Windows, you should consider updating R to a version >= 4.2.0,",
            "which supports UTF-8."),
          "")
    ))
  }

  unit_env <- new.env(parent = emptyenv())

  makeActiveBinding("UNITS", function() {
    if (!exists("valud", envir = unit_env)) {
      if (!requireNamespace("xml2", quietly = TRUE)) { # for units
        return(character(0))
      }
      valud <- suppressMessages(units::valid_udunits())
      assign("valud", valud, envir = unit_env)
    }
    valud <- get("valud", envir = unit_env)
    u <- valud[["symbol"]]
    u <- u[!util_empty(u)]
    u
  }, env = asNamespace(packageName()))

  makeActiveBinding("UNIT_IS_COUNT", function() {
    if (!exists("valud", envir = unit_env)) {
      if (!requireNamespace("xml2", quietly = TRUE)) { # for units
        return(character(0))
      }
      valud <- suppressMessages(units::valid_udunits())
      assign("valud", valud, envir = unit_env)
    }
    valud <- get("valud", envir = unit_env)
    valud <- valud[!util_empty(valud[["symbol"]]), , FALSE]
    u <- valud[is.na(valud$def) ==
             is.na(suppressWarnings(as.numeric(valud$def))), "symbol", TRUE]
    u <- u[!util_empty(u)]
    u <- setNames(nm = u)
    attr(u, "def") <-
      suppressWarnings(setNames(nm = valud$symbol, as.numeric(valud$def))[u])
    u
  }, env = asNamespace(packageName()))

  makeActiveBinding("UNIT_SOURCES", function() {
    if (!exists("valud", envir = unit_env)) {
      if (!requireNamespace("xml2", quietly = TRUE)) { # for units
        return(character(0))
      }
      valud <- suppressMessages(units::valid_udunits())
      assign("valud", valud, envir = unit_env)
    }
    valud <- get("valud", envir = unit_env)
    valud <- valud[!util_empty(valud[["symbol"]]), , FALSE]
    u <- setNames(nm = valud[["symbol"]], valud[["source_xml"]])
    u
  }, env = asNamespace(packageName()))

  makeActiveBinding("UNIT_PREFIXES", function() {
    if (!exists("p", envir = unit_env)) {
      if (!requireNamespace("xml2", quietly = TRUE)) { # for units
        return(character(0))
      }
      p <- suppressMessages(unique(
        unname(unlist(units::valid_udunits_prefixes()[, 1:3]))))
      p <- p[!util_empty(p)]
      assign("p", trimws(unlist(strsplit(p, ",", fixed = TRUE))), envir =
               unit_env)
    }
    get("p", envir = unit_env)
  }, env = asNamespace(packageName()))

  makeActiveBinding("UNIT_PREFIX_FACTORS", function() {
    if (!exists("pf", envir = unit_env)) {
      if (!requireNamespace("xml2", quietly = TRUE)) { # for units
        return(character(0))
      }
      ..pf <- suppressMessages(units::valid_udunits_prefixes())
      pf <- suppressMessages(unique(
        unname(unlist(..pf[, 1:3]))))
      pf <- pf[!util_empty(pf)]
      pf <- vapply(trimws(unlist(strsplit(pf, ",", fixed = TRUE))),
                          function(pfx) {
                            r <- ..pf[..pf[[1]] == pfx |
                                   ..pf[[2]] == pfx |
                                   ..pf[[3]] == pfx, "value", drop = TRUE]
                            if (length(r) == 0) {
                              r <- 0
                            }
                            r
                          },
                          FUN.VALUE = numeric(1))
      pf <- pf[pf != 0]
      dps <- duplicated(names(pf))
      if (any(dps)) {
        util_warning(
          c("Removing duplicated unit prefixes %s from udunits. Please",
            "verify, if your installation of the package units returns valid",
            "prefixes."),
          util_pretty_vector_string(sort(unique(names(pf)[dps])))
        )
        pf <- pf[!dps]
      }
      assign("pf",
             pf,
             envir =
               unit_env)
    }
    get("pf", envir = unit_env)
  }, env = asNamespace(packageName()))

  namespaceExport(asNamespace("dataquieR"),
                  c("UNITS", "UNIT_PREFIXES", "UNIT_SOURCES", "UNIT_IS_COUNT"))

  suppressMessages({
    # methods::setMethod('vapply', methods::signature(X="dataquieR_resultset2"),
    #                    function(X, FUN, FUN.VALUE, ..., USE.NAMES = TRUE) {
    #                      vapply(setNames(nm = names(X)), function(nm) {
    #                        FUN(X[[nm]], ...)
    #                      }, FUN.VALUE = FUN.VALUE, USE.NAMES = USE.NAMES)
    #                    })
    #
    # methods::setMethod('lapply', methods::signature(X="dataquieR_resultset2"),
    #                    function(X, FUN, ...) {
    #                      lapply(setNames(nm = names(X)), function(nm) {
    #                        FUN(X[[nm]], ...)
    #                      })
    #                    })
    #
    # methods::setMethod('sapply', methods::signature(X="dataquieR_resultset2"),
    #                    function(X, FUN, ..., simplify = TRUE, USE.NAMES = TRUE) {
    #                      sapply(setNames(nm = names(X)), function(nm) {
    #                        FUN(X[[nm]], ...)
    #                      }, simplify = simplify, USE.NAMES = USE.NAMES)
    #                    })

    # methods::setMethod('vapply', methods::signature(X="dataquieR_resultset2"),
    #                    function(X, FUN, FUN.VALUE, ..., USE.NAMES = TRUE) {
    #                      vapply(setNames(nm = names(X)), function(nm) {
    #                        FUN(X[[nm]], ...)
    #                      }, FUN.VALUE = FUN.VALUE, USE.NAMES = USE.NAMES)
    #                    }, where = globalenv())
    #
    # methods::setMethod('lapply', methods::signature(X="dataquieR_resultset2"),
    #                    function(X, FUN, ...) {
    #                      lapply(setNames(nm = names(X)), function(nm) {
    #                        FUN(X[[nm]], ...)
    #                      })
    #                    }, where = globalenv())
    #
    # methods::setMethod('sapply', methods::signature(X="dataquieR_resultset2"),
    #                    function(X, FUN, ..., simplify = TRUE, USE.NAMES = TRUE) {
    #                      sapply(setNames(nm = names(X)), function(nm) {
    #                        FUN(X[[nm]], ...)
    #                      }, simplify = simplify, USE.NAMES = USE.NAMES)
    #                    }, where = globalenv())


    if (!inherits(try(as.environment("tools:rstudio"),
                      silent = TRUE), "try-error") &&
        exists(".rs.getNames", as.environment("tools:rstudio"))) {
      # no need to load the full object just to fetch the names.
      .dataquieR_orig_util_rs_get_names <- # FIXME: don't, if already done
        get(".rs.getNames", as.environment("tools:rstudio"))
      if (!is.null(attr(.dataquieR_orig_util_rs_get_names,
                        ".dataquieR_orig_util_rs_get_names"))) {
        .dataquieR_orig_util_rs_get_names <-
          attr(.dataquieR_orig_util_rs_get_names,
               ".dataquieR_orig_util_rs_get_names")
      }
      util_rs_get_names <- function(object) {
        if (inherits(object, "dataquieR_resultset2")) {
          r <- names(object)
          if (!is.null(r))
            attr(r, "meta") <- rep("", length(r))
          return(r)
        } else {
          eval.parent(body(.dataquieR_orig_util_rs_get_names))
        }
      }
      environment(util_rs_get_names) <-
        environment(.dataquieR_orig_util_rs_get_names)
      attr(.dataquieR_orig_util_rs_get_names,
           ".dataquieR_orig_util_rs_get_names") <-
        .dataquieR_orig_util_rs_get_names
      attr(util_rs_get_names, ".dataquieR_orig_util_rs_get_names") <-
        .dataquieR_orig_util_rs_get_names
      assign(".dataquieR_orig_util_rs_get_names",
             .dataquieR_orig_util_rs_get_names,
             environment(util_rs_get_names))
      assign(".rs.getNames", util_rs_get_names, as.environment("tools:rstudio"))
    }
  })

  assign("MEM_COMPRESS_CAPABILITIES",
         names(which(.util_mem_compress_capabilities())),
         environment(.util_mem_compress_capabilities))

  if (requireNamespace("S7", quietly = TRUE)) {
    # register all S7 methods for the package (recommended by S7)
    S7::methods_register()

    # ensure your dq_lazy S7 class + operator methods exist in THIS session
    suppressMessages(dq_lazy_register_s7())
  }


}
# nocov end

MEM_COMPRESS_CAPABILITIES <- "none"

# name of the additional system missingness column in com_item_missingness
.SM_LAB <- "NAs (System missings)"

#' Data frame with labels for missing- and jump-codes
#' #' Metadata about value and missing codes
#'
#'

#' @name value/missing-lists
#' @aliases cause_label_df missing_matchtable CODE_VALUE CODE_LABEL CODE_CLASS CODE_INTERPRET value_label_table
#' @description
#' [data.frame] with the following columns:
#'   - `CODE_VALUE`: [numeric] | [DATETIME] Missing or categorical code
#'                                (the number or date representing a
#'                                missing/category)
#'   - `CODE_LABEL`: [character] a label for the missing code or category
#'   - `CODE_CLASS`: [enum] JUMP | MISSING. For missing lists: Class of the
#'                             missing code.
#'   - `CODE_INTERPRET` [enum] I | P | PL | R | BO | NC | O | UH | UO | NE.
#'                             For missing lists: Class of the missing code
#'                             according to
#'       [`AAPOR`](https://aapor.org/standards-and-ethics/standard-definitions/).
#'   - `resp_vars`: [character] For missing lists: optional, if a missing code
#'                              is specific for some
#'                              variables, it is listed for each such variable
#'                              with one entry in `resp_vars`, If `NA`, the
#'                              code is assumed shared among all variables.
#'                              For v1.0 metadata, you need to refer to
#'                              `VAR_NAMES` here.
#'
#' @seealso [Online](https://dataquality.qihs.uni-greifswald.de/VIN_Item_Level_Metadata.html#MISSING_LIST_TABLE)
#' @seealso [com_item_missingness()]
#' @seealso [com_segment_missingness()]
#' @seealso [com_qualified_item_missingness()]
#' @seealso [com_qualified_segment_missingness()]
#' @seealso [con_inadmissible_categorical()]
#' @seealso [con_inadmissible_vocabulary()]
#' @seealso [MISSING_LIST_TABLE]
#' @seealso [VALUE_LABEL_TABLE]
#' @seealso [STANDARDIZED_VOCABULARY_TABLE]
#' @seealso [cause_label_df]
NULL

#' Data frame with contradiction rules
#' @name check_table
#' @seealso [meta_data_cross]
#' @description
#' Two versions exist, the newer one is used by [con_contradictions_redcap] and
#' is described [here](https://dataquality.qihs.uni-greifswald.de/VIN_con_impl_contradictions_redcap.html#Example_output).,
#' the older one used by [con_contradictions] is described
#' [here](https://dataquality.qihs.uni-greifswald.de/VIN_con_impl_contradictions.html#Example_output).
NULL

#' Data frame with metadata about the study data on variable level
#'
#' @name meta_data
#' @description
#'
#' Variable level metadata.
#'
#' @seealso [further details on variable level metadata.](https://dataquality.qihs.uni-greifswald.de/VIN_Annotation_of_Metadata.html)
#' @seealso [meta_data_segment]
#' @seealso [meta_data_dataframe]
#'
NULL

#' Data frame with the study data whose quality is being assessed
#' @name study_data
#' @description
#' Study data is expected in wide format. If should contain all variables
#' for all segments in one large table, even, if some variables are not
#' measured for all observational utils (study participants).
NULL

#' Well known columns on the `meta_data_segment` sheet
#' @name meta_data_segment
#' @description
#' Metadata describing study segments, e.g., a full questionnaire, not its
#' items.
NULL

#' Default Name of the Table featuring Code Lists
#'
#' @export
CODE_LIST_TABLE = "CODE_LIST_TABLE"

# this has already been defined
# @inherit meta_data_segment
# @export
# STUDY_SEGMENT <- "STUDY_SEGMENT"

#' Segment level metadata attribute name
#'
#' Number of expected data records in each segment.
#' [numeric]. Check only conducted if number entered
#'
#' @seealso [meta_data_segment]
#'
#' @export
SEGMENT_RECORD_COUNT <- "SEGMENT_RECORD_COUNT"

#' Segment level metadata attribute name
#'
#' The name of the data frame containing the reference IDs to be compared
#' with the IDs in the targeted segment.
#'
#' @seealso [meta_data_segment]
#'
#' @export
SEGMENT_ID_REF_TABLE <- "SEGMENT_ID_REF_TABLE"

#' Deprecated segment level metadata attribute name
#'
#' The name of the data frame containing the reference IDs to be compared
#' with the IDs in the targeted segment.
#'
#' Please use [SEGMENT_ID_REF_TABLE]
#'
#' @export
SEGMENT_ID_TABLE <- SEGMENT_ID_REF_TABLE

#' Segment level metadata attribute name
#'
#' The type of check to be conducted when comparing the reference ID
#' table with the IDs in a segment.
#'
#' @seealso [meta_data_segment]
#'
#' @export
SEGMENT_RECORD_CHECK <- "SEGMENT_RECORD_CHECK"

#' Segment level metadata attribute name
#'
#' All variables that are to be used as one single ID
#' variable (combined key) in a segment.
#'
#' @seealso [meta_data_segment]
#'
#' @export
SEGMENT_ID_VARS <- "SEGMENT_ID_VARS"

#' Segment level metadata attribute name
#'
#' @seealso [DF_UNIQUE_ID]
#'
#' @seealso [meta_data_segment]
#'
#' @export
SEGMENT_UNIQUE_ID <- "SEGMENT_UNIQUE_ID"

#' Segment level metadata attribute name
#'
#' Specifies whether identical data is permitted across rows in a
#' segment (excluding ID variables)
#'
#' @seealso [meta_data_segment]
#'
#' @export
SEGMENT_UNIQUE_ROWS <- "SEGMENT_UNIQUE_ROWS"

#' Segment level metadata attribute name
#'
#' The name of the segment participation status variable
#'
#' @seealso [meta_data_segment]
#'
#' @export
SEGMENT_PART_VARS <- "SEGMENT_PART_VARS"

#' Segment level metadata attribute name
#'
#' `true` or `false` to suppress crude segment missingness output
#' (`Completeness/Misg. Segments` in the report). Defaults to compute
#' the output, if more than one segment is available in the item-level
#' metadata.
#'
#' @seealso [meta_data_segment]
#'
#' @export
SEGMENT_MISS <- "SEGMENT_MISS"

#' Well known columns on the `meta_data_dataframe` sheet
#' @name meta_data_dataframe
#' @description
#' Metadata describing data delivered on one data frame/table sheet,
#' e.g., a full questionnaire, not its items.
NULL

#' Data frame level metadata attribute name
#'
#' Name of the data frame
#'
#' @seealso [meta_data_dataframe]
#'
#' @export
DF_NAME <- "DF_NAME"

#' Data frame level metadata attribute name
#'
#' Name of the data frame
#'
#' @seealso [meta_data_dataframe]
#'
#' @export
DF_CODE <- "DF_CODE"

#' Data frame level metadata attribute name
#'
#' Number of expected data elements in a data frame.
#' [numeric]. Check only conducted if number entered
#'
#' @seealso [meta_data_dataframe]
#'
#' @export
DF_ELEMENT_COUNT <- "DF_ELEMENT_COUNT"

#' Data frame level metadata attribute name
#'
#' Number of expected data records in a data frame.
#' [numeric]. Check only conducted if number entered
#'
#' @seealso [meta_data_dataframe]
#'
#' @export
DF_RECORD_COUNT <- "DF_RECORD_COUNT"

#' Data frame level metadata attribute name
#'
#' The name of the data frame containing the reference IDs to be
#' compared with the IDs in the study data set.
#'
#' @seealso [meta_data_dataframe]
#'
#' @export
DF_ID_REF_TABLE <- "DF_ID_REF_TABLE"

#' Data frame level metadata attribute name
#'
#' The type of check to be conducted when comparing the reference
#' ID table with the IDs delivered in the study data files.
#'
#' @seealso [meta_data_dataframe]
#'
#' @export
DF_RECORD_CHECK <- "DF_RECORD_CHECK"

#' Data frame level metadata attribute name
#'
#' Defines expectancies on the uniqueness of the IDs
#' across the rows of a data frame, or the number of times some ID can be repeated.
#'
#' @seealso [meta_data_dataframe]
#'
#' @export
DF_UNIQUE_ID <- "DF_UNIQUE_ID"

#' Data frame level metadata attribute name
#'
#' All variables that are to be used as one single ID
#' variable (combined key) in a data frame.
#'
#' @seealso [meta_data_dataframe]
#'
#' @export
DF_ID_VARS <- "DF_ID_VARS"

#' Data frame level metadata attribute name
#'
#' Specifies whether identical data is permitted across rows in a
#' data frame (excluding ID variables)
#'
#' @seealso [meta_data_dataframe]
#'
#' @export
DF_UNIQUE_ROWS <- "DF_UNIQUE_ROWS"

#' Well known columns on the `item_computation_level` sheet
#' @name meta_data_computation
#' @family meta_data_cross
#' @description
#' Computation rules
#' TODO
#'
NULL

#' Well known columns on the `cross-item_level` sheet
#' @name meta_data_cross
#' @seealso [check_table]
#' @seealso [Online Documentation](https://dataquality.qihs.uni-greifswald.de/VIN_Cross_Item_Level_Metadata.html)
#' @family meta_data_cross
#' @description
#' Metadata describing groups of variables, e.g., for their multivariate
#' distribution or for defining contradiction rules.
#'
NULL

#' Valid unit symbols according to [units::valid_udunits()]
#'
#' like m, g, N, ...
#'
#' @name UNITS
#' @family UNITS
NULL

#' Is a unit a count according to [units::valid_udunits()]
#'
#' see column `def`, therein
#'
#' like `%`, `ppt`, `ppm`
#'
#' @name UNIT_IS_COUNT
#' @family UNITS
NULL

#' Maturity stage of a unit according to [units::valid_udunits()]
#'
#' see column `source_xml` therein, i.e., base, derived, accepted, or common
#'
#' @name UNIT_SOURCES
#' @family UNITS
NULL

#' Factors related to unit prefixes [units::valid_udunits_prefixes()]
#'
#' named `numeric` vector
#'
#' translates k, m, M, c, ... to 1000, 0.001, ...
#'
#' @name UNIT_PREFIX_FACTORS
#' @family UNITS
NULL

#' Valid unit prefixes according to [units::valid_udunits_prefixes()]
#'
#' like k, m, M, c, ...
#'
#' @name UNIT_PREFIXES
#' @family UNITS
NULL

#' Cross-item level metadata attribute name
#'
#' Specifies the unique labels for cross-item level metadata records
#'
#' if missing, `dataquieR` will create such labels
#'
#' @seealso [meta_data_cross]
#' @family meta_data_cross
#'
#' @export
CHECK_LABEL <- "CHECK_LABEL"

#' Cross-item level metadata attribute name
#' TODO
#' @seealso [meta_data_cross]
#' @family meta_data_cross
#' @export
SCALE_NAME <- "SCALE_NAME"

#' Cross-item level metadata attribute name
#' TODO
#' @seealso [meta_data_cross]
#' @family meta_data_cross
#' @export
SCALE_ACRONYM <- "SCALE_ACRONYM"

#' Cross-item level metadata attribute name
#' TODO
#' internal use, only
#' @seealso [meta_data_cross]
#' @family meta_data_cross
#' @export
VARIABLE_LIST_ORDER <- "VARIABLE_LIST_ORDER"

#' Cross-item level metadata attribute name
#'
#' Specifies the unique IDs for cross-item level metadata records
#'
#' if missing, `dataquieR` will create such IDs
#'
#' @seealso [meta_data_cross]
#' @family meta_data_cross
#'
#' @export
CHECK_ID <- "CHECK_ID"

#' Cross-item level metadata attribute name
#'
#' Specifies a group of variables for multivariate analyses. Separated
#' by `r SPLIT_CHAR`, please use variable names from [VAR_NAMES] or
#' a label as specified in `label_col`, usually [LABEL] or [LONG_LABEL].
#'
#' if missing, `dataquieR` will create such IDs from [CONTRADICTION_TERM],
#' if specified.
#'
#' @seealso [meta_data_cross]
#' @family meta_data_cross
#'
#' @export
VARIABLE_LIST <- "VARIABLE_LIST"

#' Cross-item level metadata attribute name
#'
#' Note: in some `prep_`-functions, this field is named `RULE`
#'
#' Specifies a contradiction rule. Use `REDCap` like syntax, see
#' [online vignette](https://dataquality.qihs.uni-greifswald.de/VIN_con_impl_contradictions_redcap.html)
#'
#' @seealso [meta_data_cross]
#' @family meta_data_cross
#' @aliases RULE
#'
#' @export
CONTRADICTION_TERM <- "CONTRADICTION_TERM"

#' Cross-item level metadata attribute name
#'
#' Specifies the type of a contradiction. According to the data quality
#' concept, there are logical and empirical contradictions, see
#' [online vignette](https://dataquality.qihs.uni-greifswald.de/VIN_con_impl_contradictions_redcap.html)
#'
#' @seealso [meta_data_cross]
#' @family meta_data_cross
#'
#' @export
CONTRADICTION_TYPE <- "CONTRADICTION_TYPE"

#' Cross-item level metadata attribute name
#'
#' Select, which outlier criteria to compute, see [acc_multivariate_outlier].
#'
#' You can leave the cell empty, then, all checks will apply. If you enter
#' a set of methods, the maximum for [N_RULES] changes. See also
#' [`UNIVARIATE_OUTLIER_CHECKTYPE`].
#'
#' @seealso [meta_data_cross]
#' @family meta_data_cross
#'
#' @export
MULTIVARIATE_OUTLIER_CHECKTYPE <- "MULTIVARIATE_OUTLIER_CHECKTYPE"

#' Cross-item level metadata attribute name
#'
#' @seealso [meta_data_computation]
#' @family meta_data_computation
#'
#' @export
COMPUTATION_RULE = "COMPUTATION_RULE"

#' Cross-item level metadata attribute name
#'
#' Select, whether to compute [acc_multivariate_outlier].
#'
#' You can leave the cell empty, then the depends on the setting of the
#' `option` [dataquieR.MULTIVARIATE_OUTLIER_CHECK]. If this column is missing,
#' all this is the same as having all cells empty and
#' `dataquieR.MULTIVARIATE_OUTLIER_CHECK` set to `"auto"`.
#'
#' See also [`MULTIVARIATE_OUTLIER_CHECKTYPE`].
#'
#' @seealso [meta_data_cross]
#' @family meta_data_cross
#'
#' @export
MULTIVARIATE_OUTLIER_CHECK <- "MULTIVARIATE_OUTLIER_CHECK"


#' Cross-item level metadata attribute name
#'
#' Select, whether to compute [acc_mahalanobis].
#'
#' You can leave the cell empty, then the depends on the setting of the
#' `option` [dataquieR.MULTIVARIATE_OUTLIER_CHECK]. If this column is missing,
#' all this is the same as having all cells empty and
#' `dataquieR.MULTIVARIATE_OUTLIER_CHECK` set to `"auto"`.
#'
#' See also [`MULTIVARIATE_OUTLIER_CHECKTYPE`].
#'
#' @seealso [meta_data_cross]
#' @family meta_data_cross
#'
#' @export
MAHALANOBIS_THRESHOLD <- "MAHALANOBIS_THRESHOLD"

#' Cross-item level metadata attribute name
#'
#' Specifies the allowable range of an association. The inclusion of the
#' endpoints follows standard mathematical notation using round brackets
#' for open intervals and square brackets for closed intervals.
#' Values must be separated by a semicolon.
#'
#' @family meta_data_cross
#' @seealso [meta_data_cross]
#'
#' @export
ASSOCIATION_RANGE <- "ASSOCIATION_RANGE"

#' Cross-item level metadata attribute name
#'
#' The metric underlying the association in [ASSOCIATION_RANGE]. The input is
#' a string that specifies the analysis algorithm to be used.
#'
#' @seealso [meta_data_cross]
#' @family meta_data_cross
#'
#' @export
ASSOCIATION_METRIC <- "ASSOCIATION_METRIC"

#' Cross-item level metadata attribute name
#'
#' The allowable direction of an association. The input is a string that can be
#' either "positive" or "negative".
#'
#' @seealso [meta_data_cross]
#' @family meta_data_cross
#'
#' @export
ASSOCIATION_DIRECTION <- "ASSOCIATION_DIRECTION"

#' Cross-item level metadata attribute name
#'
#' The allowable form of association. The string specifies the form based on a
#' selected list.
#'
#' @seealso [meta_data_cross]
#' @family meta_data_cross
#'
#' @export
ASSOCIATION_FORM <- "ASSOCIATION_FORM"

#' Cross-item level metadata attribute name
#'
#' Specifies the type of reliability or validity analysis. The string specifies
#' the analysis algorithm to be used, and can be either "inter-class" or
#' "intra-class".
#'
#' @seealso [meta_data_cross]
#' @family meta_data_cross
#'
#' @export
REL_VAL <- "REL_VAL"

#' Cross-item level metadata attribute name
#'
#' Defines the measurement variable to be used as a known gold standard. Only
#' one variable can be defined as the gold standard.
#'
#' @seealso [meta_data_cross]
#' @family meta_data_cross
#'
#' @export
GOLDSTANDARD <- "GOLDSTANDARD"

#' Cross-item level metadata attribute name
#'
#' For contradiction rules, the required pre-processing steps that can be given.
#' Note: `MISSING_LABEL`, `MISSING_INTERPRET` may not work for non-factor
#' variables
#'
#' LABEL LIMITS MISSING_NA MISSING_LABEL MISSING_INTERPRET
#'
#' @seealso [meta_data_cross]
#' @family meta_data_cross
#'
#' @export
DATA_PREPARATION <- "DATA_PREPARATION"

#' @export
CODE_VALUE <- "CODE_VALUE"

#' @export
RULE <- "RULE"

#' @export
CODE_LABEL <- "CODE_LABEL"

#' @export
CODE_INTERPRET <- "CODE_INTERPRET"

#' @export
CODE_CLASS <- "CODE_CLASS"

# TODO Only existence of CODE_ORDER is checked, order not yet used
#' Only existence is checked, order not yet used
#' @export
CODE_ORDER <- "CODE_ORDER"

#' types of value codes
#' @export
CODE_CLASSES <- list(MISSING = "MISSING",
                  JUMP = "JUMP",
                  VALUE = "VALUE")

#' Dimension Titles for Prefixes
#'
#' order does matter, because it defines the order in the `dq_report2`.
#'
#' @seealso `util_html_for_var()`
#' @seealso `util_html_for_dims()`
dims <- c(
  des = "Descriptive statistics",
  int = "Integrity",
  com = "Completeness",
  con = "Consistency",
  acc = "Accuracy"
)

# Use cli, if available.
rlang::local_use_cli()

#' Metadata sheet name containing VALUE_LABEL_TABLES
#' This metadata sheet can contain both value labels of several
#' VALUE_LABEL_TABLE and also Missing and JUMP tables
#' @export
CODE_LIST_TABLE <- "CODE_LIST_TABLE"

# for global options for dataquieR not exposed to the user like options()
dataquieR.properties <- new.env(parent = emptyenv())

.set_properties <- function(p) {
  list2env(p, dataquieR.properties)
}

with_pipeline <- withr::with_(new = FALSE,
  function(x) {
    res <- force(.dq2_globs$.called_in_pipeline)
    if (missing(x)) {
      .dq2_globs$.called_in_pipeline <- TRUE
    } else {
      .dq2_globs$.called_in_pipeline <- x
    }
    res
  })

without_pipeline <- withr::with_(new = FALSE,
  function(x) {
    res <- force(.dq2_globs$.called_in_pipeline)
    if (missing(x)) {
      .dq2_globs$.called_in_pipeline <- FALSE
    } else {
      .dq2_globs$.called_in_pipeline <- x
    }
    res
  })

.called_in_pipeline2 <- function() {
  n <- 1L
  repeat {
    cl <- rlang::caller_call(n)
    if (is.null(cl)) return(FALSE)  # End of call stack reached

    if (identical(rlang::call_name(cl), "dq_report2")) {
      # Try to resolve the actual function and its environment
      fn <- rlang::caller_fn(n)
      if (is.null(fn)) return(TRUE)  # If we can't resolve it, assume match
      # is good

      fn_env_top <- base::topenv(rlang::fn_env(fn))
      # Accept both Namespace (normal case) and Package environment
      # (e.g. devtools::load_all)
      is_from_pkg <- identical(fn_env_top, rlang::ns_env("dataquieR")) ||
        identical(rlang::env_name(fn_env_top), "package:dataquieR")
      return(is_from_pkg)
    }

    n <- n + 1L  # Check next caller up the stack
  }
}

dq_lazy_register_s7 <- function() {
  if (.dq_lazy_state$s7_ready) return(invisible(TRUE))
  if (!requireNamespace("S7", quietly = TRUE)) return(invisible(FALSE))

  # Create an S7 wrapper class around your existing S3 lazy object
  cls <- S7::new_class(
    "dq_lazy_ggplot_s7",
    properties = list(payload = S7::class_any)
  )
  .dq_lazy_state$s7_class <- cls

  # Define S7 operator methods for the wrapper.
  # NOTE: S7’s method-registration API is `S7::method()` in recent versions.
  # We guard it so you get a clear error if the API name differs.
  if (!exists("method", envir = asNamespace("S7"), inherits = FALSE)) {
    stop("S7 is installed but S7::method() was not found; adjust registration for your S7 version.")
  }

  base_ops <- get("base_ops", envir = asNamespace("S7"))

  S7::method(base_ops[["|"]], signature = list(cls, S7::class_any)) <- function(e1, e2) {
    p1 <- prep_realize_ggplot(dq_lazy_unwrap(e1))
    e2 <- dq_lazy_unwrap(e2)
    if (inherits(e2, "dq_lazy_ggplot")) e2 <- prep_realize_ggplot(e2)
    f <- get("|.ggplot", envir = asNamespace("patchwork"))
    f(p1, e2)
  }

  S7::method(base_ops[["/"]], signature = list(cls, S7::class_any)) <- function(e1, e2) {
    p1 <- prep_realize_ggplot(dq_lazy_unwrap(e1))
    e2 <- dq_lazy_unwrap(e2)
    if (inherits(e2, "dq_lazy_ggplot")) e2 <- prep_realize_ggplot(e2)
    f <- get("/.ggplot", envir = asNamespace("patchwork"))
    f(p1, e2)
  }

  S7::method(base_ops[["+"]], signature = list(cls, S7::class_any)) <- function(e1, e2) {
    p1 <- prep_realize_ggplot(dq_lazy_unwrap(e1))
    e2 <- dq_lazy_unwrap(e2)
    if (inherits(e2, "dq_lazy_ggplot")) e2 <- prep_realize_ggplot(e2)
    p1 + e2
  }

  S7::method(base_ops[["-"]], signature = list(cls, S7::class_any)) <- function(e1, e2) {
    p1 <- prep_realize_ggplot(dq_lazy_unwrap(e1))
    e2 <- dq_lazy_unwrap(e2)
    if (inherits(e2, "dq_lazy_ggplot")) e2 <- prep_realize_ggplot(e2)
    p1 - e2
  }

  S7::method(base_ops[["&"]], signature = list(cls, S7::class_any)) <- function(e1, e2) {
    p1 <- prep_realize_ggplot(dq_lazy_unwrap(e1))
    e2 <- dq_lazy_unwrap(e2)
    if (inherits(e2, "dq_lazy_ggplot")) e2 <- prep_realize_ggplot(e2)
    p1 & e2
  }

  S7::method(base_ops[["*"]], signature = list(cls, S7::class_any)) <- function(e1, e2) {
    p1 <- prep_realize_ggplot(dq_lazy_unwrap(e1))
    e2 <- dq_lazy_unwrap(e2)
    if (inherits(e2, "dq_lazy_ggplot")) e2 <- prep_realize_ggplot(e2)
    p1 * e2
  }

  .dq_lazy_state$s7_ready <- TRUE
  invisible(TRUE)
}
