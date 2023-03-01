#' All available probability distributions for
#' [acc_shape_or_scale]
#'
#'  - `uniform` For uniform distribution
#'  - `normal` For Gaussian distribution
#'  - `GAMMA` For a gamma distribution
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
#'   https://dataquality.ship-med.uni-greifswald.de/DQconceptNew.html)
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
#'              the meta data attribute `VAR_NAMES` or, if `label_col` is set,
#'              given in the meta data attribute given in that argument.
#'              Labels can easily be translated using [prep_map_labels]
#' - `variable list` Function arguments of this type expect a character vector
#'                   that specifies variables using the variable identifiers
#'                   given in the meta data attribute `VAR_NAMES` or,
#'                   if `label_col` is set, given in the meta data attribute
#'                   given in that argument. Labels can easily be translated
#'                   using [prep_map_labels]
#'
#' @seealso [integer] [string]
#' @aliases integer string float datetime numeric enum
#'          FLOAT INTEGER STRING DATETIME variable set
#' @rawRd \alias{variable list}
#'
#' @export
DATA_TYPES <- list(
  INTEGER = "integer",
  STRING = "string",
  FLOAT = "float",
  DATETIME = "datetime"
)

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
  times = DATA_TYPES$DATETIME,
  chron = DATA_TYPES$DATETIME,
  double = DATA_TYPES$FLOAT,
  numeric = DATA_TYPES$FLOAT,
  character = DATA_TYPES$STRING
)

#' Variable roles can be one of the following:
#'
#'   - `intro` a variable holding consent-data
#'   - `primary` a primary outcome variable
#'   - `secondary` a secondary outcome variable
#'   - `process` a variable describing the measurement process
#'
#' @rawRd \alias{variable roles}
#' @export
VARIABLE_ROLES <- list(
  INTRO = "intro",
  PRIMARY = "primary",
  SECONDARY = "secondary",
  PROCESS = "process"
)

#' Character used  by default as a separator in meta data such as
#'           missing codes
#'
#' This 1 character is according to our metadata concept `r dQuote(SPLIT_CHAR)`.
#' @export
SPLIT_CHAR <- "|"

# constants for the names of defined meta data attributes / meta reference
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
#' names of the variable attributes in the meta data frame holding
#' the names of the respective observers, devices, lower limits for plausible
#' values, upper limits for plausible values, lower limits for allowed values,
#' upper limits for allowed values, the variable name (column name, e.g.
#' v0020349) used in the study data,  the variable name used for processing
#' (readable name, e.g. RR_DIAST_1) and in parameters of the QA-Functions, the
#' variable label, variable long label, variable short label, variable data
#' type (see also [DATA_TYPES]), re-code for definition of lists of event
#' categories, missing lists and jump lists as CSV strings.
#'
#' all entries of this list will be mapped to the package's exported NAMESPACE
#' environment directly, i.e. they are available directly by their names too:
#'
#' `r paste0(' - [', names(WELL_KNOWN_META_VARIABLE_NAMES), ']')`
#'
#' @rawRd \alias{variable attribute}
#'
#' @eval c("@aliases", paste(names(WELL_KNOWN_META_VARIABLE_NAMES)))
#'
#' @seealso [meta_data_segment] for `STUDY_SEGMENT`
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
  VALUE_LABELS = structure("VALUE_LABELS", var_att_required =
                             VARATT_REQUIRE_LEVELS$RECOMMENDED),
  MISSING_LIST = structure("MISSING_LIST", var_att_required =
                             VARATT_REQUIRE_LEVELS$REQUIRED),
  JUMP_LIST = structure("JUMP_LIST", var_att_required =
                          VARATT_REQUIRE_LEVELS$RECOMMENDED),
  # TODO: Support MISSING_LIST_TABLE in addition to MISSING_LIST/JUMP_LIST for item missingness / NA replacements
  MISSING_LIST_TABLE = structure("MISSING_LIST_TABLE", var_att_required =
                                   VARATT_REQUIRE_LEVELS$RECOMMENDED),
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
  RECODE = structure("recode", var_att_required =
                       VARATT_REQUIRE_LEVELS$OPTIONAL)

)

.onLoad <- function(...) { # nocov start
  if (getRversion() >= "2.15.1") {
    utils::globalVariables(
      c(
        names(WELL_KNOWN_META_VARIABLE_NAMES),
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
        "label_col" # generated by prep_prepare_dataframes like ds1
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

  makeActiveBinding(".manual", function() {
    if (!length(ls(..manual))) {
      util_load_manual()
    }
    return(..manual)
  }, environment(util_load_manual))

  util_fix_rstudio_bugs()
}
# nocov end

# name of the additional system missingness column in com_item_missingness
.SM_LAB <- "ADDED: SysMiss"

# TODO: Update the following block to also comprise the new columns for qualified missingness and add a link to the documentation of ? cause_label_df (CAVE: In the new version, we would not need it, but we should (do already?) support linking other names than VAR_NAMES, e.g., LABEL).
#' Data frame with labels for missing- and jump-codes
#' @name cause_label_df
#' @aliases missing_matchtable
#' @description
#' [data.frame] with the following columns:
#'   - `CODE_VALUE`: [numeric] Missing code (the number)
#'   - `CODE_LABEL`: [character] a label for the missing code
#'   - `CODE_CLASS`: [enum] JUMP | MISSING. Class of the missing code.
#'   - `resp_vars`: [character] optional, if a missing code is specific for some
#'                              variables, it is listed for each such variable
#'                              with one entry in `resp_vars`, If `NA`, the
#'                              code is assumed shared among all variables.
#'                              For v1.0 metadata, you need to refer to
#'                              `VAR_NAMES` here.
NULL

#' Data frame with contradiction rules
#' @name check_table
#' @aliases meta_data_cross_item
#' @description
#' Two versions exist, the newer one is used by [con_contradictions_redcap] and
#' is described [here](https://dataquality.ship-med.uni-greifswald.de/VIN_con_impl_contradictions_redcap.html#Example_output).,
#' the older one used by [con_contradictions_redcap] is described
#' [here](https://dataquality.ship-med.uni-greifswald.de/VIN_con_impl_contradictions.html#Example_output).
NULL

#' Data frame with metadata about the study data on variable level
#'
#' @name meta_data
#' @description
#'
#' Variable level metadata.
#'
#' @seealso [further details on variable level metadata.](https://dataquality.ship-med.uni-greifswald.de/Annotation_of_Metadata.html)
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
#' Meta data describing study segments, e.g., a full questionnaire, not its
#' items.
NULL

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
SEGMENT_ID_TABLE <- "SEGMENT_ID_TABLE"

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


#' Well known columns on the `meta_data_dataframe` sheet
#' @name meta_data_dataframe
#' @description
#' Meta data describing data delivered on one data frame/table sheet,
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

# Use cli, if available.
rlang::local_use_cli()
