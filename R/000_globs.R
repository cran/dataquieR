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
#'   https://dfg-qa.ship-med.uni-greifswald.de/DQconceptNew.html)
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
#'          FLOAT INTEGER STRING DATETIME variable
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
#' @export
DATA_TYPES_OF_R_TYPE <- list(
  logical = DATA_TYPES$INTEGER,
  integer = DATA_TYPES$INTEGER,
  factor = DATA_TYPES$STRING,
  ordered = DATA_TYPES$STRING,
  character = DATA_TYPES$STRING,
  numeric = DATA_TYPES$FLOAT,
  POSIXct = DATA_TYPES$DATETIME,
  POSIXlt = DATA_TYPES$DATETIME,
  POSIXt = DATA_TYPES$DATETIME,
  Date = DATA_TYPES$DATETIME,
  dates = DATA_TYPES$DATETIME,
  times = DATA_TYPES$DATETIME,
  chron = DATA_TYPES$DATETIME
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
#'
#' @examples
#' print(VAR_NAMES)
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
  KEY_OBSERVER = structure("KEY_OBSERVER", var_att_required =
                             VARATT_REQUIRE_LEVELS$RECOMMENDED),
  KEY_DEVICE = structure("KEY_DEVICE", var_att_required =
                           VARATT_REQUIRE_LEVELS$RECOMMENDED),
  KEY_DATETIME = structure("KEY_DATETIME", var_att_required =
                             VARATT_REQUIRE_LEVELS$RECOMMENDED),
  KEY_STUDY_SEGMENT = structure("KEY_STUDY_SEGMENT", var_att_required =
                                  VARATT_REQUIRE_LEVELS$RECOMMENDED),
  VARIABLE_ROLE = structure("VARIABLE_ROLE", var_att_required =
                              VARATT_REQUIRE_LEVELS$OPTIONAL),
  VARIABLE_ORDER = structure("VARIABLE_ORDER", var_att_required =
                               VARATT_REQUIRE_LEVELS$OPTIONAL),
  LONG_LABEL = structure("LONG_LABEL", var_att_required =
                           VARATT_REQUIRE_LEVELS$OPTIONAL),
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
  #  VARSHORTLABEL = structure("varshortlabel", var_att_required =
  #                                     VARATT_REQUIRE_LEVELS$OPTIONAL),
  RECODE = structure("recode", var_att_required =
                       VARATT_REQUIRE_LEVELS$OPTIONAL)
)

.onLoad <- function(...) { # nocov start
  if (getRversion() >= "2.15.1") {
    utils::globalVariables(c(names(WELL_KNOWN_META_VARIABLE_NAMES),
                             "ds1", "meta_data", "variable", "value", "Rules"))
  }
  for (name in names(WELL_KNOWN_META_VARIABLE_NAMES)) {
    if (exists(name, asNamespace("dataquieR"))) {
      util_warning(sprintf("Variable %s is in dataquieR too!", name))
    }
    assign(name, WELL_KNOWN_META_VARIABLE_NAMES[[name]],
           asNamespace("dataquieR"))
  }
  namespaceExport(asNamespace("dataquieR"),
                  names(WELL_KNOWN_META_VARIABLE_NAMES))
  util_fix_rstudio_bugs()
}
# nocov end
