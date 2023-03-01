# dataquieR 2.0.0
  * `dq_report2` replaces `dq_report`. Please use `dq_report2` from now on.
  * Full new reporting engine (needs `htmtools` and supports `plotly`)
  * Better report layout and improved functionality 
  * Support for reading and referring to data in files/URLs
  * Support for the integrity dimension in data quality report
  * Included distribution and multivariate outlier (provide cross-item level metadata for the latter) plots in data quality report
  * Metadata scheme update (segment, `data.frame`, and cross-item levels). No required action by user, previous version still supported
  * `REDCap` rules for contradictions (cross-item level metadata), previous contradictions function still supported
  * Support metadata describing segment data and study data tables (segment and `data.frame`-level metadata)
  * New item-level metadata version (backwards compatible)
  * Support for computation of qualified missingness based on labels from the `AAPOR` concept
  * `acc_univariate_outlier` and `acc_multivariate_outlier` now allow selecting the methods used to flag `outliers`
  * Included distributional checks in the accuracy dimension for location and proportion
  * Rotation of plots can now be controlled
  * Improved many figures
  * Better control over warnings
  * If `whoami` is installed, reports now show a more suitable user name
  * Many minor improvements
  * Updated citations

# dataquieR 1.0.13
  * fixed a left-over `~` from the `ggplot2` updates causing `acc_margins` to
    fail for categorical variables

# dataquieR 1.0.12

# dataquieR 1.0.12
  * Addressed a problem with the markdown template underlying the
  `dq_report` reports with wrong brackets
  * Addressed deprecations from `ggplot2 3.4.0`
  * Added `ORCIDs` for two authors
  * Updated the `CITATION` file
  * Updated the `README.md` file adding the funding sources.

# dataquieR 1.0.11
  * Addressed a problem with some test platforms
  * Added funding agencies in the manual
  
# dataquieR 1.0.10
  * Fixed `NEWS.md` file
  * Fixed documentation

# dataquieR 1.0.9
  * Fixed bug in `sigmagap` and made missing guessing more robust.
  * Fixed checks on missing code detection failing for `logical`.
  * Fixed a damaged check for numeric threshold values in `acc_margins`.
  * Fixed wrongly named `GRADING` columns.
  * Improved parallel execution by automatic detection of cores.
  * Tidy html dependency

# dataquieR 1.0.8
  * Removed formal arguments from `rbind.ReportSummaryTable` since these are
    not needed anyways and the inherited documentation for those arguments
    `rbind` from `base` contains an invalid URL triggering a `NOTE`.

# dataquieR 1.0.7
  * ***Fixed bugs in example metadata.***
  * Figures now have size hints as attributes.
  * Added simple type conversion check indicator function of dimension 
    integrity, `int_datatype_matrix`.
  * Corrected some error classifications
  * `prep_study2meta` can now also convert factors to `dataquieR` compatible
    `meta_data`/`study_data`
  * Slightly improved documentation.
  * Bug fix in `com_item_missingness` for textual response variables.
  * Added new output slot with heat-map like tables. Implemented some generics
    for those.

# dataquieR 1.0.6
  * Robustness: Ensure `DT JS` is always loaded when a dq_report report is 
    rendered
  * Bug fix: More robust handling of DECIMALS variable attribute, if
    this is delivered as a character.
  * Bug Fix: `com_segment_missingness` with 
      `strata_vars` / `group_vars` did not work
  * Bug Fix: If `label_col` was set to something else than `LABEL`, 
      `strata_vars` did not work for `com_unit_missingness`
  * More precise documentation.
  * Fixed a bug in a utility function for the univariate outliers indicator 
    function, which caused many data points flagged as outliers by the sigma-
    gap criterion.
  * Made outlier function aware of too many non-outlier points causing too
    complex graphics (e.g. pdf rendering crashes the PDF reader).
  * Fixes and small improvements in `dq_report`.
  * Switched from `cowplot` to `patchwork` in `acc_margins` yielding figures 
    that can be easier manipulated. Please note, that this change could break
    existing output manipulations, since the structure of the margins plots
    has changed internally. However, output manipulations were hardly
    possible for margins plots before, so it is unlikely, that there
    are pipelines affected.
  * More control about the output of the `acc_loess` function.
  * More robust `prep_create_meta` handling length-0 arguments by ignoring
    these variable attributes at all.
  * Added a classification system for warnings and error messages to
    distinguish errors based on mismatching variables for a function from
    other error messages.
  * https://github.com/openjournals/joss-reviews/issues/3093#issuecomment-840695360
  * Some tidy up and more tests.
  
# dataquieR 1.0.5
  * Fixed two bugs in `con_inadmissible_categorical` (one `resp_var` only and
    value-limits all the same for all `resp_vars`)
  * Changed LICENSE to BSD-2
  * Slightly updated documentation
  * Updated `README`-File

# dataquieR 1.0.4
  * Fixed CITATION, a broken reference in Rd and a problem with the vignette
    on `pandoc`-less systems
  * Improved an inaccurate argument description for multivariate outliers
  * Fixed a problem with error messages, if a `dataquieR` function was called 
    by a generated function `f` that lives in an environment 
    directly inheriting from the empty environment, e.g. 
    `environment(f) <- new.env(parent = emptyenv())`.
  * Marked some examples as `dontrun`, because they sometimes caused `NOTE`s
    on `rhub`.

# dataquieR 1.0.3
  * Addressed all comments by the CRAN reviewers, thank you.

# dataquieR 1.0.2
  * Bug Fix: If an empty data frame was delivered in the `SummaryTable` entry 
    of a result within a `dq_report` output, the `summary` and also 
    `print` generic did not work on the report.
  
# dataquieR 1.0.1
  * Skipping some of the slower tests on CRAN now. On my local system,
   a full `devtools::check(cran = TRUE, env_vars = c(NOT_CRAN = "false"))`
   takes 2:22 minutes now.
  
# dataquieR 1.0.0
  * Initial CRAN release candidate
