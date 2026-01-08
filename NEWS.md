# dataquieR 2.8.7

* Fixed a bug in generated `JavaScript` containing variable labels with
  quotes
* Fixed a bug in rendering, if some of the variables from the study data
  that are not covered in the metadata

# dataquieR 2.8.6

* Fixed race-condition with `JavaScript` and `css` in output
* Fix in build-pipeline not affecting end-users

# dataquieR 2.8.5

* Improved `css` in output
* Smaller package

# dataquieR 2.8.4

* Added regression test based on the bug fixed in version `2.8.3`.
* Fixed a bug preventing some internal links from working.

# dataquieR 2.8.3

* Fixed a bug causing reports to be incomplete.

# dataquieR 2.8.2

* disabled slow tests

# dataquieR 2.8.1

* small fixes

# dataquieR 2.8.0

* Improved memory performance
* Improved parallel rendering
* Reduced internal documentation size
* Documentation typos fixed; documentation improved and streamlined
* Several test improvements; more robust handling of suggested packages
* Removed bundled `jsPDF` HTML dependency; `visNetwork` is now suggested instead  
  (saves ~356 KB installed size)
* Various bug fixes related to new `S7` rendering
* Improved handling of `options()`
* `dq_report_by()` no longer errors if `plotly` is missing but disables
  `plotly` functionality gracefully
* Improved metadata page rendering

## Major internal change (may imply breaking changes)

* `ggplot2` objects are now returned as lightweight promises (`dq_lazy_ggplot`)
  to avoid serializing large `S7` objects  
  * If required, materialize explicitly via `prep_realize_ggplot(p)`
  * This reduces RAM demands but may require adaptation in downstream plot
    processing
  * If necessary, memory usage caused by `S7` compatibility can be reduced via  
    `options(dataquieR.lazy_plots_gg_compatibility = "FALSE")`, which may
    require more frequent calls to `prep_realize_ggplot()`

* If you use saved report objects created with older
  `ggplot2` / `patchwork` versions, they may stop working. Either
  recompute them or temporarily downgrade:
```r
remotes::install_version("patchwork", version = "1.3.0")
remotes::install_version("ggplot2", version = "3.5.2")
```
  This workaround is only intended for restoring compatibility with
  previously saved report objects. No guarantee is given and you use it
  at your own risk. For long-term use, we recommend recomputing the
  reports with current package versions.

## Other noteworthy changes

### Indicators, analytics & semantics
* Added complex limits for `cross-item_level` metadata  
  (`HARD_LIMITS`, `SOFT_LIMITS`, `DETECTION_LIMITS`).  
  If both complex and item-level limits exist, complex limits take precedence
* Work in progress: Added careless responding measures (new **Scales** menu), 
  including:  
  maximum long string, missing responses per participant,  
  intra-individual response variability, response times, completion speed,  
  Mahalanobis distance
* Removed indicator `PCT_acc_ud_loc` from `acc_margins()` and grading rulesets
* Removed metric `FLG_acc_ud_loc` from `acc_margins()`

### Rendering, tables & plots
* Improved `des_summary()`:
  * restructured and more robust
  * column `"No. categories/Freq. table"` split into  
    `"No. categories (incl. NAs)"` and `"Level_freq"`
  * column `"Variables"` renamed `"Variable_names"`  
    (previous content now in attribute `plain_label`)
* Factors are no longer converted to integers; they are converted to `character`
  first (controlled via option `dataquieR.old_factor_handling`)
* Correlation plots no longer depend on `GGally`
* Proper and consistent date/time parsing
* Better control over visibility of unused heatmap levels
* Improved thumbnail handling and table variable columns

### Performance, robustness & infrastructure
* Parallel rendering now more stable
* Reduced RAM demands
* Improved JavaScript utilities for handling results
* More robust rendering in general
* Improved error detection for inadmissible values in group variables with
  `VALUE_LABELS`
* Experimental `prep_init_parallel_print()` removed (no longer needed)


# dataquieR 2.5.1

* News
  * fixed a bug found by latest R-developer-version caused by parentheses in 
    the wrong position in encapsulated function calls. this did not cause any 
    harm, but was nevertheless a bug.
  * properly deprecated the argument `threshold_value` from `acc_varcomp()`
  * `loess` and margins plot slightly improved

* Amendment to 2.5.0 news
  * deprecated (and accidentally removed already) the argument `threshold_value`
    from `acc_varcomp()`

# dataquieR 2.5.0

* New features 
  * improved support for categorical variables, including: 
     * time trends w/ and w/o grouping variable
     * observer/device effects
     * distribution plots
  * `dq_report2()` can store results on the disk instead of the RAM with the
    new argument `storr_factory`. This can be useful in reducing issues of 
    memory consumption, but we suggest to use fast `SSD`s or `NVMe`s
  * all indicator functions now create result objects with nice print 
    functions (visible in the Data Viewer instead of the Console window). 
    However, this also implies, that warnings, errors and messages
    are returned as part of the result object and are printed with that object.
    If you want to restore the original behavior, use the option 
    `options(dataquieR.dontwrapresults = TRUE)`.
     With `options(dataquieR.testdebug = TRUE)`, you can switch off this 
     behavior.
  * `dataquieR` can provision your function arguments from the metadata.
    In order to enable `lapply` and `Vectorize(SIMPLIFY = FALSE)` with 
    indicator functions, the first argument is now always
    `resp_vars` for item level functions.
    `dataquieR` tries to guess if a function that features both `resp_vars` and
    `study_data` as its first arguments was called w/o `resp_vars` but only with
    `study_data` as its first unnamed argument. If that is the case, it sets 
    `resp_vars` to the default for `resp_vars` (typically all variables). 
    With `options(dataquieR.testdebug = TRUE)`, you can switch off this
    behavior, if you need. 
  * an improved version of `dq_report_by`, in which it is possible to specify:
     * how to split the data in parts (strata and/or variable groups)
     * strata and/or variable groups to include/exclude
     * how to filter observational units
     * a selection of variables to analyze (`resp_vars`)
     * a variable/s containing ID information for merging data frames (`id_vars`)
  * a new function `int_encoding_errors` checking invalid characters present in
    the text with respect to the expected character encoding / code page, 
    e.g., a code place in the `latin1` table is used but the encoding 
    is `utf8` resulting in damaged text output
  * a new dashboard in the General menu, in `Item-level data quality dashboard`, 
    usable to customize data summaries
  * new selection buttons are now present in the report to select visible
    columns in the displayed tables (it also applied to the export buttons) 
  * support for a sheet `CODE_LIST_TABLE` in the metadata, 
    where it is possible to state both value label tables and 
    missing list tables all in one table. 
  * support for a sheet `item_computation_level` in the metadata,
    where it is possible to state variables to be computed from the provided 
    study data.
    
* Breaking changes 
  * moved example data from the package to our website. If you are
    already using `prep_get_data_frame("ship")` or 
    `prep_get_data_frame("study_data")` in your code to access example data,
    no change is needed. If you are still accessing example data using
    `system.file()` (e.g. using
    `load(system.file("extdata", "study_data.RData", package = "dataquieR"))`), 
    you need to switch to `prep_get_data_frame()`, i.e.:
      `load(system.file("extdata", "study_data.RData", package = "dataquieR"))`
    would become `study_data <- prep_get_data_frame("study_data")`
  * changes in the output names:
     * renamed `SummaryData` in `ResultData` (functions: `acc_shape_or_scale`, 
       `acc_margins`, `com_segment_missingness`) 
     * removed column `GRADING` from `SummaryData` outputs. 
       `SummaryTable` outputs still feature the column, since these are meant 
       to be a machine readable interface
     * `con_contradictions_redcap` used to return a result named `SummaryTable`,
       while the documentation spoke about `SummaryData`. Alas, it should have 
       been `VariableGroupTable` in both cases. 
       If you relied on `SummaryTable` in the results of 
       `con_contradictions_redcap`, you need to change your code 
       to use now the correct output name `VariableGroupTable`. Also, the table
       has been slightly modified. 
     * `VariableGroupData` as returned by `con_contradictions_redcap` is a 
       version optimized for human readers. 
     * in `VariableGroupTable` as returned by `con_contradictions_redcap` 
       the column `category` has been renamed to `CONTRADICTION_TYPE`
     * in `con_contradictions_redcap`, if `summarize_categories` is selected 
       the result will now be in a sub-list named `Other`
     * in `prep_add_computed_variables`, the column `resp_vars` is now named 
       `VAR_NAMES`, to be more in line with other data frames.
  
* Reporting 
  * improved button to export Excel, pdf, and print (colors supported)
  * improved rendering time introducing thumbnails as first visible result in 
    the report. Clicking on the image, the thumbnail is replaced by `plotly`'s 
    interactive figures
  * implementation of `[.dataquieR_resultset2` and `[[.dataquieR_result` and
    related functions have changed slightly. You can now for a
    report (`r <- dq_report2(...)`) call, e.g.,   
    `r[, "com_item_missingness", "ReportSummaryTable"]` to get a balloon plot or
    `r[, "com_item_missingness", "SummaryData"]` to get a table, for all 
    variables that were assessed with `com_item_missingness()` in the report `r`
  * if you print a list of `dataquieR_result` objects, these will be combined,
    but due to restrictions in `R`, this only works, if you call `print()`
    explicitly on this list, not with "auto-printing" (see 
    https://stackoverflow.com/a/53983005), for example:  
    `a <- lapply(c("v00001", "v00004", "v00005", "v00006"), acc_loess, meta_data_v2 = "meta_data_v2", study_data = "study_data")`
    `print(a)` works, but typing `a` alone does not. 
    You have to call `print()` or to put `lapply()` in brackets:
    `(lapply())`

* (Indicator) Functions related 
  * `acc_distributions()` was split in `acc_distributions()` and
    `acc_distributions_ecdf()` 
    (`prep_acc_distributions_with_ecdf()` creates the original plot)
  * there is a new function `acc_cat_distributions()`
  * all functions now feature:
     * a `meta_data_v2` argument
     * new argument `item_level`, as synonyms for `meta_data`,
       new argument `segment_level`, as synonyms for `meta_data_segment`,
       new argument `dataframe_level`, as synonyms for `meta_data_dataframe`,
       new argument `cross-item_level`, as synonyms for `meta_data_cross_item`,
       new argument `item_computation_level`, as synonyms for
       `meta_data_item_computation`
  * if you call functions without `label_col`, the `label_col` will now
    default to `LABEL`, except you set the option 
    `options(dataquieR.testdebug = TRUE)` or 
    `options(dataquieR.dontwrapresults = TRUE)`
  * the argument `resp_vars` in `prep_scalelevel_from_data_and_metadata()` was
    never working correctly and not used neither, so it has been deprecated. 
    It is already not functional and it never was
  * the function `des_summary` is still present, but you can now get results for
    continuous or categorical variables only, using 
    `des_summary_continuous` and `des_summary_categorical`respectively
  * `con_contradictions_redcap` plot colors vary depending
    on `CONTRADICTION_TYPES` 
  * `acc_loess()` uses `lowess` instead of `loess` (both from the `stats` 
    package)   

* General
  * test coverage increased, again
  * fixed bug in `prep_check_for_dataquieR_updates()`, so, maybe, you need to
    manually install the latest beta release using 
    `devtools::install_gitlab("libreumg/dataquieR", auth_token = NULL)`
  * figure sizes have been overworked in the default report
  * `options(dataquieR.ELEMENT_MISSMATCH_CHECKTYPE = "subset_u")` is now the
    default assuming a one-fits-all-metadata-file (see 
    `? dataquieR.ELEMENT_MISSMATCH_CHECKTYPE`)
  * fewer custom implementations of stuff available from `rlang` or `withr`,
    most prominently a faster `prep_prepare_dataframes()` and `rlang` compatible
    condition (error) handling.
  * small changes in the behavior of the `dataquieR_result` class, which is
    now applied also to results outside a pipeline.
  * many small fixes to figures
  * small fixes to menu titles
  * bug fixes

# dataquieR 2.1.0
  * renamed metadata column `SEGMENT_ID_TABLE` to `SEGMENT_ID_REF_TABLE` in
    segment level metadata
  * scale level metadata support and heuristics
  * significantly improved data quality summaries
  * consolidated some of the indicator functions (limits, work in progress)
  * many minor optimizing changes
  * figure sizing (work in progress), also resize handles
  * improved report files structure
  * improved `dq_report_by` files structure
  * fixes, e.g., in rules, fixes in label shortening, computation speed and 
    cache pre-filling-control
  * improved Excel export from the `HTML` reports
  * missing codes: column `CODE_INTERPRET` changed to be in line with the 
    `AAPOR` definitions, so the following translation: 
    `PP -> P; P -> I; OH -> UO`
  * fixed tests
  * updated concept excerpt
  * excluded nominal and ordinal variables from marginal means analysis
  * improved data type handling

# dataquieR 2.0.1

## Reporting 
  * New functions `prep_save_report` and `prep_load_report`
  * Update and simplification of summary overview, empty columns/rows omitted from the matrices. Also, better classification of errors
  * Many small updates in the usability of the report
  * Fixes in `HTML/JS` output for `Firefox`
  * Bug fixes of report outputs that were not looking as expected (in contradiction checks and limit violations)
  * Fixed mixed distribution plots called several times
  * Enable auto-resizing of `plot.ly`-plots
  * Fixed rendering problems for the new, automatically size-reduced plots causing the report rendering to fail if having `gginnards` installed; removed dependency from `gginnards`.
  * Do not show superfluous axis labels (e.g., variables, if variable names are on an axis because these usually overlap without improving the output)
  * Prevent a warning of `robustbase` about `doScale`
  * Less noisy display of conditions (e.g., warnings, errors, messages) with the results in `dq_report2` reports
  * `summarytools` are included in `dq_report2` reports, if installed.
  * New report rendering code polished, parallel execution of `HTML` generation prepared
  * New parallel mode for `dq_report2` using a queue improves speed
  * Full support for `VARIABLE_ROLES` in `dq_report2` and suppressing helper variable outputs in `dq_report_by`
  * Do not show conditions (e.g., warnings, messages, errors) in reports if they address the call of the function (e.g., "using default for argument...") by `dq_report2` and not directly by the user
  * No unit-missingness in `dq_report2` because it is not so useful in its current implementation
  * More robust `dq_report_by` for large reports (can write and optionally render results to disk rather than returning them)
  * Bug fix in `dq_report_by` causing `DATA_PROCESS` not to work
  * Fixed some errors and `TODO`'s in `dq_report_by` and add dependent variables on the fly but with `VARIABLE_ROLE` suppress:
     * If no role is given, add "primary" by default for single reports as well as for `dq_report_by`
     * Support meta_data_v2 in `dq_report_by`
     * FIXED: referred variables did not correctly resolve co_vars and labels instead of variable names
  * Several bug fixes:
     * Addressed most parts of https://gitlab.com/libreumg/dataquier/-/issues/242    
     * Addressed https://gitlab.com/libreumg/dataquier/-/issues/244 and https://gitlab.com/libreumg/dataquier/-/issues/212
     * Default for result-slot-filter was not set (`filter_result_slots` in `dq_report2`)
     * Sometimes, long labels in the first columns of a `JS`-table prevented controlling the table

## (Indicator) Functions related 
  * Fixed missed check for missing cross-item level metadata and earlier check for valid item-level metadata
  * Control crude segment missingness output, so that we see it only if there is more than one segment on the item-level after the removal of `VARIABLE_ROLES` filtered items
  * Outliers should work with empty metadata in `UNIVARIATE_OUTLIER_CHECKTYPE` and `MULTIVARIATE_OUTLIER_CHECKTYPE`
  * Fixed successive dates to ignore empty dates
  * New functions in `REDCap` syntax: `strictly_successive_dates` and `successive_dates`
  * Bug fixes for `REDCap` rules and `NA` handling and `DATA_PROCESS`.
  * Checked, that code is in line with https://gitlab.com/libreumg/dataquier/-/issues/243#note_1419465360
  * Default for contradictions with the new syntax is now that hard limits and missing codes are not removed. The argument `use_value_labels` is not supported anymore. You can specify the behavior on the rules level in the new cross-item-level metadata column `DATA_PREPARATION`
  * Compute end digit preferences only if explicitly requested by a new item-level metadata column `END_DIGIT_CHECK` in `dq_report2`, (`DATA_ENTRY_TYPE` is still supported and auto-converted). If missing, `END_DIGIT_CHECK` defaults to `FALSE`
  * Bug fix: Contradiction rules failed in specific cases if `NA` were in the data
  * Bug fix: cross-item_level normalization crashed, causing rules to fail, e.g., `JUMP_LIST` could be added to the item-level metadata if missing, but causing this type of failing rules
  * Bug fixes for `Windows` and uncommon variable names

## General 
  * Workbooks can now be loaded from the internet (using `prep_load_workbook_like_file` and `meta_data_v2 = ` formal in `dq_report2`) supporting `http` and `https` URLs (e.g., `Excel` or `OpenOffice` workbooks)
  * Documentation updates
    
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
  * Robustness: Ensure `DT JS` is always loaded when a `dq_report` report is 
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
  * [JOSS](https://github.com/openjournals/joss-reviews/issues/3093#issuecomment-840695360)
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
