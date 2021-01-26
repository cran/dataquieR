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
  
# dataquieR (1.0.0)
  * Initial CRAN release candidate
