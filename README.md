
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `dataquieR`

<!-- badges: start -->

[![minimal R
version](https://img.shields.io/badge/R%3E%3D-3.6.0-6666ff.svg)](https://cran.r-project.org/)
[![Pipeline
Status](https://gitlab.com/libreumg/dataquier/badges/master/pipeline.svg?ignore_skipped=true)](https://libreumg.gitlab.io/dataquier/)
[![Coverage](https://codecov.io/gl/libreumg/dataquier/branch/master/graph/badge.svg?token=79TK6GQTMG)](https://app.codecov.io/gl/libreumg/dataquier)
[![CRAN-Version](https://www.r-pkg.org/badges/version/dataquieR)](https://cran.r-project.org/package=dataquieR)
[![CRAN-Downloads](https://cranlogs.r-pkg.org/badges/dataquieR)](https://www.r-pkg.org/pkg/dataquieR)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![`Lifecycle`](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![license](https://img.shields.io/badge/license-BSD_2_clause%20+%20file%20LICENSE-00be00.svg)](https://opensource.org/license/bsd-2-clause)
[![DOI](https://joss.theoj.org/papers/10.21105/joss.03093/status.svg)](https://doi.org/10.21105/joss.03093)

<!-- badges: end -->

The goal of `dataquieR` is to provide functions for assessing data
quality issues in studies, that can be used alone or in a data quality
pipeline. `dataquieR` also implements one generic pipeline producing
`flexdashboard` based HTML5 reports.

See also

[`https://dataquality.qihs.uni-greifswald.de`](https://dataquality.qihs.uni-greifswald.de)

------------------------------------------------------------------------

## Installation

You can install the released version of `dataquieR` from
[CRAN](https://CRAN.R-project.org/package=dataquieR) with:

``` r
install.packages("dataquieR")
```

The suggested packages can be directly installed by:

``` r
install.packages("dataquieR", dependencies = TRUE)
```

The developer version from
[`GitLab.com`](https://gitlab.com/libreumg/dataquier) can be installed
using:

``` r
if (!requireNamespace("devtools")) {
  install.packages("devtools")
}
devtools::install_gitlab("libreumg/dataquier")
```

For examples and additional documentation, please refer to our
[website](https://dataquality.qihs.uni-greifswald.de).

## Suggested packages

`dataquieR` reports can now use
[`plotly`](https://cran.r-project.org/package=plotly) if installed. That
means that, in the final report, you can zoom in the figures and get
information by hovering on the points, etc. To install `plotly` type:

``` r
install.packages("plotly")
```

## References

- [Software Paper](https://doi.org/10.21105/joss.03093) [![JOSS
  Article](https://joss.theoj.org/papers/10.21105/joss.03093/status.svg)](https://doi.org/10.21105/joss.03093)
- [Data Quality Concept
  Paper](https://doi.org/10.1186/s12874-021-01252-7)
- [Data Quality Concept and Software Web
  Site](https://dataquality.qihs.uni-greifswald.de)

## Funding – see also [here](https://dataquality.qihs.uni-greifswald.de/Contact.html)

- [German Research Foundation](https://www.dfg.de/) (DFG:
  `SCHM 2744/3–1` – initial concept and dataquieR development,
  `SCHM 2744/9-1` – `NFDI` Task Force `COVID-19` use case application;
  `SCHM 2744/3-4` – concept extensions, ongoing )

- [European Union’s Horizon 2020 research and innovation
  program](https://research-and-innovation.ec.europa.eu/funding/funding-opportunities/funding-programmes-and-open-calls/horizon-2020_en):
  [euCanSHare, grant agreement No. 825903](http://www.eucanshare.eu/) –
  [dataquieR](https://cran.r-project.org/package=dataquieR) refinements
  and implementations in the
  [Square2](https://pubmed.ncbi.nlm.nih.gov/28423853/) web application.

- [National Research Data Infrastructure for Personal Health
  Data](https://www.nfdi4health.de/en/): `NFDI 13/1` – extension based
  on revised metadata concept, ongoing.

- [German National Cohort (NAKO Gesundheitsstudie)
  NAKO](https://nako.de/): `BMBF` (`https://www.bmbf.de/`): `01ER1301A`
  and `01ER1801A`
