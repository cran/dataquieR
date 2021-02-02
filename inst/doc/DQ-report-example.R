## ----include=FALSE------------------------------------------------------------
library(knitr)
library(DT)
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)
if (rmarkdown::pandoc_available()) {
  knit_print.data.frame = function(x, ...) {
    knit_print(DT::datatable(head(x, 10)), ...)
  }
  registerS3method("knit_print", "data.frame", knit_print.data.frame)
}
library(dataquieR)

## ----echo=TRUE, warning=FALSE, message=FALSE----------------------------------
load(system.file("extdata", "study_data.RData", package = "dataquieR"))
sd1 <- study_data

## ----echo=TRUE, warning=FALSE, message=FALSE----------------------------------
load(system.file("extdata", "meta_data.RData", package = "dataquieR"))
md1 <- meta_data

## ----message=FALSE, warning=FALSE---------------------------------------------
appmatrix <- pro_applicability_matrix(study_data = sd1, 
                                      meta_data = md1, 
                                      label_col = LABEL)

## ----message=FALSE, warning=FALSE, fig.height = 10, fig.width = 6-------------
appmatrix$ApplicabilityPlot

## ----message = FALSE, warning = FALSE-----------------------------------------
my_unit_missings2 <- com_unit_missingness(study_data  = sd1, 
                                          meta_data   = md1, 
                                          id_vars     = c("CENTER_0", "PSEUDO_ID"), 
                                          strata_vars = "CENTER_0", 
                                          label_col   = "LABEL")

## -----------------------------------------------------------------------------
my_unit_missings2$SummaryData

## ----message=FALSE, warning=FALSE---------------------------------------------
MissSegs <- com_segment_missingness(study_data = sd1, 
                                    meta_data = md1, 
                                    label_col = "LABEL", 
                                    threshold_value = 5, 
                                    direction = "high",
                                    exclude_roles = c("secondary", "process"))

## ----message=FALSE, echo=TRUE, warning=FALSE, results = 'hide', fig.keep = 'all',  fig.align="center", fig.height = 3, fig.width = 4----
MissSegs$SummaryPlot

## ----message=FALSE, warning=FALSE---------------------------------------------
# use the month function of the lubridate package to extract month of exam date
require(lubridate)
# apply changes to copy of data
sd2 <- sd1
# indicate first/second half year
sd2$month <- month(sd2$v00013)

## ----message=FALSE, warning=FALSE---------------------------------------------
MD_TMP <- prep_add_to_meta(VAR_NAMES    = "month",
                           DATA_TYPE    = "integer",
                           LABEL        = "EXAM_MONTH",
                           VALUE_LABELS = "1 = January | 2 = February | 3 = March | 
                                          4 = April | 5 = May | 6 = June | 7 = July |
                                          8 = August | 9 = September | 10 = October |
                                          11 = November | 12 = December",
                           meta_data    = md1)

## ----message=FALSE, warning=FALSE---------------------------------------------
MissSegs <- com_segment_missingness(study_data = sd2, 
                                    meta_data = MD_TMP, 
                                    group_vars = "EXAM_MONTH", 
                                    label_col = "LABEL", 
                                    threshold_value = 1, 
                                    direction = "high",
                                    exclude_roles = c("secondary", "process"))

## ----message=FALSE, echo=TRUE, warning=FALSE, results = 'hide', fig.keep = 'all',  fig.align="center", fig.height = 6, fig.width = 4----
MissSegs$SummaryPlot

## ----message=FALSE, warning=FALSE---------------------------------------------
code_labels <- read.csv2(system.file("extdata", 
                                     "Missing-Codes-2020.csv", 
                                     package = "dataquieR"), 
                         stringsAsFactors = FALSE, na.strings = c())

## ----message = FALSE, warning = FALSE-----------------------------------------
item_miss <- com_item_missingness(study_data      = sd1, 
                                  meta_data       = meta_data, 
                                  label_col       = 'LABEL', 
                                  show_causes     = TRUE, 
                                  cause_label_df  = code_labels,
                                  include_sysmiss = TRUE, 
                                  threshold_value = 80
                                ) 

## ----message=FALSE, echo=TRUE, warning=FALSE----------------------------------
item_miss$SummaryTable

## ----message=FALSE, echo=TRUE, warning=FALSE, fig.height=5, fig.width = 5-----
item_miss$SummaryPlot

## -----------------------------------------------------------------------------
MyValueLimits <- con_limit_deviations(resp_vars  = NULL,
                                      label_col  = "LABEL",
                                      study_data = sd1,
                                      meta_data  = md1,
                                      limits     = "HARD_LIMITS")

## ----message=FALSE, echo=TRUE, warning=FALSE----------------------------------
MyValueLimits$SummaryTable

## -----------------------------------------------------------------------------
# select variables with deviations
whichdeviate <- as.character(MyValueLimits$SummaryTable$Variables)[MyValueLimits$SummaryTable$GRADING == 1]

## ----message=FALSE, echo=TRUE, warning=FALSE, results = 'hide', fig.keep = 'all', fig.align="center", fig.height = 3, fig.width = 4----
ggpubr::ggarrange(plotlist = MyValueLimits$SummaryPlotList[whichdeviate], ncol = 2) 

## ----message=FALSE, warning=FALSE---------------------------------------------
IAVCatAll <- con_inadmissible_categorical(study_data = sd1, 
                                          meta_data  = md1, 
                                          label_col  = "LABEL")

## ----message=FALSE, warning=FALSE---------------------------------------------
checks <- read.csv(system.file("extdata", 
                               "contradiction_checks.csv",
                               package = "dataquieR"), 
                   header = TRUE, sep = "#")

## -----------------------------------------------------------------------------
AnyContradictions <- con_contradictions(study_data      = sd1,
                                        meta_data       = md1,
                                        label_col       = "LABEL",
                                        check_table     = checks,
                                        threshold_value = 1)

## ----message=FALSE, echo=TRUE, warning=FALSE----------------------------------
AnyContradictions$SummaryTable

## ----message=FALSE, echo=TRUE, warning=FALSE, fig.height = 4, fig.width = 6----
AnyContradictions$SummaryPlot 

## ----echo = TRUE--------------------------------------------------------------
ruol <- dataquieR:::acc_robust_univariate_outlier(study_data = sd1, meta_data = md1, label_col = LABEL)

## ---- fig.height = 3, fig.width = 4-------------------------------------------
myloess <- dataquieR::acc_loess(resp_vars = "SBP_0",
                                group_vars = "USR_BP_0",
                                time_vars = "EXAM_DT_0",
                                label_col = "LABEL",
                                study_data = sd1,
                                meta_data = md1)

myloess$SummaryPlotList$Loess_fits_combined

