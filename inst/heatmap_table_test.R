load(system.file("extdata", "study_data.RData", package = "dataquieR"))
sd1 <- study_data
load(system.file("extdata", "meta_data.RData", package = "dataquieR"))
md1 <- meta_data
code_labels <- read.csv2(system.file("extdata",
                                     "Missing-Codes-2020.csv",
                                     package = "dataquieR"),
                         stringsAsFactors = FALSE, na.strings = c())

item_miss <- com_item_missingness(study_data      = sd1,
meta_data       = md1,
label_col       = 'LABEL',
show_causes     = TRUE,
cause_label_df  = code_labels,
include_sysmiss = TRUE,
threshold_value = 80
)
View(item_miss$ReportSummaryTable)
View(md0)
View(mdmd1)
View(md1)
View(item_miss$ReportSummaryTable)
x <- (item_miss$ReportSummaryTable)
rownames(x)
rownames(x) <- x$Variables
x$Variables <- NULL
x
x <- x / X$N
x <- x / x$N
x
x$N <- NULL
x <- x * 100
View(x)
melt(x)
x$Variable <- rownames(x)
x
melt(x, measure.vars = setdiff(colnames(x, c("Variable"))))
melt(x)
ggballoonplot(melt(x))



apm <- pro_applicability_matrix(study_data, meta_data)

print(item_miss$ReportSummaryTable, dt = TRUE)
print(apm$ReportSummaryTable, dt = TRUE)

item_missc <- com_item_missingness(study_data      = sd1,
                                   resp_vars       = c("CENTER_0"),
                                   meta_data       = md1,
                                   label_col       = 'LABEL',
                                   show_causes     = TRUE,
                                   cause_label_df  = code_labels,
                                   include_sysmiss = TRUE,
                                   threshold_value = 80
)

item_misss <- com_item_missingness(study_data      = sd1,
                                   resp_vars       = c("SEX_0"),
                                   meta_data       = md1,
                                   label_col       = 'LABEL',
                                   show_causes     = TRUE,
                                   cause_label_df  = code_labels,
                                   include_sysmiss = TRUE,
                                   threshold_value = 80
)

item_missa <- com_item_missingness(study_data      = sd1,
                                   resp_vars       = c("AGE_0"),
                                   meta_data       = md1,
                                   label_col       = 'LABEL',
                                   show_causes     = TRUE,
                                   cause_label_df  = code_labels,
                                   include_sysmiss = TRUE,
                                   threshold_value = 80
)

item_missb <- com_item_missingness(study_data      = sd1,
                                   resp_vars       = c("SBP_0"),
                                   meta_data       = md1,
                                   label_col       = 'LABEL',
                                   show_causes     = TRUE,
                                   cause_label_df  = code_labels,
                                   include_sysmiss = TRUE,
                                   threshold_value = 80
)

item_miss_combined <- rbind(item_missa$ReportSummaryTable,
                            item_missb$ReportSummaryTable,
                            item_missc$ReportSummaryTable,
                            item_misss$ReportSummaryTable)

item_miss_combined

print(item_miss_combined, dt = TRUE)
print(item_missa$ReportSummaryTable, dt = TRUE)
print(item_missb$ReportSummaryTable, dt = TRUE)
print(item_missc$ReportSummaryTable, dt = TRUE)
print(item_misss$ReportSummaryTable, dt = TRUE)
