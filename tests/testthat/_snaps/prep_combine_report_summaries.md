# prep_combine_report_summaries works

    Code
      comb_sum
    Output
      $Data
        STUDY_SEGMENT VAR_NAMES acc_varcomp_observer.ICC_acc_ud_loc
      1        v20000    v00004                               0.100
      2        v20000    v00005                               0.112
        com_item_missingness.PCT_com_crm_mv
      1                              14.63%
      2                              15.20%
      
      $Table
        STUDY_SEGMENT VAR_NAMES acc_varcomp_observer.ICC_acc_ud_loc
      1        v20000    v00004                               0.100
      2        v20000    v00005                               0.112
        com_item_missingness.PCT_com_crm_mv
      1                               14.63
      2                               15.20
      
      $meta_data
        VAR_NAMES     LABEL DATA_TYPE
      1    v00000  CENTER_0   integer
      2    v00001 PSEUDO_ID    string
      3    v00002     SEX_0   integer
      4    v00003     AGE_0   integer
      5    v00004     SBP_0     float
      6    v00005     DBP_0     float
      7    v00012  USR_BP_0    string
                                                                                                                                                                                                                                                                                                                                                                                         MISSING_LIST
      1                                                                                                                                                                                                                                                                                                                                                                                             |
      2                                                                                                                                                                                                                                                                                                                                                                                             |
      3                                                                                                                                                                                                                                                                                                                                                                                             |
      4                                                                                                                                                                                                                                                                                                                                                                                             |
      5 99980 = MISSING 99980 | 99981 = MISSING 99981 | 99982 = MISSING 99982 | 99983 = MISSING 99983 | 99984 = MISSING 99984 | 99985 = MISSING 99985 | 99986 = MISSING 99986 | 99987 = MISSING 99987 | 99988 = MISSING 99988 | 99989 = MISSING 99989 | 99990 = MISSING 99990 | 99991 = MISSING 99991 | 99992 = MISSING 99992 | 99993 = MISSING 99993 | 99994 = MISSING 99994 | 99995 = MISSING 99995
      6 99980 = MISSING 99980 | 99981 = MISSING 99981 | 99982 = MISSING 99982 | 99983 = MISSING 99983 | 99984 = MISSING 99984 | 99985 = MISSING 99985 | 99986 = MISSING 99986 | 99987 = MISSING 99987 | 99988 = MISSING 99988 | 99989 = MISSING 99989 | 99990 = MISSING 99990 | 99991 = MISSING 99991 | 99992 = MISSING 99992 | 99993 = MISSING 99993 | 99994 = MISSING 99994 | 99995 = MISSING 99995
      7                                                                                                                                                                                                                                                                                                                                                 99981 = MISSING 99981 | 99982 = MISSING 99982
        JUMP_LIST HARD_LIMITS DETECTION_LIMITS CONTRADICTIONS SOFT_LIMITS
      1         |        <NA>             <NA>           <NA>        <NA>
      2         |        <NA>             <NA>           <NA>        <NA>
      3         |        <NA>             <NA>           1002        <NA>
      4         |    [18;Inf)             <NA>           1001        <NA>
      5         |    [80;180]          [0;265]           <NA>    (90;170)
      6         |    [50;Inf)          [0;265]           <NA>    (55;100)
      7         |        <NA>             <NA>           <NA>        <NA>
        DISTRIBUTION DECIMALS GROUP_VAR_OBSERVER GROUP_VAR_DEVICE VARIABLE_ROLE
      1         <NA>       NA               <NA>             <NA>         intro
      2         <NA>       NA               <NA>             <NA>         intro
      3         <NA>       NA               <NA>             <NA>         intro
      4         <NA>       NA               <NA>             <NA>         intro
      5       normal        0             v00012             <NA>       primary
      6       normal        0             v00012             <NA>       primary
      7         <NA>       NA               <NA>             <NA>       process
        VARIABLE_ORDER                 LONG_LABEL LOCATION_RANGE LOCATION_METRIC
      1              1                   CENTER_0           <NA>            <NA>
      2              2                  PSEUDO_ID           <NA>            <NA>
      3              3                      SEX_0           <NA>            <NA>
      4              4                      AGE_0        [45;55]            Mean
      5              9  SYSTOLIC_BLOOD_PRESSURE_0      (100;140)            Mean
      6             10 DIASTOLIC_BLOOD_PRESSURE_0       (60;100)            Mean
      7             18              EXAMINER_BP_0           <NA>            <NA>
        PROPORTION_RANGE END_DIGIT_CHECK STUDY_SEGMENT TIME_VAR
      1          [15;30]           FALSE        v10000     <NA>
      2             <NA>           FALSE        v10000     <NA>
      3     0 in [48;52]           FALSE        v10000     <NA>
      4             <NA>           FALSE        v10000     <NA>
      5             <NA>           FALSE        v20000   v00013
      6             <NA>           FALSE        v20000   v00013
      7             <NA>           FALSE        v20000     <NA>
                     VALUE_LABEL_TABLE N_RULES UNIVARIATE_OUTLIER_CHECKTYPE
      1  LABS_Berlin Hamburg...::97896      NA                         <NA>
      2                           <NA>      NA                         <NA>
      3      LABS_females males::8a1bf      NA                         <NA>
      4                           <NA>      NA                         <NA>
      5                           <NA>      NA                         <NA>
      6                           <NA>      NA                         <NA>
      7 LABS_USR_121 USR_123...::c993d      NA                         <NA>
          MISSING_LIST_TABLE SCALE_LEVEL
      1 MISSING_LIST_TABLE_0     nominal
      2 MISSING_LIST_TABLE_0          na
      3 MISSING_LIST_TABLE_0     nominal
      4 MISSING_LIST_TABLE_0       ratio
      5 MISSING_LIST_TABLE_0       ratio
      6 MISSING_LIST_TABLE_0       ratio
      7 MISSING_LIST_TABLE_0     nominal
      
      attr(,"class")
      [1] "dq_report2_summary"

