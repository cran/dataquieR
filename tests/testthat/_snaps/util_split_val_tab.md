# prep_unsplit_val_tabs works

    Code
      clt
    Output
          CODE_VALUE                               CODE_LABEL CODE_ORDER
      1            1                                (-Inf,20]          1
      2            2                                  (20,30]          2
      3            3                                (30, Inf]          3
      4            1                                   Berlin         NA
      5            2                                  Hamburg         NA
      6            3                                  Leipzig         NA
      7            4                                  Cologne         NA
      8            5                                   Munich         NA
      9       SR_120                                   SR_120         NA
      10     USR_125                                  USR_125         NA
      11     USR_130                                  USR_130         NA
      12     USR_201                                  USR_201         NA
      13     USR_247                                  USR_247         NA
      14     USR_277                                  USR_277         NA
      15     USR_321                                  USR_321         NA
      16     USR_333                                  USR_333         NA
      17     USR_357                                  USR_357         NA
      18     USR_492                                  USR_492         NA
      19     USR_493                                  USR_493         NA
      20     USR_494                                  USR_494         NA
      21     USR_500                                  USR_500         NA
      22     USR_510                                  USR_510         NA
      23     USR_520                                  USR_520         NA
      24     USR_101                                  USR_101         NA
      25     USR_103                                  USR_103         NA
      26     USR_155                                  USR_155         NA
      27     USR_211                                  USR_211         NA
      28     USR_213                                  USR_213         NA
      29     USR_215                                  USR_215         NA
      30     USR_321                                  USR_321         NA
      31     USR_333                                  USR_333         NA
      32     USR_342                                  USR_342         NA
      33     USR_402                                  USR_402         NA
      34     USR_403                                  USR_403         NA
      35     USR_404                                  USR_404         NA
      36     USR_590                                  USR_590         NA
      37     USR_592                                  USR_592         NA
      38     USR_599                                  USR_599         NA
      39     USR_121                                  USR_121         NA
      40     USR_123                                  USR_123         NA
      41     USR_165                                  USR_165         NA
      42     USR_201                                  USR_201         NA
      43     USR_243                                  USR_243         NA
      44     USR_275                                  USR_275         NA
      45     USR_301                                  USR_301         NA
      46     USR_303                                  USR_303         NA
      47     USR_352                                  USR_352         NA
      48     USR_482                                  USR_482         NA
      49     USR_483                                  USR_483         NA
      50     USR_484                                  USR_484         NA
      51     USR_537                                  USR_537         NA
      52     USR_542                                  USR_542         NA
      53     USR_559                                  USR_559         NA
      54           0                                below 10k          1
      55           1                                 [10-30k)          2
      56           2                                 [30-50k)          3
      57           3                                 [50-70k)          4
      58           4                                 [70-90k)          5
      59           5                                above 90k          6
      60           A                                excellent          1
      61           B                                     good          2
      62           C                                 moderate          3
      63           D                               restricted          4
      64           E                             pathological          5
      65           0                                  females         NA
      66           1                                    males         NA
      67           0                                    never          1
      68           1                              1-2d a week          2
      69           2                              3-4d a week          3
      70           3                              5-6d a week          4
      71           4                                    daily          5
      72           0                                       no         NA
      73           1                                      yes         NA
      74           0                                     none         NA
      75           1                               vegetarian         NA
      76           2                                    vegan         NA
      77           0                              pre-primary          1
      78           1                                  primary          2
      79           2                                secondary          3
      80           3                              uppersecond          4
      81           4                               postsecond          5
      82           5                                 tertiary          6
      83           6                           secondtertiary          7
      84      single                                   single         NA
      85     married                                  married         NA
      86    divorced                                 divorced         NA
      87     widowed                                  widowed         NA
      88       99980                   Missing - other reason         NA
      89       99981             Missing - exclusion criteria         NA
      90       99982                        Missing - refusal         NA
      91       99983                 Missing - not assessable         NA
      92       99984              Missing - technical problem         NA
      93       99985       Missing - not available (material)         NA
      94       99986          Missing - not usable (material)         NA
      95       99987                 Missing - reason unknown         NA
      96       99988                 Missing - optional value         NA
      97       99989                   Deleted - other reason         NA
      98       99990                  Deleted - contradiction         NA
      99       99991           Deleted - value outside limits         NA
      100      99992                Inaccurate - other reason         NA
      101      99993 Inaccurate - value above detection limit         NA
      102      99994 Inaccurate - value below detection limit         NA
      103      99995                  Data management ongoing         NA
      104      88880                               JUMP 88880         NA
      105      88890                               JUMP 88890         NA
                         VALUE_LABEL_TABLE CODE_CLASS CODE_INTERPRET
      1    LABS_(-Inf,20 (20,30]...::8d957      VALUE           <NA>
      2    LABS_(-Inf,20 (20,30]...::8d957      VALUE           <NA>
      3    LABS_(-Inf,20 (20,30]...::8d957      VALUE           <NA>
      4      LABS_Berlin Hamburg...::97896      VALUE           <NA>
      5      LABS_Berlin Hamburg...::97896      VALUE           <NA>
      6      LABS_Berlin Hamburg...::97896      VALUE           <NA>
      7      LABS_Berlin Hamburg...::97896      VALUE           <NA>
      8      LABS_Berlin Hamburg...::97896      VALUE           <NA>
      9      LABS_SR_120 USR_125...::be858      VALUE           <NA>
      10     LABS_SR_120 USR_125...::be858      VALUE           <NA>
      11     LABS_SR_120 USR_125...::be858      VALUE           <NA>
      12     LABS_SR_120 USR_125...::be858      VALUE           <NA>
      13     LABS_SR_120 USR_125...::be858      VALUE           <NA>
      14     LABS_SR_120 USR_125...::be858      VALUE           <NA>
      15     LABS_SR_120 USR_125...::be858      VALUE           <NA>
      16     LABS_SR_120 USR_125...::be858      VALUE           <NA>
      17     LABS_SR_120 USR_125...::be858      VALUE           <NA>
      18     LABS_SR_120 USR_125...::be858      VALUE           <NA>
      19     LABS_SR_120 USR_125...::be858      VALUE           <NA>
      20     LABS_SR_120 USR_125...::be858      VALUE           <NA>
      21     LABS_SR_120 USR_125...::be858      VALUE           <NA>
      22     LABS_SR_120 USR_125...::be858      VALUE           <NA>
      23     LABS_SR_120 USR_125...::be858      VALUE           <NA>
      24    LABS_USR_101 USR_103...::f7ae9      VALUE           <NA>
      25    LABS_USR_101 USR_103...::f7ae9      VALUE           <NA>
      26    LABS_USR_101 USR_103...::f7ae9      VALUE           <NA>
      27    LABS_USR_101 USR_103...::f7ae9      VALUE           <NA>
      28    LABS_USR_101 USR_103...::f7ae9      VALUE           <NA>
      29    LABS_USR_101 USR_103...::f7ae9      VALUE           <NA>
      30    LABS_USR_101 USR_103...::f7ae9      VALUE           <NA>
      31    LABS_USR_101 USR_103...::f7ae9      VALUE           <NA>
      32    LABS_USR_101 USR_103...::f7ae9      VALUE           <NA>
      33    LABS_USR_101 USR_103...::f7ae9      VALUE           <NA>
      34    LABS_USR_101 USR_103...::f7ae9      VALUE           <NA>
      35    LABS_USR_101 USR_103...::f7ae9      VALUE           <NA>
      36    LABS_USR_101 USR_103...::f7ae9      VALUE           <NA>
      37    LABS_USR_101 USR_103...::f7ae9      VALUE           <NA>
      38    LABS_USR_101 USR_103...::f7ae9      VALUE           <NA>
      39    LABS_USR_121 USR_123...::c993d      VALUE           <NA>
      40    LABS_USR_121 USR_123...::c993d      VALUE           <NA>
      41    LABS_USR_121 USR_123...::c993d      VALUE           <NA>
      42    LABS_USR_121 USR_123...::c993d      VALUE           <NA>
      43    LABS_USR_121 USR_123...::c993d      VALUE           <NA>
      44    LABS_USR_121 USR_123...::c993d      VALUE           <NA>
      45    LABS_USR_121 USR_123...::c993d      VALUE           <NA>
      46    LABS_USR_121 USR_123...::c993d      VALUE           <NA>
      47    LABS_USR_121 USR_123...::c993d      VALUE           <NA>
      48    LABS_USR_121 USR_123...::c993d      VALUE           <NA>
      49    LABS_USR_121 USR_123...::c993d      VALUE           <NA>
      50    LABS_USR_121 USR_123...::c993d      VALUE           <NA>
      51    LABS_USR_121 USR_123...::c993d      VALUE           <NA>
      52    LABS_USR_121 USR_123...::c993d      VALUE           <NA>
      53    LABS_USR_121 USR_123...::c993d      VALUE           <NA>
      54  LABS_below 10 [10-30k)...::7c9d1      VALUE           <NA>
      55  LABS_below 10 [10-30k)...::7c9d1      VALUE           <NA>
      56  LABS_below 10 [10-30k)...::7c9d1      VALUE           <NA>
      57  LABS_below 10 [10-30k)...::7c9d1      VALUE           <NA>
      58  LABS_below 10 [10-30k)...::7c9d1      VALUE           <NA>
      59  LABS_below 10 [10-30k)...::7c9d1      VALUE           <NA>
      60      LABS_excellen good...::dfd3a      VALUE           <NA>
      61      LABS_excellen good...::dfd3a      VALUE           <NA>
      62      LABS_excellen good...::dfd3a      VALUE           <NA>
      63      LABS_excellen good...::dfd3a      VALUE           <NA>
      64      LABS_excellen good...::dfd3a      VALUE           <NA>
      65         LABS_females males::8a1bf      VALUE           <NA>
      66         LABS_females males::8a1bf      VALUE           <NA>
      67     LABS_never 1-2d a w...::9f0f5      VALUE           <NA>
      68     LABS_never 1-2d a w...::9f0f5      VALUE           <NA>
      69     LABS_never 1-2d a w...::9f0f5      VALUE           <NA>
      70     LABS_never 1-2d a w...::9f0f5      VALUE           <NA>
      71     LABS_never 1-2d a w...::9f0f5      VALUE           <NA>
      72                LABS_no yes::cde3d      VALUE           <NA>
      73                LABS_no yes::cde3d      VALUE           <NA>
      74      LABS_none vegetari...::ba106      VALUE           <NA>
      75      LABS_none vegetari...::ba106      VALUE           <NA>
      76      LABS_none vegetari...::ba106      VALUE           <NA>
      77   LABS_pre-prim primary...::66ffb      VALUE           <NA>
      78   LABS_pre-prim primary...::66ffb      VALUE           <NA>
      79   LABS_pre-prim primary...::66ffb      VALUE           <NA>
      80   LABS_pre-prim primary...::66ffb      VALUE           <NA>
      81   LABS_pre-prim primary...::66ffb      VALUE           <NA>
      82   LABS_pre-prim primary...::66ffb      VALUE           <NA>
      83   LABS_pre-prim primary...::66ffb      VALUE           <NA>
      84     LABS_single married...::5eb6b      VALUE           <NA>
      85     LABS_single married...::5eb6b      VALUE           <NA>
      86     LABS_single married...::5eb6b      VALUE           <NA>
      87     LABS_single married...::5eb6b      VALUE           <NA>
      88                              <NA>    MISSING              O
      89                              <NA>    MISSING             NE
      90                              <NA>    MISSING              R
      91                              <NA>    MISSING             NC
      92                              <NA>    MISSING              O
      93                              <NA>    MISSING             NC
      94                              <NA>    MISSING             NC
      95                              <NA>    MISSING              O
      96                              <NA>    MISSING             NE
      97                              <NA>    MISSING              O
      98                              <NA>    MISSING              O
      99                              <NA>    MISSING              O
      100                             <NA>    MISSING              O
      101                             <NA>    MISSING              O
      102                             <NA>    MISSING              O
      103                             <NA>    MISSING              O
      104                             <NA>       JUMP             NE
      105                             <NA>       JUMP             NE
          MISSING_LIST_TABLE
      1                 <NA>
      2                 <NA>
      3                 <NA>
      4                 <NA>
      5                 <NA>
      6                 <NA>
      7                 <NA>
      8                 <NA>
      9                 <NA>
      10                <NA>
      11                <NA>
      12                <NA>
      13                <NA>
      14                <NA>
      15                <NA>
      16                <NA>
      17                <NA>
      18                <NA>
      19                <NA>
      20                <NA>
      21                <NA>
      22                <NA>
      23                <NA>
      24                <NA>
      25                <NA>
      26                <NA>
      27                <NA>
      28                <NA>
      29                <NA>
      30                <NA>
      31                <NA>
      32                <NA>
      33                <NA>
      34                <NA>
      35                <NA>
      36                <NA>
      37                <NA>
      38                <NA>
      39                <NA>
      40                <NA>
      41                <NA>
      42                <NA>
      43                <NA>
      44                <NA>
      45                <NA>
      46                <NA>
      47                <NA>
      48                <NA>
      49                <NA>
      50                <NA>
      51                <NA>
      52                <NA>
      53                <NA>
      54                <NA>
      55                <NA>
      56                <NA>
      57                <NA>
      58                <NA>
      59                <NA>
      60                <NA>
      61                <NA>
      62                <NA>
      63                <NA>
      64                <NA>
      65                <NA>
      66                <NA>
      67                <NA>
      68                <NA>
      69                <NA>
      70                <NA>
      71                <NA>
      72                <NA>
      73                <NA>
      74                <NA>
      75                <NA>
      76                <NA>
      77                <NA>
      78                <NA>
      79                <NA>
      80                <NA>
      81                <NA>
      82                <NA>
      83                <NA>
      84                <NA>
      85                <NA>
      86                <NA>
      87                <NA>
      88       missing_table
      89       missing_table
      90       missing_table
      91       missing_table
      92       missing_table
      93       missing_table
      94       missing_table
      95       missing_table
      96       missing_table
      97       missing_table
      98       missing_table
      99       missing_table
      100      missing_table
      101      missing_table
      102      missing_table
      103      missing_table
      104      missing_table
      105      missing_table

---

    Code
      prep_list_dataframes()
    Output
       [1] "LABS_(-Inf,20 (20,30]...::8d957"  "LABS_Berlin Hamburg...::97896"   
       [3] "LABS_SR_120 USR_125...::be858"    "LABS_USR_101 USR_103...::f7ae9"  
       [5] "LABS_USR_121 USR_123...::c993d"   "LABS_below 10 [10-30k)...::7c9d1"
       [7] "LABS_excellen good...::dfd3a"     "LABS_females males::8a1bf"       
       [9] "LABS_never 1-2d a w...::9f0f5"    "LABS_no yes::cde3d"              
      [11] "LABS_none vegetari...::ba106"     "LABS_pre-prim primary...::66ffb" 
      [13] "LABS_single married...::5eb6b"    "missing_table"                   

