README
================

## Article Information

Title: Ecosystem size and complexity are extrinsic drivers of food chain
length in branching networks

## File descriptions

`code/` includes all R codes to reproduce findings from this research:

| Name              | Description                             |
|:------------------|:----------------------------------------|
| analysis_stvy.R   | code for sensitivity analysis           |
| figure_concept.R  | code for figure 1                       |
| figure_delta.R    | code for figure 1D                      |
| figure_food_web.R | code for figure 1C                      |
| figure_main_sim.R | code for figure 2, 3                    |
| figure_network.R  | code for figure 1A, 1B                  |
| figure_si_fcl.R   | code for figure S1-S4                   |
| figure_stvy.R     | code for figure of sensitivity analysis |
| figure_theme.R    | code for figure theme                   |
| library.R         | package list                            |
| run_sim.R         | run main simulations                    |
| run_sim_stvy.R    | run sensitivity simulations             |
| table_param.R     | table for parameter values              |
| table_stvy.R      | table for sensitivity analysis          |

## Session Information

    ## R version 4.1.0 (2021-05-18)
    ## Platform: x86_64-w64-mingw32/x64 (64-bit)
    ## Running under: Windows 10 x64 (build 19044)
    ## 
    ## Matrix products: default
    ## 
    ## locale:
    ## [1] LC_COLLATE=English_United States.1252 
    ## [2] LC_CTYPE=English_United States.1252   
    ## [3] LC_MONETARY=English_United States.1252
    ## [4] LC_NUMERIC=C                          
    ## [5] LC_TIME=English_United States.1252    
    ## 
    ## attached base packages:
    ## [1] parallel  stats     graphics  grDevices utils     datasets  methods  
    ## [8] base     
    ## 
    ## other attached packages:
    ##  [1] MetBrewer_0.2.0   igraph_1.2.11     patchwork_1.1.1   NetIndices_1.4.4 
    ##  [5] MASS_7.3-54       ggraph_2.0.5      knitr_1.39        broom_0.7.9      
    ##  [9] stargazer_5.2.3   doSNOW_1.0.19     snow_0.4-3        doParallel_1.0.16
    ## [13] iterators_1.0.13  foreach_1.5.1     tidygraph_1.2.0   mcbrnet_1.2.3    
    ## [17] forcats_0.5.1     stringr_1.4.0     dplyr_1.0.9       purrr_0.3.4      
    ## [21] readr_2.0.0       tidyr_1.1.3       tibble_3.1.6      ggplot2_3.3.5    
    ## [25] tidyverse_1.3.1  
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] viridis_0.6.2      httr_1.4.2         viridisLite_0.4.0  jsonlite_1.7.2    
    ##  [5] here_1.0.1         modelr_0.1.8       assertthat_0.2.1   highr_0.9         
    ##  [9] cellranger_1.1.0   yaml_2.2.1         ggrepel_0.9.1      pillar_1.7.0      
    ## [13] backports_1.2.1    glue_1.6.2         digest_0.6.29      polyclip_1.10-0   
    ## [17] rvest_1.0.1        colorspace_2.0-3   htmltools_0.5.2    pkgconfig_2.0.3   
    ## [21] haven_2.4.1        scales_1.2.0       tweenr_1.0.2       tzdb_0.1.2        
    ## [25] ggforce_0.3.3      generics_0.1.2     farver_2.1.0       ellipsis_0.3.2    
    ## [29] pacman_0.5.1       withr_2.5.0        cli_3.3.0          magrittr_2.0.3    
    ## [33] crayon_1.5.1       readxl_1.3.1       evaluate_0.15      fs_1.5.0          
    ## [37] fansi_1.0.3        xml2_1.3.2         tools_4.1.0        hms_1.1.0         
    ## [41] lifecycle_1.0.1    munsell_0.5.0      reprex_2.0.0       compiler_4.1.0    
    ## [45] rlang_1.0.2        grid_4.1.0         rstudioapi_0.13    rmarkdown_2.13    
    ## [49] gtable_0.3.0       codetools_0.2-18   DBI_1.1.1          graphlayouts_0.7.1
    ## [53] R6_2.5.1           gridExtra_2.3      lubridate_1.7.10   fastmap_1.1.0     
    ## [57] utf8_1.2.2         rprojroot_2.0.2    stringi_1.6.1      Rcpp_1.0.7        
    ## [61] vctrs_0.4.1        dbplyr_2.1.1       tidyselect_1.1.2   xfun_0.30
