README
================

## Article Information

Title: Ecosystem size and complexity are extrinsic drivers of food chain
length in branching networks

## File descriptions

`code/` includes all R codes to reproduce findings from this research:

| Name                   | Description                                                                                                                                                                                     |
|:-----------------------|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| figure_concept.R       | Code for figures: ’\_concept’, Figure 1; ’\_delta’, Figure 1D; ’\_food_web’, Figure 1C; ’\_treeplot’, Figure2; ’\_main_geom’, Figure 3, 4; ’\_network’, Figure 1A; ’\_patch_state’, Figure 5-10 |
| figure_delta.R         | Code for figures: ’\_concept’, Figure 1; ’\_delta’, Figure 1D; ’\_food_web’, Figure 1C; ’\_treeplot’, Figure2; ’\_main_geom’, Figure 3, 4; ’\_network’, Figure 1A; ’\_patch_state’, Figure 5-10 |
| figure_food_web.R      | Code for figures: ’\_concept’, Figure 1; ’\_delta’, Figure 1D; ’\_food_web’, Figure 1C; ’\_treeplot’, Figure2; ’\_main_geom’, Figure 3, 4; ’\_network’, Figure 1A; ’\_patch_state’, Figure 5-10 |
| figure_main_geom.R     | Code for figures: ’\_concept’, Figure 1; ’\_delta’, Figure 1D; ’\_food_web’, Figure 1C; ’\_treeplot’, Figure2; ’\_main_geom’, Figure 3, 4; ’\_network’, Figure 1A; ’\_patch_state’, Figure 5-10 |
| figure_network.R       | Code for figures: ’\_concept’, Figure 1; ’\_delta’, Figure 1D; ’\_food_web’, Figure 1C; ’\_treeplot’, Figure2; ’\_main_geom’, Figure 3, 4; ’\_network’, Figure 1A; ’\_patch_state’, Figure 5-10 |
| figure_patch_state.R   | Code for figures: ’\_concept’, Figure 1; ’\_delta’, Figure 1D; ’\_food_web’, Figure 1C; ’\_treeplot’, Figure2; ’\_main_geom’, Figure 3, 4; ’\_network’, Figure 1A; ’\_patch_state’, Figure 5-10 |
| figure_si_geom.R       | Code for figures: ’\_concept’, Figure 1; ’\_delta’, Figure 1D; ’\_food_web’, Figure 1C; ’\_treeplot’, Figure2; ’\_main_geom’, Figure 3, 4; ’\_network’, Figure 1A; ’\_patch_state’, Figure 5-10 |
| figure_si_one_patch.R  | Code for figures: ’\_concept’, Figure 1; ’\_delta’, Figure 1D; ’\_food_web’, Figure 1C; ’\_treeplot’, Figure2; ’\_main_geom’, Figure 3, 4; ’\_network’, Figure 1A; ’\_patch_state’, Figure 5-10 |
| figure_si_r_disp.R     | Code for figures: ’\_concept’, Figure 1; ’\_delta’, Figure 1D; ’\_food_web’, Figure 1C; ’\_treeplot’, Figure2; ’\_main_geom’, Figure 3, 4; ’\_network’, Figure 1A; ’\_patch_state’, Figure 5-10 |
| figure_si_within_net.R | Code for figures: ’\_concept’, Figure 1; ’\_delta’, Figure 1D; ’\_food_web’, Figure 1C; ’\_treeplot’, Figure2; ’\_main_geom’, Figure 3, 4; ’\_network’, Figure 1A; ’\_patch_state’, Figure 5-10 |
| figure_treeplot.R      | Code for figures: ’\_concept’, Figure 1; ’\_delta’, Figure 1D; ’\_food_web’, Figure 1C; ’\_treeplot’, Figure2; ’\_main_geom’, Figure 3, 4; ’\_network’, Figure 1A; ’\_patch_state’, Figure 5-10 |
| format_sim_data.R      | Code for formatting                                                                                                                                                                             |
| library.R              | Code for figures                                                                                                                                                                                |
| run_sim.R              | Code for simulations                                                                                                                                                                            |
| run_sim_one_patch.R    | Code for simulations                                                                                                                                                                            |
| run_sim_within_net.R   | Code for simulations                                                                                                                                                                            |
| set_figure_theme.R     | Code for setting figure themes                                                                                                                                                                  |
| table_param.R          | Code for tables                                                                                                                                                                                 |

## Session Information

    ## R version 4.2.1 (2022-06-23 ucrt)
    ## Platform: x86_64-w64-mingw32/x64 (64-bit)
    ## Running under: Windows 10 x64 (build 19045)
    ## 
    ## Matrix products: default
    ## 
    ## locale:
    ## [1] LC_COLLATE=English_United States.utf8 
    ## [2] LC_CTYPE=English_United States.utf8   
    ## [3] LC_MONETARY=English_United States.utf8
    ## [4] LC_NUMERIC=C                          
    ## [5] LC_TIME=English_United States.utf8    
    ## 
    ## attached base packages:
    ## [1] parallel  stats     graphics  grDevices utils     datasets  methods  
    ## [8] base     
    ## 
    ## other attached packages:
    ##  [1] here_1.0.1           tictoc_1.1           MetBrewer_0.2.0     
    ##  [4] igraph_1.4.1         patchwork_1.1.2.9000 NetIndices_1.4.4.1  
    ##  [7] MASS_7.3-58.1        ggraph_2.1.0         knitr_1.40.4        
    ## [10] broom_1.0.1          stargazer_5.2.3      doSNOW_1.0.20       
    ## [13] snow_0.4-4           doParallel_1.0.17    iterators_1.0.14    
    ## [16] foreach_1.5.2        tidygraph_1.2.3      mcbrnet_1.3.1       
    ## [19] forcats_0.5.2        stringr_1.5.0        dplyr_1.1.2         
    ## [22] purrr_1.0.1          readr_2.1.3          tidyr_1.3.0         
    ## [25] tibble_3.2.1         ggplot2_3.4.2        tidyverse_1.3.2     
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] fs_1.5.2            lubridate_1.8.0     httr_1.4.4         
    ##  [4] rprojroot_2.0.3     tools_4.2.1         backports_1.4.1    
    ##  [7] utf8_1.2.3          R6_2.5.1            DBI_1.1.3          
    ## [10] colorspace_2.1-0    withr_2.5.0         tidyselect_1.2.0   
    ## [13] gridExtra_2.3       compiler_4.2.1      cli_3.6.1          
    ## [16] rvest_1.0.3         pacman_0.5.1        xml2_1.3.3         
    ## [19] scales_1.2.1        digest_0.6.31       rmarkdown_2.17     
    ## [22] pkgconfig_2.0.3     htmltools_0.5.5     dbplyr_2.2.1       
    ## [25] fastmap_1.1.0       highr_0.9           rlang_1.1.1        
    ## [28] readxl_1.4.1        rstudioapi_0.14     farver_2.1.1       
    ## [31] generics_0.1.3      jsonlite_1.8.3      googlesheets4_1.0.1
    ## [34] magrittr_2.0.3      Rcpp_1.0.10         munsell_0.5.0      
    ## [37] fansi_1.0.4         viridis_0.6.2       lifecycle_1.0.3    
    ## [40] stringi_1.7.12      yaml_2.3.6          grid_4.2.1         
    ## [43] ggrepel_0.9.3       crayon_1.5.2        graphlayouts_0.8.4 
    ## [46] haven_2.5.1         hms_1.1.2           pillar_1.9.0       
    ## [49] codetools_0.2-18    reprex_2.0.2        glue_1.6.2         
    ## [52] evaluate_0.17       modelr_0.1.9        vctrs_0.6.3        
    ## [55] tzdb_0.3.0          tweenr_2.0.2        cellranger_1.1.0   
    ## [58] gtable_0.3.3        polyclip_1.10-4     assertthat_0.2.1   
    ## [61] xfun_0.34           ggforce_0.4.1       googledrive_2.0.0  
    ## [64] viridisLite_0.4.2   gargle_1.2.1        ellipsis_0.3.2
