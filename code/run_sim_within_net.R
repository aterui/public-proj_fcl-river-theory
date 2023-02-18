
# setup -------------------------------------------------------------------

rm(list = ls())
source(here::here("code/library.R"))

ncore <- detectCores() - 2
cl <- makeCluster(ncore)
registerDoSNOW(cl)

# set parameters ----------------------------------------------------------

# igpsim parameters
df_param <- expand.grid(mean_disturb_source = 0.8,
                        phi_disturb = 1000,
                        sd_disturb_source = c(0.01, 1),
                        sd_disturb_lon = c(0.01, 1),
                        
                        # carring capacity
                        base_k = 100,
                        z = 0.54, # Finlay 2011 Ecosphere
                        
                        # timestep
                        n_timestep = 1000,
                        n_warmup = 200,
                        n_burnin = 400,
                        
                        # food web parameter
                        r_b = 8,
                        e = 1, # to conv_eff
                        a_bc = 0.5, # to attack_rate[1]
                        a_bp = c(0, 0.02, 0.04), # to attack_rate[2]
                        a_cp = 0.02, # to attack_rate[3]
                        h = 0.5, # to handling_time
                        s = c(0, 1),
                        p_disturb = 0.1,
                        p_dispersal = 0.01,
                        theta = 0.1) %>% 
  as_tibble() %>% 
  filter(sd_disturb_source > sd_disturb_lon,
         !(s == 1 & a_bp == 0), # remove s = 1 for chain scenario
         !(s == 0 & a_bp != 0), # remove s = 0 for omnivory scenarios
         !(p_disturb == 0 & mean_disturb_source == 0.2)) %>% 
  arrange(p_disturb,
          theta) %>% 
  mutate(param_set = seq_len(nrow(.))) %>% 
  relocate(param_set,
           p_disturb,
           theta)

# geometry parameters
df_net <- tibble(n_patch = c(50, 100, 150),
                 p_branch = c(0.2, 0.5, 0.8)) %>% 
  expand(n_patch,
         p_branch)


# run simulation ----------------------------------------------------------

pb <- txtProgressBar(max = nrow(df_param), style = 3)
fun_progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = fun_progress)

tic()
result <- foreach(x = iter(df_param, by = 'row'),
                  .combine = "bind_rows",
                  .packages = c("foreach", "dplyr", "mcbrnet", "tidyr"),
                  .options.snow = opts) %dopar% {
                  
                    foreach(j = 1:nrow(df_net)) %do% {
                      # network generation
                      net <- brnet(n_patch = df_net$n_patch[j],
                                   p_branch = df_net$p_branch[j],
                                   mean_disturb_source = x$mean_disturb_source,
                                   sd_disturb_source = x$sd_disturb_source,
                                   sd_disturb_lon = x$sd_disturb_lon,
                                   plot = FALSE)
                      
                      v_between <- net$adjacency_matrix %>%
                        igraph::graph.adjacency() %>%
                        igraph::betweenness(normalized = T)
                      
                      # patch attributes
                      v_k <- x$base_k * net$df_patch$n_patch_upstream^x$z
                      v_i_disturb <- net$df_patch$disturbance
                      
                      dyn <- igpsim(n_patch = df_net$n_patch[j],
                                    n_warmup = x$n_warmup,
                                    n_burnin = x$n_burnin,
                                    n_timestep = x$n_timestep,
                                    r_b = x$r_b,
                                    conv_eff = x$e,
                                    attack_rate = c(x$a_bc,
                                                    x$a_bp,
                                                    x$a_cp),
                                    handling_time = x$h,
                                    s = x$s,
                                    carrying_capacity = v_k,
                                    p_disturb = x$p_disturb,
                                    i_disturb = v_i_disturb,
                                    phi_disturb = x$phi_disturb,
                                    p_dispersal = x$p_dispersal,
                                    theta = x$theta,
                                    distance_matrix = net$distance_matrix)
                      
                      dyn$df_patch <- dyn$df_patch %>% 
                        mutate(betweenness = v_between) %>% 
                        mutate(x,
                               df_net[j, ])
                      
                      return(dyn$df_patch)
                    }
                    
                  }
toc()

# return ------------------------------------------------------------------

stopCluster(cl)
saveRDS(result,
        file = here::here("output/sim_within_net.rds"))
