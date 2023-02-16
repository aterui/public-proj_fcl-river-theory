
# setup -------------------------------------------------------------------

rm(list = ls())
source(here::here("code/library.R"))

ncore <- detectCores() - 2
cl <- makeCluster(ncore)
registerDoSNOW(cl)

# set parameters ----------------------------------------------------------

# igpsim parameters
df_param <- expand.grid(mean_disturb_source = c(0.2, 0.8),
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
                        r_b = seq(2, 20, length = 4),
                        e = 1, # to conv_eff
                        a_bc = 0.5, # to attack_rate[1]
                        a_bp = c(0, 0.02, 0.04), # to attack_rate[2]
                        a_cp = c(0.02), # to attack_rate[3]
                        h = 0.5, # to handling_time
                        s = c(0, 1),
                        p_disturb = seq(0, 0.15, length = 4),
                        p_dispersal = 0.01,
                        theta = c(0.1, 1)) %>% 
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
n_rep <- 500

repeat {
  n_patch <- round(runif(n_rep, 10, 150))
  p_branch <- runif(n_rep, 0.01, 0.99)
  
  if(min(n_patch) < 15 & 
     max(n_patch) > 145 &
     min(p_branch) < 0.05 &
     max(p_branch) > 0.95) break    
}

# run simulation ----------------------------------------------------------

pb <- txtProgressBar(max = nrow(df_param), style = 3)
fun_progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = fun_progress)

tic()
result <- foreach(x = iter(df_param, by = 'row'),
                  .combine = "bind_rows",
                  .packages = c("foreach", "dplyr", "mcbrnet", "tidyr"),
                  .options.snow = opts) %dopar% {
                    
                    df_set <- foreach(j = seq_len(n_rep),
                                      .combine = "bind_rows") %do% {
                                        
                                        # network generation
                                        net <- brnet(n_patch = n_patch[j],
                                                     p_branch = p_branch[j],
                                                     mean_disturb_source = x$mean_disturb_source,
                                                     sd_disturb_source = x$sd_disturb_source,
                                                     sd_disturb_lon = x$sd_disturb_lon,
                                                     plot = FALSE)
                                        
                                        # patch attributes
                                        v_k <- x$base_k * net$df_patch$n_patch_upstream^x$z
                                        v_i_disturb <- net$df_patch$disturbance
                                        
                                        dyn <- igpsim(n_patch = n_patch[j],
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
                                        
                                        df_occ <- dyn$df_dynamics %>%
                                          group_by(species) %>% 
                                          summarize(p = mean(abundance > 0))
                                        
                                        df_state <- dyn$df_dynamics %>% 
                                          mutate(one = as.numeric(abundance > 0)) %>% 
                                          pivot_wider(id_cols = c(patch_id, timestep),
                                                      values_from = one,
                                                      names_from = species) %>% 
                                          mutate(state = case_when(basal == 1 & `ig-prey` == 1 & `ig-predator` == 1 ~ 4,
                                                                   basal == 1 & `ig-prey` == 0 & `ig-predator` == 1 ~ 3,
                                                                   basal == 1 & `ig-prey` == 1 & `ig-predator` == 0 ~ 2,
                                                                   basal == 1 & `ig-prey` == 0 & `ig-predator` == 0 ~ 1,
                                                                   basal == 0 & `ig-prey` == 0 & `ig-predator` == 0 ~ 0)) %>% 
                                          drop_na(state) %>% 
                                          group_by(state) %>% 
                                          tally() %>% 
                                          mutate(p = n / sum(n)) %>% 
                                          full_join(tibble(state = 0:4),
                                                    by = "state") %>% 
                                          mutate(n = replace_na(n, 0),
                                                 p = replace_na(p, 0)) %>% 
                                          arrange(state)
                                        
                                        df <- tibble(n_rep = j,
                                                     mc_capacity = sum(v_k),
                                                     n_patch = n_patch[j],
                                                     p_branch = p_branch[j],
                                                     x,
                                                     fcl = mean(dyn$df_patch$fcl),
                                                     p_basal = df_occ %>%
                                                       filter(species == "basal") %>% 
                                                       pull(p),
                                                     p_igprey = df_occ %>%
                                                       filter(species == "ig-prey") %>% 
                                                       pull(p),
                                                     p_igpred = df_occ %>%
                                                       filter(species == "ig-predator") %>% 
                                                       pull(p),
                                                     s0 = df_state %>% 
                                                       filter(state == 0) %>% 
                                                       pull(p),
                                                     s1 = df_state %>% 
                                                       filter(state == 1) %>% 
                                                       pull(p),
                                                     s2 = df_state %>% 
                                                       filter(state == 2) %>% 
                                                       pull(p),
                                                     s3 = df_state %>% 
                                                       filter(state == 3) %>% 
                                                       pull(p),
                                                     s4 = df_state %>% 
                                                       filter(state == 4) %>% 
                                                       pull(p))
                                        
                                        return(df)
                                      }
                    return(df_set)
                  }
toc()

# return ------------------------------------------------------------------

stopCluster(cl)
saveRDS(result,
        file = here::here("output/sim_main.rds"))
