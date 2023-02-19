
# setup -------------------------------------------------------------------

rm(list = ls())
source(here::here("code/library.R"))

ncore <- detectCores() - 2
cl <- makeCluster(ncore)
registerDoSNOW(cl)

# param -------------------------------------------------------------------

df_param <- expand.grid(n_timestep = 1000,
                        n_warmup = 200,
                        n_burnin = 400,
                        r_b = seq(4, 16, length = 4),
                        k = c(100, 1000),
                        e = 1, # to conv_eff[1]
                        a1 = c(0.25, 0.5),
                        a2 = seq(0, 0.1, length = 20),
                        a3 = seq(0, 0.05, length = 20),
                        h = seq(0, 1, by = 0.25),
                        s = 1) %>%
  mutate(param_set = seq_len(nrow(.))) %>% 
  as_tibble()

# run simulation ----------------------------------------------------------
tic()
df_fcl <- foreach(i = 1:nrow(df_param),
                  .combine = bind_rows,
                  .packages = "dplyr") %dopar%{
                    
                    x <- df_param[i,]
                    dyn <- suppressMessages(mcbrnet::igpsim(n_patch = 10,
                                                            n_warmup = x$n_warmup,
                                                            n_burnin = x$n_burnin,
                                                            n_timestep = x$n_timestep,
                                                            r_b = x$r_b,
                                                            conv_eff = x$e,
                                                            attack_rate = c(x$a1,
                                                                            x$a2,
                                                                            x$a3),
                                                            handling_time = x$h,
                                                            s = x$s,
                                                            carrying_capacity = x$k,
                                                            p_disturb = 0,
                                                            i_disturb = 0,
                                                            p_dispersal = 0))
                    
                    state <- dyn$df_dynamics %>% 
                      group_by(species, patch_id) %>% 
                      slice(which.max(timestep)) %>% 
                      ungroup() %>% 
                      mutate(y0 = as.numeric(abundance > 0),
                             lambda = case_when(species != "ig-predator" ~ 1,
                                                species == "ig-predator" ~ 2),
                             y = y0 * lambda) %>%
                      group_by(patch_id) %>% 
                      summarize(state = sum(y)) %>% 
                      pull(state)
                      
                    re <- dplyr::tibble(x,
                                        fcl = dyn$df_patch$fcl,
                                        state = state)
                    
                    return(re)                 
                  }
toc()

# return ------------------------------------------------------------------

stopCluster(cl)
saveRDS(df_fcl, file = here::here("output/sim_one_patch.rds"))
