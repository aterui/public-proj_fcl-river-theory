
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
                        r_b = c(2, 20),
                        k = c(100, 500),
                        e = 1, # to conv_eff[1]
                        a = seq(0.001, 0.5, length = 30),
                        h = seq(0, 2, length = 30),
                        s = c(0, 0.5)) %>%
  mutate(param_set = seq_len(nrow(.))) %>% 
  as_tibble()

# run simulation ----------------------------------------------------------
tic()
df_fcl <- foreach(i = 1:nrow(df_param),
                  .combine = bind_rows) %dopar%{
                    
                    x <- df_param[i,]
                    dyn <- suppressMessages(mcbrnet::igpsim(n_patch = 10,
                                                            n_warmup = x$n_warmup,
                                                            n_burnin = x$n_burnin,
                                                            n_timestep = x$n_timestep,
                                                            r_b = x$r_b,
                                                            conv_eff = x$e,
                                                            attack_rate = x$a,
                                                            handling_time = x$h,
                                                            s = x$s,
                                                            carrying_capacity = x$k,
                                                            p_disturb = 0,
                                                            m_disturb = 0,
                                                            p_dispersal = 0))
                    
                    re <- dplyr::tibble(x, fcl = dyn$df_patch$fcl)
                    
                    return(re)                 
                  }
toc()

# return ------------------------------------------------------------------

stopCluster(cl)
saveRDS(df_fcl, file = here::here("output/sim_one_patch.rds"))
