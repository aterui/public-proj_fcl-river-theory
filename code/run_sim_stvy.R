
# setup -------------------------------------------------------------------

rm(list = ls(all.names = TRUE))
source(here::here("code/library.R"))

cl <- makeCluster(detectCores())
registerDoSNOW(cl)

# set parameters ----------------------------------------------------------

# igpsim parameters
n_para <- 500
df_param <- tibble(# parameters for brnet
                   mean_disturb_source = 0.8,
                   sd_disturb_source = runif(n_para, 0.05, 5),
                   sd_disturb_lon = runif(n_para, 0.05, 5),
                   
                   # carring capacity
                   base_k = 100,
                   z = 0.54, # Finlay 2011 Ecosphere
                   
                   # parameters for igpsim
                   n_timestep = 1000,
                   n_warmup = 200,
                   n_burnin = 400,
                   r_b = runif(n_para, 1, 10),
                   e_bc = runif(n_para, 1, 10), # to conv_eff[1]
                   e_bp = runif(n_para, 1, 10), # to conv_eff[2]
                   e_cp = runif(n_para, 1, 10), # to conv_eff[3]
                   a_bc = runif(n_para, 0.05, 0.5), # to attack_rate[1]
                   a_bp = runif(n_para, 0.05, 0.5), # to attack_rate[2]
                   a_cp = runif(n_para, 0.05, 0.5), # to attack_rate[3]
                   h_bc = runif(n_para, 0.5, 5), # to handling_time[1]
                   h_bp = runif(n_para, 0.5, 5), # to handling_time[2]
                   h_cp = runif(n_para, 0.5, 5), # to handling_time[3]
                   s0 = runif(n_para, 0.5, 1),
                   p_disturb = runif(n_para, 0, 0.2),
                   p_dispersal = runif(n_para, 0, 0.1),
                   theta = runif(n_para, 0.01, 1)) %>% 
  mutate(param_set = seq_len(nrow(.)))

# geometry parameters
n_rep <- 100
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

result <- foreach(x = iter(df_param, by = 'row'),
                  .combine = "bind_rows",
                  .packages = c("foreach", "dplyr", "mcbrnet"),
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
                                        v_m_disturb <- net$df_patch$disturbance
                                        
                                        dyn <- igpsim(n_patch = n_patch[j],
                                                      n_warmup = x$n_warmup,
                                                      n_burnin = x$n_burnin,
                                                      n_timestep = x$n_timestep,
                                                      r_b = x$r_b,
                                                      conv_eff = c(x$e_bc,
                                                                   x$e_bp,
                                                                   x$e_cp),
                                                      attack_rate = c(x$a_bc,
                                                                      x$a_bp,
                                                                      x$a_cp),
                                                      handling_time = c(x$h_bc,
                                                                        x$h_bp,
                                                                        x$h_cp),
                                                      s0 = rep(x$s0, 3),
                                                      carrying_capacity = v_k,
                                                      p_disturb = x$p_disturb,
                                                      m_disturb = v_m_disturb,
                                                      p_dispersal = x$p_dispersal,
                                                      theta = x$theta,
                                                      distance_matrix = net$distance_matrix)
                                        
                                        df <- tibble(n_rep = j,
                                                     mc_capacity = sum(v_k),
                                                     n_patch = n_patch[j],
                                                     p_branch = p_branch[j],
                                                     x,
                                                     fcl = mean(dyn$df_patch$fcl))
                                        
                                        return(df)
                                      }
                    return(df_set)
                  }

# return ------------------------------------------------------------------

stopCluster(cl)

sim_stvy_result <- result
save(sim_stvy_result, file = here::here("output/result_stvy.RData"))
