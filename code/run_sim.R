
# setup -------------------------------------------------------------------

rm(list = ls())
source(here::here("code/library.R"))

ncore <- detectCores() - 2
cl <- makeCluster(ncore)
registerDoSNOW(cl)

# set parameters ----------------------------------------------------------

# igpsim parameters
df_param <- expand.grid(mean_disturb_source = c(0.2, 0.8),
                        sd_disturb_source = c(0.01, 1),
                        sd_disturb_lon = c(1, 0.01),
                        
                        # carring capacity
                        base_k = 500,
                        z = 0.54, # Finlay 2011 Ecosphere
                        
                        n_timestep = 1000,
                        n_warmup = 200,
                        n_burnin = 400,
                        r_b = c(5, 10),
                        e = 1, # to conv_eff[3]
                        a_bc = c(0.025), # to attack_rate[1]
                        a_bp = c(0, 0.025, 0.5), # to attack_rate[2]
                        a_cp = c(0.025), # to attack_rate[3]
                        h = c(0.75, 1.5), # to handling_time[3]
                        s = c(0, 1),
                        p_disturb = seq(0, 0.1, length = 5),
                        p_dispersal = 0.01,
                        theta = c(0.1, 1)) %>% 
  as_tibble() %>% 
  filter(sd_disturb_source > sd_disturb_lon,
         !(p_disturb == 0 & mean_disturb_source == 0.2)) %>% 
  arrange(p_disturb,
          theta) %>% 
  mutate(param_set = seq_len(nrow(.))) %>% 
  relocate(param_set,
           p_disturb,
           theta)


# geometry parameters
n_rep <- 250
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
                                                      conv_eff = x$e,
                                                      attack_rate = c(x$a_bc,
                                                                      x$a_bp,
                                                                      x$a_cp),
                                                      handling_time = x$h,
                                                      s = x$s,
                                                      carrying_capacity = v_k,
                                                      p_disturb = x$p_disturb,
                                                      m_disturb = v_m_disturb,
                                                      p_dispersal = x$p_dispersal,
                                                      theta = x$theta,
                                                      distance_matrix = net$distance_matrix)
                                        
                                        df_occ <- dyn$df_dynamics %>%
                                          group_by(species) %>% 
                                          summarize(p = mean(abundance > 0))
                                        
                                        df <- tibble(n_rep = j,
                                                     mc_capacity = sum(v_k),
                                                     n_patch = n_patch[j],
                                                     p_branch = p_branch[j],
                                                     x,
                                                     fcl = mean(dyn$df_patch$fcl),
                                                     p_basal = df_occ$p[1],
                                                     p_igprey = df_occ$p[2],
                                                     p_igpred = df_occ$p[3])
                                        
                                        return(df)
                                      }
                    return(df_set)
                  }

# return ------------------------------------------------------------------

stopCluster(cl)
saveRDS(result,
        file = here::here("output/sim_main.rds"))
