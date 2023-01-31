
# setup -------------------------------------------------------------------

rm(list = ls(all.names = TRUE))
source(here::here("code/library.R"))
source(here::here("code/figure_theme.R"))


# figure ------------------------------------------------------------------

## sample matrix
m_x <- rbind(seq(0, 100, length = 100),
             rep(50, 100),
             rep(10, 100))

## simulation scenarios
df_param <- expand.grid(e = 1, # to conv_eff
                        a_bc = 0.025, # to attack_rate
                        a_bp = c(0, 0.025, 0.5), # to attack_rate
                        a_cp = 0.025, # to attack_rate
                        h = 0.5, # to handling_time
                        s = 1) %>% 
  as_tibble() %>% 
  mutate(omn = case_when(a_bp == 0 ~ "Chain",
                         a_bp == 0.025 ~ "Weak",
                         a_bp == 0.5 ~ "Strong"),
         omn = factor(omn,
                      levels = c("Chain",
                                 "Weak",
                                 "Strong")))

## calc delta values
df_m <- foreach(i = seq_len(nrow(df_param)),
                .combine = bind_rows) %do% {
                  
                  list_igp <- fun_igp(m_x,
                                      e = rep(df_param$e[i], 3),
                                      a = c(df_param$a_bc[i],
                                            df_param$a_bp[i],
                                            df_param$a_cp[i]),
                                      h = rep(df_param$h[i], 3),
                                      s = df_param$s[i])
                  
                  df_delta <- list_igp$delta %>% 
                    as_tibble() %>% 
                    rename(delta = value) %>% 
                    mutate(x = m_x[1,],
                           omn = df_param$omn[i])
                  
                  return(df_delta)
                  
                }


# figure ------------------------------------------------------------------

theme_set(plt_theme)

g_delta <- df_m %>% 
  ggplot(aes(x = x,
             y = delta,
             linetype = omn)) +
  geom_line(color = grey(0.4)) +
  labs(x = "Basal species density",
       y = expression("Proportional contribution to predator"~~(delta)),
       linetype = "Omnivory")
