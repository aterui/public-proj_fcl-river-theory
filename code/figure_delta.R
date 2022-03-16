
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
df_param <- expand.grid(e_bc = 4, # to conv_eff[1]
                        e_bp = c(0, 2, 4), # to conv_eff[2]
                        e_cp = c(2, 4), # to conv_eff[3]
                        a_bc = 0.5, # to attack_rate[1]
                        a_bp = c(0.1, 0.5), # to attack_rate[2]
                        a_cp = c(0.1, 0.5), # to attack_rate[3]
                        h_bc = 0.5, # to handling_time[1]
                        h_bp = c(0.5, 5), # to handling_time[2]
                        h_cp = c(0.5, 5) # to handling_time[3]
) %>% 
  as_tibble() %>% 
  filter(e_bp == 0 & a_bp == 0.1 & h_bp == 5 &
           e_cp == 4 & a_cp == 0.5 & h_cp == 0.5 |
           
           e_bp == 2 & a_bp == 0.1 & h_bp == 5 &
           e_cp == 4 & a_cp == 0.5 & h_cp == 0.5 |
           
           e_bp == 4 & a_bp == 0.5 & h_bp == 0.5 &
           e_cp == 2 & a_cp == 0.1 & h_cp == 5) %>% 
  mutate(omn = case_when(e_bp == 0 ~ "Chain",
                         e_bp == 2 ~ "Weak",
                         e_bp == 4 ~ "Strong"),
         omn = factor(omn,
                      levels = c("Chain",
                                 "Weak",
                                 "Strong")))

## calc delta values
df_m <- foreach(i = seq_len(nrow(df_param)),
                .combine = bind_rows) %do% {
                  
                  list_igp <- fun_igp(m_x,
                                      e = c(df_param$e_bc[i],
                                            df_param$e_bp[i],
                                            df_param$e_cp[i]),
                                      a = c(df_param$a_bc[i],
                                            df_param$a_bp[i],
                                            df_param$a_cp[i]),
                                      h = c(df_param$h_bc[i],
                                            df_param$h_bp[i],
                                            df_param$h_cp[i]))
                  
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
       y = expression("Preference to basal over IG-prey"~~(delta)),
       linetype = "Omnivory") +
  ggtitle("B")
