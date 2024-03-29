
# setup -------------------------------------------------------------------

rm(list = ls(all.names = TRUE))
source(here::here("code/library.R"))
source(here::here("code/set_figure_theme.R"))


# figure ------------------------------------------------------------------

## sample matrix
m_x <- rbind(seq(0, 100, length = 100),
             rep(50, 100),
             rep(10, 100))

## simulation scenarios
df_param <- tibble(e = 1, # to conv_eff
                   a_bc = 0.5, # to attack_rate
                   a_bp = c(0, 0.02, 0.04), # to attack_rate
                   a_cp = c(0.0025, 0.025, 0.0025), # to attack_rate
                   h = c(0.5, 0.5, 0.75), # to handling_time
                   s = c(0, 1, 1)) %>% 
  as_tibble() %>% 
  mutate(omn = case_when(a_bp == 0 ~ "Chain",
                         a_bp == 0.02 ~ "Weak",
                         a_bp == 0.04 ~ "Strong"),
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
       y = expression("Contribution to predator"~~(delta)),
       linetype = "Omnivory")

print(g_delta)