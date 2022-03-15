
# setup -------------------------------------------------------------------

rm(list = ls())
source(here::here("code/library.R"))
source(here::here("code/analysis_stvy.R"))


# table -------------------------------------------------------------------

term_label <- c("Intercept",
                "Disturbance prob.",
                "Disturbance variation at headwaters",
                "Local disturbance variation",
                "Reproductive rate of basal species",
                "Conversion efficiency (B to C)",
                "Conversion efficiency (B to P)",
                "Conversion efficiency (C to P)",
                "Attack rate (B to C)",
                "Attack rate (B to P)",
                "Attack rate (C to P)",
                "Handling time (B to C)",
                "Handling time (B to P)",
                "Handling time (C to P)",
                "Survival prob.",
                "Dispersal prob.",
                "Inverse of mean dispersal distance")

param_label <- c("",
                 "$p_{m}$",
                 "$\\sigma_{s}$",
                 "$\\sigma_{l}$",
                 "$r_b$",
                 "$e_{bc}$",
                 "$e_{bp}$",
                 "$e_{cp}$",
                 "$a_{bc}$",
                 "$a_{bp}$",
                 "$a_{cp}$",
                 "$h_{bc}$",
                 "$h_{bp}$",
                 "$h_{cp}$",
                 "$s_{0}$",
                 "$p_{d}$",
                 "$\\theta$")

table_stvy <- foreach(i = 1:2) %do% {
  fit_sense$fit[[i]] %>% 
    tidy() %>%
    mutate(Term = param_label,
           Interpretation = term_label) %>% 
    select(Term,
           Interpretation,
           Estimate =estimate,
           SE = std.error) %>% 
    kable(format = "markdown",
          digits = 3)
}
