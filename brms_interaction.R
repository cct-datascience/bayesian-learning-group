data(rugged, package = "rethinking")
d <- rugged
rm(rugged)

# may as well load this, too
library(brms)
library(dplyr)

# make the log version of criterion
d <- 
  d %>%
  mutate(log_gdp = log(rgdppc_2000))

# extract countries with GDP data
dd <-
  d %>%
  filter(complete.cases(rgdppc_2000)) %>% 
  # re-scale variables
  mutate(log_gdp_std = log_gdp / mean(log_gdp), 
         rugged_std  = rugged / max(rugged))

dd <-
  dd %>%
  mutate(rugged_std_c  = rugged_std - mean(rugged_std))

dd <- 
  dd %>% 
  mutate(cid = if_else(cont_africa == 1, "1", "2"))

b8.3 <- 
  brm(data = dd, 
      family = gaussian,
      bf(log_gdp_std ~ 0 + a + b * rugged_std_c, 
         a ~ 0 + cid, 
         b ~ 0 + cid,
         nl = TRUE),
      prior = c(prior(normal(1, 0.1), class = b, coef = cid1, nlpar = a),
                prior(normal(1, 0.1), class = b, coef = cid2, nlpar = a),
                prior(normal(0, 0.3), class = b, coef = cid1, nlpar = b),
                prior(normal(0, 0.3), class = b, coef = cid2, nlpar = b),
                prior(exponential(1), class = sigma)),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      seed = 8)


library(ratdat)
library(dplyr)
library(ggplot2)
library(ggdist)
library(brms)
library(tidybayes)
library(tidyr)
theme_set(theme_minimal())

complete_annual <- ratdat::complete |>
  group_by(year, species_id) |>
  filter(species_id %in% c("DS", "PP")) |>
  tally() |>
  ungroup() |>
  mutate(n = scale(n))

ggplot(complete_annual, aes(year, n, color = species_id)) +
  geom_point()

ratmod <- 
  brm(data = complete_annual, 
      family = gaussian,
      bf(n ~ 0 + a + b * year, 
         a ~ 0 + species_id,
         b ~ 0 + species_id,
         nl = TRUE),
      iter = 4000, warmup = 1000, chains = 4, cores = 4,
      seed = 8,
      file = "ratmod")

summary(ratmod)
plot(ratmod)

ratmod_predictions <- predicted_draws(ratmod, newdata = complete_annual,ndraws = 50)

ggplot(ratmod_predictions, aes(year, .prediction, color = species_id)) +
  geom_point()
