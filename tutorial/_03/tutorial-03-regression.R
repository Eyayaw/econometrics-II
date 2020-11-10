
library(dplyr)

ezunem <- haven::read_dta("http://fmwww.bc.edu/ec-p/data/wooldridge/ezunem.dta")

# first differencing to kill time invariant fixed effects

first_diff <- function(x, order_by = NULL) {
  x - dplyr::lag(x, 1L, order_by = order_by)
}

# transform the variables into first differences

ezunem_fd <- ezunem %>%
  group_by(city) %>%
  mutate(year_t = row_number()) %>% # create a year_t variable
  mutate_at(vars(-c(year, city, guclms, cez)), first_diff)

# a) first difference

# run the regression, d81 not included (otherwise singularity would arise) and is a reference year
fd_model <- lm(luclms ~ ez + d82 + d83 + d84 + d85 + d86 + d87 + d88,
  data = ezunem_fd
)

# since guclms and cez are given as first differences of luclms and ez, we can we could have run this way too:
# lm(formula = guclms ~ cez + d81 + d82 + d83 + d84 + d85 + d86 + d87, data = ezunem_fd)

# b) first difference with interaction terms (alpha_i * year_t)
# let's create an interaction term between alpha_i and year_t, we do not need to first difference it because year_t is already in first difference.
ezunem_fd <- ezunem_fd %>%
  mutate(cityear = city * year_t)

fd_model_interaction <- lm(luclms ~ ez + cityear, data = ezunem_fd)

# c) first difference with interaction terms (alpha_i * year_t) and year dummies

fd_model_interaction_withdummies <-
  lm(luclms ~ ez + cityear + d82 + d83 + d84 + d85 + d86 + d87 + d88,
    data = ezunem_fd
  )

lapply(list(fd_model, fd_model_interaction, fd_model_interaction_withdummies), summary)
