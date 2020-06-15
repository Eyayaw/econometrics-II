## ---- load-packages---------------------------------
library(dplyr)
library(stargazer)


## ---- read-in data----------------------------------

# url <- "https://www.stata.com/data/jwooldridge/eacsap/nls80.dta"
# download.file(url, "nls80.dta", quiet = TRUE)

nls_80 <- haven::read_dta("nls80.dta") # read in `.dta` data

# get the labels of the variables, `Label` in Stata.
var_labels <- sapply(nls_80, function(x) attr(x, "label"))


# tibble::glimpse(nls_80) # or head(nls_80, 5) to view the first 5 rows


## ---- description of variables, results="asis"------
nms <- colnames(nls_80)

var_labels <- tibble::tibble(
  "Variable name" = nms,
  "Variable label" = var_labels,
  row.names = NULL
)


## latex table -----------------------

stargazer(as.data.frame(nls_80, stringsAsFactors = FALSE),
  title = "Descriptive statistics",
  header = FALSE, no.space = TRUE, align = TRUE,
  label = "tab:ds", summary.logical = TRUE
)

# make married, black, south, and urban explicit dummy/factor
nls_80 <- mutate_at(nls_80, vars(married:urban), as.factor)


## ---- distribution of wage, fig.cap="Distribution of monthly earnings"----
hist(nls_80$wage, xlab = "wage (monthly earnings)", main = NULL)


## ---- marital-status--------------------------------
nls_80_married <- subset(nls_80, married == 1)

nls_80_unmarried <- subset(nls_80, married == 0)


## ---- create-dummies-----------------

# high_iq = 1 if iq is > the median value of iq, 0 otherwise. Then, we can give it optional labels.
nls_80 <- mutate(nls_80,
  high_iq = ifelse(iq > median(iq), 1, 0),
  high_iq = factor(high_iq, c(0, 1), c("low", "high"))
)

# education dummies
# medium vs high; let's cut it at 13 years of education---high school completed?
nls_80 <- mutate(nls_80,
  educ_dummy = case_when(
    educ < 13 ~ 0,
    educ == 13 ~ 1,
    educ > 13 ~ 2
  ),
  educ_dummy = factor(educ_dummy,
    levels = c(0, 1, 2),
    labels = c("low", "medium", "high")
  ),
  medium_educ = ifelse(educ == 13, 1, 0),
  high_educ = ifelse(educ > 13, 1, 0),
  low_educ = ifelse(educ < 13, 1, 0)
)

contrasts(nls_80$educ_dummy) # one var with 3 categories can be represented in two dummies

## simple ols -----------------

eqn.1 <- substitute(wage ~ married + high_iq + high_educ + medium_educ + low_educ + exper + age + tenure + feduc)

mod.1 <- lm(eqn.1, nls_80)

# drops low_educ because of singularity, as one is a linear combination of the other. See cor(nls_80[, c("high_educ", "medium_educ", "low_educ")])

eqn.2 <- substitute(wage ~ married + high_iq + high_educ + exper + age + tenure + feduc) # we drop medium_educ

mod.2 <- lm(eqn.2, nls_80)


eqn.3 <- substitute(wage ~ married + high_iq + married:high_iq + high_educ + exper + age + tenure + feduc)

mod.3 <- lm(eqn.3, nls_80)

## latex table -----------------------
stargazer(mod.1, mod.2, mod.3,
  title = "Regression output: level-level",
  dep.var.labels = "wage", font.size = "small",
  omit.stat = c("aic", "bic"), header = FALSE,
  no.space = TRUE, align = TRUE
)


## log wage and log rhs vars------------------------------------
eqn.4 <- substitute(lwage ~ married + educ + exper + I(exper**2) + age + tenure + feduc)

eqn.5 <- substitute(lwage ~ married + log(educ) + exper + I(exper**2) + age + tenure + feduc)

eqn.6 <- substitute(wage ~ married + log(educ) + exper + I(exper**2) + age + tenure + feduc)

mod456 <- list(eqn.4, eqn.5, eqn.6)
names(mod456) <- paste0("mod.", 4:6)

mod456.result <- lapply(mod456, function(x) lm(x, data = nls_80))

## latex table -----------------------
stargazer(mod456.result,
  font.size = "small",
  title = "Regression output: log-level, log-log and level-log",
  dep.var.labels = c("ln(wage)", "wage"),
  omit.stat = c("aic", "bic"), header = FALSE,
  no.space = TRUE, align = TRUE
)
