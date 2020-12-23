#' ---
#' title: "IV Estimation: Strangetown"
#' author: Eyayaw Beze
#' date: '`r format.Date("2020/07/11", "%B %d, %Y")`'
#' output: pdf_document
#' colorlinks: yes
#' ---
#'
#' Let $y_i$ be a measure of good health, and
#'  $D_i$ an indicator for smoking
#'  $$ D_i = \left\{
#'  \begin{array}{lr}
#'  1, \quad if \;i \; smokes\\
#'  0, \quad otherwise
#'  \end{array}
#'  \right.
#'  $$
#'
#'
#' $$ Z_i = \left\{
#'  \begin{array}{lr}
#'  1, \quad if \;i \; \text{received a pack of cigarettes}\\
#'  0, \quad otherwise
#'  \end{array}
#'  \right.
#'  $$
#'
#'
#'  The true model reads:
#'  \begin{equation}
#'  y_{i}=\alpha+\beta_{1}^{\text {true}} D_{i}+\beta_{2} w_{i}+\epsilon_{i}\label{eq:1}
#'  \end{equation}
#'  with $w_{i}$ denoting individual income. But since you do not observe income you can only estimate the following model:
#'  \begin{equation}
#'  y_{i}=\alpha+\beta_{1} D_{i}+u_{i}\label{eq:2}
#'  \end{equation}
#'  a) Over- or under-estimate $\beta_{1}^{\text {true}}$ if we don't observe wage in (\ref{eq:1})? Show the inconsistency of the coefficient if we estimate (\ref{eq:2}) instead?
#'
#+ setup, include = FALSE, echo = FALSE
knitr::opts_chunk$set(comment = "#>", echo = TRUE)

#+ helpers
# data generating process
dgp <- function(n, y.bar, d, z) {
  sigma <- 1 / sqrt(n)
  y <- y.bar + sigma * scale(rnorm(n)) # with fixed mean and sd; 
                                       # mean(y) == y.bar, sd(x) == sigma
  return(data.frame(y, D = rep(d, n), Z = rep(z, n)))
}

# to vectorize over n, y.bar, D, and Z
map_df <- function(..., f, binder = rbind) {
  return(as.data.frame(do.call(binder, Map(f, ...))))
}

#+ data generating process
n <- 70 ## sample size
# Di = Zi = 0
n00 <- 30
y00.bar <- 1.0

# Di = 0; Zi = 1
n01 <- 10
y01.bar <- 0.8
# Di = 1; Zi = 0
n10 <- 20
y10.bar <- 1.5
# Di = Zi = 1
n11 <- 10
y11.bar <- 1.2

n.vec <- c(n00, n01, n10, n11)
y.bar <- c(y00.bar, y01.bar, y10.bar, y11.bar)
D <- c(0, 0, 1, 1)
Z <- c(0, 1, 0, 1)

set.seed(123) # for reproducibility
toy_data <- map_df(n.vec, y.bar, D, Z, f = dgp)

head(toy_data) # view 6 rows of the data

subsets <- alist(
  D == 0 & Z == 0,
  D == 0 & Z == 1,
  D == 1 & Z == 0,
  D == 1 & Z == 1
)

# check whether the mean of the generated data matches the sample means
sapply(subsets, function(x) mean(toy_data[eval(x, toy_data), "y"]))
y.bar

#' c) Calculate $\beta^{OLS}_1$ that you obtain by estimating equation (2) by OLS. Interpret the coefficient.

#' \begin{gather}
#' \begin{aligned}
#' \beta^{OLS}_1 &= \frac{Cov(y_i, D_i)}{Var(D_i)} = \frac{E\left[y_i D_i\right]-E\left[y_i\right] E\left[D_i\right]}{E[D_i^2]-(E[D_i])^2} = \frac{E\left[y_i\mid D_i=1\right]-E\left[y_i\right] E[D_i]}{E[D_i]-(E[D_i])^2} \\
#' & = \frac{\frac{(n_{10}\bar{y}_{10} + n_{11}\bar{y}_{11})}{n} - \frac{(n_{00}\bar{y}_{00} + n_{01}\bar{y}_{01} + n_{10}\bar{y}_{10} + n_{11}\bar{y}_{11})}{n}\frac{(n_{10} + n_{11})}{n}}{(\frac{n_{10} + n_{11}}{n}) - \left(\frac{n_{10} + n_{11}}{n}\right)^2}  \\
#' &= \frac{(1/70)\left(20\cdot1.5 + 10\cdot1.2)\right) - \left[(1/70) \left(30\cdot1.0 + 10\cdot0.8 + 20\cdot1.5 + 10\cdot1.2\right)\right]\left[(1/70)(20 + 10)\right]}{(\frac{20 + 10}{70})-(\frac{20 + 10}{70})^2} \label{eq:3}
#' \end{aligned}
#' \end{gather}
#' Note: $E[D_i] = p$ and $Var(D_i) = p(1-p)$ where p is the probability that $D_i$ takes on 1---**since $\boldsymbol{D_i}$ is a Bernoulli random variable**. In our case, $Pr(D_i=1) = p = \frac{n_{10} + n_{11}}{n_{00} + n_{01} + n_{10} + n_{11}} = (20 + 10) / 70 = 3/7$.
#'
#' # IV Estimation: LATE/Wald
#' By (\ref{eq:3}),
#+ estimation, echo = TRUE

b_ols <- ((1 / n) * (n10 * y10.bar + n11 * y11.bar) -
  ((1 / n) * (n00 * y00.bar + n01 * y01.bar + n10 * y10.bar + n11 * y11.bar) * (n10 + n11) / n
  )) /
  ((n10 + n11) / n - ((n10 + n11) / n)^2)

#' $\hat{\beta}^{OLS}_1=$ `r b_ols`.
#'
#' Or using the `covariance` and `variance` formula:
#+ cov-var-formula
beta_ols <- with(toy_data, cov(y, D) / var(D))
alpha <- with(toy_data, mean(y) - beta_ols * mean(D))
#' $\hat{\beta}^{OLS}_1=$ `r beta_ols` and $\hat\alpha$ = `r alpha`.
#' 
#' Or using `lm`---a linear model estimation workhorse in `R`:
#'
#+ lm
lm(y ~ D, toy_data)$coefficients
#' (d) Calculate ${\beta}^{IV}_1$ and discuss your result w.r.t. to the previous findings.
#' Using The Wald Estimator:
#' $$
#' {\beta}^{IV}_1=\frac{\mathbb{E}\left(Y_{i} \mid z_{i}=1\right)-\mathbb{E}\left(Y_{i} \mid z_{i}=0\right)}{\mathbb{E}\left(D_{i} \mid z_{i}=1\right)-\mathbb{E}\left(D_{i} \mid z_{i}=0\right)}
#' $$


# Wald Estimator 
with(
  toy_data,
  (mean(y[Z == 1]) - mean(y[Z == 0])) / (mean(D[Z == 1]) - mean(D[Z == 0]))
)


#' Using matrix notation:
#'
#' $$
#' \widehat{\boldsymbol{\beta}}_{I V}=\left[\mathbf{Z^\prime} \mathbf{X}\right]^{-1} [\mathbf{Z^\prime} y]
#' $$
#' 
# the [1, 1, 1,...]' is instrumented by itself
with(toy_data, {
  constant <- rep(1, length(D))
  Z.mat <- cbind(constant, Z) # Z.mat = [[1, 1,..., 1]', Z]
  D.mat <- cbind(constant, D) # [[1, 1,..., 1]', D]
  solve(t(Z.mat) %*% D.mat) %*% t(Z.mat) %*% y
})

#' The Wald estimand of $\widehat{\boldsymbol{\beta}}_{I V}$ can be interpreted as the effect of smoking on those whose treatment status can be changed by the instrument. The effect of smoking on health of those who smoked because they were given packs of cigarettes, but would not otherwise have smoked. This obviously excludes voluntary smokers and those who did not, but it includes smokers for whom receiving the cigarettes was important.

#+ message = FALSE, warning = FALSE, results = "asis"
# Or using AER package
library(AER)
library(stargazer)
model_iv <- ivreg(y ~ D | Z, data = toy_data)
stargazer(model_iv, header = FALSE)

#' * **Compliers**. The subpopulation with $d_{1i} = 1$ and $d_{0i} = 0$:
#' * **Always-takers**. The subpopulation with $d_{1i} =d_{0i} = 1$:
#' * **Never-takers**. The subpopulation with $d_{1i} =d_{0i} = 0$:
#'
#' Using the exclusion restriction, we can define potential outcomes indexed solely against treatment status using the single-index ( $\mathrm{Y}_{1 i}, \mathrm{Y}_{0 i}$ ) notation. In particular,
#' \[
#' \begin{aligned}
#' \mathrm{Y}_{1 i} & \equiv \mathrm{Y}_{i}(1,1)=\mathrm{Y}_{i}(1,0) \\
#' \mathrm{Y}_{0 i} & \equiv \mathrm{Y}_{i}(0,1)=\mathrm{Y}_{i}(0,0)
#' \end{aligned}
#' \]
#' The observed outcome, $\mathrm{Y}_{i},$ can therefore be written in terms of potential outcomes as:
#' \[
#' \begin{aligned}
#' \mathrm{Y}_{i} &=\mathrm{Y}_{i}\left(0, \mathrm{Z}_{i}\right)+\left[\mathrm{Y}_{i}\left(1, \mathrm{Z}_{i}\right)-\mathrm{Y}_{i}\left(0, \mathrm{Z}_{i}\right)\right] \mathrm{D}_{i} \\
#' &=\mathrm{Y}_{0 i}+\left(\mathrm{Y}_{1 i}-\mathrm{Y}_{0 i}\right) \mathrm{D}_{i}
#' \end{aligned} \] A random-coefficients notation for this is
#' \[
#' \mathrm{Y}_{i}=\alpha_{0}+\rho_{i} \mathrm{D}_{i}+\eta_{i}
#' \]
#'  with $\alpha_{0} \equiv E\left[\mathrm{Y}_{0 i}\right]$ and $\rho_{i} \equiv \mathrm{Y}_{1 i}-\mathrm{Y}_{0 i}$

always.takers <- with(toy_data, sum(D == 1)) # n(d,z): n11 + n10 -> 30
never.takers <- with(toy_data, sum(D == 0)) # n00 + n01  -> 40
compliers <- with(toy_data, 
                  sum(D == 0 & Z == 0) + 
                    sum(D == 1 & Z == 1)) # n11 + n00 -> 40
defiers <- with(toy_data, 
                sum(D == 0 & Z == 1) + 
                  sum(D == 1 & Z == 0)) # n01 + n10 -> 30

# population average treatment effect
with(toy_data, mean(y[D == 1]) - mean(y[D == 0]))

# The treatment effect on the treated
(ATT <- with(subset(toy_data, D == 1), 
             mean(y[Z == 1]) - mean(y[Z == 0])))

# TE on compliers
with(subset(toy_data, (D == 1 & Z == 1) | (D == 0 & Z == 0)), 
     mean(y[Z == 1]) - mean(y[Z == 0]))

# TE on always-takers
with(toy_data, mean(y[(D == 1 & Z == 1)]) - mean(y[(D == 1 & Z == 0)]))

# proportions of subpopulation 
props <- with(toy_data, 
              c(
    sum(Z[D == 1] == 0) / length(Z[D == 1]), # D_0i = 1 | D_i = 1
    sum(Z[D == 1] == 1) / length(Z[D == 1]), # D_1i = 1 | D_i = 1
    sum(Z[D == 0] == 0) / length(Z[D == 0]), # D_0i = 0 | D_i = 0
    sum(Z[D == 0] == 1) / length(Z[D == 0])  # D_1i = 0 | D_i = 0
  ))
props

#+ eval = FALSE, include = FALSE
# Helper for proportions
prop <- function(x, d, z) {
  p <- with(x, sum(Z[D == d] == z) / length(Z[D == d]))
  comment <- sprintf("# D_%2$si = %1$s | D_i = %1$s", d, z)
  sprintf("%.2f %2$s", p, comment)
}
Map(prop,
    d = c(1, 1, 0, 0), 
    z = c(0, 1, 0, 1), 
    MoreArgs = list(x = toy_data))

#+ eval = FALSE, include = FALSE
bchhs <- haven::read_dta("tutorial/_05/BCHHS_data.dta")

var_description <- data.frame(
  vars = names(bchhs),
  labels = unname(sapply(bchhs, attr, "label"))
)

knitr::kable(var_description)

# generating log earnings and age squared
bchhs <- within(bchhs, {
  lnearn <- log(earning)
  agesq <- age**2
})

# ****************************************************************************
#  Table 2, Column 2: pooled OSL
# regressing all for whom we have complete wage information (428 individuals)
# contols: schooling, age, age_2

# OSL
ols <- lm(lnearn ~ highqua + age + agesq, bchhs)

#+ eval = FALSE, include = FALSE, echo = FALSE

# Stata into R
gen <- function(cmd, x) {
  stopifnot(grepl("^gen.*", cmd))
  x <- deparse(substitute(x))
  expr <- substring(cmd, 5)
  expr_split <- strsplit(expr, "=", fixed = TRUE)[[1]]
  new.var <- expr_split[[1]]
  rhs <- expr_split[[2]]
  rexpr <- sprintf("within(%s, {%s = %s})", x, new.var, rhs)
  eval(parse(text = rexpr), parent.frame())
}

translate_function <- function(cmd) {
  if (grepl("ln", cmd)) {
    gsub("ln", "log", cmd)
  }
}

#+ include = FALSE
knitr::spin("tutorial/_05/tutorial-05.R", knit = FALSE)
