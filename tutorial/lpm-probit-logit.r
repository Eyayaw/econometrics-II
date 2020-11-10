#' ---
#'  title: LPM Probit Logit
#'  date: July 14, 2020
#'  author: Eyayaw Beze
#'  subtitle: Taken from `https://www.econometrics-with-r.org/`; created via `rawr::rawr(...)`
#'  output: 
#'      pdf_document:
#'        df_print: kable
#'  
#' ---
#' # Linear Probability Model (LPM)
#'  
#+ message = FALSE, waring = FALSE, echo = FALSE
# load `AER` package and attach the HMDA data
library(AER)
data(HMDA)
#+
# inspect the data
head(HMDA)

summary(HMDA)

as.numeric(HMDA$deny)

# convert 'deny' to numeric
HMDA$deny <- as.numeric(HMDA$deny) - 1

# estimate a simple linear probability model
denymod1 <- lm(deny ~ pirat, data = HMDA)
denymod1

# plot the data
plot(x = HMDA$pirat, 
     y = HMDA$deny,
     main = "Mortgage Application Denial and the Payment-to-Income Ratio",
     xlab = "P/I ratio",
     ylab = "Deny",
     pch = 20,
     ylim = c(-0.4, 1.4),
     cex.main = 0.8)

# add horizontal dashed lines and text
abline(h = 1, lty = 2, col = "darkred")
abline(h = 0, lty = 2, col = "darkred")
text(2.5, 0.9, cex = 0.8, "Mortgage denied")
text(2.5, -0.1, cex= 0.8, "Mortgage approved")

# add the estimated regression line
abline(denymod1, 
       lwd = 1.8, 
       col = "steelblue")

# print robust coefficient summary
coeftest(denymod1, vcov. = vcovHC, type = "HC1")

# rename the variable 'afam' for consistency
colnames(HMDA)[colnames(HMDA) == "afam"] <- "black"

# estimate the model
denymod2 <- lm(deny ~ pirat + black, data = HMDA)
coeftest(denymod2, vcov. = vcovHC)

#' # Probit Model
#' 
# estimate the simple probit model
denyprobit <- glm(deny ~ pirat, 
                  family = binomial(link = "probit"), 
                  data = HMDA)

coeftest(denyprobit, vcov. = vcovHC, type = "HC1")


# plot data
plot(x = HMDA$pirat, 
     y = HMDA$deny,
     main = "Probit Model of the Probability of Denial, Given P/I Ratio",
     xlab = "P/I ratio",
     ylab = "Deny",
     pch = 20,
     ylim = c(-0.4, 1.4),
     cex.main = 0.85)

# add horizontal dashed lines and text
abline(h = 1, lty = 2, col = "darkred")
abline(h = 0, lty = 2, col = "darkred")
text(2.5, 0.9, cex = 0.8, "Mortgage denied")
text(2.5, -0.1, cex= 0.8, "Mortgage approved")

# add estimated regression line
x <- seq(0, 3, 0.01)
y <- predict(denyprobit, list(pirat = x), type = "response")

lines(x, y, lwd = 1.5, col = "steelblue")

# 1. compute predictions for P/I ratio = 0.3, 0.4
predictions <- predict(denyprobit, 
                       newdata = data.frame("pirat" = c(0.3, 0.4)),
                       type = "response")

# 2. Compute difference in probabilities
diff(predictions)


denyprobit2 <- glm(deny ~ pirat + black, 
                   family = binomial(link = "probit"), 
                   data = HMDA)

coeftest(denyprobit2, vcov. = vcovHC, type = "HC1")



# 1. compute predictions for P/I ratio = 0.3
predictions <- predict(denyprobit2, 
                       newdata = data.frame("black" = c("no", "yes"), 
                                            "pirat" = c(0.3, 0.3)),
                       type = "response")

# 2. compute difference in probabilities
diff(predictions)


#' # Logit Model
#' 
denylogit <- glm(deny ~ pirat, 
                 family = binomial(link = "logit"), 
                 data = HMDA)

coeftest(denylogit, vcov. = vcovHC, type = "HC1")


# plot data
plot(x = HMDA$pirat, 
     y = HMDA$deny,
     main = "Probit and Logit Models Model of the Probability of Denial, Given P/I Ratio",
     xlab = "P/I ratio",
     ylab = "Deny",
     pch = 20,
     ylim = c(-0.4, 1.4),
     cex.main = 0.9)

# add horizontal dashed lines and text
abline(h = 1, lty = 2, col = "darkred")
abline(h = 0, lty = 2, col = "darkred")
text(2.5, 0.9, cex = 0.8, "Mortgage denied")
text(2.5, -0.1, cex= 0.8, "Mortgage approved")

# add estimated regression line of Probit and Logit models
x <- seq(0, 3, 0.01)
y_probit <- predict(denyprobit, list(pirat = x), type = "response")
y_logit <- predict(denylogit, list(pirat = x), type = "response")

lines(x, y_probit, lwd = 1.5, col = "steelblue")
lines(x, y_logit, lwd = 1.5, col = "black", lty = 2)

# add a legend
legend("topleft",
       horiz = TRUE,
       legend = c("Probit", "Logit"),
       col = c("steelblue", "black"), 
       lty = c(1, 2))

# estimate a Logit regression with multiple regressors
denylogit2 <- glm(deny ~ pirat + black, 
                  family = binomial(link = "logit"), 
                  data = HMDA)

coeftest(denylogit2, vcov. = vcovHC, type = "HC1")


# 1. compute predictions for P/I ratio = 0.3
predictions <- predict(denylogit2, 
                       newdata = data.frame("black" = c("no", "yes"), 
                                            "pirat" = c(0.3, 0.3)),
                       type = "response")

predictions

# 2. Compute difference in probabilities
diff(predictions)




