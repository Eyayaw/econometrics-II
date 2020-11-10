# Quasi-experiment
## DiD - RDD

# RDD ---------------------------------------------------------------------
library(RDDtools)

set.seed(1212)
W <- runif(1000, -1, 1)
y <- 3 + 2 * W + 10 * (W>=0) + rnorm(1000)


# construct rdd_data 
data <- RDDdata(y, W, cutpoint = 0)

# plot the sample data
plot(data,
     col = "steelblue",
     cex = 0.35, 
     xlab = "W", 
     ylab = "Y")


# initialize plot and add control group
plot(c(0, 1), c(6, 8), 
     type = "p",
     ylim = c(5, 12),
     xlim = c(-0.3, 1.3),
     main = "The Differences-in-Differences Estimator",
     xlab = "Period",
     ylab = "Y",
     col = "steelblue",
     pch = 20,
     xaxt = "n",
     yaxt = "n")

axis(1, at = c(0, 1), labels = c("before", "after"))
axis(2, at = c(0, 13))

# add treatment group
points(c(0, 1, 1), c(7, 9, 11), 
       col = "darkred",
       pch = 20)

# add line segments
lines(c(0, 1), c(7, 11), col = "darkred")
lines(c(0, 1), c(6, 8), col = "steelblue")
lines(c(0, 1), c(7, 9), col = "darkred", lty = 2)
lines(c(1, 1), c(9, 11), col = "black", lty = 2, lwd = 2)

# add annotations
text(1, 10, expression(hat(beta)[1]^{DID}), cex = 0.8, pos = 4)
text(0, 5.5, "s. mean control", cex = 0.8 , pos = 4)
text(0, 6.8, "s. mean treatment", cex = 0.8 , pos = 4)
text(1, 7.9, "s. mean control", cex = 0.8 , pos = 4)
text(1, 11.1, "s. mean treatment", cex = 0.8 , pos = 4)

# set sample size
n <- 200

# define treatment effect
TEffect <- 4

# generate treatment dummy
TDummy <- c(rep(0, n/2), rep(1, n/2))

# simulate pre- and post-treatment values of the dependent variable
y_pre <- 7 + rnorm(n)
y_pre[1:n/2] <- y_pre[1:n/2] - 1
y_post <- 7 + 2 + TEffect * TDummy + rnorm(n)
y_post[1:n/2] <- y_post[1:n/2] - 1 

library(scales)

pre <- rep(0, length(y_pre[TDummy==0]))
post <- rep(1, length(y_pre[TDummy==0]))

# plot control group in t=1
plot(jitter(pre, 0.6), 
     y_pre[TDummy == 0], 
     ylim = c(0, 16), 
     col = alpha("steelblue", 0.3),
     pch = 20, 
     xlim = c(-0.5, 1.5),
     ylab = "Y",
     xlab = "Period",
     xaxt = "n",
     main = "Artificial Data for DID Estimation")

axis(1, at = c(0, 1), labels = c("before", "after"))

# add treatment group in t=1
points(jitter(pre, 0.6), 
       y_pre[TDummy == 1], 
       col = alpha("darkred", 0.3), 
       pch = 20)

# add control group in t=2
points(jitter(post, 0.6),
       y_post[TDummy == 0], 
       col = alpha("steelblue", 0.5),
       pch = 20)

# add treatment group in t=2
points(jitter(post, 0.6), 
       y_post[TDummy == 1], 
       col = alpha("darkred", 0.5),
       pch = 20)

# compute the DID estimator for the treatment effect 'by hand'
mean(y_post[TDummy == 1]) - mean(y_pre[TDummy == 1]) - 
  (mean(y_post[TDummy == 0]) - mean(y_pre[TDummy == 0]))


# compute the DID estimator using a linear model
lm(I(y_post - y_pre) ~ TDummy)


# prepare data for DID regression using the interaction term 
d <- data.frame("Y" = c(y_pre,y_post),
                "Treatment" = TDummy, 
                "Period" = c(rep("1", n), rep("2", n)))

# estimate the model
lm(Y ~ Treatment * Period, data = d)

# generate some sample data
W <- runif(1000, -1, 1)
y <- 3 + 2 * W + 10 * (W>=0) + rnorm(1000)

# load the package 'rddtools'
library(rddtools)

# construct rdd_data 
data <- RDDdata(y, W, cutpoint = 0)

# plot the sample data
plot(data,
     col = "steelblue",
     cex = 0.35, 
     xlab = "W", 
     ylab = "Y")

# estimate the sharp RDD model
rdd_mod <- RDDreg_lm(rdd_object = data, slope = "same")
summary(rdd_mod)

# plot the RDD model along with binned observations
plot(rdd_mod,
     cex = 0.35, 
     col = "steelblue", 
     xlab = "W", 
     ylab = "Y")

# generate sample data
mu <- c(0, 0)
sigma <- matrix(c(1, 0.7, 0.7, 1), ncol = 2)

set.seed(1234)
d <- as.data.frame(MASS::mvrnorm(2000, mu, sigma))
colnames(d) <- c("W", "Y")

# introduce fuzziness
d$treatProb <- ifelse(d$W < 0, 0, 0.8)

fuzz <- sapply(X = d$treatProb, FUN = function(x) rbinom(1, 1, prob = x))

# treatment effect
d$Y <- d$Y + fuzz * 2

# generate a colored plot of treatment and control group
plot(d$W, d$Y,
     col = c("steelblue", "darkred")[factor(fuzz)], 
     pch= 20, 
     cex = 0.5,
     xlim = c(-3, 3),
     ylim = c(-3.5, 5),
     xlab = "W",
     ylab = "Y")

# add a dashed vertical line at cutoff
abline(v = 0, lty = 2)

# estimate the Fuzzy RDD
data <- RDDdata(d$Y, d$W, 
                 cutpoint = 0, 
                 z = d$treatProb)

frdd_mod <- RDDreg_lm(data, slope = "same")
frdd_mod

# plot estimated FRDD function
plot(frdd_mod, 
     cex = 0.5, 
     lwd = 0.4,
     xlim = c(-4, 4),
     ylim = c(-3.5, 5),
     xlab = "W",
     ylab = "Y")

# estimate SRDD
data <- RDDdata(d$Y, d$W, cutpoint = 0)

srdd_mod <- RDDreg_lm(data, slope = "same")
srdd_mod
