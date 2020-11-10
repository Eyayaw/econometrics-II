* MMA08P3DIAGNOSTICS.DO   March 2005 for Stata version 8.2

log using mma08p3diagnostics.txt, text replace

********** OVERVIEW OF MMA08P3DIAGNOSTICS.DO **********

* STATA Program 
* copyright C 2005 by A. Colin Cameron and Pravin K. Trivedi 
* used for "Microeconometrics: Methods and Applications" 
* by A. Colin Cameron and Pravin K. Trivedi (2005)
* Cambridge University Press 

* Chapter 8.7.3 pages 290-1
* Model diagnostics example  (Table 8.3)

* (A) DIFFERENT R-SQUAREDS
* (B) CALCULATION OF RESIDUALS
* for a Poisson model with simulated data (see below).

* The data generation requires free Stata add-on command rndpoix
* In Stata: search rndpoix

* This program gives results for model 2
* For model 1 need to rerun with only x3 as regressor

********** SETUP **********

set more off
version 8.0
set scheme s1mono   /* Used for graphs */
  
********** GENERATE DATA **********

* Model is
*   y ~ Poisson[exp(b1 + b2*x2 + b3*x3]
* where 
*    x2 and x3 are iid ~ N[0,1] 
* and b1=0.5 and b2=0.5 and b3=0.5.

* The Diagnostics below are from Poisson regression of y on x3 alone
* or from Poisson regression of y on x3 and x3sq.   [Note" x2 is omitted]

set seed 10001
set obs 100
scalar b1 = 0.5
scalar b2 = 0.5
scalar b3 = 0.5

* Generate regressors
gen x2 = invnorm(uniform())
gen x3 = invnorm(uniform())

* Generate y
gen mupoiss = exp(b1+b2*x2+b3*x3)
* The next requires Stata add-on. In Stata: search rndpoix
rndpoix(mupoiss)
gen y = xp
sum

* Write data to a text (ascii) file so can use with programs other than Stata
outfile y x2 x3 using mma08p3diagnostics.asc, replace

********* SETUP FOR THIS PROGRAM **********

* Change this if want different regressors
gen x3sq = x3*x3
* global XLIST x3       /* Model 1 */
global XLIST x3 x3sq  /* Model 2 */

********* R-SQUARED (reported in Table 8.3 p.291) **********

* The following code can be changed to diffferent models than poisson
* For RsqRES, RsqEXP and RsqCOR need  
*     y        dependent variable
*     yhat     predicted value of dependent variable
* For RsqWRSS additionally need
*     sigmasq  predicted variance of dependent variable
* For RsqRG need log density evaluated at values given below

* Obtain exp(x'b)   Will vary with the model
poisson y $XLIST
predict yhat       
scalar dof = e(N)-e(k)

* RsqRES and RsqEXP are R-squared from sums of squares
* First get TSS, ESS and RSS
egen ybar = mean(y)
gen ylessybarsq = (y - ybar)^2
quietly sum ylessybarsq
scalar totalss = r(mean)
gen yhatlessybarsq = (yhat - ybar)^2
quietly sum yhatlessybarsq
scalar explainedss = r(mean)
gen residualsq = (y - yhat)^2
quietly sum residualsq
scalar residualss = r(mean)
* Second computed the rsquared
scalar sereg = sqrt(residualss/dof)
scalar RsqRES = 1 - residualss/totalss
scalar RsqEXP = explainedss/totalss

* RsqCOR uses sample correlation
quietly correlate y yhat
scalar RsqCOR = r(rho)^2

di "standard error of regression: " sereg
di "totalss:     " totalss _n "explainedss: " explainedss _n "residualss:  " residualss
di "RsqRES: " RsqRES _n  "RsqEXP: " RsqEXP _n "RsqCOR: " RsqCOR

* RsqWRSS uses weighted sums of squares
* First generate estimated variance of y
* Here for Poisson use fact that variance = mean
gen sigmasq = yhat
gen weightedylessybarsq = ((y - ybar)^2) / sigmasq
quietly sum weightedylessybarsq
scalar weightedtotalss = r(mean)
gen weightedresidualsq = ((y - yhat)^2) / sigmasq
quietly sum weightedresidualsq
scalar weightedresidualss = r(mean)
scalar RsqWRSS = 1 - weightedresidualss/weightedtotalss
di "RsqWRSS: " RsqWRSS

* RsqRG is from ML. Difficult to generalize beyond LEF models.
* Need 
*   lnL_fit  log-likelihood at fitted values  (the usual)
*   lnL_0    log-likelihood at intecept only 
*   lnL_max  log-likelihood at best fit 
quietly poisson y $XLIST
scalar lnL_fit = e(ll)
scalar lnL_0 = e(ll_0)
* The following applies only for Poisson. Differs for otehr models.
* lnf(y) = -mu + y*ln(mu) - ln(y!) 
* is maximized at mu = y
* so compute lnL_max = sum of [-y + y*ln(y) - lny!]
* Following sets 0*ln0 = 0
gen ylny = 0
replace ylny = y*ln(y)  if y > 0
gen lnfyatmax = -y + ylny - lnfact(y)
quietly sum lnfyatmax
scalar lnL_max = r(sum)
scalar RsqRG = (lnL_fit - lnL_0) / (lnL_max - lnL_0)

* RsqQ should only be used for binary and other discrete choice models
* And definitely use only if lnL_fit < 0
scalar RsqQ = 1 - lnL_fit/lnL_0

di "lnL_0:   " lnL_0 _n "lnL_fit: " lnL_fit _n "lnL_max: " lnL_max
di "RsqRG: " RsqRG _n "RsqQ: " RsqQ

* Check
sum
poisson y $XLIST   /* Stata Rsq = RsqQ */

*** The following results are for Model 2 in Table 8.3 p.291
*** For model 1 R-squareds need to rerun with only x3 as regressor
di "standard error of regression: " sereg
di "RsqRES: " RsqRES _n  "RsqEXP: " RsqEXP _n "RsqCOR: " RsqCOR 
di "RsqWRSS: " RsqWRSS _n "RsqRG: " RsqRG _n "RsqQ: " RsqQ

********* RESIDUAL ANALYSIS (text bottom p.290 to top p.291) **********

* Assume that from earlier have yhat

* raw residual
gen raw = y - yhat
gen sigma = sqrt(yhat)
gen Pearson = (y - yhat)/sigma
* Note that earlier defined ylny = 0 if y=0 and = yln(y) otherwise
gen deviance = sign(y-yhat)*sqrt(2*(-y+ylny)-2*(-yhat+y*ln(yhat)))

*** The following are results reported in text bottom p.290 to top p.291
sum raw Pearson deviance
corr raw Pearson deviance
* Example of use to find whether x3 belongs in the model
* graph twoway scatter Pearson x3

********** CLOSE OUTPUT
log close
clear
exit


