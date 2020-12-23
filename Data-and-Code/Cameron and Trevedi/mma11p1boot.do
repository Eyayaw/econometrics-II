* MMA11P1BOOT.DO  March 2005 using Stata version 8.0

log using mma11p1boot.txt, text replace

********** OVERVIEW OF MMA11P1BOOT.DO **********

* STATA Program 
* copyright C 2005 by A. Colin Cameron and Pravin K. Trivedi 
* used for "Microeconometrics: Methods and Applications" 
* by A. Colin Cameron and Pravin K. Trivedi (2005)
* Cambridge University Press 

* Chapter 11.3 pages 366-368
* Bootstrap applied to exponential regression model
* Provides
* (1) Bootstrap distribution of beta and t-statistic (Table 11.1)
* (2) Various statistics from bootstrap (pages 366-8)
* (3) Bootstrap density of the t-statistic (Figure 11.1) 
* using generated data (see below)

* Note: To speed up progam reduce breps - the number of bootstrap replications
*       But final program should use many repications

* Note: This program uses ereg which is an old Stata command 
*       superceded by streg, dist(exp)

* Note: For bootstrap see also mm07p4boot.do
*       which has additional commands / ways to bootstrap

********** SETUP **********

set more off
version 8
  
********** GENERATE DATA **********

* Model is  y ~ exponential(exp(a + bx + cz))
* where  x and z are joint normal (1,1,0.1,0.1,0.5)
* i.e. means 0.1  and  0.1
*      sd's  0.1  and  0.1 and correln 0.5 (so correln^2 = .25)
*  variances 0.01 and 0.01 and covariance 0.005

* Generate data from joint normal
* Use fact that x is N(mu0.1,0.1)
*       and z | x is N(0.1 + .05/.1*(x - .1), .01x.75 = .0075)
*       so that st dev = sqrt(0.0075) = 0.0866025

set obs 50 
set seed 10001
* Generate x and z bivariate normal
scalar mu1=0.1
scalar mu2=0.1
scalar sig1=0.1
scalar sig2=0.1
scalar rho=0.5
scalar sig12=rho*sig1*sig2 
gen x = mu1 + sig1*invnorm(uniform()) 
gen muzgivx = mu2+(sig12/(sig2*sig2))*(x-mu1)
gen sigzgivx = sqrt(sig2*sig2*(1-rho*rho))
gen z = muzgivx + sigzgivx*invnorm(uniform()) 
* To generate y exponential with mean mu=Ey use
*   Integral 0 to a of (1/mu)exp(-x/mu) dx   by change of variables
* = Integral 0 to a/mu of exp(-t)dt
* = incomplete gamma function P(0,a/mu) in the terminology of Stata
gen Ey = exp(-2.0+2*x+2*z)
gen y = Ey*invgammap(1,uniform())
gen logy = log(y)

* Descriptive Statistics
summarize
ereg y x z

save mma11p1boot, replace

* Write data to a text (ascii) file so can use with programs other than Stata
outfile y x z using mma11p1boot.asc, replace

********** SIMPLE BOOTSTRAP ********** 

* Stata produces four bootstrap 100*(1-alpha) confidence intervals
* (N) and (P) have no asymptotic refinement
* (BC)-(BCA) have asymptotic refinement
* For details see program mma07p4boot.do

* Change the following for different number of simulations S
* From page 399, for testing better to use 999 than 1000
global breps = 999  /* The number of bootstrap reps used below */ 

set seed 20001

* A simple and adequate bootstrap command for the slope coefficients is
bs "ereg y x z"   "_b[x] _b[z]", reps($breps) level(95)

********** MORE DETAILED BOOTSTRAP **********

* The following bootstrap also gives standard error at each replication
* and saves data from replications for further analysis

* In partiulcar, want to use the percentile-t method, 
* which provides asymtptotic refinement

* Stata does not give this. For methods see
* e.g. Efron and Tibsharani (1993, pp.160-162)  
* e.g. Cameron and Trivedi (2005) Chapter 11.2.6-11.2.7
* For sample s compute t-test(s) = (bhat(s)-bhat) / se(s)
* where bhat is initial estimate 
* and bhat(s) and se(s) are for sth round.
* Order the t-test(s) statistics and choose the alpha/2 percentiles
* which give the critical values for the t-test

* Implementation requires saving the results from each bootstrap replication 
* in order to obtain ccritical values from percentiles of bootstrap distribution

use mma11p1boot.dta, clear

* Get and store coefficients (b) 
* for regressors in the original model and data before bootstrap
quietly ereg y x z
global bx=_b[x]
global sex=_se[x]
global bz=_b[z]
global sez=_se[z]
di " Coefficients    bx:  " $bx  "  and bz:  " $bz
di " Standard error sex:  " $sex "  and sez: " $sez

* Bootstrap and save coeff estimates and se's from each replication
set seed 20001
bs "ereg y x z"   "_b[x] _b[z] _se[x] _se[z]", reps($breps) level(95) saving(mma11p1bootreps) replace

* Now use the bootstrap estimates
use mma11p1bootreps, clear
sum
* Order comes from  "_b[x] _b[z] _se[x] _se[z]" in earlier bs
gen bxs = _bs_1
gen bzs = _bs_2
gen sexs = _bs_3
gen sezs = _bs_4
gen ttestxs = (bxs - $bx)/sexs
gen ttestzs = (bzs - $bz)/sezs

**********  (1) TABLE 11.1 (page 367)

summarize bzs ttestzs, d

* Additionally need the 2.5 and 97.5 percentiles not given in summarize, d

* Coefficient of z
_pctile bzs, p(2.5,97.5)
di " Lower 2.5 and upper 2.5 percentile of coeff b for z: " r(r1) "  and  " r(r2) 

* t-statistic for z 
_pctile ttestzs, p(2.5,97.5)
di " Lower 2.5 and upper 2.5 percentile of ttest on z: " r(r1) "  and  " r(r2) 

********** (2) RESULTS IN TEXT PAGES 366-7 ********** 

* (2A) Bootstrap standard error estimate (no refinement)
* These are given earlier in bootstrap table output
* Equivalently get the standard deviation of bzs

quietly sum bzs
scalar bzbootse = r(sd)
di "Bootstrap estimate of standard error: " bzbootse

* (2B) Test b3 = 0 using percentile-t method (asymptotic refinement)
* Use the 2.5% and 97.5% bootstrap critical values for t-statistic for z 

_pctile ttestzs, p(2.5,97.5)
di " Lower 2.5 and upper 2.5 percentile of ttest on z: " r(r1) "  and  " r(r2) 

* (2D) 95% confidence interval with asymptotic refinement
* Use the preceding critical values

scalar lbz = $bz + r(r1)*$sez    /* Note the plus sign here */
scalar ubz = $bz + r(r2)*$sez 
di " Percentile-t interval lower and upper bounds:  (" lbz   ","  ubz ")" 

* (2B-Var) Variation for symmetric two-sided test on z 

gen absttestzs = abs(ttestzs)
_pctile absttestzs, p(95)
di " Upper 5 percentile of symmetric two-sided test on z: " r(r1) "

* (2C) Test b3 = 0 without asymptotic refinement
* Usual Wald test except use bootstrap estimate of standard error

scalar Wald = ($bz - 0) / bzbootse
di "Wald statistic using bootstrap standard error: " Wald

* (2E) Bootstrap estimate of bias
* This is given in the earlier bootstrap results table
* and is explained in the text

********** (3) FIGURE 11.1 (p.368) PLOTS ESTIMATED DENSITY OF T-STATISTIC FOR Z

set scheme s1mono
label var ttestzs "Bootstrap t-statistic"
kdensity ttestzs, normal /*
  */ scale (1.2) plotregion(style(none)) /*
  */ title("Bootstrap Density of 't-Statistic'") /*
  */ xtitle("t-statistic from each bootstrap replication", size(medlarge)) xscale(titlegap(*5)) /* 
  */ ytitle("Density", size(medlarge)) yscale(titlegap(*5)) /* 
  */ legend(pos(11) ring(0) col(1)) legend(size(small)) /*
  */ legend( label(1 "Bootstrap Estimate") label(2 "Standard Normal")) 
graph save ch11boot, replace

********** CLOSE OUTPUT **********
log close
* clear
* exit

