* MMA16P1TOBIT.DO  March 2005 for Stata version 8.0

log using mma16p1tobit.txt, text replace

********** OVERVIEW OF MMA16P1TOBIT.DO **********

* STATA Program 
* copyright C 2005 by A. Colin Cameron and Pravin K. Trivedi 
* used for "Microeconometrics: Methods and Applications" 
* by A. Colin Cameron and Pravin K. Trivedi (2005)
* Cambridge University Press 

* Chapter 16.2.1 pages 530-1 and 16.9.2 page 565
* Classic Tobit model with generated data
* Provides
*   (1) Graph of various conditional means Figure 16.1 (ch16condmeans.wmf) 
*   (2) Tobit model estimation: various estimators not reported in book
*   (3) Tobit model estimation: CLAD estimation mentioned on page 565 
* using generated data (see below)

********** SETUP **********

set more off
version 8.0
set scheme s1mono   /* Used for graphs */
  
********** GENERATE DATA **********

* Data generating process is 
* Regressor:           lnwage ~ N(2.75, 0.6^2)
* Error term:               e ~ N(0, 1000^2)
* Latent variable:      ystar = -2500 + 1000*lnwage + e
* Truncated variable:  ytrunc = 1(ystar>0)*ystar
* Censored variable:    ycens = 1(ystar<=0)*0 + 1(ystar>0)*ystar
* Censoring Indicator:     dy = 1(ycens>0) 
 
set seed 10101
set obs 200    
gen e = 1000*invnorm(uniform( ))
gen lnwage = 2.75 + 0.6*invnorm(uniform( ))
gen ystar = -2500 + 1000*lnwage + e
gen ytrunc = ystar
replace ytrunc = . if (ystar < 0)
gen ycens = ystar
replace ycens = 0 if (ystar < 0)
gen dy = ycens
replace dy = 1 if (ycens>0)

summarize

* Save data as text (ascii) so that can use programs other than Stata
outfile e lnwage ystar ytrunc ycens dy using mma16p1tobit.asc, replace

********** (1) PLOT THEORETICAL CONDITIONAL MEANS **********

* Here we use the true parameter values used in the dgp 

* Compute the censored and truncated means
gen xb = -2500 + 1000*lnwage
gen sigma = 1000
gen capphixb = normprob(xb/sigma)
gen phixb = normd(xb/sigma)
gen lamda = phixb/capphixb
gen eytrunc = xb + sigma*lamda
gen eycens = capphixb*eytrunc

* Descriptive Statistics
summarize

* Plot Figure 16.1 on page 531
sort lnwage
graph twoway (scatter ystar lnwage, msize(small)) /* 
  */ (scatter eytrunc lnwage, c(l) msize(vtiny) clstyle(p3) clwidth(medthick)) /*
  */ (scatter eycens lnwage, c(l) msize(vtiny) clstyle(p2) clwidth(medthick)) /*
  */ (scatter xb lnwage, c(l) msize(vtiny) clstyle(p1) clwidth(medthick)), /*
  */ scale (1.2) plotregion(style(none)) /*
  */ title("Tobit: Censored and Truncated Means") /*
  */ xtitle("Natural Logarithm of Wage", size(medlarge)) xscale(titlegap(*5)) /* 
  */ ytitle("Different Conditional Means", size(medlarge)) yscale(titlegap(*5)) /*
  */ legend(pos(5) ring(0) col(1)) legend(size(small)) /*
  */ legend( label(1 "Actual Latent Variable") label(2 "Truncated Mean") /*
  */        label(3 "Censored Mean") label(4 "Uncensored Mean"))
graph export ch16condmeans.wmf, replace

********** (2) TOBIT MODEL ESTIMATION FOR THESE DATA **********

* These are computations not reported in the book.

* With only 200 observations the Heckman 2-step estimates given below 
* are very inefficient.  To verify that they are consistent 
* increase the sample size e.g. set obs 20000

* (2A) ESTIMATE THE VARIOUS MODELS

*** UNCENSORED OLS REGRESSION 
* Possible here since for these generated data we actually know ystar
* Yelds consistent estimate. Expect slope = 1000 approximately.
regress ystar lnwage, robust
estimates store ols
predict ystarols

*** CENSORED OLS REGRESSION
* Yields inconsistent estimates
* From subsection 16.3.6 for slope coefficient OLS converges to p times b
* where p is fraction of sample with positive values. Here 0.65*1000 = 650.
regress ycens lnwage, robust
estimates store censols
predict ycensols

*** TRUNCATED OLS REGRESSION for POSITIVE WAGE
* Yields inconsistent estimates
* See subsection 16.3.6 for discussion.
regress ytrunc lnwage, robust
estimates store truncols
predict ytrunols

*** CENSORED TOBIT MLE REGRESSION for HWAGE
* Yields consistent estimates
tobit ycens lnwage, ll(0)
estimates store censtobit
predict ycenstob

*** TRUNCATED TOBIT MLE REGRESSION for HWAGE
* If done propoerly yields consistent estimates
* Not sure how to do this in Stata 
* The obvious command is 
*    tobit ytrunc lnwage, ll(0)
* but this gives the same estimates as truncated OLS

*** PROBIT REGRESSION for HWAGE
* Yields consistent estimates for slope b/s = 1000/1000 = 1 
* but uses less information so expect less efficient than tobit
probit dy lnwage
estimates store probit
predict yprobit

*** HECKMAN 2-STEP ESTIMATOR DONE MANUALLY
* Yields consistent estimates but less efficient than censored tobit MLE
* The second stage standard errors will be incorrect
probit dy lnwage
predict probity, xb
gen invmills = normd(probity)/normprob(probity)
summarize dy probity invmills
regress ytrunc lnwage invmills
estimates store heck2step
correlate lnwage invmills
* And more robust standard errors may be found by
regress ytrunc lnwage invmills, robust
estimates store heck2srobust

*** HECKMAN 2-STEP ESTIMATOR DONE USING BUILT-IN HECKMAN COMMAND
* Yields consistent estimates but less efficient than censored tobit MLE
heckman ytrunc lnwage, select(lnwage) twostep
estimates store heckman
predict ystarhec, xb
predict ytrunhec, ycond
predict ycenshec, yexpected
predict yinvmill, mills
predict yprobsel, psel
correlate lnwage yinvmill

* (2B) DISPLAY COEFFICIENT ESTIMATES

* OLS estimates  True model is -2500 + 1000*lnwage
estimates table ols censols truncols, b(%10.2f) se(%10.2f) t stats(N ll) 

* Tobit estimates  True model is -2500 + 1000*lnwage
estimates table censtobit probit, b(%10.2f) se(%10.2f) t stats(N ll) 

* Tobit estimates using Heckman manual  True model is -2500 + 1000*lnwage
estimates table heck2step heck2srobust, b(%10.2f) se(%10.2f) t stats(N ll) 

* Tobit estimates using Heckman built-in True model is -2500 + 1000*lnwage
estimates table heckman, b(%10.2f) se(%10.2f) t stats(N ll)  

********** (3) CLAD ESTIMATION FOR THESE DATA page 565 **********

* Compare tobit MLE with censored least absolute deviations (CLAD) estimator
* Gives results at end of section 16.9.3 page 565

tobit ycens lnwage, ll(0)
clad ycens lnwage, reps(100) ll(0)

********** CLOSE OUTPUT
log close
clear 
exit

