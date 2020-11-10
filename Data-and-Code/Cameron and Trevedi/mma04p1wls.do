* MMA04P1WLS.DO   March 2005 for Stata version 8.0

log using mma04p1wls.txt, text replace

********** OVERVIEW OF MMA04P1WLS.DO **********

* STATA Program 
* copyright C 2005 by A. Colin Cameron and Pravin K. Trivedi 
* used for "Microeconometrics: Methods and Applications" 
* by A. Colin Cameron and Pravin K. Trivedi (2005)
* Cambridge University Press 

* Chapter 4.5.3 pages 84-5 
* Robust Standard Errors for OLS, WLS and GLS
* (1) Robust and nonrobust standard errors for OLS, WLS and GLS.
* (2) Table 4.3 
* using generated data (see below)

********** SETUP **********

set more off
version 8
set scheme s1mono   /* Used for graphs */
  
********** GENERATE DATA and SUMMARIZE **********

* Model is  y = 1 + 1*x + u
* where     u = abs(x)*e
*           x ~ N(0, 5^2)
*           e ~ N(0, 2^2)

* Errors are conditionally heteroskedastic with V[u|x]=4*x^2
* OLS, WLS and GLS are consistent 
* but need to use robust standard errors for OLS and WLS.

set seed 10105
set obs 100
gen x = 5*invnorm(uniform())
gen e = 2*invnorm(uniform())
gen u = abs(x)*e
gen y = 1 + 1*abs(x) + u

* Descriptive Statistics
summarize

* Write data to a text (ascii) file so can use with programs other than Stata
outfile y x e u using mma04p1wls.asc, replace

********** ESTIMATE THE MODELS **********

** (1) OLS - first column of Table 4.3

* (1A) OLS with wrong standard errors
regress y x
estimates store olsusual

* (1B) OLS with correct standard errors (robust sandwich)
regress y x, robust
estimates store olsrobust

** (2) WLS - second column of Table 4.3

* (2A) WLS with wrong standard errors
* Use the aweight option (not clearly explained in Stata manual).
* The aweight option MULTIPLIES y and x by sqrt(aweight).
* Here we suppose V[u]=constant*|x|
* So want to divide by sqrt(|x|), so let aweight=1/|x|
gen absx = abs(x)
regress y x [aweight=1/absx] 
estimates store wlsusual

* (2B) WLS with correct standard errors (robust sandwich)
regress y x [aweight=1/absx], robust 
estimates store wlsrobust

** (3) GLS - last column of Table 4.3

* (3A) GLS with usual standard errors (correct)
* Here we know V[u]=constant*x^2
* So want to divide by x, so let aweight=1/(x^2)
gen xsq = x*x
regress y x [aweight=1/xsq]
estimates store glsusual

* (3B) GLS with standard errors (robust sandwich - unnecessary here)
regress y x [aweight=1/xsq], robust
estimates store glsrobust

* (3C) Check that aweight works as expected. 
* Do GLS by OLS on daya transformed by dividing by x.
gen try = y/x
gen trint = 1/x
gen trx = x/x
regress try trx trint, noconstant

********** DISPLAY KEY RESULTS **********

* Table 4.3
estimates table olsusual olsrobust wlsusual wlsrobust glsusual glsrobust, /*
       */ se stats(N r2) b(%7.3f) keep(_cons x)

* Minor typo in Table 4.3: 
*  for GLS Constant has robust s.e. of [0.008] not [0.006]

********** CLOSE OUTPUT **********
log close
clear
exit

