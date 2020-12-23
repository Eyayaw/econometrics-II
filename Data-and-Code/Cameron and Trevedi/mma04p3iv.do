* MMA04P3IV.DO  March 2005 for Stata version 8.0

log using mma04p3iv.txt, text replace

********** OVERVIEW OF MMA04P3IV.DO **********

* STATA Program 
* copyright C 2005 by A. Colin Cameron and Pravin K. Trivedi 
* used for "Microeconometrics: Methods and Applications" 
* by A. Colin Cameron and Pravin K. Trivedi (2005)
* Cambridge University Press 

* Chapter 4.8.8 pages 102-3
* Instrumental variables analysis.
* (1) IV Regression (with robust s.e.'s though not needed here for iid error).
* (2) Table 4.4 
* using generated data (see below)

********** SETUP **********

set more off
version 8

********** GENERATE DATA and SUMMARIZE **********

* Model is 
*    y = b1 + b2*x + u
*    x = c1 + c2*z + v
*    z ~ N[2,1]
* where b1=0, b2=0.5, c1=0 and c2=1
* and   u and v are joint normal (0,0,1,1,0.8)

* OLS of y on z is inconsistent as z is correlated with u
* Instead need to do IV with instrument x for z
* Also try using 
 
set seed 10001
set obs 10000
scalar b1 = 0
scalar b2 = 0.5
scalar c1 = 0
scalar c2 = 1

* Generate errors u and v
* Use fact that u is N(0,1)
* and v | u is N(0 + (.8/1)(u - 0), 1 - .8x.8/1 = 0.36)
gen u = 1*invnorm(uniform()) 
gen muvgivnu = 0.8*u
gen v = 1*(muvgivnu+sqrt(0.36)*invnorm(uniform()))

* Generate instrument z (which is purely random)
gen z = 2 + 1*invnorm(uniform()) 

* Generate regressor x which is correlated with z, and with u via v 
gen x = c1 + c2*z + v
 
* Generate dependent variable y
gen y = b1 + b2*x + u

* Generate z-cubed. Used as an alternative instrument
gen zcube = z*z*z

* Descriptive Statistics
describe
summarize
correlate y x z u v
correlate y x z u v, cov
graph matrix y x z u v

* Write data to a text (ascii) file so can use with programs other than Stata  
outfile y x z u v using mma04p3iv.asc, replace

********** DO THE ANALYSIS: ESTIMATE MODELS **********

* (1) OLS is inconsistent (first column of Table 4.4)
regress y x
regress y x, robust
estimates store olswrong

* (2) IV with instrument x is consistent and efficient (second column of Table 4.4)
ivreg y (x = z) 
ivreg y (x = z), robust
estimates store iv

* (3) IV estimator in (3) can be computed by 
*       regress y on z  gives dy/dz
*       regress x on z  gives dx/dz
* and divide the two
regress y z
matrix byonz = e(b)
regress x z
matrix bxonz = e(b)
matrix ivfirstprinciples = byonz[1,1]/bxonz[1,1]
matrix list byonz
matrix list bxonz
matrix list ivfirstprinciples

* (4) IV can be computed as 2SLS, but wrong standard errors
*     (third column of Table 4.4)
* (4A) OLS of x on z gives xhat
regress x z
predict xhat, xb
* (4B) OLS of x on xhat gives IV but wrong standard errors
regress y xhat
regress y xhat, robust
estimates store twosls
 
* (5) IV with instrument xcubed is consistent but inefficient
*     (last column of Table 4.4)
ivreg y (x = zcube) 
ivreg y (x = zcube), robust
estimates store ivineff

********** DISPLAY KEY RESULTS in Table 4.4 p.103 **********

* Table 4.4 page 103
estimates table olswrong iv twosls ivineff, se stats(N r2) b(%8.3f) keep(_cons x xhat)

********** CLOSE OUTPUT
log close
clear
exit


