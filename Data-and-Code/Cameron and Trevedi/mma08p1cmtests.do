* MMA08P1CMTESTS.DO   March 2005 for Stata version 8.0

log using mma08p1cmtests.txt, text replace

********** OVERVIEW OF MMA08P1CMTESTS.DO **********

* STATA Program 
* copyright C 2005 by A. Colin Cameron and Pravin K. Trivedi 
* used for "Microeconometrics: Methods and Applications" 
* by A. Colin Cameron and Pravin K. Trivedi (2005)
* Cambridge University Press 

* Chapter 8.2.6 pages 269-71
* Conditional moment tests example producing Table 8.1

* (A) TEST OF THE CONDITIONAL MEAN
* (B) TEST THAT CONDITIONAL VARIANCE = MEAN 
* (C) ALTERNATIVE TEST THAT CONDITIONAL VARIANCE = MEAN 
* (D) INFORMATION MATRIX TEST
* (E) CHI-SQUARE GOODNESS OF FIT TEST 
* for a Poisson model with generated data (see below).

* The data generation requires free Stata add-on command rndpoix
* In Stata: search rndpoix

********** SETUP **********

set more off
version 8.0
set scheme s1mono   /* Used for graphs */
  
********** GENERATE DATA **********

* Model is
*   y ~ Poisson[exp(b1 + b2*x2]
* where 
*    x2 is iid ~ N[0,1]
* and b1=0 and b2=1.

set seed 10001
set obs 200
scalar b1 = 0
scalar b2 = 1

* Generate regressors
gen x2 = invnorm(uniform())

* Generate y
gen mupoiss = exp(b1+b2*x2)
* The next requires Stata add-on. In Stata: search rndpoix
rndpoix(mupoiss)
gen y = xp

* Write data to a text (ascii) file so can use with programs other than Stata
outfile y x2 using mma08p1cmtests.asc, replace

********* POISSON REGRESSION **********

poisson y x2
* Obtain exp(x'b)

* Obtain the scores to be used later
predict yhat       
* For the Poisson s = dlnf(y)/db = (y - exp(x'b))*x
gen s1 = (y - yhat)
gen s2 = (y - yhat)*x2

* Summarize data
* Should get s1 and s2 summing to zero
sum

********** ANALYSIS: CONDITIONAL MOMENTS TESTS **********

* The program is appropriate for MLE with density assumed to be correctly specified.
* Let H0: E[m(y,x,theta)] = 0 
* Then CM = explained sum of squares or N times uncentered Rsq from 
* auxiliary regression of 1 on m and the components of s = dlnf(y)//dtheta
* The test is chi-squared with dim(m) degrees of freedom.

* Define the dependent variable one for the aucxiliary regressions
gen one = 1
 
*** (A) TEST OF THE CONDITIONAL MEAN  (Table 8.1 p.270 row 1)

* Test H0: E[(y - exp(x'b))*z] = 0  where z = x2sq

* A smilar test is relevant for many nonlinear models
* Just change the expression for the conditional mean.
* Here we used E[y|x] = exp(x'b) for the Poisson 
* Also for the Poisson z cannot be x as this sums to zero by Poisson foc
* For some other models (basically non-LEF models) z can be x 

gen z = x2*x2
gen mA = (y - yhat)*z
regress one mA s1 s2, noconstant
scalar CMA = e(N)*e(r2)
di "CMA: " CMA    " p-value: " chi2tail(1,CMA)

* Check that three different ways give same answer.
di "N times Uncentered R-squared:    " e(N)*e(r2)
di "Explained Sum of Squares:        " e(mss)
di "N minus Residual Sum of Squares: " e(N) - e(rss)

*** (B) TEST THAT CONDITIONAL VARIANCE = MEAN  (Table 8.1 p.270 row 2)

* Test H0: E[{(y - exp(x'b))^2 - exp(x'b)}*x] = 0

* This test is peculiar to Poisson which restricts mean = variance

* Here m has 2 terms 
gen mB1 = ((y - yhat)^2 - yhat)
gen mB2 = ((y - yhat)^2 - yhat)*x2
regress one mB1 mB2 s1 s2, noconstant
scalar CMB = e(N)*e(r2)
di "CMB: " CMB    "  p-value: " chi2tail(2,CMB)

*** (C) ALTERNATIVE TEST THAT CONDITIONAL VARIANCE = MEAN  (Table 8.1 p.270 row 3)

* Test H0: E[{(y - exp(x'b))^2 - y}*x] = 0

* This test is peculiar to Poisson which restricts mean = variance
* This test is also peculiar as here dm/db = 0

* Here m has 2 terms 
gen mC1 = ((y - yhat)^2 - y)
gen mC2 = ((y - yhat)^2 - y)*x2

* To be consistent with other tests include s1 and s2. 
regress one mC1 mC2 s1 s2, noconstant
scalar CMC = e(N)*e(r2)
di "CMC: " CMC     "  p-value: " chi2tail(2,CMC)

* Since dm/db = 0 could just do the regression without the scores
regress one mC1 mC2, noconstant
scalar CMCnoscores = e(N)*e(r2)
di "CMCnoscores: " CMC     "  p-value: " chi2tail(2,CMCnoscores)

*** (D) INFORMATION MATRIX TEST   (Table 8.1 p.270 row 4)

* Test H0: E[{(y - exp(x'b))^2 - y}*vech(xx')] = 0

* A similar test is relevant for other parametric models
* In general m = vech(d2lnf(y)/dbdb')
* and for Poisson this yields above

* Here m is a 3x1 vector
gen mD1 = ((y - yhat)^2 - y)
gen mD2 = ((y - yhat)^2 - y)*x2
gen mD3 = ((y - yhat)^2 - y)*x2*x2

* To be consistent with other tests include s1 and s2. 
regress one mD1 mD2 mD3 s1 s2, noconstant
scalar CMD = e(N)*e(r2)
di "CMD: " CMD     "  p-value: " chi2tail(3,CMD)

* Since dm/db = 0 could just do the regression without the scores
regress one mD1 mD2 mD3, noconstant
scalar CMDnoscores = e(N)*e(r2)
di "CMDnoscores: " CMDnoscores     "  p-value: " chi2tail(3,CMDnoscores)

*** (E) CHI-SQUARE GOODNESS OF FIT TEST  (Table 8.1 p.270 row 5)

* Test H0: E[{d_j - Pr[y = j]] = 0
* where d_j = 1 if y = j   for j = 0, 1, 2, and 3 or more
* and  Pr[y = j] = exp(-lamda)*lamda^y/y!  for lamda = exp(x'b)
* Cells get too small if have more cells than up to 3 or more.

* A similar test is relevant for other parametric models,
* though a natural partitioning for y may be less obvious. 

* Here m has 4 terms
gen d0 = 0
replace d0 = 1 if y==0
gen d1 = 0
replace d1 = 1 if y==1
gen d2 = 0
replace d2 = 1 if y==2
gen p0 = exp(-yhat)
gen p1 = exp(-yhat)*yhat
gen p2 = exp(-yhat)*(yhat^2)/2
gen mE1 = d0 - p0
gen mE2 = d1 - p1
gen mE3 = d2 - p2
regress one mE1 mE2 mE3 s1 s2, noconstant
scalar CME = e(N)*e(r2)
di "CME: " CME     "  p-value: " chi2tail(3,CME)

* Wrong alternative is basic chisquare
quietly sum d0
scalar sumd0 = r(sum)
quietly sum d1
scalar sumd1 = r(sum)
quietly sum d2
scalar sumd2 = r(sum)
scalar sumd3 = 1 - sumd0 - sumd1 - sumd2
quietly sum p0
scalar sump0 = r(sum)
quietly sum p1
scalar sump1 = r(sum)
quietly sum p2
scalar sump2 = r(sum)
scalar sump3 = 1 - sump0 - sump1 - sump2
scalar chisq = (sumd0-sump0)^2/sump0 + (sumd1-sump1)^2/sump1 /*
            */  + (sumd2-sump2)^2/sump2 + (sumd3-sump3)^2/sump3 
di "Wrong Traditional chi-square: " chisq " p = " chi2tail(3,chisq)


********** DISPLAY RESULTS  (Table 8.1 p.270) **********

sum

* Gives Rows 1-5 of Table 8.1 (The CMxnoscores are not reported)
di "CMA: " CMA    " p-value: " chi2tail(1,CMA)
di "CMB: " CMB    " p-value: " chi2tail(2,CMB)
di "CMC: " CMC    " p-value: " chi2tail(2,CMC)
di "CMD: " CMD    " p-value: " chi2tail(3,CMD)
di "CME: " CME    " p-value: " chi2tail(3,CME)
di "CMCnoscores: " CMCnoscores     "  p-value: " chi2tail(2,CMCnoscores)
di "CMDnoscores: " CMDnoscores     "  p-value: " chi2tail(3,CMDnoscores)

********** FURTHER ANALYSIS gives M** column in Table 8.1 **********

* The following drops the scores from the regression. Provides lower bound.
* Results are reported in last column in Table 8.1
quietly regress one mA, noconstant
di "CMA without scores:" e(N)*e(r2) " with p = " chi2tail(1,e(N)*e(r2))
quietly regress one mB1 mB2, noconstant
di "CMB without scores:" e(N)*e(r2) " with p = " chi2tail(2,e(N)*e(r2))
quietly regress one mC1 mC2, noconstant
di "CMC without scores:" e(N)*e(r2) " with p = " chi2tail(2,e(N)*e(r2))
quietly regress one mD1 mD2 mD3, noconstant
di "CMD without scores:" e(N)*e(r2) " with p = " chi2tail(3,e(N)*e(r2))
quietly regress one mE1 mE2 mE3, noconstant
di "CME without scores:" e(N)*e(r2) " with p = " chi2tail(3,e(N)*e(r2))

********** CLOSE OUTPUT
log close
clear
exit

