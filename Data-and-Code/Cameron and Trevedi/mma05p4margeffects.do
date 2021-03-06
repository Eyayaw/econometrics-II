* MMA05P4MARGINALEFFECTS.DO for Stata version 8.0
 
log using mma05p4margeffects.txt, text replace

********** OVERVIEW OF MMA05P4MARGINALEFFECTS.DO **********

* STATA Program 
* copyright C 2005 by A. Colin Cameron and Pravin K. Trivedi 
* used for "Microeconometrics: Methods and Applications" 
* by A. Colin Cameron and Pravin K. Trivedi (2005)
* Cambridge University Press 

* Chapter 5.9.4 pp.162-3
* Marginal effects analysis for a nonlinear model (here exponential regression).

* Provides 
*   (1) Sample average marginal effect using derivative
*   (2) Sample average marginal effect using first difference
*   (3) Marginal effect evaluated at the sample mean
*   (4) Marginal effects (1)-(3) when model estimated by Stata ml command
* using generated data (see below)

* Related programs:
*   mma05p1mle.do      OLS and MLE for the same data 
*   mma05p2nls.do      NLS, WNLS, FGNLS for same data using nl command
*   mma05p3nlsbyml.do  NLS for same data using ml command

* To run this program you need data and dictionary files
*    mma05data.asc    ASCII data set generated by mma05p1mle.do

********** SETUP **********

set more off
version 8

********** READ IN DATA and SUMMARIZE **********

* Model is  y ~ exponential(exp(a + bx))
*           x ~ N[mux, sigx^2]
*        f(y) = exp(a + bx)*exp(-y*exp(a + bx))
*      lnf(y) = (a + bx) - y*exp(a + bx)
*        E[y] = exp(-(a + bx))    note sign reversal for the mean
*        V[y] = exp(-(a + bx)) = E[y]
* Here a = 2, b = -1  and  x ~ N[mux=1, sigx^21]
* and Table 5.7 uses N=10,000 

* Data was generated by program mma05p1mle.do
infile y x using mma05data.asc

* Descriptive Statistics
describe
summarize

********** MARGINAL EFFECTS for CHAPTER 5.9.4 **********

** (1) DERIVATIVE METHOD FOR SAMPLE AVERAGE MARGINAL EFFECT 

* (1A) METHOD A: Use analytical results
* Since E[y] = exp(-(a + bx))   Note: here sign reversal for the mean !!
*      dE[y]/dx = -b*exp(-(a + bx)) = -b*E[y]

* Estimate the model
* The Stata code for exponential regression is unusual as st command
* Need to declare data to be st data with dependent variable y
stset y
quietly streg x, distribution(exponential)  nohr
gen dEydxanalyticalderivative = -_b[x]*exp(-_b[_cons] - _b[x]*x)
* Alternative is to (1) predict the mean and (2) multiply by -_b[x]
quietly sum dEydxanalyticalderivative
scalar mesaad = r(mean)
di "Sample average marginal effect by analytical derivative = " mesaad

* (1B) METHOD B: Use numerical derivative (here one-sided)
* This is same as first difference code, except have small change in x
* Note: precision problems can arise with small changes in x
* The following code tries to minimize such problems
* Change in x will be 0.0001 times the standard deviation of x
egen sdx = sd(x)
quietly streg x, distribution(exponential) nohr
* Need to tell streg to predict the mean as this is not the default.
predict y0, mean time     
gen xoriginal = x
replace x = x+0.0001*sdx
predict y1, mean time
gen dEydxnumericalderivative = (y1 - y0)/(0.0001*sdx)
quietly sum dEydxnumericalderivative
scalar mesand = r(mean)
di "Sample average marginal effect by numerical derivative = " mesand
replace x = xoriginal
drop xoriginal sdx y0 y1

** (2) FINITE DIFFERENCE METHOD FOR SAMPLE AVERAGE MARGINAL EFFECT 

streg x, distribution(exponential) nohr    /* y is dependent variable */

* The following method can be used following many stata estimation commands
* 1. Predict y using sample data.
*    Need to say predict the mean as this is not the streg default.
predict y0, mean time
* 2. Predict y with regressor of x increased by one
gen xoriginal = x
replace x = x+1
predict y1, mean time
replace x = xoriginal  /* Put x back to initial value for later analysis */
* 3. Calculate difference
gen dEydxfinitedifference = y1 - y0
quietly sum dEydxfinitedifference 
scalar mesafd = r(mean)
di "Sample average marginal effect by first differences = " mesafd
drop xoriginal y0 y1

** (3) DERIVATIVE METHOD FOR MARGINAL EFFECT AT SAMPLE MEAN 

* (3A) Use Stata command mfx
quietly streg x, distribution(exponential) nohr
* Need to tell mfx to predict the mean as this is not the streg default.
mfx compute, dydx predict(mean time)
di "Marginal effect by analytical derivative at mean of x using mfx: "
matrix list e(Xmfx_dydx)

* (3B) Write ones own code
quietly streg x, distribution(exponential) nohr
quietly sum x
scalar meanx = r(mean)
scalar dEydxatmeanx = -_b[x]*exp(-_b[_cons] - _b[x]*meanx)
di "Marginal effect by analytical derivative at mean of x done manually: "
di dEydxatmeanx

** (4) MARGINAL EFFECTS AFTER ML COMMAND

* Preceding (1) - (3) presume there is a built-in command to get MLE.
* Now consider ML estimation using Stata's ml command.
* After ml command cannot use predict or mfx. 
* Need to be more manual, as follows.

* Estimate model by ml: for details see mma0p1mle.do
program define mleexp0
  version 8.0
  args lnf theta      /* Must use lnf while could use name other than theta */
  quietly replace `lnf' = `theta' - $ML_y1*exp(`theta')
end
quietly ml model lf mleexp0 (y = x)
quietly ml search
quietly ml maximize

* Note that here the mean is in fact exp(-a-b*x)

* (1A) Sample average marginal effect by calculus methods
gen mldEydxanalyticalderivative = -_b[x]*exp(-_b[_cons] - _b[x]*x)
quietly sum mldEydxanalyticalderivative
scalar mlmesaad = r(mean)
di "Sample average marginal effect by analytical derivative = " mlmesaad

* (1B) Sample average marginal effect by numerical derivative
egen sdx = sd(x)
gen y0 = exp(-_b[_cons] - _b[x]*x)
gen xoriginal = x
replace x = x+0.0001*sdx
gen y1 = exp(-_b[_cons] - _b[x]*x)
gen mldEydxnumericalderivative = (y1 - y0)/(0.0001*sdx)
quietly sum mldEydxnumericalderivative
scalar mlmesand = r(mean)
di "ML sample average marginal effect by numerical derivative = " mlmesand
replace x = xoriginal
drop xoriginal sdx y0 y1

* (2) Sample average marginal effect by increase x by one unit (finite difference)
gen mldEydxfinitedifference = exp(-_b[_cons]-_b[x]*(x+1)) - exp(-_b[_cons]-_b[x]*x)
quietly sum mldEydxfinitedifference
scalar mlmesafd = r(mean)
di "Sample average marginal effect by first differnce = " mlmesafd

* (3) Marginal effect estimated at the sample mean of x
quietly sum x
scalar meanx = r(mean)
scalar mldEydxatmeanx = -_b[x]*exp(-_b[_cons] - _b[x]*meanx)
di "ML marginal effect at mean of x by analytical derivative: "
di mldEydxatmeanx

********** DISPLAY RESULTS on p.162-3 **********

di "Marginal Effects: (1A) Analytical deriv (1B) Numerical Deriv (2) First diff" 
sum dEydxfinitedifference dEydxanalyticalderivative dEydxnumericalderivative

di "KEY RESULTS FOR CHAPTER 5.9.4 pp.162-3 FOLLOW"
di "(1A) Sample average marginal effect by analytical derivative = " mesaad
di "(1B) Sample average marginal effect by numerical derivative =  " mesand
di "(2)  Sample average marginal effect by first differences =     " mesafd
di "(3)  Marginal effect at mean of x by analytical derivative =   " dEydxatmeanx

********** CLOSE OUTPUT **********
log close
clear
exit
