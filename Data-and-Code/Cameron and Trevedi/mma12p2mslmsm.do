* MMA12P2MSLMSM.DO  March 2005 for Stata Version 8.0

log using mma12p2msmmsl.txt, text replace

********** OVERVIEW OF MMA12P2MSLMSM.DO **********

* STATA Program 
* copyright C 2005 by A. Colin Cameron and Pravin K. Trivedi 
* used for "Microeconometrics: Methods and Applications" 
* by A. Colin Cameron and Pravin K. Trivedi (2005)
* Cambridge University Press 

* Chapter 12.4.5 pages 397-8 and 12.5.5 pages 402-4
* Computes integral numerically and by simulation
*  (1) Maximum Simulated likelihood  Table 12.2
*  (2) Method of Simulated Moments   Table 12.3
* with application to generated data

* The application is only illustrative.
* This is not a template program for MSL or MSM.

* Different number of simulations S lead to different estimators.
* This program gives entries in Tables 12.2 and 12.3 for S = 100
* For other values of S change the value of simreps 
* from the current  global simreps 100

********** SETUP **********

set more off
version 8

********** DATA DESCRIPTION **********

* Model is  y = theta + u + e
* where  theta is a scalar parameter equal to 1
*        u is extreme value type 1
*        e is N(0,1)
* n is set in global numobs

********** DEFINE GLOBALS **********

global simreps 100  /* change this to change the number of simulations */
global numobs 100   /* change this to change the number of observations */


********** (1) MAXIMUM SIMULATED LIKELIHOOD (Table 12.2 p.398) **********

* This MSL program is inefficiently written computer code 
* as it requires drawing the same random variates at each iteration

* Generate data
clear
set obs $numobs
set seed 10101
gen u = -log(-log(uniform()))
gen e = invnorm(uniform())
gen y = 1 + u + e
summarize u e y

* Write data to a text (ascii) file so can use with programs other than Stata
outfile u e y using mma12p2mslmsm.asc, replace

* Use the variant ml d0 as this gives the entire likelihood, not just one observation. 
* I want this so that seed is only reset for the entire data.
* My program is inefficient as variates needs to be redrawn at each iteration 
program define msl
  version 6.0
  args todo b lnf        /* Need to use the names todo b and lnf
                            todo always contains 1 and may be ignored 
                            b is parameters and lnf is log-density   */
  tempvar theta1         /* create as needed to calculate lf, g, ... */
  mleval `theta1' = `b', eq(1)   /* theta1 is theta1_i = x_i'b       */
  local y "$ML_y1"       /* create to make program more readable     */ 
  set seed 10101
  tempvar denssim 
  global isim=1
  quietly gen `denssim' = exp(-0.5*(`y'-`theta1'+log(-log(uniform())))^2)/sqrt(2*_pi)
  while $isim < $simreps {
     quietly replace `denssim' = `denssim' + exp(-0.5*(`y'-`theta1'+log(-log(uniform())))^2)/sqrt(2*_pi)
  global isim=$isim+1
  }
  mlsum `lnf' = ln(`denssim'/$isim)
end

gen one = 1
ml model d0 msl (y = one, nocons )
ml maximize

*** Display MSL results in one column of Table 12.2 p.398

di "For number of simulations S = " $simreps
di "MSL estimator:   " _b[one]
di "Standard error:  " _se[one]

********** (2) METHOD OF SIMULATED MOMENTS (Table 12.3 p.404) **********

clear
set obs $numobs 
set seed 10101
gen u = -log(-log(uniform()))
gen e = invnorm(uniform())
gen y = 1 + u + e
summarize u e y

global isim=1
  gen usim = -log(-log(uniform()))
  gen esim = invnorm(uniform())
while $isim < $simreps {
  quietly replace usim = usim-log(-log(uniform()))
  quietly replace esim = esim+invnorm(uniform())
  global isim=$isim+1
  }
gen usimbar = usim/$isim
gen esimbar = esim/$isim
gen theta = y - usimbar - esimbar
summarize

* Results for Table 12.3 on page 404
* Here the st.eror of theta_MSM is approximated by the st. dev. of theta
* divided by the square root of S (the number of simulations)
quietly sum theta
scalar theta_MSM = r(mean)
scalar approx_sterror = r(sd)/sqrt($simreps)

* Display MSM results in one column of Table 12.3 p.404 
di "For number of simulations S = " $simreps
di "MSM estimator:  " theta_MSM
di "Approximate standard error:  " approx_sterror

* As written this will not give the correct standard errors (see p.403).
* Can get this by also computing the squared rv to get E[y^2]

********** CLOSE OUTPUT **********
log close
clear
exit
