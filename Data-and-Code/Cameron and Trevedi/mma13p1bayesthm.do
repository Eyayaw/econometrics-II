* MMA13P1BAYESTHM.DO March 2005 for Stata version 8.0

log using mma13p1bayesthm.txt, text replace

********** OVERVIEW OF MMA13P1BAYESTHM.DO **********

* STATA Program 
* copyright C 2005 by A. Colin Cameron and Pravin K. Trivedi 
* used for "Microeconometrics: Methods and Applications" 
* by A. Colin Cameron and Pravin K. Trivedi (2005)
* Cambridge University Press 

* Chapter 13.2.2 page 424 
* Create Figure 13.1
* (1) Bayes Analysis illustrated using normal distribution and prior

* No data are needed.

********** SETUP

set more off
version
set scheme s1mono  /* Graphics scheme */

********** DATA DESCRIPTION **********

* Model is  y ~ normal(theta, sigmesq) where sigmasq is known.
* and the prior is theta ~ normal(mu, tau)
* which gives a normal posterior
* n is set below in set obs

********** CREATE DATA **********

* The likleihood and prior are normal so the posterior is also normal

* Will evaluate the densities at points between 0 and 15
set obs 150
gen xeval = 0.1*_n 

* Likelihood with sigmasq known
scalar nobs = 50
scalar ybar = 10
scalar sigmasq = 100
gen likelihood = normden(xeval,ybar,sqrt(sigmasq/nobs))

* Prior
scalar mu = 5
scalar tausq = 3
gen prior = normden(xeval,mu,sqrt(tausq))

* Posterior given sample mean of using 
scalar tau1sq=1/((nobs/sigmasq)+(1/tausq))
scalar mu1 = tau1sq*((ybar*nobs/sigmasq)+(mu/tausq))
gen posterior = normden(xeval,mu1,sqrt(tau1sq))

scalar list
summarize

graph twoway (line likelihood xeval, clstyle(p2)) /*
  */ (line prior xeval, clstyle(p3)) /*
  */ (line posterior xeval, clstyle(p1)), /*
  */ scale (1.2) plotregion(style(none)) /*
  */ title("Bayes: Likelihood, Prior and Posterior") /*
  */ xtitle("Evaluation point", size(medlarge)) xscale(titlegap(*5)) /* 
  */ ytitle("Density", size(medlarge)) yscale(titlegap(*5)) /* 
  */ legend(pos(10) ring(0) col(1)) legend(size(small)) /*
  */ legend( label(1 "Likelihood N[10,2]") label(2 "Prior N[5,3]") /*
  */         label(3 "Posterior N[8,1.2]") )
graph save Ch13_Bayes1, replace
graph export Ch13_Bayes1.wmf, replace

********** CLOSE OUTPUT **********
log close
* clear 
* exit

