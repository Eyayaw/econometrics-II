* MMA07P2POWER.DO  March 2005 for Stata version 8.0

log using mma07p2power.txt, text replace

********** OVERVIEW OF MMA07P2POWER.DO **********

* STATA Program 
* copyright C 2005 by A. Colin Cameron and Pravin K. Trivedi 
* used for "Microeconometrics: Methods and Applications" 
* by A. Colin Cameron and Pravin K. Trivedi (2005)
* Cambridge University Press 

* Chapter 7.6.3 pages 248-9
* Asymptotic Power of Wald test 

* (1) Chapter 7.6.3 obtains power for noncentral chisquare
* (2) Figure 7.2 (ch7power.wmf) plots against the noncentrality parameter lamda
* No data needed

********** SETUP **********

set more off
version 8.0
set scheme s1mono  /* Graphics scheme */

********** ANALYSIS **********

* Obtain power of chi-square tests 
* with df degrees of freedom
* and noncentrality parameter (ncp) lamda from 0 to 20
* for size alpha = 0.01, 0.05 and 0.10

set obs 201
scalar df = 1           /* Degrees of freedom */
gen lamda = 0.1*(_n-1)  /* Lamda = 0, 0.1, 0.2, ..., 19.9, 20.0 */ 

* Obtain power 
*      = Pr[W > chi-square(alpha) | W ~ chi-square(alpha)]
* for alpha = 0.01, 0.05 and 0.10

* Critical value at size alpha uses central chisquare     
* invchi2tail gives cv such that Pr(Chi2 > cv) = alpha
* Power is 1 minus cdf of noncentral chisquare 
* nchi2 gives the cdf of noncentral chisquare

scalar alpha = 0.01
scalar criticalvalue = invchi2tail(df,alpha)   
gen power01 = 1-nchi2(df,lamda,criticalvalue)  

scalar alpha = 0.05
scalar criticalvalue = invchi2tail(df,alpha)   
gen power05 = 1-nchi2(df,lamda,criticalvalue)  

scalar alpha = 0.10
scalar criticalvalue = invchi2tail(df,alpha)   
gen power10 = 1-nchi2(df,lamda,criticalvalue)  

sum
* For lamda = 0 have size = power, here 0.01, 0.05 and 0.10
list if lamda==0 | lamda==5 | lamda==10 | lamda==20

********** FIGURE 7.1 (p.249): PLOT THE POWER FUNCTION **********

graph twoway (line power10 lamda, clstyle(p1)) /*
  */  (line power05 lamda, clstyle(p2)) /*
  */  (line power01 lamda, clstyle(p3)), /*
  */ scale (1.2) plotregion(style(none)) /*
  */ title("Test Power as a function of the ncp") /*
  */ xtitle("Noncentrality parameter lamda", size(medlarge)) xscale(titlegap(*5)) /* 
  */ ytitle("Test Power", size(medlarge)) yscale(titlegap(*5)) /* 
  */ legend(pos(3) ring(0) col(1)) legend(size(small)) /*
  */ legend( label(1 "Test size = 0.10") label(2 "Test size = 0.05") /*
  */         label(3 "Test size = 0.01")) 
graph export ch7power.wmf, replace

********** CLOSE OUTPUT **********
log close
* clear
* exit
