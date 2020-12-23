* MMA17P3WEIB.DO  March 2005 for Stata 8.0

log using mma17p3weib.txt, text replace

********** OVERVIEW OF MMA17P3WEIB.DO **********

* STATA Program 
* copyright C 2005 by A. Colin Cameron and Pravin K. Trivedi 
* used for "Microeconometrics: Methods and Applications" 
* by A. Colin Cameron and Pravin K. Trivedi (2005)
* Cambridge University Press 

* Chapter 17.6.1 (pages 584-6)
* Plot of Weibull density, survuvor, hazard and cumulative hazard functions
* Provides  
*   (1) Figure 17.2 (ch17weibull.wmf)  

* This program requires no data

********** SETUP **********

set more off
version 8.0
set scheme s1mono   /* Used for graphs */
  
********** GENERATE DATA AND FUNCTIONS **********

set obs 800

gen t = 0.1*_n   /* duration time */

* Generate the survivor, hazard, cumulative hazard and density
scalar g = 0.01  /* gamma */
scalar a = 1.5   /* alpha */
gen surv = exp(-g*(t^(a)))
gen density = g*a*(t^(a-1))*exp(-g*(t^(a)))
gen hazard = g*a*(t^(a-1))
gen cumhaz = -ln(surv)

********** DO THE FOUR SEPARATE GRAPHS FOR FIGURE 17.2 **********

* Weibull density
graph twoway (scatter density t, c(l) msize(vtiny) clwidth(medthick) clstyle(p1)), /*
  */ scale (1.2) plotregion(style(none)) /*
  */ xtitle("Duration time", size(large)) xscale(titlegap(*5)) /* 
  */ ytitle("Weibull density", size(large)) yscale(titlegap(*5)) /*
  */ xlabel(,labsize(medlarge)) ylabel(,labsize(medlarge)) 
graph save ch17fig2a, replace

* Weibull survivor
graph twoway (scatter surv t, c(l) msize(vtiny) clwidth(medthick) clstyle(p1)), /*
  */ scale (1.2) plotregion(style(none)) /* 
  */ xtitle("Duration time", size(large)) xscale(titlegap(*5)) /* 
  */ ytitle("Weibull survivor", size(large)) yscale(titlegap(*5)) /*
  */ xlabel(,labsize(medlarge)) ylabel(,labsize(medlarge))
graph save ch17fig2b, replace

* Weibull hazard
graph twoway (scatter hazard t, c(l) msize(vtiny) clwidth(medthick) clstyle(p1)), /*
  */ scale (1.2) plotregion(style(none)) /*
  */ xtitle("Duration time", size(large)) xscale(titlegap(*5)) /* 
  */ ytitle("Weibull hazard", size(large)) yscale(titlegap(*5)) /*
  */ xlabel(,labsize(medlarge)) ylabel(,labsize(medlarge))
graph save ch17fig2c, replace

* Weibull cumulative hazard
graph twoway (scatter cumhaz t, c(l) msize(vtiny) clwidth(medthick) clstyle(p1)), /*
  */ scale (1.2) plotregion(style(none)) /*
  */ xtitle("Duration time", size(large)) xscale(titlegap(*5)) /* 
  */ ytitle("Cumulative hazard", size(large)) yscale(titlegap(*5)) /*
  */ xlabel(,labsize(medlarge)) ylabel(,labsize(medlarge))
graph save ch17fig2d, replace

********** COMBINE THE FOUR GRAPHS FOR FIGURE 17.2 (page 585) **********

graph combine ch17fig2a.gph ch17fig2b.gph ch17fig2c.gph ch17fig2d.gph, /*
  */ title("Weibull Distribution", margin(b=2) size(vlarge)) 
graph export ch17weibull.wmf, replace

********** CLOSE OUTPUT
log close
* clear
* exit
