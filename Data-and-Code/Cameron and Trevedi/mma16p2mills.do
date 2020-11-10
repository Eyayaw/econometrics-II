* MMA16P2MILLS.DO  March 2005 for Stata version 8.2

log using mma16p2mills.txt, text replace

********** OVERVIEW OF MMA16P2MILLS.DO **********

* STATA Program 
* copyright C 2005 by A. Colin Cameron and Pravin K. Trivedi 
* used for "Microeconometrics: Methods and Applications" 
* by A. Colin Cameron and Pravin K. Trivedi (2005)
* Cambridge University Press 

* Chapter 16.3.4 page 540
* Presentation of Mills ratio
* It provides  
*   (1) Figure 16.1 (ch16millsratio.wmf)  
* This program requires no data

********** SETUP ***********

set more off
version 8
set scheme s1mono   /* Used for graphs */
  
********** GENERATE DATA AND FUNCTIONS

* Create density cdf Mills ratio for N[0,1] 
set obs 100 
gen c = 4*(50-_n)/100
gen PHIc = norm(c)
gen phic = normden(c)
gen lamdac = phic/(1-PHIc)

* Descriptive statistics
summarize

*********** FIGURE 16.2 page 540 ***********

* This graph shows Mills ratio and cdf and density
graph twoway (scatter lamdac c, c(l) msize(vtiny) clstyle(p1) clwidth(medthick)) /* 
  */ (scatter PHIc c, c(l) msize(vtiny) clstyle(p3) clwidth(medthick)) /*
  */ (scatter phic c, c(l) msize(vtiny) clstyle(p2) clwidth(medthick)), /*
  */ scale (1.2) plotregion(style(none)) /*
  */ title("Inverse Mills Ratio as Cutoff Varies") /*
  */ xtitle("Cutoff point c", size(medlarge)) xscale(titlegap(*5)) /* 
  */ ytitle("Inverse Mills, pdf and cdf", size(medlarge)) yscale(titlegap(*5)) /*
  */ legend(pos(11) ring(0) col(1)) legend(size(small)) /*
  */ legend( label(1 "Inverse Mills ratio") label(2 "N[0,1] Cdf") label(3 "N[0,1] Density"))
graph export ch16millsratio.wmf, replace

********** CLOSE OUTPUT ***********
log close
* clear
* exit
