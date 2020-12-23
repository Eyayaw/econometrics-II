* MMA18P1HETEROGENEITY.DO  March 2005 for Stata version 8.0

log using mma18p1heterogeneity.txt, text replace

********** OVERVIEW OF MMA18P1HETEROGENEITY.DO **********

* STATA Program 
* copyright C 2005 by A. Colin Cameron and Pravin K. Trivedi 
* used for "Microeconometrics: Methods and Applications" 
* by A. Colin Cameron and Pravin K. Trivedi (2005)
* Cambridge University Press

* Chapter 18.8 Pages 632-6
* Unobserved Heterogeneity with Duration data Example
*   (1) Exponential with and without heterogeneity
*       Residuals Plots: Figures 18.2 (exp.wmf) and 18.3 (exp_gamma.wmf)
*       Tabulate Model Estimates: Table 18.1
*   (2) Weibull with and without heterogeneity: Generalized Residuals Plots
*       Residuals Plots: Figures 18.4 (Weibul16.wmf) and 18.5 (Weibul16_IG.wmf)
*       Tabulate model Estimates: Table 18.2

* To run this program you need data file
*    ema1996.dta

********** SETUP **********

set more off
version 8.0
set scheme s1mono   /* Used for graphs */
set matsize 100
  
********** DATA DESCRIPTION **********

* The data is from 
* B.P. McCall (1996), "Unemployment Insurance Rules, Joblessness, 
*                      and Part-time Work," Econometrica, 64, 647-682.

* There are 3343 observations from the CPS Displaced Worker Surveys
* of 1986, 1988, 1990 and 1992 on 33 variables including
*    spell = length of spell in number of two-week intervals
*  CENSOR1 = 1 if re-employed at full-time job

* See program mma17p4duration.do for further description of the data set

********** READ DATA **********

use ema1996.dta

********** CREATE ADDITIONAL VARIABLES **********

gen RR = reprate
gen DR = disrate
gen UI = ui
gen RRUI = RR*UI
gen DRUI = DR*UI
gen LOGWAGE = logwage
sum

********* ANALYSIS: UNEMPLOYMENT DURATION **********

* Stata st curves require defining the dependent variable
* and the censoring variable if there is one
stset spell, fail(censor1=1)
stdes

* Define $xlist = list of regressors used in subsequent regressions
global xlist RR DR UI RRUI DRUI LOGWAGE /*
*/ tenure slack abolpos explose stateur houshead married /*
*/ female child ychild nonwhite age schlt12 schgt12 smsa bluecoll /*
*/ mining constr transp trade fire services pubadmin /*
*/ year85 year87 year89 midatl /*
*/ encen wncen southatl escen wscen mountain pacific

* (1) EXPONENTIAL REGRESSION

* Estimate exponential without heterogeneity
streg $xlist, nolog nohr dist(exponential) robust
estimates store bexp

* Figure 18.2 (p.633) - Generalized (Cox-Snell) Residuals for Exponential
predict resid, csnell
stset resid, fail(censor1)
sts generate survivor=s
generate cumhaz = -ln(survivor)
sort resid
graph twoway (scatter cumhaz resid, c(J) msymbol(i) msize(small) clstyle(p1)) /*
  */ (scatter resid resid, c(l) msymbol(i) msize(small) clstyle(p2)), /*
  */ scale (1.2) plotregion(style(none)) /*
  */ title("Exponential Model Residuals") /*
  */ xtitle("Generalized (Cox-Snell) Residual", size(medlarge)) xscale(titlegap(*5)) /* 
  */ ytitle("Cumulative Hazard", size(medlarge)) yscale(titlegap(*5)) /*
  */ legend(pos(6) ring(0) col(1)) legend(size(small)) /*
  */ legend( label(1 "Cumulative Hazard") label(2 "45 degree line"))
graph export exp.wmf, replace
drop resid survivor cumhaz

* Estimate exponential with gamma heterogeneity
stset spell, fail(censor1)
streg $xlist, nolog nohr dist(exponential) frailty(gamma) robust
estimates store bexpgamma

* Figure 18.3 (p.633) - Generalized (Cox-Snell) Residuals for Exponential-Gamma
predict resid, csnell
stset resid, fail(censor1)
sts generate survivor=s
generate cumhaz = -ln(survivor)
sort resid
graph twoway (scatter cumhaz resid, c(J) msymbol(i) msize(small) clstyle(p1)) /*
  */ (scatter resid resid, c(l) msymbol(i) msize(small) clstyle(p2)), /*
  */ scale (1.2) plotregion(style(none)) /*
  */ title("Exponential-Gamma Model Residuals") /*
  */ xtitle("Generalized (Cox-Snell) Residual", size(medlarge)) xscale(titlegap(*5)) /* 
  */ ytitle("Cumulative Hazard", size(medlarge)) yscale(titlegap(*5)) /*
  */ legend(pos(6) ring(0) col(1)) legend(size(small)) /*
  */ legend( label(1 "Cumulative Hazard") label(2 "45 degree line"))
graph export exp_gamma.wmf, replace
drop resid survivor cumhaz

/*
* Following did not work, even with starting values provided 
* Results in book obtained on different computer with different Stata version
* Estimate exponential with IG heterogeneity
stset spell, fail(censor1=1)
quietly streg $xlist, nolog nohr dist(exponential) robust
matrix theta = 1.6
matrix bstart = e(b),theta
streg $xlist, nohr dist(exponential) frailty(invgauss) robust from(bstart)
* estimates store bexpIG
*/

* Table 18.1 (p.634) - Display Parameter Estimates
* Note that exponetial-IG missing
estimates table bexp bexpgamma, t(%9.3f) stats(N ll) b(%9.3f) /*
 */ keep(RR DR UI RRUI DRUI LOGWAGE _cons) 

* (2) WEIBULL REGRESSION

* Estimate Weibull without heterogeneity
stset spell, fail(censor1=1)
streg $xlist, nolog nohr dist(weibull) robust
estimates store bweib

* Figure 18.4 (p.635) - Generalized (Cox-Snell) Residuals for Weibull
predict resid, csnell
stset resid, fail(censor1)
sts generate survivor=s
generate cumhaz = -ln(survivor)
sort resid
graph twoway (scatter cumhaz resid, c(J) msymbol(i) msize(small) clstyle(p1)) /*
  */ (scatter resid resid, c(l) msymbol(i) msize(small) clstyle(p2)), /*
  */ scale (1.2) plotregion(style(none)) /*
  */ title("Weibull Model Residuals") /*
  */ xtitle("Generalized (Cox-Snell) Residual", size(medlarge)) xscale(titlegap(*5)) /* 
  */ ytitle("Cumulative Hazard", size(medlarge)) yscale(titlegap(*5)) /*
  */ legend(pos(6) ring(0) col(1)) legend(size(small)) /*
  */ legend( label(1 "Cumulative Hazard") label(2 "45 degree line"))
graph export Weibul16.wmf, replace
drop resid survivor cumhaz

* Estimate Weibull with gamma heterogeneity
stset spell, fail(censor1=1)
streg $xlist, nolog nohr dist(weibull) frailty(invgauss) robust
estimates store bweibIG

* Figure 18.5 (p.636) - Generalized (Cox-Snell) Residuals for Weibull-IG
predict resid, csnell
stset resid, fail(censor1)
sts generate survivor=s
generate cumhaz = -ln(survivor)
sort resid
graph twoway (scatter cumhaz resid, c(J) msymbol(i) msize(small) clstyle(p1)) /*
  */ (scatter resid resid, c(l) msymbol(i) msize(small) clstyle(p2)), /*
  */ scale (1.2) plotregion(style(none)) /*
  */ title("Weibull-IG Model Residuals") /*
  */ xtitle("Generalized (Cox-Snell) Residual", size(medlarge)) xscale(titlegap(*5)) /* 
  */ ytitle("Cumulative Hazard", size(medlarge)) yscale(titlegap(*5)) /*
  */ legend(pos(6) ring(0) col(1)) legend(size(small)) /*
  */ legend( label(1 "Cumulative Hazard") label(2 "45 degree line"))
graph export Weibul16_IG.wmf, replace
drop resid survivor cumhaz

* Table 18.2 (p.635) - Display Parameter Estimates
estimates table bweibIG bweib, t(%9.3f) stats(N ll) b(%9.3f) /*
 */ keep(RR DR UI RRUI DRUI LOGWAGE _cons) 

********** CLOSE OUTPUT **********
log close
* clear
* exit


