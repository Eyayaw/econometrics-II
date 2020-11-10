* MMA19P1COMPRISKS.DO  march 2005 for Stata version 8.0

log using mma19p1comprisks.txt, text replace

********** OVERVIEW OF MMA18P1COMPRISKS.DO **********

* STATA Program 
* copyright C 2005 by A. Colin Cameron and Pravin K. Trivedi 
* used for "Microeconometrics: Methods and Applications" 
* by A. Colin Cameron and Pravin K. Trivedi (2005)
* Cambridge University Press 

* Chapter 19.5 pages 658-62
* Competing Risks Example with censoring mechanism each of the three risks
*   (1A) Table 19.2 p.659  Exponential
*   (1B) Table 19.2 p.659  Exponential with IG frailty
*   (2A) Table 19.3 p.659  Weibull
*   (2B) Table 19.3 p.659  Weibull with IG frailty
*   (2C) Table 19.3 p.660  Cox model
*   (2D) Graph the resulting Cox baseline survival and cumulative hazards
*        Figure 19.1: (combined_bsf.wmf) baseline survival functions
*        Figure 19.2: (combined_cbh.wmf) baseline cumulative hazards

* To run this program you need data file
*    ema1996.dta

* NOTE: The IG Heterogeneity estimation was unsuccessful for exponential
*       but successful for Weibull

********** SETUP **********

set more off
version 8
set scheme s1mono   /* Used for graphs */
set matsize 80     /* Needed for this program */
  
********** DATA DESCRIPTION **********

* The data is from 
* B.P. McCall (1996), "Unemployment Insurance Rules, Joblessness, 
*                      and Part-time Work," Econometrica, 64, 647-682.

* There are 3343 observations from the CPS Displaced Worker Surveys
* of 1986, 1988, 1990 and 1992 on 33 variables including
*    spell = length of spell in number of two-week intervals
*  CENSOR1 = 1 if re-employed at full-time job
*  CENSOR2 = 1 if re-employed at part-time job
*  CENSOR3 = 1 if re-employed but left job: pt-ft status unknown
*  CENSOR4 = 1 if still jobless

* See program mma17p4duration.do for further description of the data set

********** READ DATA and CREATE ADDITIONAL VARIABLES **********

use ema1996.dta

gen RR = reprate
gen DR = disrate
gen UI = ui
gen RRUI = RR*UI
gen DRUI = DR*UI
gen LOGWAGE = logwage
sum

********* COMPETING RISKS FOR UNEMPLOYMENT DURATION **********

* Stata analysis requires using stset to define the dependent variable
* and the censoring variable if there is one

* For the competing risks model there are three censoring variables
*  CENSOR1 = 1 if re-employed at full-time job
*  CENSOR2 = 1 if re-employed at part-time job
*  CENSOR3 = 1 if re-employed but left job: pt-ft status unknown

* Define $xlist = list of regressors used in subsequent regressions
global xlist RR DR UI RRUI DRUI LOGWAGE /* 
  */ tenure slack abolpos explose stateur houshead married /*
  */ female child ychild nonwhite age schlt12 schgt12 smsa bluecoll /*
  */ mining constr transp trade fire services pubadmin /*
  */ year85 year87 year89 midatl /*
  */ encen wncen southatl escen wscen mountain pacific

*** (1A) EXPONENTIAL WITH NO HETEROGENEITY  Table 19.2

stset spell, fail(censor1=1)
streg $xlist, nolog nohr robust dist(exponential)
estimates store bexpr1

stset spell, fail(censor2=1)
streg $xlist, nolog nohr robust dist(exponential)
estimates store bexpr2

stset spell, fail(censor3=1)
streg $xlist, nolog nohr robust dist(exponential)
estimates store bexpr3

* Table 19.2 (page 658) first three columns
estimates table bexpr1 bexpr2 bexpr3, b(%10.3f) se(%10.3f) stats(N ll) /*
 */ keep(RR DR UI RRUI DRUI LOGWAGE tenure)

*** (1B) EXPONENTIAL WITH IG HETEROGENEITY  Table 19.2

/* Did not work even though Weibull with IG heterogeneity did

stset spell, fail(censor1=1)
streg $xlist, nohr robust dist(exponential) frailty(invgauss)
estimates store bexpigr1

stset spell, fail(censor2=1)
streg $xlist, nolog nohr robust dist(exponential) frailty(invgauss)
estimates store bexpigr2

stset spell, fail(censor3=1)
streg $xlist, nolog nohr robust dist(exponential)
estimates store bexpiggr3

* Table 19.2 (page 658) first three columns
estimates table bexpigr1 bexpigr2 bexpigr3, b(%10.3f) se(%10.3f) stats(N ll) /*
 */ keep(RR DR UI RRUI DRUI LOGWAGE tenure)

*/

*** (2A) WEIBULL WITH NO HETEROGENEITY  Table 19.3

stset spell, fail(censor1=1)
streg $xlist, nolog nohr robust dist(weibull) 
estimates store bweibr1

stset spell, fail(censor2=1)
streg $xlist, nolog nohr robust dist(weibull)
estimates store bweibr2

stset spell, fail(censor3=1)
streg $xlist, nolog nohr robust dist(weibull)
estimates store bweibr3

* Table 19.3 (page 659) first three columns
estimates table bweibr1 bweibr2 bweibr3, b(%10.3f) se(%10.3f) stats(N ll) /*
 */ keep(RR DR UI RRUI DRUI LOGWAGE tenure)

*** (2B) WEIBULL WITH IG HETEROGENEITY  Table 19.3

stset spell, fail(censor1=1)
streg $xlist, nohr robust dist(weibull) frailty(invgauss)
estimates store bweibigr1

stset spell, fail(censor2=1)
streg $xlist, nolog nohr robust dist(weibull) frailty(invgauss)
estimates store bweibigr2

stset spell, fail(censor3=1)
streg $xlist, nolog nohr robust dist(weibull) frailty(invgauss)
estimates store bweibigr3

* Table 19.3 (page 659) first three columns
estimates table bweibigr1 bweibigr2 bweibigr3, b(%10.3f) se(%10.3f) stats(N ll) /*
 */ keep(RR DR UI RRUI DRUI LOGWAGE tenure)

*** (2C) ESTIMATE COX MODEL SPECIFICATION OF COMPETING RISKS

stset spell, fail(censor1=1)
stcox $xlist, nolog nohr robust basesurv(survrisk1) basechazard(chrisk1)
estimates store bcoxrisk1

stset spell, fail(censor2=1)
stcox $xlist, nolog nohr robust basesurv(survrisk2) basechazard(chrisk2)
estimates store bcoxrisk2

stset spell, fail(censor3=1)
stcox $xlist, nolog nohr robust basesurv(survrisk3) basechazard(chrisk3)
estimates store bcoxrisk3

* Table 19.3 (page 659) last three columns
* NOTE: The results from this program differ a little from those 
*       given in text.  Need to resolve this.
estimates table bcoxrisk1 bcoxrisk2 bcoxrisk3, b(%10.3f) se(%10.3f) stats(N ll) /*
 */ keep(RR DR UI RRUI DRUI LOGWAGE tenure)

*** (2D) GRAPHS FOR COX COMPETING RISKS MODEL

* Figure 19.1 (page 661) - Plot the three baseline survival functions
sort _t
graph twoway (scatter survrisk1 _t, c(J) msymbol(i) msize(small) clstyle(p1)) /*
  */ (scatter survrisk2 _t, c(J) msymbol(i) msize(small) clstyle(p2)) /*
  */ (scatter survrisk3 _t, c(J) msymbol(i) msize(small) clstyle(p3)), /*
  */ scale (1.2) plotregion(style(none)) /*
  */ title("Baseline Survival Functions") /*
  */ xtitle("Unemployment Duration in 2-week intervals", size(medlarge)) xscale(titlegap(*5)) /* 
  */ ytitle("Baseline Survival Probability", size(medlarge)) yscale(titlegap(*5)) /*
  */ legend(pos(3) ring(0) col(1)) legend(size(small)) /*
  */ legend( label(1 "Risk 1 (full-time job)") label(2 "Risk 2 (part-time job)") label(3 "Risk 3 (unknown job)"))
graph export combined_bsf.wmf, replace

* Figure 19.2 (page 659) - Plot the three baseline cumulative hazards
sort _t
graph twoway (scatter chrisk1 _t, c(J) msymbol(i) msize(small) clstyle(p1)) /*
  */ (scatter chrisk2 _t, c(J) msymbol(i) msize(small) clstyle(p2)) /*
  */ (scatter chrisk3 _t, c(J) msymbol(i) msize(small) clstyle(p3)), /*
  */ scale (1.2) plotregion(style(none)) /*
  */ title("Baseline Cumulative Hazard Functions") /*
  */ xtitle("Unemployment Duration in 2-week intervals", size(medlarge)) xscale(titlegap(*5)) /* 
  */ ytitle("Baseline Cumulative Hazard", size(medlarge)) yscale(titlegap(*5)) /*
  */ legend(pos(11) ring(0) col(1)) legend(size(small)) /*
  */ legend( label(1 "Risk 1 (full-time job)") label(2 "Risk 2 (part-time job)") label(3 "Risk 3 (unknown job)"))
graph export combined_cbh.wmf, replace

********** CLOSE OUTPUT **********
log close
* clear
* exit

