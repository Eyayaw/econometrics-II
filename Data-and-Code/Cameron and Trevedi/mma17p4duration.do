* MMA17P4DURATION.DO  March 2005 for Stata version 8.0

log using mma17p4duration.txt, text replace

********** OVERVIEW OF MMA17P4DURATION.DO **********

* STATA Program 
* copyright C 2005 by A. Colin Cameron and Pravin K. Trivedi 
* used for "Microeconometrics: Methods and Applications" 
* by A. Colin Cameron and Pravin K. Trivedi (2005)
* Cambridge University Press 

* Chapter 17.11 (pages 603-8)
* Duration regression with censored data example
* Provides  
*   (1) Data summary: Table 17.6 
*   (2) List of Survivor Function and Cumulative Hazard Estimates: Table 17.7
*   (3) Various graphs describing the data
*       (3A) K-M Survival Graph for all data (Figure 17.3: km_pt1.wmf) 
*       (3B) K-M Survival Graph by unemployment insurance (Figure 17.4: km_pt2.wmf) 
*       (3C) N-A Cumulative Hazard Graph for all data (Figure 17.5: na_pt1.wmf) 
*       (3D) N-A Cumulative Hazard Graph by unemployment insurance (Figure 17.6: na_pt2.wmf) 
*   (4) Coefficient Estimates of Some Parametric Models (Table 17.8)
*   (4) Hazard Rate Estimates of Some Parametric Models (Table 17.9)

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

* McCalls data set named ema_1996_pt_lastweek.dta 
* has name changed to ema1996.dta
 
* There are 3343 observations from the CPS Displaced Worker Surveys
* of 1986, 1988, 1990 and 1992 
* 1. spell  is length of spell in number of two-week intervals
* 2. CENSOR1 = 1 if re-employed at full-time job
* 3. CENSOR2 = 1 if re-employed at part-time job
* 4. CENSOR3 = 1 if re-employed but left job: pt-ft status unknown
* 5. CENSOR4 = 1 if still jobless
* 6. ui (UI) = 1 if filed UI claim
* 7. reprate (RR) = eligible replacement rate
* 8. disrate (DR) = eligible disregard rate
* 9. tenure (TENURE) = years tenure in lost job
* 10. logwage (LOGWAGE) = log weekly earnings in lost job (1985$)
* 11.-43. other variables listed in McCall (1986) table 2 p.657

********** READ DATA **********
 
use ema1996.dta
sum

* The following gives variables in same order as Table 2 p.657 of McCall (1996) 
* which gives fuller names for the variables
sum spell censor1 censor2 censor3 censor4 age /*
 */ ui reprate disrate logwage tenure slack abolpos explose bluecoll /*
 */ houshead married child ychild female schlt12 schgt12 nonwhite smsa /*
 */ midatl encen wncen southatl escen wscen mountain pacific /*
 */ mining constr transp trade fire services pubadmin /*
 */ year85 year87 year89 

* The following creates a space-delimited data set with 
* variables in same order as Table 2 p.657 of McCall (1996) 
* Permits use by programs other than Stata
* Note that order has been changed a little from the original Stata data set

outfile spell censor1 censor2 censor3 censor4 age /*
 */ ui reprate disrate logwage tenure slack abolpos explose bluecoll /*
 */ houshead married child ychild female schlt12 schgt12 nonwhite smsa /*
 */ midatl encen wncen southatl escen wscen mountain pacific /*
 */ mining constr transp trade fire services pubadmin /*
 */ year85 year87 year89 using ema1996.asc, replace
 
********* ANALYSIS: UNEMPLOYMENT DURATION ********** 

* Stata st curves require defining the dependent variable
* and the censoring variable if there is one
stset spell, fail(censor1=1)
stdes

* (1) SUMMARIZE KEY VARIABLES (Table 17.6, p.603) 

sum spell censor1 censor2 censor3 censor4 ui reprate disrate tenure logwage 

* (2) LIST SURVIVAL CURVE AND CUMULATIVE HAZARD ESTIMATES (Table 17.7, p.605) 

* Kaplan-Meier Estimates of Survival Function
sts list

* Nelson-Aalen Estimates of Cumulative Hazard
sts list, na

* (3) VARIOUS GRAPHS (Figures 17.3-17.6) 

* (3A) Figure 17.3: Overall Survival Function (page 604) 
* sts graph, gwood 
* Nicer graphs and also confidence bands are bolder and easier to read
sts gen surv = s
sts gen lbsurv = lb(s)
sts gen ubsurv = ub(s)
sort spell
graph twoway (line ubsurv spell, msize(vtiny) mstyle(p2) c(J) clstyle(p1) clcolor(gs10)) /* 
  */ (line surv spell, msize(vtiny) mstyle(p1) c(J) clstyle(p1)) /* 
  */ (line lbsurv spell, msize(vtiny) mstyle(p2) c(J) clstyle(p1) clcolor(gs10)), /*
  */ scale(1.2) plotregion(style(none)) /*
  */ title("Overall Survival Function Estimate") /*
  */ xtitle("Unemployment Duration in 2-week intervals", size(medlarge)) xscale(titlegap(*5)) /* 
  */ ytitle("Survival Probability", size(medlarge)) yscale(titlegap(*5)) /*
  */ ylabel(0.00(0.25)1.00,grid)/*
  */ legend(pos(1) ring(0) col(1)) legend(size(small)) /*
  */ legend( label(1 "Upper 95% confidence band") label(2 "Survival Estimate") /*
  */         label(3 "Lower 95% confidence band") )
graph export km_pt1.wmf, replace

* (3B) Figure 17.4: Survival Function by Treatment (here ui) (p.605)
* sts graph, by(ui) 
sts graph, by(ui) /*
  */ scale (1.2) plotregion(style(none)) /*
  */ title("Survival Function Estimates by UI Status") /*
  */ xtitle("Unemployment Duration in 2-week intervals", size(medlarge)) xscale(titlegap(*5)) /* 
  */ ytitle("Survival Probability", size(medlarge)) yscale(titlegap(*5)) /*
  */ legend(pos(1) ring(0) col(1)) legend(size(small)) /*
  */ legend(label(1 "No UI (UI = 0)") label(2 "Received UI (UI = 1)") ) 
graph export km_pt2.wmf, replace

* (3C) Figure 17.5: Overall Cumulative Hazard Function (p.606)
* sts graph, cna 
* Nicer graphs and also confidence bands are bolder and easier to read
sts gen cumhaz = na
sts gen lbcumhaz = lb(na)
sts gen ubcumhaz = ub(na)
sort spell
graph twoway (line ubcumhaz spell, msize(vtiny) mstyle(p2) c(J) clstyle(p1) clcolor(gs10)) /* 
  */ (line cumhaz spell, msize(vtiny) mstyle(p1) c(J) clstyle(p1)) /* 
  */ (line lbcumhaz spell, msize(vtiny) mstyle(p2) c(J) clstyle(p1) clcolor(gs10)), /*
  */ scale(1.2) plotregion(style(none)) /*
  */ title("Overall Cumulative Hazard Estimate") /*
  */ xtitle("Unemployment Duration in 2-week intervals", size(medlarge)) xscale(titlegap(*5)) /* 
  */ ytitle("Cumulative Hazard", size(medlarge)) yscale(titlegap(*5)) /*
  */ ylabel(0.00(0.50)1.50,grid)/*
  */ legend(pos(11) ring(0) col(1)) legend(size(small)) /*
  */ legend( label(1 "Upper 95% confidence band") label(2 "Cumulative Hazard Estimate") /*
  */         label(3 "Lower 95% confidence band") )
graph export na_pt1.wmf, replace

* (3D) Figure 17.6: Cumulative Hazard Function by Treatment (here ui) (p.606)
* sts graph, na by(ui) 
sts graph, na by(ui) /*
  */ scale (1.2) plotregion(style(none)) /*
  */ title("Cumulative Hazard Estimates by UI Status") /*
  */ xtitle("Unemployment Duration in 2-week intervals", size(medlarge)) xscale(titlegap(*5)) /* 
  */ ytitle("Cumulative Hazard", size(medlarge)) yscale(titlegap(*5)) /*
  */ legend(pos(1) ring(0) col(1)) legend(size(small)) /*
  */ legend(label(1 "No UI (UI = 0)") label(2 "Received UI (UI = 1)") ) 
graph export na_pt2.wmf, replace

* (4) VARIOUS PARAMETRIC MODELS: COEFFICIENTS (Table 17.8)

* streg default is to report hazard rates ratehr than coeffcients
* streg with nohr option reports coefficients

* Create regressors
gen RR = reprate
gen DR = disrate
gen UI = ui
gen RRUI = RR*UI
gen DRUI = DR*UI
gen LOGWAGE = logwage

* Define $xlist = list of regressors used in subsequent regressions
global xlist RR DR UI RRUI DRUI LOGWAGE /*
*/ tenure slack abolpos explose stateur houshead married /*
*/ female child ychild nonwhite age schlt12 schgt12 smsa bluecoll /*
*/ mining constr transp trade fire services pubadmin /*
*/ year85 year87 year89 midatl /*
*/ encen wncen southatl escen wscen mountain pacific

* Exponential regression
streg $xlist, nohr robust dist(exponential)
estimates store bexponential

* Weibull regression
streg $xlist, nohr robust dist(weibull)
estimates store bweibull

* Gompertz regression
streg $xlist, nohr robust dist(gompertz)
estimates store bgompertz

* Weibull regression
stcox $xlist, nohr robust 
estimates store bcox

* Display Results for Table 17.8 (page 607)
estimates table bexponential bweibull bgompertz, t stats(N ll) b(%8.3f) /*
 */ keep(RR DR UI RRUI DRUI LOGWAGE _cons) 
estimates table bcox, t stats(N ll) b(%8.3f) keep(RR DR UI RRUI DRUI LOGWAGE) 

* (5) VARIOUS PARAMETRIC MODELS: HAZARD RATIOS (Table 17.9, page 608))

* streg default is to report hazard rates rather than coeffcients
* streg with nohr option reports coefficients

* Exponential regression
streg $xlist, robust dist(exponential)

* Weibull regression
streg $xlist, robust dist(weibull)

* Gompertz regression
streg $xlist, robust dist(gompertz)

* Cox regression
stcox $xlist, robust 

* Display results for Table 17.9 page 608
* Not possible here as estimates table gives coefficients not hazard rates
* Instead need to use output for each model 
* Not sure why t-statistics differ somewhat from those in Table 17.9
 
********** CLOSE OUTPUT **********
log close
* clear
* exit


