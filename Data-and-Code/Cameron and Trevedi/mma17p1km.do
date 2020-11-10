* MMA17P1KM.DO  March 2005 for Stata version 8.0

log using mma17p1km.txt, text replace

********** OVERVIEW OF MMA17P1KM.DO **********

* STATA Program 
* copyright C 2005 by A. Colin Cameron and Pravin K. Trivedi 
* used for "Microeconometrics: Methods and Applications" 
* by A. Colin Cameron and Pravin K. Trivedi (2005)
* Cambridge University Press 

* Chapter 17.2 (pages 574-5) and 17.5.1 (pages 581-3)
* Nonparametric Duration Analysis
* It provides  
*   (1) Kaplan-Meier Survival Estimate Graph (Figure 17.1: kennanstrk.wmf) 
*   (2) Nelson-Aalen Cumulative Hazard Estimate Graph
*   (3) Kaplan-Meier Survivor Function Estimates (Table 17.3)
*   (4) Shows that Cox regression on intercept gives same results

* To run this program you need data file
*    strkdur.dta 

********** SETUP **********

set more off
version 8
set scheme s1mono   /* Used for graphs */
  
********** DATA DESCRIPTION

* The data is the same data as given in Table 1 of 
*   J. Kennan, "The Duration of Contract strikes in U.S. Manufacturing",
*   Journal of Econometrics, 1985, Vol. 28, pp.5-28.

* There are 566 observations from 1968-1976 with two variables
* 1. dur  is duration of the strike in days
* 2. gdp  is a measure of stage of business cycle
*         (deviation of monthly log industrial production in manufacturing 
*          from prediction from OLS on time, time-squared and monthly dummies)

* All observations are complete for these data. There is no censoring !!
* For an example with censoring see mma17p2kmextra.do or mma17p4duration.do

********** READ DATA **********

use strkdur.dta
sum

* Create ASCII data set so that can use programs other than Stata
outfile dur gdp using strkdur.asc, replace

********* ANALYSIS: NONPARAMETRIC SURVIVAL CURVE AND HAZARD FUNCTION **********

* Stata st curves require defining the dependent variable
stset dur

* The data here are complete. If dur is instead right-censored,
* then also need to define a censoring indicator. For example 
*   stset dur, fail(censor=1)
* where the variable censor=1 if data are right-censored and =0 otherwise
* See mma17p3duration.do

* (1) GRAPH KAPLAN-MEIER SURVIVAL CURVE

* Minimal command that gives 95% confidence bands
sts graph, gwood

* Longer command for Figure 17.1 (page 575)
* Nicer graphs and also confidence bands are bolder and easier to read
sts gen surv = s
sts gen lbsurv = lb(s)
sts gen ubsurv = ub(s)
sort dur
graph twoway (line ubsurv dur, msize(vtiny) mstyle(p2) c(J) clstyle(p1) clcolor(gs10)) /* 
  */ (line surv dur, msize(vtiny) mstyle(p1) c(J) clstyle(p1)) /* 
  */ (line lbsurv dur, msize(vtiny) mstyle(p2) c(J) clstyle(p1) clcolor(gs10)), /*
  */ scale(1.2) plotregion(style(none)) /*
  */ title("Kaplan-Meier Survival Function Estimate") /*
  */ xtitle("Strike duration in days", size(medlarge)) xscale(titlegap(*5)) /* 
  */ ytitle("Survival Probability", size(medlarge)) yscale(titlegap(*5)) /*
  */ ylabel(0.00(0.25)1.00,grid)/*
  */ legend(pos(3) ring(0) col(1)) legend(size(small)) /*
  */ legend( label(1 "Upper 95% confidence band") label(2 "Survival Function") /*
  */         label(3 "Lower 95% confidence band") )
graph export kennanstrk.wmf, replace

* (2) GRAPH NELSON-AALEN CUMULATIVE HAZARD FUNCTION 

* Minimal command that gives 95% confidence bands
sts graph, cna

* Longer command gives nicer figure
sts graph, cna /*
  */ scale (1.2) plotregion(style(none)) /*
  */ title("Nelson-Aalen Cumulative Hazard") /*
  */ xtitle("Strike duration in days", size(medlarge)) xscale(titlegap(*5)) /* 
  */ ytitle("Cumulative Hazard", size(medlarge)) yscale(titlegap(*5)) /*
  */ legend(pos(12) ring(0) col(1)) legend(size(small)) /*
  */ legend(label(1 "95% confidence bands") label(2 "Cumulative Hazard")) 

* (3) LIST SURVIVOR and NELSON-AALEN CUMULATIVE HAZARD ESTIMATES

* Gives a lot of output

* Table 17.2: Kaplan-Meier Survivor Function (page 583)
sts list

* And Nelson-Aalen Integrated Hazard
* sts list, na

* (4) STCOX REGRESS ON INTERCEPT GIVES SAME RESULTS AS ABOVE

* Cox Regression on an intercept
gen one = 1
stcox one, basesurv(coxbasesurv) basechazard(coxbasecumhaz) basehc(coxbasehaz) 

* Instead use sts which analyzes dependent in isolation
* sts gen surv = s
sts gen cumhaz = na
sts gen haz = h

* Compare to verify that same answers
sum surv coxbasesurv cumhaz coxbasecumhaz haz coxbasehaz
corr surv coxbasesurv
corr cumhaz coxbasecumhaz
corr haz coxbasehaz

* (5) ESTIMATE HAZARD FUNCTION 

* sts graph does not give the true hazard function - it instead gives the 
* difference in the cumulative hazard (without division by time difference).

********** CLOSE OUTPUT
log close
* clear
* exit

