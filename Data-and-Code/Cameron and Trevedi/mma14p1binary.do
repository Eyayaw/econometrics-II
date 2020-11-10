* MMA14P1BINARY.DO  March 2005 for Stata version 8.0

log using mma14p1binary.txt, text replace

********** OVERVIEW OF MMA14P1BINARY.DO **********

* STATA Program 
* copyright C 2005 by A. Colin Cameron and Pravin K. Trivedi 
* used for "Microeconometrics: Methods and Applications" 
* by A. Colin Cameron and Pravin K. Trivedi (2005)
* Cambridge University Press 

* Chapter 14.2 (pages 464-6)  Logit and probit models.
* Provides  
*   (1) Table 14.1:  Data summary
*   (2) Table 14.2:  Logit, Probit and OLS slope estimates
*   (3) Figure 14.1: Plot of Logit Probit and OLS predicted probabilities

* To run this program you need data file
*    Nldata.asc 

********** SETUP

set more off
version 8.0
set scheme s1mono  /* Graphics scheme */
  
********** DATA DESCRIPTION

* Data Set comes from :
* J. A. Herriges and C. L. Kling, 
* "Nonlinear Income Effects in Random Utility Models", 
* Review of Economics and Statistics, 81(1999): 62-72

* The data are given as a combined observation with data on all 4 choices.
* This will work for multinomial logit program.
* For conditional logit will need to make a new data set which has
* four separate entries for each observation as there are four alternatives. 

* Filename: NLDATA.ASC
* Format: Ascii
* Number of Observations: 1182
* Each observations appears over 3 lines with 4 variables per line 
* so 4 x 1182 = 4728 observations 
* Variable Number and Description
* 1	Recreation mode choice. = 1 if beach, = 2 if pier; = 3 if private boat; = 4 if charter
* 2	Price for chosen alternative
* 3	Catch rate for chosen alternative
* 4	= 1 if beach mode chosen; = 0 otherwise
* 5	= 1 if pier mode chosen; = 0 otherwise
* 6	= 1 if private boat mode chosen; = 0 otherwise
* 7	= 1 if charter boat mode chosen; = 0 otherwise
* 8	= price for beach mode
* 9	= price for pier mode
* 10	= price for private boat mode
* 11	= price for charter boat mode
* 12	= catch rate for beach mode
* 13	= catch rate for pier mode
* 14	= catch rate for private boat mode
* 15	= catch rate for charter boat mode
* 16	= monthly income

********** READ IN DATA **********

infile mode price crate dbeach dpier dprivate dcharter pbeach ppier /*
   */ pprivate pcharter qbeach qpier qprivate qcharter income /*
   */ using nldata.asc

* Divide income by 1000 so that results are easy to read 
gen ydiv1000 = income/1000

label define modetype 1 "beach" 2 "pier" 3 "private" 4 "charter"
label values mode modetype
summarize

********** CREATE BINARY DATA: CHARTER vs PIER **********

* Binary logit of charter (mode = 2) versus pier (mode = 4)
keep if mode == 2 | mode == 4
* charter is 1 if fish from charter boat and 0 if fish from pier
gen charter = 0
replace charter = 1 if mode == 4

gen pratio = 100*ln(pcharter/ppier)
gen lnrelp = ln(pchart/ppier)

* Overall summary
summarize
* Summary by charter or by pier
sort mode
by mode: summarize

* Write final data to a text (ascii) file so can use with programs other than Stata
* And also used for LIMDEP program mma14p2maxscore.lim
outfile charter lnrelp using mma14p1binary.asc, replace

********** TABLE 14.1 - DATA SUMMARY BY OUTCOME AND OVERALL **********

* Following gives Table 14.1 page 464
summarize charter pcharter ppier lnrelp
sort mode
by mode: summarize charter pcharter ppier lnrelp

********** TABLE 14.2 - ESTIMATE LOGIT, PROBIT AND OLS MODELS

logit charter lnrelp
estimates store blogit

probit charter lnrelp
estimates store bprobit

regress charter lnrelp
estimates store bOLS

* Heteroskedastic robust standard errors only needed for OLS 
* but given for other models for completeness

logit charter lnrelp, robust
estimates store bloghet

probit charter lnrelp, robust
estimates store bprobhet

regress charter lnrelp, robust
estimates store bOLShet

* Following gives Table 14.2 page 465
estimates table blogit bprobit bOLS bloghet bprobhet bOLShet, /*
   */ t stats(N ll r2 r2_p) b(%8.3f) keep(_cons lnrelp)

********** FIGURE 14.1 - PLOT PREDICTED PROBABILITY AGAINST X FOR MODELS

quietly logit charter lnrelp
predict plogit, p

quietly probit charter lnrelp
predict pprobit, p

quietly regress charter lnrelp
predict pOLS

sum charter plogit pprobit pOLS

sort lnrelp

* Following gives Figure 14.1 page 466
graph twoway (scatter charter lnrelp, msize(vsmall) jitter(3)) /*
  */ (line plogit lnrelp, clstyle(p1)) /*
  */ (line pprobit lnrelp, clstyle(p2)) /*
  */ (line pOLS lnrelp, clstyle(p3)), /*
  */ scale (1.2) plotregion(style(none)) /*
  */ title("Predicted Probabilities Across Models") /*
  */ xtitle("Log relative price (lnrelp)", size(medlarge)) xscale(titlegap(*5)) /* 
  */ ytitle("Predicted probability", size(medlarge)) yscale(titlegap(*5)) /*
  */ legend(pos(1) ring(0) col(1)) legend(size(small)) /*
  */ legend( label(1 "Actual Data (jittered)") label(2 "Logit") /*
  */         label(3 "Probit") label(4 "OLS"))
graph export ch14binary.wmf, replace

********** CLOSE OUTPUT **********
log close
* clear
* exit
