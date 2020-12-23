* MMA15P2GEV.DO  March 2005 for Stata 8.0

log using mma15p2gev.txt, text replace

********** OVERVIEW OF MMA15P2GEV.DO **********

* STATA Program 
* copyright C 2005 by A. Colin Cameron and Pravin K. Trivedi 
* used for "Microeconometrics: Methods and Applications" 
* by A. Colin Cameron and Pravin K. Trivedi (2005)
* Cambridge University Press 

* Chapter 15.6.3 page 511
* Nested logit (GEV) model analysis.
*   (1)  Set data up and reproduce Mixed estimates in Table 15.2 p.493
*   (2A) Nested logit model estimates (page 511)
*   (2B) Restricted nested logit model estimates (page 511)
*   (2C) Equivalent conditional logit model estimates (same as (2B))

* Related programs are 
*    mma15p1mnl.do   multinomial and conditional logit using Stata
*    mma15p3mnl.lim  multinomial logit using Limdep
*    mma15p4gev.lim  conditional and nested logit using Limdep and Nlogit

* To run this program you need data file
*    Nldata.asc 

* NOTE: The example here is deliberately simple and merely illustrative.
*       with nesting structure 
*             /     \
*           /  \   /  \
* In this case with parameter rho_j differing across alternatives
* Stata 8 estimates the earlier variant of the nested logit model
* rather than the preferred variant given in the text.
* See the discussion at bottom of page 511 and also Train (2003, p.88)

********** SETUP **********

set more off
version 8.0
set scheme s1mono  /* Graphics scheme */
  
********** DATA DESCRIPTION **********

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

******* (1) CONDITIONAL LOGIT MODEL (Table 15.2 p.493 Mixed column) *********

infile mode price crate dbeach dpier dprivate dcharter pbeach ppier /*
   */ pprivate pcharter qbeach qpier qprivate qcharter income /*
   */ using nldata.asc

gen ydiv1000 = income/1000

* Data are one entry per individual
* Need to reshape to 4 observations per individual - one for each alternative
* Use reshape to do this which also creates variable (see below)
*   alternatv = 1 if beach, = 2 if pier; = 3 if private boat; = 4 if charter
gen id = _n
gen d1 = dbeach
gen p1 = pbeach
gen q1 = qbeach
gen d2 = dpier
gen p2 = ppier
gen q2 = qpier
gen d3 = dprivate
gen p3 = pprivate
gen q3 = qprivate
gen d4 = dcharter
gen p4 = pcharter
gen q4 = qcharter
summarize

reshape long d p q, i(id) j(alterntv)
* This automatically creates alterntv = 1 (beach), ... 4 (charter)
describe
summarize

* Bring in alternative specific dummies
* Since d2-d4 already used instead call them dummy2 - dummy4
gen obsnum=_n
gen dummy1 = (mod(obsnum,4)==1) * 1
gen dummy2 = (mod(obsnum,4)==2) * 1
gen dummy3 = (mod(obsnum,4)==3) * 1
gen dummy4 = (mod(obsnum,4)==0) * 1
gen d1y = (mod(obsnum,4)==1) * ydiv1000
gen d2y = (mod(obsnum,4)==2) * ydiv1000
gen d3y = (mod(obsnum,4)==3) * ydiv1000
gen d4y = (mod(obsnum,4)==0) * ydiv1000

summarize

* The following gives Mixed column of Table 15.2 p.493
* Note that dummy1 and d1y are omitted to avoid dummy variablle trap

clogit d dummy2 dummy3 dummy4 d2y d3y d4y p q, group(id)

******* (2) NESTED LOGIT MODEL (p.511) *********

* Define the Tree for Nested logit
*       with nesting structure 
*             /     \
*           /  \   /  \
* In this case with parameter rho_j differing across alternatives
* Stata 8 estimates the earlier variant of the nested logit model
* rather than the preferred variant given in the text.
* See the discussion at bottom of page 511 and also Train (2003, p.88)

nlogitgen type = alterntv(shore: 1 | 2 , boat: 3 | 4)
nlogittree alterntv type

*** (2A) Estimate the nested logit model 
***      This is the model on p.511 that has "higher log-likelihood"

* For the top level we use regressors that do not vary at the lower level
* So not p or q, but could be income or alternative dummy 
* Here use income and alternative dummy
gen dshore = (type ==1) * 1
gen dshorey = (type ==1) * ydiv1000
nlogit d (alterntv = p q) (type = dshore dshorey), group(id)
estimates store nlogitunrest

*** (2B) Estimate the restricted nested logit model 
***      This is the model on p.511 that has log L = -1252

* Set the inclusive value parameters to 1 
nlogit d (alterntv = p q) (type = dshore dshorey), group(id) ivc(shore=1, boat=1)
estimates store nlogitrest

* Perform a likelihood ratio test that inclusive parameters = 1
lrtest nlogitunrest nlogitrest

*** (2C) As a check, verify that this restricted nested logit = conditional logit

clogit d p q dshore dshorey, group(id)

********** CLOSE OUTPUT **********
log close
clear 
exit


