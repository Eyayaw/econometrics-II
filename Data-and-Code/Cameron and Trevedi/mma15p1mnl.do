* MMA15P1MNL.DO March 2005 for Stata version 8.0

log using mma15p1mnl.txt, text replace

********** OVERVIEW OF MMA15P1MNL.DO **********

* STATA Program 
* copyright C 2005 by A. Colin Cameron and Pravin K. Trivedi 
* used for "Microeconometrics: Methods and Applications" 
* by A. Colin Cameron and Pravin K. Trivedi (2005)
* Cambridge University Press 

* Chapter 15.2.1-3 pages 491-5
* Multinomial and conditional logit models analysis.
* It provides ....
*   (0)  Data summary (Table 15.1)
*   (1A) Multinomial Logit estimates (Table 15.1)
*   (1B) Multinomial Logit marginal effects (text page 494)
*   (2A) Conditional Logit estimates (Table 15.2)
*   (2B) Conditional Logit marginal effects (Table 15.3)
*   (3)  Multinomial estimates obtained using Cinditional Logit
*   (4)  "Mixed Model" estimates (Table 15.1)

* Related programs are 
*    mma15p2gev.do   estimates a nested logit model using Stata
*    mma15p3mnl.lim  estimates multinomial models using Limdep
*    mma15p4gev.lim  estimates conditional and nested logit models using Limdep

* To run this program you need data file
*    Nldata.asc 

/* Program summary:

  (1) Multinomial logit of mode on alternative-invariant regressor (income)
        mlogit mode income

   (2) Conditional logit of mode on alternative-specific regressor (price, catch rate)
       First reshape data so 4 observations per individual - one for each mode.
       clogit mode p q

   (3) Conditional logit of mode on alternative-invariant regressor (income)
       First reshape data so 4 observations per individual - one for each mode.
       Then create dummy variables for each mode d2 d3 d4
       clogit mode d2 d3 d4 d2y d3y d4y
       This gives same results as (1)
       
   (4) Conditional logit of mode on alternative-invariant regressor (income)
       and on alternative-sepcific regressor (price, catch rate) 
       First reshape data so 4 observations per individual - one for each mode.
       Then create dummy variables for each mode d2 d3 d4
       clogit mode d2 d3 d4 d2y d3y d4y p q
*/  

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

********** READ IN DATA and SUMMARIZE (Table 15.1, p.492) **********

* Method to read in depends on model used

/* Data are on fishing mode: 1 beach, 2 pier, 3 private boat, 4 charter
   Data come as one observation having data for all 4 modes.
   Both alternative specific and alternative invariant regresssors.
*/

infile mode price crate dbeach dpier dprivate dcharter pbeach ppier /*
   */ pprivate pcharter qbeach qpier qprivate qcharter income /*
   */ using nldata.asc

gen ydiv1000 = income/1000

* Look at data by alternative 
label define modetype 1 "beach" 2 "pier" 3 "private" 4 "charter"
label values mode modetype

summarize
sort mode
by mode: summarize

* Following commands give Table 15.1, p.492
summarize ydiv100 pbeach ppier pprivate pcharter qbeach qpier /* 
    */ qprivate qcharter dbeach dpier dprivate dcharter
sort mode
by mode: summarize ydiv100 pbeach ppier pprivate pcharter qbeach qpier /* 
    */ qprivate qcharter dbeach dpier dprivate dcharter

********** (1) MULTINOMIAL LOGIT: ALTERNATIVE-INVARIANT REGRESSOR *********

*** (1A) Estimate the model

* Data are already in form for mlogit

* The following gives MNL column of Table 15.2, p.493
mlogit mode ydiv1000, basecategory(1)

*** (1B) Calculate the marginal effects

quietly mlogit mode ydiv1000, basecategory(1)
* Predict by default gives the probabilities
predict p1 p2 p3 p4

* As check compare predicted to actual probabilities
summarize dbeach p1 dpier p2 dprivate p3 dcharter p4

* Quick way to compute marginal effects (or semi-elasticities dp/dlnx or elasticities) 
* is to use built-in Stata function whcih evaluates at sample mean
* dydx, eyex, dwex or eydx
mfx compute, dydx predict(outcome(1))  
mfx compute, dydx predict(outcome(2))
mfx compute, dydx predict(outcome(3))
mfx compute, dydx predict(outcome(4))

* Better is to evaluate marginal effect for each observation and average
* The following calculates marginal effects using noncalculus methods 
* by comparing the predicted probability before and after change in x
* Here consider small change of 0.0001 - then multiply by 1000
* So should be similar to using calculus methods.
replace ydiv1000 = ydiv1000 + 0.0001
predict p1new p2new p3new p4new
gen dp1dy = 10000*(p1new - p1)
gen dp2dy = 10000*(p2new - p2)
gen dp3dy = 10000*(p3new - p3)
gen dp4dy = 10000*(p4new - p4)

* The computed marginal effects follow. 
* These are close to those given in text page 494 (which were calculated using Limdep)
sum dp1dy dp2dy dp3dy dp4dy 

* Note that here these are similar to the earlier values at means
* This is because little variation in predicted probability across individuals here

* ASIDE: Binary logit will differ a little from MNL
keep if mode == 1 | mode == 2
mlogit mode ydiv1000

******* (2) CONDITIONAL LOGIT: ALTERNATIVE-SPECIFIC REGRESSOR *********

*** (2A) Estimate the model

* This requires reshaping the data
clear
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
describe
summarize

reshape long d p q, i(id) j(alterntv)
* This automatically creates alterntv = 1 (beach), ... 4 (charter)
describe
summarize

clogit d q, group(id)
clogit d p, group(id)

* The following gives CL column of Table 15.2
clogit d p q, group(id)

*** (2B) Calculate the marginal effects

quietly clogit d p q, group(id)
predict pinitial

* Now compute marginal effects
* Consider in turn a change in each price and catch rate 
* Change price by 1 unit and then multiply by 100 as in Table 15.2
* Change catch rate by 0.001 and then multiply by 1000

* Change p1: price beach
replace p = p + 1 if alterntv==1
predict pnewp1 
gen mep1 = 100*(pnewp1 - pinitial)
replace p = p - 1 if alterntv==1

* Change p2: price pier
replace p = p + 1 if alterntv==2
predict pnewp2 
gen mep2 = 100*(pnewp2 - pinitial)
replace p = p - 1 if alterntv==2

* Change p3: price private boat
replace p = p + 1 if alterntv==3
predict pnewp3 
gen mep3 = 100*(pnewp3 - pinitial)
replace p = p - 1 if alterntv==3

* Change p4: price charter boat
replace p = p + 1 if alterntv==4
predict pnewp4 
gen mep4 = 100*(pnewp4 - pinitial)
replace p = p - 1 if alterntv==4

* Change q1: catch rate beach
replace q = q + 0.001 if alterntv==1
predict pnewq1 
gen meq1 = 1000*(pnewq1 - pinitial)
replace q = q - 0.001 if alterntv==1

* Change q2: catch rate pier
replace q = q + 0.001 if alterntv==2
predict pnewq2 
gen meq2 = 1000*(pnewq2 - pinitial)
replace q = q - 0.001 if alterntv==2

* Change q1: catch rate private boat
replace q = q + 0.001 if alterntv==3
predict pnewq3 
gen meq3 = 1000*(pnewq3 - pinitial)
replace q = q - 0.001 if alterntv==3

* Change q1: catch rate charter boat
replace q = q + 0.001 if alterntv==4
predict pnewq4 
gen meq4 = 1000*(pnewq4 - pinitial)
replace q = q + 0.001 if alterntv==4

* Following gives Table 15.3 on page 493
sort alterntv
by alterntv: sum pinitial mep1 mep2 mep3 mep4 meq1 meq2 meq3 meq4 

******* (3) CONDITIONAL LOGIT: ALTERNATIVE-INVARIANT REGRESSOR *********

* Here we get clogit to do something that is easier done by mlogit

clear
infile mode price crate dbeach dpier dprivate dcharter pbeach ppier /*
   */ pprivate pcharter qbeach qpier qprivate qcharter income /*
   */ using nldata.asc

gen ydiv1000 = income/1000

* Data are one entry per individual
* Need to reshape to 4 observations per individual - one for each alternative
* Use reshape to do this but first create variable
* Alternative = 1 if beach, = 2 if pier; = 3 if private boat; = 4 if charter
gen id = _n
gen d1 = dbeach
gen d2 = dpier
gen d3 = dprivate
gen d4 = dcharter
describe
summarize

reshape long d, i(id) j(alterntv)
describe
summarize

gen obsnum=_n
gen d2 = 0
replace d2 = 1 if mod(obsnum,4)==2
gen d3 = 0
replace d3 = 1 if mod(obsnum,4)==3
gen d4 = 0
replace d4 = 1 if mod(obsnum,4)==0
gen d2y = 0
replace d2y = d2*ydiv1000
gen d3y = 0
replace d3y = d3*ydiv1000
gen d4y = 0
replace d4y = d4*ydiv1000
summarize

* The following gives MNL column of Table 15.2, p.493, 
* which was more easily obtained using mlogit earlier
clogit d d2 d3 d4 d2y d3y d4y, group(id)

******* (4) "MIXED LOGIT" = CONDITIONAL LOGIT WITH BOTH 
*                           ALTERNATIVE-SPECIFIC REGRESSOR 
*                           AND ALTERNATIVE INVARIANT REGRESSOR *********

clear
infile mode price crate dbeach dpier dprivate dcharter pbeach ppier /*
   */ pprivate pcharter qbeach qpier qprivate qcharter income /*
   */ using nldata.asc

gen ydiv1000 = income/1000

* Data are one entry per individual
* Need to reshape to 4 observations per individual - one for each alternative
* Use reshape to do this but first create variable
* Alternative = 1 if beach, = 2 if pier; = 3 if private boat; = 4 if charter
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

reshape long d p q, i(id) j(alterntv)
summarize

* Bring in alternative specific dummies
* Since d2-d4 already used instead call them dummy2 - dummy4
gen obsnum=_n
gen dummy1 = 0
replace dummy1 = 1 if mod(obsnum,4)==1
gen dummy2 = 0
replace dummy2 = 1 if mod(obsnum,4)==2
gen dummy3 = 0
replace dummy3 = 1 if mod(obsnum,4)==3
gen dummy4 = 0
replace dummy4 = 1 if mod(obsnum,4)==0
* And interact with income
gen d1y = 0
replace d1y = dummy1*ydiv1000
gen d2y = 0
replace d2y = dummy2*ydiv1000
gen d3y = 0
replace d3y = dummy3*ydiv1000
gen d4y = 0
replace d4y = dummy4*ydiv1000

summarize

clogit d dummy2 dummy3 dummy4 p q, group(id)

* The following gives Mixed column of Table 15.2, p.493
clogit d p q dummy2 dummy3 dummy4 d2y d3y d4y, group(id)

* Output data file for Read into Limdep program mma15p4gev.lim
outfile id d p q ydiv1000 dummy2 dummy3 dummy4 d2y d3y d4y using mma15p4gev.asc, replace

********** CLOSE OUTPUT **********
log close
clear
exit
