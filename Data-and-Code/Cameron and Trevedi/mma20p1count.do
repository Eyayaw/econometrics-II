* MMA20P1COUNT.DO  March 2005 for Stata version 8.0

log using mma20p1count.txt, text replace

********* OVERVIEW OF MMA20P1COUNT.DO **********

* STATA Program 
* copyright C 2005 by A. Colin Cameron and Pravin K. Trivedi 
* used for "Microeconometrics: Methods and Applications" 
* by A. Colin Cameron and Pravin K. Trivedi (2005)
* Cambridge University Press

* Chapter 20.3 pages 671-4 and 20.7 page 690
* Count data regression example
* It provides
*   (1) Frequency distribution for count (Table 20.3)
*   (2) Data summary (Table 20.4)
*   (3) Poisson regression with various standard errors (Table 20.5)
*   (4) Negative binomial regression with various standard errors (Table 20.5)

* To use this program you need health expenditure data in Stata data set
*   randdata.dta    
 
********** SETUP **********

set more off
version 8.0
set scheme s1mono   /* Used for graphs */

********** DATA DESCRIPTION **********

* Essentially same data as in P. Deb and P.K. Trivedi (2002)
* "The Structure of Demand for Medical Care: Latent Class versus
*  Two-Part Models", Journal of Health Economics, 21, 601-625
* except that paper used different outcome (counts rather than $)

* Each observation is for an individual over a year.
* Individuals may appear in up to five years.
* All available sample is used except only fee for service plans included.
* In analysis here only year 2 is used so panel complications are avoided.
* Clustering of individuals within household is ignored here.

* Dependent variable is 
*      MED      med        Annual medical expenditures in constant dollars 
*                          excluding dental and outpatient mental 
*      LNMED    lnmeddol   Ln(Medical expenditures) given meddol > 0
*                          Missing otherwise
*      DMED     binexp     1 if medical expenditures > 0

* Regressors are 
*  - Health insurance measures
*       LC       logc      log(coinsrate+1)  where coinsurance rate is 0 to 100
*       IDP      idp       1 if individual deductible plan
*       LPI      lpi       1og(annual participation incentive payment) or 0 if no payment 
*       FMDE     fmde      log(max(medical deductible expenditure)) if IDP=1 and MDE>1 or 0 otherwise.
*  - Health status measures
*       NDISEASE disea     number of chronic diseases
*       PHYSLIM  physlm    1 if physical limitation
*       HLTHG    hlthg     1 if good health
*       HLTHF    hlthf     1 if good health
*       HLTHP    hlthp     1 if good health  (omitted is excellent)
*  - Socioeconomic characteristics
*       LINC     linc      log of annual family income (in $)
*       LFAM     lfam      log of family size
*       EDUCDEC  educdec   years of schooling of decision maker
*       AGE      xage      exact age
*       BLACK    black     1 if black
*       FEMALE   female    1 if female 
*       CHILD    child     1 if child
*       FEMCHILD fchild    1 if female child

* If panel data used then clustering is on
*       zper      person id

********** READ DATA, SELECT AND TRANSFORM **********

use randdata.dta, clear
sum

/* Describe and summarize the original data.
describe
summarize
* The orignal data are a panel. 
* The following summarizes panel features for completeness
iis zper
tis year
xtdes
xtsum meddol lnmeddol binexp
*/

* Note that unlike chapter 16 we use all years, not just year 2

* educdec is missing for some observations
drop if educdec==.

* rename variables
rename mdvis MDU
rename meddol MED
rename binexp DMED
rename lnmeddol LNMED
rename linc LINC
rename lfam LFAM
rename educdec EDUCDEC
rename xage AGE
rename female FEMALE
rename child CHILD 
rename fchild FEMCHILD
rename black BLACK
rename disea NDISEASE
rename physlm PHYSLIM
rename hlthg HLTHG
rename hlthf HLTHF
rename hlthp HLTHP
rename idp IDP
rename logc LC
rename lpi LPI
rename fmde FMDE

* Define the regressor list which in commands can refer to as $XLIST
global XLIST LC IDP LPI FMDE PHYSLIM NDISEASE HLTHG HLTHF HLTHP /* 
     */ LINC LFAM EDUCDEC AGE FEMALE CHILD FEMCHILD BLACK

sum MDU $XLIST 

* Write final data to a text (ascii) file so can use with programs other than Stata
outfile MDU LC IDP LPI FMDE PHYSLIM NDISEASE HLTHG HLTHF HLTHP /* 
     */ LINC LFAM EDUCDEC AGE FEMALE CHILD FEMCHILD BLACK /*
     */ using mma20p1count.asc, replace

********** (1) FREQUENCIES OF COUNT (Table 20.3, page 672) **********

* Following ggives Table 20.3 (page 672) frequencies
tabulate MDU

* Histogram with kernel density estimate
hist MDU, discrete kdensity

********** (2) DATA SUMMARY (Table 20.4, page 672) **********

* Following gives variables in same order as Table 20.4 (page 672)
sum MDU LC IDP LPI FMDE LINC LFAM AGE FEMALE CHILD FEMCHILD BLACK /* 
     */ EDUCDEC PHYSLIM NDISEASE HLTHG HLTHF HLTHP


*********** (3, 4) REGRESSION ANALYSIS  **************

* Here just two estimators - Poisson and negative binomial
* but three ways to calculate standard errors
* (A) default ML
* (B) robust (to misspecification of heteroskedasticity)
* (C) cluster-robust needed here as data are actually panel (see chapter 21, 24) 

*** Table 20.5  Poisson regression estimates

* Default standard errors assume variance = mean (ignoring overdispersion)
* This is first t-ratio in Table 20.5
poisson MDU $XLIST
estimates store poisml

* Should always control for possible overdispersion 
* This is second t-ratio in Table 20.5
poisson MDU $XLIST, robust
estimates store poisrobust

* Should also control here for clustering (see chapter 24) 
* as up to four years of data for each person. 
* Table 20.5 did not report these results
poisson MDU $XLIST, cluster(zper)
estimates store poiscluster

*** Table 20.5  Negative binomial regression estimates

* Default standard errors assume variance = mean (ignoring overdispersion)
* This is first t-ratio in Table 20.5
nbreg MDU $XLIST
estimates store nbml

* Should always control for possible overdispersion 
* This is second t-ratio in Table 20.5
nbreg MDU $XLIST, robust
estimates store nbrobust

* Should also control here for clustering (see chapter 24) 
* as up to four years of data for each person. 
* Table 20.5 did not report these results
nbreg MDU $XLIST, cluster(zper)
estimates store nbcluster

************ DISPLAY RESULTS FOR TABLE 20.5 (page 673) ************

* Note for brevity the coefficients for only some of the regressors 
* are given in Table 20.5

* First columns of Table 20.5 (page 673) plus cluster-robust 
estimates table poisml poisrobust poiscluster, t stats(N ll rank aic bic) b(%10.4f) t(%10.3f)

* Last columns of Table 20.5 (page 673) give bnbml. Also give others.
estimates table nbml nbrobust nbcluster, t stats(N ll rank aic bic) b(%10.4f) t(%10.3f)

* For Poisson correcting for overdispersion is most important.
* For negative binomial overdispersion is already incorporated.
* For both contreolling for clustering (in this example with panel data)
* is also needed.

********** CLOSE OUTPUT
log close
clear
exit


