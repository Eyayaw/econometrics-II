* MMA16P3SELECTION.DO  March 2005 for Stata version 8.0

log using mma16p3selection.txt, text replace

********** OVERVIEW OF MMA16P3SELECTION.DO **********

* STATA Program 
* copyright C 2005 by A. Colin Cameron and Pravin K. Trivedi 
* used for "Microeconometrics: Methods and Applications" 
* by A. Colin Cameron and Pravin K. Trivedi (2005)
* Cambridge University Press 

* Chapter 16.6 pages 553-5
* Selection models example
* It provides
*   (1) Two-part model estimation (Table 16.1)
*   (2) Selection model estimation
*     (2A) ML estimates  (Table 16.1)
*     (2B) Heckman 2-step estimates  (Table 16.1)
*     (2C) Check for possible collinearity problems in Heckman 2-Step 

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

********** READ DATA **********

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

********** DATA SELECTION AND TRANSFORMATIONS **********

* Use only Year 2
keep if year==2

* educdec is missing for one observation
drop if educdec==.

* rename variables
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

* Summarize the dependents and regressors
sum MED DMED LNMED $XLIST

* Detailed summary shows that MED>0 very skewed whereas LNMED is not
sum MED LNMED if MED>0, detail

* Write final data to a text (ascii) file so can use with programs other than Stata
outfile DMED MED LNMED LC IDP LPI FMDE PHYSLIM NDISEASE HLTHG HLTHF HLTHP /* 
     */ LINC LFAM EDUCDEC AGE FEMALE CHILD FEMCHILD BLACK /*
     */ using mma16p3selection.asc, replace

****************** CHAPTER 16.6 REGRESSION ANALYSIS  **************

* The analysis below models log expenditure (lny), not expenditure (y)
* where here y = MED and lny = LNMED.

* This makes regular tobit difficult as it is not clear 
* what the censoring/truncation point is since ln(0) = -infinity
* Also note that some LNMED<0 as 0<MED<1 is possible.
* So just do two-part model and sample selection model.

* Interested in comparing MED not LNMED at end of day.
* So use 
*   If   lny = xb + u,  u ~ N[0, s^2]     for y > 0  
*   Then E[y] = exp(xb + (s^2)/2)         for y > 0
*   and  E[y] = Pr[y>0]*exp(xb + (s^2)/2) for all y

* The models estimated are 
* (1) Two-part model using 
*     (a) probit for whether positive y 
*     (b) regress with lny as dependent variable
* (2) Sample selection model similar to (3) 
*     except that inverse Mills ratio appears in (b), estimated by
*     (a) MLE
*     (b) Heckman 2-step

* Additionally censored tobit and truncated tobit commands in levels 
* are given below for completeness. 

************ (1) TWO-PART MODEL ************

* Two-part model: binary probit and then lognormal for expenditures

* First part: probit for MED > 0
probit DMED $XLIST          /* global XLIST defined earlier */
estimates store twoparta    /* version 8 command for later table */ 
scalar llprobit = e(ll)     /* Log-likelihood */
predict probsel2part, p     /* Pr[y>0] = PHI(x'b) */ 
predict xbprobit, xb        /* x'b */

* Second part: OLS for log of positive values 
*  Here LNMED where LNMED missing if MED < 0
regress LNMED $XLIST 
estimates store twopartb   
scalar lllognormal = e(ll)  /* Log-likelihood */
scalar sols = e(rmse)       /* Standard error of the regression */
predict pLNMED, xb          /* Predicted mean from OLS */
predict rLNMED, residuals

* Check for normal errors
hettest
* imtest
sktest LNMED rLNMED

* Create two-part model log-likelihood
scalar lltwopart = llprobit + lllognormal
di "lltwopart = " lltwopart

* Create predictions of level of expenditures not logs
* E[y] = exp(pLNMED + (s^2)/2)  for y > 0
* and E[y] = Pr[y>0]*exp(xb + (s^2)/2) for all y
gen pMEDpos2part = exp(pLNMED + (sols^2)/2) 
gen pMEDall2part = probsel2part*pMEDpos2part

* Compare predictions to actual for MED > 0
sum LNMED pLNMED MED pMEDpos2part if MED > 0 
corr LNMED pLNMED MED pMEDpos2part if MED > 0

* Compare predictions to actual including zeroes
sum MED pMEDall2part DMED probsel2part
corr MED pMEDall2part DMED probsel2part

************ (2) SELECTION MODEL ************

* Sample selection model for log expenditures
* Selection equation:  
*      Observe y = y* if I = z'a + u > 0   u ~ N[0,1]
* Regression equation: 
*            y* = x'b + v   v ~ N[0,s^2] and Corr[u,v]=rho

* (2A) MLE for sample selection model
heckman LNMED $XLIST, select (DMED = $XLIST)
estimates store heckmle
scalar llhecklogs = e(ll)      /* Log-likelihood */
scalar shml = e(sigma)         /* s where Var[v]=s^2 */

* Save the Stata predictions: 
* Distinguish between ystar=E[y*], ypos=E[y|I>0] and yall=E[y] 
predict ystarhml, xb           /* E[y*] = x'b */
predict yposhml, ycond         /* E[y|I>0] = E[y*|I>0] = x'b+c*lamda(z'a) */
predict invmillhml, mills      /* lamda(z'a) = phi(z'a)/PHI(z'a) */
predict probselhml, psel      /* PHI(z'a) */
* The following not appropriate here as it sets y=0 if I<0
* whereas here data is in logs and y=ln(MED)=-infinity if I<0  
predict yallhml, yexpected     /* E[y] = PHI(z'a)*E[y|I>0] */
sum ystarhml yposhml invmillhml probselhml yallhml

* Create predictions of level of expenditures not logs
* E[y] = exp(ypos + (s^2)/2)  for y > 0    Var[v]=s^2
* and E[y] = Pr[y>0]*exp(ypos + (s^2)/2) for all y
gen pMEDposhml = exp(yposhml + (shml^2)/2) 
gen pMEDallhml = probselhml*pMEDposhml

* Compare predictions to actual for MED > 0
sum LNMED yposhml MED pMEDposhml if MED > 0 
corr LNMED yposhml MED pMEDpos2part if MED > 0

* Compare predictions to actual including zeroes
sum MED pMEDallhml DMED probselhml
corr MED pMEDallhml DMED probselhml

* (2B) Heckman 2 step for sample selection model
*     Same as MLE execpt add option twostep in heckman command
heckman LNMED $XLIST, select (DMED = $XLIST) twostep
estimates store heck2step
scalar sh2s = e(sigma)         /* s where Var[v]=s^2 */

* Save the Stata predictions: 
* Distinguish between ystar=E[y*], ypos=E[y|I>0] and yall=E[y] 
predict ystarh2s, xb           /* E[y*] = x'b */
predict yposh2s, ycond         /* E[y|I>0] = E[y*|I>0] = x'b+c*lamda(z'a) */
predict invmillh2s, mills      /* lamda(z'a) = phi(z'a)/PHI(z'a) */
predict probselh2s, psel      /* PHI(z'a) */
* The following not appropriate here as it sets y=0 if I<0
* whereas here data is in logs and y=ln(MED)=-infinity if I<0  
predict yallh2s, yexpected     /* E[y] = PHI(z'a)*E[y|I>0] */
sum ystarh2s yposh2s invmillh2s probselh2s yallh2s

* Create predictions of level of expenditures not logs
* E[y] = exp(ypos + (s^2)/2)  for y > 0    Var[v]=s^2
* and E[y] = Pr[y>0]*exp(ypos + (s^2)/2) for all y
gen pMEDposh2s = exp(yposh2s + (sh2s^2)/2) 
gen pMEDallh2s = probselh2s*pMEDposh2s

* Compare predictions to actual for MED > 0
sum LNMED yposh2s MED pMEDposh2s if MED > 0 
corr LNMED yposh2s MED pMEDpos2part if MED > 0

* Compare predictions to actual including zeroes
sum MED pMEDallh2s DMED probselh2s
corr MED pMEDallh2s DMED probselh2s

* (2C) Check for possible collinearity problems in Heckman 2-Step 

* Check variation in inverse mills ratio and related measures
gen zprimea = invnorm(probselh2s)
gen zprimeasq = zprimea*zprimea
sum invmillh2s probselh2s zprimea ystarh2s
sum invmillh2s probselh2s zprimea ystarh2s, detail

* Check for Mills ratio linear in zprimea
regress invmillh2s zprimea
regress invmillh2s zprimea zprimeasq
* twoway scatter yinvmill probitxb

* Check R-squared from regress yinvmill on other regressors
regress invmillh2s $XLIST

* Find the condition number with inverse mills ratio included
matrix accum XX = invmillh2s $XLIST
matrix XXScaled = corr(XX)
matrix symeigen XXSeigvec XXSeigval = XXScaled 
scalar rowsXX = rowsof(XX) 
scalar condnum1 = sqrt(XXSeigval[1,1]/XXSeigval[1,rowsXX])
scalar condnum2 = sqrt(XXSeigval[1,1]/XXSeigval[1,(rowsXX-1)])

* Find the condition number without inverse mills ratio
matrix accum ZZ = $XLIST  
matrix ZZScaled = corr(ZZ)
matrix symeigen ZZSeigvec ZZSeigval = ZZScaled 
scalar rowsZZ = rowsof(ZZ) 
scalar condnumnoinvmills1 = sqrt(ZZSeigval[1,1]/ZZSeigval[1,rowsZZ])
scalar condnumnoinvmills2 = sqrt(ZZSeigval[1,1]/ZZSeigval[1,(rowsZZ-1)])

* Condition numbers between 30 and 100 indicate a strong near dependency
scalar list condnum1 condnum2
scalar list condnumnoinvmills1 condnumnoinvmills2

* (2D) Do Heckman 2 step manually (this is unnecessary)
quietly probit DMED $XLIST          /* global XLIST defined earlier */
predict pselmanual, p       /* Pr[y>0] = PHI(x'b) */ 
predict xbmanual, xb        /* x'b */
gen invmillsmanual = normden(xbmanual)/pselmanual
regress LNMED $XLIST invmillsmanual if MED > 0
predict yposmanual, xb 
* Predictions here should equal those from heckman two-step earlier
sum yposh2s yposmanual invmillh2s invmillsmanual probselh2s pselmanual
* And put in squared invmills ratio 
gen invmillssq = invmillsmanual*invmillsmanual
regress LNMED $XLIST invmillsmanual invmillssq if MED > 0

************ (3) DISPLAY RESULTS FOR TABLE 16.1 (page 554) ************

* Note for brevity the coefficients for only some of the regressors are reported

* First two columns of Table 16.1 (page 554) 
* Two part estimates: probit for first part and lognormal for second
estimates table twoparta twopartb, t stats(N ll rank aic bic) b(%10.3f)
di "lltwopart = " lltwopart

* Last four columns of Table 16.1 (page 554) 
* Sample selection estimates: 2step and MLE estimates
set matsize 60
estimates table heck2step heckmle, t stats(N ll rank aic bic) b(%10.3f)

************ (4) A LITTLE FURTHER ANALYSIS **********

* Predictions
* Compare predictions to actual for MED > 0
sum MED pMEDpos2part pMEDposhml pMEDposh2s if MED > 0
corr MED pMEDpos2part pMEDposhml pMEDposh2s if MED > 0  

* Compare predictions to actual including zeroes
sum MED pMEDall2part pMEDallhml pMEDallh2s DMED probsel2part probselhml probselh2s
corr MED pMEDall2part pMEDallhml pMEDallh2s DMED probsel2part probselhml probselh2s

********** CLOSE OUTPUT
log close
clear
exit


