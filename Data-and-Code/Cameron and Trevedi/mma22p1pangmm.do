* MMA22P1PANGMM.DO  March 2005 for Stata version 8.0

log using mma22p1pangmm.txt, text replace

********** OVERVIEW OF MMA22P1PANGMM.DO **********

* STATA Program 
* copyright C 2005 by A. Colin Cameron and Pravin K. Trivedi 
* used for "Microeconometrics: Methods and Applications" 
* by A. Colin Cameron and Pravin K. Trivedi (2005)
* Cambridge University Press

* Chapter 22.3 pages 754-6
* Panel 2SLS and GMM for a linear model with endogenous regressors
* Fixed effects are first differenced. 
* Then 2SLS and GMM applied to first differenced model.

* Program derives Table 22.2 and does other analysis in section
*  (1) pooled OLS
*  (2) 2SLS in base instruments case
*  (3) 2SLS in stacked instruments case
*  (4) 2SGMM in base instruments case
*  (5) 2SGMM in stacked instruments case
*  (6) F-statistics for weak instruments
*  (7) Partial R-squared for weak instruments

* The pooled OLS and 2SLS replicate Ziliak (1997) Table 1 Top left-hand corner
*   for Base Case (9 instruments) and first Stacked Case (72 instruments) 
* 2SLS in first differences where both 1979 and 1980 are dropped

* To run you need file
*   MOMprecise.dat    
* in your directory

* NOTE: This data set is different from MOM.dat used in chapter 21.
*       The data here has more significant digits.
*       leading to some difference in resulting coefficient estiamtes.

********** SETUP **********

set more off
version 8.0
set scheme s1mono  /* Graphics scheme */

********** DATA DESCRIPTION **********

*  The original data is from 
*  Jim Ziliak (1997)
*  "Efficient Estimation With Panel Data when Instruments are Predetermined: 
* An Empirical Comparison of Moment-Condition Estimators" 
* Journal of Business and Economic Statistics, 15, 419-431
* NOTE: Data originally posted on JBES website was to only 2 dec places
* Here more accurate data is used (the same as the data used by Ziliak)
* Ziliak used Gauss. Here Stata is used. 

* File MOM.dat has data on 532 men over 10 years (1979-1988) 
* Data are space-delimited ordered by person with separate line for each year
* So id 1 1979, id 1 1980, ..., id 1 1988, id 2 1979, 1d 2 1980, ...
* 8 variables: 
* lnhr lnwg kids ageh agesq disab id year

* File MOMprecise.dat has more significant digits than file MOM.dat 
* (the version of the data posted at the JBES website (used in chapter 21)

********** READ DATA **********

* The data are in ascii file MOM.dat
* There are 532 individuals with 10 lines (years) per individual
* Read in using Infile: FREE FORMAT WITHOUT DICTIONARY
infile lnhr lnwg kids ageh agesq disab id year using MOMprecise.dat
describe
summarize

********** FIRST DIFFERENCES REGRESSION **********

* Stata has no command for first differences regression
* Though may be possible with xtivreg

* The following only works if each observation is (i,t) 
* and within i the data are ordered by t
gen dlnhr = lnhr - lnhr[_n-1]
gen dlnwg = lnwg - lnwg[_n-1]
gen dkids = kids - kids[_n-1]
gen dageh = ageh - ageh[_n-1]
gen dagesq = agesq - agesq[_n-1]
gen ddisab = disab - disab[_n-1]

* The regression is of 
* dlnhr on constant dlnwg dkids dageh dagesq ddisab 

********** GENERATE THE INSTRUMENTS **********

* The endogenous variable is dlnwg. The others are exogenous.
* It is not clear whether current values of the exogenous variables are used as instruments.
* I would think so but there is no mention in the paper of this.
* In addition Table 1 considers various instrument sets
* We consider the first (first rows) and second (second rows)

* (1) Use the levels of the exogenous regressors lagged one and two periods 
*     and the level of the endogenous regressor lagged two periods
* This gives nine instruments   
gen kidsl1 = kids[_n-1] 
gen kidsl2 = kids[_n-2] 
gen agehl1 = ageh[_n-1] 
gen agehl2 = ageh[_n-2]
gen agesql1 = agesq[_n-1] 
gen agesql2 = agesq[_n-2] 
gen disabl1 = disab[_n-1] 
gen disabl2 = disab[_n-2]
gen lnwgl2 = lnwg[_n-2] 

* (2) Use the same instruments as in (1) except now stacked so that 
*     now the instrument matrix is block-diagonal.
* This gives nine instruments times number of time periods.
* The original data are 1979 to 1988.
* We will eventually drop the first two years as lose 2 years due to lags. 
* For short hand call the instruments z1 to z9 and the years 1981 to 1988 y1 to y8.    
* Pad out to 8 x 9 = 72 instruments for 8 years

program define makeZ
forvalues i=1(1)8 {
  gen z1y`i'=0
  replace z1y`i' = ageh[_n-1] if year==1980+`i'
  gen z2y`i'=0 
  replace z2y`i' = agesq[_n-1] if year==1980+`i'
  gen z3y`i'=0
  replace z3y`i' = kids[_n-1] if year==1980+`i'
  gen z4y`i'=0 
  replace z4y`i' = disab[_n-1] if year==1980+`i'
  gen z5y`i'=0
  replace z5y`i' = ageh[_n-2] if year==1980+`i'
  gen z6y`i'=0 
  replace z6y`i' = agesq[_n-2] if year==1980+`i'
  gen z7y`i'=0
  replace z7y`i' = kids[_n-2] if year==1980+`i'
  gen z8y`i'=0 
  replace z8y`i' = disab[_n-2] if year==1980+`i'
  gen z9y`i'=0 
  replace z9y`i' = lnwg[_n-2] if year==1980+`i'
}
end
quietly makeZ
sum

* Define variable lists for regressors X and instruments Z

global XREG dlnwg dkids dageh dagesq ddisab

global ZBASECASE kidsl1 agehl1 agesql1 disabl1 agehl2 kidsl2 agesql2 disabl2 lnwgl2

global ZSTACKED z1y1 z2y1 z3y1 z4y1 z5y1 z6y1 z7y1 z8y1 z9y1 /*
  */          z1y2 z2y2 z3y2 z4y2 z5y2 z6y2 z7y2 z8y2 z9y2 /*
  */          z1y3 z2y3 z3y3 z4y3 z5y3 z6y3 z7y3 z8y3 z9y3 /*
  */          z1y4 z2y4 z3y4 z4y4 z5y4 z6y4 z7y4 z8y4 z9y4 /*
  */          z1y5 z2y5 z3y5 z4y5 z5y5 z6y5 z7y5 z8y5 z9y5 /*
  */          z1y6 z2y6 z3y6 z4y6 z5y6 z6y6 z7y6 z8y6 z9y6 /*
  */          z1y7 z2y7 z3y7 z4y7 z5y7 z6y7 z7y7 z8y7 z9y7 /*
  */          z1y8 z2y8 z3y8 z4y8 z5y8 z6y8 z7y8 z8y8 z9y8

* Define variable lists for weak instruments test which drops 

save momfdiffgmm, replace
sum

********** (1)-(3) 2SLS USING IVREG IS STRAIGHTFORWARD (Table 22.2, p.755) **********

* Note that this will automatically includes the exogenous variables as instrumetns
* It is not clear that Ziliak does this 

* The following drops the first two years which here are 1979 and 1980
drop if year == 1979 | year == 1980

* (1) OLS results at bottom Ziliak table 1
* Table 22.2 (page 755) OLS column with various standard errors estimates 
regress dlnhr $XREG, noconstant
estimates store olsiid
regress dlnhr $XREG, noconstant robust
estimates store olshet
regress dlnhr $XREG, noconstant cluster(id)
estimates store olspanel

* (2) 2SLS using the base case instrument set 
* Table 22.2 (page 755) 2SLS column base case with various se estimates 
ivreg dlnhr ($XREG = $ZBASECASE), noconstant
estimates store baseiid
ivreg dlnhr ($XREG = $ZBASECASE), noconstant robust
estimates store basehet
ivreg dlnhr ($XREG = $ZBASECASE), noconstant cluster(id)
estimates store basepanel

* (3) 2SLS using the stacked instrument set 
* Table 22.2 (page 755) 2SLS column stacked case with various se estimates 
set matsize 100
ivreg dlnhr ($XREG = $ZSTACKED), noconstant
estimates store stackiid
ivreg dlnhr ($XREG = $ZSTACKED), noconstant robust
estimates store stackhet
ivreg dlnhr ($XREG = $ZSTACKED), noconstant cluster(id)
estimates store stackpanel
ivreg dlnhr ($XREG = $ZSTACKED), noconstant robust cluster(id)

* DISPLAY THE OLS AND 2SLS RESULTS

* The following are used in Table 22.2 (page 755)

* OLS column with various standard errors estimates 
estimates table olspanel olshet olsiid, /*
   */ se stats(N ll r2 tss rss mss rmse df_r) b(%10.3f)

* 2SLS column base case with various standard errors estimates 
estimates table basepanel basehet baseiid, /*
   */ se stats(N ll r2 tss rss mss rmse df_r) b(%10.3f)

* 2SLS column stacked case with various standard errors estimates 
estimates table stackpanel stackhet stackiid, /*
   */ se stats(N ll r2 tss rss mss rmse df_r) b(%10.3f)

********** (4)-(5) 2SGMM REQUIRES SPECIAL MARTRIX CODING **********

*** PROGRAM PANELGMM DOES 2SLS (as check) and 2SGMM USING MATRIX COMMANDS

* This program:
*  - requires as inputs the global macros 
*      y   gives the dependent variable name
*      X   gives the list of regressor names
*      Z   gives the list of instrument names
*  - assumes the appropriate data is in memory
*  - assumes the cluster identifier is called id

* If the regressors and instruments include an intercept include 
* this as a separate regressor, say called ONE, in X and Z. 
* Then continue to use the following code with the noconstant option for accum and optaccum.
* (accum and optaccum automatically include a constant AT THE END, 
*  which is not where we want the constant.)

* This program computes the 2SLS and two-step GMM estimators 
*       [(X'Z)(Z'Z)_inv Z'X]_inv (X'Z)(Z'Z)_inv Z'y
* and   [(X'Z)S_inv Z'X]_inv (X'Z)S_inv Z'y
* and appropriate panel robust standard errors
* assuming a short panel with errors correlated over t for given i and heteroskedastic.

program define panelgmm

* (1) Create Z'Z and check that full rank
matrix accum ZZ = $Z, noconstant
scalar dimz = rowsof(ZZ)
scalar detzz = det(ZZ)
di "Redundant instruments if det(Z'Z) zero. Here det(Z'Z) = " detzz

* (2) Create Z'X which is trickier
* Create ZX'ZX = [Z X]' [Z X] using accum which automatically adds a constant
matrix accum ZXZX = $Z $X, noconstant
* Then Z'X is the (1,2) submatrix: rows 1 to dimz and columns dimz+1 to dimzx
scalar dimzx = rowsof(ZXZX)
* Also need dimension of X
matrix accum XX = $X, noconstant
scalar dimx = rowsof(XX)
matrix ZX = ZXZX[1..dimz,dimz+1...]

* (3) Create Z'y 
* Create Zy'Zy = [Z y]' [Z y] using accum which automatically adds a constant
matrix accum ZyZy = $Z $y, noconstant
* Then Z'y is the (1,2) submatrix: rows 1 to dimz and the last column
matrix Zy = ZyZy[1..dimz,dimz+1]

* (4) Compute 2SLS Estimator
di " "
di "2SLS results: "
matrix b2SLS = syminv(ZX'*syminv(ZZ)*ZX)*ZX'*syminv(ZZ)*Zy
matrix list b2SLS

* (5) Compute S = Sum_i Zi'u_i*u_i'Z_i using opaccum
* Key is use of opaccum.
* Need to compute the residuals. 
gen yhat = 0
foreach var of varlist $X {
matrix a`var' = b2SLS["`var'",1]
scalar b`var' = trace(a`var')  /* converts matrix to scalar */
quietly replace yhat = yhat + (b`var')*(`var')
}
gen uhat = $y - yhat
gen uhatsq = uhat*uhat
quietly sum(uhatsq)
scalar rmse = sqrt(r(sum)/(_N-dimx))
di "rmse = " rmse
* Alternative and check uses ivreg. 
quietly ivreg $y ($X = $Z), noconstant cluster(id)
predict uhat2, residuals
quietly sum uhat uhat2
* Sort data for opaccum to work
preserve
sort id
matrix opaccum S = $Z, group(id) opvar(uhat) noconstant
/*
* Ziliak uses heteroskedastic errors but not correlated. 
* Then instead use the following which assumes time identifier is year.
* Make a unique identifier obsid so that group(obsid) does not group
gen obsid = 10000*id + year
sort obsid
matrix opaccum S = $Z, group(obsid) opvar(uhat) noconstant
*/
restore

* (6) Compute Variance of 2SLS. 
matrix v2SLS = syminv(ZX'*syminv(ZZ)*ZX)*ZX'*syminv(ZZ)*S*syminv(ZZ)*ZX*syminv(ZX'*syminv(ZZ)*ZX)
* matrix list v2SLS
* Now need to get standard errors
matrix se2SLS = J(dimx,1,0)   /* Initially column vector of zeroes */
scalar icol = 1
* Need loop here as Stata does not do square root on a vector
while icol <= dimx {
  matrix se2SLS[icol,1] = sqrt(v2SLS[icol,icol])
  scalar icol = icol+1
  }
matrix list se2SLS

* (7) Compute Two-step GMM 
di " "
di "2SGMM results: "
matrix b2SGMM = syminv(ZX'*syminv(S)*ZX)*ZX'*syminv(S)*Zy
matrix list b2SGMM

* (8) Compute Variance of Two-step GMM 
* Compute the residuals to recompute S at the new estimates. 
* Note that could just use the old S
drop yhat uhat uhatsq
gen yhat = 0
foreach var of varlist $X {
matrix a`var' = b2SGMM["`var'",1]
scalar b`var' = trace(a`var')  /* converts matrix to scalar */
quietly replace yhat = yhat + (b`var')*(`var')
}
gen uhat = $y - yhat
gen uhatsq = uhat*uhat
quietly sum(uhatsq)
scalar rmse = sqrt(r(sum)/(_N-dimx))
di "rmse = " rmse
* Sort data for opaccum to work
preserve
sort id
matrix opaccum S = $Z, group(id) opvar(uhat) noconstant
matrix v2SGMM = syminv(ZX'*syminv(S)*ZX)
* matrix list v2SGMM
matrix se2SGMM = J(dimx,1,0)   /* Initially column vector of zeroes */
scalar icol = 1
* Need loop here as Stata does not do square root on a vector
while icol <= dimx {
  matrix se2SGMM[icol,1] = sqrt(v2SGMM[icol,icol])
  scalar icol = icol+1
  }
matrix list se2SGMM

* (9) Compute the overidentifying restrictions test
* Create row vector u'Z using vecaccum which automatically adds a constant
matrix vecaccum uZ = uhat $Z, noconstant
matrix maxobjfunction = uZ*syminv(S)*uZ'
scalar ortest = maxobjfunction[1,1]
scalar dof = dimz - dimx
di " Over-identifying restrictions test " ortest " dof " dof " p-value " chi2tail(dof,ortest)

end

*** EXECUTE THE PROGRAM PANEL GMM FOR THESE DATA

* Note that Ziliak does not use an intercept. 
* If have an intercept then need to add in the constant explicitly
* generate ONE = 1
* and then add this to the X and Z

* Define the dependent variable
global y dlnhr

* Define the regressors. 
global X $XREG

* (4) 2SGMM (and 2SLS as check) using the base case instrument set
* Gives 2SGMM Base Case column of Table 22.2 (page 755)

global Z $ZBASECASE
panelgmm

* (5) 2SGMM (and 2SLS as check) using the stacked instrument set
* Gives 2SGMM Stacked Case column of Table 22.2 (page 755)

drop uhat yhat uhatsq uhat2 /* Obtained in panelgmm */
global Z $ZSTACKED
* dlnwg dkids dageh dagesq ddisab
panelgmm

********** (6) F-STATISTICS FOR WEAK INSTRUMENTS (page 756) **********

* (1) Weak Instruments using base case instrument set

* Test weak instruments for dlnwg using panel robust inference
quietly regress dlnwg $ZBASECASE, cluster(id)
quietly test $ZBASECASE
* This value should have been reported in the text on page 756
* [Instead by mistake the F assuning iid errors below was reported]
di "r2 = " e(r2) " F = " r(F) " p = " r(p) " dof = " r(df)

* Same except use wrong inference assuming iid errors
quietly regress dlnwg $ZBASECASE
quietly test $ZBASECASE
di "r2 = " e(r2) " F = " r(F) " p = " r(p) " dof = " r(df)

* (2) Weak Instruments using stacked instrument set

* Test weak instruments for dlnwg using panel robust inference
quietly regress dlnwg $ZSTACKED, cluster(id)
quietly test $ZSTACKED
* This value was reported in the text on page 756
di "r2 = " e(r2) " F = " r(F) " p = " r(p) " dof = " r(df)

* Same except use wrong inference assuming iid errors
quietly regress dlnwg $ZSTACKED
quietly test $ZSTACKED
di "r2 = " e(r2) " F = " r(F) " p = " r(p) " dof = " r(df)

* (3) Weak Instruments for other regressors
* Here all regressors are instrumented. So should test all as above.
* These find no problems.
* For example, for dkids and base case instrument set
quietly regress dkids $ZSTACKED, cluster(id)
quietly test $ZSTACKED
di "r2 = " e(r2) " F = " r(F) " p = " r(p) " dof = " r(df)
quietly regress dageh $ZSTACKED, cluster(id)
quietly test $ZSTACKED
di "r2 = " e(r2) " F = " r(F) " p = " r(p) " dof = " r(df)
quietly regress dagesq $ZSTACKED, cluster(id)
quietly test $ZSTACKED
di "r2 = " e(r2) " F = " r(F) " p = " r(p) " dof = " r(df)
quietly regress ddisab $ZSTACKED, cluster(id)
quietly test $ZSTACKED
di "r2 = " e(r2) " F = " r(F) " p = " r(p) " dof = " r(df)

********** PARTIAL R-SQUARED FOR WEAK INSTRUMENTS (page 756) **********

* (1) Weak Instruments using base case instrument set

* Test weak instruments for dlnwg using panel robust inference
quietly regress dlnwg $ZBASECASE, cluster(id)
quietly test $ZBASECASE
di "r2 = " e(r2) " F = " r(F) " p = " r(p) " dof = " r(df)

**** (D) Shea (1997) partial R-squared

* Here we have five endogenous regressors and no exogenous regressors.
* Need to change code below if there are exogenous regressors. See ch4ivkling.do
* Focus on the endogenous wage regressor. 
* For the other four just need to replace dlnwg in the first line of (1)
* and replace the first line of (2B)

* (1) Form x1 - x1tilda: residual from regress x1 on other regressors
quietly reg dlnwg dkids dageh dagesq ddisab
* quietly reg dkids dlnwg dageh dagesq ddisab
predict x1minusx1tilda, resid

* (2) Form x1hat - x1hattilda: residual from regress x1hat on fitted values of other regressors
* (2A) First get the fitted values from regress endogenous on instruments
quietly reg dlnwg $ZBASECASE
predict dlnwghat, xb
di e(r2) "  r2 from regress x1 on Z" 
quietly reg dkids $ZBASECASE
predict dkidshat, xb
di e(r2) "  r2 from regress second endog regressor on Z"
quietly reg dageh $ZBASECASE
predict dagehhat, xb
di e(r2) "  r2 from regress third endog regressor on Z"
quietly reg dagesq $ZBASECASE
predict dagesqhat, xb
di e(r2) "  r2 from regress fourth endog regressor on Z"
quietly reg ddisab $ZBASECASE
predict ddisabhat, xb
di e(r2) "  r2 from regress fifth endog regressor on Z"
* (2B) Run the regression of x1hat on fitted values of other regressors
quietly reg dlnwghat dkidshat dagehhat dagesqhat ddisabhat
* quietly reg dkidshat dlnwghat dagehhat dagesqhat ddisabhat
di e(r2) "  r2 from regress prediction of x1 on predictions of x2
predict x1hatminusx1hattilda, resid

* (3) Form the correlation between (1) and (2)  
* This value is reported in the text on page 756
corr x1minusx1tilda x1hatminusx1hattilda
di r(rho)^2 "  Shea's partial R-squared measure" 

********** CLOSE OUTPUT

log close
clear
exit
