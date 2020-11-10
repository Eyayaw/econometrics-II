* MMA04P4IVWEAK.DO for Stata version 8.0

log using mma04p4ivweak.txt, text replace

********** OVERVIEW OF MMA04P4IVWEAK.DO **********

* STATA Program 
* copyright C 2005 by A. Colin Cameron and Pravin K. Trivedi 
* used for "Microeconometrics: Methods and Applications" 
* by A. Colin Cameron and Pravin K. Trivedi (2005)
* Cambridge University Press 

* Chapter 4.9.5 pages 110-2
* IV regression with potentially weak instruments
* (1) Compares OLS and IV estimation of log-wages on schooling regression
*     where schooling, experience and experience-squared are endogenous
*     and proximity to 4-year college, age and age-squared are instruments
*     so model is just-identified.
* (2) Verifies that here can treat errors as homoskedastic
* (3) Looks at weak instruments 
*     (A) instrument relevance: Whether Shea's partial R-squared is low
*     (B) finite sample bias: whether first-stage partial F is low
* (4) Provides Table 4.5
* (5) Does more analysis than reported in the book

* To run this program you need data and dictionary files
*    DATA66.dat    ASCII data set
*    DATA66.dct    Stata dictionary that labels variables

********** SETUP **********

set more off
version 8.0
set memory 20m
set linesize 150   /* Permits long inputline commands with delimit */

********** ORIGINAL DATA SOURCE **********

* Program mma4p4ivweak.do based on Kling Analys66.d0  September 2003
* written for Jeffrey R. Kling (2001) "Interpreting Instrumental Variables Estimates
* of the Return to Schooling", Journal of Business and Economic Statistics,
* July 2001, 19 (3), pp.358-364.
* This program focuses on Columns (1) and (2) of Kling's Table 1 on p.359
* in turn based on 
* David Card (1995), "Using Geographic Variation in College Proximity to
*   Estimate the Returns to Schooling", in 
*   Aspects of Labor Market Behavior: Essays in Honor of John Vanderkamp, 
*   eds. L.N. Christofides et al., Toronto: University of Toronto Press, pp.201-221.
 
********** READ IN DATA and SUMMARIZE **********

infile using DATA66.dct, using(DATA66.dat)
* save DATA66, replace
desc
sum

* Define the exogenous regressors using the global macro exogregressors
global exogregressors black south76 smsa76 reg2-reg9 /*
 */  smsa66 momdad14 sinmom14 nodaded nomomed daded momed famed1-famed8

* Write data to a text (ascii) file so can use with programs other than stata  
outfile wage76 grade76 exp76 expsq76 col4 age76 agesq76 black south76 smsa76 reg2-reg9 /*
 */  smsa66 momdad14 sinmom14 nodaded nomomed daded momed famed1-famed8 /*
 */  using mma04p4ivweak.asc, replace


********** (1) OLS AND IV ESTIMATES: COLUMNS 1 AND 2 OF KLING TABLE 1

* RETAIN cases for the analysis
* Here drop if missing wages or missing schooling or not at first interview
keep if wage76!=. & grade76!=. & nlsflt==1

* DESCRIBE dependent variable, regressors and instruments
desc wage76 grade76 exp76 expsq76 col4 age76 agesq76 $exogregressors 

* SUMMARIZE dependent variable, regressors and instruments
sum wage76 grade76 exp76 expsq76 col4 age76 agesq76 $exogregressors 

* OLS estimates of return to schooling.
*  This regression computes schooling coeff, se for Table1 col 1 p.359
*  based on all cases (age grp 14-24) reported highest grd cmpl 76

reg wage76 grade76 exp76 expsq76 $exogregressors 
estimates store ols

* IV Instrumental variables estimates of return to schooling.
*  This regression computes schooling coeff and se for Table 1. col 2 p.359
*   Endogenous variables: schooling, experience, experience squared
*   Excl instruments: college in cnty, age age^2
*   based on all cases (age grp 14-24) reported highest grd cmpl 76 ***/

ivreg wage76 $exogregressors    /*
  */  (grade76 exp76 expsq76 = col4 age76 agesq76 $exogregressors)
estimates store iv

********** (2) NEW ANALYSIS: HETEROSKEDASTIC ROBUST STANDARD ERRORS **********

* Heteroskedastic errors makes little difference here.

quietly reg wage76 grade76 exp76 expsq76 $exogregressors
hettest   /* Shows that here there is no heteroskeadsticity for OLS */
quietly reg wage76 grade76 exp76 expsq76 $exogregressors, robust
estimates store olshet

quietly ivreg wage76 $exogregressors   /*
  */ (grade76 exp76 expsq76 = col4 age76 agesq76 $exogregressors), robust 
estimates store ivhet

**** DISPLAY RESULTS IN TABLE 4.5 p.111

* Table 4.5 p.111: OLS and IV estimates, s.e.'s and R^2 in Table 4.5

* Table reports only the coefficient and standard erros for grade76
estimates table ols olshet iv ivhet, /*
   */ se stats(N ll r2 rss mss rmse df_r) b(%10.4f)

********** (3) NEW ANALYSIS: CHECK FOR WEAK INSTRUMENTS **********

* Model is y = b1*x1 + x2'b2 + u
* where x1 is scalar endogenous  (grade76)
* where x2 is vector of regressors that includes 
*          exp76 and exp76 which are also endogenous
*          and $exogregressors which are exogenous
* and the instruments Z are grade76 col4 age76 agesq76 $exogregressors       

* Check for weak instruments
* Focus on grade76 but can also do this for the other two endogenous regressors. 
* In this example no problems for the other two: 
* as age and age-squared are good instruments for exp and exp-squared.

**** (A) Simple analysis R-squared and F-test  [Given in Table 4.5]

* R2 from regress endogenous regressor on instruments
* This is same as correlation between x1 and projection of x1 on Z
quietly reg grade76 col4 age76 agesq76 $exogregressors 
di e(r2) "  r2 of x1 on Z" 

* Do the partial F-test on the three instruments
* This is the standard first-stage regression F-test

**** DISPLAY RESULT IN TABLE 4.5 page 111 

* First-stage F statistic given in Table 4.5
test col4 age76 agesq76 

* Compare this to R-squared when only regress on instruments without Z
quietly reg grade76 $exogregressors 
di e(r2) "  r2 of x1 on Z with the three additional instruments dropped"

* Obtain first-stge F for the other two endogenous
quietly reg exp76 col4 age76 agesq76 $exogregressors 
test col4 age76 agesq76
quietly reg expsq76 col4 age76 agesq76 $exogregressors 
test col4 age76 agesq76 

**** (B) Minimum eigenvalue of matrix analog of the first-stage F statistic
*        proposed by Stock et al (2002) and tables in Stock and Yogo (2003)
* This test is not done here.

**** (C) Bound et al (1995) partial R-squared

* Not relevant here as more than one endogenous regressor
* If only one endogenous regressor x1 Bound et al purge the effect of x2
* by (1) get residual from regress x1 on x2  
*    (2) get the residuals from regress z on x2
* and then get the R-squared from regress (1) on (2).

**** (D) Shea (1997) partial R-squared [Given in Table 4.5]

* Here we have three endogenous regressors.
* Focus on the endogenous schooling regressor. 
* For the other two just need to replace the first line of (1)
* e.g. quietly reg exp76 grade76 expsq76 $exogregressors
* and replace the first line of (2B)
* e.g. quietly reg exp76hat grade76hat expsq76hat $exogregressors

* (1) Form x1 - x1tilda: residual from regress x1 on other regressors
quietly reg grade76 exp76 expsq76 $exogregressors
predict x1minusx1tilda, resid

* (2) Form x1hat - x1hattilda: residual from regress x1hat on fitted values of other regressors
* (2A) First get the fitted values from regress endogenous on instruments
quietly reg grade76 col4 age76 agesq76 $exogregressors
predict grade76hat, xb
di e(r2) "  r2 from regress x1 on Z" 
quietly reg exp76 col4 age76 agesq76 $exogregressors
predict exp76hat, xb
di e(r2) "  r2 from regress second endog regressor on Z"
quietly reg expsq76 col4 age76 agesq76 $exogregressors
predict expsq76hat, xb
di e(r2) "  r2 from regress third endog regressor on Z"
* Fitted values for the exogenous from regress exogenous on instruments are the exogenous
* (2B) Run the regression of x1hat on fitted values of other regressors
quietly reg grade76hat exp76hat expsq76hat $exogregressors
di e(r2) "  r2 from regress prediction of x1 on predictions of x2
predict x1hatminusx1hattilda, resid

* (3) Form the correlation between (1) and (2)  
corr x1minusx1tilda x1hatminusx1hattilda

**** DISPLAY RESULT IN TABLE 4.5 page 111

* Shea's Partial R^2 in Table 4.5
di r(rho)^2 "  Shea's partial R-squared measure" 

sum grade76 grade76hat exp76 exp76hat expsq76 expsq76hat grade76 x1minusx1tilda x1hatminusx1hattilda grade76hat

**** (E) Poskitt-Skeels (2002) partial R-squared
* Not done here

**** (F) If model was over-identified then do test of over-identifying restrictions
* Not done here as model is just-identified

********** CLOSE OUTPUT
log close
clear 
exit
