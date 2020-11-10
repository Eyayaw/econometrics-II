* MMA21P4PANGLS.DO  March 2005 for Stata version 8.0

log using mma21p4pangls.txt, text replace

********** OVERVIEW OF MMA21P4PANGLS.DO **********

* STATA Program 
* copyright C 2005 by A. Colin Cameron and Pravin K. Trivedi 
* used for "Microeconometrics: Methods and Applications" 
* by A. Colin Cameron and Pravin K. Trivedi (2005)
* Cambridge University Press

* Chapter 21.5.5 page 725 Table 21.6 Pooled panel OLS and GLS
* Demonstrate pooled GLS estimation using XTGEE
*  (1) No correlation (i.e. pooled OLS)
*  (2) Equicorrelated
*  (3) AR1
*  (4) Unrestricted
* Standard errors are default plus panel boostrap

* To run you need file
*   MOM.dat    
* in your directory

* The four basic linear panel programs are
*   mma21p1panfeandre.do    Linear fixed and random effects using xtreg
*   mma21p2panfeandre.do    Linear fe and re using transformation and regress
*                           plus also has valid Hausman test
*   mma21p3panresiduals.do  Residual analysis after linear fe and re 
*   mma21p4panpangls.do     Pooled panel OLS and GLS

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

* File MOM.dat has data on 532 men over 10 years (1979-1988) 
* Data are space-delimited ordered by person with separate line for each year
* So id 1 1979, id 1 1980, ..., id 1 1988, id 2 1979, 1d 2 1980, ...
* 8 variables: 
* lnhr lnwg kids ageh agesq disab id year

* File MOM.dat is the version of the data posted at the JBES website
* Note that in chapter 22 we instead use MOMprecise.dat
* which is the same data set but with more significant digits

********** READ DATA AND SUMMARIZE **********
*
* The data are in ascii file MOM.dat
* There are 532 individuals with 10 lines (years) per individual
* Read in using Infile: FREE FORMAT WITHOUT DICTIONARY
infile lnhr lnwg kids ageh agesq disab id year using MOM.dat

describe
summarize

********** DEFINE GLOBALS INCLUDING REGRESSOR LIST *********

* Number of reps for the boostrap
* Table 21.6 used 500
global nreps 500

********* ANALYSIS: DIFFERENT POOLED GLS ESTIMATES USING XTGEE *********

*** (1) N0 ERROR CORRELATION - SAME AS POOLED OLS Table 21.7 first column

* Default standard error
xtgee lnhr lnwg, corr(independent) i(id)
estimates store ind
* "Robust" standard error
xtgee lnhr lnwg, corr(independent) i(id) robust
estimates store indrob
* Correct panel bootstrap standard errors
set seed 10001
bootstrap "xtgee lnhr lnwg, corr(independent) i(id)" "_b[lnwg] _b[_cons]", cluster(id) reps($nreps) level(95)
matrix indbootse = e(se)

*** (2) EQUICORRELATED - SAME AS RE-GLS Table 21.7 second column

* Default standard error
xtgee lnhr lnwg, corr(exchangeable) i(id)
estimates store exch
* "Robust" standard error
xtgee lnhr lnwg, corr(exchangeable) i(id) robust
estimates store exchrob
* Correct panel bootstrap standard errors
set seed 10001
bootstrap "xtgee lnhr lnwg, corr(exchangeable) i(id)" "_b[lnwg] _b[_cons]", cluster(id) reps($nreps) level(95)
matrix exchbootse = e(se)

*** (3) AR(1) Table 21.7 third column

* Default standard error
xtgee lnhr lnwg, corr(ar 1) i(id) t(year)
estimates store ar1
* "Robust" standard error
xtgee lnhr lnwg, corr(ar 1) i(id) t(year) robust
estimates store ar1rob
* Correct panel bootstrap standard errors
set seed 10001
bootstrap "xtgee lnhr lnwg, corr(ar 1) i(id)" "_b[lnwg] _b[_cons]", cluster(id) reps($nreps) level(95)
matrix ar1bootse = e(se)

*** (4) HOMOSKEDASTIC UNSTRUCTURED Table 21.7 fourth column

* Default standard error
xtgee lnhr lnwg, corr(unstructured) i(id) t(year)
estimates store unstr
* "Robust" standard error
xtgee lnhr lnwg, corr(unstructured) i(id) t(year) robust
estimates store unstrrob
* Correct panel bootstrap standard errors
set seed 10001
/* For some reason the following did not work 
bootstrap "xtgee lnhr lnwg, corr(unstructured) i(id)" "_b[lnwg] _b[_cons]", cluster(id) reps($nreps) level(95)
matrix unstrbootse = e(se)
*/

********** DISPLAY RESULTS IN TABLE 21.7 page 725 **********

* Standard error using iid errors and in some cases panel
estimates table ind indrob exch exchrob, /*
   */ se stats(N ll r2 tss rss mss rmse df_r) b(%10.3f)
estimates table ar1 ar1rob unstr unstrrob, /*
   */ se stats(N ll r2 tss rss mss rmse df_r) b(%10.3f)

* Standard errors using panel bootstrap (regular bootstrap for between)
matrix list indbootse
matrix list exchbootse
matrix list ar1bootse
matrix list unstrbootse

********** CLOSE OUTPUT **********
log close
clear
exit

