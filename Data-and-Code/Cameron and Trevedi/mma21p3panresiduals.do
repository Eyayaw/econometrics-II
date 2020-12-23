* MMA21P3PANRESIDUALS.DO  March 2005 for Stata version 8.0

log using mma21p3panresiduals.txt, text replace

********** OVERVIEW OF MMA21P3PANRESIDUALS.DO **********

* STATA Program 
* copyright C 2005 by A. Colin Cameron and Pravin K. Trivedi 
* used for "Microeconometrics: Methods and Applications" 
* by A. Colin Cameron and Pravin K. Trivedi (2005)
* Cambridge University Press

* Chapter 21.3.4 pages 713-15 Residual analysis
* This program 
* (1) estimates correlations for 
*     - dependent variable
*     - regressors variable
*     - residuals from pooled ols [Table 21.3]
*     - residuals from within estimation [Table 21.4]
*     - residuals from random effects estimation
* (2) separately estimates correlations for
*     - residuals from first differences estiamtion
* (3) gets correlations for each individual observation

* The code is very limited:
*   - it considers only one regressor
*   - it assumes a balanced data set with exactly 10 years of data per obnservations
*   - it does not use loops for transformations which would generalize code

* The four basic linear panel programs are
*   mma21p1panfeandre.do    Linear fixed and random effects using xtreg
*   mma21p2panfeandre.do    Linear fe and re using transformation and regress
*                           plus also has valid Hausman test
*   mma21p3panresiduals.do  Residual analysis after linear fe and re 
*   mma21p4panpangls.do     Pooled panel OLS and GLS

* To run you need file
*   MOM.dat    
* in your directory

********** SETUP **********

set more off
version 8.0
set scheme s1mono  /* Graphics scheme */

********** DATA DESCRIPTION **********

*  The original data is from 
*  Jim Ziliak (1997)
*  "Efficient Estimation With Panel Data when Instruments are Predetermined: 
* An Emprirical Comparison of Moment-Condition Estimators" 
* Journal of Business and Economic Statistics, 15, 419-431

* File MOM.dat has data on 532 men over 10 years (1979-1988) 
* Data are space-delimited ordered by person with separate line for each year
* So id 1 1979, id 1 1980, ..., id 1 1988, id 2 1979, 1d 2 1980, ...
* 8 variables: 
* lnhr lnwg kids ageh agesq disab id year

* File MOM.dat is the version of the data posted at the JBES website
* Note that in chapter 22 we instead use MOMprecise.dat
* which is the same data set but with more significant digits

********** READ DATA **********
*
* The data are in ascii file MOM.dat
* There are 532 individuals with 10 lines (years) per individual
* Read in using Infile: FREE FORMAT WITHOUT DICTIONARY
infile lnhr lnwg kids ageh agesq disab id year using MOM.dat
summarize

************ (1) ANALYSIS: OBTAIN KEY AUTOCORRELATIONS Tables 21.3, 21.4 **********

** RUN REGRESSIONS AND GET RESIDUALS OF INTEREST

* pooled ols
regress lnhr lnwg
predict upols, residuals

* fixed effects (within)
xtreg lnhr lnwg, fe i(id)
predict ufe, e

* random effects
xtreg lnhr lnwg, re i(id)
predict ure, e

summarize upols ufe ure
save mom3, replace

** TRANSFORM DATA FROM LONG FORM TO WIDE FORM

* Here just do this for lnhr and lnwg and the residuals 
keep lnhr lnwg id year upols ufe ure
reshape wide lnhr lnwg upols ufe ure, i(id) j(year)

* Since year is 1979 to 1988 this will create 
* lnhr1979 to lnhr1988 and lnwg1979 to lnwg1988 

summarize

** OBTAIN THE VARIOUS CORRELATIONS

corr lnhr1979 lnhr1980 lnhr1981 lnhr1982 lnhr1983 lnhr1984 lnhr1985 lnhr1986 lnhr1987 lnhr1988
corr lnwg1979 lnwg1980 lnwg1981 lnwg1982 lnwg1983 lnwg1984 lnwg1985 lnwg1986 lnwg1987 lnwg1988
* The following gives Table 21.3 p.714
corr upols1979 upols1980 upols1981 upols1982 upols1983 upols1984 upols1985 upols1986 upols1987 upols1988
corr ure1979 ure1980 ure1981 ure1982 ure1983 ure1984 ure1985 ure1986 ure1987 ure1988
* The following gives Table 21.4 p.715
corr ufe1979 ufe1980 ufe1981 ufe1982 ufe1983 ufe1984 ufe1985 ufe1986 ufe1987 ufe1988

* The following does estimation for just one year
regress lnhr1979 lnwg1979

************ (2) ANALYSIS: OBTAIN AUTOCORRELATIONS FOR FIRST DIFFERNCES

** SET UP THE DATA
use mom, clear
gen dlnhr = lnhr - lnhr[_n-1]
gen dlnwg = lnwg - lnwg[_n-1]
* The following drops the first year which here is 1979
drop if year == 1979
regress dlnhr dlnwg
predict ufdiff, residuals
* Here just do this for lnhr and lnwg and the residuals 
keep dlnhr dlnwg ufdiff id year
reshape wide dlnhr dlnwg ufdiff, i(id) j(year)
summarize

** GET THE CORRELATIONS
corr dlnhr1980 dlnhr1981 dlnhr1982 dlnhr1983 dlnhr1984 dlnhr1985 dlnhr1986 dlnhr1987 dlnhr1988
corr dlnwg1980 dlnwg1981 dlnwg1982 dlnwg1983 dlnwg1984 dlnwg1985 dlnwg1986 dlnwg1987 dlnwg1988
corr ufdiff1980 ufdiff1981 ufdiff1982 ufdiff1983 ufdiff1984 ufdiff1985 ufdiff1986 ufdiff1987 ufdiff1988

************ (3) ANALYSIS: CORRELATIONS FOR AN INDIVIDUAL OBSERVATION

* Look at correlations for each individual

** TRANSFORM DATA FROM LONG FORM TO WIDE FORM FOR INDIVIDUALS

use mom3, replace
* Here just do this for lnhr and lnwg and the residuals 
keep lnhr lnwg id year
reshape wide lnhr lnwg, i(year) j(id)
* Note that i and j are reversed

* Since year is 1979 to 1988 this will create 
* lnhr1979 to lnhr1988 and lnwg1979 to lnwg1988 

tsset year

* First-order Correlation over T years for the first observation
corr lnhr1 L.lnhr1
* First-order Correlation over T years for the second observation
corr lnhr2 L.lnhr2
* And so on

********** CLOSE OUTPUT
log close
clear
exit

