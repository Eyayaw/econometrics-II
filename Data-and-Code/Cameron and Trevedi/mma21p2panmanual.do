* MMA21P2PANMANUAL.DO  March 2005 for Stata version 8.0

log using mma21p2panmanual.txt, text replace

********** OVERVIEW OF MMA21P2PANMANUAL.DO **********

* STATA Program 
* copyright C 2005 by A. Colin Cameron and Pravin K. Trivedi 
* used for "Microeconometrics: Methods and Applications" 
* by A. Colin Cameron and Pravin K. Trivedi (2005)
* Cambridge University Press

* Chapter 21.3.1-3 pages 709-14 
* Program performs basic panel analysis and gets panel robust se's
* by first transforming model and then using REGRESS
* It also presents a valid Hausman test of FE versus RE model

* This program estimates 
*    (2) between estimator by regress y_bar on x_bar
*    (4) within estimator by regress (y - y_bar) on (x - x_bar)
*    (5) random effects gls by regress (y - rho*y_bar) on (x - rho*x_bar)
*    (6) random effects mle by regress (y - rho*y_bar) on (x - rho*x_bar)
*    (7) robust variant of the Hausman test
* and calculates 
*    - usual standard errors 
*        (which may differ from xtreg due to different degrees of freedom) 
*    - panel robust standard errors
*        (which for RE simplify by assuming lamda_hat is known not estimated)
*    - panel bootstrap standard errors 
*        (which should equal panel robust from ch21panel.do as #bootstrap reps --> infinity)
*    - heteroskedasticity robust standard errors
*        (which are wrong but included for comparison with others)

* The code is very limited:
*   - it considers only one regressor
*   - it assumes a balanced data set with exactly 10 years of data per obnservations
*   - it does not use loops for transformations which would generalize code

* NOTE: If have Stata Version 9 (rather than version 8) a simpler way to proceed is
* to directly use XTREG (see program mma21p1panfeandre.do) with option cluster(id)

* The four basic linear panel programs are
*   mma21p1panfeandre.do    Linear fixed and random effects using xtreg
*   mma21p2panfeandre.do    Linear fe and re using transformation and regress
*                           plus also has valid Hausman test
*   mma21p3panresiduals.do  Residual analysis after linear fe and re 
*   mma21p4panpangls.do     Pooled panel OLS and GLS

* To run this program you need data file
*    MOM.dat  
* in your directory

* To speed up this program reduce nreps, the number of bootstraps 
* used in the panel bootstrap. 

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

* The data are in ascii file MOM.dat
* There are 532 individuals with 10 lines (years) per individual
* Read in using Infile: FREE FORMAT WITHOUT DICTIONARY
infile lnhr lnwg kids ageh agesq disab id year using MOM.dat
summarize

********** DEFINE GLOBALS **********

* Number of reps for the boostrap
* Table 21.1 used 500
global nreps 500

******** RUN REGRESSIONS USING XTREG **********

* This is to verify alternative estimates later on
* And for random effects it saves lamda 
* used later on to construct transformed regression
* of (y - lamda*y_1) on (x - lamda*x_1)

xtreg lnhr lnwg, be i(id)
estimates store bextreg

xtreg lnhr lnwg, fe i(id)
estimates store fextreg

xtreg lnhr lnwg, re i(id)
estimates store reglsxtreg
scalar sesq = e(sigma_e)^2
scalar susq = e(sigma_u)^2
scalar lamdaregls = 1 - sqrt( sesq / (e(Tbar)*susq + sesq) )
di lamdaregls

xtreg lnhr lnwg, mle i(id)
estimates store remlextreg
scalar sesq2 = e(sigma_e)^2
scalar susq2 = e(sigma_u)^2
scalar lamdaremle = 1 - sqrt( sesq2 / (e(g_avg)*susq2 + sesq2) )
di lamdaremle

******** ANALYSIS: FE, RE and FD ESTIMATORS CALCULATED MANUALLY **********

*** FIRST TRANSFORM DATA FROM LONG FORM TO WIDE FORM

* Here just do this for lnhr and lnwg
keep lnhr lnwg id year
reshape wide lnhr lnwg, i(id) j(year)

* Since year is 1979 to 1988 this will create 
* lnhr1979 to lnhr1988 and lnwg1979 to lnwg1988 

summarize

*** (1) POOLED OLS (OVERALL) REGRESSION

* Not relevant

*** (2) CREATE INDIVIDUAL AVERAGES AND DO BETWEEN REGRESSION

gen avelnhr = (lnhr1979+lnhr1980+lnhr1981+lnhr1982+lnhr1983+lnhr1984+ /*
              */  lnhr1985+lnhr1986+lnhr1987+lnhr1988) / 10
gen avelnwg = (lnwg1979+lnwg1980+lnwg1981+lnwg1982+lnwg1983+lnwg1984+ /*
              */  lnwg1985+lnwg1986+lnwg1987+lnwg1988) / 10

* Should replicate xtreg, be
regress avelnhr avelnwg
estimates store bebyols

* Better is the following as gives heteroskedastic robust standard errors
regress avelnhr avelnwg, robust
estimates store behet

* Or could bootstrap
bootstrap "regress avelnhr avelnwg" "_b[avelnwg] _b[_cons]", reps(200) level(95)
matrix bebootse = e(se)

*** (3) CREATE DIFFERENCED DATA FOR FE AND RE

* Continue with data already and then reshape
* Mean difference for FE and quasi for RE-GLS and RE-MLE

* Mean difference for FE
gen mdlnhr1979 = lnhr1979 - avelnhr
gen mdlnhr1980 = lnhr1980 - avelnhr
gen mdlnhr1981 = lnhr1981 - avelnhr
gen mdlnhr1982 = lnhr1982 - avelnhr
gen mdlnhr1983 = lnhr1983 - avelnhr
gen mdlnhr1984 = lnhr1984 - avelnhr
gen mdlnhr1985 = lnhr1985 - avelnhr
gen mdlnhr1986 = lnhr1986 - avelnhr
gen mdlnhr1987 = lnhr1987 - avelnhr
gen mdlnhr1988 = lnhr1988 - avelnhr
gen mdlnwg1979 = lnwg1979 - avelnwg
gen mdlnwg1980 = lnwg1980 - avelnwg
gen mdlnwg1981 = lnwg1981 - avelnwg
gen mdlnwg1982 = lnwg1982 - avelnwg
gen mdlnwg1983 = lnwg1983 - avelnwg
gen mdlnwg1984 = lnwg1984 - avelnwg
gen mdlnwg1985 = lnwg1985 - avelnwg
gen mdlnwg1986 = lnwg1986 - avelnwg
gen mdlnwg1987 = lnwg1987 - avelnwg
gen mdlnwg1988 = lnwg1988 - avelnwg

* Quasi difference for RE - GLS
gen reglsdlnhr1979 = lnhr1979 - lamdaregls*avelnhr
gen reglsdlnhr1980 = lnhr1980 - lamdaregls*avelnhr
gen reglsdlnhr1981 = lnhr1981 - lamdaregls*avelnhr
gen reglsdlnhr1982 = lnhr1982 - lamdaregls*avelnhr
gen reglsdlnhr1983 = lnhr1983 - lamdaregls*avelnhr
gen reglsdlnhr1984 = lnhr1984 - lamdaregls*avelnhr
gen reglsdlnhr1985 = lnhr1985 - lamdaregls*avelnhr
gen reglsdlnhr1986 = lnhr1986 - lamdaregls*avelnhr
gen reglsdlnhr1987 = lnhr1987 - lamdaregls*avelnhr
gen reglsdlnhr1988 = lnhr1988 - lamdaregls*avelnhr
gen reglsdlnwg1979 = lnwg1979 - lamdaregls*avelnwg
gen reglsdlnwg1980 = lnwg1980 - lamdaregls*avelnwg
gen reglsdlnwg1981 = lnwg1981 - lamdaregls*avelnwg
gen reglsdlnwg1982 = lnwg1982 - lamdaregls*avelnwg
gen reglsdlnwg1983 = lnwg1983 - lamdaregls*avelnwg
gen reglsdlnwg1984 = lnwg1984 - lamdaregls*avelnwg
gen reglsdlnwg1985 = lnwg1985 - lamdaregls*avelnwg
gen reglsdlnwg1986 = lnwg1986 - lamdaregls*avelnwg
gen reglsdlnwg1987 = lnwg1987 - lamdaregls*avelnwg
gen reglsdlnwg1988 = lnwg1988 - lamdaregls*avelnwg

* Quasi difference for RE - MLE
gen remledlnhr1979 = lnhr1979 - lamdaremle*avelnhr
gen remledlnhr1980 = lnhr1980 - lamdaremle*avelnhr
gen remledlnhr1981 = lnhr1981 - lamdaremle*avelnhr
gen remledlnhr1982 = lnhr1982 - lamdaremle*avelnhr
gen remledlnhr1983 = lnhr1983 - lamdaremle*avelnhr
gen remledlnhr1984 = lnhr1984 - lamdaremle*avelnhr
gen remledlnhr1985 = lnhr1985 - lamdaremle*avelnhr
gen remledlnhr1986 = lnhr1986 - lamdaremle*avelnhr
gen remledlnhr1987 = lnhr1987 - lamdaremle*avelnhr
gen remledlnhr1988 = lnhr1988 - lamdaremle*avelnhr
gen remledlnwg1979 = lnwg1979 - lamdaremle*avelnwg
gen remledlnwg1980 = lnwg1980 - lamdaremle*avelnwg
gen remledlnwg1981 = lnwg1981 - lamdaremle*avelnwg
gen remledlnwg1982 = lnwg1982 - lamdaremle*avelnwg
gen remledlnwg1983 = lnwg1983 - lamdaremle*avelnwg
gen remledlnwg1984 = lnwg1984 - lamdaremle*avelnwg
gen remledlnwg1985 = lnwg1985 - lamdaremle*avelnwg
gen remledlnwg1986 = lnwg1986 - lamdaremle*avelnwg
gen remledlnwg1987 = lnwg1987 - lamdaremle*avelnwg
gen remledlnwg1988 = lnwg1988 - lamdaremle*avelnwg

*** NOW BACK TO LONG FORM

* Then back to long form
reshape long lnhr lnwg mdlnhr mdlnwg reglsdlnhr reglsdlnwg remledlnhr remledlnwg, i(id) j(year)

describe
summarize
save MOM2, replace

*** (4) FIXED EFFECTS ESTIMATOR USING DIFFERENCED DATA

* This should replicate xtreg, fe
regress mdlnhr mdlnwg
estimates store febyols

* This gives panel corrected standard errors
regress mdlnhr mdlnwg, cluster(id)
estimates store fepanel

* This gives panel bootstrap standard errors
* Similar to bootstrap applied to xtreg, fe
set seed 10001
bs "regress mdlnhr mdlnwg" "_b[mdlnwg] _b[_cons]", cluster(id) reps($nreps) level(95)
matrix febootse = e(se)

* This gives heteroskedasticity corrected standard errors that are not panel robust
regress mdlnhr mdlnwg, robust
estimates store fehet

*** (5) RANDOM EFFECTS - GLS ESTIMATOR USING DIFFERENCED DATA

* Should give same coefficient estimates as xtreg
* May give different standard errors as treats lamda as known
* but in practice the differnece is not great as lamda precisely estimated

* This should replicate xtreg, re
regress reglsdlnhr reglsdlnwg
estimates store reglsbyols

* This gives panel corrected standard errors
regress reglsdlnhr reglsdlnwg, cluster(id)
estimates store reglspanel

* This gives panel bootstrap standard errors
* Similar to bootstrap applied to xtreg, fe
set seed 10001
bs "regress reglsdlnhr reglsdlnwg" "_b[reglsdlnwg] _b[_cons]", cluster(id) reps($nreps) level(95)
matrix reglsbootse = e(se)

* This gives heteroskedasticity corrected standard errors that are not panel robust
regress reglsdlnhr reglsdlnwg, robust
estimates store reglshet

*** (6) RANDOM EFFECTS - MLE ESTIMATOR USING DIFFERENCED DATA

* Should give same coefficient estimates as xtreg
* May give different standard errors as treats lamda as known
* but in practice the differnece is not great as lamda precisely estimated

* This should replicate xtreg, mle
regress remledlnhr remledlnwg
estimates store remlebyols

* This gives panel corrected standard errors
regress remledlnhr remledlnwg, cluster(id)
estimates store remlepanel

* This gives panel bootstrap standard errors
* Similar to bootstrap applied to xtreg, fe
set seed 10001
bs "regress remledlnhr remledlnwg" "_b[remledlnwg] _b[_cons]", cluster(id) reps($nreps) level(95)
matrix remlebootse = e(se)

* This gives heteroskedasticity corrected standard errors that are not panel robust
regress reglsdlnhr reglsdlnwg, robust
estimates store remlehet

*** (7) ROBUST VARIANT OF HAUSMAN TEST

* From Section 21.4.3 pages 717-9 the usual implementation of the Hausman test
* is invalid if there is any intracluster correlation left in the RE model
* as then the RE estimator is no longer fully efficient
* so  Var[b_RE - b_FE] does not equal Var[b_FE] - V[b_RE]

* (7A) Nonrobust version of Hausman test by auxiliary regression
*      [will be similar to nonrobust version in mma21p1panfeandre.do]
regress reglsdlnhr reglsdlnwg mdlnwg
scalar Hnonrobust = (_b[mdlnwg]/_se[mdlnwg])^2
di Hnonrobust

* Perform preferred valid robust version of Hausman test
* This gives the results presented on p.719
regress reglsdlnhr reglsdlnwg mdlnwg, cluster(id)
scalar Hrobust = (_b[mdlnwg]/_se[mdlnwg])^2
di Hrobust

********* DISPLAY RESULTS - Table 21.2 on page 710 *********

* All estimates should be equal for a given estimator.
* The standard errors will vary. 
* The first and second assume iid errors and generally will be the same.
* The third assumes heteroskedastic errors, but are not panel robust.
* The fourth are panel robust and also allow for heteroskedasticity. 
estimates table bextreg bebyols behet, b(%10.3f) se /*
    */ stats(N ll r2 tss rss mss rmse df_r)
estimates table fextreg febyols fehet fepanel, b(%10.3f) se /*
    */ stats(N ll r2 tss rss mss rmse df_r)
estimates table reglsxtreg reglsbyols reglshet reglspanel, b(%10.3f) se /*
    */ stats(N ll r2 tss rss mss rmse df_r)
estimates table remlextreg remlebyols remlehet remlepanel, b(%10.3f) se /*
    */ stats(N ll r2 tss rss mss rmse df_r)

* The following are (panel) bootstrap standard errors
matrix list bebootse
matrix list febootse
* Note that the following two differ from mma21p1panfeandre.do
* as here the same value of lamda is used throught the bootstraps
matrix list remlebootse
matrix list reglsbootse

* For completeness give lamda
di lamdaregls
di lamdaremle

* Robust and nonrobust versions of Hausman test given on p.719
di Hnonrobust     /* Not valid if intracluster correlation */ 
di Hrobust        /* Valid if intracluster correlation */

********** CLOSE OUTPUT
log close
clear
exit

