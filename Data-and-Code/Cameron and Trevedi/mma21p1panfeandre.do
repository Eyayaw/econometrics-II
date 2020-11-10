* MMA21P1FEANDRE.DO  March 2005 for Stata version 8.0

log using mma21p1panfeandre.txt, text replace

********** OVERVIEW OF MMA21P1PANBFEANDRE.DO **********

* STATA Program 
* copyright C 2005 by A. Colin Cameron and Pravin K. Trivedi 
* used for "Microeconometrics: Methods and Applications" 
* by A. Colin Cameron and Pravin K. Trivedi (2005)
* Cambridge University Press

* Chapter 21.3.1-3 pages 709-14 
* Program performs basic panel analysis, mainly using XTREG:
* It derives most of Table 21.1 and Figures 21.1-21.4
*  (1) pooled OLS
*  (2) between
*  (3) within (or fixed effects)
*  (4) first differences
*  (5) random effects - GLS
*  (6) random effects - MLE
*  (7) Hausman test of FE versus RE
* Standard errors are default plus panel bootstrap

* The individual effects model is 
*   y_it = x_it'b + a_i + e_it
* Default panel output assumes e_it is random.
* This is usually too strong an assumption.
* Instead should get panel-robust or cluster-robust errors after xtreg
* See Section 21.2.3 pages 709-12
* Stata Version 8 does not do this but Stata version 9 does. 

* Three ways to obtain panel-robust se's for fixed and random effects models:
* (1) Use Stata version 9 and cluster option in xtreg
* (2) Use Stata version 8 xtreg and then panel bootstrap (this program)
* (3) Use Stata version 8 regress cluster option on transformed model (next program)

* The four basic linear panel programs are
*   mma21p1panfeandre.do    Linear fixed and random effects using xtreg
*   mma21p2panfeandre.do    Linear fe and re using transformation and regress
*                           plus also has valid Hausman test
*   mma21p3panresiduals.do  Residual analysis after linear fe and re 
*   mma21p4panpangls.do     Pooled panel OLS and GLS

* To run this program you need data file
*    MOM.dat  

* To speed up this program reduce nreps, the number of bootstraps 
* used in the panel bootstrap to get panel-robust standard errors

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

********** READ DATA **********

* The data are in ascii file MOM.dat
* There are 532 individuals with 10 lines (years) per individual
* Read in using Infile: FREE FORMAT WITHOUT DICTIONARY
infile lnhr lnwg kids ageh agesq disab id year using MOM.dat

********** DATA TRANSFORMATIONS AND CHECK **********

* Create year dummies
tabulate year, generate(dyear)

* The following lists the variables in data set and summarizes data
describe
summarize
save mom, replace

* The following summarizes panel features for completeness
iis id
tis year
xtdes
xtsum lnhr lnwg kids ageh agesq disab

********** DEFINE GLOBALS INCLUDING REGRESSOR LIST **********

* Number of reps for the boostrap
* Table 21.2 pge 710 used 500
global nreps 500

* The regression below are of lnhrs on lnwg
* Additional regressors to be included below are defined in xextra
* Choose one of the following

* No additional regressors
global xextra
global xextrashort

* Include year dummies with one ommitted (or two omitted for first differences)
* global xextra dyear1 dyear2 dyear3 dyear3 dyear4 dyear5 dyear6 dyear7 dyear8 dyear9
* global xextrashort dyear2 dyear3 dyear3 dyear4 dyear5 dyear6 dyear7 dyear8 dyear9

* Include socioeconomic characteristics
* global xextra kids ageh agesq disab
* global xextrashort kids ageh agesq disab

********* DIFFERENT PANEL ESTIMATES pages 709-14 **********

* Note that in the first xt command need to give  , i(id)
* to indicate that the ith observation is for the ith id

* XTDATA permits plots of between, within and overall
* Useful for looking at the data. See Stata manual under xtdata for example.
* XTREG gives between, within and RE estiamtes though not correct standard errors
 
* The graphs below use new Stata 8 graphics
* Change graphics scheme from default s2color to s1mono for printing
set scheme s1mono
* The following graphs include
*   legend(pos(4) ring(0) col(1)) 
*        changes position of legend to four o'clock
*   legend( label(1 "Data used") label(2 "Smoothed fit") label(3 "Linear fit"))
*        changes labels for the legends

*** (1) POOLED OLS (OVERALL) REGRESSION (Table 21.2 POLS column and Figure 21.1)

use mom, clear

* Wrong formula OLS standard errors require e_it is i.i.d.
regress lnhr lnwg $xextra
estimates store polsiid

* Wrong White heteroskesdastic-consistent standard errors 
* assume standard errors require e_it is independent over i
regress lnhr lnwg $xextra, robust
estimates store polshet

* Correct panel robust standard errors
regress lnhr lnwg $xextra, cluster(id)
estimates store polspanel 

* Correct panel bootstrap standard errors
* Note that use cluster option so that bootstrap is over just i and not both i and t
set seed 10001
bs "regress lnhr lnwg $xextra" "_b[lnwg] _b[_cons]", cluster(id) reps($nreps) level(95)
matrix polsbootse = e(se)

* Overall plot of data with lowess local regression line - Figure 21.1 page 712
graph twoway (scatter lnhr lnwg, msize(vsmall)) (lowess lnhr lnwg) (lfit lnhr lnwg), /* 
  */ scale (1.2) plotregion(style(none)) /*
  */ title("Pooled (Overall) Regression") /* 
  */ xtitle("Log hourly wage", size(medlarge)) xscale(titlegap(*5)) /*
  */ ytitle("Log annual hours", size(medlarge)) yscale(titlegap(*5)) /*
  */ legend(pos(4) ring(0) col(1)) legend(size(small)) /*
  */ legend( label(1 "Original data") label(2 "Nonparametric fit") label(3 "Linear fit")) 
graph export ch21pantot.wmf, replace

*** (2) BETWEEN REGRESSION (Table 21.2 Between column and Figure 21.2)

use mom, clear

* Usual standard errors assume iid error
xtreg lnhr lnwg, be i(id)
estimates store beiid

* Heteroskedasticity robust standard errors
* Stata has no option for this. See ch21panel2.do 

* Correct panel bootstrap standard errors
set seed 10001
bootstrap "xtreg lnhr lnwg, be i(id)" "_b[lnwg] _b[_cons]", cluster(id) reps($nreps) level(95)
matrix bebootse = e(se)

* Betweeen plot of data with lowess local regression line - Figure 21.2 page 712
iis id
xtdata, be
graph twoway (scatter lnhr lnwg, msize(vsmall)) (lowess lnhr lnwg) (lfit lnhr lnwg), /* 
  */ scale (1.2) plotregion(style(none)) /*
  */ title("Between Regression") /*
  */ xtitle("Log hourly wage", size(medlarge))  xscale(titlegap(*5)) /*
  */ ytitle("Log annual hours", size(medlarge))  yscale(titlegap(*5)) /*
  */ legend(pos(4) ring(0) col(1)) legend(size(small)) /*
  */ legend( label(1 "Averages") label(2 "Nonparametric fit") label(3 "Linear fit")) 
graph export ch21panbe.wmf, replace

*** (3) WITHIN (FIXED EFFECTS) REGRESSION (Table 21.2 Within column and Figure 21.3)

use mom, clear

* Usual standard errors assume iid error
xtreg lnhr lnwg $xextra, fe i(id)
estimates store feiid

* Correct panel robust standard errors
* Stata has no option for this. See ch21panel2.do

* Correct panel bootstrap standard errors
set seed 10001
bootstrap "xtreg lnhr lnwg $xextra, fe i(id)" "_b[lnwg] _b[_cons]", cluster(id) reps($nreps) level(95)
matrix febootse = e(se)

* Within plot of data with lowess local regression line - Figure 21.3 page 712
iis id
xtdata, fe
graph twoway (scatter lnhr lnwg, msize(vsmall)) (lowess lnhr lnwg) (lfit lnhr lnwg), /* 
  */ scale (1.2) plotregion(style(none)) /*
  */ title("Within (Fixed Effects) Regression") /*
  */ xtitle("Log hourly wage", size(medlarge)) xscale(titlegap(*5)) /*
  */ ytitle("Log annual hours", size(medlarge)) yscale(titlegap(*5)) /*
  */ legend(pos(4) ring(0) col(1)) legend(size(small)) /*
  */ legend( label(1 "Deviations from average") label(2 "Nonparametric fit") label(3 "Linear fit")) 
graph export ch21panfe.wmf, replace

*** (4) FIRST DIFFERENCES REGRESSION (Table 21.2 First diff column and Figure 21.4)

* Stata has no command for first differences regression
* Though may be possible with xtabond
* Instead need to create differenced data

use mom, clear
* The following only works if each observation is (i,t) 
* and within i the data are ordered by t
gen dlnhr = lnhr - lnhr[_n-1]
gen dlnwg = lnwg - lnwg[_n-1]
gen dkids = kids - kids[_n-1]
gen dageh = ageh - ageh[_n-1]
gen dagesq = agesq - agesq[_n-1]
gen ddisab = disab - disab[_n-1]
* The following drops the first year which here is 1979
drop if year == 1979

* Usual standard errors assume iid error
regress dlnhr dlnwg $xextrashort
estimates store fdiffiid

* Correct panel robust standard errors
regress dlnhr dlnwg $xextrashort, cluster(id) 
estimates store fdiffpanel

* "Robust" standard errors only control for heteroskedasticity
regress dlnhr dlnwg $xextrashort, robust
estimates store fdiffhet

* Correct panel bootstrap standard errors
set seed 10001
bs "regress dlnhr dlnwg $xextrashort" "_b[dlnwg] _b[_cons]", cluster(id) reps($nreps) level(95)
matrix fdiffbootse = e(se)

* First differences plot with lowess local regression line - Figure 21.4 page 713
graph twoway (scatter dlnhr dlnwg, msize(vsmall)) (lowess dlnhr dlnwg) (lfit dlnhr dlnwg), /* 
  */ scale (1.2) plotregion(style(none)) /*
  */ title("First Differences Regression") /*
  */ xtitle("Log hourly wage", size(medlarge)) xscale(titlegap(*5)) /* 
  */ ytitle("Log annual hours", size(medlarge)) yscale(titlegap(*5)) /*
  */ legend(pos(4) ring(0) col(1)) legend(size(small)) /*
  */ legend( label(1 "First differences") label(2 "Nonparametric fit") label(3 "Linear fit"))
graph export ch21panfd.wmf, replace

*** (5) RANDOM EFFECTS GLS REGRESSION (Table 21.2 RE-GLS column)

use mom, clear

* Usual standard errors assume iid error
xtreg lnhr lnwg, re i(id)
estimates store reglsiid

* Correct panel robust standard errors
* Stata has no option for this. See ch21panel2.do
* or use xtgee corr(exchangeable), robust see ch21panel4.do

* Correct panel bootstrap standard errors
set seed 10001
bootstrap "xtreg lnhr lnwg, re i(id)" "_b[lnwg] _b[_cons]", cluster(id) reps($nreps) level(95)
matrix reglsbootse = e(se)

*** (6) RANDOM EFFECTS MLE REGRESSION (Table 21.2 RE-MLE column) 

use mom, clear

* Usual standard errors assume iid error
xtreg lnhr lnwg, mle i(id)
estimates store remleiid

* Correct panel robust standard errors
* Stata has no option for this. See ch21panel2.do

* Correct panel bootstrap standard errors
set seed 10001
bootstrap "xtreg lnhr lnwg, mle i(id)" "_b[lnwg] _b[_cons]", cluster(id) reps($nreps) level(95)
matrix remlebootse = e(se)

* Population averaged is similar to re  (gives similar to mle version of re)
* Exactly same as xtgee, i(id)
xtreg lnhr lnwg, pa i(id)
estimates store paiid

*** (7) HAUSMAN TEST (NOT ROBUST)

* Hausman test of fixed versus random effects
* The FE estimates are saved in feiid
* The RE estimates are saved in reglsiid

* From Section 21.4.3 pages 717-9 this usual implementation of the Hausman test
* is invalid if there is any intracluster correlation left in the RE model
* as then the RE estimator is no longer fully efficient
* so  Var[b_RE - b_FE] does not equal Var[b_FE] - V[b_RE]

* Following is not valid - see MMA21P2PANMANUAL.DO for robust version
hausman feiid reglsiid

********* DISPLAY RESULTS - Table 21.2 on page 710 *********

* Standard error using iid errors and in somce cases panel
estimates table polsiid polshet polspanel beiid feiid, /*
   */ se stats(N ll r2 tss rss mss rmse df_r) b(%10.3f)
estimates table fdiffiid fdiffhet fdiffpanel reglsiid remleiid, /*
   */ se stats(N ll r2 tss rss mss rmse df_r) b(%10.3f)
estimates table paiid, se stats(N ll r2 tss rss mss rmse df_r) b(%10.3f)

* Standard errors using panel bootstrap (regular bootstrap for between)
matrix list polsbootse
matrix list bebootse
matrix list febootse
matrix list fdiffbootse
matrix list reglsbootse
matrix list remlebootse

********** CLOSE OUTPUT *********
log close
* clear
* exit
