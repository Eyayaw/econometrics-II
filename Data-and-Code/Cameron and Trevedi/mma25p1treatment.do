* MMA25P1TREATMENT.DO  May 2005 for Stata version 8.0

log using mma25p1treatment.txt, text replace

********** OVERVIEW OF MMA25P1TREATMENT.DO **********

* STATA Program 
* copyright C 2005 by A. Colin Cameron and Pravin K. Trivedi 
* used for "Microeconometrics: Methods and Applications" 
* by A. Colin Cameron and Pravin K. Trivedi (2005)
* Cambridge University Press 

* Chapter 25.8.1-25.8.4 pages 889-893 Tables 25.3-25.4 and Fig. 25.3
* Evaluating treatment effect of training on Earnings
* using Dehejia-Wahba data (originally Lalonde data)

* (0) Summarize data for treatments and controls (Table 25.3)
* (1) Calculate the treatment effect by simple methods (Table 25.4)
*     To replicate some results in DW 1999
*     (1A) treatment-control
*     (1B) control function
*     (1C) before-after cpmparison
*     (1D) differences-in-differences
* (2) Calculate treatment effect by propensity score (matching by strata)
*     Last entry in Table 25.4 and Figure 25.3.

* The program MMA25P2MATCHING.DO uses propensity scores with matching
* methods more sophisticated than those usd in the MMA25P1TREAMENT.DO

* To run this program you need file 
*     nswpsid.da1

********** STATA SETUP **********

set more off
version 8
set scheme s1mono   /* Used for graphs */

********** DATA DESCRIPTION **********

* Data set nswpsid.da1 is data set nswpsid.da1 from Guido Imbens 
* http://emlab.berkeley.edu/users/imbens/index.shtml

* Data originally from DW99
*    R.H. Dehejia and S. Wahba (1999) 
*    "Causal Effects in Nonexperimental Studies: reevaluating the 
*    Evaluation of Training Programs", JASA, 1053-1062
* or DW02
*    R.H. Dehejia and S. Wahba (2002) 
*    "Propensity-score Matching Methods for Nonexperimental Causal
*     Studies", ReStat, 151-161
* which in turn are from 
*    Lalonde, R. (1986), "Evaluating the Econometric Evaluations of 
*    Training Programs with Experimental Data," AER,  604-620.

* Each observation is for an individual. 
* There are 2,675 observations: 185 in treated group and 2490 in control

* Variables are 
*  TREAT 1 if treated (NSW treated) and 0 if not (PSID-1 control)
*  AGE   in years
*  EDUC  in years   
*  BLACK 1 if black
*  HISP  1 if hispanic
*  MARR  1 if married
*  RE74  Real annual earnings in 1974  (pre-treatment)
*  RE75  Real annual earnings in 1974  (pre-treatment)
*  RE78  Real annual earnings in 1974  (post-treatment)
*  U74   1 if unemployed in 1974
*  U75   1 if unemployed in 1974

* NOTE: U74 and U75 are miscoded in these data and also in the 
*       summary statistics table of DW02
*       See below for correction to data

********** READ DATA AND TRANSFORMATIONS **********

infile TREAT AGE EDUC BLACK HISP MARR RE74 RE75 RE78 U74 U75 /*
  */ using nswpsid.da1

* The original data reversed U74 and U75
* Should be U74=1 if R74=0 and U74=0 if R74>0 anmd similar for U75
* This effects results with propensity score though not eariler results

* Wrong U74 and U75
sum U74 U75

* Correct the original data
drop U74 U75
gen U74 = cond(RE74 == 0, 1, 0)
gen U75 = cond(RE75 == 0, 1, 0) 

* Correct U74 and U75
sum U74 U75

* Create regressors used as additional controls in regressions below
gen AGESQ = AGE*AGE
gen EDUCSQ = EDUC*EDUC
* DW99 do not define NODEGREE but following gives Table 1 means
gen NODEGREE = 0
replace NODEGREE = 1 if EDUC < 12
gen RE74SQ = RE74*RE74
gen RE75SQ = RE75*RE75
gen U74BLACK = U74*BLACK
gen U74HISP = U74*HISP

sum AGE EDUC NODEGREE BLACK HISP MARR U74 U75 RE74 RE75 RE78 TREAT /*
   */ AGESQ EDUCSQ RE74SQ RE75SQ U74BLACK U74HISP

* Reproduce DW99 Table 1: RE74subset Treated and PSID-1 rows
* Same as CT Table 25.3 page 890
* except for changes to U74, U75 and U74BLACK
bysort TREAT: sum AGE EDUC NODEGREE BLACK HISP MARR U74 U75 RE74 RE75 RE78 TREAT /*
   */ AGESQ EDUCSQ RE74SQ RE75SQ U74BLACK

save nswpsid, replace

********** ANALYSIS: (1) CALCULATE EFFECT OF TRAINING (Table 25.4, p.891) ********** 

***** (1A) TREATMENT-CONTROL COMPARISON USING POST_TREATMENT EARNINGS 
*****      [Difference in means]

* DW99 Table 5 column 1 and Table 3 column 1
regress RE78 T

* CT Table 25.4 p.891 first row uses heteroskedastic-robust standard errors
regress RE78 TREAT, robust
estimates store treatcontrol

***** (1B) CONTROL FUNCTION ESTIMATOR Additionally Include pre-treatment controls 

* DW99 Table 5 column 2 using regressors in footnote a 
* Same as DW99 Table 2 column 14 
regress RE78 TREAT AGE AGESQ EDUC NODEGREE BLACK HISP RE74 RE75

* CT Table 25.4 p.891 second row uses heteroskedastic-robust standard errors
regress RE78 TREAT AGE AGESQ EDUC NODEGREE BLACK HISP RE74 RE75, robust
estimates store controlfunction

* Variation that lets OLS coefficients differ across treatment and controls 
* Interaction of regressors with T
gen TAGE = TREAT*AGE
gen TAGESQ = TREAT*AGESQ
gen TEDUC = TREAT*EDUC
gen TNODEGREE = TREAT*NODEGREE
gen TBLACK = TREAT*BLACK
gen THISP = TREAT*HISP
gen TRE74 = TREAT*RE74
gen TRE75 = TREAT*RE75
regress RE78 TREAT AGE AGESQ EDUC NODEGREE BLACK HISP RE74 RE75 /*
   */TAGE TAGESQ TEDUC TNODEGREE TBLACK THISP TRE74 TRE75

***** (1D) DIFFERENCE-IN-DIFFERENCES

* Need to stack two separate years of data RE75 and RE78
* into a panel of two years on RE
gen id = _n
label variable id "id"
gen EARNS1 = RE75
gen EARNS2 = RE78
reshape long EARNS, i(id) j(year)
gen dyear2 = 0
replace dyear2 = 1 if year==2
gen Tdyear2 = TREAT*dyear2
regress EARNS Tdyear2 TREAT dyear2

* CT Table 25.4 p.891 fourth row usea heteroskedastic-robust standard errors
regress EARNS Tdyear2 TREAT dyear2, robust
estimates store diffindiff

* Adding pretreatment controls makes no differnce as timne-invariant
regress EARNS Tdyear2 TREAT dyear2 AGE AGESQ EDUC NODEGREE BLACK HISP

***** (1C) BEFORE-AFTER COMPARISON 

* Regression for treated only 
regress EARNS Tdyear2 if TREAT==1

* CT Table 25.4 p.891 third row uses heteroskedastic-robust standard errors
regress EARNS Tdyear2 if TREAT==1, robust
estimates store beforeafter

***** DISPLAY RESULTS FOR FIRST FOUR ROWSM OF Table 25.4, p.891

estimates table treatcontrol controlfunction beforeafter diffindiff, /*
   */ b(%10.0f) se(%10.0f) stats(N) 

********** ANALYSIS: (2) PROPENSITY SCORE USING STRATA (Table 25.4, p.891) ********** 

use nswpsid, clear

***** (2A) COMPUTE PROPENSITY SCORE 

* Calculate propensity score using regressors in DW99 Table 3 footnote e
logit TREAT AGE AGESQ EDUC EDUCSQ MARR NODEGREE BLACK HISP RE74 RE75 RE74SQ RE75SQ U74BLACK
* Note that Table 25.6 footnote b is wrong in stating RE74*RE75 is regressor
predict PSCORE

***** (2B) PLOT PROPENSITY SCORE BY TREATMENT STATUS TO SEE OVERLAP

* Observations with no overlap in propensity score across treatment status are dropped

sum PSCORE if TREAT==1
scalar PTMIN = r(min)
scalar PTMAX = r(max)
sum PSCORE if TREAT==0
scalar PCMIN = r(min)
scalar PCMAX = r(max)
drop if PSCORE < PTMIN
drop if PSCORE < PCMIN
drop if PSCORE > PTMAX
drop if PSCORE > PCMAX
* Following gives number of observations left
sum PSCORE

* This differs from CT text page 893 as now U74 and U75 are corrected
* Instead of losing  1423 controls and 8 treated leaving 1244
* now          lose  1344 controls and 6 treated leaving 1325
* versus DW Figure 1 1333 controls are dropped leaving 1342
* and Dw Table 3 column 6 says that there are 1255 left

***** (2C) CREATE FIGURE 25.3 ON PAGE 892

* This will differ a little from figure in text due to U74 and U75 corrected

label define tstatus 0 Comparison_sample 1 Treated_sample
label values TREAT tstatus
label variable TREAT "Treatment Status"
graph twoway (scatter RE78 PSCORE if RE78 < 20000, msize(small)) /*
  */ (lowess RE78 PSCORE, bwidth(0.5) clpattern(solid)), /*
  */ by(TREAT, title("Post-treatment Earnings against Propensity Score", margin(b=3) size(vlarge))) /*
  */ subtitle(, bfcolor(none)) /* 
  */ scale (1.2) plotregion(style(none)) /*
  */ xtitle("     Propensity Score                             Propensity Score", size(medlarge)) xscale(titlegap(*5)) /*
  */ ytitle("Real Earnings 1978", size(medlarge)) yscale(titlegap(*5)) /*
  */ legend(pos(12) ring(0) col(2)) /*
  */ legend( label(1 "Original data") label(2 "Nonparametric regression")) 
graph export ch25treatment.wmf, replace

***** (2D) ADJUSTED DIFFERENCE  Use PSCORE to summarize pre-treatment controls 

* A simple method regressors RE78 on a quadratic on PSCORE and on TREAT
* And measures the treatment effect as coefficient of TREATED

gen PSCORESQ = PSCORE*PSCORE
regress RE78 TREAT PSCORE PSCORESQ

* This yields coefficient of 301 with nonrobust se of 1388
* which is close to DW 99 Table 3 column 3 
*             coefficient of 294 with nonrobust se of 1389

***** (2E) CREATE STRATA 

* DW are not clear on how formed. 
* NBER Working Paper W6829 appendix suggests that form five cells 
* according to range of PSCORE (where nonoverlapping PSCOREs already dropped)

* Here we instead create ten strata 
* for PSCORE <0.1, 0.1-0.2, ...., 0.8-0.9 and > 0.9
global cut1 = 0.1
global cut2 = 0.2
global cut3 = 0.3
global cut4 = 0.4
global cut5 = 0.5
global cut6 = 0.6
global cut7 = 0.7
global cut8 = 0.8
global cut9 = 0.9
gen STRATA = 1
replace STRATA = 2 if PSCORE > $cut1 & PSCORE <= $cut2
replace STRATA = 3 if PSCORE > $cut2 & PSCORE <= $cut3
replace STRATA = 4 if PSCORE > $cut3 & PSCORE <= $cut4
replace STRATA = 5 if PSCORE > $cut4 & PSCORE <= $cut5
replace STRATA = 6 if PSCORE > $cut5 & PSCORE <= $cut6
replace STRATA = 7 if PSCORE > $cut6 & PSCORE <= $cut7
replace STRATA = 8 if PSCORE > $cut7 & PSCORE <= $cut8
replace STRATA = 9 if PSCORE > $cut8 & PSCORE <= $cut9
replace STRATA = 10 if PSCORE > $cut9

tab STRATA T

***** (2F) Test for similar regressor means for treated and nontreated within each Strata

* Compare means within Strata across treatment status
tab STRATA TREAT, sum(AGE) nostand nofreq
tab STRATA TREAT, sum(EDUC) nostand nofreq
tab STRATA TREAT, sum(MARR) nostand nofreq
tab STRATA TREAT, sum(NODEGREE) nostand nofreq
tab STRATA TREAT, sum(BLACK) nostand nofreq
tab STRATA TREAT, sum(HISP) nostand nofreq
tab STRATA TREAT, sum(RE74) nostand nofreq
tab STRATA TREAT, sum(RE75) nostand nofreq
tab STRATA TREAT, sum(U74BLACK) nostand nofreq

* Formal test of difference in means within strata across treatment status
* Example is for education
* bysort STRATA: oneway EDUC T

***** (2G) Calculate weighted average of within strata mean difference in outcome

#delimit ;
global sum = 0 ;       * Sums the estimate of interest over strata ;
global sumwgt = 0 ;    /* Sums the number of treated obs over strata */ 
global count = 0 ;     /* This gives the number of Strata used       */   
global numcut = 10;

* Possibly include extra regressors. 
* Not clear which ones, so same as DW99 Table 3 footnote a for column 2
global XLIST AGE AGESQ EDUC NODEGREE BLACK HISP RE74 RE75;

forvalues i = 1/$numcut { ;
   global addon = 0 ;  /* Within strata estiamte of interest  */
   global tobs = 0 ;   /* Within strata number of treated obs */
   capture { ;
	quiet regress RE78 TREAT $XLIST if STRATA == `i' ;
	global addon = _b[TREAT] ;
	quiet summarize TREAT if TREAT==1 & STRATA==`i' ;
	global tobs = _result(1) ;  * # of treatment observations ;
    } ;
   di "`i' estimate = $addon        Top cut = ${cut`i'}    #treat obs = $tobs" ;
   if $addon ~= 0 { ; 
	global sum = $sum + $addon * $tobs ;
	global sumwgt = $sumwgt + $tobs ;
	global count = $count + 1 ;
   } ;
} ;

#delimit cr ; 

***** DISPLAY RESULT: "Propensity Score" estimate in last row Table 25.4

* Weighted estimate
di $sum / $sumwgt "      Count = " $count 

* This differs from value 995 given in text due to 
* previously mentioned correction of U74 and U75. 
* Now get 1562 with se not estimated
* compared to DW99 estimates Table 3 column 4 1608 and column 5 1494

********** CLOSE OUTPUT **********
log close
exit
clear

