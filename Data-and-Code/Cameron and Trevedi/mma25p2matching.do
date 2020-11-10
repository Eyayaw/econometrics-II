* MMA25P2MATCHING.DO  May 2005 for Stata version 8

log using mma25p2matching.txt, text replace

********** OVERVIEW OF MMA25P2MATCHING.DO **********

* STATA Program 
* copyright C 2005 by A. Colin Cameron and Pravin K. Trivedi 
* used for "Microeconometrics: Methods and Applications" 
* by A. Colin Cameron and Pravin K. Trivedi (2005)
* Cambridge University Press 

* Chapter 25.8.5 pages 893-6 Tables 25.5-25.7
* Evaluating treatment effect of training on Earnings
* using Dehejia-Wahba data (originally Lalonde data)

* (1) For DW 2002 specification of the logit model for propensity score
*     calculate treatment effect by matching methods (Tables 25.5-6)
*     (  ) give distribution of propensity score (Table 25.5)
*     (1A) nearest neighbor matching
*     (1B) radius matching r = 0.001
*     (1C) radius matching r = 0.001
*     (1D) radius matching r = 0.001
*     (1E) stratification
*     (1F) kernel matching
* (2) For DW 1999 specification of the logit model for propensity score
*     calculate treatment effect by matching methods (Table 25.6)

* The program MMA25P1TREATMENT.DO provides simpler nonmatching methods
* for the same data. 

* To run this program you need data file 
*     nswpsid.da1

* To run this program you need the Stata add-ons 
* pscore.ado, atts.ado, attr.ado, attnd.ado, attnw.ado 
* due to  Sascha O. Becker and Andrea Ichino (2002)
* "Estimation of average treatment effects based on propensity scores", 
* The Stata Journal, Vol.2, No.4, pp. 358-377.

* This program uses version 2.02 May 13 2005 for Stata version 8
* downloadable from http://www.iue.it/Personal/Ichino/#pscore
* We earlier used version 1.29 October 8 2002 for Stata version 7
* downloadable from http://www.iue.it/Personal/Ichino/#pscore
* and obtained the same results

* To speed up the program reduce breps: the number of bootstrap 
* replications used to obtain bootstrap standard errors
* Bootstrap se's will differ from text as here seed is set to 10101

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

****** propensity score for nsw-psid composite sample*************
****** output for MMA Tables 25.6 & 25.7 ***********************

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

bysort TREAT: sum AGE EDUC NODEGREE BLACK HISP MARR U74 U75 RE74 RE75 RE78 TREAT /*
   */ AGESQ EDUCSQ RE74SQ RE75SQ U74BLACK U74HISP

*** NOTE: The benchmark estimate obtained from NSW experiment is 
***       $1,794 = Average(RE_78 for NSW treated) - Average (RE_78 for NSW comtrols)
***       See MMA25P3EXTRA.DO

********** (1) ANALYSIS for DW02 SPECIFICATION OF THE PROPENSITY SCORE **********

* Following defines number of bootstrap replications
* Table 25.6 used 200 (or 100 in some places)
global breps 200

* From DW02 Table 3 footnote a the propensity score uses the following regressors
global XDW02 AGE AGESQ EDUC EDUCSQ MARR NODEGREE BLACK HISP RE74 RE75 RE74SQ U74 U75 U74HISP

**** Table 25.5 p.894 summarizes propensity score 
**** using just those observations with common support

pscore TREAT $XDW02, pscore(myscore) comsup blockid(myblock) numblo(5) level(0.005) logit

**** For completeness do same with common support option NOT selected 

drop myscore myblock
pscore TREAT $XDW02, pscore(myscore) blockid(myblock) numblo(5) level(0.005) logit

**** All of the following use common support

****************************************************************************
**** Note: The results in the first half of Table 25.6 
****       erroneously added RE75SQ as a regressor.
****       This does not effect Table 25.5 (done correctly) or 
****       stratification estimates (which used myscore from correct model).
****       But it does effect NN, radius and kernel estimates.
****       To enable comparison with the text we do analysis here 
****       both with and without RE75SQ.
****       Even dropping RE75SQ the results continue to differ from DW02.
****                            Text      Corrected
****                         Table 25.6   Table 25.6    DW 2002
****       NN                  2385         1286         1202
****       Radius = 0.001     -7815        -7808         1187
****       Radius = 0.0001    -9333        -6401         1191
****       Radius = 0.00001   -2200        -1135         1198
****       Stratification      1497         1497          
****       Kernel              1309         1342
****************************************************************************

**** Row 1 Table 25.6: Nearest neighbor matching (random version)
set seed 10101
attnd RE78 TREAT $XDW02 RE75SQ, comsup boot reps($breps) dots logit
set seed 10101
attnd RE78 TREAT $XDW02, comsup boot reps($breps) dots logit

**** Row 2 Table 25.6: Radius matching for Radius=0.001
set seed 10101
attr RE78 TREAT $XDW02 RE75SQ, comsup boot reps($breps) dots logit radius(0.001)
set seed 10101
attr RE78 TREAT $XDW02, comsup boot reps($breps) dots logit radius(0.001)

**** Row 3 Table 25.6: Radius matching for Radius=0.0001
set seed 10101
attr RE78 TREAT $XDW02 RE75SQ, comsup boot reps($breps) dots logit radius(0.0001)
set seed 10101
attr RE78 TREAT $XDW02, comsup boot reps($breps) dots logit radius(0.0001)

**** Row 4 Table 25.6: Radius matching for Radius=0.00001
set seed 10101
attr RE78 TREAT $XDW02 RE75SQ, comsup boot reps($breps) dots logit radius(0.00001)
set seed 10101
attr RE78 TREAT $XDW02, comsup boot reps($breps) dots logit radius(0.00001)

**** Row 5 Table 25.6: Stratification Matching
set seed 10101
atts RE78 TREAT, pscore(myscore) blockid(myblock) comsup boot reps($breps) dots

**** Row 6 Table 25.6: Kernel Matching
set seed 10101
attk RE78 TREAT $XDW02 RE75SQ, comsup boot reps($breps) dots logit
set seed 10101
attk RE78 TREAT $XDW02, comsup boot reps($breps) dots logit

********** (2) ANALYSIS for DW99 SPECIFICATION OF THE PROPENSITY SCORE **********

* From DW99 Table 3 footnote e the propensity score uses the following regressors
global XDW99 AGE AGESQ EDUC EDUCSQ MARR NODEGREE BLACK HISP RE74 RE75 RE74SQ RE75SQ U74BLACK

* Note that CT Table 25.6 footnote b erroneously lists RE74*RE75 as regressor
* but this program (correctly) did not include RE74*RE75

**** Propensity score with just those observations with common support

drop myscore myblock
pscore TREAT $XDW99, pscore(myscore) comsup blockid(myblock) numblo($breps) level(0.005) logit

**** For completeness do same with common support option NOT selected 

drop myscore myblock
pscore TREAT $XDW99, pscore(myscore) blockid(myblock) numblo($breps) level(0.005) logit

**** All of the following use common support

**** Row 7 Table 25.6: Nearest neighbor matching (random version)
set seed 10101
attnd RE78 TREAT $XDW99, comsup boot reps($breps) dots logit

**** Row 8 Table 25.6: Radius matching for Radius=0.001
set seed 10101
attr RE78 TREAT $XDW99, comsup boot reps($breps) dots logit radius(0.001)

**** Row 9 Table 25.6: Radius matching for Radius=0.0001
set seed 10101
attr RE78 TREAT $XDW99, comsup boot reps($breps) dots logit radius(0.0001)

**** Row 10 Table 25.6: Radius matching for Radius=0.00001
set seed 10101
attr RE78 TREAT $XDW99, comsup boot reps($breps) dots logit radius(0.00001)

**** Row 11 Table 25.6: Stratification Matching
set seed 10101
atts RE78 TREAT, pscore(myscore) blockid(myblock) comsup boot reps($breps) dots

**** Row 12 Table 25.6: Kernel Matching
* pscore TREAT $XDW99, pscore(myscore) comsup blockid(myblock) numblo($breps) level(0.005) logit
set seed 10101
attk RE78 TREAT $XDW99, comsup boot reps($breps) dots logit

********** CLOSE OUTPUT **********
log close
exit
clear


