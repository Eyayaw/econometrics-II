* MMA25P3EXTRA.DO  May 2005 for Stata version 8.0

log using mma25p3extra.txt, text replace

********** OVERVIEW OF MMA25P3EXTRA.DO **********

* STATA Program 
* copyright C 2005 by A. Colin Cameron and Pravin K. Trivedi 
* used for "Microeconometrics: Methods and Applications" 
* by A. Colin Cameron and Pravin K. Trivedi (2005)
* Cambridge University Press 

* Chapter 25.8 pages 889-893 
* Evaluating treatment effect of training on Earnings
* This program provides additional analysis and data not in the book
* (1) Compare NSW experiment treated to NSW experiment controls
* (2) Compare NSW experiment treated to CPS "controls"
*     [Same as text except "controls" are from CPS not PSID]

* The program is based on 
*      MMA25P2MATCHING.DO  propensity score matching

* To run this program you need STATA data files
*    nswre74_treated.dta     NSW Treated sample
*    nswre74_control.dta     NSW Control sample (not analyzed earlier)
*    propensity_cps.dta      CPS Control sample (rather than PSID)

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

*    nswre74_treated.dta   N=185    NSW Treated sample only
*    nswre74_control.dta   N=260    NSW Control sample only
*    propensity_cps.dta    N=16177  NSW Treated + CPS Control sample (Full CPS or CPS-1)

********** (1) ANALYSIS: NSW TREATED VERSUS NSW CONTROLS **********

* Read in NSW treated and control and combine
use nswre74_treated.dta, clear
append using nswre74_control.dta

** Summarize these data
sum
bysort treat: sum

* Write data to a text (ascii) file so can use with programs other than Stata  
outfile treat age edu black hisp married nodegree re74 re75 re78 u74 u75 /*
   */using nswre74_all.asc, replace

**  Calculate the benchmark Treatment Effect
**  Same as DW02 Tables 2 and 3 NSW row second last column
**  and is the number given in CT page 894 second last line

regress re78 treat

********** (2) ANALYSIS: NSW TREATED VERSUS CPS CONTROLS **********

* This data set has NSW treated and full CPS controls
use propensity_cps.dta, clear

* Variables u74, u75 were evaluated wrongly in the original file
* So make the following correction 
drop u74 u75
gen u74=0
replace u74=1 if re74==0
gen u75=0
replace u75=1 if re75==0
gen age2=age*age
gen age3=age2*age
gen edu2=edu*edu
gen edure74=edu*re74
* Not sure whether this is needed
* Does DW99 use edu*re74*age3 or separately edu*re74 and age3 ?
gen edre74age3=edu*re74*age3

** Summarize these data
sum
bysort treat: sum

* Write data to a text (ascii) file so can use with programs other than Stata  
* This has data as original except for recode of u74 and u75
outfile treat age edu black hisp married nodegree re74 re75 re78 u74 u75 /*
   */ using propensity_cps.asc, replace

** Number of replications to use in the bootstrap
** Ideally at least 400
global breps 200

*** (2A) CPS propensity score model from DW02 Table 2 footnote A 

global CPSDW02 age age2 age3 edu edu2 married nodegree black hisp re74 re75 u74 u75 edure74

* With common support option
pscore treat $CPSDW02, pscore(myscore) blockid(myblock) comsup numblo(5) level(0.005) logit

* Without common support option
drop myscore myblock
pscore treat $CPSDW02, pscore(myscore) blockid(myblock) numblo(5) level(0.005) logit

* Nearest neighbor matching (random version)
attnd re78 treat $CPSDW02, comsup boot reps($breps) dots logit

* Radius matching: Radius=0.0001
attr re78 treat $CPSDW02, comsup boot reps($breps) dots logit radius(0.0001)

* Kernel Matching
attk re78 treat $CPSDW02, comsup boot reps($breps) dots logit

* Stratification Matching
atts re78 treat, pscore(myscore) blockid(myblock) comsup boot reps($breps) dots

*** (2B) CPS propensity score model from DW99 Table 2 footnote A

global CPSDW99 age age2 edu edu2 nodegree married black hisp re74 re75 u74 u75 edure74 age3

* With common support option
drop myscore myblock
pscore treat $CPSDW99, pscore(myscore) blockid(myblock) comsup numblo(5) level(0.005) logit

* Without common support option
drop myscore myblock
pscore treat $CPSDW99, pscore(myscore) blockid(myblock) numblo(5) level(0.005) logit

* Nearest neighbor matching (random version)
attnd re78 treat $CPSDW99, comsup boot reps($breps) dots logit

* Radius matching: Radius=0.0001
attr re78 treat $CPSDW99, comsup boot reps($breps) dots logit radius(0.0001)

* Kernel Matching
attk re78 treat $CPSDW99, comsup boot reps($breps) dots logit

* Stratification Matching
atts re78 treat, pscore(myscore) blockid(myblock) comsup boot reps($breps) dots

*** (2C) CPS propensity score model from Becker-Ichino, 2002 (BI02)

gen re742 = re74*re74
gen re752 = re75*re75
gen blacku74 = black*u74
global CPSBI02 age age2 edu edu2 married black hisp re74 re75 re742 re752 blacku74

* With common support option
drop myscore myblock
pscore treat $CPSBI02, pscore(myscore) blockid(myblock) comsup numblo(5) level(0.005) logit

* Without common support option
drop myscore myblock
pscore treat $CPSBI02, pscore(myscore) blockid(myblock) numblo(5) level(0.005) logit

* Nearest neighbor matching (random version)
attnd re78 treat $CPSBI02, comsup boot reps($breps) dots logit

* Radius matching: Radius=0.0001
attr re78 treat $CPSBI02, comsup boot reps($breps) dots logit radius(0.0001)

* Kernel Matching
attk re78 treat $CPSBI02, comsup boot reps($breps) dots logit

* Stratification Matching
atts re78 treat, pscore(myscore) blockid(myblock) comsup boot reps($breps) dots

********** CLOSE OUTPUT **********
log close
clear
exit


