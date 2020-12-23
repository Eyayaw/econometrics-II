* MMA23P1PANNONLIN.DO  March 2005 for Stata version 8.0

log using mma23p1pannonlin.txt, text replace

********** OVERVIEW OF MMA23P1PANNONLIN.DO **********

* STATA Program 
* copyright C 2005 by A. Colin Cameron and Pravin K. Trivedi 
* used for "Microeconometrics: Methods and Applications" 
* by A. Colin Cameron and Pravin K. Trivedi (2005)
* Cambridge University Press

* Chapter 23.3 pages 792-5
* Example of nonlinear model (multiplicative effects)

* This program derives Table 23.1 and Figure 23.1.
* It performs nonlinear panel analysis for multiplicative effects model 
*   y_it = a_i*exp(x_it'b) = exp(c_i+x_it'b)
* and parametric count data models

*  (1) Linear (xtreg) for log(PAT) with adjustment for PAT=0
*      Output include Figure 23.1
*  (2) Poisson (xtpoisson) fixed and random effects
*  (3) GEE (xtgee) which includes pooled NLS

* The Poisson individual effects model is 
*   y_it ~ Poisson(x_it'b + a_i)
* The standard errors assume this model correctly specified
* i.e. Variance = mean given x+it and a_i

* FOr "panel robust se's see section 23.2.6 pages 788-791
* To obtain more panel robust standard errors this program panel bootstraps
* Note that the panel se entries of 0.033 under GEE, Poisson-RE and Poisson-FE
* are not panel robust to the extent that the bootstrap se's are panel robust
* and in fact are the usual se's in the case of Poisson-RE and Poisson-FE
* Unlike ch.21 here "panel se" means "defaul panel se" and not "panel-robust se".

* To speed up program reduce nreps, the number of bootstrap replications

* To run this program you need data file
*    patr7079.asc  

********** SETUP **********

set more off
version 8.0
set scheme s1mono  /* Graphics scheme */

********** DATA DESCRIPTION **********

* There are ten years of data but only five years 1975-79 are used in estimation

*  The original data is from 
*  Bronwyn Hall, Zvi Griliches, and Jerry Hausman (1986), 
* "Patents and R&D: Is There a Lag?", 
*  International Economic Review, 27, 265-283.

* File patr7079.dat has data on 346 firms
* There are 4 lines per firm, with 25 variables
*   Time-invariant:  CUSIP,ARDSSIC,SCISECT,LOGK,SUMPAT,
*   Time-varying X:  LOGR70,LOGR71,LOGR72, ....., LOGR77,LOGR78,LOGR79
*   Time-varying Y:  PAT70,PAT71,PAT72, ....., PAT77,PAT78,PAT79 
* in the format:
*   I7,I3,I2,5F12.6/6F12.6/6F12.6/5F12.6/ 
* where
* CUSIP    Compustat's identifying number for the firm (Committee on
*          Uniform Security Identification Procedures number).
* ARDSIC   A two-digit code for the applied R&D industrial classification
*          (roughly that in Bound, Cummins, Griliches, Hall, and Jaffe, in
*          the Griliches R&D, Patents, and Productivity volume).
* SCISECT  Dummy equal to one for firms in the scientific sector.
* LOGK     The logarithm of the book value of capital in 1972.
* SUMPAT   The sum of patents applied for between 1972-1979.
* LOGR70-  The logarithm of R&D spending during the year (in 1972 dollars).
*  LOGR79
* PAT70-   The number of patents applied for during the year that were
*  PAT79  eventually granted.

********** READ DATA **********

* The data are in ascii file patr7079.asc
* There are 346 observations on 25 variables with four lines per obs
* The data are fixed format with 
*   line 1  variables  1-8   I7,I3,I2,5F12.6 
*   line 2  variables  9-14  6F12.6
*   line 3  variables 15-20  6F12.6
*   line 4  variables 20-25  6F12.6

* Read in using Infile: FREE FORMAT WITHOUT DICTIONARY
* As there is space between each observation data is also space-delimited 
* free format and then there is no need for a dictionary file
* The following command spans more that one line so use /* and */
infile CUSIP ARDSSIC SCISECT LOGK SUMPAT LOGR70 LOGR71 LOGR72 LOGR73   /*
   */  LOGR74 LOGR75 LOGR76 LOGR77 LOGR78 LOGR79 PAT70 PAT71 PAT72     /*
   */  PAT73 PAT74 PAT75 PAT76 PAT77 PAT78 PAT79 using patr7079.asc

********** DATA TRANSFORMATIONS **********

* Use observation number as an identifier, not just CUSIP
gen id = _n
label variable id "id"
* The following lists the variables in data set and summarizes data
describe
summarize

******** CHANGE ORGANIZATION OF DATA USING RESHAPE AND MORE TRANSFORMATIONS

reshape long PAT LOGR, i(id) j(year)
describe
summarize

* Create new variable log(patents) with adjustment for patents = 0
gen NEWPAT = PAT
replace NEWPAT = 0.5 if NEWPAT==0.
gen LPAT = ln(NEWPAT)
label variable LPAT "Ln(Patents)"
label variable PAT "Patents"
* Dummy variable for logit analysis
gen DPAT = 0
replace DPAT = 1 if PAT>0
label variable DPAT "Patent Indicator"
* R and D
gen RANDD = exp(LOGR)
label variable LOGR "Ln(R&D)"
label variable RANDD "R&D"
* Lagged log R and D
tsset id year
gen LOGRL1 = L1.LOGR
gen LOGRL2 = L2.LOGR
gen LOGRL3 = L3.LOGR
gen LOGRL4 = L4.LOGR
gen LOGRL5 = L5.LOGR
label variable LOGRL1 "Ln(R&D) lagged once"
label variable LOGRL2 "Ln(R&D) lagged twice"
label variable LOGRL3 "Ln(R&D) lagged three times"
label variable LOGRL4 "Ln(R&D) lagged four times"
label variable LOGRL5 "Ln(R&D) lagged five times"
* Year dummies
gen dyear2 = 0
replace dyear2 = 1 if year==76
gen dyear3 = 0
replace dyear3 = 1 if year==77
gen dyear4 = 0
replace dyear4 = 1 if year==78
gen dyear5 = 0
replace dyear5 = 1 if year==79
 
* Check data and Save data as Stata data set
describe
summarize
drop NEWPAT
save patr7079, replace
summarize
xtsum, i(id)

********** DEFINE GLOBALS INCLUDING REGRESSOR LIST **********

* Number of reps for the bootstrap
* Table 23.1 used 500
global nreps 500

* The regressions below are of patents on LOGR ??? on ???
* Additional regressors to be included below are defined in xextra
* Here no additional regressors
global xextra

********** (1) LINEAR PANEL RANDOM AND FIXED EFFECTS FOR LOG(PAT) **********

* This adhoc method uses as dependent variable 
*     LPAT = ln(PAT) if PAT > 0
*          = ln(0.5) if PAT = 0
* which is analyzed using chapter 21 methods

* Note that in the first xt command need to give  , i(id)
* to indicate that the ith observation is for the ith id
* Time invariant regressors LOGK SCISECT are not included

use patr7079, clear
drop if year<75

* Overall plot of data 
* The graphs below use new Stata 8 graphics
* Change graphics scheme from default s2color to s1mono for printing
set scheme s1mono

* Figure 21.1 page 792 [with axis labels corrected - book is wrong]
graph twoway (scatter LPAT LOGR, msize(vsmall)) (lowess LPAT LOGR) (lfit LPAT LOGR), /* 
  */ scale (1.2) plotregion(style(none)) /*
  */ title("Pooled (Overall) Regression") /*
  */ xtitle("Log R&D Spending", size(medlarge)) xscale(titlegap(*5)) /*
  */ ytitle("Log Patents", size(medlarge)) yscale(titlegap(*5)) /*
  */ legend(pos(4) ring(0) col(1)) legend(size(small)) /*
  */ legend( label(1 "Original data") label(2 "Nonparametric fit") label(3 "Linear fit"))
graph export ch23fig1.wmf, replace

* OLS
regress LPAT LOGR $xextra, cluster(id)
estimates store linolspan

* Fixed effects
xtreg LPAT LOGR $xextra, fe i(id)
estimates store linfe

* Random effects
xtreg LPAT LOGR $xextra, re i(id)
estimates store linre


********** (2) POISSON RANDOM AND FIXED EFFECTS (Table 32.1 p.794 ) **********

use patr7079, clear
drop if year<75

* Poisson Cross-section with Poisson standard errors
* Table 23.1 Poisson column

poisson PAT LOGR $xextra
estimates store poisiid

* Poisson Cross-section with heteroskedastic robust standard errors
poisson PAT LOGR $xextra, robust
estimates store poishet

* Poisson Cross-section with panel robust standard errors
poisson PAT LOGR $xextra, cluster(id)
estimates store poispan

* Poisson panel fixed effects
* Table 23.1 p.794 Poisson-FE column

* Poisson fixed effects
xtpoisson PAT LOGR $xextra, fe i(id)  
estimates store poisfe

/* 
* Alternative way is to put in dummy variables
set matsize 400
xi: poisson PAT LOGR $xextra i.id
*/

* Poisson panel random effects
* Table 23.1 p.794 Poisson-RE column

* Poisson random effects
xtpoisson PAT LOGR $xextra, re i(id)  
estimates store poisre

* Poisson random effects with normal error
xtpoisson PAT LOGR $xextra, re i(id) normal
estimates store poisrenormal

* Poisson random effects population averaged
xtpoisson PAT LOGR $xextra, pa i(id)  
estimates store poispa

* Poisson random effects population averaged with robust se
xtpoisson PAT LOGR $xextra, robust pa i(id)  
estimates store poispapan

********** (3) POISSON GEE (GENERALIZED ESTIMATING EQUATIONS **********

* Xtgee should reproduce Poisson random effects population averaged
xtgee PAT LOGR $xextra, corr(exchangeable) family(poisson) link(log) i(id)  
estimates store poisgee

* Xtgee should reproduce Poisson random effects population averaged with robust se
xtgee PAT LOGR $xextra, corr(exchangeable) family(poisson) link(log) i(id) robust 
estimates store poisgeepan

* Xtgee should give NLS of exponential mean with iid standard errors
xtgee PAT LOGR $xextra, corr(independent) family(gaussian) link(log) i(id)  
estimates store nls

* Xtgee should give NLS of exponential mean with robust standard errors
xtgee PAT LOGR $xextra, corr(independent) family(gaussian) link(log) i(id) robust
estimates store nlspan

********** (4) PANEL ROBUST STANDARD ERRORS BY BOOTSTRAP **********

* For discussion of panel robust standard errors 
* see text Section 23.2.6 page 788-9 (nonlinear panel) 
* and text Section 21.2.3 page 705-8 (linear panel) 

* Pooled Poisson panel robust bootstrap standard errors
set seed 10001
bootstrap "poisson PAT LOGR $xextra" "_b[LOGR] _b[_cons]", cluster(id) reps($nreps) level(95)
matrix poisbootse = e(se)

* Poisson fixed effects panel bootstrap standard errors
set seed 10001
bootstrap "xtpoisson PAT LOGR $xextra, fe i(id)" "_b[LOGR]", cluster(id) reps($nreps) level(95)
matrix poisfebootse = e(se)

* Poisson random effects panel bootstrap standard errors
set seed 10001
bootstrap "xtpoisson PAT LOGR $xextra, re i(id)" "_b[LOGR] _b[_cons]", cluster(id) reps($nreps) level(95)
matrix poisrebootse = e(se)

* Poisson population averaged panel bootstrap standard errors
set seed 10001
bootstrap "xtpoisson PAT LOGR $xextra, pa i(id)" "_b[LOGR] _b[_cons]", cluster(id) reps($nreps) level(95)
matrix poispabootse = e(se)
set seed 10001

* Xtgee should give exponential mean (NLS) with iid errors with boostrap se's
bootstrap "xtgee PAT LOGR $xextra, corr(independent) family(gaussian) link(log) i(id)"  "_b[LOGR] _b[_cons]", cluster(id) reps($nreps) level(95)  

* Results fiven in same order as in Table 23.1 page 794
matrix nlsbootse = e(se)
matrix list poisbootse
matrix list poisfebootse
matrix list poisrebootse
matrix list poispabootse

********** DISPLAY RESULTS FOR (1)-(3) GIVEN IN TABLE 23.1 page 794 **********

* Standard error using iid errors and in some cases panel

estimates table linolspan linfe linre, t se /*
   */ stats(N ll r2 tss rss mss rmse df_r) b(%10.3f)
estimates table poisiid poishet poispan, t se /*
   */ stats(N ll r2 tss rss mss rmse df_r) b(%10.3f)
estimates table poisfe poisre poisrenormal poispa poispapan, t se /*
   */ stats(N ll r2 tss rss mss rmse df_r) b(%10.3f)
estimates table poisgee poisgeepan nls nlspan, t se /*
   */ stats(N ll r2 tss rss mss rmse df_r) b(%10.3f)

********** CLOSE OUTPUT
log close
* clear
* exit

