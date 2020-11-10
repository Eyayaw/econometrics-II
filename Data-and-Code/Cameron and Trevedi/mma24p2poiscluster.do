* MMA24P2POISCLUSTER.DO  May 2005 for Stata version 8.0

log using mma24p2poiscluster.txt, text replace

********** OVERVIEW OF MMA24P2POISCLUSTER.DO **********

* STATA Program 
* copyright C 2005 by A. Colin Cameron and Pravin K. Trivedi 
* used for "Microeconometrics: Methods and Applications" 
* by A. Colin Cameron and Pravin K. Trivedi (2005)
* Cambridge University Press 

* Chapter 24.7 pages 848-53 Table 24.6
* Cluster robust inference for Poisson cross-section application using
* Vietnam Living Standard Survey data

* (0) Descriptive Statistics (Table 24.3 second half)
* (1) Frequencies of data (Table 24.5)
* (2) Poisson regression with individual-level data (Table 24.6)

* The results differ in second significant digit from those in text
* despite same sample size. Not sure why.

* For Table 24.4 for clustered household data see MMA24P1OLSCLUSTER.DO

* The Poisson cluster effects model is 
*   y_it ~ Poiss0n(x_it'b + a_i)
* Default xtreg output assumes Poisson distribution - var = mean.
* This is usually too strong an assumption.
* Instead should get cluster-robust errors after xtpois
* See Section 21.2.3 pages 709-12 and section 23.26 pages 788-9
* Stata Version 8 does not do this. 
* Here we do a panel bootstrap - results not reported in the text

* To speed up programs reduce breps - the number of bootstrap reps
* This program takes a long time if bootstrap

* To run this program you need data set 
*     vietnam_ex2.dta

********** SETUP **********

set more off
version 8.0
set scheme s1mono   /* Used for graphs */

********** DATA DESCRIPTION **********

* The data comes from World Bank 1997 Vietnam Living Standards Survey 
* A subset was used in chapter 4.6.4.
* The larger sample here is described on pages 848-9

* The data are HOUSEHOLD data
* There are N=5006 individuals in 194 clusters (communes)

* The separate data set vietnam_ex1.dta has individual level data

********** READ IN INDIVIDUAL-LEVEL DATA and SUMMARIZE (Table 24.3) **********

use vietnam_ex2.dta, clear
desc
sum

rename COMPED98 EDUC
rename ILLDUM ILLNESS
rename INJDUM INJURY
rename HLTHINS INSURANCE
rename lnhhinc LNHHEXP
rename commune COMMUNE

* Following should give same descriptive statistics 
* as in bottom half (Household) in Table 24.3 p.850
* But there are is a difference for LNHHEXP plus here no data on MEDEXP
sum PHARVIS LNHHEXP AGE SEX MARRIED EDUC ILLNESS INJURY ILLDAYS ACTDAYS INSURANCE COMMUNE
sum LNHHEXP, detail

* Following gives Table 24.5 (page 852) frequencies
* These differ in some places from Table 24.5 - especially for number = 0
tabulate PHARVIS

* Histogram with kernel density estimate
hist PHARVIS, discrete kdensity

* Write data to a text (ascii) file so can use with programs other than Stata  
outfile PHARVIS LNHHEXP AGE SEX MARRIED EDUC ILLNESS INJURY ILLDAYS /*
   */ ACTDAYS INSURANCE COMMUNE using vietnam_ex2.asc, replace

********** ANALYSIS: CLUSTER ANALYSIS FOR POISSON MODEL [Table 24.6 p.851] *********

* Regressor list for the Poisson regressions
global XLISTPOISSON LNHHEXP INSURANCE SEX AGE MARRIED ILLDAYS ACTDAYS INJURY ILLNESS EDUC

* Poisson with usual standard errors (Table 24.6 columns 1-2) 
poisson PHARVIS $XLISTPOISSON
estimates store poisiid

* Poisson with heteroskedastic-robust standard errors (Table 24.6 column 3) 
poisson PHARVIS $XLISTPOISSON, robust
estimates store poishet

* Poisson with cluster-robust standard errors (Table 24.6 column 4) 
poisson PHARVIS $XLISTPOISSON, cluster(COMMUNE)
estimates store poisclust

* Random effects estimation (Table 24.6 columns 5-6) 
* This uses the xtpois command which first requires identifying the cluster
iis COMMUNE
xtpois PHARVIS $XLISTPOISSON, re
estimates store poisre

* Following shows that cluster option for xtpois in Stata version does nothing
xtpois PHARVIS $XLISTPOISSON, i(COMMUNE) re

* Note that can cluster bootstrap if desired to get more robust standard errors
* This is done at end of program

* Fixed effects estimation (FGLS) (Table 24.6 columns 7-8) 
xtpois PHARVIS $XLISTPOISSON, fe
estimates store poisfe

* Note that can cluster bootstrap if desired to get more robust standard errors
* This is done at end of program

********** DISPLAY TABLE 24.6 RESULTS page 852 **********
 
* The results here differ in the second significant digit from those in text 
* despite same sample size. Not sure why.

estimates table poisiid poishet poisclust, /*
   */ b(%10.3f) t(%10.2f) stats(r2 N)
estimates table poisre poisfe, /*
   */ b(%10.3f) t(%10.2f) stats(r2 N)

********** ADDITIONALLY DO CLUSTER BOOTSTRAPS **********

* These results not given in the text

* Output at website uses breps 500
global breps 50

* Note that can bootstrap if desired to get more robust standard errors
* The first reproduces pois , cluster(COMMUNE)
bootstrap "poisson PHARVIS $XLISTPOISSON" _b, cluster(COMMUNE) reps($breps) level(95)
* The t-statistic vector is e(b)./e(se) where ./ is elt. by elt. division
* But Stata Version 8 does not do ./ so instead need the following
matrix tpois = (vecdiag(diag(e(b))*syminv(diag(e(se)))))'
matrix list tpois, format(%10.2f)

* The next two reproduce xtpois , cluster(COMMUNE)
* but xtpois has no cluster option so instead cluster boostrap

* Fixed effects estimator
bootstrap "xtpois PHARVIS $XLISTPOISSON, fe" _b, cluster(COMMUNE) reps($breps) level(95)
matrix tpoisfe = (vecdiag(diag(e(b))*syminv(diag(e(se)))))'
matrix list tpoisfe, format(%10.2f)

* Random effects estimator
bootstrap "xtpois PHARVIS $XLISTPOISSON, re" _b, cluster(COMMUNE) reps($breps) level(95)
matrix tpoisre = (vecdiag(diag(e(b))*syminv(diag(e(se)))))'
matrix list tpoisre, format(%10.2f)

********** CLOSE OUTPUT **********
log close
clear 
exit
 
