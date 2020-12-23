* MMA24P1OLSCLUSTER.DO  May 2005 for Stata version 8.0

log using mma24p1olscluster.txt, text replace

********** OVERVIEW OF MMA24P1OLSCLUSTER.DO **********

* STATA Program 
* copyright C 2005 by A. Colin Cameron and Pravin K. Trivedi 
* used for "Microeconometrics: Methods and Applications" 
* by A. Colin Cameron and Pravin K. Trivedi (2005)
* Cambridge University Press 

* Chapter 24.7 pages 848-53 Table 24.4
* Cluster robust inference for OLS cross-section application using
* Vietnam Living Standard Survey data

* (0) Descriptive Statistics (Table 24.3 first half)
* (1) Linear regression (in logs) with household data (Table 24.4)

* For Tables 24.5-6 for clustered count data see MMA24P2POISCLUSTER.DO

* The cluster effects model is 
*   y_it = x_it'b + a_i + e_it
* Default xtreg output assumes e_it is iid.
* This is usually too strong an assumption.
* Instead should get cluster-robust errors after xtreg
* See Section 21.2.3 pages 709-12
* Stata Version 8 does not do this but Stata version 9 does. 
* Here we do a panel bootstrap - results not reported in the text

* To speed up programs reduce breps - the number of bootstrap reps

* To run this program you need data set 
*     vietnam_ex1.dta

********** SETUP **********

set more off
version 8.0
set scheme s1mono   /* Used for graphs */

********** DATA DESCRIPTION **********

* The data comes from World Bank 1997 Vietnam Living Standards Survey 
* A subset was used in chapter 4.6.4.
* The larger sample here is described on pages 848-9

* The data are HOUSEHOLD data
* There are N=5006 households in 194 clusters

* The separate data set vietnam_ex2.dta has household-level data

********** READ IN HOUSEHOLD DATA and SUMMARIZE (Table 24.3) **********

use vietnam_ex1.dta
desc
sum

rename sex SEX
rename age AGE
rename comped98 EDUC
rename farm FARM
rename hhsize HHSIZE
rename commune COMMUNE
rename lhhexp1 LNHHEXP
rename lhhex12m LNEXP12M
gen HHEXP = exp(LNHHEXP)

* Following should give same descriptive statistics 
* as in top half (Household) in Table 24.3 p.850
* But there are some differences plus here have FARM not URBAN
sum LNEXP12M AGE SEX HHSIZE FARM EDUC HHEXP LNHHEXP COMMUNE 

* Write data to a text (ascii) file so can use with programs other than Stata  
* Note that LNEXP12M has some missing values coded as .
outfile LNEXP12M AGE SEX HHSIZE FARM EDUC LNHHEXP COMMUNE /*
   */using vietnam_ex1.asc, replace

********** ANALYSIS: CLUSTER ANALYSIS FOR LINEAR MODEL [Table 24.4 p.851] **********

* Regressor list for the linear regressions
global XLISTLINEAR LNHHEXP AGE SEX HHSIZE FARM EDUC

* OLS with usual standard errors (Table 24.4 columns 1-2) 
regress LNEXP12M $XLISTLINEAR
estimates store olsiid

* OLS with heteroskedastic-robust standard errors (Table 24.4 column 3) 
regress LNEXP12M $XLISTLINEAR, robust
estimates store olshet

* OLS with cluster-robust standard errors (Table 24.4 column 4) 
regress LNEXP12M $XLISTLINEAR, cluster(COMMUNE)
estimates store olsclust

* Random effects estimation (FGLS) (Table 24.4 columns 5-6) 
* This uses the xtreg command which first requires identifying the cluster
iis COMMUNE
xtreg LNEXP12M $XLISTLINEAR, re
estimates store refgls

* Note that can cluster bootstrap if desired to get more robust standard errors
* This is done at end of program

* Fixed effects estimation (FGLS) (Table 24.4 columns 7-8) 
xtreg LNEXP12M $XLISTLINEAR, fe
estimates store fe

* Note that can cluster bootstrap if desired to get more robust standard errors
* This is done at end of program

* Random effects estimation by MLE assuming normality (Table 24.4 columns 5-6) 
* This uses the xtreg command which first requires identifying the cluster
iis COMMUNE
xtreg LNEXP12M $XLISTLINEAR, mle
estimates store remle

* Test of the RE specification using Breusch-Pagan test
* This is statistic in third bottom row of Table 24.4 
quietly xtreg LNEXP12M $XLISTLINEAR, re 
xttest0

* Hausman test of FE vs. RE specification
* This test is not a robust version. 
* Its validity asswumes that errors are iid after including COMMUNE-specific effect
* For this example this may be reasonable as cluster bootstrap se's close to usual se's
xthausman

* Alternative GLS estimation using the GEE approach 
* Same as xtgee with family(gaussian) link(id) corr(exchangeable)
* So GLS with equicorrelated errors
xtreg LNEXP12M $XLISTLINEAR, pa 
estimates store pa

********** DISPLAY TABLE 24.4 RESULTS page 851 **********
 
estimates table olsiid olshet olsclust, /*
   */ b(%10.3f) t(%10.2f) stats(r2 N)
estimates table pa fe refgls remle, /*
   */ b(%10.3f) t(%10.2f) stats(r2 N)

********** ADDITIONALLY DO CLUSTER BOOTSTRAPS **********

* These results not given in the text

* Output at websidet uses breps 500
global breps = 500

* Note that can bootstrap if desired to get more robust standard errors
* The first reproduces reg , cluster(COMMUNE)
bootstrap "reg LNEXP12M $XLISTLINEAR" _b, cluster(COMMUNE) reps($breps) level(95)
* The t-statistic vector is e(b)./e(se) where ./ is elt. by elt. division
* But Stata Version 8 does not do ./ so instead need the following
matrix tols = (vecdiag(diag(e(b))*syminv(diag(e(se)))))'
matrix list tols, format(%10.2f)

* The next two reproduce xtreg , cluster(COMMUNE)
* but the cluster option for xtreg is not available for Stata version 8

* For this example the cluster bootstrap se's are within 10 percent
* of the usual xtreg se's, so usual se's may be okay here

* Fixed effects estimator
bootstrap "xtreg LNEXP12M $XLISTLINEAR, fe" _b, cluster(COMMUNE) reps($breps) level(95)
matrix tfe = (vecdiag(diag(e(b))*syminv(diag(e(se)))))'
matrix list tfe, format(%10.2f)

* Random effects estimator
bootstrap "xtreg LNEXP12M $XLISTLINEAR, re" _b, cluster(COMMUNE) reps($breps) level(95)
matrix tre = (vecdiag(diag(e(b))*syminv(diag(e(se)))))'
matrix list tre, format(%10.2f)

********** CLOSE OUTPUT
log close
clear 
exit
 
