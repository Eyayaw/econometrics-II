* MMA07P4BOOT.DO  March 2005 using Stata version 8.0

log using mma07p4boot.txt, text replace

********** OVERVIEW OF MMA07BOOT4.DO **********

* STATA Program 
* copyright C 2005 by A. Colin Cameron and Pravin K. Trivedi 
* used for "Microeconometrics: Methods and Applications" 
* by A. Colin Cameron and Pravin K. Trivedi (2005)
* Cambridge University Press 

* Chapter 7.8 pages 254-256
* Bootstrap applied to probit model
* Provides
*  (1) Bootstrap confidence intervals
*  (2) Bootstrap hypothesis test without refinement
*  (3) Bootstrap hypothesis test with refinement: percentile-t method

* Note corrections to book
*  - sample size is N=40  not N=30
*  - use 999 bootstrap replications not 1000
*  - for asymptotic refinement p.256 the critical region 
*      is (-1.89, 1.80) not (-2.62, 1.83)

* For more detail on bootstrap see 
* Chapter 11: Bootstrap Methods pages 355-383
* and program mma11p1boot.do

********** SETUP **********

set more off
version 8
  
********** GENERATE DATA **********

* DGP is Probit: Pr[y=1] = PHI(a + bx)
* where  x is N[0,1]
* and  a = 0  and b = 1

* Change the following for different sample size N
global numobs "40"

* Probit example with slope coefficient equal to 1
set seed 10105
set obs $numobs
gen x = invnorm(uniform())
gen y = 0
replace y = 1 if 0+1.0*x+invnorm(uniform()) > 0
save xyforsim, replace
summarize 
probit y x
save mma07p4boot, replace

* Write data to a text (ascii) file so can use with programs other than Stata
outfile y x using mma07p4boot.asc, replace

********** (1) BOOTSTRAP CONFIDENCE INTERVALS ********** 

* Stata produces four bootstrap 100*(1-alpha) confidence intervals
* (1)-(2) have no asymptotic refinement
* (3)-(4) have asymptotic refinement

* (1) Regular asymptotic normal: bhat +/- t(S-1)_alpha/2*se(bhat)
*     except instead of using the initial se(bhat) 
*     we use the standard deviation of bhat from the bootstrap reps
*     and use t(S-1) rather than z for critical value
*     where S = number of bootstrap reps

* (2) Percentile method: which orders the bhat(s) from simulations and
*     goes from alpha/2 lowest bhat(s) to the alpha/2 highest bhat(s) 
*     where (s) denotes the s-th bootstrap sample

* (3) Bootstrap-corrected. Same as (4) with a=0

* (4) Bootstrap-corrected and accelerated. 
*     This works with the pivotal Wald statistic.
*     See the manual [R]bootstrap or a textbook.
*     e.g. Efron and Tibsharani (1993, pp.184-188) with a=0
*     This orders the bhats from simulations and
*     goes from p1 to the p2 highest
*     where p1 and p2 are bias-correction adjustments to alpha/2 and 1-alpha/2
*     Let p1 = Phi(2z0 - z_alpha/2)
*         p2 = Phi(2z0 + z_alpha/2)
*         z0 measures the median bias in bhat with
*         z0 = Phi-inv(fraction of the bhat(s) < bhat)
*     And if z0=0 then p1 = alpha/2 and no correction 

* Change the following for different number of simulations S
* From page 399, for testing better to use 999 than 1000
global breps "999"   /* The number of bootstrap reps used below */ 

* (1A) Simplest bootstrap is of all the estimated coefficients
set seed 10105
bootstrap "probit y x" _b, reps($breps) bca

* (1B) This bootstrap is of MLE of b2 and the associated standard error
*     and additionally gives the bias-accelerated method of Efron
set seed 10105
bootstrap "probit y x" _b[x] _se[x], reps($breps) bca

* (1C) This bootstrap repeats (2) 
*     but will permit bootstrapping if Stata commands are more than one line
use mma07p4boot, clear
program define commandtobootstrap, rclass
  version 8.0
  quietly probit y x
  return scalar b2hat=_b[x]
  return scalar seb2hat=_se[x]
end
set seed 10105
bootstrap "commandtobootstrap" r(b2hat) r(seb2hat), reps($breps)

********** (2) BOOTSTRAP HYPOTHESIS TESTS - NO REFINEMENT p.255 ********** 

* We want to test H0: b2 = 1 against Ha: b2 not equal 1

* For a simple test such as this we can just use
* the bootstrap confidence intervals from (1) 
* and reject if bhat2 is not in the confidence interval

* Here we instead present a common method without refinement
* essentially (1) above, performing the usual Wald test, 
* except the standard error is estimated by bootstrap.
* This is useful when hard to obtain standard error by other means.
* Here  W = (b2hat - b2_0) / seb2hat_boot  where b2_0 = 1
* and reject at level .05 if |W| > z_.025 = 1.96

use mma07p4boot, clear
* Save the estimate 
quietly probit y x
scalar b2est = _b[x]
* Obtain the bootstrap standard error
set seed 10105
bootstrap "probit y x" _b, reps($breps) bca
matrix sebboot = e(se)
scalar seb2boot = sebboot[1,1]   /* x is first then constant */
* Calculate the test statistic
scalar Wald = (b2est - 1)/seb2boot

* DISPLAY RESULTS at bottom p.255
* Note: Text had typo: 
* (1-0.817)/0.376 = -0.487 should be (0.817-1)/0.376 = -0.487

di "Probit slope estimate is:        "   b2est
di "Bootstrap standard estimate is:  "   seb2boot 
di "Wald statistic (no refinement) is: " Wald
di "Reject at level .05 if |Wald| > 1.96"

********** (3) BOOTSTRAP HYPOTHESIS TESTS - PERCENTILE-T p.256 ********** 

* Stata does not give this. For methods see
* e.g. Efron and Tibsharani (1993, pp.160-162)  
* e.g. Cameron and Trivedi (2005) Chapter 11.2.6-11.2.7
* For sample s compute t-test(s) = (bhat(s)-bhat) / se(s)
* where bhat is initial estimate 
* and bhat(s) and se(s) are for sth round.
* Order the t-test(s) statistics and choose the alpha/2 percentiles
* which give the critical values for the t-test

* Implementation requires saving the results from each bootstrap replication 
* in order to obtain ccritical values from percentiles of bootstrap distribution

* (3A) Here bootstrap computes (b(s) - bhat) / se(s)  s = 1,...,S
 
use mma07p4boot, clear
* Save the estimate and the Wald test statistic 
quietly probit y x
scalar b2est = _b[x]
scalar Wald = (_b[x] - 1)/_se[x]
* Then bootstrap calculates (b(s) - bhat) / se(s)
set seed 10105
bootstrap "probit y x" ((_b[x]-b2est)/_se[x]), reps($breps) /*
   */ level(95) saving(mma07p4bootreps) replace
* Then get data sets with result from each bootstrap
use mma07p4bootreps, clear
sum                 /* Here just _bs_1 */
gen b2test = _bs_1  /* _bs_1 is the bootstrap result of interest */
sum b2test, detail  /* Gives percentiles but not 2.5% and 97.5%  */
_pctile b2test, p(2.5,97.5)

* DISPLAY RESULTS on p.256

* Note: Error on p.256  Here get (-1.89, 1.80) not (-2.62, 1.83)
di "Lower 2.5 and upper 2.5 percentile of coeff b for z: " r(r1) "  and  " r(r2) 
di "Reject H0 if Wald = " Wald " lies outside " r(r1) " ," r(r2) ")"

* (3B) Equivalently bootstrap calculates b(s) and se(s)   s = 1,...,S
*      and then later calculate (b(s) - bhat) / se(s)

use mma07p4boot, clear
* Save the estimate and the Wald test statistic 
quietly probit y x
scalar b2est = _b[x]
scalar Wald = (_b[x] - 1)/_se[x]
* Then bootstrap calculates b(s) and se(s)
set seed 10105
bootstrap "probit y x" _b[x] _se[x], reps($breps) /*
   */ level(95) saving(mma07p4bootreps) replace
* Then get data sets with result from each bootstrap
use mma07p4bootreps, clear
sum                 /* Here _bs_1 and _bs_2 */
gen b2test = (_bs_1 - b2est)/_bs_2
_pctile b2test, p(2.5,97.5)

* DISPLAY RESULTS on p.256
* Note: Error on p.256  Here get (-1.89, 1.80) not (-2.62, 1.83)
di "Lower 2.5 and upper 2.5 percentile of coeff b for z: " r(r1) "  and  " r(r2) 
di "Reject H0 if Wald = " Wald " lies outside " r(r1) " ," r(r2) ")"

********** CLOSE OUTPUT
log close
clear 
exit

