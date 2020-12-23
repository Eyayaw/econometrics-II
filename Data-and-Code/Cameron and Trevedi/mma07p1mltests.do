* MMA07P1MLTESTS.DO  March 2005 for Stata version 8.0

log using mma07p1mltests.txt, text replace

********** OVERVIEW OF MMA07P1MLTESTS.DO **********

* STATA Program 
* copyright C 2005 by A. Colin Cameron and Pravin K. Trivedi 
* used for "Microeconometrics: Methods and Applications" 
* by A. Colin Cameron and Pravin K. Trivedi (2005)
* Cambridge University Press 

* Chapter 7.4 pp.241-3
* Likelihood-based hypothesis tests

* Implements the three likelihood-based tests presented in Table 7.1:
*    Wald test
*    LR test
*    LM test direct
*    LM test via auxiliary regression
* for a Poisson model with simulated data (see below).

* NOTE: To implement this program requires:
*       the free Stata add-on rndpoix
* To obtain this, in Stata give command: search rndpoix
* If you don't want to do this, instead use the data set 

********** SETUP ***********

version 8
set more off
  
********** GENERATE DATA ***********

* Model is
*   y ~ Poisson[exp(b1 + b2*x2 + b3*x3 + b4*x4]
* where 
*    x2, x3 and x4 are iid ~ N[0,1]
* and b1=0, b2=0.1, b3=0.1 and b4=0.1

set seed 10001
set obs 200
scalar b1 = 0
scalar b2 = 0.1
scalar b3 = 0.1
scalar b4 = 0.1

* Generate regressors
gen x2 = invnorm(uniform())
gen x3 = invnorm(uniform()) 
gen x4 = invnorm(uniform()) 

* Generate y
gen mupoiss = exp(b1+b2*x2+b3*x3+b4*x4)
* The next requires Stata add-on. In Stata: search rndpoix
rndpoix(mupoiss)
gen y = xp

sum

* Write data to a text (ascii) file so can use with programs other than Stata
outfile y x2 x3 x4 using mma07p1mltests.asc, replace

********** ANALYSIS: LIKELIHOOD-BASED HYPOTHESIS TESTS ***********

* Hypotheses to test are    
* (A) Single exclusion:    b3 = 0
* (B) Multiple exclusion:  b3 = 0, b4 = 0
* (C) Linear:              b3 = b4 
* (B) Nonlinear:           b3/b4 = 1

* Tests are Wald, LR, LM and LM (auxiliary)

****** (A)  TEST H0: b3 = 0

* First skip to (B) where many comments given.

****** (B) TEST H0: b3 = 0, b4 = 0. 

* (1) Wald test requires estimation of unrestricted model only
poisson y x2 x3 x4

* (1A) Stata Wald test command
test (x3=0) (x4=0)

* (1B) Wald test done manually
* Use h'[RVR]-inv*h. 
* Details below will change for each example.
* In particular, for nonlinear restrictions more work in forming R
* Note that Stata puts the intercept last, not first. 
* So here the second and third elements of b are set to zero.
matrix bfull = e(b)                    /* 1xq row vector */   
matrix vfull = e(V)                    /* qxq matrix */   
matrix h = (bfull[1,2]\bfull[1,3])     /* hx1 vector */
matrix R = (0,1,0,0\0,0,1,0)           /* h x q matrix */
matrix Wald = h'*syminv(R*vfull*R')*h  /* scalar */
matrix list h
matrix list R
matrix list Wald 
scalar WaldB = Wald[1,1]

* (2) Likelihood ratio test requires estimating both models

poisson y x2 x3 x4
estimates store unrestricted           /* Used for Stata lrtest */ 
scalar llunrest = e(ll)                /* Used for manual lrtest */
poisson y x2 
estimates store restrictedB            /* Used for Stata lrtest */
scalar llrestB = e(ll)                  /* Used for Stata lrtest */

* (2A) Stata likelihood ratio test
lrtest unrestricted restrictedB

* (2B) Likelihood test done manually 
scalar LRB = -2*(llrestB-llunrest)
di "LR  " LRB

* (3) LM test via direct compuation requires estimating only the restricted model. 

* For exclusion restrictions in the Poisson, from 7.6.2
* LM = dlnL/db * V[b]-inv * dlnL/db   where b evaluated at restricted
*    = [Sum_i u_i*x_i]'[Sum_i exp(x_i'b)*x_i*x_i'][Sum_i u_i*x_i]
* First calculate Sum_i u_i*x_i' : a 1x4 row vector 

quietly poisson y x2 
predict yhatrest
gen u = y - yhatrest     /* yhatrest = exp(x_brest) calculated earlier */
gen one = 1
matrix vecaccum dlnL_db = u one x2 x3 x4, noconstant  
* Then calculate Sum_i exp(x_i'b)*x_i*x_i'
gen trx1 = sqrt(yhatrest)
gen trx2 = sqrt(yhatrest)*x2
gen trx3 = sqrt(yhatrest)*x3
gen trx4 = sqrt(yhatrest)*x4
matrix accum Vb = trx1 trx2 trx3 trx4, noconstant
matrix LMdirect = dlnL_db*syminv(Vb)*dlnL_db' 
matrix list dlnL_db
matrix list Vb
matrix list LMdirect
scalar LMdirectB = LMdirect[1,1]

* (4) LM test via auxiliary regression 

* N uncentered Rsq from regress (noconstant) 1 on the scores
* Begin by computing the unrestricted scores at the restricted estimates.
* This varies from problem to problem.
* In general could compute lnf(y) at current parameters
* and then get numerical derivative when perturb beta a little. 
* Here use analytical derivative.
* s_j = dlnf(y)/db_j = (y-exp(x'b))*x_j for the Poisson

drop yhatrest
quietly poisson y x2 
predict yhatrest
gen s1 = (y-yhatrest)*1
gen s2 = (y-yhatrest)*x2
gen s3 = (y-yhatrest)*x3
gen s4 = (y-yhatrest)*x4
regress one s1 s2 s3 s4, noconstant
* LM equals N times uncentered Rsq
scalar LMauxB = e(N)*e(r2)
* Check: LM equals explained sum of squares 
scalar LMauxB2 = e(mss)
di "LMauxB " LMauxB "    LMauxB2 " LMauxB2

* (5) DISPLAY RESULTS

estimates table unrestricted restrictedB, se stats(N ll r2) b(%8.3f)
* Wald test using stata default Poisson variance matrix
di "WaldB " WaldB  " p-value " chi2tail(2,WaldB) 
* LR test using Poisson log-likelihoods
di " LRB " LRB " p-value " chi2tail(2,LRB) 
* LM test direct 
di " LMdirectB " LMdirectB " p-value " chi2tail(2,LMdirectB)
* LM test direct by auxiliary regression
di " LMauxB " LMauxB " p-value " chi2tail(2,LMauxB)

****** (A)  TEST H0: b3 = 0

* (1) Wald test
quietly poisson y x2 x3 x4
test (x3=0) 
scalar WaldA = r(chi2)

* (2) LR test
poisson y x2 x4
estimates store restrictedA
lrtest unrestricted    /* Uses estimates store unrestricted from earlier */
scalar LRA = r(chi2)

* (3) LM test via direct compuation requires estimating only the restricted model. 
* See (B) for more explanation
drop one yhatrest u trx1 trx2 trx3 trx4
matrix drop dlnL_db Vb LMdirect
quietly poisson y x2 x4
predict yhatrest
gen u = y - yhatrest     /* yhatrest = exp(x_brest) calculated earlier */
gen one = 1
matrix vecaccum dlnL_db = u one x2 x3 x4, noconstant  
gen trx1 = sqrt(yhatrest)
gen trx2 = sqrt(yhatrest)*x2
gen trx3 = sqrt(yhatrest)*x3
gen trx4 = sqrt(yhatrest)*x4
matrix accum Vb = trx1 trx2 trx3 trx4, noconstant
matrix LMdirect = dlnL_db*syminv(Vb)*dlnL_db' 
matrix list dlnL_db
matrix list Vb
matrix list LMdirect
scalar LMdirectA = LMdirect[1,1]

* (4) LM test via auxiliary regression 
* See (B) for more explanation
drop yhatrest s1 s2 s3 s4 one
quietly poisson y x2 x4
predict yhatrest
gen s1 = (y-yhatrest)*1
gen s2 = (y-yhatrest)*x2
gen s3 = (y-yhatrest)*x3
gen s4 = (y-yhatrest)*x4
gen one = 1
regress one s1 s2 s3 s4, noconstant
* LM equals N times uncentered Rsq
scalar LMauxA = e(N)*e(r2)
di "LMauxA " LMauxA 

* (5) DISPLAY RESULTS in Table 7.1 page 242

estimates table unrestricted restrictedA, se stats(N ll r2) b(%8.3f)
di "WaldA " WaldA  " p-value " chi2tail(1,WaldA) 
di " LRA " LRA " p-value " chi2tail(1,LRA) 
di " LMdirectA " LMdirectA " p-value " chi2tail(1,LMdirectA)
di " LMauxA " LMauxA " p-value " chi2tail(1,LMauxA)

****** (C) TEST H0: b3 = b4 
 
* (1A) Wald test
poisson y x2 x3 x4
test (x3=x4)

* (1B) Wald test done manually 
* Note that Stata puts the intercept last, not first. 
* So here the second and third elements of b are tested as equal.
matrix drop h R Wald
matrix bfull = e(b)                    /* 1xq row vector */   
matrix vfull = e(V)                    /* qxq matrix */   
matrix h = (bfull[1,2]-bfull[1,3])     /* hx1 vector */
matrix R = (0,1,-1,0)                  /* h x q matrix */
matrix Wald = h'*syminv(R*vfull*R')*h  /* scalar */
matrix list h
matrix list R
matrix list Wald 
scalar WaldC = Wald[1,1]
di " WaldC " WaldC " p-value " chi2tail(1,WaldC) 

* (2) LR Test 
* In general getting the restricted MLE requires constrained ML
* Here simple as if b3=b4 then mean is exp(b1+b2*x2+B3*(x3+x4))
gen x3plusx4 = x3+x4
poisson y x2 x3plusx4
estimates store restrictedC
lrtest unrestricted      /* Uses estimates store unrestricted from earlier */
scalar LRC = r(chi2)

* (3) LM test direct
* Can use same code as earlier. Just different restricted estimates.
* Now from poisson y x2 x3plusx4
drop one yhatrest u trx1 trx2 trx3 trx4
matrix drop dlnL_db Vb
quietly poisson y x2 x3plusx4
predict yhatrest
gen u = y - yhatrest     /* yhatrest = exp(x_brest) calculated earlier */
gen one = 1
matrix vecaccum dlnL_db = u one x2 x3 x4, noconstant  
gen trx1 = sqrt(yhatrest)
gen trx2 = sqrt(yhatrest)*x2
gen trx3 = sqrt(yhatrest)*x3
gen trx4 = sqrt(yhatrest)*x4
matrix accum Vb = trx1 trx2 trx3 trx4, noconstant
matrix LMdirect = dlnL_db*syminv(Vb)*dlnL_db' 
matrix list dlnL_db
matrix list Vb
matrix list LMdirect
scalar LMdirectC = LMdirect[1,1]

* (4) LM test via auxiliary regression 
drop yhatrest s1 s2 s3 s4 one
quietly poisson y x2 x3plusx4
predict yhatrest
gen s1 = (y-yhatrest)*1
gen s2 = (y-yhatrest)*x2
gen s3 = (y-yhatrest)*x3
gen s4 = (y-yhatrest)*x4
gen one = 1
regress one s1 s2 s3 s4, noconstant
* LM equals N times uncentered Rsq
scalar LMauxC = e(N)*e(r2)
di "LMauxC " LMauxC 
 
* (5) DISPLAY RESULTS in Table 7.1 page 242

estimates table unrestricted restrictedC, se stats(N ll r2) b(%8.3f)
di "WaldC " WaldC  " p-value " chi2tail(1,WaldC) 
di " LRC " LRC " p-value " chi2tail(1,LRC) 
di " LMdirectC " LMdirectC " p-value " chi2tail(1,LMdirectC)
di " LMauxC " LMauxC " p-value " chi2tail(1,LMauxC)

****** (D) TEST H0: b3/b4 - 1 = 0

* (1) Wald test of b3 /b4 - 1 = 0
* Stata does not do nonlinear hypotheses.
* Instead do 7.2.5 algebra.
matrix drop h R Wald
matrix h = (bfull[1,2]/bfull[1,3] - 1) 
matrix R = (0, 1/bfull[1,3], -bfull[1,2]/(bfull[1,3]^2), 0)  
matrix Wald = h'*syminv(R*vfull*R')*h  
matrix list h
matrix list R
matrix list Wald 
scalar WaldD = Wald[1,1]
di " WaldD " WaldD " p-value " chi2tail(1,WaldD) 

* (2) LR Test 
* This requires MLE subject to nonlinear constraints. 
* This is difficult so not done here.
* But note that here will get same result as if 
* get MLE subject to b3 = b4 which was done in (C).

* (3) LM test direct
* Like (2) requires restricted MLE.
* This is difficult so not done here.
* But note that here will get same result as if 
* get MLE subject to b3 = b4 which was done in (C).
 
* (4) LM test via auxiliary regrression
* Same as for (3)

* (5) DISPLAY RESULTS
di "WaldD " WaldD  " p-value " chi2tail(1,WaldD) 


*********** DISPLAY RESULTS GIVEN IN TABLE 7.1 on page 242 ***********

estimates table unrestricted restrictedA restrictedB restrictedC, se stats(N ll r2) b(%8.3f)
di "WaldA " WaldA  " p-value " chi2tail(1,WaldA) 

* Wald test statistics
di "Wald A to D: (A) " %8.3f WaldA " (B) " %8.3f WaldB " (C) " %8.3f WaldC " (D) " %8.3f WaldD
di " p-values  : (A) " %8.3f chi2tail(1,WaldA) " (B) " %8.3f chi2tail(2,WaldB) " (C) " %8.3f chi2tail(1,WaldC) " (D) " %8.3f chi2tail(1,WaldD) 

* LR test statistics
di "LR A to D:   (A) " %8.3f LRA " (B) " %8.3f LRB " (C) " %8.3f LRC " (D) " %8.3f LRC
di " p-values :  (A) " %8.3f chi2tail(1,LRA) " (B) " %8.3f chi2tail(2,LRB) " (C) " %8.3f chi2tail(1,LRC) " (D) " %8.3f chi2tail(1,LRC) 

* Direct LM test statistics
di "LM A to D:   (A) " %8.3f LMdirectA " (B) " %8.3f LMdirectB " (C) " %8.3f LMdirectC " (D) " %8.3f LMdirectC
di " p-values:   (A) " %8.3f chi2tail(1,LMdirectA) " (B) " %8.3f chi2tail(2,LMdirectB) " (C) " %8.3f chi2tail(1,LMdirectC) " (D) " %8.3f chi2tail(1,LMdirectC) 

* Auxiliary Regression LM test statistics
di "LM* A to D:  (A) " %8.3f LMauxA " (B) " %8.3f LMauxB " (C) " %8.3f LMauxC " (D) " %8.3f LMauxC
di " p-values :  (A) " %8.3f chi2tail(1,LMauxA) " (B) " %8.3f chi2tail(2,LMauxB) " (C) " %8.3f chi2tail(1,LMauxC) " (D) " %8.3f chi2tail(1,LMauxC) 

********** CLOSE OUTPUT ***********
log close
clear
exit

