* MMA05P2NLS.DO March 2005 for Stata version 8.0

clear
capture log close 
log using mma05p2nls.txt, text replace

********** OVERVIEW OF MMA05P2NLS.DO **********

* STATA Program 
* copyright C 2005 by A. Colin Cameron and Pravin K. Trivedi 
* used for "Microeconometrics: Methods and Applications" 
* by A. Colin Cameron and Pravin K. Trivedi (2005)
* Cambridge University Press 

* Chapter 5.9 pp.159-63
* Nonlinear least squares

* Provides last three columns of Table 5.7 results for
*   (1) NLS     using Stata command nl (hard to get robust s.e.'s)
*   (2) FGNLS   using Stata command nl (hard to get robust s.e.'s)
*   (3) WNLS    using Stata command nl (hard to get robust s.e.'s)
* using generated data set mma05data.asc

* Note: Stata 8 does not give robust se's for nl
*       But ml does - see program mma05p3nlsbyml.do
*       New Stata 9 does have a robust se option (unlike Stata 8)

* Related programs:
*   mma05p1mle.do          OLS and MLE for the same data 
*   mma05p3nlsbyml.do      NLS using ml rather than nl
*   mma05p4margeffects.do  Calculates marginal effects     

* To run this program you need data and dictionary files
*    mma05data.asc    ASCII data set generated by mma05p1mle.do

********** SETUP **********
 
set more off
version 8

********** READ IN DATA and SUMMARIZE **********

* Model is  y ~ exponential(exp(a + bx))
*           x ~ N[mux, sigx^2]
*        f(y) = exp(a + bx)*exp(-y*exp(a + bx))
*      lnf(y) = (a + bx) - y*exp(a + bx)
*        E[y] = exp(-(a + bx))    note sign reversal for the mean
*        V[y] = exp(-(a + bx)) = E[y]^2
* Here a = 2, b = -1  and  x ~ N[mux=1, sigx^21]
* and Table 5.7 uses N=10,000 

* Data was generated by program mma05p1mle.do
infile y x using mma05data.asc

* Descriptive Statistics
describe
summarize

********** DO THE ANALYSIS: NLS, WNLS and NFGLS **********

*** (1) NLS ESTIMATION USING STATA NL COMMAND (Nonlinear LS)

* To do this in Stata
* (A) program define nlfcn    where fcn is the function name
*        defines g(x_i'b) and says what the regressors x are
* (B) nl fcn y                where fcn is the function name in (A)
*                             and y is the dependent variable
*        does NLS of y on fcn defined in (A)
* (C) Heteroskedastic-consistent standard errors requires extra coding

* (1A) Define g(x'b)
*      Note: Since E[y] = exp(-(a + bx)) there is sign reversal for the mean
program define nlexpnls
  version 7.0 
  if "`1'" == "?" {                /* if query call ...     */
     global S_1 "b1int b2x"           /* declare parameters */
     global b1int=1                   /* initial values     */
     global b2x=0
     exit}
  replace `1'=exp(-$b1int-$b2x*x)   /* calculate function */
end

* (1B) Do NLS of y on the function expnls defined in (A)
nl expnls y
estimates store bnls

* Complications now begin: getting standard erors. Easier to use (1) !!

* (1C) Get sandwich heteroskedastic-robust standard errors for NLS 

* Note that robust option does not work for nl
* So wrong standard errors are given for this problem as errors are heterosckeastic

* To get robust standard errors is not straightforward

* Obtain them by OLS regress y - g(x,b) on dg/db with robust option.
* Explanation: OLS regress y - g(x,b) = (dg/db)'a + v
* This is NR algorithm for update of b
* But a = 0 since iterations have converged, so v = y - g(x,b)
* So nonrobust standard errors from this OLS regression yield
*   V[a] = s^2 (Sum_i (dg_i/db)(dg_i/db)') 
*   where s^2 = (Sum_i(y - g(x_i,b)^2)) 
* This is the nonrobust standard errors for NLS 
* And robust option gives robust standard errors from this OLS regression.

* Obtain the derivatives dg/db
* Here g = exp(x'b) so dg/db = exp(x'b)*x = yhat*x
quietly nl expnls y
predict residnls, residuals 
predict yhatnls, yhat
scalar snls = e(rmse)    /* Use in earlier code */
gen d1 = yhatnls
gen d2 = x*yhatnls 
* This OLS regression gives robust standard errors
regress residnls d1 d2, noconstant robust
estimates store bnlsrobust

* Check: Do OLS regression that gives nonrobust standard errors
*        and verify that same results as in (1B) 
regress residnls d1 d2, noconstant
estimates store bnlscheck

* (1D) Alternative to (1C) robust NLS standard errors that are better.
* These are sandwich form but use knowledge that V[u]=exp(x'b)^2
* which can be estimated by Vhat[u] = yhat
* Now use this knowledge here in computing S in DSD.
* Form DSDknown = D'SD with S = Diag(yhat^2)
gen ds1known = yhatnls*yhatnls
gen ds2known = x*yhatnls*yhatnls
matrix accum DSDknown = ds1known ds2known, noconstant
matrix accum DD2 = d1 d2, noconstant     /* DD commented above */
* Form the robust variance matrix estimate
matrix vnlsknown = syminv(DD2)*DSDknown*syminv(DD2)
* Calculate the robust standard errors
scalar seb1intnlsknown = sqrt(vnlsknown[1,1])
scalar seb2xnlsknown = sqrt(vnlsknown[2,2])
di "Robust standard errors of NLS estimates of b1int and b2x: " 
di "Using knowledge that Var[u] = exp(x'b)^2 estimated by yhat"
di seb1intnlsknown "  " seb2xnlsknown 

* (1E) Calculate R-squared and log-likelihood at the NLS estimates
* Note that Stata version 8 reports the wrong R-squared
* as uses TSS = Sum_i y_i^2 and not Sum_i(y_i - ybar)^2
* lnL sums lnf(y) = ln(lamda) - y*lamda
gen lamdanls = 1 / yhatnls        /* yhatnls saved earlier */
gen lnfnls = ln(lamdanls) - y*lamdanls
quietly means lnfnls
scalar LLnls = r(mean)*r(N)
* R-squared = 1 - Sum_i(y_i - yhat_i)^2 / Sum_i(y_i - ybar)^2
egen ybar = mean(y)
* quietly means y
* scalar ybar = r(mean)
gen y_ybarsq = (y - ybar)^2
quietly means y_ybarsq
scalar SStotal = r(mean)
gen y_yhatsqnls = (y - yhatnls)^2
quietly means y_yhatsqnls
scalar SSresidnls = r(mean)
scalar Rsqnls = 1 - SSresidnls/SStotal     /* SStotal found earlier */
di LLnls "    " Rsqnls

** (2) FGNLS ESTIMATION USING STATA NL COMMAND

* The following gives FGNLS in Table 5.7
* To instead get the WNLS estimates in Table 5.7
* replace gen wfgnls = (1/yhatnls)^2 below by gen wfgnls = 1/yhatnls

* The Feasible generalized NLS estimator minimizes 
*  SUM_i (y_i - g(x_i'b))^2 / s_i^2   where s_i^2 = estimate of sigma_i^2
* This is y_i = g(x_i'b) + u_i  where  u_i ~ (0,s_i^2)
* Can do NLS with weighting option  [aweight = 1/(s_i^2)]
* Here s_i^2 = [exp(x_i'b)]^2 = yhatnls^2 

* The simplest way to proceed is to use the aweights option.

* (2A) nls program expnls already defined in (1A)
 
* (2B) For FGNLS do this nls but now with weights
gen wfgnls = (1/yhatnls)^2
* gen wfgnls = 1/yhatnls
nl expnls y [aweight=wfgnls]
estimates store bfgnls

* (2C) Robust standard errors 
* The standard errors obtained given are consistent 
* assuming correct model for heteroskedasticity.
* To guard against misspecification use similar approach to nls case 
* Obtain the derivatives dg/db
* Here g = exp(x'b) so dg/db = exp(x'b)*x = yhat*x
predict residoptnls, residuals 
predict yhatoptnls, yhat
gen d1opt = yhatoptnls
gen d2opt = x*yhatoptnls 
* This OLS regression gives robust standard errors
regress residoptnls d1opt d2opt [aweight=wfgnls], noconstant robust
estimates store bfgnlsrobust
* This OLS regression gives nonrobust standard errors
* It is a check and should equal (C)
regress residoptnls d1opt d2opt [aweight=wfgnls], noconstant
estimates store bfgnlscheck

* (2D) Calculate R-squared and log-likelihood at the NLS estimates
* Note that Stata version 8 reports the wrong R-squared
* as uses TSS = Sum_i y_i^2 and not Sum_i(y_i - ybar)^2
* lnL sums lnf(y) = ln(lamda) - y*lamda
gen lamdafgnls = 1 / yhatoptnls        /* yhatoptnls saved earlier */
gen lnffgnls = ln(lamdafgnls) - y*lamdafgnls
quietly means lnffgnls
scalar LLfgnls = r(mean)*r(N)
* R-squared = 1 - Sum_i(y_i - yhat_i)^2 / Sum_i(y_i - ybar)^2
gen y_yhatsqfgnls = (y - yhatoptnls)^2
quietly means y_yhatsqfgnls
scalar SSresidfgnls = r(mean)
scalar Rsqfgnls = 1 - SSresidfgnls/SStotal     /* SStotal found earlier */
di LLfgnls "    " Rsqfgnls

** (3) WNLS ESTIMATION USING STATA NL COMMAND

* To get WNLS estimates in Table 5.7
* replace gen wfgnls = (1/yhatnls)^2 in (3) FGNLS by gen wfgnls = 1/yhatnls
* Code is shorter as all comments are dropped

gen wwnls = 1/yhatnls
nl expnls y [aweight=wwnls]
estimates store bwnls
predict residwnls, residuals 
predict yhatwnls, yhat
gen d1w = yhatwnls
gen d2w = x*yhatwnls 
regress residwnls d1w d2w [aweight=wwnls], noconstant robust
estimates store bwnlsrobust
regress residwnls d1w d2w [aweight=wwnls], noconstant
estimates store bwnlscheck
gen lamdawnls = 1 / yhatwnls        /* yhatwnls saved earlier */
gen lnfwnls = ln(lamdawnls) - y*lamdawnls
quietly means lnfwnls
scalar LLwnls = r(mean)*r(N)
gen y_yhatsqwnls = (y - yhatwnls)^2
quietly means y_yhatsqwnls
scalar SSresidwnls = r(mean)
scalar Rsqwnls = 1 - SSresidwnls/SStotal     /* SStotal found earlier */
di LLwnls "    " Rsqwnls

***** PRINT RESULTS: Last three columns of Table 5.7 page 161

* (1) NLS using NL - nonrobust and robust standard errors 
*     Here nonrobust differs from robust asymptotically

* Table 5.7 NLS nonrobust standard errors
estimates table bnls, b(%10.4f) se(%10.4f) t stats(N ll)
* Table 5.7 NLS robust standard errors
estimates table bnlscheck bnlsrobust, b(%10.4f) se(%10.4f) t stats(N ll)

/*
* Check: Nonrobust standard errors of NLS b1int and b2x:  
di seb1intnlsnr "  " seb2xnlsnr 
* Robust standard errors of NLS estimates of b1int and b2x:  
di seb1intnls "  " seb2xnls 
*/
* Alternative Robust standard errors of NLS estimates of b1int and b2x:  
* These use knowledge that Var[u] = exp(x'b)
di seb1intnlsknown "  " seb2xnlsknown

* (3) WNLS - nonrobust and robust standard errors 
*     Here nonrobust = robust asymptotically as WNLS in LEF
*     Also should be same as MLE asymptotically
* Table 5.7 WNLS nonrobust standard errors
estimates table bwnls, b(%10.4f) se(%10.4f) t stats(N ll)
* Table 5.7 WNLS robust standard errors
estimates table bwnlscheck bwnlsrobust, b(%10.4f) se(%10.4f) t stats(N ll)

* (2) FGNLS - nonrobust and robust standard errors 
*     Here nonrobust = robust asymptotically as FGNLS in LEF
*     Also should be same as MLE asymptotically
* Table 5.7 FGNLS nonrobust standard errors
estimates table bfgnls, b(%10.4f) se(%10.4f) t stats(N ll)
* Table 5.7 FGNLS robust standard errors
estimates table bfgnlscheck bfgnlsrobust, b(%10.4f) se(%10.4f) t stats(N ll)

* (4) Print the various log-likelihoods and R-squared
* Log-likelihood for NLS and FNGLS
di "LLnls: " LLnls "  LLfgnls: " LLfgnls "  LLwnls: " LLwnls 
* R-squared for MLE, NLS and FNGLS
di "Rsqnls: " Rsqnls "  Rsqfgnls: " Rsqfgnls "  Rsqwnls: " Rsqwnls

********** CLOSE OUTPUT **********
log close
clear
exit