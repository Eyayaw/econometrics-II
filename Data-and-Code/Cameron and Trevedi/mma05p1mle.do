* MMA05P1MLE.DO March 2005 for Stata version 8.0

log using mma05p1mle.txt, text replace

********** OVERVIEW OF MMA05P1MLE.DO **********

* STATA Program 
* copyright C 2005 by A. Colin Cameron and Pravin K. Trivedi 
* used for "Microeconometrics: Methods and Applications" 
* by A. Colin Cameron and Pravin K. Trivedi (2005)
* Cambridge University Press 

* Chapter 5.9 pp.159-63 
* Maximum likelihood analysis.

* Provides first two columns of Table 5.7
*   (1) OLS      using Stata command regress 
*   (2) MLE      using Stata command exp for exponential MLE  
*   (3) MLE      using Stata command ml for user-provided log-likelihood
* using generated data (see below)

* Related programs:
*   mma05p2nls.do          NLS, WNLS, FGNLS for same data using nl command
*   mma05p3nlsbyml.do      NLS, WNLS, FGNLS for same data using ml command
*   mma05p4margeffects.do  Calculates marginal effects     

********** SETUP **********

set more off
version 8

********** GENERATE DATA and SUMMARIZE **********

* Model is  y ~ exponential(exp(a + bx))
*           x ~ N[mux, sigx^2]
*        f(y) = exp(a + bx)*exp(-y*exp(a + bx))
*      lnf(y) = (a + bx) - y*exp(a + bx)
*        E[y] = exp(-(a + bx))    note sign reversal for the mean
*        V[y] = exp(-(a + bx)) = E[y]^2

* The dgp sets particular values of a, b, mux and sigx
* Here a = 2, b = -1  and  x ~ N[1, 1]
scalar a = 2
scalar b = -1
scalar mux = 1  
scalar sigx = 1 

* Set the sample size. Table 5.7 uses N=10,000 
set obs 10000

* Generate x and y
set seed 2003
gen x = mux + sigx*invnorm(uniform()) 
gen lamda = exp(a + b*x)
gen Ey = 1/lamda
* To generate exponential with mean mu=Ey use
*   Integral 0 to a of (1/mu)exp(-x/mu) dx   by change of variables
* = Integral 0 to a/mu of exp(-t)dt
* = incomplete gamma function P(0,a/mu) in the terminology of Stata
gen y = Ey*invgammap(1,uniform())
gen lny = ln(y)
gen lnfy = ln(lamda) - y*lamda
* twoway scatter Ey x

* Descriptive Statisitcs
describe
summarize

********** WRITE DATA TO A TEXT FILE **********

* Write data to a text (ascii) file
* used for programs mma05p2nlsbyml.do, mma05p3nlsbynl.do 
* and mma05p4margeffects.do
* and can also use with programs other than Stata  
outfile y x using mma05data.asc, replace

********** DO THE ANALYSIS: OLS and MLE **********

** (1) OLS ESTIMATION

* OLS is inconsistent in this example
regress y x
estimates store rols
regress y x, robust
estimates store rolsrobust

** (2) ML ESTIMATION USING STATA COMMAND FOR EXPONENTIAL MLE

* The following uses Stata duration model commands. 
* First need to define the duration variable (here y)
stset y
streg x, dist(exp) nohr
estimates store rexp
streg x, dist(exp) nohr robust
estimates store rexprobust

** (3) ML ESTIMATION USING STATA ML COMMAND

* For MLE computation can use the following Stata commands
*   ml model lf      provide the log-density
*   ml model D0      provide the log-likelihood 
*   ml model D1      provide the log-likelihood and gradient
*   ml model D2      provide the log-likelihood, gradient and hessian

* At a minimum need to provide 
* (A) program define fcn     where fcn is the function name
*        defines the log-density  (independent observations assumed)                    
* (B) ml model lf fcn + some extras 
*        the extras give the dependent variable and regressors
* (C) ml maximize 
*        obtains the mle
* (D) ml model lf fcn + some extras, robust 
*        provides robust sandwich standard errors

* Here we provide the log-density (ml model lf) as this is simplest,
* and the Stata manual says that numerically only D2 is better.

* (A) Define the log-density
*      lnf(y) = (a+bx) - y*exp(a+bx) = theta - y*exp(theta) where theta = x'b
program define mleexp0
  version 8.0
  args lnf theta      /* Must use lnf while could use name other than theta */
  quietly replace `lnf' = `theta' - $ML_y1*exp(`theta')
end

* (B) Say that dependent variable is y and regressors are x plus a constant
ml model lf mleexp0 (y = x)

* (C) Obtain the MLE
ml search        /* Optional - can provide better starting values */
ml maximize
estimates store rmle

* (D) Obtain robust standard errors 
ml model lf mleexp0 (y = x), robust
ml search
ml maximize
estimates store rmlerobust

* (E) Calculate R-squared and log-likelihood at the ML estimates
* lnL sums lnf(y) = ln(lamda) - y*lamda
gen lamdaml = exp(_b[_cons] + _b[x]*x)
gen lnfml = ln(lamdaml) - y*lamdaml
quietly means lnfml
scalar LLml = r(mean)*r(N)
* R-squared = 1 - Sum_i(y_i - yhat_i)^2 / Sum_i(y_i - ybar)^2
gen yhatml = 1/lamdaml
egen ybar = mean(y)
* quietly means y
* scalar ybar = r(mean)
gen y_yhatsqml = (y - yhatml)^2
gen y_ybarsq = (y - ybar)^2
quietly means y_yhatsqml
scalar SSresidml = r(mean)
quietly means y_ybarsq
scalar SStotal = r(mean)
scalar Rsqml = 1 - SSresidml/SStotal
di LLml "    " Rsqml

********** DISPLAY RESULTS: First two columns of Table 5.7 p.161

* (1) OLS - nonrobust and robust standard errors 
*     Here OLS is inconsistent. 
*     And expect sign reversal for slope as in true model mean E[y] = exp(-x'b)
estimates table rols rolsrobust, b(%10.4f) se(%10.4f) t stats(N ll r2) keep(_cons x)

* (2) MLE by command ereg - nonrobust and robust standard errors 
estimates table rexp rexprobust, b(%10.4f) se(%10.4f) t stats(N ll) keep(_cons x)

* (3) MLE by command ml - nonrobust and robust standard errors 
estimates table rmle rmlerobust, b(%10.4f) se(%10.4f) t stats(N ll) keep(_cons x)
* And ML log-likelihood (check) and R-squared (needed to be computed)
di "Log likeihood for ML: " LLml
di "R-squared for MLE:    " Rsqml

********** CLOSE OUTPUT **********
log close
clear
exit
