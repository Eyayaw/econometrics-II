****************************************************************************** 
* Econometrics II, Summer 202ß

* Do-file for Problem-Set 6
* Date: 30.06.2019

* fabian.dehos@rwi-essen.de
****************************************************************************** 

clear 
set more off

 
set type double

* all open log-files will be closed
capture log close

* Working directory anzeigen lassen
cd

* Define the path where you want to store your log-file:
log using "log_problem_set_5.log", replace

* Open the data-set:
use "BCHHS_data.dta" , clear



*****************************************************
* Ex. 2 (a) 
*****************************************************

*generating log earnings
gen lnearn=ln(earning)

*generating age squared
gen agesq=age*age

*******************************************************************************
* Table 2, Column 2: pooled OSL 
* regressing all for whom we have complete wage information (428 individuals)
* contols: schooling, age, age_2

* OSL
reg lnearn highqua age agesq, vce(robust)

*******************************************************************************

* Table 2, Column 3: IV estimate, same covariates as OLS
* idea behind IV? -> control for measurement error
ivregress  2sls  lnearn (highqua=twihigh) age agesq, vce(robust)



*****************************************************
* Ex. 2 (b) 
*****************************************************
* Interpretation of highqua-coefficient in OLS model:
* -> return to education of 7.7 percent

*****************************************************
* Ex. 2 (c) 
*****************************************************
* two concerns mentioned wrt. education:
* first, measurement bias
* second, omitted variables bias (ability)

*****************************************************
* Ex. 2 (d) 
*****************************************************
* proposed instrument:
* twihigh (twin’s estimated years of schooling)

* model of constant returns. Two conditions have to be fullfilled.

* 1. relevance condition: first stage exists
* Check:
reg   highqua twihigh age agesq, vce(robust)

* 2. exclusion restriction:
* instrument uncorrelated with error term of the structural equation 
* within the pooled model, instrument certainly correlated with error term...
* ... which includes for instance the unobserved family background.

*****************************************************
* Ex. 2 (e) 
*****************************************************
* Which of the previously mentioned concerns is addressed by the instrument?
* -> measurement error but not omitted variables bias.

*****************************************************
* Ex. 2 (f) 
*****************************************************
* Calculation of  standard errors in column (2) of table 2
* -> regular std. errors neither clustered nor robust

* Comparison of std. errors
* OSL
reg lnearn highqua age agesq 
reg lnearn highqua age agesq, vce(robust)
*-> no big difference

ivregress  2sls  lnearn (highqua=twihigh) age agesq 
ivregress  2sls  lnearn (highqua=twihigh) age agesq, vce(robust)

* -> comparison of standard errors OLS vs IV: 
* sniff test -> std. errrors of IV > OLS

*****************************************************
* Ex. 2 (f) 
*****************************************************
* reshape data set
reshape wide earning-agesq, i(family)j(twinno)

* generate first differences 
gen dlnearn=lnearn1-lnearn2
gen dhigh = highqua1-highqua2
gen dtwihi=twihigh1-twihigh2

* Do we need a constant for the first difference regression:

* first-difference OLS
reg dlnearn dhigh, noc

* first-difference IV
ivregress  2sls dlnearn (dhigh= dtwihi), noc
* -> But, first stage changes sign.


*****************************************************
* Ex. 2 (f) 
*****************************************************
* advantage of first differences 
* -> kills time invariant unobservables such as family background. 


*************************************************
* end session
*************************************************
log close
exit, clear

