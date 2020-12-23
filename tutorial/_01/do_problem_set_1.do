****************************************************************************** 
* Econometrics II, Summer 2020

* Do-file for Problem-Set 1
* Introduction to Stata
* Date: 05.05.2019

* fabian.dehos@rwi-essen.de
****************************************************************************** 

clear 
set more off

 
set type double
******************************************************************************
* Precision of numeric storage types:
* Numbers are stored as byte, int, long, float, double, with default being float.

* floats have  about  7  digits  of  accuracy.
* the  magnitude  of  the  number  does  not  matter. 
 
* Thus, 1234567 can be stored perfectly as afloat, as can 1234567e+20.  
* The number 123456789, however,would be rounded to 123456792.  
* In general, this rounding does not matter.

* If you are storing identification numbers, the rounding could matter.  
* If the identification numbers are integers and take 9 digits or less, 
* store them as longs; otherwise, store them as doubles.
* doubleshave 16 digits of accuracy.
******************************************************************************

* all open log-files will be closed
capture log close

* Working directory anzeigen lassen
cd

* Define the path where you want to store your log-file:
log using "log_problem_set_1.log", replace

* Open the data-set:
use "data\nls80.dta" , clear


*****************************************************
* Ex. a) 
*****************************************************

* Describes the variables and the 
describe
browse
*-> have a look at the variables-window
*-> have a look at the properties-window

* Proportion of individuals on board who survived:
sum  hours
sum  hours, detail
tab  hours

tab married
tab black
tab married black

sum wage if black==1
sum wage if black==0

*****************************************************
* Ex. b) 
*****************************************************
sum wage
sum wage, detail

* Histogram
histogram wage, frequency kdensity

* expected wage of a working man if he is married
sum wage if married==1 



*****************************************************
* Ex. c) 
*****************************************************

centile iq, level(50)
* return list
* display r(c_1)
local median=r(c_1)
 
display `median'
         
* High IQ dummy generieren:
gen 	high_iq=0
replace high_iq=1 if iq>`median'

*******************************************
* low education
gen low_edu= (educ<13)  

* medium education
gen medium_edu= (educ==13)  

* high education
gen high_edu= (educ>13)  

* Intepretation of beta_1
* Holding all other factors constant (ceteris paribus),...
* beta measures the difference in average earnings between married and unmarried.


* Interpretation of a constant
* The constant is the expected mean value of Y when all X=0.
* In this case: mean wage of a men who is unmarried, with low IQ & low education. 

*****************************************************
* Ex. d) 
*****************************************************
reg wage married high_iq  low_edu medium_edu high_edu, vce(robust)

* vce(robust): Why robust standard errors: Because of heteroskedasticity! 

* vce(cluster variable): clustered standard errors- 
* ... if error term is correlated for groups of observations or within clusters  

*****************************************************
* Ex. e) 
*****************************************************
reg wage married high_iq  
reg wage married high_iq  c.married#c.high_iq  
reg wage married##high_iq   

* What is the marginal effect of married?

*****************************************************
* Ex. f) 
*****************************************************
reg lwage educ married

*****************************************************
* Ex. g) 
*****************************************************
gen ln_educ=ln(educ)

reg lwage ln_educ married

*****************************************************
* Ex. h) 
*****************************************************
reg wage ln_educ married


*************************************************
* end session
*************************************************
log close
exit, clear

