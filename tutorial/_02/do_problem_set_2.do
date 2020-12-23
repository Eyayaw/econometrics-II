****************************************************************************** 
* Econometrics II, Summer 2019

* Do-file for Problem-Set 2
* Get familiar with Stata
* Date: 07.05.2019

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
log using "log_problem_set_2.log", replace

* Open the data-set:
use "cps78_85.dta" , clear


* Using the "y85\-dummy" restrict the data set to the year 1985
keep if y85==1


*****************************************************
* Ex. a) 
*****************************************************

* Estimate the model by OLS
reg lwage female union nonwhite educ exper expersq, vce(robust)

* Where does the identifying variation come from?
* -> across individuals


* Could you account for time constant unobservables if an individual 
* identifier was available? -> no, because this is just a cross section.


* How does the coeficient on union-membership interpret?
* -> Ceteris paribus, union-members earn 21 percent more wage compared to 
*    non-union members.

* Assume that you have a panel data set with individual identifiers available 
* and that you want to estimate the impact of union-membership using an 
* individual FE-approach. What kind of variation do you need to identify 
* the impact of union-membership? -> Individuals changing unionship-status.



*****************************************************
* Ex. b) 
*****************************************************

* What is the marginal effect of experience?
* -> beta_5 + 2* beta_6 * experience
* -> 0.035  - 2* 0.0005 * experience

* At which level of experience is lwage maximized in this model?
* -> 0.035  - 2* 0.0005 * experience =0
*    0.035  =   2* 0.0005 * experience 
*    experience= 32.66

* Plot the experience-earnings profile.

reg lwage female union nonwhite educ exper expersq, vce(robust)

* generate a variable that contains the predicted wage-return given the individual
* level of experience
gen pred_return_to_exp =_b[exper]*exper + _b[expersq] *exper^2
twoway (qfit   pred_return_to_exp exper)

*****************************************************
* Ex. c) 
*****************************************************

/* In this data set the variable exper is potential experience, i.e. it is 
defined as age minus educ. Can we include age as an additional regressor in the 
above model? Explain and show what Stata does.
*/

* -> if everything was right coded there should be perfect multicollinearity.

*****************************************************
* Ex. c) 
*****************************************************

* Interaction of education and experience
* -> generate the respective interaction
gen educ_exper= educ*exper
reg lwage female union nonwhite educ_exper educ exper expersq, vce(robust)

*************************************************
* end session
*************************************************
log close
exit, clear

