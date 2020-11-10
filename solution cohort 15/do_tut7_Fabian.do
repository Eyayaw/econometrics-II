****************************************************************************** 
* Econometrics II, Summer 2019

* Do-file for Problem-Set 7
* Date: 02.07.2019

* fabian.dehos@rwi-essen.de
****************************************************************************** 


clear 
set more off

 
set type double

* all open log-files will be closed
capture log close
cd "J:\Microeconometrics\Tutorials\7\Task4"

* Define the path where you want to store your log-file:
log using "log_problem_set_7.log", replace


******************************************************************************
* Open the data-set:
use "affairs.dta" , clear
******************************************************************************
	
	
*-------------------------------------------------------------------------------
* Ex 4(a): Linear Probability Model
*-------------------------------------------------------------------------------

reg affair yrsmarr relig ratemarr, vce(robust)

* One additional year of marriage increases the probability of an affair...
* ... by 0.9 percentage points holding all other factors constant.

*********************
matrix list e(b)


/*
At yrsmarr= 1 and relig= 4,
estimated difference in the probability of having an affair for someone who is
... very happy married (ratemarr==5)
compared to someone who is 
... very unhappy married (ratemarr==1) ?
*/
display (_b[_cons]+ _b[yrsmarr]*1 + _b[relig]*4 +_b[ratemarr]*5) -(_b[_cons] + _b[yrsmarr]*1 + _b[relig]*4 +_b[ratemarr]*1)
* same as since all other things are constant
display (_b[ratemarr]*5) -( _b[ratemarr]*1)
* -> approx 35 p.p. lower probability to have an affair.


********************** 
 
 * -> verry religous  	(relig==5) 
 * -> very happy married 	(ratemarr==5)
 * -> married for one year (yrsmarr=1)
 display (_b[_cons]+_b[yrsmarr]*1 + _b[relig]*5 +_b[ratemarr]*5)
 

 
*-------------------------------------------------------------------------------
* Ex 4(b): advantages and disadvantages of the LPM
*-------------------------------------------------------------------------------

* Cons / disadvantages of a linear probability model (slide 9)

* a) Theory states that the probability of an event must be contained 
*    within the interval [0,1]
*	 However, within an LPM predicted probabilities may be larger than one or smaller 
*    than zero. -> see last subquestion of c.

* b) The variance of an LPM error term depends on the value of the independent variable(s).
* 	 Thus, the error term in an LPM is heteroskedastic.


* Pros/ advantages of a linear probability model
* a) easy estimation and interpretation
* b) in practice: estimated effects and predictions often reasonably good

 
*-------------------------------------------------------------------------------
* Ex 4(c):  Probit/Logit
*-------------------------------------------------------------------------------
probit  affair yrsmarr relig ratemarr, vce(robust)
logit  	affair yrsmarr relig ratemarr, vce(robust)

* -> Coefficients cannot be interpreted, just their signs.


*-------------------------------------------------------------------------------
* Ex 4(d): Post-estimation calculation
*-------------------------------------------------------------------------------

probit  affair yrsmarr relig ratemarr, vce(robust)

* Prob affair -> very happy married 	(ratemarr==5)
* ... given s.o. is verry religous (relig==4) & married for one year (yrsmarr=1)
 
 local affair_happy= 	_b[_cons]+_b[yrsmarr]*1 + _b[relig]*4 +_b[ratemarr]*5

 
* Prob affair -> very unhappy married 	(ratemarr==1)
* ... given s.o. is verry religous (relig==4) & married for one year (yrsmarr=1)
 local affair_unhappy= 	_b[_cons]+_b[yrsmarr]*1 + _b[relig]*4 +_b[ratemarr]*1

 
*Difference
display normal(`affair_happy')-normal(`affair_unhappy')



*********************************************************
logit  affair yrsmarr relig ratemarr, vce(robust)

* Logistic Distribution: Pr(y=1|x)= exp(xß)/(1*exp(xß))

* Prob affair -> very happy married 	(ratemarr==5)
* ... given s.o. is verry religous (relig==4) & married for one year (yrsmarr=1)
 
 local affair_happy= 	_b[_cons]+_b[yrsmarr]*1 + _b[relig]*4 +_b[ratemarr]*5

 
* Prob affair -> very unhappy married 	(ratemarr==1)
* ... given s.o. is verry religous (relig==4) & married for one year (yrsmarr=1)
 local affair_unhappy= 	_b[_cons]+_b[yrsmarr]*1 + _b[relig]*4 +_b[ratemarr]*1

 
*Difference
display   (exp(`affair_happy')/( 1+exp(`affair_happy'))) ///
			- (exp(`affair_unhappy')/( 1+exp(`affair_unhappy')))

*-------------------------------------------------------------------------------
* Ex 4(e):  Probit/Logit Pros/Cons
*-------------------------------------------------------------------------------
 
* Pros of a probit model
* a) Estimated response probabilities are strictly between zero and one.
 
 * Cons of a probit model (nonlinear binary response model)

* a) Partial effects depend on the level of x. Thus, regression outputs do not 
*    directly reveal the marginal effect.
* 	 However, partial effect of x always has the same sign as the beta-coefficient.

* b) Stricter funcitional form assumptions. Probit: standard normal assumption. 


*-------------------------------------------------------------------------------
* Ex 4(f):  Post-estimation using margins command
*-------------------------------------------------------------------------------
 
 
* MEM: marginal effect of the mean 
probit  affair yrsmarr relig ratemarr, vce(robust)

margins, dydx(yrsmarr) atmeans

* AME: sampel average of the margianl effects  
margins, dydx(yrsmarr)  
 

 
 
 
 
 
*************************************************
* end session
*************************************************
log close
exit, clear
