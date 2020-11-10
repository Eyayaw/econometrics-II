****************************************************************************** 
* Econometrics II, Summer 2020

* Do-file for Problem-Set 7
* Date: 07.07.2020

* fabian.dehos@rwi-essen.de
****************************************************************************** 


clear 
set more off

 
set type double

* all open log-files will be closed
capture log close

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
* or
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

* see discussion 
 
*-------------------------------------------------------------------------------
* Ex 4(c):  Probit/Logit
*-------------------------------------------------------------------------------
probit  affair yrsmarr relig ratemarr, vce(robust)
logit  	affair yrsmarr relig ratemarr, vce(robust)

* -> Coefficients cannot be interpreted, just their signs.
* Estimated coefficients are parameters of the latent model.


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
 
* see discussion 
 

*-------------------------------------------------------------------------------
* Ex 4(e):  Post-estimation using margins command
*-------------------------------------------------------------------------------
 
 
* MEM: marginal effect at the mean 
probit  affair yrsmarr relig ratemarr, vce(robust)

margins, dydx(yrsmarr) atmeans

* AME: sampel average of the margianl effects  
margins, dydx(yrsmarr)  
 

 
 
 
 
 
*************************************************
* end session
*************************************************
log close
exit, clear
