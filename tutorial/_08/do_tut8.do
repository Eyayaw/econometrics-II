****************************************************************************** 
* Econometrics II, Summer 2020

* Do-file for Problem-Set 8
* Date: 15.07.2020

* fabian.dehos@rwi-essen.de
****************************************************************************** 


clear 
set more off

 
set type double

* all open log-files will be closed
capture log close

* Define the path where you want to store your log-file:
log using "log_problem_set_6.log", replace


******************************************************************************
* Open the data-set:
use "NHIS.dta" , clear
******************************************************************************

***************************************	
* Ex.1  Non-Parametric Methods
***************************************

**************************************************
* (a) histogram days since 21st birthday
**************************************************

hist days_21    

* define the width of the bin
hist days_21, width(4)

* define the number of bins
hist days_21, bin(3000)  

* -> But: options bin() and width() may not be combined



**************************************************
* (b) kernel density
**************************************************



*  epan2 (the default)
kdens days_21


**********************************************************************************
* Epanechnikov
kdens days_21, kernel(epanechnikov) title( "epanechnikov")  saving(epanechnikov, replace) 

* Gaussian
kdens days_21, kernel(gaussian) title( "gaussian")  saving(gaussian, replace)

* Triangle
kdens days_21, kernel(triangle) title( "triangle") saving(triangle, replace)
* Cosine
kdens days_21, kernel(cosine) title( "cosine") saving(cosine, replace)
 
gr combine epanechnikov.gph gaussian.gph cosine.gph triangle.gph, title( "Different kernels")  

**********************************************************************************


* Different bandwiths
* the following local can be changed  
local bandwidth 10

* Epanechnikov
kdens days_21,  bw(`bandwidth') kernel(epanechnikov) title( "epanechnikov")  saving(epanechnikov, replace) 

* Gaussian
kdens days_21, bw(`bandwidth') kernel(gaussian) title( "gaussian")  saving(gaussian, replace)

* Triangle
kdens days_21, bw(`bandwidth') kernel(triangle) title( "triangle") saving(triangle, replace)
* Cosine
kdens days_21, bw(`bandwidth') kernel(cosine) title( "cosine") saving(cosine, replace)
 
gr combine epanechnikov.gph gaussian.gph cosine.gph triangle.gph, title( "Different kernels")  

*************************************************************************************
**********************************************************************************
* Kernel densities wrt drinking


twoway ( kdens days_21 if drinks_alcohol==1 ) ///
       ( kdens days_21 if drinks_alcohol==0) , legend(order(1 "Drink" 2 "Don't drink")) 


* the following local can be changed and adjusted
local bandwidth 100

* Epanechnikov
twoway 	( kdens days_21 if drinks_alcohol==1,  bw(`bandwidth') kernel(epanechnikov) ) ///
		( kdens days_21 if drinks_alcohol==0,  bw(`bandwidth') kernel(epanechnikov) ) ///
		, legend(order(1 "Drink" 2 "Don't drink")) title( "epanechnikov")  saving(epanechnikov, replace) 

		
 
* Gaussian
twoway 	( kdens days_21 if drinks_alcohol==1,  bw(`bandwidth') kernel(gaussian) ) ///
		( kdens days_21 if drinks_alcohol==0,  bw(`bandwidth') kernel(gaussian) ) ///
		, legend(order(1 "Drink" 2 "Don't drink")) title( "gaussian")  saving(gaussian, replace) 

	
* Triangle
twoway 	( kdens days_21 if drinks_alcohol==1,  bw(`bandwidth') kernel(triangle) ) ///
		( kdens days_21 if drinks_alcohol==0,  bw(`bandwidth') kernel(triangle) ) ///
		, legend(order(1 "Drink" 2 "Don't drink")) title( "triangle")  saving(triangle, replace) 


* Cosine
kdens days_21, bw(`bandwidth') kernel(cosine) title( "cosine") saving(cosine, replace)

twoway 	( kdens days_21 if drinks_alcohol==1,  bw(`bandwidth') kernel(cosine) ) ///
		( kdens days_21 if drinks_alcohol==0,  bw(`bandwidth') kernel(cosine) ) ///
		, legend(order(1 "Drink" 2 "Don't drink")) title( "cosine")  saving(cosine, replace) 
 
 
 
gr combine epanechnikov.gph gaussian.gph cosine.gph triangle.gph, title( "Different kernels")  
	   



	   
**************************************************
* (c) For age bins of 30 days plot the means of drinks alcohol
**************************************************



preserve

* Define the size of an age bin
* Anzahl der Tage 30
local bin_size 30  


gen 	 bin_num	= int(days_21/`bin_size') 	if days_21>=0
replace  bin_num	= int(days_21/`bin_size')-1 if days_21<0

gen approx_days_21=(bin_num* `bin_size')+(`bin_size'/2)
  
 

* Define in the local the outcome you want to anaylze: 
* non_alc or alc
local out drinks_alcohol

local age approx_days_21
    
collapse (mean) `out'    , by(`age')

/*
The relationship between age and alcohl consumption is not straightforward
as it could either be a quadratic, a linear heteroskedastic, or even a
non-linear relationship
*/ 

* Month bins
twoway 	(scatter `out' `age' if  `age'>-1095 & `age'<1095  )||  ///  
		(lfit `out' `age' if  `age'<0 & `age'>-1095  , lcolor(edkblue)  ) ||  ///
		(lfit `out' `age' if  `age'<1095 & `age'>=0   , lcolor(edkblue)  ) ||  ///  
		(lpoly `out' `age' if  `age'<0 & `age'>-1095  , lcolor(red) kernel(triangle ) degree(2)  ) ||  ///
		(lpoly `out' `age' if  `age'<1095 & `age'>=0  , lcolor(red) kernel(triangle ) degree(2) )


restore 


**************************************************
* (d) rdrobust
**************************************************
rdrobust drinks_alcohol days_21 if  days_21<1095  & days_21>-1095   , c(0) p(1)   kernel(epanechnikov)

* Compare estimation with graphs as well as with results from tut. 6.
*-------------------------------------------------------------------------------

 
 
 
**************************************************************
* Ex.1  Selection on observables: Propensity score matching
**************************************************************

**************************************************
* (a) histogram days since 21st birthday
**************************************************
clear all

******************************************************************************
* Open the data-set:
use "soep.dta" , clear
 
******************************************************************************

*************************************************************
* globale Definition der Kontrollvariablen  
*************************************************************

global X "age female german feduc2 feduc3 feduc4 feduc5 dummytwoparents"

egen varmiss=rowmiss(Y D $X)
drop if varmiss>0
 
*******************************************
* (a)
*******************************************

* Regress the treatment indicator D on all control variables
logit D $X

* predict for each individual the respective propensity score
capture drop prop_score
predict prop_score 



* Common support: graph
twoway ( hist prop_score if D==1,   color(red%90) disc freq)  ( hist prop_score if D==0,  color(green%30) disc freq ), legend(order(1 "D==1" 2 "D==0"))
			   
	
* How many individuals on / off support
sum prop_score if D==1
local min_d1=r(min)
local max_d1=r(max)


sum prop_score if D==0
local min_d0=r(min)
local max_d0=r(max)

*Off support
count if (( prop_score>`max_d0' |  prop_score>`max_d1') |  ( prop_score<`min_d1' | prop_score<`min_d0' )) 
* -> in total there are 78 observations off the common support


* Off support 
* Destinguished by treatment status
bysort D: sum Y if  (( prop_score>`max_d0' |  prop_score>`max_d1') |  ( prop_score<`min_d1' | prop_score<`min_d0' ))  
* -> 1 observations off the common support  within the treatment group (if D==1)
* -> 77 observations off the common support within the control group (if D==0)



* On support
* Destinguished by treatment status
bysort D: sum Y if  (( prop_score<=`max_d0' &  prop_score<=`max_d1') &  ( prop_score>=`min_d1' & prop_score>=`min_d0' ))  

psmatch2 D $X, outcome(Y) logit common neighbor(1) ate




********************************************
* (b) NN(1) matching with replacement
********************************************
psmatch2 D $X, outcome(Y) logit common neighbor(1) 

*->  tradeoff between variance and bias:
* (many neighbours per treated) reduces variance but increases bias (poorer matches used)

* Which effect do you estimate? -> ATT (average treatment effect of the treated) 

********************************************
* (c) Common support
********************************************

 psgraph ate
 psgraph, bin(150)
* you can change the number of bins (increase or decrease it)

********************************************
* (d) 
********************************************

psmatch2 D $X, outcome(Y) logit common neighbor(1) 
* -> there are difference wrt to the row with the untreated.
* -> Why? Because psmatch2 refers by default to the ATT 


* Compare this to the following command:
psmatch2 D $X, outcome(Y) logit common neighbor(1)  ate
* -> which 2*2 matrix do you obtain now? should be the same as in a)


********************************************
* (e) nearest neighbor matching
********************************************

* disadvantages: Main weakness of NN matching: closest neighbour might be far away.

* Caliper matching imposes tolerance level on maximum PS distance (caliper).
* Bad matches are avoided and matching quality rises.

* Downside: fewer matches used ⇒ variance increases.
* Problem: Difficult to know a priori what the appropriate tolerance level is.

* Radius matching uses all comparison members within the caliper.
* Thus, the number of matches used depends on the quality of matches.
* Hence, it combines advantages of oversampling and caliper.


********************************************
* (f) matching assumption
********************************************

* matching based on observables. If there are further unobservables that
* might play a role, you do not identify the causal effect.

 