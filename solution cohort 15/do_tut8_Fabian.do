clear all			/* Reset memory, remove active data */
capture log close	/* In case no log is open */
set more off		/* tells Stata not to pause */
set matsize 4000	/* Expand maximum number of variables */
set type double 	/* Excel stores numeric values in double precision */

* Set working directory where dataset is located:
*Duisburg
*cd "C:\Users\Karolin Süß\sciebo\SoSe2019\Microeconometrics\Tutorial\6"

*Essen
cd "J:\Microeconometrics\Tutorials\8"
* ECONOMETRICS II - Problem Set 5 
* Karolin Süß
* June 19, 2019
use NHIS.dta, clear


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


/*
A histogram plots the empirical distribution of a variable where observations 
within certain equally sized intervals are grouped into bins. 
Due to this grouping, information on the distribution within the bins is lost 
and the actual distribution might be smoother than it appears.

-> Histogram is not continous.
 */


**************************************************
* (b) kernel density
**************************************************

* Def. histogram: It splits the range of a random variable x into equally
* spaced intervals containing the respective fraction of the sample.

* kernel density estimator generalizes the histogram density using a
* different weighting function

* In most applications, both the kernel function and bandwidth can be chosen
* optimally to minimize the mean squared error (MSE)



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


**********************************************************************************
* Kernel densities wrt drinking


twoway ( kdens days_21 if drinks_alcohol==1 ) ///
       ( kdens days_21 if drinks_alcohol==0) , legend(order(1 "Drink" 2 "Don't drink")) 

*************************************************************************************
* Epanechnikov
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
	   
	   
/*
* OPTIMAL bandwidth: 
Small values of h lead to very spiky estimates
while larger h values lead to oversmoothing.
*/

**********************************
* TAKE AWAY:
* The quality of a kernel estimate depends less on the shape of the K 
* than on the value of its bandwidth h.



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
clear 

******************************************************************************
* Open the data-set:
use "soep.dta" , clear
******************************************************************************

*************************************************************
* globale Definition der Kontrollvariablen + Missings loeschen
*************************************************************
# d ;
global X "age female german feduc2 feduc3 feduc4 feduc5 dummytwoparents";
# d cr
 
 
*******************************************
* (a)
*******************************************

* Regress the treatment indicator D on all control variables
logit D $X

* predict for each individual the respective propensity score
capture drop prop_score
predict prop_score 

* Calculate propsentiy score by hand
capture drop prop_scma
gen 	prop_scma		=_b[ _cons]+ _b[age]* age + _b[ female]*female + _b[ german]*german + _b[ feduc2]*feduc2 + _b[ feduc3]*feduc3 + _b[feduc4]*feduc4 + _b[feduc5]*feduc5 + _b[ dummytwoparents ]*dummytwoparents
replace prop_scma	= exp(prop_scma)/(1+exp(prop_scma))


* Common support

twoway ( hist prop_score if D==1,   color(red) disc freq)  ( hist prop_score if D==0,  color(green) disc freq ), legend(order(1 "D==1" 2 "D==0"))
			   
			   

* How many individuals on / off support
sum prop_score if D==1
local min_d1 r(min)
local max_d1 r(max)


sum prop_score if D==0
local min_d0 r(min)
local max_d0 r(max)


*** by hand: 

*On support
bysort D: sum Y if  (prop_score>=`min_d1' & prop_score>=`min_d0')  &  (prop_score<=`max_d1' & prop_score<=`max_d0')

*Off support
bysort D: sum Y if  (prop_score<=`min_d1' & prop_score<=`min_d0')  &  (prop_score>=`max_d1' & prop_score>=`max_d0')




********************************************
* (b) NN(1) matching with replacement
********************************************
psmatch2 D $X, outcome(Y) logit common neighbor(1)   


* more neigbhours: tradeoff btw. bias <-> precision

********************************************
* (c) Common support
********************************************
 psgraph

* Individuals on/off support 
bysort D: sum Y if _support==1
bysort D: sum Y if _support==0


twoway (hist _pscore if D==1)(hist _pscore if D==0)


*** by hand
bysort D: sum Y
sum Y _Y if D==1 & _support==1

*************************************************
* end session
*************************************************
log close
exit, clear


 