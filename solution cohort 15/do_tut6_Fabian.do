****************************************************************************** 
* Econometrics II, Summer 2019

* Do-file for Problem-Set 6
* Date: 11.06.2019

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
	
		
*-------------------------------------------------------------------------------
* Ex 2(a) Graph: Age Profile of Drinking Participation
*-------------------------------------------------------------------------------

preserve

* Define the size of an age bin
* Anzahl der Tage 30
local bin_size 60  


gen 	 bin_num	= int(days_21/`bin_size') 	if days_21>=0
replace  bin_num	= int(days_21/`bin_size')-1 if days_21<0

gen approx_days_21=(bin_num* `bin_size')+(`bin_size'/2)
  
 

* Define in the local the outcome you want to anaylze: 
* non_alc or alc
local out drinks_alcohol

local age approx_days_21
    
collapse (mean) `out'    , by(`age')


 

* Month bins
twoway 	(scatter `out' `age' if  `age'>-1095 & `age'<1095   , mcolor(gray)   legend(order( 4 "21{superscript:st} birthday" 1 "age bin consisting out of `bin_size' days" )) ///
		, ytitle(avg. prob. of drinking alcohol)  xtitle({it:days relative to 21{superscript:st} birthday }) xscale(range(-1095 1095)) xlabel(-1095[320]1095  ) yscale(range(0 1)) ylabel(0[0.1]1 )  ///
		 title(Age Profile of Drinking Participation))   ///
		 (qfit  `out' `age' if `age'<1095  & `age'>=0)  ///
		 (qfit  `out' `age' if `age'>-1095 & `age'< 0)  ///
		(pci 0 0 1 0  , lpattern(longdash)  lcolor(cranberry ))  
restore 

*-------------------------------------------------------------------------------
* Ex 2(b) Regression estimates: Drinking participation
*-------------------------------------------------------------------------------
gen 	treat21=1 if days_21>=0
replace treat21=0 if days_21<0

 				 
gen days_21_2 =(days_21^2)
gen days_21_3 =(days_21^3)
gen days_21_4 =(days_21^4)

gen days_21_T=(days_21*treat21)
gen days_21_2_T=(days_21_2*treat21)
gen days_21_3_T=(days_21_3*treat21)
gen days_21_4_T=(days_21_4*treat21)
 
  eststo clear 
eststo: reg drinks_alcohol treat21 days_21  days_21_T   if days_21>-1095 & days_21<1095 , vce(robust)
eststo: reg drinks_alcohol treat21 days_21  days_21_T  days_21_2 days_21_2_T  if days_21>-1095 & days_21<1095 , vce(robust)
eststo: reg drinks_alcohol treat21 days_21  days_21_T  days_21_2 days_21_2_T  days_21_3 days_21_3_T if days_21>-1095 & days_21<1095 , vce(robust)
eststo: reg drinks_alcohol treat21 days_21  days_21_T  days_21_2 days_21_2_T  days_21_3 days_21_3_T if days_21>-1095 & days_21<1095 , vce(robust)
eststo: reg drinks_alcohol treat21 days_21  days_21_T  days_21_2 days_21_2_T  days_21_3 days_21_3_T days_21_4 days_21_4_T if days_21>-1095 & days_21<1095 , vce(robust)



* Export Results to a text-table
 esttab using "ex_c", cells(b( star  fmt(4)) se( par fmt(4))) ///
 unstack mtitle("under 2 oz" "2-4 oz" "more than 9 oz" "all labs" ) ///
 replace  star(* 0.10 ** 0.05 *** 0.01)   ar2  label  drop(  days_21*    )  wide              


 * Latex
 esttab using "ex_c.tex" , append drop(  *days_21*  )  ////
 cells(b(star  fmt(4))  se(  par  fmt(4)))   star(* 0.10 ** 0.05 *** 0.01) ar2  ///
 booktabs label     noobs        ///
 mgroups(, pattern(1 0 1 0)       ///
 prefix(\multicolumn{@span}{c}{) suffix(})   ///
 span erepeat(\cmidrule(lr){@span}))         ///
 alignment(c c) page(dcolumn) nonumber

*-------------------------------------------------------------------------------
* Ex 2(c) Bandwith selection
*-------------------------------------------------------------------------------	

********************************************************************************
preserve 


* Define treatment variable as well as interactions for the polynomial
gen 	treat21=1 if days_21>=0
replace treat21=0 if days_21<0

				 
gen days_21_2 =(days_21^2)

gen days_21_T=(days_21*treat21)
gen days_21_2_T=(days_21_2*treat21)


* In the present question you are asked to reduce the bandwith in 30-day steps
* Thus, there are 36 increases at the left and right of the cut-off
local bin_size 30
gen 	 increase	= int(days_21/`bin_size')+1 	if days_21>=0
replace  increase	= int(days_21/`bin_size')-1 if days_21<0 

* Startmonat des oberen und unteren Limits wird festgelegt
local upper=0
local lower=0

tempname myresults 
postfile `myresults' increase intercept treatment  se_treatment using myresults.dta , replace
foreach i of num 1/36 {

local upper	=`upper'+1
local lower =`lower'-1
reg  drinks_alcohol treat21 days_21  days_21_T  days_21_2 days_21_2_T  if  increase>=`lower' & increase<=`upper' , vce(robust)


post `myresults' (`i') (`=_b[_cons]') (`=_b[treat21]') (`=_se[treat21]') 
}

postclose `myresults' 
use myresults , clear 
list


*----------------------------------------------------------------------
* construct  95% Confidence Interval:
gen upper_95=treat+se_treatment*1.96
gen lower_95=treat-se_treatment*1.96

gen  	bandwidth=(increase*30)

*


twoway 	(scatter treatment bandwidth, msymbol(S) color(black) /// 
		title( Bandwidth Selection, color(black)) ///
		subtitle( Increase of the bandwidth in 30-day steps on each side of the cut-off, color(black))) ///
		(line treatment bandwidth , color(black) yline(0, lcolor(edkblue) lwidth(medthin) ) ) 		///
		(line upper_95 bandwidth , lpattern(dash) color(black)) 		///
		(line lower_95 bandwidth , lpattern(dash) color(black)) 
		
*---------------------------------------------------------------------- 
restore
******************************************************************************** 
 
*-------------------------------------------------------------------------------
* Ex 2(d) Check for discontinuities in characteristics
*-------------------------------------------------------------------------------	

* Define treatment variable as well as interactions for the polynomial
gen 	treat21=1 if days_21>=0
replace treat21=0 if days_21<0

				 
gen days_21_2 =(days_21^2)

gen days_21_T=(days_21*treat21)
gen days_21_2_T=(days_21_2*treat21)

eststo clear 
eststo: reg male treat21 days_21  days_21_T  days_21_2 days_21_2_T  if days_21>-1095 & days_21<1095 , vce(robust)
eststo: reg uninsured  treat21 days_21  days_21_T  days_21_2 days_21_2_T  if days_21>-1095 & days_21<1095 , vce(robust)
eststo: reg hs_diploma  treat21 days_21  days_21_T  days_21_2 days_21_2_T  if days_21>-1095 & days_21<1095 , vce(robust)
eststo: reg hispanic  treat21 days_21  days_21_T  days_21_2 days_21_2_T  if days_21>-1095 & days_21<1095 , vce(robust)
eststo: reg white  treat21 days_21  days_21_T  days_21_2 days_21_2_T  if days_21>-1095 & days_21<1095 , vce(robust)
eststo: reg black  treat21 days_21  days_21_T  days_21_2 days_21_2_T  if days_21>-1095 & days_21<1095 , vce(robust)
eststo: reg employed  treat21 days_21  days_21_T  days_21_2 days_21_2_T  if days_21>-1095 & days_21<1095 , vce(robust)
eststo: reg married  treat21 days_21  days_21_T  days_21_2 days_21_2_T  if days_21>-1095 & days_21<1095 , vce(robust)
eststo: reg working_lw  treat21 days_21  days_21_T  days_21_2 days_21_2_T  if days_21>-1095 & days_21<1095 , vce(robust)
eststo: reg going_school  treat21 days_21  days_21_T  days_21_2 days_21_2_T  if days_21>-1095 & days_21<1095 , vce(robust)



* Export Results to a text-table
 esttab using "ex_c", cells(b( star  fmt(4)) se( par fmt(4))) ///
 replace  star(* 0.10 ** 0.05 *** 0.01)   ar2  label  drop(  days_21*    )  wide              
 
 
 * Latex
 esttab using "ex_c.tex" , append drop(  *days_21*  )  ////
 cells(b(star  fmt(4))  se(  par  fmt(4)))   star(* 0.10 ** 0.05 *** 0.01) ar2  ///
 booktabs label     noobs        ///
 mgroups(, pattern(1 0 1 0)       ///
 prefix(\multicolumn{@span}{c}{) suffix(})   ///
 span erepeat(\cmidrule(lr){@span}))         ///
 alignment(c c) page(dcolumn) nonumber

 
*************************************************
* end session
*************************************************
log close
exit, clear
