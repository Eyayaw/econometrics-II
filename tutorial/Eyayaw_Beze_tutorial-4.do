local path "~/Documents/PhD Archive/semester-II/Econometrics-2/tutorial/"

cd "`path'"

set more 1
capture log close

log using "tutorial-04.log", replace

use otc_regulation.dta, clear 

encode state_ab, gen(state_name)
drop state_ab 
rename state_name state_ab 



// encode any_law and event_date in date format

gen anylaw  = date(any_law, "MDY")
format %tdDD-Mon-CCYY anylaw // e.g. 01-Jan-2020
drop any_law
rename anylaw any_law

gen ev_date  = date(event_date, "DMY")
format %tdDD-Mon-CCYY ev_date
drop event_date
rename ev_date event_date 


// remove missing data

list state_ab event_date any_law if event_date == . // there are missings for e.g. State FL

keep if !missing(any_law, event_date, tot_labs) // keep only not missing

di "`r(N_drop)' observations dropped"

* new var: for labs producing 9 and more oz of methamphetamine
gen cap_above_9_oz = tot_labs - (cap_under_2_oz + cap_2_8_oz)

* convert to monthly dates

gen any_law_my = mofd(any_law)
gen event_date_my = mofd(event_date)

format *_my %tm  // e.g. 2000m1

***** OTC indicator *****

* The variable otc is an indicator set to 0 in the months prior to implementation
* of a state's regulation, 1 afterwards, and the fraction of the month a regulation 
* was implemented in months a regulation was enacted.  If a state law was enacted 
* on the 15th of February of 2005 otc would be coded as (28−14)/28 = 0.5 during 
* that time period for that state, for example.

* The last day of the month is the day before the first day of the next month.

gen end_of_month = dofm(any_law_my + 1)-1

format end_of_month %td 

gen OTC = 0 if event_date_my < any_law_my
replace OTC = 1 if event_date_my > any_law_my
replace OTC = (1 - (day(event_date)-1)/day(end_of_month)) if event_date_my == any_law_my
label var OTC "OTC restriction" 

order state_ab /* relocate to the first column */
order *_my, after (event_date)

** US pop covered by the OTC law
gen tot_pop_covered = OTC * pop_all_fitted 

// we need this form of the dataset for later part of the exercise, so let's save it before collapsing.   

compress /* to save data as efficiently as possible */

save otc_regulation_cleaned.dta, replace // 

// collapse the dataset, by event_date

collapse (mean) cap_* (sum) pop_all_fitted tot_pop_covered , by (event_date)

** proportion of the US pop covered by the OTC law
gen pop_covered = tot_pop_covered/pop_all_fitted 


***********************************************************
* For plotting purpose
***********************************************************
// label the three lab capacity vars as in the paper

label var cap_under_2_oz "Lab capacity under 2 oz"
label var cap_2_8_oz "Lab capacity 2-8 oz"
label var cap_above_9_oz "Lab capacity 9 oz or more" 

// title note: line1 line 2
local l1 "Methamphetamine labs discovered or seized by capacity"
local l2 "The figure contains the average number of labs discovered in a state by month."
************************************************************


#delimit;
twoway 
(scatter cap_under_2_oz event_date, lp(dot) connect(l) color(blue) msymbol(t))
(scatter cap_2_8_oz event_date, lp(dash) connect(l) color(red) msymbol(o))
(scatter cap_above_9_oz event_date, lp(dash) connect(l) color(green) msymbol(sh)
ytitle("Average Number of Labs Siezed Per Month")
xtitle("Date")) 
(
line pop_covered event_date, yaxis(2) color(black) connect(L) msize(tiny)
ytitle("Proportion of Population Covered by Laws", axis(2)) 
tlabel(,format("%tdCCYY")) legend(cols(1) order(1 2 3)) 
ylabel(0 .5 1, axis(2))
title("`l1'") note( "`l2'"));
#delimit cr 


graph export "lab-seized-timeseries.png", replace 

/* *******************************************************
   -----------------(b)------------- 
   Plot Months From (Until) Implementation of Law 
   vs 
   Number of labs seized per month
   ******************************************************* */

/* re-import the cleaned data, since it's lost its true form due to collapsed it. */

use otc_regulation_cleaned.dta, clear 
keep state_ab any_* event_* cap_* OTC tot_pop_covered 

// **** Months From (Until) Implementation of Law ******** //

gen months_from_until_otc =  event_date_my - any_law_my

// collapse the dataset, by event time (i.e., months_from_until_otc)

keep if months_from_until_otc >= -12 & months_from_until_otc <= 24

collapse (mean) cap_*, by (months_from_until_otc)

label var cap_under_2_oz "Lab capacity under 2 oz"
label var cap_2_8_oz "Lab capacity 2-8 oz"
label var cap_above_9_oz "Lab capacity 9 oz or more" 

twoway scatter cap_under_2_oz cap_2_8_oz ///
	cap_above_9_oz months_from_until_otc , /// 
	connect(l l l) ///                       
	msymbol(t o sh) ///
	mcolor(blue red dkgreen) ///
	lp(dot dash dash) ///
	ytitle("Average Number of Labs Siezed Per Month") ///
	xtitle("Months From (Until) Implementation of Law") ///
	legend(cols(1) order(1 2 3)) title("`l1'") note( "`l2'") ///
	xlabel(-12(6)24) xline(0, lcolor(cranberry) lwidth(.75))

graph export "month_from_until_otc-vs-lab_size.png", replace 


/* *******************************************************
   (c)   
   Estimate a standard FE model

   ******************************************************* */

/* re-import the cleaned data */

clear 
set matsize 2000
use otc_regulation_cleaned.dta

xtset state_ab event_date // declare dataset as panel

// **** Months From (Until) Implementation of Law ******** //

gen months_from_until_otc =  event_date_my - any_law_my

label var tot_labs "Number of Labs Seized" 
label var cap_under_2_oz "Lab capacity under 2 oz"
label var cap_2_8_oz "Lab capacity 2-8 oz"
label var cap_above_9_oz "Lab capacity 9 oz or more" 

gen event_date_my_sq  = c.event_date_my # c.event_date_my // quadratic trend in event time

/* define local macros */ 
local dvs tot_labs cap_under_2_oz cap_2_8_oz cap_above_9_oz // dep variables
// sequential models : 

local col1 OTC i.event_date_my
local col2 "`col1' state_ab#c.event_date_my" // adds state-specific linear time trends
local col3 "`col2' state_ab#(c.event_date_my_sq)" // adds quadratic state-specific time trends, and
local col4 "`col3' cov_food_st_person cov_unemp_rate" // the final column adds covariates.

local controls " "`col1'" "`col2'" "`col3'" "`col4'" "


#delimit ;
foreach dv in  `dvs'  
{;

	foreach col in `controls'
	{;
		eststo: quie xtreg `dv' `col', fe vce(cluster state_ab);
		estadd ysumm ,  mean ; // return only mean
		di "model = `e(cmdline)'"; // display model equation
	};	

	local dvlabel: var label `dv'; // get variable label

	esttab using tutorial-4-equation-1-`dv'.tex,  keep (OTC) replace
	nostar se nonumber 
	stats(ymean N N_g, labels("Mean" "Observations" "States"))
	title(`dvlabel') mtitle("(1)" "(2)" "(3)" "(4)");
	eststo clear;
};
#delimit cr 


/* *******************************************************
   (f)  
   Run a regression of the number of labs dis-covered on state FE, 
   time dummies and event time dummies. Plot the estimates of the
   event time dummies and their confidence intervals. I

   ******************************************************* */

gen int tau_st = 0
replace tau_st = months_from_until_otc if months_from_until_otc >= -12 & months_from_until_otc <= 24

label var tau_st "month relative to introduction of an OTC regulation"

egen J = group(tau_st) /* tau_st has negative values, Stata does not allow 
   negative values to be used as factor vars.	*/

// alternatively, tab tau_st, gen(j_) // then egen J = group(j_*)

/* In the following loop, we substract event dummies cofficients from the coefficient of dummy for tau_st = -1;
   the month before an OTC restriction was put into place
*/
#delimit ; 

local dvs tot_labs cap_under_2_oz cap_2_8_oz cap_above_9_oz;
foreach dv of local dvs  
{;
	qui xtreg `dv' ib(13).J state_ab#c.event_date_my 
	if months_from_until_otc > -13, fe;

	gen pi_j_`dv' = . ;

	forvalues j = 1/37 
	{;
		qui replace pi_j_`dv' =  _b[`j'.J] - _b[12.J]
		if months_from_until_otc >-13 & months_from_until_otc == `j'-13;
	};

	label variable pi_j_`dv' "event time coefficients, `dv'";
};
#delimit cr 


twoway scatter pi_j_cap* tau_st, ///                        
	connect(L L L) ///
	msymbol(t o sh) ///
	mcolor(blue red dkgreen) ///
	lp(dot dash dash) ///
	ytitle("Number of Labs Siezed Per Month") ///
	xtitle("Months From (Until) Implementation of Law") ///
	xlabel(-12(6)24) ///
	ylabel(-10(5)5) ///
	legend(cols(1)) ///
	xline(0, lcolor(cranberry) lwidth(.75))

foreach var of varlist pi_j_cap* {
	reg `var' tau_st
}                                

log close 
