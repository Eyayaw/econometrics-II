local path "~/insert-path-here"

cd "`path'"

set more 1
capture log close

log using "tutorial-4.log", replace

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

drop if event_date == . // keep only not missing

di "`r(N_drop)' observations dropped"

* new var: for labs producing 9 and more oz of methamphetamine
gen cap_above_9_oz = tot_labs - (cap_under_2_oz + cap_2_8_oz)


* convert to monthly dates

gen any_law_my = mofd(any_law)
gen event_date_my = mofd(event_date)

format *_my %tmCCYY-Mon  // e.g. 2020-M4

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

// we need the data in this form for later part of the exercise, so let's save it before collapsing.   

compress /* to save data as efficiently as possible */

save otc_regulation_cleaned.dta, replace // 

/* collapse the dataset, by event_date to compute monthly average lab seizures 
   and total US population and total US population covered by the law */

collapse (mean) cap_* (sum) pop_all_fitted tot_pop_covered , by (event_date)

** proportion of the US pop covered by the OTC law
gen pop_covered = tot_pop_covered/pop_all_fitted 


/***********************************************************
   For plotting purpose
   ***********************************************************/
// label the three lab capacity vars as in the paper

label var cap_under_2_oz "Lab capacity under 2 oz"
label var cap_2_8_oz "Lab capacity 2-8 oz"
label var cap_above_9_oz "Lab capacity 9 oz or more" 

// title note: line1 line 2
* local l1 "Methamphetamine labs discovered or seized by capacity"
* local l2 "The figure contains the average number of labs discovered in a state by month."
************************************************************



twoway (scatter cap_* event_date, yaxis(1) connect(l l l) msymbol(t o sh) /// 
	mcolor(blue red dkgreen) lp(dot dash dash) ///
	ytitle("Average Number of Labs Siezed Per Month") ///
	xtitle("Date")) ///
	line pop_covered event_date, yaxis(2) /// 
	color(black) connect(L) /// 
	ytitle("Proportion of Population Covered by Laws", axis(2)) ///
	tlabel(,format("%tdCCYY")) ylabel(0 0.5 1, axis(2)) /// 
	legend(cols(1) order(1 2 3) ) /// // title("`l1'") note( "`l2'")
	scheme(s1mono) ///
	graphregion(fcolor(white) lcolor(white) lwidth(none) ifcolor(white) ilcolor(white) margin(zero)) ///					///
	plotregion(fcolor(white) lcolor(white) ifcolor(white) ilcolor(white)) 

graph export "number_of_labs_siezed-timeseries.eps",  as(eps) preview(on) replace 

/* *******************************************************
   -----------------(b)------------- 
   Plot Months From (Until) Implementation of Law 
   vs 
   Number of labs seized per month
   ******************************************************* */

/* re-import the cleaned data, since it's lost its true form due to collapsing. */

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

twoway scatter cap_* months_from_until_otc , /// 
	connect(l l l) msymbol(t o sh) mcolor(blue red dkgreen) lp(dot dash dash) ///
	ytitle("Average Number of Labs Siezed Per Month") ///
	xtitle("Months From (Until) Implementation of Law") ///
	legend(pos(0) bplacement(neast) cols(1) order(1 2 3) region(lcolor(white))) /// // title("`l1'") note( "`l2'")
	xlabel(-12(6)24) xline(0, lcolor(cranberry) lwidth(.75)) ///
	scheme(s1mono) ///
	graphregion(fcolor(white) lcolor(white) lwidth(none) ifcolor(white) ilcolor(white) margin(zero)) ///					///
	plotregion(fcolor(white) lcolor(white) ifcolor(white) ilcolor(white)) 

graph export "month_from_until_otc-vs-lab_size.eps", as(eps) preview(on) replace 


/* *******************************************************
   ------(c)-------  
   Estimate a standard FE model

   ******************************************************* */

/* re-import the cleaned data */

clear 
set emptycells drop // drop empty factor vars (created with i. or c. operators) if any in regression calls
set matsize 4000
use otc_regulation_cleaned.dta

xtset state_ab event_date_my // declare dataset as panel

// **** Months From (Until) Implementation of Law ******** //

gen months_from_until_otc =  event_date_my - any_law_my

label var months_from_until_otc "month relative to introduction of an OTC regulation"

label var tot_labs "Number of Labs Seized" 
label var cap_under_2_oz "Lab capacity under 2 oz"
label var cap_2_8_oz "Lab capacity 2-8 oz"
label var cap_above_9_oz "Lab capacity 9 oz or more" 

* let's create event time index so that instead of event_date_my = 2000m1, 2000m2, ... we have 1, 2, ...

by state_ab: gen state_time = _n

gen trend_sq  = c.state_time # c.state_time // quadratic trend in event time

/* define local macros */ 
local dvs tot_labs cap_under_2_oz cap_2_8_oz cap_above_9_oz // dep variables
// sequential models : 

local col1 OTC i.event_date_my
local col2 "`col1' state_time" // adds state-specific linear time trends
local col3 "`col2' state_ab#(c.trend_sq)" // adds quadratic state-specific time trends, and
local col4 "`col3' cov_food_st_person cov_unemp_rate" // the final column adds covariates.

local rhss " "`col1'" "`col2'" "`col3'" "`col4'" "


eststo clear 

local i = 1
foreach dv of local dvs {
	foreach col of local rhss {
		quiet xtreg `dv' `col', fe vce(cluster state_ab)		
		estimates store m_`i'_`dv'
		estadd ysumm ,  mean
		local ++i
		di "model = `e(cmdline)'" // display model equation
	}

}

local l1 : var lab tot_labs
local l2 : var lab cap_under_2_oz


// estimation results: tot_labs and cap_under_2_oz

esttab m_*_tot_labs m_*_cap_under_2_oz using "models-01.tex", ///
	replace keep(OTC_Restriction) rename(OTC "OTC_Restriction") se nonumber compress ///
	stats(ymean N N_g, labels("Mean" "Observations" "States")) ///
	title(Impact of OTC regulations on methamphetamine lab seizures. "`l1' and `l2'" \label{fe-results}) ///
	note("Notes: All regressions include state fixed effects and year/month fixed effects. The dependent variable in the regressions is count of labs seized or discovered in a month in a particular state.") ///
	mtitles("(1)" "(2)" "(3)" "(4)" "(1)" "(2)" "(3)" "(4)" )



// estimation results: cap_2_8_oz and cap_above_9_oz

local l3 : var lab  cap_2_8_oz  
local l4 : var lab cap_above_9_oz 

esttab m_*_cap_2_8_oz m_*_cap_above_9_oz using "models-02.tex", ///
	replace  keep(OTC_Restriction) rename(OTC "OTC_Restriction") se nonumber compress ///
	stats(ymean N N_g, labels("Mean" "Observations" "States")) ///
	title("Impact of OTC regulations on methamphetamine lab seizures. `l3' and `l4' ") ///
	note("Notes: All regressions include state fixed effects and year/month fixed effects. The dependent variable in the regressions is count of labs seized or discovered in a month in a particular state.") ///
	mtitles("(1)" "(2)" "(3)" "(4)" "(1)" "(2)" "(3)" "(4)" )
eststo clear



/* *******************************************************
   ----(f)----- 
   Run a regression of the number of labs discovered on state FE, 
   time dummies and event time dummies. Plot the estimates of the
   event time dummies and their confidence intervals. I

   ******************************************************* */

// create event time dummies
forvalues i = -12/24 {
	local ii = `i' + 13
	gen event_time_`ii' = 1 if `i' == months_from_until_otc
	replace event_time_`ii' = 0 if `i' != months_from_until_otc
}

/* In the following block of looping, we substract event dummies' cofficients 
   from the coefficient of dummy for months_from_until_otc = -1;
   the month before an OTC restriction was put into place */


#delimit ; 
foreach dv of varlist cap_under_2_oz cap_2_8_oz cap_above_9_oz 
{;
	qui reg `dv' event_time_* i.state_ab i.event_date_my i.state_time, noconstant; 

	gen pi_j_`dv'  = . ; // pi_j coefficients as in the paper notation
    gen ci_ll_`dv' = . ; // ci lower limit
	gen ci_ul_`dv' = . ; // ci upper limit

	forvalues i = 1/37 
	{;
		qui replace pi_j_`dv'  = _b[event_time_`i'] in `i';
		qui replace ci_ll_`dv' =  _b[event_time_`i'] - invttail(e(df_r),0.025)* _se[event_time_`i'] in `i' ;
		qui replace ci_ul_`dv' =  _b[event_time_`i'] + invttail(e(df_r),0.025)* _se[event_time_`i'] in `i' ;
	};


	gen pi_j_`dv'_from_et12  = pi_j_`dv'  - pi_j_`dv'[12]   in 1/37; // relative to tau_st or j = -1
	gen ci_ll_`dv'_from_et12 = ci_ll_`dv' -  ci_ll_`dv'[12] in 1/37; 
	gen ci_ul_`dv'_from_et12 = ci_ul_`dv' -  ci_ul_`dv'[12] in 1/37; 

	label variable pi_j_`dv'_from_et12  "event time coefficients, `dv'";
	label variable ci_ll_`dv'_from_et12 "lower limit 95% ci, `dv'";
	label variable ci_ul_`dv'_from_et12 "upper limit 95% ci, `dv'";
};
#delimit cr 

gen months_from_until_otc2 = .
forvalues i = -12/24 {
	local inn = `i' + 13
	replace months_from_until_otc2 = `i'  in `inn' 
}

// reproduce Figure (3) of the paper 
twoway scatter pi_j_cap_*_from_et12 months_from_until_otc2, connect(L L L) msymbol(t o sh) ///
	mcolor(blue red dkgreen) lp(dot dash dash) ///
	ytitle("Number of Labs Siezed Per Month") ///
	xtitle("Months From (Until) Implementation of Law") ///
	xlabel(-12(6)24) ylabel(-10(5)5) ///
	legend(cols(1) pos(0) symxsize(8) order(1 "Lab capacity under 2 oz" 2 "Lab capacity 2-8 oz" 3 "Lab capacity 9 oz or greater") ///
	bplacement(neast) region(lstyle(none))) ///
	xline(0, lcolor(cranberry) lwidth(.75)) /// 
	scheme(s2mono) graphregion(color(white) lwidth(none)  margin(zero)) ///
	plotregion(color(white))


graph export "event-study.eps", as(eps) preview(on) replace


keep in 1/37 // there is one outlier in event_time = 37, for e.g. pi_j_cap_under_2_oz = -8.133, removed in the paper as well.

* Plots of coefficients with their confidence intervals 

local x months_from_until_otc2
foreach var of varlist cap_under_2_oz cap_2_8_oz cap_above_9_oz {
	if "`var'" == "cap_under_2_oz" {
		local color blue 
	}
	
	else if "`var'" == "cap_2_8_oz" {
		local color red 
	}
	
	else if "`var'" == "cap_above_9_oz" {
		local color dkgreen 
	}
	twoway (scatter pi_j_`var' `x', connect(L) msize(medium) msymbol(S) mcolor(`color')) ///
		(rcap ci_ul_`var' ci_ll_`var' `x' ), ///
		ytitle("Number of Labs Siezed Per Month") xtitle("Months From (Until) Implementation of Law") xlabel(-12(6)24) /// 
		scheme(s2mono) graphregion(color(white) lwidth(none)  margin(zero)) ///
		plotregion(color(white)) legend(off)
	graph export "confidence-interval-plot-for-`var'.eps", as(eps) preview(on) replace 
}


log close 
