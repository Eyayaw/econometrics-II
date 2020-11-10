* ECONOMETRICS II - Problem Set 2 
* Philipp Nickol
* MAY 21, 2019

clear all			/* Reset memory, remove active data */
capture log close	/* In case no log is open */
set more off		/* tells Stata not to pause */
set matsize 4000	/* Expand maximum number of variables */
set type double 	/* Excel stores numeric values in double precision */

* Set working directory where dataset is located:
//cd "C:\Users\PN\Dropbox\RGS\Econometrics II\Tutorial\tutorial_210519\"


* Set path/filename for log-file:
log using "ProblemSet4_logfile.log", replace

* Load dataset
//use otc_regulation13.dta, clear
use otc_regulation.dta, clear
	drop if event_date == ""

* Generate month and year variable which identify the time aspec
gen date = date(event_date, "DMY")
gen month = month(date)
gen year = year(date)
		drop date

* Convert dataset to utilize panel functionality of Stata
* Extract month-year
gen sub_d = substr(event_date,3,.)

* Generate month-year variable
gen date = monthly(sub_d, "MY")
	format date %tm

* Convert state variable to numerical value for declaration as panel
encode state_ab, gen(state)

* Declare as panel
tsset state date

* Drop auxilary variables no longer needed
drop sub_d event_date state_ab

* Order variables
order state date month year


* Convert enactment date to numerical
gen sub_d = date(any_law, "MDY")
	format sub_d %td
	
* Generate month, year, and day variable for later use in variable construction
gen day_enacted = day(sub_d)
gen month_enacted = month(sub_d)
gen year_enacted = year(sub_d)

* Drop auxiliary variable
drop sub_d any_law


* Exercise (a)
* Generate variable with total number of days in a month
gen days_in_month = 30 if month_enacted == 4 | month_enacted == 6 | month_enacted == 9 | month_enacted == 11 
	replace days_in_month = 31 if  month_enacted == 1 | month_enacted == 3 | month_enacted == 5 | month_enacted == 7 | month_enacted == 8 | month_enacted == 10 | month_enacted == 12 
	* February can be ignored since the law was not enacted in any state in February

* Generate proportion of days in effect for weighting (the share has to be
* substracted from 1 to get the share of days the law in force).
gen proportion_days = 1-(day_enacted/days_in_month)

* Calculate total population in all states in given month-year 
bysort date: egen pop_allstates = sum(pop_all_fitted)
	* Sort for convenient structure of dataset
	sort state date 
	* Order variables for convenience
	order state date month year day_e month_e year_e

* Calculate population under new regulation by state

* Whole population of a state falls under the new regulation in any year after 
* the new law was enacted
gen pop_under_reg = pop_all_fitted if year >= year_enacted
	* No population falls under the new regulation in the months before the new 
	* law was enacted in the year of enactment
	replace pop_under_reg = . if year == year_enacted & month < month_enacted
	* Population under new regulation weighted by proportion of days the law is
	* in effect in month and year of implementation
	replace pop_under_reg = proportion_days*pop_all_fitted if year == year_enacted & month == month_enacted
	
* Calculate the total population under new regulation across all states
bysort date: egen total_pop_under_reg = sum(pop_under_reg)

* Calculate population in all states under regulation as a share of total 
* population
gen pop_share_under_reg = total_pop_under_reg/pop_allstates
	* Sort for convenient structure of dataset
	sort state date
	
* Calculate the number of labs seized with 9 oz capacity and above
gen cap_9_oz = tot_labs -  cap_under_2_oz - cap_2_8_oz 

* Calculate the average number of labs seized per months for the three lab sizes
bysort date: egen avg_2oz = mean(cap_under_2_oz)
bysort date: egen avg_8oz = mean(cap_2_8_oz)
bysort date: egen avg_9oz = mean(cap_9_oz)


* Replicate Figure 2 of the Dobnik et al. (2014) paper
preserve
drop if date < monthly("November 2001", "MY")
drop if date > monthly("April 2008", "MY")
twoway 	(scatter avg_2oz date, connect(l) msymbol(T ) msize(small) yaxis(1) mcolor(blue) lcolor(blue) lwidth(medium) lpattern(dot) connect(direct) cmissing(n)) ///
		(scatter avg_8oz date, connect(l) msymbol(Oh) msize(small) yaxis(1) mcolor(red) lcolor(red) lwidth(medium) lpattern(dash) connect(direct) cmissing(n))  ///
		(scatter avg_9oz date, connect(l) msymbol(S ) msize(small) yaxis(1) mcolor(green) lcolor(green) lwidth(medium) lpattern(dash) connect(direct) cmissing(n))  ///
		(line pop_share_under_reg date, yaxis(2)  lcolor(black) lwidth(medium) connect(direct) cmissing(n)) if  date > monthly("November 2001", "MY"), ///
		ylabel(, angle(horizontal) format(%9.1f)) ylabel(, axis(2) angle(horizontal) format(%9.1f)) ytitle("Average number of labs seized per month") ytitle("Proportion of population covered by laws", axis(2)) ///
		title("") xtitle("Date")	///
		tlabel(2002m1(24)2008m2, format(%tmCY)) ///
		legend(on order(1 "Lab capacity under 2 oz" 2 "Lab capacity 2-8 oz" 3 "Lab capacity 9 oz or greater" 4 "OTC law coverage") 	///
		region(lcolor(white))) scheme(s1mono) graphregion(fcolor(white) 			///
		lcolor(white) lwidth(none) ifcolor(white) ilcolor(white) margin(zero)) 					///
		plotregion(fcolor(white) lcolor(white) ifcolor(white) ilcolor(white))
graph export "Figure2_Replication.eps", as(eps) preview(on) replace
restore


* Exercise (b) 
* Sort for convenient structure of dataset
sort state date
	
* Generate event horizon, i.e. variable that is 0 in month of enactment
* -1 in the month before and 1 in the month afer and so on and so forth
by state: gen datenum = _n
	by state: gen target=datenum if month == month_enacted & year == year_enacted
	egen td=min(target), by(state)
		drop target
	* Finally generate event_horizon variable
	gen event_horizon = datenum-td
		* Drop auxiliary variable
		drop td

* Calculate average number of seized labs in specific month relative to 
* enactment month for each of the three lab-size categories
bysort event_horizon: egen avg_2oz_event = mean(cap_under_2_oz)
bysort event_horizon: egen avg_8oz_event = mean(cap_2_8_oz)
bysort event_horizon: egen avg_9oz_event = mean(cap_9_oz)
	

* Plot averages centered around the event horizon
twoway 	(scatter avg_2oz_event event_horizon, connect(l) msymbol(T ) msize(small) yaxis(1) mcolor(blue) lcolor(blue) lwidth(medium) lpattern(dot) connect(direct) cmissing(n)) ///
		(scatter avg_8oz_event event_horizon, connect(l) msymbol(Oh) msize(small) yaxis(1) mcolor(red) lcolor(red) lwidth(medium) lpattern(dash) connect(direct) cmissing(n)) ///
		(scatter avg_9oz_event event_horizon, connect(l) msymbol(S ) msize(small) yaxis(1) mcolor(green) lcolor(green) lwidth(medium) lpattern(dash) connect(direct) cmissing(n)) if event_horizon > -13 & event_horizon < 25 ///
		, ylabel(, angle(horizontal) format(%9.1f)) ytitle("Average number of labs seized per month") ///
		title("") xtitle("Months from (until) implementation of OTC regulation")	///
		tlabel(-12(6)24) xline(0)	///
		legend(on order(1 "Lab capacity under 2 oz" 2 "Lab capacity 2-8 oz" 3 "Lab capacity 9 oz or greater") 	///
		region(lcolor(white))) scheme(s1mono) graphregion(fcolor(white) 			///
		lcolor(white) lwidth(none) ifcolor(white) ilcolor(white) margin(zero)) 					///
		plotregion(fcolor(white) lcolor(white) ifcolor(white) ilcolor(white))
graph export "Figure_Exercise_b.eps", as(eps) preview(on) replace


* Exercise (c)
* Generate OTC variable as indicator set to zero in the months prior to 
* implementation of a state's regulation, one afterwards, and the fraction of 
* the month a regulation was implemented in months a regulation was enacted
gen OTC = 0
	replace OTC = 1 if year > year_enacted & year !=.
	replace OTC = 1 if year == year_enacted & year !=. & month > month_enacted
	replace OTC = (days_in_month - day_enacted - 1)/days_in_month if year == year_enacted & month == month_enacted & year !=.
	
* Fixed effects regression(s) with clustered standard errors on the state level
* Lab sizes under 2oz:
xtreg cap_under_2_oz OTC i.date, fe vce(cluster state) 
	estimates store low
* Lab sizes 2 - 8 oz
xtreg cap_2_8_oz OTC i.date, fe vce(cluster state)
	estimates store med
* Lab sizes 9oz or greater
xtreg cap_9_oz OTC i.date, fe vce(cluster state)
	estimates store high
* All labs:
xtreg tot_labs OTC i.date, fe vce(cluster state)
	estimates store all
* Generate output table using outreg2
ssc install outreg2
outreg2 OTC [low med high all] using "Resultstable.tex", tex replace
	 estimates drop all high med low

	
* Exercise (d): See accompanying PDF
* Exercise (e): See accompanying PDF

	
* Exercise (f)
* Generate dummy variables from event_horizon variable which measures the month
* relative to the introduction of an OTC regulation.
forval i = -12(1)24{
	if `i'<0{
	gen  t_neg`=`i'*(-1)' = 1 if `i' == event_horizon
		replace t_neg`=`i'*(-1)' = 0 if `i' != event_horizon
	}
	else{
	gen t_`i' = 1 if `i' == event_horizon
		replace t_`i' = 0 if `i' != event_horizon
	}
}

* Generate state-specific time-trend
forvalues i = 1(1)51{
	gen datenum_`i' = datenum
		replace datenum_`i' = 0 if state != `i'
	}



* Regress equation (2) of the Dobnik et al. (2014) paper with the number of labs
* producing under 2oz as the dependent variable
reg cap_under_2_oz t_* i.state i.date datenum_*, noconst 


* Extract ceofficients and confidence intervals from regression output
* Save regression output in matrix and convert matrix to variables
matrix rt = r(table)
	svmat rt
	
	* Generate variables before looping over regression output (F3 subindex used
	* for variables created to replicate Figure 3 below)
	gen coeffs2oz = .	
	gen ci_lb_2oz = .
	gen ci_ub_2oz = .
	gen coeffs2ozF3 = .
	gen ci_lb_2ozF3 = .
	gen ci_ub_2ozF3 = .
	
	* Looping over regression output and assign respective values to newly 
	* generated variables 
	forval i = 1(1)37{
		replace coeffs2oz = rt`i'[1] in `i'
		replace coeffs2ozF3 = coeffs2oz - rt12[1] in `i'
		replace ci_lb_2oz = rt`i'[5] in `i'
		replace ci_lb_2ozF3 = ci_lb_2oz - rt12[5] in `i'
		replace ci_ub_2oz = rt`i'[6] in `i'
		replace ci_ub_2ozF3 = ci_ub_2oz - rt12[6] in `i'
	}
* Drop the variables associated with the converted matrix of regression output
* to avoid messing up the whole dataset
drop rt*
	
* Regress equation (2) of the Dobnik et al. (2014) paper with the number of labs
* producing 2 - 8oz as the dependent variable
reg cap_2_8_oz t_* i.state i.date datenum_*, noconst

* Extract ceofficients and confidence intervals from regression output
* Save regression output in matrix and convert matrix to variables
matrix rt = r(table)
	svmat rt

	* Generate variables before looping over regression output (F3 subindex used
	* for variables created to replicate Figure 3 below)
	gen coeffs28oz = .	
	gen ci_lb_28oz = .
	gen ci_ub_28oz = .
	gen ci_ub_28ozF3 = .
	gen ci_lb_28ozF3 = .
	gen coeffs28ozF3 = .
	
	* Looping over regression output and assign respective values to newly 
	* generated variables
	forval i = 1(1)37{
		replace coeffs28oz = rt`i'[1] in `i'
		replace coeffs28ozF3 = coeffs28oz - rt12[1] in `i'
		replace ci_lb_28oz = rt`i'[5] in `i'
		replace ci_lb_28ozF3 = ci_lb_28oz - rt12[5] in `i'
		replace ci_ub_28oz = rt`i'[6] in `i'
		replace ci_ub_28ozF3 = ci_ub_28oz - rt12[6] in `i'
	}
* Drop the variables associated with the converted matrix of regression output
* to avoid messing up the whole dataset
drop rt*
		
* Regress equation (2) of the Dobnik et al. (2014) paper with the number of labs
* producing 9oz and more as the dependent variable
reg cap_9_oz t_* i.state i.date datenum_*, noconst

* Extract ceofficients and confidence intervals from regression output
* Save regression output in matrix and convert matrix to variables
matrix rt = r(table)
	svmat rt

	* Generate variables before looping over regression output (F3 subindex used
	* for variables created to replicate Figure 3 below)
	gen coeffs9oz = .	
	gen ci_lb_9oz = .
	gen ci_ub_9oz = .
	gen coeffs9ozF3 = .
	gen ci_lb_9ozF3 = .
	gen ci_ub_9ozF3 = .

	* Looping over regression output and assign respective values to newly 
	* generated variables
	forval i = 1(1)37{
		replace coeffs9oz = rt`i'[1] in `i'
		replace coeffs9ozF3 = coeffs9oz - rt12[1] in `i'
		replace ci_lb_9oz = rt`i'[5] in `i'
		replace ci_lb_9ozF3 = ci_lb_9oz - rt12[5] in `i'
		replace ci_ub_9oz = rt`i'[6] in `i'
		replace ci_ub_9ozF3 = ci_ub_9oz - rt12[6] in `i'
	}
* Drop the variables associated with the converted matrix of regression output
* to avoid messing up the whole dataset
drop rt*

* Generate second event_horizon variable (only for convenience when plotting 
* that thing)
gen event_horizon2 = -12 if _n == 1
	replace event_horizon2 = event_horizon2[_n-1] + 1 if _n >1

* Replicate Figure 3 of the Dobnik et al. (2014) paper
twoway 	(scatter coeffs2ozF3  event_horizon2, connect(l) msymbol(T ) msize(small) yaxis(1) mcolor(blue) lcolor(blue) lwidth(medium) lpattern(dot) connect(direct) cmissing(n)) ///
		(scatter coeffs28ozF3 event_horizon2, connect(l) msymbol(Oh) msize(small) yaxis(1) mcolor(red) lcolor(red) lwidth(medium) lpattern(dash) connect(direct) cmissing(n)) ///
		(scatter coeffs9ozF3  event_horizon2, connect(1) msymbol(S ) msize(small) yaxis(1) mcolor(green) lcolor(green) lwidth(medium) lpattern(dash) connect(direct) cmissing(n)) if event_horizon2 < 24 ///
		, ylabel(, angle(horizontal) format(%9.1f)) ytitle("") ///
		title("") xtitle("Months from (until) implementation of OTC regulation")	///
		tlabel(-12(6)24) xline(0)	///
		legend(on order(1 "Lab capacity under 2 oz" 2 "Lab capacity 2-8 oz" 3 "Lab capacity 9 oz or greater") 	///
		region(lcolor(white))) scheme(s1mono) graphregion(fcolor(white) 			///
		lcolor(white) lwidth(none) ifcolor(white) ilcolor(white) margin(zero)) 					///
		plotregion(fcolor(white) lcolor(white) ifcolor(white) ilcolor(white))
graph export "Figure3_Replication.eps", as(eps) preview(on) replace


* Plot the coefficients with confidence intervals as asked in the exercise 
twoway 	(scatter coeffs2oz  event_horizon2, connect(l) msymbol(T ) msize(small) yaxis(1) mcolor(blue) lcolor(blue) lwidth(medium) lpattern(dot) connect(direct) cmissing(n)) ///
		(scatter coeffs28oz event_horizon2, connect(l) msymbol(Oh) msize(small) yaxis(1) mcolor(red) lcolor(red) lwidth(medium) lpattern(dash) connect(direct) cmissing(n)) ///
		(scatter coeffs9oz  event_horizon2, connect(1) msymbol(S ) msize(small) yaxis(1) mcolor(green) lcolor(green) lwidth(medium) lpattern(dash) connect(direct) cmissing(n)) ///
		(line ci_lb_2oz event_horizon2, lcolor(eltblue) lwidth(medium) lpattern(shortdash_dot) connect(direct) cmissing(n)) ///
		(line ci_ub_2oz event_horizon2, lcolor(eltblue) lwidth(medium) lpattern(shortdash_dot) connect(direct) cmissing(n)) ///
		(line ci_lb_28oz event_horizon2, lcolor(erose) lwidth(medium) lpattern(shortdash_dot) connect(direct) cmissing(n)) ///
		(line ci_ub_28oz event_horizon2, lcolor(erose) lwidth(medium) lpattern(shortdash_dot) connect(direct) cmissing(n)) ///
		(line ci_lb_9oz event_horizon2, lcolor(eltgreen) lwidth(medium) lpattern(shortdash_dot) connect(direct) cmissing(n)) ///
		(line ci_ub_9oz event_horizon2, lcolor(eltgreen) lwidth(medium) lpattern(shortdash_dot) connect(direct) cmissing(n)) if event_horizon2 < 24 ///
		, ylabel(, angle(horizontal) format(%9.1f)) ytitle("") ///
		title("") xtitle("Months from (until) implementation of OTC regulation")	///
		tlabel(-12(6)24) xline(0)	///
		legend(on order(1 "Lab capacity under 2 oz" 4 "95% Confidence Interval" 2 "Lab capacity 2-8 oz" 6 "95% Confidence Interval" 3 "Lab capacity 9 oz or greater" 8 "95% Confidence Interval") 	///
		region(lcolor(white))) scheme(s1mono) graphregion(fcolor(white) 			///
		lcolor(white) lwidth(none) ifcolor(white) ilcolor(white) margin(zero)) 		///
		plotregion(fcolor(white) lcolor(white) ifcolor(white) ilcolor(white))
graph export "Figure_exercise_f.eps", as(eps) preview(on) replace

* Close log-file:
log close
