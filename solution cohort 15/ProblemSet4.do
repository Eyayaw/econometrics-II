clear all			/* Reset memory, remove active data */
capture log close	/* In case no log is open */
set more off		/* tells Stata not to pause */
set matsize 4000	/* Expand maximum number of variables */
set type double 	/* Excel stores numeric values in double precision */

* Set working directory where dataset is located:
cd "C:\Users\ksueß\Documents\RGS\SoSe 19\Microeconometrics\Tutorials\4"

log using "ProblemSet4_logfile_KS", replace

* ECONOMETRICS II - Problem Set 4 
* Karolin Süß
* MAY 21, 2019
use otc_regulation

********************************************************************************
*a)
*Proportion of Population covered by law
*Total population for each event date
drop if mi(event_date)
drop if mi(pop_all_fitted)

sort event_date
bysort event_date: egen total_pop=total(pop_all_fitted)

* convert dates into STATA-Dates
generate event_date_aux = date(event_date , "DMY")
format event_date_aux %td
drop event_date
rename event_date_aux event_date

generate any_law_aux = date(any_law , "MDY")
format any_law_aux %td
drop any_law
rename any_law_aux any_law

*Create weights for population under law
*reduce dates to month and year
gen event_date_ym=mofd(event_date)
format event_date_ym %tm

gen any_law_ym=mofd(any_law)
format any_law_ym %tm

*Dummy=1 if law already inact
gen OTC=(event_date>any_law)

*Attention if law was implemented in respective month
gen attention=(event_date_ym==any_law_ym)

*Find end of month to extract number of days of the month
gen dd = day(any_law)
gen mm = month(any_law)
gen yyyy = year(any_law)
gen mm1=mm+1 if mm<12
replace mm1=1 if mm==12
replace yyyy=yyyy+1 if mm==12
gen end_month = mdy(mm1,1,yyyy)-1
format end_month %td
gen days_month=day(end_month)

*Fraction of days under law
gen frac=(days_month-dd)/days_month 
replace OTC=frac if attention==1

*drop helping variables
drop dd
drop mm
drop yyyy
drop mm1
drop end_month
drop days_month
drop frac
drop attention

*weighted population
gen weighted_population=OTC*pop_all_fitted
*Population under law
bysort event_date: egen pop_underlaw=total(weighted_population)
*fraction
bysort event_date: gen frac_underlaw=pop_underlaw/total_pop

*labs with 9oz or greater
gen cap_over_9_oz=tot_labs - cap_under_2_oz-cap_2_8_oz

*avg number of labs per event date
bysort event_date: egen avg_cap_under_2_oz=mean(cap_under_2_oz)
bysort event_date: egen avg_cap_2_8_oz=mean(cap_2_8_oz)
bysort event_date: egen avg_cap_over_9_oz=mean(cap_over_9_oz)
*plot
twoway scatter avg_cap_under_2_oz avg_cap_2_8_oz avg_cap_over_9_oz event_date_ym, ///
		msymbol(T Oh S) lwidth(medium)  mcolor(blue red green) lcolor(blue red green) lpattern(dot) connect(1 2 3) yaxis(1) yscale(range(0) axis(1)) ytitle("Proportion of Population Covered by Laws")  ///
 ||  line frac_underlaw event_date_ym, yaxis(2) yscale(range(0) axis(2)) lcolor(black) lwidth(medium) connect(direct) xtitle("Date") tlabel(2000m1(24)2008m2, format(%tmCY)) /// 
		legend(symxsize(9) cols(2) rows(2) order(1 "Lab capacity under 2 oz" 2 "Lab capacity 2-8 oz" 3 "Lab capacity 9 oz or greater" 4 "OTC law coverage")) ytitle("Average number of labs seized per month", axis(2)) 
graph export Dobkinetal_Fig2.pdf, replace
		
********************************************************************************
*Ex.b)
*Center event dates around any_law
gen diff=event_date_ym-any_law_ym

bysort diff: egen avg_cap_under_2_oz_diff=mean(cap_under_2_oz)
bysort diff: egen avg_cap_2_8_oz_diff=mean(cap_2_8_oz)
bysort diff: egen avg_cap_over_9_oz_diff=mean(cap_over_9_oz)
 
twoway scatter avg_cap_under_2_oz_diff avg_cap_2_8_oz_diff avg_cap_over_9_oz_diff diff, ///
		tlabel(-75(25)50) xline(0) msymbol(T Oh S) lwidth(medium)  mcolor(blue red green) lcolor(blue red green) lpattern(dot) connect(1 2 3)   ///
		xtitle("Month From (Until) Implementation of Law") /// 
		legend(symxsize(9) cols(2) rows(2) order(1 "Lab capacity under 2 oz" 2 "Lab capacity 2-8 oz" 3 "Lab capacity 9 oz or greater")) ytitle("Average number of labs seized per month") 
graph export labs_distance_any_law.pdf, replace

tab diff if diff>30
tab diff if diff>24
********************************************************************************
*Ex.c)
encode state_ab, gen(state)
xtset state event_date

eststo: xtreg cap_under_2_oz OTC i.event_date_ym, fe vce(cluster state)
eststo: xtreg cap_2_8_oz OTC i.event_date_ym, fe vce(cluster state)
eststo: xtreg cap_over_9_oz OTC i.event_date_ym, fe vce(cluster state)
eststo: xtreg tot_labs OTC i.event_date_ym, fe vce(cluster state)
esttab using labs_OTC.tex, replace b(4) se(4) ar2(4) ///
  title(Effect of law on number of discovered labs)
eststo clear		 


********************************************************************************
*Ex.f)

gen event_time_plus100=diff+100 //not possible to create dummy for negative values therefore add 100 to each difference to law inacted
label define l_event_time_plus100  88 "-12" 89 "-11" 90 "-10" 91 "-9" 92 "-8" 93 "-7" 94 "-6" 95 "-5" 96 "-4" 97 "-3" 98 "-2" 99 "-1" 100 "0" 101 "1" 102 "2" 103 "3" 104 "4" 105 "5" 106 "6" 107 "7" 108 "8" 109 "9" 110 "10" 111 "11" 112 "12" 113 "13" 114 "14" 115 "15" 116 "16" 117 "17" 118 "18" 119 "19" 120 "20" 121 "21" 122 "22" 123 "23" 124 "24" 
label value event_time_plus100 l_event_time_plus100

xtreg cap_under_2_oz i.event_date_ym ib88.event_time_plus100 event_time_plus100 if event_time_plus100>87 & event_time_plus100<125, fe vce(cluster state) //dummy 88 is the event-time dummy one year before the law was enacted. Footnote 24 states that all effects one year before the law was enacted are set to zero
coefplot, vertical keep(*.event_time_plus100) yline(0) 
graph export coefplot_cap_under_2_oz.pdf, replace

xtreg cap_2_8_oz i.event_date_ym ib88.event_time_plus100 event_time_plus100  if event_time_plus100>87 & event_time_plus100<125, fe vce(cluster state)
coefplot, vertical keep(*.event_time_plus100) yline(0) 
graph export coefplot_cap_2_8_oz.pdf, replace

xtreg cap_over_9_oz i.event_date_ym ib88.event_time_plus100 event_time_plus100  if event_time_plus100>87 & event_time_plus100<125, fe vce(cluster state)
coefplot, vertical keep(*.event_time_plus100) yline(0) 
graph export coefplot_cap_over_9_oz.pdf, replace 


translate ProblemSet4_logfile_KS.smcl ProblemSet4_logfile_KS.pdf, replace 
log close
