clear 
set more off

* good for precision 
set type double 

* close any potential opened log file
capture log close 


// a) ============================================================
use "https://www.stata.com/data/jwooldridge/eacsap/nls80.dta", clear

* open the log file to record our output

log using "exercise-1.log", replace 

// let's look at the data (structure)
desc 
// let's get some summaries/stats

summ 



// b) ========================================================

egen avewage = mean(wage)
egen medwage = median(wage) // or pctile(wage), p(50) for median

list avewage medwage in 1/1


hist wage, freq kdensity // wage distribution 
hist wage, by(married)

egen avewage_married = mean(wage) if married
list avewage_married in 1/1

// c) ===========================================================
// let's create some dummies from the existing variables

// we need to call `summ iq` and then Stata saves the result in the memory in ...
// an object called `r` and we can access 
// the `median` `r(p50)`; or create a new var which captures this constant.

// -------------------------------------------------------
// way 1:
summ iq, detail            //return list 

local median_iq = r(p50)
display `median_iq'

// we can create the dummy: gen high_iq = 1 if iq > r(p50)
//-------------------------------------------------------

// ----------------------
// // way 2:
// egen median_iq = median(iq)
gen high_iq = 1 if iq > `median_iq'
replace high_iq = 0 if high_iq == . // replace missing from the previous expression 
// --------------------------


// educ dummies
gen educ_dummy :medium = 1 if educ == 13
replace educ_dummy = 0  if educ < 13
replace educ_dummy = 2 if educ > 13

label variable educ_dummy "educ level"
label define educ_dummy 0 "low" 1 "medium" 2 "high" 
label value educ_dummy educ_dummy 

tabulate educ_dummy, gen(educ_) // creates 3 educ dummies low, medium, and high
list educ* in 1/10 // to see the 10 rows of the new vars

rename (educ_1 educ_2 educ_3) (low_educ  medium_educ high_educ)

// d) ========================================================


//model 1
 
reg wage married high_iq low_educ medium_educ high_educ exper tenure age feduc
// model 2
reg wage married high_iq medium_educ exper tenure age feduc




// e) ===========================================================

// model 3
// we would have created an int term between married and high_ig as:
// `gen married_high_iq = married * high_iq`

reg wage married high_iq c.married#c.high_iq medium_educ exper tenure age feduc


// f) ===========================================================

// model 4 --- log-level

reg lwage married educ exper tenure age feduc 

// g) ======================================================
// model 5 --- log-log

gen leduc = ln(educ)

reg lwage married  leduc exper tenure age feduc 

// h) ==================================================
// model 6 --- level-log 

reg wage married leduc exper tenure age feduc 

log close 

exit, clear 







