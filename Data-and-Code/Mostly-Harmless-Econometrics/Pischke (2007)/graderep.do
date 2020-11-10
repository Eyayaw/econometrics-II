version 8
set more 1
capture log close
log using stabu, replace text

/* prepare aggregate data from German statistical office to present results on
   grade repetition and secondary track choice */


clear


use graderep

for var stud* repeat*: replace X = . if X == 0

/* grade repetition rates */

gen rq1 = repeat1/stud1
gen rq2 = repeat2/stud2
gen rq3 = repeat3/stud3
gen rq4 = repeat4/stud4

replace rq4 = . if bula=="saar" & year==1961

save temp, replace

/* collapse data for grade repetition graphs */

drop if bula=="ns"
gen treat = 1                                           /* short school year states */
replace treat = 2 if bula=="bay"                        /* Bavaria with no transition */
replace treat = 3 if bula=="ber" | bula=="hh"           /* Berlin/Hamburg with no short school years */

collapse (sum) repeat1-repeat4 stud1-stud4, by(year treat)

for var repeat*: replace X = . if year >= 1962 & year <= 1964

gen rq1 = repeat1/stud1
gen rq2 = repeat2/stud2
gen rq3 = repeat3/stud3
gen rq4 = repeat4/stud4

format rq* %8.3f
l year rq* if treat==1
l year rq* if treat==2
l year rq* if treat==3


/* reshape data for regressions of grade repetition */

clear
set matsize 400
use temp

drop if year >= 1962 & year <= 1964

reshape groups grade 1-4
reshape vars rq stud
reshape cons bula year
reshape long



/* year is when school year starts */

gen ksj = 0
replace ksj = 1 if year == 1966
replace ksj = 1 if year == 1967 & grade >= 2
replace ksj = 1 if year == 1968 & grade >= 3
replace ksj = 1 if year == 1969 & grade == 4
replace ksj = 0 if bula=="bay" | bula=="ber" | bula=="hh"

gen ksj2 = ksj
replace ksj2 = 0 if bula=="ns"

gen ns = bula=="ns"
gen ber = bula=="ber"

sum rq [aw=stud]
xi: reg rq ksj i.bula i.grade i.year [aw=stud]
xi: reg rq ksj2 i.bula i.grade i.year [aw=stud]
xi: reg rq ksj i.bula i.grade i.year if ns==0  [aw=stud]

xi: reg rq ksj i.bula*i.grade i.year [aw=stud]
xi: reg rq ksj2 i.bula*i.grade i.year [aw=stud]
xi: reg rq ksj i.bula*i.grade i.year if ns==0  [aw=stud]

sum rq if grade >= 2 [aw=stud]
xi: reg rq ksj i.bula*i.grade i.year if grade >= 2 [aw=stud]
xi: reg rq ksj2 i.bula*i.grade i.year if grade >= 2 [aw=stud]
xi: reg rq ksj i.bula*i.grade i.year if grade >= 2 & ns==0  [aw=stud]

xi: reg rq ksj i.bula*i.grade i.bula*i.year i.grad*i.year [aw=stud]
xi: reg rq ksj2 i.bula*i.grade i.bula*i.year i.grad*i.year [aw=stud]
xi: reg rq ksj i.bula*i.grade i.bula*i.year i.grad*i.year if ns==0  [aw=stud]

/* standard errors */

xi: reg rq ksj i.bula i.grade i.year [aw=stud], cluster(bula)
xi: reg rq ksj2 i.bula i.grade i.year [aw=stud], cluster(bula)
xi: reg rq ksj i.bula i.grade i.year if ns==0  [aw=stud], cluster(bula)


log close
set more 0
