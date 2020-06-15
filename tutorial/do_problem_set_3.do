
****************************************************************************** 
* Econometrics II, tutorial 3

* Do-file for Problem-Set 3

****************************************************************************** 

clear 

set more off

set type double

* all open log-files will be closed
capture log close

* Working directory anzeigen lassen
* cd ...


* Define the path where you want to store your log-file:
log using "log_problem_set_3.log", replace

* Open the data-set:

use "http://fmwww.bc.edu/ec-p/data/wooldridge/ezunem.dta", clear


* declare dataset to be panel data

xtset city year

* a) first difference 
* model: d.luclms_it = d.ez_it + d.year_dummies + d.u_it

* we can use the D or L operators to generate differences, gen dx = d.x or gen dx = x-l.x 
* we have to drop one of the year dummies, let's drop d81 so it's the reference year
reg d.luclms d.ez d.d82 d.d83 d.d84 d.d85 d.d86 d.d87 d.d88

* b) first difference with fixed effect-year interaction
* model: d.luclms_it = d.ez_it + city_i*d.year_t + d.u_it

* let's create year index so that instead of year = 1981, 1982, ... we have 1, 2, ...
by city: gen year_t = _n

* let's create an interaction between individual fixed effect and year; city * year_t
gen cityear = c.city#c.year_t 

reg d.luclms d.ez  d.cityear 

* c) include full set of year dummies in b)
reg d.luclms d.ez  d.cityear d.d82 d.d83 d.d84 d.d85 d.d86 d.d87 d.d88

log close 
