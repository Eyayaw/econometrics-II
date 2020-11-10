clear

foreach color in red blue green {
	di "`color'"
}

local colors red bue green
foreach color in `colors' {
	di "`color'"
}

foreach color of local colors {
	di "`color'"
}

sysuse auto

foreach var in price mpg rep78 {
	di "`var'"
	sum `var'
}

foreach var of varlist price-rep78 {
	di "`var'"
	sum `var'
}

foreach var in price-rep78 {
	di "`var'"
	sum `var'
}


foreach var of newlist x1 x2 x3 x4 x5 {
	gen `var'=0
}

foreach i of numlist 1/5 {
	gen y`i'=0
}

forvalues i=1/5 {
	gen z`i'=0
}
