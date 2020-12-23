********************************************************************************
*Karolin Süß

*Microeconometrics SoSe19
*Exercise No. 1

********************************************************************************

clear 									// clear everything
set type double 						// keep all digits (not important here, but might be in other cases)
capture log close 						//close all log files
log using "C:\Users\ksueß\Documents\RGS\SoSe 19\Microeconometrics\Tutorials\1\log_Tut1", replace
cd "C:\Users\ksueß\Documents\RGS\SoSe 19\Microeconometrics\Tutorials\1"


use "C:\Users\ksueß\Documents\RGS\SoSe 19\Microeconometrics\Tutorials\1\nls80.dta"
********************************************************************************
************************Exercise 1**********************************************
*a)
describe
sum
sum wage, detail
tab married south
tab married black

*b)
sum wage, detail
egen avg_wage=mean(wage)
*Mean wage is larger than median
hist wage
kdensity wage, xline(957.9455)
sum wage if married==1

*c)
*Generate high_iq variable
egen median_iq=pctile(iq), p(50)

gen high_iq=(iq>median_iq)

*Generate education dummies
gen medium_educ=(educ==13)

gen high_educ=(educ>13)


gen low_educ=(educ<13)

*d)
reg wage married high_iq medium_educ high_educ
reg wage married high_iq medium_educ high_educ, vce(robust)
reg wage married high_iq medium_educ high_educ hours exper tenure age black south urban 

*e)
gen married_high_iq=married*high_iq
reg wage married high_iq married_high_iq medium_educ high_educ

*f)
reg lwage married high_iq educ hours age black south urban 

*g)
gen leduc=log(educ)
reg lwage married high_iq leduc hours age black south urban 

*h)
reg wage married high_iq leduc hours age black south urban 
********************************************************************************
*end session

log close									// close log file
exit, clear

********************************************************************************
*Tutorial 30th April 2019
edit //open data
