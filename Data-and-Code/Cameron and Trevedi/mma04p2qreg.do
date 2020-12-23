* MMA04P2QREG.DO   March 2005 for Stata version 8.0

log using mma04p2qreg.txt, text replace

********** OVERVIEW OF MMA04P2QREG.DO **********

* STATA Program 
* copyright C 2005 by A. Colin Cameron and Pravin K. Trivedi 
* used for "Microeconometrics: Methods and Applications" 
* by A. Colin Cameron and Pravin K. Trivedi (2005)
* Cambridge University Press 

* Chapter 4.6.4 pages 88-90
* Quantile Regression analysis.
*   (1) Quantile regression estimates for different quantiles
*   (2) Figure 4.1: Quantile Slope Coefficient Estimates as Quantile Varies
*   (3) Figure 4.2: Quantile Regression Lines as Quantile Varies

* To run this program you need data file
*    qreg0902.dta 
* or for programs other than Stata use qreg92.asc

* Step (3) takes a long time due to bootstrap to get standard errors.
* To speed up the program reduce the number of repititions in qsreg
* But any final results should use a large number of bootstraps  

********** SETUP **********

set more off
version 8.0
set scheme s1mono   /* Used for graphs */
   
********** DATA DESCRIPTION **********

* The data from World Bank 1997 Vietnam Living Standards Survey 
* are described in chapter 4.6.4.
* A larger sample from this survey is studied in Chapter 24.7

********** READ DATA, TRANSFORM and SAMPLE SELECTION **********

use qreg0902
describe
summarize

* Write data to a text (ascii) file so can use with programs other than Stata  
outfile sex age educyr98 farm urban98 hhsize lhhexp1 lhhex12m lnrlfood /*
        */ using qreg0902.asc, replace

* drop zero observations for medical expenditures
drop if lhhex12m == .

* lhhexp1 is natural logarithm of household total expenditure
* lhhex12m is natural logarithm of household medical expenditure
gen lntotal = lhhexp1
gen lnmed = lhhex12m
label variable lntotal "Log household total expenditure"
label variable lnmed "Log household medical expenditure"
describe 
summarize

********* ANALYSIS: QUANTILE REGRESSION **********

* (0) OLS
reg lnmed lntotal
predict pols
reg lnmed lntotal, robust
* Bootstrap standard errors for OLS
set seed 10101
* bs "reg lnmed lntotal" "_b[lntotal]", reps(100)

* (1) Quantile and median regression for quantiles 0.1, 0.5 and 0.9
*     Save prediction to construct Figure 4.2. 
qreg lnmed lntotal, quant(.10)
predict pqreg10
qreg lnmed lntotal, quant(.5)
predict pqreg50
qreg lnmed lntotal, quant(.90)
predict pqreg90

* (2) Create Figure 4.2 on page 90 first as this is easy
graph twoway (scatter lnmed lntotal, msize(vsmall)) (lfit pqreg90 lntotal, clstyle(p2)) /*
  */ (lfit pqreg50 lntotal, clstyle(p1)) (lfit pqreg10 lntotal, clstyle(p3)), /*
  */ scale (1.2) plotregion(style(none)) /*
  */ title("Regression Lines as Quantile Varies") /*
  */ xtitle("Log Household Medical Expenditure", size(medlarge)) xscale(titlegap(*5)) /* 
  */ ytitle("Log Household Total Expenditure", size(medlarge)) yscale(titlegap(*5)) /*
  */ legend(pos(11) ring(0) col(1)) legend(size(small)) /*
  */ legend( label(1 "Actual Data") label(2 "90th percentile") /*
  */         label(3 "Median") label(4 "10th percentile"))
graph export ch4fig2QR.wmf, replace

* (3) Create Figure 4.1 second as this is more difficult
* Simultaneous quantile regression for quantiles 0.05, 0.10, ..., 0.90, 0.95 
* with standard errors by bootstrap - here 200 replications
set seed 10101
sqreg lnmed lntotal, quant(.05,.1,.15,.2,.25,.3,.35,.4,.45,.5,.55,.6,.65,.7,.75,.8,.85,.9,.95) reps(200)
* Test equality of slope coefffiients for 25th and 75th quantiles
test [q25]lntotal = [q75]lntotal
* Create vectors of slope cofficients and estimated variances
* Code here specific for this problem
* with single slope coefficient is 1st, 3rd, 5th , ... entry 
matrix b = e(b)
matrix bslopevector = b[1,1]\b[1,3]\b[1,5]\b[1,7]\b[1,9]\b[1,11]\b[1,13]  /*
               */ \b[1,15]\b[1,17]\b[1,19]\b[1,21]\b[1,23]\b[1,25]  /*
               */ \b[1,27]\b[1,29]\b[1,31]\b[1,33]\b[1,35]\b[1,37] 
matrix V = e(V)
matrix Vslopevector = V[1,1]\V[3,3]\V[5,5]\V[7,7]\V[9,9]\V[11,11]\V[13,13] /*
               */ \V[15,15]\V[17,17]\V[19,19]\V[21,21]\V[23,23]\V[25,25] /*
               */ \V[27,27]\V[29,29]\V[31,31]\V[33,33]\V[35,35]\V[37,37] 
matrix q = e(q1)\e(q2)\e(q3)\e(q4)\e(q5)\e(q6)\e(q7)\e(q8)\e(q9)\e(q10) /*
            */ \e(q11)\e(q12)\e(q13)\e(q14)\e(q15)\e(q16)\e(q17)\e(q18)\e(q19)
* Convert column vectors to variables as graph handles variables
svmat bslopevector, name(bslope)
svmat Vslopevector, name(Vslope)
svmat q, name(quantiles) 
gen upper = bslope1 + 1.96*sqrt(Vslope1)
gen lower = bslope1 - 1.96*sqrt(Vslope1)
* Also include OLS slope ccoefficient
quietly reg lnmed lntotal
gen bols=_b[lntotal]
sum upper bslope1 lower bols

* Following produces Figure 4.1 om page 89
graph twoway (line upper quantiles1, msize(vtiny) mstyle(p2) clstyle(p1) clcolor(gs12)) /* 
  */ (line bslope1 quantiles1, msize(vtiny) mstyle(p1) clstyle(p1)) /* 
  */ (line lower quantiles1, msize(vtiny) mstyle(p2) clstyle(p1) clcolor(gs12)) /*
  */ (line bols quantiles1, msize(vtiny) mstyle(p3) clstyle(p2)), /*
  */ scale(1.2) plotregion(style(none)) /*
  */ title("Slope Estimates as Quantile Varies") /*
  */ xtitle("Quantile", size(medlarge)) xscale(titlegap(*5)) /* 
  */ ytitle("Slope and confidence bands", size(medlarge)) yscale(titlegap(*5)) /*
  */ legend(pos(4) ring(0) col(1)) legend(size(small)) /*
  */ legend( label(1 "Upper 95% confidence band") label(2 "Quantile slope coefficient") /*
  */         label(3 "Lower 95% confidence band") label(4 "OLS slope coefficient") )
graph export ch4fig1QR.wmf, replace

********** CLOSE OUTPUT **********
log close
clear
exit
