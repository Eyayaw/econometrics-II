* MMA09P2NPMORE.DO  March 2005 for Stata version 8.0

log using mma09p2npmore.txt, text replace

********** OVERVIEW OF MMA09P2NPMORE.DO **********

* STATA Program 
* copyright C 2005 by A. Colin Cameron and Pravin K. Trivedi 
* used for "Microeconometrics: Methods and Applications" 
* by A. Colin Cameron and Pravin K. Trivedi (2005)
* Cambridge University Press 

* Chapter 9.4-9.5 (pages 307-19)
* More on nonparametric regression, including Figures 9.5 - 9.7

* It provides
* (1) Nonparametric regression
*       k-nearest neighbors regression: Figure 9.5 in chapter 9.4.2 (ch9ksmma)
*       Lowess regression: Figure 9.6 in chapter 9.4.3 (ch9ksmlowess)
*       Kernel regression (using Stata add-on kernreg)
* (2) Nonparametric derivative estimation
*       Figure 9.7 in chapter 9.5.5 (ch9kderiv)  
* (3) Cross-validation - still incomplete
* using generated data (see below)

* See also mma09p1np.do for nonparametric density estimation and regression 

* This program uses free Stata add-on command kernreg
* To obtain in Stata give command search kernreg

********** SETUP **********

di "mma09p2npmore.do Cameron and Trivedi: Stata nonparametrics with generated data"
set more off
version 8.0
set scheme s1mono  /* Graphics scheme */
  
********** GENERATE DATA  **********

* Model is  y = 150 + 6.5*x - 0.15*x^2 + 0.001*x^3 + u
* where     u ~ N[0, 25^2]
*           x = 1, 2, 3, ... , 100
*           e ~ N[0, 2^2]

set seed 10101
set obs 100
gen u = 25*invnorm(uniform())
gen x = _n
gen y = 150 + 6.5*x - 0.15*x^2 + 0.001*x^3 + u
sum

* Write data to a text (ascii) file so can use with programs other than Stata
outfile y x using mma09p2npmore.asc, replace

******** PARAMETRIC REGRESSION **********

* OLS regression on cubic polymomial
gen xsquared = x^2
gen xcubed = x^3
reg y x xsquared xcubed
predict ycubic
summarize y ycubic

******** (1) NONPARAMETRIC REGRESSION **********

* K-NEAREST NEIGHBORS REGRESSION - FIGURE 9.5
* ksm without options gives running mean = moving average = centered kNN
* Here _N = 100 so bwidth = 0.05 gives 100*0.05 = 5 nearest neighbours
graph twoway (scatter y x, msize(medsmall) msymbol(o)) /*
  */ (lowess y x, mean noweight bwidth(0.05) clstyle(p1)) /* 
  */ (lfit y x, clstyle(p3)) /*
  */ (lowess y x, mean noweight bwidth(0.25) clstyle(p2)), /*  
  */ scale (1.2) plotregion(style(none)) /*
  */ title("k-Nearest Neighbours Regression as k Varies") /*
  */ xtitle("Regressor x", size(medlarge)) xscale(titlegap(*5)) /* 
  */ ytitle("Dependent variable y", size(medlarge)) yscale(titlegap(*5)) /*
  */ legend(pos(12) ring(0) col(1)) legend(size(small)) /*
  */ legend( label(1 "Actual Data") label(2 "kNN (k=5)") /*
  */         label(3 "Linear OLS") label(4 "kNN (k=25)")) 
graph save ch9ksmma, replace
graph export ch9ksmma.wmf, replace

* VERIFY THAT kNN SAME AS MOVING AVERAGE  
* Do moving average by hand for k = 5
gen yma5 = (y[_n-2] + y[_n-1] + y + y[_n+1] + y[_n+2])/5
replace yma5 = (y[_n]+y[_n+1]+y[_n+2])/3 if _n==1
replace yma5 = (y[_n-1]+y[_n]+y[_n+1]+y[_n+2])/4 if _n==2
replace yma5 = (y[_n+1]+y[_n]+y[_n-1]+y[_n-2])/4 if _n==99
replace yma5 = (y[_n]+y[_n-1]+y[_n-2])/3 if _n==100
lowess y x, mean noweight bwidth(0.05) nogr gen(yknn5)
sum yma5 yknn5

* LOWESS REGRESSION - FIGURE 9.6
graph twoway (scatter y x, msize(medsmall) msymbol(o)) /*
  */ (lowess y x, bwidth(0.25) clstyle(p1)) /* 
  */ (line ycubic x, clstyle(p3)), /*
  */ scale (1.2) plotregion(style(none)) /*
  */ title("Lowess Nonparametric Regression") /*
  */ xtitle("Regressor x", size(medlarge)) xscale(titlegap(*5)) /* 
  */ ytitle("Dependent variable y", size(medlarge)) yscale(titlegap(*5)) /*
  */ legend(pos(12) ring(0) col(1)) legend(size(small)) /*
  */ legend( label(1 "Actual Data") label(2 "Lowess (k=25)") /*
  */         label(3 "OLS Cubic Regression") )
graph save ch9ksmlowess, replace
graph export ch9ksmlowess.wmf, replace

* KERNEL REGRESSION COMPARED TO k NEAREST NEIGHBORS REGRESSION
* For this artificial example (with equally spaced x) 
*  knn = kernel regression using uniform prior 
* Kercode 1 = Uniform; 2 = Triangle; 3 = Epanechnikov; 4 = Quartic (Biweight);
*         5 = Triweight; 6 = Gaussian; 7 = Cosinus
* bwidth(#) defines width of the weight function window around each grid point.
* npoint(#) specifies the number of equally spaced grid points over range of x.
* Here bwidth(12) gives e.g. positive weight from x=15 to x=39 if current x=37  
kernreg y x, bwidth(12) kercode(1) npoint(100) ylabel gen(pykernreg xkernreg)
lowess y x, mean noweight bwidth(0.25) gen(yknn25)
sum pykernreg yknn25

******** (2) DERIVATIVE ESTIMATION **********

* DERIVATIVE ESTIMATION - FIGURE 9.7
* Here use Lowess regression
lowess y x, xlab ylab bwidth(0.25) lowess nogr gen(yplowess)  
* Need to first sort data on regressor if data on regressor are not ordered
sort x 
gen dydxlowess = (yplowess - yplowess[_n-1])/(x - x[_n-1])
* And do the same for the earlier fitted cubic
gen dydxcubic = (ycubic - ycubic[_n-1])/(x - x[_n-1])
graph twoway (line dydxlowess x, clstyle(p1)) /* 
  */ (line dydxcubic x, clstyle(p3)), /*
  */ scale (1.2) plotregion(style(none)) /*
  */ title("Nonparametric Derivative Estimation") /*
  */ xtitle("Regressor x", size(medlarge)) xscale(titlegap(*5)) /* 
  */ ytitle("Dependent variable y", size(medlarge)) yscale(titlegap(*5)) /*
  */ legend(pos(12) ring(0) col(1)) legend(size(small)) /*
  */ legend( label(1 "From Lowess (k=25)") /*
  */ label(2 "From OLS Cubic Regression") )
graph save ch9kderiv, replace
graph export ch9kderiv.wmf, replace

******** (3) CROSS-VALIDATION [PRELIMINARY] **********

/* The following does not work. 
   I need to figure out use of macros */

forvalues i = 5/25 { 
  scalar bd`i' = 0.01*`i'
  global bw`i' = bd`i'
  lowess y x, mean noweight bwidth($bw`i') gen(py`i') nogr
  gen cv`i' = sum(3/2*(y-py`i')^2)
} 
sum
* Then need to choose the `i' with minimum cv`i'
* Problem here is that this gives e.g. $bw5 = 5 not 0.05

********** CLOSE OUTPUT
log close
* clear
* exit


