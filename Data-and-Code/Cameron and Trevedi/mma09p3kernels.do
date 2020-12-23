* MMA09P3KERNELS.DO  March 2005 for Stata version 8.0

log using mma09p3kernels.txt, text replace

********** OVERVIEW OF MMA09P3KERNELS.DO **********

* STATA Program 
* copyright C 2005 by A. Colin Cameron and Pravin K. Trivedi 
* used for "Microeconometrics: Methods and Applications" 
* by A. Colin Cameron and Pravin K. Trivedi (2005)
* Cambridge University Press 

* This program plots different kernel regression functions
* This is not included in the book
* There is no data 

* Results: 
*   Epanstata is similar to Gaussian kernel. Less peaked than Epanechnikov.
*   Triangular, Quartic, Triweight and Tricubic are similar, 
*   and are more peaked than Epanechnikov
*   The fourth oreder Kernels can take negative values.

* NOTE: For kernel density Stata uses an alternative formulation of Epanechnikov
*       To follow book and e.g. Hardle (1990) use epan2 
*       (available in Stata version 8.2) rather than epan 

********** SETUP **********

di "mma09p3kernels.do Cameron and Trivedi: Stata Kernel Functions"
set more off
version 8.0
set scheme s1mono  /* Graphics scheme */
  
********** GENERATE DATA  **********

* Graphs will be for z = -2.5 to 2.5 in increments of 0.02
set obs 251
gen z = -2.52 + 0.02*_n

********** CALCULATE THE KERNELS **********

* Indicator for |z| < 1
gen abszltone = 1
replace abszltone = 0 if abs(z)>=1

gen kuniform = 0.5*abszltone

gen ktriangular = (1 - abs(z))*abszltone

* Stata calls the usual Epanechnikov kernel epan2
gen kepanechnikov = (3/4)*(1 - z^2)*abszltone

* Stata uses alternative epanechnikov
gen abszltsqrtfive = 1
replace abszltsqrtfive = 0 if abs(z)>=sqrt(5)
gen kepanstata = (3/4)*(1 - (z^2)/5)/sqrt(5)*abszltsqrtfive

gen kquartic = (15/16)*((1 - z^2)^2)*abszltone

gen ktriweight = (35/32)*((1 - z^2)^3)*abszltone

gen ktricubic = (70/81)*((1 - (abs(z))^3)^3)*abszltone

gen kgaussian = normden(z)

gen k4thordergauss = (1/2)*(3-(z^2))*normden(z)

* This is the optimal 4th order - Pagan and Ullah p.57
gen k4thorderquartic = (15/32)*(3 - 10*z^2 + 7*z^4)*abszltone

sum

* Write data to a text (ascii) file so can use with programs other than Stata
outfile z abszltone kuniform ktriangular kepanechnikov abszltsqrtfive /*
  */ kepanstata kquartic ktriweight ktricubic kgaussian /* 
  */ k4thordergauss k4thorderquartic using mma09p3kernels.asc, replace

********** PLOT THE KERNEL FUNCTIONS **********

* Epanstata is similar to Gaussian kernel. Less peaked than Epanechnikov
graph twoway (line kuniform z) (line kepanechnikov z) (line kepanstata z) /*
    */ (line kgaussian z), title("Four standard kernel functions")

* Triangular, Quartic, Triweight and Tricubic are similar 
* and are more peaked than Epanechnikov
graph twoway (line ktriangular z) (line kquartic z) (line ktriweight z) /*
    */ (line ktricubic z), title("Four similar kernel functions")

graph twoway (line k4thordergauss z) (line k4thorderquartic z), /*
    */ title("Two fourth order kernel functions")

********** CLOSE OUTPUT **********
log close
* clear
* exit



