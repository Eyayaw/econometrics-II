* MMA09P1NP.DO  March 2005 for Stata version 8.2

log using mma09p1np.txt, text replace

********** OVERVIEW OF MMA09P1NP.DO **********

* STATA Program 
* copyright C 2005 by A. Colin Cameron and Pravin K. Trivedi 
* used for "Microeconometrics: Methods and Applications" 
* by A. Colin Cameron and Pravin K. Trivedi (2005)
* Cambridge University Press 

* Chapter 9.2 p.295-297
* Nonparametric density estimation and nonparametric regression using actual data.

* (1) Histogram: Figure 9.1 in chapter 9.2.1 (ch9hist)
* (2) Kernel density estimate as bandwidth varies: Figure 9.2 in chapter 9.2.1 (ch9kd1)
* (3) Kernel density estimate as kernel varies: Figure 9.4 in chapter 9.3.4 (ch9kdensu1)
* (4) Lowess regression: Figure 9.3 in chapter 9.4.3 (ch9ksm1)
* (5) Extra: Nearest neighbours regression: using Lowess and using add-on knnreg
* (6) Extra: Kernel regression: using add-on kernreg

* using data on earnings and education (see below)

* NOTE: This particular program uses version 8.2 rather than 8.0
*       For kernel density Stata uses an alternative formulation of Epanechnikov
*       To follow book and e.g. Hardle (1990) use epan2 rather than epan 
*       epan = epan2 if epan bandwidth is epan2 bandwidth divided by sqrt(5) 
*       where kernel epan2 is an update to Stata version 8.2

* To run this program you need file
*    psidf3050.dat 
* in your directory

* To do (5) and (6) you need Stata add-ons knnreg and kernreg
* In Stata give command  search knnreg  and  search kernreg

* See also mma9p2npmore.do for more on nonparametric regression (Figures 9.5-9.7)

********** SETUP

di "mma09p1np.do Cameron and Trivedi: Stata nonparametrics with wages and education"
set more off
version 8
set scheme s1mono  /* Graphics scheme */

********** DATA DESCRIPTION
*
* The original data are from the PSID Individual Level Final Release 1993 data
* From www.isr.umich.edu/src/psid  then choose Data Center 
* 4856 observations on 9 variables for Females 30 to 50 years 

* Fixed width data
* intnum    1-4  V30001="1968 INTERVIEW NUMBER" 
* persnum   5-7  V30002="PERSON NUMBER" 
* age       8-9  V30809="AGE OF INDIVIDUAL                     93" 
* educatn  10-11 V30820="G90 HIGHEST GRADE COMPLETED           93" 
* earnings 12-17 V30821="TOTAL LABOR INCOME                    93" 
* hours    18-21 V30823="1992 ANNUAL WORK HOURS                93" 
* sex       22   V32000="SEX OF INDIVIDUAL" 
* kids     23-24 V32022="# LIVE BIRTHS TO THIS INDIVIDUAL" 
* [NOTE: DO NOT USE THE kids VARIABLE AS IT IS NUMBER OF BIRTHS 
*        NOT NUMBER OF KIDS CURRENTLYU IN HOUSEHOLD]
* married   25   V32049="LAST KNOWN MARITAL STATUS" 

********** READ DATA ********** 

* Data are fixed format so use infix
infix intnum 1-4 persnum 5-7 age 8-9 educatn 10-11 earnings 12-17  /* 
  */  hours 18-21 sex 22 kids 23-24 married 25 using psidf3050.dat
summarize

********** MISSING VALUES, DATA TRANSFORMATIONS and SAMPLE SELECTION

* For Highest grade codes the missing codes are 98 DK and 99 NA and 0 inappropriate
* Here treat these as missing
replace educatn = . if (educatn==0 | educatn==98 | educatn==99)

* For marital status the codes are 
*   1 married; 2 Never married; 3 Widowed; 4 Divorced, annulment;
*   5 Separated; 8 NA / DK; 9 No histories 85-93
* Recode 2-5 as not married and treat 8 and 9 as missing
replace married = . if (married==8 | married==9) 
replace married = 0 if married > 1

* For kids the missing codes are 98 DK/NA and 99 no birth history
replace kids = . if (kids==98 | kids==99)
* But do not use these data as it is number of births 
* not number of kids currently in household
* So I drop kids
drop kids 

* Work with positive earnings only
drop if earnings==0
* Topcode women with very high earnings
replace earnings=100000 if earnings>100000 
* Create log hourly wage 
gen hwage = earnings/hours
gen lnhwage = ln(hwage)

* Work with age 36 and nonmissing education data
keep if age == 36
drop if educatn == .
summarize

* Write data to a text (ascii) file so can use with programs other than Stata
outfile intnum persnum age educatn earnings hours sex married hwage /*
   */ lnhwage using mma09p1np.asc, replace

********* ANALYSIS: (1)-(3) NONPARAMETRIC DENSITY ESTIMATES 

set scheme s1mono

* Here give bin width for histogram and kdensity  

* Calculate Silberman's plugin estimate of optimal bandwidth in (9.13)
*   with delta given in Table 9.1 for Epanechnikov kernel
quietly sum lnhwage, detail
global sadj = min(r(sd),(r(p75)-r(p25))/1.349)
di "sadj: " $sadj " iqr/1349: " (r(p75)-r(p25))/1.349 " stdev: " r(sd)
global bwepan2 = 1.3643*1.7188*$sadj/(r(N)^0.2)
di "Bandwidth: " $bwepan2 

* HISTOGRAM ONLY - Figure 9.1
graph twoway (histogram lnhwage, bin(20) bcolor(*.2)), /*
  */ scale (1.2) plotregion(style(none)) /*
  */ title("Histogram for Log Wage") /*
  */ xtitle("Log Hourly Wage", size(medlarge)) xscale(titlegap(*5)) /*
  */ ytitle("Density", size(medlarge)) yscale(titlegap(*5)) /*
  */ legend(pos(10) ring(0) col(1)) legend(size(small)) /*
  */ legend( label(1 "Histogram") label(2 "Kernel")) 
graph save ch9hist, replace
graph export ch9hist.wmf, replace

* COMBINED HISTOGRAM AND KERNEL DENSITY ESTIMATE
graph twoway (histogram lnhwage, bin(20) bcolor(*.2)) /*
  */ (kdensity lnhwage, width($bwepan2) epan2 clstyle(p1)), /* 
  */ title("Histogram and Kernel Density for Log Wage") /*
  */ caption("Note: Kernel is Epanechnikov with bandwidth 0.55") 

* KERNEL DENSITY ESTIMATE FOR 3 BANDWIDTHS - Figure 9.2
global bwonehalf = 0.5*$bwepan2
global btwotimes = 2*$bwepan2
graph twoway (kdensity lnhwage, width($bwonehalf) epan2 clstyle(p2)) /*
  */  (kdensity lnhwage, width($bwepan2) epan2 clstyle(p1)) /*
  */  (kdensity lnhwage, width($btwotimes) epan2 clstyle(p3)), /*
  */ scale (1.2) plotregion(style(none)) /*
  */ title("Density Estimates as Bandwidth Varies") /*
  */ xtitle("Log Hourly Wage", size(medlarge)) xscale(titlegap(*5)) /* 
  */ ytitle("Kernel density estimates", size(medlarge)) yscale(titlegap(*5)) /* 
  */ legend(pos(1) ring(0) col(1)) legend(size(small)) /*
  */ legend( label(1 "One-half plug-in") label(2 "Plug-in") /*
  */         label(3 "Two times plug-in"))  
graph save ch9kd1, replace
graph export ch9kd1.wmf, replace

* KERNEL DENSITY ESTIMATE FOR 4 DIFFERENT KERNELS - Figure 9.4
* Calculate Silberman's plugin optimal bandwidths using (9.13)
* with delta given in Table 9.1 for the different kernels

* Use sadj calculated earlier for Epanecnnikov
global bwgauss = 1.3643*0.7764*$sadj/(_N^0.2)
global bwbiweight = 1.3643*2.0362*$sadj/(_N^0.2)
global bwrectang = 0.5*1.3643*1.3510*$sadj/(_N^0.2)
di "Usual Epanechnikov (epan2):      " $bwepan2 
di "Gaussian:                        " $bwgauss 
di "Quartic or biweight:             " $bwbiweight
di "Uniform or rectangular:          " $bwrectang
graph twoway (kdensity lnhwage, width($bwepan2) epan2) /*
  */ (kdensity lnhwage, width($bwgauss) gauss) /*
  */ (kdensity lnhwage, width($bwbiweight) biweight) /* 
  */ (kdensity lnhwage, width($bwrectang) rectangle), /*
  */ scale (1.2) plotregion(style(none)) /*
  */ title("Density Estimates as Kernel Varies") /*
  */ xtitle("Log Hourly Wage", size(medlarge)) xscale(titlegap(*5)) /* 
  */ ytitle("Kernel density estimates", size(medlarge)) yscale(titlegap(*5)) /* 
  */ legend(pos(3) ring(0) col(1)) legend(size(small)) /*
  */ legend( label(1 "Epanechnikov (h=0.545)") label(2 "Gaussian (h=0.246)") /*
  */         label(3 "Quartic (h=0.646)") label(4 "Uniform (h=0.214)"))
graph save ch9kdensu1, replace
graph export ch9kdensu1.wmf, replace

* SHOW THAT STATA EPANECHNIKOV = USUAL EPANECHNIKOV
* Once divide usual Epanechnikov bandwidth by sqrt(5). 
* (Pagan and Ullah (1999, p.28) have formulae.)
global bwepan = $bwepan2/sqrt(5)
graph twoway (kdensity lnhwage, width($bwepan2) epan2) /*
   */  (kdensity lnhwage, width($bwepan) epan), /*
   */  title("Epan = Epan2 if bandwidth adjusted") /*
   */  legend( label(1 "Usual Epanechnikov") label(2 "Stata Epanechnikov"))   


********* ANALYSIS: (4) LOWESS NONPARAMETRIC REGRESSION ESTIMATES

* LOWESS WITH DEFAULT BANDWIDTH of 0.8 
lowess lnhwage educatn

* LOWESS REGRESSION WITH BANDWIDTHS of 0.1, 0.4 and 0.8 - Figure 9.3
graph twoway (scatter lnhwage educatn, msize(medsmall) msymbol(o)) /*
  */ (lowess lnhwage educatn, bwidth(0.8) clstyle(p2)) /*
  */ (lowess lnhwage educatn, bwidth(0.4) clstyle(p1)) /* 
  */ (lowess lnhwage educatn, bwidth(0.1) clstyle(p3)), /*  
  */ scale (1.2) plotregion(style(none)) /*
  */ title("Nonparametric Regression as Bandwidth Varies") /*
  */ xtitle("Years of Schooling", size(medlarge)) xscale(titlegap(*5)) /* 
  */ ytitle("Log Hourly Wage", size(medlarge)) yscale(titlegap(*5)) /*
  */ legend(pos(12) ring(0) col(2)) legend(size(small)) /*
  */ legend( label(1 "Actual data") label(2 "Bandwidth h=0.8") /*
  */         label(3 "Bandwidth h=0.4") label(4 "Bandwidth h=0.1"))
graph save ch9ksm1, replace
graph export ch9ksm1.wmf, replace

********* ANALYSIS: (5) EXTRA: K-NEAREST NEIGHBORS NONPARAMETRIC REGRESSION

* NEAREST NEIGHBOURS REGRESSION USING LOWESS
* Use lowess with mean and noweight options to give running means = centered kNN
global knnbwidth = 0.3
di "knn via Lowess uses following % of sample: " $knnbwidth
lowess lnhwage educatn, bwidth($knnbwidth) mean noweight

* LOWESS COMPARED TO NEAREST NEIGHBOURS
graph twoway (lowess lnhwage educatn, bwidth(0.3) mean noweight) /*
  */ (lowess lnhwage educatn, bwidth(0.3)), /*
  */ title("Centered kNN versus Lowess") /*
  */ legend( label(1 "Centered kNN") label(2 "Lowess 0.8"))

* NEAREST NEIGHBOURS REGRESSION USING KNNREG COMPARED TO USING LOWESS
* knnreg is a Stata add-on (in Stata search knnreg to find and download)
* Here we verify that same as lowess knn except knnreg drops endpoints
global k = round($knnbwidth*_N)
di "knnreg uses following number of neighbours: " $k
knnreg lnhwage educatn, k($k) gen(knnregpred) ylabel nograph
lowess lnhwage educatn, bwidth($knnbwidth) gen(knnlowesspred) mean noweight nograph
* Following shows that the same except knnreg drops endpoints and lowess does not
sum knnlowesspred knnregpred
corr knnlowesspred knnregpred

********* ANALYSIS: (6) EXTRA: KERNEL NONPARAMETRIC REGRESSION

* KERNEL REGRESSION
* Kercode 1 = Uniform; 2 = Triangle; 3 = Epanechnikov; 4 = Quartic (Biweight);
*         5 = Triweight; 6 = Gaussian; 7 = Cosinus
* bwidth(#) defines width of the weight function window around each grid point.
* npoint(#) specifies the number of equally spaced grid points over range of x.
* Here bwidth(3) gives e.g. positive weight from x=4 to x=10 if current x0=7  
kernreg lnhwage educatn, bwidth(3) kercode(3) npoint(100) ylabel gen(kernregpred1 xkernreg)
graph twoway (lowess lnhwage educatn, bwidth(0.5) clstyle(p2)) /*
  */ (line kernregpred xkernreg, clstyle(p1)), /*
  */ title("Lowess versus kernel regression") /*
  */ legend( label(1 "Lowess") label(2 "Kernreg"))

********** CLOSE OUTPUT
log close
* clear
* exit
