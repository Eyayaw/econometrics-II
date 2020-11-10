* MMA08P28NONNESTED.DO   March 2005 for Stata version 8.0

log using mma08p2nonnested.txt, text replace

********** OVERVIEW OF MMA08P2NONNESTED.DO **********

* STATA Program 
* copyright C 2005 by A. Colin Cameron and Pravin K. Trivedi 
* used for "Microeconometrics: Methods and Applications" 
* by A. Colin Cameron and Pravin K. Trivedi (2005)
* Cambridge University Press 

* Chapter 8.5.3 pages 283-4
* Nonnested model comparison given in Table 8.2:

*  (A) AIC AND VARIATIONS
*  (B) VUONG TEST for Overlapping Models
* for a Poisson model with simulated data (see below).

* This example requires the free Stata add-on command rndpoix. 
* In Stata: search rndpoix

********** SETUP **********

set more off
version 8.0
set scheme s1mono   /* Used for graphs */
  
********** GENERATE DATA ********** 
 
* Dgp is
*   y ~ Poisson[exp(b1 + b2*x2 + b3*x3]
* where 
*    x2, x3 is iid ~ N[0,1]
* and b1=0 and b2=1 and b3=1.

* The Models compared are 
* Poisson of y on x2
* Poisson of y on x3 and x3^2 

set seed 10001
set obs 100
scalar b1 = 0.5
scalar b2 = 0.5
scalar b3 = 0.5

* Generate regressors
gen x2 = invnorm(uniform())
gen x3 = invnorm(uniform())
gen x2sq = x2*x2
gen x3sq = x3*x3

* Generate y
gen mupoiss = exp(b1+b2*x2+b3*x3)
* The next requires Stata add-on. In Stata: search rndpoix
rndpoix(mupoiss)
gen y = xp

* Write data to a text (ascii) file so can use with programs other than Stata
outfile y x2 x3 x2sq x3sq using mma08p2nonnested.asc, replace

********* SETUP FOR THIS PROGRAM *********

* Change this if want different regressors
* Here both models differ from the dgp
* The Vuong test below assumes that the two models are OVERLAPPING
global XLISTMODEL1 x2
global XLISTMODEL2 x3 x3sq

********* (A) AIC AND VARIATIONS *********

* Stata output from Poisson saves much of this.
* Also calculate manually.

* The following code can be changed to different models than poisson
* provided 
* ereturn list yields N = e(N); q = e(k); and LnL = e(ll)
* We use AIC = -2lnL+2q; BIC = -2lnL+lnN*q; CAIC = -2lnL+(1+lnN)*q

poisson y $XLISTMODEL1
estimates store model1
scalar ll1 = e(ll)
scalar q1 = e(k)
scalar N1 = e(N)
scalar aic1 = -2*ll1 + 2*q1
scalar bic1 = -2*ll1 + ln(N1)*q1
scalar caic1 = -2*ll1 + (1 + ln(N1))*q1

poisson y $XLISTMODEL2
estimates store model2
scalar ll2 = e(ll)
scalar q2 = e(k)
scalar N2 = e(N)
scalar aic2 = -2*ll2 + 2*q2
scalar bic2 = -2*ll2 + ln(N2)*q2
scalar caic2 = -2*ll2 + (1 + ln(N2))*q2

* Display results given in first three rows of Table 8.2 page 284

estimates table model1 model2, stats(N k ll aic bic)

di "Model 1: " _n  "lnL:  " ll1  " q: " q1 _n " N: " N1
di "-2lnL: " -2*ll1 _n "AIC:  " aic1  _n " BIC: " bic1 _n "caic: " caic1

di "Model 2: " _n  "lnL:  " ll2  " q: " q2 _n " N: " N2
di "-2lnL: " -2*ll2 _n "AIC:  " aic2  _n " BIC: " bic2 _n "caic: " caic2

********* (B) VUONG TEST FOR OVERLAPPING MODELS *********

* The test has three variants
* (1) Nested models: G is contained in F 
* (2) Strictly non-nested models: F intersection G equals null set
* (3) Overlapping models: F intersection G does not equal null set

* Need to compute lnf(y) for models 1 and 2, 
* where density f is model 1 and density g is model 2

* The procedures will vary with model. Here use Poisson.

* (0) COMPUTE THE LR TEST STATISTIC

* This is LR = Sum_i [ ln (fy1_i / gy2_i) ]
*            = Sum_i lnfy1_i - Sum_i lngy2_i
*            = difference in log-likelihood for the two models

* Easiest if program output gives logL
* Otherwise need to generate manually

quietly poisson y $XLISTMODEL1
scalar llf = e(ll)
quietly poisson y $XLISTMODEL2
scalar llg = e(ll)
scalar LR = llf - llg
di "LR = " LR "  and llf = " llf " llg = " llg

* (1) NESTED MODELS

* Not done here as not relevant for the example of this application.

* (1A) Usual LR test if assume densities correctly specified.

* (1B) If instead want robustified version then need to compute W 
* and use the weighted chi-square test.
* This is not the appropriate test here,
* but in 3(A) below W is computed and a weighted chi-square test used. 
* This code could be easily adapted to here.

* (2) STRICTLY NON-NESTED MODELS

* Not done here as not relevant for the example of this application. 
* Test uses LR/what ~ normal where what is computed in 3(B) below.

* (3) OVERLAPPING MODELS

* This is the relevant test here
* First test whether overlapping (even though here know that is)
* THen do the test

* (3A-1) Compute what^2 

* Calculate what^2 
* = (1/N)*Sum_i[ln(fy1_i/gy2_i)^2] - [(1/N)*Sum_i[ln(fy1_i/gy2_i)]^2
* = (1/N) * Sum_i [(ln(fy1_i) - ln(gy2_i))^2] - (LR/N)^2  

* For the Poisson
*      f(y) = exp(-mu)*mu^y/y!
* so lnf(y) = -mu + y*ln(mu) - lny!
quietly poisson y $XLISTMODEL1
predict yhatf
* Poisson default predict gives yhat = exp(x'b)
gen lnf = -yhatf + y*ln(yhatf) - lnfact(y)
quietly poisson y $XLISTMODEL2
predict yhatg
gen lng = -yhatg + y*ln(yhatg) - lnfact(y)
gen lnratiosq = (lnf-lng)^2
sum lnratiosq
scalar whatsq = r(sum)/_N - (LR/_N)^2
scalar Nwhatsq = _N*whatsq
di "First-stage test statistic whatsq - still need to find critical value"
di "N*omegahatsq = " Nwhatsq

* Aside: Check by recomputing LR this long way
gen lnratio = (lnf-lng)
sum lnratio
scalar LRcheck = r(sum)

*** Display results given in second last row of Table 8.2 page 284

di "LR = " LR "  and LRcheck = " LRcheck

* (3A-2) Find the critical value by first find W, then eigenvalues lamda, then simulate

* Calculate estimate of the W matrix on page ?? of Vuong. 
* (a) Can estimate Af = E[d2lnf(y)/dbdb'] as inverse of usual ML variance matrix
* (b) Since the robust ML variance matrix is V = Ainv*B*Ainv 
*     can estimate Bf = -E[dlnf(y)/dbxdlnf(y)/db'] by A*V*A   where A is in (a) 
* (c) For Ag same as in part (a) except for model g
* (d) For Bg same as in part (a) except for model g
* (e) The only tricky bit is computation of Bfg

gen one = 1
* (a) Af
quietly poisson y one $XLISTMODEL1, noconstant
matrix Af = syminv(e(V))
* (b) Bf
quietly poisson y one $XLISTMODEL1, noconstant robust
* robust gives Ainv*B*Ainv so pre and post multiply by A gives B
* Also make adjustment s Stata divides by (_N-1). Here use _N.
matrix Bf = Af*e(V)*Af*(_N-1)/_N
* (c) Ag
quietly poisson y one $XLISTMODEL2, noconstant
matrix Ag = syminv(e(V))
* (d) Bg
quietly poisson y one $XLISTMODEL2, noconstant robust
matrix Bg = Ag*e(V)*Ag*(_N-1)/_N

* (e) Bfg requires more specialized code pecuuliar to this example
*     For Poisson  dlnf(y)/db  = Sum_I (y_i - mu_i)*x_i 
*     so Bfg = (1/N)*Sum_i [(y_i - muf_i)*xf_i]*[(y_i - mug_i)*xg_i]'  
* For model 1 x is intercept and x2  (global XLISTMODEL1 x2)
gen bf1 = (y - yhatf)         /* yhatf saved earlier = y - muf */
gen bf2 = (y - yhatf)*x2
* For model 2 x is intercept, x3 and x3sq  (global XLISTMODEL2 x3 x3sq)
gen bg1 = (y - yhatg)         /* yhatg saved earlier = y - mug */
gen bg2 = (y - yhatg)*x3
gen bg3 = (y - yhatg)*x3sq
* Create Bfg 
matrix accum BfBg = bf1 bf2 bg1 bg2 bg3, noconstant
* and Bfg is the (1,2) submatrix: rows 1 to 2 and columns 3 to 5
matrix Bfg = BfBg[1..2,3..5]

* Form the matrix W
* Note there is no need for minus sign as A has been defined as -A
matrix W11 = Bf*syminv(Af)
matrix W12 = Bfg*syminv(Ag)
matrix W21 = Bfg'*syminv(Af)
matrix W22 = Bg*syminv(Ag)
matrix W = W11,W12\W21,W22
matrix list W

* Calculate the eigenvalues of W
matrix eigenvalues reigvalW ceigvalW = W
* Real eigenvalues
matrix list reigvalW
* Complex eigenvalues - hopefully none
matrix list ceigvalW

* This gives the vector lamda of eigenvalus of W
matrix lamda = reigvalW
scalar l1 = lamda[1,1]
scalar l2 = lamda[1,2]
scalar l3 = lamda[1,3]
scalar l4 = lamda[1,4]
scalar l5 = lamda[1,5]

* Now obtain the p-value and critical value at level 0.05
preserve
* Obtain the 5 percent critical value by simulating 10000 draws from 
* M_p+q(lamda) = Sum_j lamda*j*z_j^2 where z_j are N[0,1] so z_j^2 are chi(1)
set seed 10101
set obs 10000
gen randomdraw = l1*invnorm(uniform())^2 + l2*invnorm(uniform())^2 +  /*
  */ l3*invnorm(uniform())^2 + l4*invnorm(uniform())^2 + l5*invnorm(uniform())^2 
gen indicator = Nwhatsq >= randomdraw
quietly sum indicator
di "p-value for the Omegahatsq test = "  1-r(mean)
sum randomdraw, detail
di "Reject overlapping at level .05 if N*omegahatsq exceeds " r(p95)
restore
di "where N*omegahatequals " Nwhatsq 
di "If reject then continue to second step."
di "Otherwise stop as cannot determine whether models are overlapping."

* (3B) Do the second stage test if reject at (3A)
gen TLR = (LR/sqrt(whatsq))/sqrt(_N)

*** Display results given in second last row of Table 8.2 page 284

di "TLR is N[0,1]. Here TLR = " TLR
di "Two-tailed test p-value: " chi2tail(1,TLR^2)

********** CLOSE OUTPUT **********
log close
clear
exit

