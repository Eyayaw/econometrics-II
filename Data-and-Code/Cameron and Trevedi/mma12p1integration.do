* MMA12P1INTEGRATION.DO  March 2005 for Stata Version 8.0

log using mma12p1integration.txt, text replace

********** OVERVIEW OF MMA12P1INTEGRATION.DO **********

* STATA Program 
* copyright C 2005 by A. Colin Cameron and Pravin K. Trivedi 
* used for "Microeconometrics: Methods and Applications" 
* by A. Colin Cameron and Pravin K. Trivedi (2005)
* Cambridge University Press 

* Chapter 12.3.3 pages 391-2
* Computes integral numerically and by simulation
* (1) Illustrate Midpoint Rule (page 392)
* (2) Illustrate Monte Carlo integral (Table 12.1 page 392)
* 
* for computing E[x] and E[exp(-exp(x))] for x ~ N[0,1] 

* No data need be read in.

********** SETUP **********

set more off
version 8.0

********** (1) NUMERICAL INTEGRATION USING MIDPOINT RULE **********

* Midpoint rule for n evaluation points between a and b is
*   Integral = Sum (j=1 to n) [(b-a)/n]*f(xbar_j)
* where xbar_j is midpoint between x_j-1 and x_j

program midpointrule, rclass
    version 8 
    /* define arguments. Here trueb2 = b2 in Phi(b1 + b2*x2) */ 
    args neval a b 
    drop _all
    scalar increment = (`b'-`a') / `neval'
    set obs `neval'
    /* Compute the function of interest */
    gen xbar = `a' - 0.5*increment + increment*_n
    gen density = exp(-xbar*xbar/2)/sqrt(2*_pi)
    * Following is contribution to E[x] when x ~ N[0,1] 
    gen f1xbar = xbar*density  
    * Following is contribution to E[exp(-exp(x))] when x ~ N[0,1]
    gen f2xbar = exp(-exp(x))*density
    /* Compute the averages */
    quietly sum f1xbar
    scalar Ex = r(sum)*increment
    quietly sum f2xbar
    scalar Eexpminexpx = r(sum)*increment
    /* Print results */
    di "Evaluation points: " `neval' " over range: (" `a' "," `b' ")
    di "Midpoint rule estimate of E[x] is:  " Ex
    di "Midpoint rule estimate of E[exp(-exp(x))] is:  " Eexpminexpx
end

midpointrule 20 -5 5 
midpointrule 200 -5 5 
midpointrule 2000 -5 5 

********** (2) MONTE CARLO INTEGRATION USING DRAWS FROM DENSITY OF X **********

* To get E[g(x)] 
* make draws from N[0,1], compute g(x), and average over draws 

program simintegration, rclass
    version 8 
    /* define arguments. Here trueb2 = b2 in Phi(b1 + b2*x2) */ 
    args nsims
    /* Generate the data: here x */ 
    drop _all
    set obs `nsims'
    set seed 10101
    gen x = invnorm(uniform()) 
    /* Compute the function of interest */
    gen f1x = x     /* For E[x] just need x */
    gen f2x = exp(-exp(x))   /* For E[exp(-exp(x))] */
    /* Compute the averages */
    quietly sum f1x
    scalar Ex = r(mean)
    quietly sum f2x
    scalar Eexpminexpx = r(mean)
    di "Number of simulations: " `nsims'
    di "Monte Carlo estimate of E[x] is:  " Ex
    di "Monte Carlo estimate of E[exp(-exp(x))] is:  " Eexpminexpx
end

* Note a different program was used to obtain Table 12.1 on page 392
* So results will differ somewhat from text, except for very high number of simulations

simintegration 10
simintegration 25
simintegration 50
simintegration 100
simintegration 500
simintegration 1000
simintegration 1000
simintegration 100000
clear
set mem 20m
simintegration 1000000

********** CLOSE OUTPUT **********
log close
clear
exit

