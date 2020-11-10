* MMA07P3MONTECARLO.DO  March 2005 for Stata version 8.0

log using mma07p3montecarlo.txt, text replace

********** OVERVIEW OF MMA07P3MONTECARLO.DO **********

* STATA Program 
* copyright C 2005 by A. Colin Cameron and Pravin K. Trivedi 
* used for "Microeconometrics: Methods and Applications" 
* by A. Colin Cameron and Pravin K. Trivedi (2005)
* Cambridge University Press 

* Chapter 7.7.1-7.7.5 pp. 250-4
* Size and power of the Wald test
 
* (1) Figure 7.2 Density of Wald test statistic
* (2) Table 7.2  Actual size of Wald test at various nominal sizes
* (3) Table 7.2  Actual power of Wald test at various nominal sizes
* (4) Table 7.2  Nominal power of Wald test at various nominal sizes
* (5) Alternative way to simulate using postfile rather than simulate

* on the slope coefficient for a Probit model with simulated data (see below).

* NOTE: Because this is a simulation using many samples (here 10,000)
* the generated data are not saved in a text file.

* Problem can arise if in one of the simulations all of sample is y=0 or y=1
* Then the probit model is not estimable.
* Then need increase sample size, change dgp or reduce number of simulations.
* Here used N=40 with S=10000 for size and for power
* Another possible change is to have same regressors x across simulations

********** SETUP ********** 

set more off
version 8.0
set scheme s1mono  /* Graphics scheme */

********** MONTE CARLO OVERVIEW ********** 

* The data generating process is 
* - Probit with Pr[y=1] = Phi(b1 + b2*x2) 
* - where b1 = 0 and b2 = 1
* - and regressor x ~ N[0,1] is fixed throughout the simulations 

* The sample size N set below in the global numobs
* The number of simulations S is set below in the global numsims
* A third option is to switch to same x in each sample. This needs to be done manually.

* The simulation is done using stata command simulate
* At the end of the program, an alternative using postfile is given

* The program investigates both size and power 
* of the Wald test that b2 = 1.
* For power the dgp instead uses b2 = 2.

********** INITIAL SIMULATION SET UP ********** 

set seed 10101
* Change the following for different sample size N
global numobs "40"
* Change the following for different number of simulations S
global numsims "10000"

****** ANALYSIS: SIMULATION OF PROBIT MODEL SLOPE ESTIMATES AND WALD TEST

* The program is rclass.
* This means the results returned by the program are put into r( )
* Here we return meany, vary, betahat, sebetahat, ztestforbetaeq1

* The probit model is Pr[y=1] = Phi(b1 + b2*x2) where b1=0 and b2=1
* For size calculations:  b2 = 1
* For power calculations: b2 = 1.5   (as an example)
* So pass the argument trueb2 as an argument.

* The following three lines are only needed 
* if the regressors are constant across simulations,
* as then need to generate once and put in a data file to be reused.
* They are commented out here as here (x,y) both resampled.
* Also simprobit and simprobit2 need one line changed if x is fixed.
/*
set obs numobs     
gen x = invnorm(uniform())
save xforsim, replace
*/
* This version of the program instead redraws both x and y in each simulation

* The program has one argument
*   - trueb2 = value of b2 in the dgp 

program simprobit, rclass
    version 8.0 
    /* define arguments. Here trueb2 = b2 in Phi(b1 + b2*x2) */ 
    args trueb2
    /* Generate the data: here x and y */ 
    drop _all
    set obs $numobs
    gen x = invnorm(uniform())
    /* If instead want same x in each simulation,
       replace above line with:  use xforsim */
    gen y = 0
    replace y = 1 if 0 + `trueb2'*x + invnorm(uniform()) > 0
    /* Summarize the generated data as a check */
    summarize y
    return scalar ymean=r(mean) 
    return scalar yvar=r(Var)
    /* Do probit and store key results */
    probit y x
    return scalar b2hat=_b[x]
    return scalar seb2hat = _se[x]
    return scalar ztestforb2eq1 = (_b[x]-1)/_se[x]
end

****** (1) DISTRIBUTION OF WALD TEST STATISTIC (Figure 7.2 p.253)

* Now call the program simprobit where
*  - include values for each argument within the quotes "   "
*    (here the argument is b2true and is set to 1 for size and 1.5 for power)
*  - make sure that ask for each of the returned results

* For size calculations set trueb2 = 1
simulate "simprobit 1" ymean=r(ymean) yvar=r(yvar) b2hat=r(b2hat) /* 
  */ seb2hat=r(seb2hat) ztestforb2eq1=r(ztestforb2eq1), reps($numsims) 

* Summary of the results returned by simulate
* For Wald test key output is ztestforb2eq1
describe 
summarize

* For b2hat there are two ways to estimate the standard deviation.
* One is the average of seb2hat, the standard error of b2hat
* The other is the standard deviation of b2hat. 
* These are equal asymptotically, but perhaps not in small samples due to bias. 
* Also aveseb2hat is used later in calculating asymptotic power.
sum seb2hat
scalar aveseb2hat = r(mean)
sum b2hat
scalar stdevb2hat = r(sd)
di "Average standard error of b2hat:  " aveseb2hat
di "Standard deviation of b2hat:      " stdevb2hat

* The Wald test statistic will be called Wald
gen Wald = ztestforb2eq1
label var Wald "Wald test statistic" 

* The mean and st.dev. should be 0 and 1 if Wald ~ N[0,1]
sum Wald

* The 2.5 and 97.5 percentiles should be -1.96 and 1.96 if Wald ~ N[0,1]
* They can be used to get size-adjusted Wald test at 5 percent.
_pctile Wald, p(2.5,99.5)
display "Wald: Lower 2.5 percentile = " r(r1) "  Upper 2.5 percentile = " r(r2) 

* The density of the simulated values of the Wald test should be 
* a standard normal density if Wald ~ N[0,1]
* The following plots kernel estimate of density of Wald and a N[0,1] density
* Could also do Student[N-k] but this looks same as N[0,1] if N>=30.
gen N01density = normden(Wald)
sum Wald

graph twoway (kdensity Wald, range(-3 3) clstyle(p1)) /*
  */  (connect N01density Wald if Wald>-3 & Wald<3, clstyle(p2) sort(Wald) s(i)),  /*
  */ scale (1.2) plotregion(style(none)) /*
  */ title("Monte Carlo Simulations of Wald Test") /*
  */ xtitle("Wald Test Statistic", size(medlarge)) xscale(titlegap(*5)) /* 
  */ ytitle("Density", size(medlarge)) yscale(titlegap(*5)) /* 
  */ legend(pos(11) ring(0) col(1)) legend(size(small)) /*
  */ legend( label(1 "Monte Carlo") label(2 "Standard Normal") /*
  */         label(3 "Test size = 0.01")) 
graph export ch7montecarlo.wmf, replace

****** (2) ACTUAL SIZE OF THE WALD TEST STATISTIC (Table 7.2, p.253)

* Obtain the size properties of a two-sided Wald test 
* That rejects if |Wald| > z_alpha/2  where alpha = .01, .05, .1, .2

* Convert to two-sided test by taking absolute value
gen absWald = abs(Wald)

* Give key percentiles of |Wald|
* Percentiles must be in ascending order for Stata
_pctile absWald, p(0.80,0.90,0.95,0.99)
display "I[Upper percentiles of |Wald|: " " 1 " r(r4) " 5 " r(r3) " 10 " r(r2) " 20 " r(r1) 

* Program to calculate actual size given nominal size
* Temporary variables and scalars are in quotes ` '
program size, rclass
     version 8.0 
     args nominalsize
     tempvar reject
     tempname normalcriticalvalue
     quietly {
        scalar `normalcriticalvalue' = invnorm(1-(`nominalsize'/2))
        gen `reject' = 0
        replace `reject' = 1 if absWald > `normalcriticalvalue'
        summarize `reject'
        return scalar actualsize = r(mean)
     } 
end

* Calculate actual size for nominal sizes 0.01, 0.05, 0.10 and 0.20
size 0.01
scalar actualsize01 = r(actualsize) 
size 0.05
scalar actualsize05 = r(actualsize) 
size 0.10
scalar actualsize10 = r(actualsize) 
size 0.20
scalar actualsize20 = r(actualsize) 

* Following gives Actual Size column of Table 7.2 (p.253)
* Nominal Sizes and Actual Sizes of Two-sided Wald Test
di "0.01: " actualsize01 _new "0.05: " actualsize05 _new /*
  */  "0.10: " actualsize10 _new "0.20: " actualsize20 

****** (3) ACTUAL POWER OF THE WALD TEST STATISTIC (Table 7.2, p.253)

* Consider power when b2 = 2 rather than 1

* Obtain the actual power by simulation 
* Use the same program simprobit as for size,  
* except the argument b2true is 2.0 rather than 1.0

drop _all

* For size calculations set trueb2 = 2
simulate "simprobit 2" ymean=r(ymean) yvar=r(yvar) b2hat=r(b2hat) /* 
  */ seb2hat=r(seb2hat) ztestforb2eq1=r(ztestforb2eq1), reps(10000) 

* Calculate |Wald| 
gen Wald = ztestforb2eq1
gen absWald = abs(Wald)

summarize 

* Calculate actual power for nominal sizes 0.01, 0.05, 0.10 and 0.20
* This can use the earlier program size
size 0.01
scalar actualpower01 = r(actualsize) 
size 0.05
scalar actualpower05 = r(actualsize) 
size 0.10
scalar actualpower10 = r(actualsize) 
size 0.20
scalar actualpower20 = r(actualsize) 

* Following gives Actual Power column of Table 7.2 (p.253)
* Nominal Sizes and Actual Power of Two-sided Wald Test
di "0.01: " actualpower01 _new "0.05: " actualpower05 _new /*
  */ "0.10: " actualpower10 _new "0.20: " actualpower20 

****** (4) ASYMPTOTIC POWER OF THE WALD TEST STATISTIC (Table 7.2, p.253)

* Consider power when b2 = 2 rather than 1

* Calculate asymptotic theoretical power using noncentral chisquare
* Asymptotic power = Pr[W > chi-square(alpha) | W ~ noncentral chi-square(alpha,ncp)
* The noncentrality parameter is 0.5*(delta^2)/(se[b2]^2)
* Here size has b2 = 1 and power has b2 = 1+delta
* So delta = b2true - 1. 
* Need to find the standard error of b2. 
* Use the average from earlier simulations.

* Program to calculate asymptotic power given nominal size
* Temporary variables and scalars and arguments are in quotes ` '
* invchi2tail gives cv such that Pr(Chi2 > cv) = nominalsize
* Power is 1 minus cdf of noncentral chisquare 
* nchi2 gives the cdf of noncentral chisquare

drop _all

* Arguments are alpha (size), lamda and df (degrees of freedom)
program power, rclass
     version 8.0 
     args alpha lamda df
     tempname criticalvalue powervianoncentralchi
     quietly {
        scalar `criticalvalue' = invchi2tail(`df',`alpha')
        scalar `powervianoncentralchi' = 1-nchi2(`df',`lamda',`criticalvalue')  
        return scalar asymppower = `powervianoncentralchi'
     } 
end

* scalar criticalvalue = invchi2tail(df,alpha)   
* replace power = 1-nchi2(df,lamda,criticalvalue)  

* Calculate df and lamda.
* This uses an estimate of se[beta] obtained earlier
scalar delta = 1     /* Here 2 - 1. Changes for different alternatives */
scalar lamda = 0.5*(delta*delta)/(aveseb2hat*aveseb2hat)
scalar df = 1
di "delta: " delta "  aveseb2hat: " aveseb2hat "  lamda: " lamda "  df: " df 

* Calculate asymptotic power for nominal sizes 0.01, 0.05, 0.10 and 0.20
power 0.01 lamda df
scalar asymppower01 = r(asymppower) 
power 0.05 lamda df
scalar asymppower05 = r(asymppower) 
power 0.10 lamda df
scalar asymppower10 = r(asymppower) 
power 0.20 lamda df
scalar asymppower20 = r(asymppower) 
   
* Following gives Asymptotic Power column of Table 7.2 (p.253)
* Nominal Sizes and Asymptotic Power of Two-sided Wald Test
di "0.01: " asymppower01 _new "0.05: " asymppower05 _new /*
  */ "0.10: " asymppower10 _new "0.20: " asymppower20 

****** (5) ALTERNATIVE ANALYSIS: SIMULATION METHOD USING POSTFILE

* This is an alternative, given for completeness.
* This fails if the model is not estimable in any of the simulation samples.
* By contrast, simulate just drops that simulation sample and continues simulating.

* For each round of the simulation, the variables in `sim' are sent 
* as a new line to a stata data set simprobitresults.
* The names of these variables are given in quotes after S_1
* Need as many names in quotes after S_1 as variables at post
* Then can analyze these using summarize etcetera

* This program has two arguments
*   - numsims = desired number of simulations
*   - trueb2  = slope coefficient used to generate the data

drop _all 

program simprobit2
     version 8.0 
     args numsims trueb2 
     tempname sim
     postfile `sim' meany vary beta sterror ztestforbeta using probitsimresults, replace
     quietly {
        forvalues i = 1/`numsims' {
            drop _all
            set obs $numobs      /* may need to change */
            gen x = invnorm(uniform())
            /* If instead want same x in each simulation 
               replace above line with:  use xforsim   */
            gen y = 0
            /* Use b2 = 1.0 for size and 1.5 for power */
            replace y = 1 if 0+`trueb2'*x+invnorm(uniform()) > 0
            summarize y
            scalar meany=r(mean) 
            scalar vary=r(Var)
            probit y x
            scalar beta=_b[x]
            scalar sterror = _se[x]
            scalar ztestforbeta = (beta-1)/sterror
     post `sim' (meany) (vary) (beta) (sterror) (ztestforbeta)
        }
     } 
     postclose `sim'
end

simprobit2 $numsims 1
use probitsimresults, clear

* Here we just summarize results for comparison with earlier
* But could do the further analysis as above
sum

********** CLOSE OUTPUT **********
log close
* clear
* exit
