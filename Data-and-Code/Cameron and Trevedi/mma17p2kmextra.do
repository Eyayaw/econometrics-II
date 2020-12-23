* MMA17P2KMEXTRA.DO  March 2005 for Stata version 8.0

log using mma17p2kmextra.txt, text replace

********** OVERVIEW OF MMA17PP2KMEXTRA.DO **********

* STATA Program 
* copyright C 2005 by A. Colin Cameron and Pravin K. Trivedi 
* used for "Microeconometrics: Methods and Applications" 
* by A. Colin Cameron and Pravin K. Trivedi (2005)
* Cambridge University Press 

* Chapter 17.5.1 pages 581-2
* Nonparametric Survival Analysis
* Provides  
*   (1) K-M Survivor Function and N_A Cum Hazard Estimates (Table 17.2)
* using artificial data

********** SETUP **********

set more off
version 8.0
set scheme s1mono   /* Used for graphs */
  
********** GENERATE DATA **********

* The time does not matter except for the hazard.
* Here arbitrarily let durations be 1, 4, 6, 11 and 20 (so irregularly spaced)
* 1. At t = 10 (time t1):  6 failures
* 2. At t = 15:            4 censored (lost) between t1 and t2
* 3. At t = 20 (time t2):  5 failures
* 4. At t = 25:            3 censored (lost) between t2 and t3  
* 3. At t = 30 (time t3):  2 failures 
* 4. At t = 35:            1 censored (lost) between t3 and t4  
* 3. At t = 40 (time t4):  1 failures 
* 4. At t = 45:            32 failures (lost) between t4 and t5			
* 5. At t = 50 (time t5):  26 censored 

* Indicator failed = 1 if fail and 0 if censored
input duration failed
  10  1
  10  1
  10  1
  10  1
  10  1
  10  1
  15  0
  15  0
  15  0
  15  0
  20  1
  20  1
  20  1
  20  1
  20  1
  25  0
  25  0
  25  0
  30  1
  30  1
  35  0
  40  1
  45  0
  45  0
  45  0
  45  0
  45  0
  45  0
  45  0
  45  0
  45  0
  45  0
  45  0
  45  0
  45  0
  45  0
  45  0
  45  0
  45  0
  45  0
  45  0
  45  0
  45  0
  45  0
  45  0
  45  0
  45  0
  45  0
  45  0
  45  0
  45  0
  45  0
  45  0
  45  0
  50  1
  50  1
  50  1
  50  1
  50  1
  50  1
  50  1
  50  1
  50  1
  50  1
  50  1
  50  1
  50  1
  50  1
  50  1
  50  1
  50  1
  50  1
  50  1
  50  1
  50  1
  50  1
  50  1
  50  1
  50  1
  50  1
end

sum

***** COMPUTATION USING STATA **********

* Stata st curves require defining the dependent variable
stset duration, fail(failed=1)
stsum
stdes

* K-M survival graph
* sts graph, gwood

* N-A Cumulative Hazard
* sts graph, cna

* Kaplan-Meier Survivor Function listed (last column Table 17.2)
sts list

* Nelson-Aalen Cumulative Hazard Listed (second last column Table 17.2)
sts list, na

***** MANUAL COMPUTATION AS IN TABLE 17.2 (page 582) **********

scalar cumhaz1 = 6/80
scalar cumhaz2 = 6/80 + 5/70
scalar cumhaz3 = 6/80 + 5/70 + 2/62
scalar surv1 = 1-6/80
scalar surv2 = (1-6/80)*(1-5/70)
scalar surv3 = (1-6/80)*(1-5/70)*(1-2/62)
di "Cumulative hazard at   t1: " cumhaz1  " at t2: " cumhaz2 " at t3: " cumhaz3
di "Survivor function at   t1: " surv1  "   at t2: " surv2 "   at t3: " surv3
 
********** CLOSE OUTPUT **********
log close
clear
exit

