clear all

capture log close	
set more off		
set matsize 4000

cd "C:\Users\Anna Werbeck\Desktop\Desktop\Desktop Ordner\AnnasUnikram\1_RGS\VL_Kram\Microeconometrics\tut\Tutorial_8\"

*log-file*
log using "PS8_1_logfile.log", replace

*dataset*
use "NHIS_stata13.dta" , clear

*1a
hist days_21, freq scheme(s1mono) ylabel(,angle(horizontal)) xtitle("days above or below 21st birthday")
graph save histo_1.pdf, replace
*not optimal: you cannot read the exact values because data is grouped

*1b
*ssc install kdens
twoway (kdens days_21, kernel(e)) (kdens days_21, kernel(g)) (kdens days_21, kernel(parzen)) (kdens days_21, kernel(biweight)), ytitle(Density) ylabel(,angle(horizontal)) legend(on order(1 "Epanechnikov" 2 "Gaussian" 3 "Parzen" 4 "Biweight")) legend(cols(1))
graph export "..\kernel_1b_1.pdf", as(pdf) replace

twoway (kdens days_21, bw(50)) (kdens days_21, bw(100)) (kdens days_21) (kdens days_21, bw(150)) (kdens days_21, bw(200)), ytitle(Density) ylabel(,angle(horizontal)) legend(on order(1 "bw 50" 2 "bw 100" 3 "bw std" 4 "bw 150" 5 "bw 200")) legend(cols(1))
graph export "..\kernel_1b_bw.png", as(png) replace

twoway (kdens days_21) (kdens days_21 if drinks_alcohol==1) (kdens days_21 if drinks_alcohol==0), ytitle(Density) ylabel(,angle(horizontal)) legend(on order(1 "all" 2 "partylöwe" 3 "mschulz")) legend(cols(1)) 
graph save "..\kernel_1b_alc.pdf", as(pdf) replace

/*if we plot the kernel density function for different specifications of 
the kernel there are no great differences observable. 

This is a little different when plotting the kernel density functions with different 
bandwiths. With a bandwidth of 50 or 100 the kernel density is undersmoothed, with 150 or
200 it is oversmoothed. So for small bandwidths, the absolute value of the bias
is smaller and the variance is larger. For greater bandwidths it is the other way
around. So by choosing different values for the bandwidth we change smoothness and
with that are confronted with the bias-variance trade-off.

When looking at the differences between alcohol drincers and non-alcoholics
we observe a swith in patterns at the 21st birthday. We observe a higher density
of anti-alcoholics before being 21 and a higher density of alcohol drincers after
turning 21.
*/

*1c
*ssc install binscatter
egen days_21_bins = cut(days_21), at(-1500(30)4000)
bysort days_21_bins: egen m_drinks_alcohol=mean(drinks_alcohol)
twoway scatter m_drinks_alcohol days_21_bins
binscatter drinks_alcohol days_21_bins, nquantiles(200) 
*because of different functional forms before and after the 
*threshold of 21.

*1d
*histogram for bins
twoway scatter m_drinks_alcohol days_21_bins || lfit m_drinks_alcohol days_21_bins if days_21<=0 || lfit m_drinks_alcohol days_21_bins if days_21>0 || lpoly m_drinks_alcohol days_21_bins 
graph export "..\1d.pdf", as(pdf)

rm bins.gph 
rm poly.gph 
binscatter drinks_alcohol days_21_bins, nquantiles(200) rd(0) nograph
lpoly m_drinks_alcohol days_21_bins
graph combine bins.gph poly.gph

*1e
*ssc install rdrobust
rdrobust drinks_alcohol days_21, kernel(epa)  
rdrobust drinks_alcohol days_21, kernel(epa) h(200) 
rdrobust drinks_alcohol days_21, kernel(epa) h(600) 

rdrobust drinks_alcohol days_21, kernel(tri) bwselect(mserd) 
rdrobust drinks_alcohol days_21, kernel(uni) bwselect(mserd) 

log close

****************************************************************
*Ex2
****************************************************************
clear all

capture log close	
set more off		
set matsize 4000

cd "C:\Users\Anna Werbeck\Desktop\Desktop\Desktop Ordner\AnnasUnikram\1_RGS\VL_Kram\Microeconometrics\tut\Tutorial_8\"

*log-file*
log using "PS8_2_logfile.log", replace

*dataset*
use "soep_stata13.dta" , clear


/*2a**********************************************************
Regress the treatment indicator D on all control variables (i.e. X) using a logit
model. In a second step predict for each individual the respective propensity score. Plot
the distribution of the propensity score for treatment and control group. What can you
say about the common support? How many observations within the treatment/control
group are located on or off the common support.*/

logit D dummytwoparents feduc5 feduc4 feduc3 feduc2 german age female
predict p
sum p
*Average probability to have D=college for all individuals is 28.73%.

twoway histogram p if D==0, fcolor(navy) lcolor(black) || histogram p if D==1, fcolor(none) lcolor(maroon) legend(order(1 "No College" 2 "College")) 

kdensity p if D==1
twoway kdensity p if D==0 || kdensity p if D==1, legend(order(1 "No College" 2 "College")) 

*probit estimation for comparability with standard psmatch2
probit D dummytwoparents feduc5 feduc4 feduc3 feduc2 german age female
predict ps
sum ps
*Average probability to have D=college for all individuals is 28.73%.


/*2b**********************************************************
Use the psmatch2 command to conduct a NN-1 (one nearest neighbor) matching
with replacement. Interpret your outcomes. Which advantages and disadvantages do you
face if you include more neighbors?*/

ssc install psmatch2
psmatch2 D, outcome(Y) pscore(p) n(1) logit
psmatch2 D dummytwoparents feduc5 feduc4 feduc3 feduc2 german age female, outcome(Y) logit
*For individuals of the treatment group, the treatment has raised the gross hourly wage by 7.72$ on average.

psmatch2 D dummytwoparents feduc5 feduc4 feduc3 feduc2 german age female, outcome(Y) n(2) 
psmatch2 D dummytwoparents feduc5 feduc4 feduc3 feduc2 german age female, outcome(Y) n(100)
*?

*compare to results for probit
psmatch2 D, outcome(Y) pscore(ps)
psmatch2 D dummytwoparents feduc5 feduc4 feduc3 feduc2 german age female, outcome(Y) 

pstest Y
pstest dummytwoparents feduc5 feduc4 feduc3 feduc2 german age female


/*2c**********************************************************
Use the psgraph command to plot the distribution of the propensity score that you 
obtained from sub-exercise (b). Do you observe any difference compared to your
graph from exercise (a).*/

psgraph
psgraph, bin(500)

/*psgraph displays the distribution of pscore in bins of twenty/(500). The kernel density
displays this distribution continously.*/

/*2d**********************************************************
Use your results form exercise (a) to compare it with the 2*2 table (showing treat-
ment assignment vs. common support) which is displayed if you execute the psmatch2
command.*/

/*The table displays that there is more common support for the group of untreated than for
the group of treated. This is also displayed in the graph obtained from ex (a). 
In the graph you can also see that the distribution of common support over the 
propensity score classes is skewed to the left. So there is more common support for the 
untreated in ps classes below around 0.35. At this threshold it changes and there is a higher
density of common support for the group of treated.*/

/*2e**********************************************************
Discuss briefly potential disadvantages of the nearest neighbor matching? Which
alternative matching approaches do you suggest?*/

/*
- only accounts for observed/obersvable factors
- large sample required, overlapping between treatment and control group necessary
Alternatives:
Exact matching or Mahalanobis distance method instead of using Euclidean distance 
for the nearest-neighbor distance.
*/

/*2f**********************************************************
Do the matching assumptions hold in the present exercise and do you identify a
causal effect?*/

/*
By matching, the differences between treatment group and non treatment group
are reduced considerably for the outcome variable Y (pstest)
Assumption 1) conditional independence probably holds: when checking for covariates, 
there is a high degree of post-match balance across them.
Assumption 2) Common support holds: When looking at the histogram, there is complete
overlap and only differences in the distributions for the two groups.

With a T-stat of 8.66 for the ATT there should be a causal inference.*/


******************************************
*****Appendix*****************************
******************************************


*ssc install kdens
*ssc install moremata
/*kdens p, ul(1) ll(0) ylabel(,angle(horizontal)) 
loc w=r(width)
kdens p if D==1, ul(1) ll(0) bw(`w') g(d1 x) 
kdens p if D==0, ul(1) ll(0) bw(`w') g(d0 x2) */
