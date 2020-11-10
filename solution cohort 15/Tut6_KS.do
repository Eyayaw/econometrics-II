clear all			/* Reset memory, remove active data */
capture log close	/* In case no log is open */
set more off		/* tells Stata not to pause */
set matsize 4000	/* Expand maximum number of variables */
set type double 	/* Excel stores numeric values in double precision */

* Set working directory where dataset is located:
*Duisburg
*cd "C:\Users\Karolin Süß\sciebo\SoSe2019\Microeconometrics\Tutorial\6"

*Essen
cd "J:\Microeconometrics\Tutorials\6"
* ECONOMETRICS II - Problem Set 5 
* Karolin Süß
* June 19, 2019
use NHIS, clear

********************************************************************************
*Exercise 2
br
*a)
gen age=21+(days_21/365)
gen test=age-age_yrs
tab test
binscatter drinks_alcohol days_21, rd(0)

binscatter drinks_alcohol age, xscale(range(19 23)) rd(21) nq(40) line(qfit) reportreg

binscatter drinks_alcohol age, xscale(range(19 23)) rd(20.9) nq(40) line(qfit) reportreg


*b)
gen treatment=(days_21>=0)
*alow for different functional forms on both sides of the cutoff
gen age_treat=age*treatment
*linear model
regress drinks_alcohol treatment age age_treat if age>=18 & age<24, vce(robust) // only model with significant estimates at the cutoff


*quadratic model
gen age2=age^2
gen age2_treat=age2*treatment
regress drinks_alcohol treatment age age2 age_treat age2_treat if age>=18 & age<24, vce(robust)

*qubic model
gen age3=age^3
gen age3_treat=age3*treatment
regress drinks_alcohol treatment age age2 age3 age_treat age2_treat age3_treat if age>=18 & age<24, vce(robust)

*c
tab days_21 if age==18
tab days_21 if age==24
regress drinks_alcohol treatment age age2 age_treat age2_treat if days_21>=-1095 & days_21<=1095, vce(robust)
estimates store bdw_1095
regress drinks_alcohol treatment age age2 age_treat age2_treat if days_21>=-1065 & days_21<=1065, vce(robust)
estimates store bdw_1065
regress drinks_alcohol treatment age age2 age_treat age2_treat if days_21>=-1035 & days_21<=1035, vce(robust)
estimates store bdw_1035
regress drinks_alcohol treatment age age2 age_treat age2_treat if days_21>=-1005 & days_21<=1005, vce(robust)
estimates store bdw_1005
regress drinks_alcohol treatment age age2 age_treat age2_treat if days_21>=-975 & days_21<=975, vce(robust)
estimates store bdw_975
regress drinks_alcohol treatment age age2 age_treat age2_treat if days_21>=-945 & days_21<=945, vce(robust)
estimates store bdw_945
regress drinks_alcohol treatment age age2 age_treat age2_treat if days_21>=-915 & days_21<=915, vce(robust)
estimates store bdw_915
regress drinks_alcohol treatment age age2 age_treat age2_treat if days_21>=-885 & days_21<=885, vce(robust)
estimates store bdw_885
regress drinks_alcohol treatment age age2 age_treat age2_treat if days_21>=-855 & days_21<=855, vce(robust)
estimates store bdw_855
regress drinks_alcohol treatment age age2 age_treat age2_treat if days_21>=-825 & days_21<=825, vce(robust)
estimates store bdw_825
regress drinks_alcohol treatment age age2 age_treat age2_treat if days_21>=-795 & days_21<=795, vce(robust)
estimates store bdw_795
regress drinks_alcohol treatment age age2 age_treat age2_treat if days_21>=-765 & days_21<=765, vce(robust)
estimates store bdw_765
regress drinks_alcohol treatment age age2 age_treat age2_treat if days_21>=-735 & days_21<=735, vce(robust)
estimates store bdw_735
regress drinks_alcohol treatment age age2 age_treat age2_treat if days_21>=-705 & days_21<=705, vce(robust)
estimates store bdw_705
regress drinks_alcohol treatment age age2 age_treat age2_treat if days_21>=-675 & days_21<=675, vce(robust)
estimates store bdw_675

coefplot bdw_1095 bdw_1065 bdw_1035 bdw_1005 bdw_975 bdw_945 bdw_915 bdw_885 bdw_855 bdw_825 bdw_795 bdw_765 bdw_735 bdw_705 bdw_675, vertical keep(treatment) yline(0) legend(cols(4) rows(3))
