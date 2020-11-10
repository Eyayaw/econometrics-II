****************************************************************************** 
* Econometrics II, Summer 2020

* Do-file for Problem-Set 3
* Exercise 4: First Differences
* Date: 19.05.2019

* fabian.dehos@rwi-essen.de
****************************************************************************** 

clear 
set more off

 
set type double


*------------------------------
* Exercise 4: (a)
*------------------------------

* ssc install bcuse
bcuse ezunem

* A description of all included variables can be found here:
* http://fmwww.bc.edu/ec-p/data/wooldridge/ezunem.des

by city (year), sort: gen ez_c=		ez[_n]-ez[_n-1]


by city (year), sort: gen luclms_c=	luclms[_n]-luclms[_n-1]



forval i = 81/88 {
by city (year), sort: gen d`i'_c=	d`i'[_n]-d`i'[_n-1]

}


reg luclms_c ez_c d81_c-d88_c 




* declare dataset to be panel data
* xtset panelvar timevar
xtset city year
reg d.luclms d.ez   d.d81 d.d82 d.d83 d.d84 d.d85 d.d86 d.d87 d.d88 , vce(cluster city)


*------------------------------
* Exercise 4: (b)
*------------------------------

xtreg luclms_c ez_c ,  i(city) fe   

* The estimate is actually larger in magnitude compared to (a)
* but we have not yet included year dummies

 

forval i = 1/22 {
gen c_t_`i'=	c`i'*year

}


reg d.luclms d.ez   d.c_t_* , vce(cluster city)


display exp(-0.2511655 ) - 1 
*-> -.22210638
* presence of an EZ (enterprise zone) causes about a 22.2% fall in unemployment claims


*------------------------------
* Exercise 4: (c)
*------------------------------

xtreg luclms_c ez_c i.year ,  i(city) fe   vce(cluster city)












xtreg luclms_c ez_c d81_c-d88_c ,  i(city) fe   vce(cluster city)


reg d.luclms d.ez   d.c_t_*  i.year , vce(cluster city)


display exp(-0.1919386  ) - 1 
*-> -.17464246
* presence of an EZ (enterprise zone) causes about a 17.5% fall in unemployment claims

