/*This do file generates the following tables in Bonjour, Cherkas, Haskel, Hawkes and Spector (2003)
Column (6) of table one - columns (1) and (2)
Columns (2) to (9) of table two
The first six rows of table three
Table four
The first five rows of table five
Other columns have been omitted due to confidentiality of the data set or 
because they are from other data sources such as the LFS*/

*type here where you have stored the data
cd "G:\Deniseh Work\Twins

use BCHHS_data, clear
log using BCHHS_tables

*table one column six
sum  schyear highqua age married earning own_exp full self

*generating log earnings
gen lnearn=ln(earning)

*generating age squared
gen agesq=age*age

*table two column two
reg lnearn highqua age agesq

*table two column three
ivreg lnearn (highqua=twihigh) age agesq

*generating differences
reshape wide earning-agesq, i(family) j(twinno)
gen dlnearn = lnearn1-lnearn2
gen dhigh = highqua1-highqua2
gen dtwihi=twihigh1-twihigh2

*table two column four
reg dlnearn dhigh, noc

*table two column five
ivreg dlnearn (dhigh=dtwihi) , noc

*with additional variables - keeping those who report all of these
gen dpart = part1-part2
gen dmarried = married1-married2
gen dLNandSE = LNandSE1-LNandSE2
gen down_exp = own_exp1-own_exp2
*note age and white both drop out.
drop if dlnearn==.|dhigh==.|dtwihi==.|dpart==.|dmarried==.|dln==.|down_exp==.
reshape long

*table two column six
reg lnearn highqua age agesq LNandSE part married own_exp

*table two column seven
ivreg lnearn (highqua=twihigh) age agesq LNandSE part married own_exp

*table two column eight
reg dlnearn dhigh dLNandSE dpart dmarried down_exp, noc

*table two column nine
ivreg dlnearn (dhigh=dtwihi) dLNandSE dpart dmarried down_exp, noc

use BCHHS_data, clear

*generating log earnings
gen lnearn=ln(earning)

*generating age squared
gen agesq=age*age

*calculating averages and differences for correlations
reshape wide earning-agesq, i(family) j(twinno)
gen dweight = bweight1 - bweight2
gen aveweigh = (bweight1 + bweight2)/2
gen dmarry = married1 - married2
gen avemarry = (married1 + married2)/2
gen dself = self1 - self2
gen aveself = (self1+self2)/2
gen dhigh = highqua1 - highqua2
gen avehigh = (highqua1+highqua2)/2
gen dpart = part1 - part2
gen avepart = (part1+part2)/2
gen dpartexp =  exp_par1 -  exp_par2
gen avepexp = (exp_par1 + exp_par2)/2
gen aveped = (parted1+parted2)/2
gen dped = parted1 - parted2
gen avesm16 = (sm161+sm162)/2
gen dsm16 = sm161-sm162
gen avesm18 = (sm181+sm182)/2
gen dsm18 = sm181-sm182
reshape long

* table three rows 1-6 column one
pwcorr avehigh avemarry aveself avepart avepexp aveped aveweigh, sig st(10)
* significant at 10%
pwcorr avehigh avemarry aveself  avepart avepexp aveped aveweigh, sig st(5)
* significant at 5%
pwcorr avehigh avemarry aveself  avepart avepexp aveped aveweigh, sig st(1)
* significant at 1%

* table three rows 1-6 column two
pwcorr  dhigh dmarry dself dpart dpartexp dped dweight, sig st(10)
*significant at 10%
pwcorr  dhigh dmarry dself dpart dpartexp dped dweight, sig st(5)
*significant at 5%
pwcorr  dhigh dmarry dself dpart dpartexp dped dweight, sig st(1)
*significant at 1%

*table four column one
pwcorr avehigh avesm16 avesm18, sig st(1)
*significant at 1%

*table four column two
pwcorr dhigh dsm16 dsm18, sig st(10)
*significant at 10%
pwcorr dhigh dsm16 dsm18, sig st(5)
*significant at 5%
pwcorr dhigh dsm16 dsm18, sig st(1)
*significant at 1%

*table five column one
reg lnearn highqua age agesq

*table five column two
ivreg lnearn (highqua=sm16) age agesq

*table five column three
ivreg lnearn (highqua=sm18) age agesq

log close
clear
