
tabstat a-d, statistics(n) // check the sample size for each var

describe, short 
browse, nolabel // to open the data viewer without var labels.


bro age if sex == "female":sex //using labels



clear
input str1 PID str9(starts_on_s ends_on_s)
"A" "01jun2010" "31may2011"
"A" "01jun2011" "30jun2011"
"A" "01jul2011" "31may2012"
"A" "01nov2011" "30jun2012"
"A" "15aug2012" "31may2013"
"A" "15aug2012" "30jun2013"
"A" "01jun2013" "31may2015"
"A" "01jul2013" "30jun2014"
"A" "01jul2014" "31jan2017"
"A" "01jun2015" "31may2016"
end

* convert to monthly dates
gen starts_my = mofd(daily(starts_on_s,"DMY"))
gen ends_my = mofd(daily(ends_on_s,"DMY"))
format %tm *_my

* expand to one obs for each month of coverage
gen long id = _n
expand ends_my - starts_my + 1
bysort id: gen month_insured = starts_my + _n - 1
format %tm month_insured

* remove overlap of coverage between policies
bysort PID month_insured: keep if _n == 1

* group by PID and year to find the number of months covered
gen year = year(dofm(month_insured))
bysort PID year: gen nmonths = _N

* reduce to one obs per year per PID
by PID year: keep if _n == 1

* reshape wide if desired
keep PID year nmonths
reshape wide nmonths, i(PID) j(year)



clear
input str1 PID long(mem_effdate mem_termdate)
"A" 18414 18778
"A" 18779 18808
"A" 18809 19144
"A" 18932 19174
"A" 19145 19509
"A" 19175 19539
"A" 19510 20239
"A" 19540 19904
"A" 19905 30346
"A" 20240 20605
end
format %d mem_effdate
format %d mem_termdate

//    CENSOR OUT OF RANGE TERMINATION DATES
replace mem_termdate = td(31dec2016) if mem_termdate >= td(1jan2017)


//    RESHAPE LONG TO GET A TIME SERIES OF EFF/TERM EVENTS FOR EACH PID
by PID (mem_effdate), sort: gen obs_no = _n
reshape long mem_@date, i(PID obs_no) j(_event) string
//    CREATE A NUMERIC 1/-1 VARIABLE CORRESPONDING TO EVENT
gen event = 1 if _event == "eff"
replace event = -1 if _event == "term"

//    CALCULATE status AS NUMBER OF COVERAGES IN EFFECT
//    AS A RUNNING SUM OF event
by PID (mem_date), sort: gen status = sum(event)
//    AND SIMPLIFY TO JUST INSURED OR NOT
replace status = status > 0

//    IDENTIFY SPELLS OF INSURANCE AND UNINSURANCE
by PID (mem_date), sort: gen spell_num = sum(status == 1 & status[_n-1] == 0 | _n == 1)


//    REDUCE EACH SPELL TO SINGLE OBSERVATION FROM FIRST TO LAST DATE
collapse (min) start = mem_date (max) end = mem_date status, by(PID spell_num)

//    CALCULATE MONTHLY DATES
foreach v of varlist start end {
    gen `v'_month = mofd(`v')
    format `v'_month %tm
}


//    NOW CALCULATE MONTHS OVERLAPPING EACH YEAR
forvalues y = 2010/2016 {
    gen insured_`y' = min(tm(`y'm12), end_month) - max(tm(`y'm1), start_month) + 1 ///
        if (yofd(start) <= `y' & yofd(end) >= `y')
}

//    AND TOTAL OVER PID
collapse (sum) insured_*, by(PID)

list, noobs clean



* The last day of the month is the day before the first day of the next month. 
* here are your days:

forval m = 1/72 { 
    local mdate = ym(2009, 12) + `m' 
    di %td dofm(`mdate' + 1) - 1 
}
