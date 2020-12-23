* MMA12P3DRAWS.DO  March 2005 for Stata Version 8.0

log using mma12p3draws.txt, text replace

********** OVERVIEW OF MMA12P3DRAWS.DO **********

* STATA Program 
* copyright C 2005 by A. Colin Cameron and Pravin K. Trivedi 
* used for "Microeconometrics: Methods and Applications" 
* by A. Colin Cameron and Pravin K. Trivedi (2005)
* Cambridge University Press 

* Chapter 12.8.2 pages 412-5
* Draws figures that illustrate two common ways to draw random variates

* (1) Illustrate Inverse Transformation method: Figure 12.2
* (2) Illustrate Envelope method: Figure 12.3

* No data need be read in.

********** SETUP **********

set more off
version 8
set scheme s1mono
  
********** (1) INVERSE TRANSFORMATION - FIGURE 12.2 page 413 **********

* Graph is for x = 0 to 5 in increments of 0.05
set obs 100
gen x = 0.05*_n
* Unit Exponential cdf
gen Fx = 1 - exp(-x) 
* Suppose uniform draw is 0.64
gen uniformdraw = 0.64

graph twoway (line Fx x, yline(0.64) xline(1.02)),  /*
  */ scale (1.2) plotregion(style(none)) /*
  */ title("Inverse Transformation Method") /*
  */ xtitle("Random variable x", size(medlarge)) xscale(titlegap(*5)) /*
  */ ytitle("Cdf  F(x)", size(medlarge)) yscale(titlegap(*5)) /*
  */ caption(" " "Draw of 0.64 (vertical axis) yields x = 1.02 (horizontal axis).")
graph save ch12fig2invtransform, replace
graph export ch12fig2invtransform.wmf, replace

********** (2) ENVELOPE METHOD - FIGURE 12.3 **********

* The following is a modification of the figure in the book
* making clear that the envelope is a scaling up of g(x)

clear

* Graph is for x = 0 to 10 in increments of 0.1
set obs 101
gen x = -0.05 + 0.1*_n
* Unit Exponential cdf
gen fx = normden(x-4)
gen gx = 1.5*normden(x-4)+0.005

graph twoway (line fx x, clstyle(p1)) /*
  */ (line gx x, clstyle(p1) clwidth(*2) clcolor(gs12)), /*
  */ scale (1.2) plotregion(style(none)) /*
  */ title("Accept-reject Method") /*
  */ xtitle("Random variable x", size(medlarge)) xscale(titlegap(*5)) /*
  */ ytitle("f(x) and kg(x)", size(medlarge)) yscale(titlegap(*5)) /*
  */ legend(pos(1) ring(0) col(1)) legend(size(small)) /*
  */ legend( label(1 "Desired density f(x)") label(2 "Envelope kg(x)") )
graph save ch12fig3envelope, replace
graph export ch12fig3envelope.wmf, replace

********** CLOSE OUTPUT **********
log close
* clear
* exit

