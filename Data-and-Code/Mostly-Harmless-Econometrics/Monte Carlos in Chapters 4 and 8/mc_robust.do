capture log close
log using mc_robust, replace

/* Monte Carlo results for Table 8.1.1
   Small Sample bias of covariance estimators */

set more 1
clear

capture program drop mcw

program define mcw
     tempname sim0 sim1 sim2 sim3 sim4
     tempfile res0 res1 res2 res3 res4
     postfile `sim1' b se rn rt using res1, replace
     postfile `sim2' hc0 r0n r0t hc1 r1n r1t using res2, replace
     postfile `sim3' hc2 var2 r2n r2t using res3, replace
     postfile `sim4' hc3 r3n r3t semax0 rmax0n rmax0t semax1 rmax1n rmax1t semax2 rmax2n rmax2t semax3 rmax3n rmax3t using res4, replace



quietly {
        local i = 1
        local repl = 10000                    /* no. of replications */
        while `i' <= `repl' {
        drop _all

        local n = 30
        set obs `n'                           /*  no. of obs. */
        local df = `n' - 2


        local a = .5                          /* degree of heteroskedasticity 0, 0.5 or .15 */
        local p = .9                          /* fraction zeros, 0.9 for an unbalanced design */


        gen x = _n/_N > `p'

        gen y = (1 - `a' + `a'*x)*invnorm(uniform())

/* OLS */

        reg y x
        predict yhat
        predict r, resid
        post `sim1' (_b[x]) (_se[x]) (normal(abs((_b[x])/_se[x])) > .975) (ttail(`df',abs((_b[x])/_se[x])) <= .025)
        local b = _b[x]
        local seols = _se[x]

/* White, HC0 (must be constructed from HC1 by removing df correction) and HC1 */

        reg y x, robust
        local t = abs((_b[x])/_se[x])
        local se0 = `df'*_se[x]/`n'
        post `sim2' (`se0') (normal(abs((_b[x])/(`se0'))) > .975) (ttail(`df',abs((_b[x])/(`se0'))) <= .025) (_se[x]) (normal(abs((_b[x])/_se[x])) > .975) (ttail(`df',abs((_b[x])/_se[x])) <= .025)
        local se1 = _se[x]

/* HC2 */

        reg y x, hc2
        post `sim3' (_se[x]) (_se[x]^2) (normal(abs((_b[x])/_se[x])) > .975) (ttail(`df',abs((_b[x])/_se[x])) <= .025)
        local se2 = _se[x]

/* HC3 */

        reg y x, hc3
        local semax0 = max(`se0', `seols')
        local semax1 = max(`se1', `seols')
        local semax2 = max(`se2', `seols')
        local semax3 = max(_se[x], `seols')
        post `sim4' (_se[x]) (normal(abs((_b[x])/_se[x])) > .975) (ttail(`df',abs((_b[x])/_se[x])) <= .025) /*
                        */   (`semax0') (normal(abs((_b[x])/`semax0')) > .975) (ttail(`df',abs((_b[x])/`semax0')) <= .025) /*
                        */   (`semax1') (normal(abs((_b[x])/`semax1')) > .975) (ttail(`df',abs((_b[x])/`semax1')) <= .025) /*
                        */   (`semax2') (normal(abs((_b[x])/`semax2')) > .975) (ttail(`df',abs((_b[x])/`semax2')) <= .025) /*
                        */   (`semax3') (normal(abs((_b[x])/`semax3')) > .975) (ttail(`df',abs((_b[x])/`semax3')) <= .025)



        noisily disp in gr "replication  " in yel `i'

        local i = `i' + 1


        }               /* end monte carlo replication loop */
        }               /* end quietly */

     postclose `sim1'
     postclose `sim2'
     postclose `sim3'
     postclose `sim4'


noisily disp
noisily disp
noisily disp

noisily disp "number of repl. " `repl'
noisily disp "number of obs. " `n'
noisily disp "fractions zero " `p'
noisily disp "heteroskedasitsity " `a'

end

mcw

quietly {
use res1, clear
merge using res2
drop _merge
merge using res3
drop _merge
merge using res4
drop _merge

}


sum

set more 0
log close
