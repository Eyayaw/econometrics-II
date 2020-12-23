* MMA13P2BAYES.SAS  March 2005 for SAS version 8.2

********** OVERVIEW OF MMA13P2BAYES.SAS **********

* SAS Program 
* copyright C 2005 by A. Colin Cameron and Pravin K. Trivedi 
* used for "Microeconometrics: Methods and Applications" 
* by A. Colin Cameron and Pravin K. Trivedi (2005)
* Cambridge University Press

* Chapter 13.6 p.452-4 
* MCMC Example: Gibbs Sampler for 2 equation SUR 
* Program creates the first column of Table 13.3 
* (though differs somewhat due to use of different seed)

* For different columns of Table 13.3 change
*   nobs = Sample size N   (1000 or 10000)
*   replics = Gibbs sample replications  (50000 or 100000)  
*   tau = 1, 10 or 0.1
* This program does first column: tau=10, nobs=1000, replics=50000

* Note that the program does not exactly replicate Table 13.3
* Table 13.3 used the computer clock for seed, 
* with third argument zero in rannor(j( , ,0))
* Here instead the seed is consecutively 10101, 20101, ... , 70101
* so third argument is eg rannor(j( , ,10101))
* to permit reproducability by other users

* This programs creates 
*     MMA13P2BAYES.1ST    SAS Output with one column of Table 13.3
*     MMA13P2BAYES.LOG    SAS log file

* This program uses generated data - so no data set required
* This program uses a lot of memory - 1 gigabyte should do
* In Unix give command   sas -MEMSIZE 1G mma13p2bayesgibbs.sas

*********************************************************************;
*****   BIVARIATE NORMAL-BAYESIAN-ESTIMATION-BY-MCMC   **************;
*********************************************************************;

OPTIONS LS=75;
options NOTES;

PROC IML;
start main;

print "A. Colin Cameron and Pravin K. Trivedi (2005)";
print "Microeconometrics: Methods and Applications, CUP";
print  "MCMC Example: Gibbs Sampler for SUR";

*************    GENERATING DATA: 2 EQUATION SUR     ****************;

nobs = 1000;
replics = 50000;
burn = 5000;
replics = replics + burn;

npar1 = 2;
npar2 = 2;

alpha1 ={1,1};
alpha2 ={1,1};

sigma = {1 -0.5,-0.5 1}; 
T = {0.15 2.18 0.725 0.45};
EPS = 1e-20;
IC = (1/2.506628275);

R1 = j(nobs,1,1)||rannor(j(nobs,1,10101));
R2 = j(nobs,1,1)||rannor(j(nobs,1,20101));

e = rannor(j(nobs,2,30101))*root(sigma);
e1 = e[,1];
e2 = e[,2];

Y1 = R1*alpha1 + e1;
Y2 = R2*alpha2 + e2;

*************      SPECIFY PRIOR DISTRIBUTIONS     ******************;

alpha01 = j(npar1,1,0);
alpha02 = j(npar2,1,0);

sigma = I(2);
p = 3;
df = 5;
tau = 10;

MUalpha = alpha01//alpha02;
OMalpha = tau*I(npar1+npar2);
OMphi = I(2);

************   ANALYSIS: GIBBS SAMLING BEGINS HERE    ***************;

do rep = 1 to replics;

*************      GENERATE ALPHA1 ALPHA2 RHO     *******************;

isigma = inv(sigma);

LL = ((isigma[1,1]*R1`*R1||isigma[1,2]*R1`*R2)//
		    (isigma[2,1]*R2`*R1||isigma[2,2]*R2`*R2));
LisigY =  ((isigma[1,1]*R1`*Y1+isigma[1,2]*R1`*Y2)//
		    (isigma[2,1]*R2`*Y1+isigma[2,2]*R2`*Y2));

alpha = inv(inv(OMalpha)+ LL)*(LisigY + inv(OMalpha)*MUalpha) 
	   + root(inv(inv(OMalpha)+ LL))`*rannor(j(npar1+npar2,1,40101));

alpha1 = alpha[1:npar1];
alpha2 = alpha[npar1+1:npar1+npar2];

e1 = Y1 - R1*alpha1;
e2 = Y2 - R2*alpha2;

*************             GENERATE SIGMA          *******************;

mt = (sqrt((rannor(j(1,nobs+df,50101))##2)[,+])||0)//
     (rannor(j(1,1,60101))||sqrt((rannor(j(1,nobs+df-1,70101))##2)[,+]));
mv = mt*mt`;
e=(e1||e2);
ms = e`*e+inv(OMphi);
ml = root(inv(ms))`;
mg = ml*mv*ml`;
sigma = inv(mg);

free mt mv e ml;

*************   WRITE TO OUTPUT FILE IF AFTER BURN-IN  **************;

if rep <= burn then goto point300;
sigma3 = sigma[1,1]||sigma[1,2]||sigma[2,2];
out1 = alpha1`||alpha2`||sigma3;

output1=output1//out1;

point300:end;

*************         END OF GIBBS SAMPLING            **************; 


*********************************************************************;
*****   RESULTS: COMPARE LAST HALF WITH ALL (AFTER BURN-IN)   *******;
*********************************************************************;

replics = replics-burn;

out1 = output1[replics/2+1:replics,];
out = output1[1:replics,];

create exp from out1;
append from out1;
summary var _num_;
close exp;

create exp from out;
append from out;
summary var _num_;
close exp;

*********************************************************************;
******   RESULTS: POSTERIOR MEAN AND SD - TABLE 13.3 P.454   ********;
*********************************************************************;

xnames1 = {"CONSTANT"} || {"R1"};
xnames2 = {"CONSTANT"} || {"R2"};
parnames = concat({"d1"}," ",xnames1)||concat({"d2"}," ",xnames2)||{"SIGMA11"}||{"SIGMA12"}||{"SIGMA22"};

meanout = out[+,]/replics;
stderr  = sqrt(((out-j(replics,1,1)*meanout)##2)[+,]/(replics-1));
parm = meanout;
stderr = stderr`;
tnpar = npar1 + npar2 + 3;

tstat = parm`/ stderr;
coeff = parm` || stderr || tstat;
info = tau // nobs // replics // burn // tnpar;
rowinfo={'TAU' '# OBSERVATIONS' '# REPLICATIONS' '# BURN-IN' '# PARAMETERS'};
estcol ={ 'ESTIMATE' 'STD ERR' 'T-STAT'};
mattrib info rowname=rowinfo label={" "};
mattrib coeff rowname=parnames colname=estcol label={" "};
print / "Results for Table 13.3 p.454";
print info;
print coeff;

*********************************************************************;
**********  RESULTS: CONVERGENCE CHECK: SEE P.454     ***************;
*********************************************************************;

print / "Convergence check on p.454";

corr = j(20,7,0);

do i = 1 to 7;
cov = covlag(out[,i],20)`;
corr[,i] = cov/cov[1];
end;

covd1 = j(20,2,0);

do k = 1 to 3;
covd1 = corr[,2*k-1:2*k];
print covd1;
end;

covd1 = corr[,7];
print covd1;

finish main;

run main;
