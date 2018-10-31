/*-------------------------------*/
/*     Recent Developments       */
/*            in Risk            */
/*                               */
/*        Dr Aric LaBarr         */
/*-------------------------------*/


/* Load Needed Data */
proc import datafile = 'stocks.csv'
	out = stocks dbms = csv replace;
run;

/* Setable Parameters - Holdings and VaR Confidence Level */  
%let var_percentile=0.01;
/*==================================================================*/

/* Making Left Tail in Data the Right Tail in the Distribution */
data neg_stocks;
	set stocks;
	neg_port_v = -1*port_v;
run;

/* Isolating the Distribution Tail */
proc univariate data=neg_stocks;
	var neg_port_v;
	output out=percentiles pctlpts = 95 pctlpre=P;
run;

proc print data=percentiles;
run;

data _null_;
	set percentiles;
	call symput("tail",P95);
run;

data stocks_tail;
	set neg_stocks;
	if neg_port_v <= &tail then delete;
run;

/* Estimating the Pareto Distribution Parameters */
proc univariate data=stocks_tail;
	histogram neg_port_v / pareto(theta=&var_p percent=80);
	ods output ParameterEstimates=PE FitQuantiles=FQ;
	ods select histogram ParameterEstimates FitQuantiles;
run;

data _null_;
	set PE;
	if upcase(Symbol) eq 'THETA' then do;
		call symput("threshold",Estimate);
	end;
	if upcase(Symbol) eq 'SIGMA' then do;
		call symput("beta",Estimate);
	end;
	if upcase(Symbol) eq 'ALPHA' then do;
		call symput("xi",Estimate);
	end;
run;

data _null_;
	set FQ;
	call symput("VaR",EstQuantile);
run;

/* Extreme Value Theory Calculations for VaR and CVaR */ 
proc iml;
title 'EVT Results';

VaR_EVT = &VaR*-1;
ES_EVT = ((&VaR + &beta - &xi*&threshold)/(1-&xi))*-1;

print "Daily VaR (Percentile level: &var_percentile); EVT" VaR_EVT[format=dollar15.2];

print "Daily CVaR/ES (Percentile level: &var_percentile); EVT" ES_EVT[format=dollar15.2];

quit;

