/*-------------------------------*/
/*   Estimation and Confidence   */
/*    Intervals for VaR & ES     */
/*                               */
/*        Dr Aric LaBarr         */
/*-------------------------------*/


/* Load Needed Data */
proc import datafile = 'stocks.csv'
	out = stocks dbms = csv replace;
run;

/* Setable Parameters - Holdings and VaR Confidence Level */
%let msft_holding = 1700; 
%let aapl_holding = 2500;  
%let var_percentile=0.05;
%let n_simulations = 10000;
/*==================================================================*/


/* Calculate the Needed Variances and Covariances */
proc corr data=stocks cov out=covar_results plots;
	var msft_r aapl_r;
run;

data _null_;
	set covar_results(where = (_type_ eq 'COV'));
	if upcase(_name_) eq 'MSFT_R' then do;
		call symput("var_msft",msft_r);
		call symput("covar",aapl_r);
	end;
	if upcase(_name_) eq 'AAPL_R' then do;
		call symput("var_aapl",aapl_r);
	end;
run;

data _null_;
	set covar_results(where = (_type_ eq 'CORR'));
	if upcase(_name_) eq 'MSFT_R' then do;
		call symput("corr",aapl_r);
	end;
run;

/* Calculate Current Total Value of Holdings (Portfolio) */
data _null_;
	set stocks end=eof;
	if eof then do;
   		msft_position= MSFT_Close*&msft_holding;call symput("msft_position",msft_position);
   		aapl_position= AAPL_Close*&aapl_holding;call symput("aapl_position",aapl_position);
		call symput("number_of_observations",_n_);
	end;
run; 

/* Variance-Covariance Approach */ 
proc iml;
title 'VaR Results';

/* Calculate Portfolio Holding Weights */
msft_p_weight = &msft_position/(&msft_position + &aapl_position);
aapl_p_weight = &aapl_position/(&msft_position + &aapl_position);

/* Calculate Portfolio Variance */
P_variance =  &var_msft*(msft_p_weight)**2 + &var_aapl*(aapl_p_weight)**2 
                   + 2*aapl_p_weight*msft_p_weight*&covar;
P_StdDev=sqrt(P_variance);
print P_StdDev;

/* Confidence Intervals for Portfolio Standard Deviation */
sigma_low = sqrt(P_variance*(&number_of_observations-1)/cinv((1-(&var_percentile/2)),&number_of_observations-1) );
sigma_up = sqrt(P_variance*(&number_of_observations-1)/cinv((&var_percentile/2),&number_of_observations-1) );
print sigma_low sigma_up;

/* Calculate Portfolio's Value at Risk, VaR CI, and Conditional Value at Risk */
VaR_normal = (&msft_position + &aapl_position)*PROBIT(&var_percentile)*SQRT(P_variance);
VaR_L= (&msft_position + &aapl_position)*PROBIT(&var_percentile)*(sigma_low);
VaR_U= (&msft_position + &aapl_position)*PROBIT(&var_percentile)*(sigma_up);
print var_normal var_l var_u;
pi=3.14159265;
ES_normal = -(&msft_position + &aapl_position)*SQRT(P_variance)*exp(-0.5*(PROBIT(&var_percentile))**2)/(&var_percentile.*sqrt(2*pi));

print "Daily VaR (Percentile level: &var_percentile); Delta-Normal" VaR_normal[format=dollar15.2];

print "Daily CVaR/ES (Percentile level: &var_percentile); Delta-Normal" ES_normal[format=dollar15.2];

quit;



/* Historical Simulation Approach - PROC IML */
proc iml;

/* Read in Stocks Data */
USE stocks var {msft_r aapl_r}; 
read all var _all_ into returns;

/* Calculate Portfolio Return */
portfolio_return = &msft_position*returns[,1] + &aapl_position*returns[,2];

/* Sort Portfolio Values */
call sort(portfolio_return,{1});
number_of_observations = nrow(portfolio_return);

/* Find Value at Risk Observation */
obs_to_use = round(&var_percentile*number_of_observations,1)+1;

VaR_historical = portfolio_return[obs_to_use,1];

PRINT "Daily VaR (Percentile level: &var_percentile); Historical" VaR_historical[format=dollar15.2];

/* Calculate the ES */
ES = sum(portfolio_return[1:obs_to_use,1])/(obs_to_use-1);
PRINT "Daily CVaR/ES (Percentile level: &var_percentile level); Historical" ES[format=dollar15.2];


title;
QUIT;


/* Historical Simulation Approach - PROC's and Data Steps */
data stocks_new;
	set stocks;
	value = &msft_position*msft_r + &aapl_position*aapl_r;
run;

%let var_clevel=%sysevalf(100*&var_percentile);

proc univariate data=stocks_new noprint;
	var value;
	output out=percentiles pctlpts = &var_clevel pctlpre=P;
run;

proc print data=percentiles;
run;

data _null_;
	set percentiles;
	call symput("var_p",P5);
run;

proc means data=stocks_new mean;
	var value;
	where value < &var_p;
run;

/* Calculate Current Price of Holdings (Portfolio) */
data _null_;
	set stocks end=eof;
	if eof then do;
   		call symput("msft_p",MSFT_Close);
   		call symput("aapl_p",AAPL_Close);
	end;
run; 


/* Monte Carlo Simulation Approach */ 
data Corr_Matrix;
	do i=1 to &n_simulations;
		_type_ = "corr";
		_name_ = "msft_r";

  		msft_r = 1.0; 
  		aapl_r = &corr;

  		output;
  		_name_ = "aapl_r";

		msft_r = &corr; 
  		aapl_r = 1.0;

  		output;
	end;
run;

data Naive;
	do i=1 to &n_simulations;
  		msft_r=0;
  		aapl_r=0;
  		output;
	end;
run;

proc model noprint;

	msft_r = 0;
	errormodel msft_r ~Normal(&var_msft);

	aapl_r = 0;
	errormodel aapl_r ~Normal(&var_aapl);

	solve msft_r aapl_r/ random=1 sdata=Corr_Matrix
	data=Naive out=mc_stocks(keep=msft_r aapl_r i);
		by i;

run;
quit;

data mc_stocks;
	set mc_stocks;
	by i;
	if first.i then delete;
	rename i=simulation;
	value = &msft_holding*(exp(msft_r + log(&msft_p))) + &aapl_holding*(exp(aapl_r + log(&aapl_p)));
	value_change = value - (&msft_holding*&msft_p + &aapl_holding*&aapl_p);
	format value_change dollar15.2;
run;

%let var_clevel=%sysevalf(100*&var_percentile);

proc univariate data=mc_stocks;
	var value_change;
	format value_change dollar15.2;
	output out=percentiles pctlpts = &var_clevel pctlpre=P;
	histogram value_change / kernel normal;
run;

proc print data=percentiles;
run;

data _null_;
	set percentiles;
	call symput("var_p",P5);
run;

proc means data=mc_stocks mean;
	var value_change;
	where value_change < &var_p;
run;


/* Confidence Interval for Value at Risk - Normal Approximation Estimation */
proc univariate data=mc_stocks cipctlnormal;
	var value_change;
run;


/* Confidence Interval for Value at Risk - Distribution Free Estimation */
proc univariate data=mc_stocks cipctldf;
	var value_change;
run;


/* Confidence Interval for Value at Risk - Bootstrap Approach */
%let n_bootstraps=1000;
%let bootstrap_prop=0.1;

proc surveyselect data=mc_stocks out=outboot seed=12345 method=srs 
				  samprate=&bootstrap_prop rep=&n_bootstraps noprint;
run; 

proc univariate data=outboot noprint;
	var value_change;
	output out=boot_var pctlpts = &var_clevel pctlpre=P;
	by replicate;
run;

proc univariate data=boot_var;
	var P5;
	output out=var_ci pctlpts = 2.5 97.5 pctlpre=P;
run;

proc print data=var_ci;
run;


/* Confidence Interval for Conditional Value at Risk - Bootstrap Approach */
%let n_bootstraps=1000;
%let bootstrap_prop=%sysevalf(&n_bootstraps/&n_simulations);

proc surveyselect data=mc_stocks out=outboot_es seed=12345 method=srs 
				  samprate=&bootstrap_prop rep=&n_bootstraps noprint;
run; 

proc sort data=outboot_es;
	by replicate value_change;
run;

data outboot_es;
	set outboot_es;
	obs = mod(_N_, &n_bootstraps);
	if obs = 0 then obs = &n_bootstraps;
run;

data outboot_es;
	set outboot_es;
	where obs < %sysevalf(&var_percentile*&n_bootstraps);
run;

proc means data=outboot_es mean noprint;
	var value_change;
	by replicate;
	output out=boot_es mean = CVaR;
run;

proc univariate data=boot_es;
	var CVaR;
	output out=cvar_ci pctlpts = 2.5 97.5 pctlpre=P;
run;

proc print data=cvar_ci;
run;
