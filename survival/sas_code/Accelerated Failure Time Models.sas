/*-----------------------------------*/
/*    MSA 2019: Survival Analysis    */
/*  Accelerated Failure Time Models  */
/*                                   */
/*          Matthew Austin           */
/*-----------------------------------*/

/* fit AFT models with proc lifereg */
/* in the model statement, the response is
like we saw in proc lifetest: time*status(), where
the # inside parenthesis is the value indicating a
CENSORED observation. otherwise, everything else is the
same as any other model statement you've seen */
proc lifereg data=survival.recid;
	model week*arrest(0) = fin age race wexp mar paro prio;
run;

/* dist statement specifies the distribution of T. default is weibull */
/* probplot asks for a qqplot to see how this fits the data */
proc lifereg data=survival.recid;
	model week*arrest(0) = fin age race wexp mar paro prio / dist=weibull;
	probplot;
run;

/* let's try some others */
/* exponential is a special case of the weibull where scale = 1 */
/* "lagrange multiplier" table tests whether or not it's 1
(null hypothesis is scale = 1; i.e., exponential is ok */
/* in the output of estimates, notice now that "Scale" is fixed as 1 */
proc lifereg data=survival.recid;
	model week*arrest(0) = fin age race wexp mar paro prio / dist=exponential;
	probplot;
run;

/* gamma includes the weibull: weibull is a special
case where scale = 1/shape
(and obviously that means that exponential is also a special
case of gamma) */
proc lifereg data=survival.recid;
	model week*arrest(0) = fin age race wexp mar paro prio / dist=gamma;
	probplot;
run;

/* log-normal */
proc lifereg data=survival.recid;
	model week*arrest(0) = fin age race wexp mar paro prio / dist=lnormal;
	probplot;
run;
/* if there were no censoring, you'd get the same
estimates as linear regression using log(week) */
proc lifereg data=survival.recid;
	/* if you don't specify the "event" part of the response, it assumes that the
	event actually happened at the value of "week" */
	model week = fin age race wexp mar paro prio / dist=lnormal;
	probplot;
	/* so our QQ plot shows this isn't a great fit... */
run;
data logrecid;
	set survival.recid;
	logweek = log(week);
run;
proc reg data=logrecid;
	model logweek = fin age race wexp mar paro prio;
run;
/* ...and all the diagnostic plots here suck, but the coefficient estimates are indeed the same. */

/* one more: log-logistic */
proc lifereg data=survival.recid;
	model week*arrest(0) = fin age race wexp mar paro prio / dist=llogistic;
	probplot;
run;

/* get predicted median survival times for each individual */
proc lifereg data=survival.recid outest=a noprint;
	/* outest saves coefficient estimates */
	model week*arrest(0) = fin age race wexp mar paro prio / dist=weibull;
	/* quantile = 0.5 is the median. that's the default, but you can
	get 0.25 or 0.75 or whatever you want. or all of them.
	"p" is naming the quantile I asked for---the median.
	"std" is the SEs of the quantiles you ask for
	"xbeta" is b0 + b1x1 + b2x2 + ... 
	"cdf" is 1 - S(t), so we'll can do that in a data step
	and see "when" (in terms of survival prob) the event happened
	for each observation */
	/* keep in mind that quantiles here are predicted EVENT times,
	so the 75th quantile means "the time by which 75% of the observations
	have had the event," so S(t) = 0.25 */
	/* you can ask for multiple quantiles like quantile=(0.25 0.5 0.75), and
	SAS will basically create a record for each quantile you request.
	so if I actually ran that statement, each observation will have three
	rows in the outputted dataset, corresponding to the 25th, 50th, and
	75th quantiles */
	output out=b p=med quantile=0.5 std=se xbeta=eta cdf=cdistfunc;
run;
data b;
set b;
survprob = 1 - cdistfunc;
run;
/* do/should you trust these numbers? why or why not? */
proc print data=b;
var survprob _prob_ med se;
run;
/* let's get the predicted survival probabilities at 10 weeks */
/* load these two macros */
%include 'D:/survival/code sas/lifehaz.sas';
%include 'D:/survival/code sas/predict.sas';
/* "predict" gives predicted survival probabilities at a time t that you specify */
/* arguments: - OUTEST= is your dataset from the outest= option in lifereg
			  - OUT= is the name of your dataset from the output statement in lifereg
			  - XBETA= is the name of your xbeta variable in your OUT= dataset
			  - TIME= is the survival time you want */
/* "lifehaz" will plot the predicted hazard. I probably won't use it today,
but you can check out the syntax and play around with it if you want. */ 
%predict(outest=a, out=b, xbeta=eta, time=10)
/* %lifehaz(outest=a, out=b, xbeta=eta) */

/* now let's predict what would happen if financial aid was
given to all those rearrested who didn't receive it */
/* create a new dataset with those people and set week to missing
so lifereg doesn't use it in the model fitting */
data pred;
set survival.recid;
if arrest=0 then delete;
if fin=1 then delete;
fin = 1;
week = .;
run;
/* attach the rows of these "new" people to our dataset */
proc append base=pred data=survival.recid;
run;
/* then we can do all the same stuff we did above */
/* you can compare the differences between the two or whatever you want */
proc lifereg data=pred outest=a2;
	model week*arrest(0) = fin age race wexp mar paro prio / dist=weibull;
	output out=b2 p=med quantile=0.5 std=se xbeta=eta;
run;
/* for the "new" people, the outputted dataset contains the predicted median.
let's subset the dataset so that we only have these new people */
data b2;
set b2;
if week ne . then delete;
run;
proc print data=b2;
var _prob_ med se;
run;
/* new predicted probabilities at time 10 */
%predict(outest=a2, out=b2, xbeta=eta, time=10)

/* new time at same probability */
/* if somebody makes this or anything above into a macro or otherwise automates this process,
your classmates, future students, and i would be grateful (in descending order of gratitude) */
/* fit the model to get stuff output */
proc lifereg data=survival.recid outest=a noprint;
	model week*arrest(0) = fin age race wexp mar paro prio / dist=weibull;
	/* output the linear predictor and the cdf
	(cdf = 1 - S(t)) */
	output out=b xbeta=eta cdf=cdistfunc;
run;

/* get S(t) */
/* this is each person's estimated survival probability
at the ACTUAL recorded event/censoring time */
data b;
set b;
survprob_old = 1 - cdistfunc;
run;

/* get subset of people who had the event
and did not have financial aid */
/* only keeping the survival probability here */
/* we'll set this aside for now, but will need it later */
data survprob;
set b;
if arrest = 0 then delete;
if fin = 1 then delete;
keep survprob_old week;
run;

/* now, back to the actual dataset,
let's pretend the people who didn't get financial
aid did get it */
data pred;
set survival.recid;
if arrest=0 then delete;
if fin=1 then delete;
fin = 1;
/* set week to missing so it's not used in model fitting,
but we will still get a prediction for eta */
week = .;
run;
/* rbind */
proc append base=pred data=survival.recid;
run;

/* now refit the model and output eta */
proc lifereg data=pred outest=a2;
	model week*arrest(0) = fin age race wexp mar paro prio / dist=weibull;
	output out=b2 xbeta=eta;
run;
/* get the subset of my "new" data */
data b2;
set b2;
if week ne . then delete;
run;
/* cbind with estimated S(t) at actual time of rearrest */
data b2;
merge b2 survprob;
run;
/* get new predicted time using weibull quantiles */
data b2;
set b2;
/* the first argument is the name of your distribution: weibull */
/* the second is the probabilities you want: the original S(t) */
/* the third is the SHAPE parameter (as I called it in the notes)---this is
"weibull shape" from your output; I don't know how to output this
so you'll just have to type it in manually */
/* the last argument is the SCALE (as I called it in the notes: 1/rate)---this is
just e^{eta} */
newtime = squantile('weibull', survprob_old, 1.4037, exp(eta));
diff = newtime - week;
run;
/* print to check this is right */
/* first few obs should have a new time of ~26, 22, 32, 30, ... */
proc print data=b2;
var survprob_old week newtime diff;
run;
