/*** Cox regression pt. 2 ***/
/* add everything below to your previous code */

/* fit cox models using phreg */
/* same as every other model structure */
proc phreg data=survival.recid concordance;
	/* I specify ties = efron (this isn't the default,
	but I can't think of any reason that you shouldn't use it),
	and risklimits=pl for a CI on the hazard ratios */
	model week*arrest(0)=fin age race wexp mar paro prio / ties = efron risklimits=pl;
run;

/* plot baseline survival curve plot with plots=survival */
/* but who is the baseline? */
proc phreg data=survival.recid plots=survival;
	model week*arrest(0)=fin age race wexp mar paro prio / ties = efron risklimits=pl;
run;

/* create a fake dataset with some real comparison
you might actually be interested in */
data ref;
input fin age race wexp mar paro prio;
datalines;
1 30 0 1 0 0 0
0 30 0 0 0 0 4
;
run;
/* we can plot curves for the two people above */
/* (overlay) will put both on the same plot instead of separate */
proc phreg data=survival.recid plots(overlay)=survival;
	model week*arrest(0)=fin age race wexp mar paro prio / ties = efron risklimits=pl;
	/* baseline statement is how specify what you want to plot */
	/* "covariates" looks for the dataset */
	/* "out" will output a dataset with the estimates so that you can
	plot them with something other than sas default graphics if you want */
	baseline covariates=ref out=refs;
run;

/**************/
/*** part 2 ***/
/**************/

/* checking linearity with "assess" statement */
/* but like what even is this? */
proc phreg data = survival.recid;
	model week*arrest(0)=fin age race wexp mar paro prio / ties=efron;
	/* this is basically just doing a simulation to check linearity */
	/* if linearity is satisfied, these plots will show a random walk starting and ending at 0,
	so the observed walk (bolded line) shouldn't be appear to be predominately positive
	or negative over time */
	/* but these also don't tell you anything about what the form actually looks like,
	so you may as well just use the residual plots too. you can output them with the
	resmart= and resdev= options, and then just plot them vs. predictor with the
	loess curve as i showed you in logistic */
	/* if you don't specify variables, it will do this for all of them */
	assess var=(age prio) / resample;
run;

/* checking proportional hazards */
/* you can use the strata statement to get stratified models, but
phreg won't do the log-log plots and i don't know how to get it by hand
(it can probably be done in proc lifetest but i don't know) */
/* instead, you can check PH a different way */
/* note this won't work with time-dependent variables (discussed later) */
proc phreg data=survival.recid;
	model week*arrest(0)=fin age race wexp mar paro prio / ties=efron;
	/* this is pretty much the same thing as the assess statement above,
	with a different type of residual */
	/* if PH is satisfied, these plots will show a random walk starting and ending at 0,
	so the observed walk (bolded line) shouldn't be appear to be predominately positive
	or negative over time */
	assess ph / resample;
run;

/* stratified cox model */
proc phreg data=survival.recid;
	model week*arrest(0)=age race wexp mar paro prio / ties=efron risklimits=pl;
	strata fin;
run;
/* separate age effect in each stratum */
proc phreg data=survival.recid;
	model week*arrest(0)=age_fin0 age_fin1 race wexp mar paro prio / ties=efron risklimits=pl;
	age_fin1 = age*(fin=1);
	age_fin0 = age*(fin=0);
	strata fin;
run;

/* checking ph assumption using zph plots */
/* default transform is rank;
km is less sensitive to censoring but harder to explain plot;
other options are log and identity */
proc phreg data = survival.recid zph(global transform=km fit=loess);
	model week*arrest(0) = fin age race wexp mar paro prio / ties = efron;
run;

/* time-varying coefficients */
/* the right way: create age*log(time) WITHIN phreg */
proc phreg data=survival.recid;
	model week*arrest(0) = fin age race wexp mar paro prio agelogweek / risklimits=pl ties=efron;
	/* here I just have age*log(week), but this can really be whatever function of time
	you want it to be---you could do age*week or whatever you want */
	agelogweek = age*log(week);
run;
/* the wrong way: create time*age interaction outside of data step */
data recidwrong;
set survival.recid;
agelogweek = age*log(week);
run;
proc phreg data=recidwrong;
	model week*arrest(0) = fin age race wexp mar paro prio agelogweek / risklimits=pl ties=efron;
run;

/* time-varying predictors */
/* basically what we're going to do is turn all those emp variables into
ONE variable at each time that represents employment status for that week */
proc phreg data=survival.recid concordance;
	model week*arrest(0) = fin age race wexp mar paro prio employed / risklimits=pl ties = efron;
	/* create an array of the emp variables */
	array emp(*) emp1-emp52;
	/* and pick out the one corresponding to the current week
	for everyone at risk during this week, not just the week
	a person was re-arrested */
	employed = emp[week];
run;
/* you can do lagged predictions as well */
/* let's instead use employment as of the previous week */
proc phreg data=survival.recid;
	/* since we're doing lags, there's no "previous" employment before week 1,
	so we're discarding everyone's data for the first week */
	where week>1;
	model week*arrest(0) = fin age race wexp mar paro prio employed / risklimits=pl ties = efron;
	/* create an array of the emp variables */
	array emp(*) emp1-emp52;
	employed = emp[week-1];
run;
/* you can do any data step stuff within phreg */

/* sas won't do concordance if programming statements or stop/start are used */
