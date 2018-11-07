/*----------------------------------*/
/*    MSA 2019: Survival Analysis   */
/*      Cox Regression Models       */
/*                                  */
/*          Matthew Austin          */
/*----------------------------------*/

/* fit cox models using phreg */
/* same as every other model structure */
proc phreg data=survival.recid;
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
	model week*arrest(0)= fin age race wexp mar paro prio / ties = efron risklimits=pl;
	/* baseline statement is how specify what you want to plot */
	/* "covariates" looks for the dataset */
	/* "out" will output a dataset with the estimates so that you can
	plot them with something other than sas default graphics if you want */
	baseline covariates=ref out=refs;
run;
