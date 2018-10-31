/*--------------------------------*/
/*   MSA 2019: Survival Analysis  */
/*       Survival & Hazards       */
/*                                */
/*         Matthew Austin         */
/*--------------------------------*/

/* simple example */
data lcs;
input team $ time event;
datalines;
MIL 3 0
LAD 4 0
ATL 4 1
COL 4 1
CHC 1 1
BOS 4 0
HOU 3 0
CLE 3 1
NYY 5 1
OAK 1 1
;

/* to get summary statistics for survival,
   we can use proc lifetest */
proc lifetest data=lcs;
	/* time is whatever your time/tenure variable is */
	time time*event(0);
run;

/* now with recidivism data */
proc lifetest data=survival.recid plots=s(cl cb=ep);
	/* for plots: "s" gives the survival curves.
		the "cl" option gives pointwise confidence limits.
		you could also use "cb" for confidence bands, which is
		basically a confidence interval for the entire curve rather
		than for each time
	/* "time" is whatever your time/tenure variable is */
	/* the structure here is time*status, where the number
		inside parenthesis is whichever value 
		of your status variable corresponds to a CENSORED observation 
		(contrast to R where you specify the value corresponding to the event */
	time week*arrest(0);
run;

/* comparing curves using log-rank test */
/* remember: null hypothesis is that all curves are equal */
proc lifetest data=survival.recid plots=s(cl) notable;
	time week*arrest(0);
	/* strata statement is whatever variable you want to
		produce separate curves for. here, we'll
		compare those who had prior work experience
		to those who didn't */
	strata wexp /* / test=peto /*;
	/* test=peto is the same weighted test that the rho=1 option in R does */
run;

/* hazard function */
/* to compute this, we'll use the life-table/actuarial method */
/* specify method=life */
proc lifetest data=survival.recid method=life;
	time week*arrest(0);
run;
/* I don't know what the default grouping is (maybe 10?),
but you can set it yourself with the "width" or "ninterval" options */
proc lifetest data=survival.recid method=life width=4;
	time week*arrest(0);
run;

/* plot hazard function */
/* (width = 1 is equivalent to looking at each time individually
instead of grouping) */
/* and for the life table method, we actually need to
recode the data for the calculations to be correct for censored observations;
otherwise they only get "credit" for the interval [51,52)
instead of the full closed interval [51,52] */
data recid2;
	set survival.recid;
	/* if censored, make week=53 instead of 52 */
	if arrest=0 then week=53;
run;
proc lifetest data=recid2 method=life plots=h(cl) width=1;
	time week*arrest(0);
	*strata wexp;
run;

/* cumulative hazard */
/* adding the "nelson" option will add the cumulative hazard estimates to the table,
but there aren't any plotting options for it in proc lifetest */
proc lifetest data=survival.recid nelson;
	time week*arrest(0);
run;
