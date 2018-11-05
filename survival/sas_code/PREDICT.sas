%macro predict (outest=, out=_last_,xbeta=,time=);
/********************************************************************
MACRO PREDICT produces predicted survival probabilities for specified
survival times, based on models fitted by LIFEREG. When fitting the
model with LIFEREG, you must request the OUTEST data set on the
PROC statement.  You must also request an OUTPUT data set with the
XBETA= keyword.  

PREDICT has four parameters:

OUTEST is the name of the data set produced with the OUTEST option.
OUT is the name of the data set produced by the OUTPUT statement.
	Default is the last created data set.
XBETA is the name of the variable specified with the XBETA= keyword.
TIME is the specified survival time that is to be evaluated. 

Example:  To get 5-year survival probabilities for every individual
in the sample (assuming that actual survival times are measured in 
years);

%predict(outest=a, out=b, xbeta=lp, time=5).

Author:  Paul D. Allison, Univ. of Pennsylvania
         allison@ssc.upenn.edu	
*********************************************************************/
data _pred_;
_p_=1;
set &outest  point=_p_;
set &out;
lp=&xbeta;
t=&time;
gamma=1/_scale_;
alpha=exp(-lp*gamma);
prob=0;
_dist_=upcase(_dist_);
if _dist_='WEIBULL' or _dist_='EXPONENTIAL' or _dist_='EXPONENT' then prob=exp(-alpha*t**gamma);
if _dist_='LOGNORMAL' or _dist_='LNORMAL' then prob=1-probnorm((log(t)-lp)/_scale_);
if _dist_='LLOGISTIC' or _dist_='LLOGISTC' then prob=1/(1+alpha*t**gamma);
if _dist_='GAMMA' then do;
  d=_shape1_;
  k=1/(d*d);
  u=(t*exp(-lp))**gamma;
  prob=1-probgam(k*u**d,k);
  if d lt 0 then prob=1-prob;
  end;
drop lp gamma alpha _dist_ _scale_ intercept
     _shape1_ _model_ _name_ _type_ _status_ _prob_ _lnlike_ d k u;
run;
proc print data=_pred_;
run;
%mend predict;
