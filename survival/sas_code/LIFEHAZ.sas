%macro lifehaz(outest=,out=,obsno=0,xbeta=lp);
/********************************************************************
Version 2.0 (9-14-01)

This version of LIFEHAZ works for SAS Release 6.12 through 
Release 8.2.  

Macro LIFEHAZ plots the hazard function for a model fitted by
LIFEREG. In the LIFEREG procedure you must specify OUTEST=name1
in the PROC statement.  You must also use the OUTPUT statement with
OUT=name2 and XBETA=name3. By default, the hazard is plotted for the
mean value of XBETA (the linear predictor).  If you want a plot for a
specific observation, you must specify the observation number
(OBSNO) when you invoke the macro.  The macro is invoked as follows:

   %lifehaz(outest=name1,out=name2,xbeta=name3,obsno=1);
   
Author: Paul D. Allison, U. of Pennsylvania, allison@ssc.upenn.edu.   

********************************************************************/
data;
  set &outest;
  call symput('time',_NAME_);
run;
proc means data=&out noprint;
  var &time &xbeta;
  output out=_c_ min(&time)=min max(&time)=max mean(&xbeta)=mean;
run;
data;
  set &outest;
  call symput('model',_dist_);
  s=_scale_;
  d=_shape1_;
  _y_=&obsno;
  set _c_ (keep=min max mean);
  if _y_=0 then m=mean;
  else do;
    set &out (keep=&xbeta) point=_y_;
    m=&xbeta;
  end;
  inc=(max-min)/300;
  g=1/s;
  alph=exp(-m*g);
  _dist_=upcase(_dist_);
if _dist_='LOGNORMAL' or _dist_='LNORMAL'  then do;
  do t=min to max by inc;
  z=(log(t)-m)/s;
  f=exp(-z*z/2)/(t*s*sqrt(2*3.14159));
  Surv=1-probnorm(z);
  h=f/Surv;
  output;
  end;
end;
else if _dist_='GAMMA' then do;
  k=1/(d*d);
  do t=min to max by inc;
  u=(t*exp(-m))**(1/s);
  f=abs(d)*(k*u**d)**k*exp(-k*u**d)/(s*gamma(k)*t);
  Surv=1-probgam(k*u**d,k);
  if d lt 0 then Surv=1-Surv;
  h=f/Surv;
  output;
  end;
end;
else if _dist_='WEIBULL' or _dist_='EXPONENTIAL' or _dist_='EXPONENT'  then do;
  do t=min to max by inc;
  h=g*alph*t**(g-1);
  output;
  end;
end;
else if _dist_='LLOGISTIC' or _dist_='LLOGISTC' then do;
  do t=min to max by inc;
  h=g*alph*t**(g-1)/(1+alph*t**g);
  output;
  end;
end;
else put 'ERROR:DISTRIBUTION NOT FITTED BY LIFEREG';
run;
proc gplot;
  plot h*t / haxis=axis2 vaxis=axis1 vzero;
  symbol1 i=join v=none c=black;
  axis1 label=(f=titalic angle=90 'Hazard');
  axis2 label=(f=titalic justify=c 'time' f=titalic justify=c "&model");
run; quit;
%mend lifehaz;
