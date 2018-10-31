/*-----------------------------*/
/*     Introduction to Risk    */
/*          Management         */
/*                             */
/*        Dr Aric LaBarr       */
/*-----------------------------*/

/* Monte Carlo Simulation */
%let Simulation_Size = 10000;

data Intro_Risk;
	format Price Var_Cost Fixed_Cost Net_Revenue dollar6.2 Units nlnum18.2;
	label Price="Price";
	label Units="Units Sold";
	label Var_Cost="Variable Cost (VC)";
	label Fixed_Cost="Fixed Cost (FC)";
	label Net_Revenue="Net Revenues";

	do i=1 to &Simulation_Size;
    
	    	Units = 500 + (2000-500)*RAND('TRIANGLE',0.67) ;
	    	Var_Cost = 1 + 0.004*Units + RAND('NORMAL', sqrt(0.8));
	    	Fixed_Cost = 2500;
	    	Price = 8 + (11-8)*RAND('TRIANGLE', 0.67);

	    	Net_Revenue=(Price - Var_Cost)*Units - Fixed_Cost;
		output;
	end;
run; 

proc sgplot data=Intro_Risk;
	title 'Sampling Distribution of Net Revenue';
	histogram Net_Revenue;
	keylegend / location=inside position=topright;
run;

/* Kernel Estimate of Net Revenues */
proc kde data=Intro_Risk; 
	univar Net_Revenue / percentiles unistats plots=histdensity;
run;

/* Bivariate Kernel Estimate of Units to Price */
proc kde data=Intro_Risk; 
	bivar Units Price / bivstats plots=histsurface;
run;

/* Bivariate Kernel Estimate of Units to Variable Cost */
proc kde data=Intro_Risk; 
	bivar Units Var_Cost / bivstats plots=histsurface out=Units_Var_Cost;
run;

proc g3d data=Units_Var_Cost;
	label Value1 = "Units";
	label Value2 = "Variable Cost (VC)";
 	plot Value1*Value2=Density;
run;
quit;
