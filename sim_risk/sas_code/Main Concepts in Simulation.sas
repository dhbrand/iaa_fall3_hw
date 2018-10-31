/*-----------------------------*/
/* Main Concepts in Simulation */
/*                             */
/*        Dr Aric LaBarr       */
/*-----------------------------*/

/* Introduction to Simulation */
data SPIndex;
	do i = 1 to 10000;
		P0 = 1000;
		r = RAND('Normal', 0.0879, 0.1475);
		
		P1 = P0*(1 + r);

		output;
	end;
run;

proc means data=SPIndex;
	var P1;
run;

proc sgplot data=SPIndex;
	title 'One Year Value Distribution';
	histogram P1;
	refline 1000 / axis=x label='Initial Inv.' lineattrs=(color=red thickness=2);
	keylegend / location=inside position=topright;
run;

proc univariate data=SPIndex;
	var P1;
	histogram P1 / normal kernel;
run;
quit;

/* Distribution Selection - Kernel Estimation */
proc kde data=SPIndex;
	univar P1 / unistats;
run;

proc iml;
	start SmoothBootstrap(x, B, Bandwidth);
		N = 1;
		s = Sample(x, N // B);
		eps = j(B, N);
		call randgen(eps, "Normal", 0, Bandwidth);
		return( s + eps );
	finish;
	
	use WORK.SPIndex;
	read all var {P1} into x;
	close WORK.SPIndex;

	call randseed(12345);
	y = SmoothBootstrap(x, 100, 25.55);
	Est_y = y`;
	create Smooth var {"Est_y"};
	append;
	close Smooth;
quit;

proc univariate data=Smooth;
	var Est_y;
	histogram Est_y / normal kernel;
run;
quit;

/* Multiple Input Probability Distributions */
data SPIndex30;
	do i = 1 to 10000;
		P0 = 1000;
		r = RAND('Normal', 0.0879, 0.1475);
		
		Pt = P0*(1 + r);

		do j = 1 to 29;
			r = RAND('Normal', 0.0879, 0.1475);
		
			Pt = Pt*(1 + r);
		end;

		output;
	end;
run;

proc means data=SPIndex30;
	var Pt;
run;

proc sgplot data=SPIndex30;
	title '30 Year Value Distribution';
	histogram Pt;
	refline 1000 / axis=x label='Initial Inv.' lineattrs=(color=red thickness=2);
	keylegend / location=inside position=topright;
run;

proc univariate data=SPIndex30;
	var Pt;
	histogram Pt / normal kernel;
run;
quit;

/* Correlated Inputs - PROC's and DATA STEP */
data Corr_Matrix;
	do i=1 to 10000;
		_type_ = "corr";
		_name_ = "S_r";

  		S_r = 1.0; 
  		B_r = -0.2;

  		output;
  		_name_ = "B_r";

		S_r = -0.2; 
  		B_r = 1.0;

  		output;
	end;
run;

data Naive;
	do i=1 to 10000;
  		S_r=0;
  		B_r=0;
  		output;
	end;
run;

proc model noprint;
  /*Distributiuon of S*/
  S_r = 0.0879;/*Mean of S*/
  errormodel S_r ~Normal(0.02175625); /*Variance is defined here as 0.1475^2*/

  /*Distribution of B*/
  B_r = 0.04;/*Mean of B*/
  errormodel B_r ~Normal(0.0049); /*Variance is defined here as 0.07^2*/

  /*Generate the data and store them in the dataset work.Correlated_X */
  solve S_r B_r/ random=30 sdata=Corr_Matrix
  data=Naive out=Correlated_X(keep=S_r B_r i _rep_);
    by i;

  run;
quit;

data Correlated_X;
  set Correlated_X;
  by i;
  if first.i then delete;
  rename i=simulation;
  rename _rep_=obs;
run;

proc transpose data=Correlated_X out=SB30;
	var S_r B_r;
	by simulation;
run;

data SB30A;
	set SB30;
	P30 = 500*(1+COL1)*(1+COL2)*(1+COL3)*(1+COL4)*(1+COL5)*
			  (1+COL6)*(1+COL7)*(1+COL8)*(1+COL9)*(1+COL10)*
			  (1+COL11)*(1+COL12)*(1+COL13)*(1+COL14)*(1+COL15)*
			  (1+COL16)*(1+COL17)*(1+COL18)*(1+COL19)*(1+COL20)*
			  (1+COL21)*(1+COL22)*(1+COL23)*(1+COL24)*(1+COL25)*
			  (1+COL26)*(1+COL27)*(1+COL28)*(1+COL29)*(1+COL30);
run;

proc transpose data=SB30A out=SB30A_Final;
	var P30;
	by simulation;
run;

data SB30A_Final;
	set SB30A_Final;
	Value = S_r + B_r;
run;

proc sgplot data=SB30A_Final;
	title '30 Year Value Distribution';
	histogram Value;
	refline 1000 / axis=x label='Initial Inv.' lineattrs=(color=red thickness=2);
	keylegend / location=inside position=topright;
run;

proc univariate data=SB30A_Final;
	var Value;
	histogram Value / normal kernel;
run;
quit;

data SB30B;
	set SB30;
	if _NAME_ = 'S_r' then P30 = 300*(1+COL1)*(1+COL2)*(1+COL3)*(1+COL4)*(1+COL5)*
			  (1+COL6)*(1+COL7)*(1+COL8)*(1+COL9)*(1+COL10)*
			  (1+COL11)*(1+COL12)*(1+COL13)*(1+COL14)*(1+COL15)*
			  (1+COL16)*(1+COL17)*(1+COL18)*(1+COL19)*(1+COL20)*
			  (1+COL21)*(1+COL22)*(1+COL23)*(1+COL24)*(1+COL25)*
			  (1+COL26)*(1+COL27)*(1+COL28)*(1+COL29)*(1+COL30);
	if _NAME_ = 'B_r' then P30 = 700*(1+COL1)*(1+COL2)*(1+COL3)*(1+COL4)*(1+COL5)*
			  (1+COL6)*(1+COL7)*(1+COL8)*(1+COL9)*(1+COL10)*
			  (1+COL11)*(1+COL12)*(1+COL13)*(1+COL14)*(1+COL15)*
			  (1+COL16)*(1+COL17)*(1+COL18)*(1+COL19)*(1+COL20)*
			  (1+COL21)*(1+COL22)*(1+COL23)*(1+COL24)*(1+COL25)*
			  (1+COL26)*(1+COL27)*(1+COL28)*(1+COL29)*(1+COL30);
run;

proc transpose data=SB30B out=SB30B_Final;
	var P30;
	by simulation;
run;

data SB30B_Final;
	set SB30B_Final;
	Value = S_r + B_r;
run;

proc sgplot data=SB30B_Final;
	title '30 Year Value Distribution';
	histogram Value;
	refline 1000 / axis=x label='Initial Inv.' lineattrs=(color=red thickness=2);
	keylegend / location=inside position=topright;
run;

proc univariate data=SB30B_Final;
	var Value;
	histogram Value / normal kernel;
run;
quit;

data SB30_Compare;
	merge SB30A_Final(rename=(Value=ValueA)) SB30B_Final(rename=(Value=ValueB));
	by simulation;
	Difference = ValueA - ValueB;
run;

proc sgplot data=SB30_Compare;
	title '30 Year Value Distribution Difference';
	title2 'Between Strategies (A-B)';
	histogram Difference;
	keylegend / location=inside position=topright;
run;

proc univariate data=SB30_Compare;
	var Difference;
	histogram Difference / normal kernel;
run;
quit;
