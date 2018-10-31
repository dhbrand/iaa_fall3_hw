/*-----------------------------*/
/* Theory and Model Assessment */
/*      Through Simulation     */
/*                             */
/*        Dr Aric LaBarr       */
/*-----------------------------*/

/* Theory Assessment - CLT */
%let Sample_Size = 50;
%let Simulation_Size = 10000;

data CLT;
	do sim = 1 to &Simulation_Size;
		do obs = 1 to &Sample_Size;
			call streaminit(12345);
			X1 = RAND('Normal', 2, 5);
			X2 = 5 + 100*RAND('Uniform');
			X3 = 3 + RAND('Exponential');
			
			output;
		end;
	end;
run;

proc means data=CLT noprint mean;
	var X1 X2 X3;
	by sim;
	output out=Means mean(X1 X2 X3)=Mean_X1 Mean_X2 Mean_X3;
run;

proc sgplot data=Means;
	title 'Sampling Distribution of Means';
	title2 'Normal Distribution';
	histogram Mean_X1;
	keylegend / location=inside position=topright;
run;

proc sgplot data=Means;
	title 'Sampling Distribution of Means';
	title2 'Uniform Distribution';
	histogram Mean_X2;
	keylegend / location=inside position=topright;
run;

proc sgplot data=Means;
	title 'Sampling Distribution of Means';
	title2 'Exponential Distribution';
	histogram Mean_X3;
	keylegend / location=inside position=topright;
run;

proc univariate data=Means;
	var Mean_X1-Mean_X3;
	histogram Mean_X1 / normal kernel;
	histogram Mean_X2 / normal kernel;
	histogram Mean_X3 / normal kernel;
run;
quit;

/* Target Shuffling */
data Fake;
	do obs = 1 to 100;
		call streaminit(12345);
		X1 = RAND('Normal', 0, 1);
		X2 = RAND('Normal', 0, 1);
		X3 = RAND('Normal', 0, 1);
		X4 = RAND('Normal', 0, 1);
		X5 = RAND('Normal', 0, 1);
		X6 = RAND('Normal', 0, 1);
		X7 = RAND('Normal', 0, 1);
		X8 = RAND('Normal', 0, 1);
		Err = RAND('Normal', 0, 8);

		Y = 5 + 2*X2 - 3*X8 + Err;
			
		output;
	end;
run;

proc reg data=Fake outest=Rsq_A plot=none;
	model Y = X1-X8 / selection=backward slstay=0.05 adjrsq;
run;
quit;

%macro shuffle(sim);
	data shuffle_&sim;
		set Fake;
		Uni = RAND('Uniform');
		Y_&sim = Y;
		keep Y_&sim Uni;
	run;

	proc sort data=shuffle_&sim;
		by Uni;
	run;

	data Fake;
		merge Fake shuffle_&sim;
		drop Uni;
	run;

	proc datasets noprint;
		delete shuffle_&sim;
	run;
	quit;
%mend shuffle;

%macro sim_shuffle(sim);
	%do i = 1 %to &sim;
		%shuffle(&i);
	%end;
%mend sim_shuffle;

%sim_shuffle(500);

proc reg data=Fake noprint outest=Rsq_A;
	model Y Y_1-Y_500 = X1-X8 / selection=backward slstay=0.05 adjrsq;
run;
quit;

proc sgplot data=Rsq_A;
	title 'Distribution of Adjusted R-Squared';
	histogram _ADJRSQ_;
	refline 0.178 / axis=x label='True Model' lineattrs=(color=red thickness=2);
	keylegend / location=inside position=topright;
run;

proc univariate data=Rsq_A;
	var _ADJRSQ_;
run;
quit;

proc sgplot data=Rsq_A;
	title 'Distribution of Number of Significant Variables';
	histogram _IN_;
	refline 2 / axis=x label='True Model' lineattrs=(color=red thickness=2);
	keylegend / location=inside position=topright;
run;

