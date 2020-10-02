* Import and clean MT Project data;
proc import out = MT
	datafile = '\\Mac\Home\Desktop\Fall 2020\Regression II\MT Project\MidtermProjectData.csv' dbms = csv replace;
	getnames = YES;
run;
data MT;
	set MT;
	drop ID; * Drop ID;
	if BEDEPON = -1 then BEDEPON = .; * Code missing data;
	if BESUBON = -1 then BESUBON = .;
	if BEDEPON = . then PREPUBER = .; * Define pre-pubertal onset (<13 years of age) depression;
	else if BEDEPON < 13 then PREPUBER = 1;
	else PREPUBER = 0;
	if DSMDEPHR = 1 then TIME = BEDEPON; * Define the survival time;
	else TIME = PTAGE;
run;

* Apply label and format;
proc format;
	value depre 1 = "Ever depressed"
				0 = "Never depressed";
	value gender 1 = "Male"
			   2 = "Female";	
	value yn 1 = "Yes"
			 0 = "No";		 
	value marital 1 = "Married with spouse"
				 2 = "Separated/Divorced"
				 3 = "Never Married";
run;
data MT;
	set MT;
	label PARDEP = "Depression status of index parent"
		DSMDEPHR = "Child depression status"
		PTSEX = "Sex of child"
		PTAGE = "Age at interview"
		BEDEPON = "Age of onset of depression"
		DSMSUBHR = "Child substance abuse status"
		BESUBON = "Age of onset of substance abuse"
		SESCLASS = "Social class of parent"
		MSPARENT = "Marital status of index parent"
		TIME = "Survival time in years"
		PREPUBER = "Pre-pubertal depression status of child";
	format PARDEP depre.
		   DSMDEPHR depre.
		   PTSEX gender.
		   DSMSUBHR yn.
		   MSPARENT marital.
		   PREPUBER yn.;
run;
proc print data = MT label;
run;

* Check the min and max of age;
proc means data = MT n min max MAXDEC=0; 
	var PTAGE BEDEPON;
	output out=meansout;
run;
* Start and end points in defining the survival time in years: 4 - 25;

* Use Kaplan-Meier survival curves and log-rank test to compare age of developing depression 
between offspring of a depressed index parent and offspring of a normal index parent;
 proc lifetest data = MT method = KM alpha = 0.05 plots = survival(test);
 	time TIME * DSMDEPHR(0);
 	strata PARDEP;
 	title "Kaplan-Meier estimates by index parent depression status";
 run;
 * (Log-Rank Test) p-value = 0.0056;
 * We conclude that there is a significant difference between offspring of a depressed index parent 
 and offspring of a normal index parent in survival functions, at significance level of 0.05;

* Generate a descriptive statistics table for the time-invariant covariates in the study, 
 stratified by parental depression status;
proc tabulate data = MT missing;
	class PTSEX DSMDEPHR DSMSUBHR SESCLASS MSPARENT PARDEP;
	table PTSEX*(n pctn) DSMDEPHR*(n pctn) DSMSUBHR*(n pctn) SESCLASS*(n pctn) MSPARENT*(n pctn), PARDEP;
	keylabel pctn = "%";
	title "Table 1: Demographic and Clinical Characteristics of the offspring stratified by parental depression status.";
run;

