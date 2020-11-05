*************************
* P8110 Regression II
* Midterm Project
* Group 7
* Edited 11/2/2020
*************************;

*************************
* Set up
*************************;

/* This data set contains information about the effects of parental 
depression on substance abuse and depression of their children. */

/* Import dataset and assign varnames */
proc import 
	out = work.midterm
	datafile = "/home/u45165017/Regression II/MT Project/MidtermProjectData.csv"
	dbms = csv replace;
	getnames = yes;
	datarow = 2;
run;

/* Investigate data set */
proc contents data = midterm order = varnum; run;

************************************************
*Results of the proc contents:
*1 PTSEX
*2 PTAGE
*3 DSMDEPHR
*4 DSMSUBHR
*5 SESCLASS *will need to reverse code values*
*6 BEDEPON
*7 BESUBON
*8 ID
*9 PARDEP
*10 MSPARENT
*of note: var10 is PARENTMS in documentation
************************************************;

/* Recode variable names */
data midterm1; set midterm;
rename ID = child_id;
rename PARDEP = parent_dep_status;
rename DSMDEPHR = child_dep_status;
rename PTSEX = child_sex;
rename PTAGE = child_interview_age;
rename BEDEPON = child_dep_age;
rename DSMSUBHR = child_subs_status;
rename BESUBON = child_subs_age;
rename SESCLASS = ses_cat;
rename MSPARENT = parent_marital;
run;

/* Recode missing values and SES categories, 
	create prepubertal var and survival time var */
proc freq data = midterm1;
tables parent_dep_status
	   child_dep_status
	   child_sex
	   child_interview_age
	   child_dep_age
	   child_subs_status
	   child_subs_age
	   ses_cat
	   parent_marital;
run;

	/* Missing values found in child_dep_age, child_subs_age,
	ses_cat. 
	Note: ses_cat is coded as 5 categories, not 4 per codebook.
	Note: child_interview_age contains values outside the 
	codebook provided range (6-23). */

data midterm2;
set midterm1;
	*recode missing values;
	if child_dep_age = -1 then child_dep_age = .;
	if child_subs_age = -1 then child_subs_age = .;
	*recode ses_cat;
	if ses_cat = 1 then ses_class = 5;
	if ses_cat = 2 then ses_class = 4;
	if ses_cat = 3 then ses_class = 3;
	if ses_cat = 4 then ses_class = 2;
	if ses_cat = 5 then ses_class = 1;
	drop ses_cat;
	*code var for prepubertal depression onset;
	if child_dep_age < 13 then prepubertal = 1;
	else prepubertal = 0;
run;

/* Apply formats and labels */
proc format;
	value depf 
		0 = "Never depressed" 
		1 = "Ever depressed";
	value sexf 
		1 = "Male" 
		2 = "Female";
	value subf 
		0 = "No substance abuse" 
		1 = "Substance abuse";
	value maritalf 
		1 = "Married with spouse" 
		2 = "Separated/Divorced" 
		3 = "Never Married";
	value ynf
		0 = "No"
		1 = "Yes";
	run;

data midterm2;
set midterm2;
	format parent_dep_status depf.
		   child_dep_status depf.
		   child_sex sexf.
		   child_subs_status subf.
		   parent_marital maritalf.
		   prepubertal ynf.;
	label child_id = "Child unique study ID"
		  parent_dep_status = "Parent depression status"
		  child_dep_status = "Child depression status"
		  child_sex = "Child biological sex"
		  child_interview_age = "Child age at interview"
		  child_dep_age = "Child age of depression onset"
		  child_subs_status = "Child substance abuse status"
		  child_subs_age = "Child age of substance abuse onset"
		  ses_class = "Family SES quantile (5 = high)"
		  parent_marital = "Parent marital status"
		  prepubertal = "Child prepubertal (< age 13) depression onset";
run;


*************************
* Test for Hypothesis 1
*************************;

/* Hypothesis: children with parents with depression are more likely to 
have prepubertal onset depression than children of parents without 
depression, but equally likely to have post-pubertal depression, after
adjusting for demographic and social covariates. */

/* Define the time to event and time to censoring */
data midterm_h1;
set midterm2;
if child_dep_status = 1 then time_dep = child_dep_age;
else time_dep = child_interview_age;

ses_bin = .;
if ses_class in (1,2) then ses_bin = 0; *Dichotomize ses_class;
if ses_class in (3,4,5) then ses_bin =1;
drop ses_class;
run;

/* K-M curves and log-rank test */
proc lifetest data=midterm_h1 method=km alpha=0.05 plots=survival;
time time_dep*child_dep_status(0);
strata parent_dep_status;
title 'Part 1 : Kaplan-Meier estimate by parent depression status';
run;

/* Test the proportionality.*/
proc phreg data=midterm_h1;
model time_dep*child_dep_status(0) = parent_dep_status child_sex parent_marital ses_bin/ ties=efron;
assess PH / resample;
run;

/*
Comment: Assumption of proportionality is violated for parent_dep_status (p < 0.0001).
*/

/* Test the interaction */
proc phreg data=midterm_h1;
class parent_dep_status (ref = last) child_sex (ref = first) parent_marital (ref = first) ses_bin (ref = last) / param= ref;
model time_dep*child_dep_status(0) = parent_dep_status child_sex parent_marital ses_bin parent_dep_time/ ties=efron; 
parent_dep_time =  parent_dep_status*prepubertal;
title 'Cox model with interaction of survival time and parental depression';
run;

/*
Comment: The interaction is significant at the 5% significance level. (p-value < 0.0001)
Because parents' depression status is a key predictor of interest in this study, 
we chose to use the interaction method to test nonproportionality.
Since the interaction does not have a significant coefficient, 
then we can conclude that the PH assumption is not violated 
for parents' depression status variable.
/*


/* Stratify by ses_bin */
proc phreg data=midterm_h1;
model time_dep*child_dep_status(0) = parent_dep_status child_sex parent_marital / ties=exact;
strata ses_bin;
title 'Cox model with SES stratification';
run;


*************************
* Test for Hypothesis 2
*************************;

/* Hypothesis: Is there an effect of prior depression in children, or 
parental depression, on age of substance use in children, adjusting for
social & environmental factors? */

/* Define survival time for substance use */
data midterm_h2;
set midterm_h1;
if child_subs_status = 1 and child_subs_age ^=. then time_sub = child_subs_age;
else time_sub = child_interview_age;
run;

/*Here I am comparing child_onset_age with the defined survival time, 
this way I take into consideration child onset age for the age of substance abuse onset 
for those who are positive for substance abuse
 
for those without SA abuse, where time = child_interview_age, they are coded as prior_depression = 0
for those who have onset after time, they are also coded as 0
OTHERWISE, they are prior depression positive since the alternative is those whose onset age preceded substance abuse time*/

/* Fit a Cox model treat prior depression as a time-dependent variable */

/* Testing on the effect of prior depression in offspring, as well as the 
effect of parent’s depression status, on the age of onset of substance abuse */

/* Adjusted for child biological sex, family SES and parent marital status*/

proc phreg data = midterm_h2;
class parent_dep_status (ref = last) child_sex (ref = first) parent_marital (ref = first) ses_bin (ref = last) / param= ref;
model time_sub*child_subs_status(0) = prior_dep parent_dep_status child_sex ses_bin parent_marital
/ ties = efron;
/* Define prior depression status for substance use */
if (time_dep >= time_sub) or (time_dep = .) 
then prior_dep = 0;
else prior_dep = 1;
title "Cox model with prior depression as a time-dependent variable";
run;

* Used age at the interview for observations with missing data at child_subs_age;
* No sufficient evidence of statistically significant effect of prior depression in offspring, 
on the age of onset of substance abuse, adjusted for child biological sex, family SES and parent marital status (p=0.7343);
* No sufficient evidence of statistically significant effect of parent’s depression status, 
on the age of onset of substance abuse, adjusted for child biological sex, family SES and parent marital status (p=0.0815);