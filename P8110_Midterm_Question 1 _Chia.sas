*************************
* P8110 Regression II
* Midterm Project
* Group 7
* Edited 10/8/2020
*************************;

/* This data set contains information about the effects of parental 
depression on substance abuse and depression of their children. */

/* Import dataset and assign varnames */
proc import 
	out = work.midterm
	datafile = "/home/u45121447/Myfolder/P8110/MidtermProjectData.csv"
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
	Note: child_interview_age contains values (5-25) outside the 
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
	prepubertal = 0;
	if child_dep_age < 13 then prepubertal = 1;
	if child_dep_age = . then prepubertal = .;
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
		   prepubertal ynf.;		/* note: we don't need a value format for ses right? */
	label child_id = "Child unique study ID"
		  parent_dep_status = "Parent depression status"
		  child_dep_status = "Child depression status"
		  child_sex = "Child biological sex"
		  child_interview_age = "Child age at interview"
		  child_dep_age = "Child age of depression onset"
		  child_subs_status = "Child substance abuse status"
		  child_subs_age = "Child age of substance abuse onset"
		  ses_class = "Family SES quantile (5 = high)" /*should be ses_class?*/
		  parent_marital = "Parent marital status"
		  prepubertal = "Child prepubertal depression onset";
run;


**************************
Test for Hypothesis 1
*************************;

/*Define the time to event and time to censoring here*/

data midterm_h1;
set midterm2;
if child_dep_status = 1 then TIME = child_dep_age; * Define the survival time;
	else TIME = child_interview_age;
run;

*****************************
**Question to ask: Should SES level also be included in the Cox model?
- Include the SES class in model first. 
****************************;

/* Test the proportionality.*/

ods graphics on;
proc phreg data=midterm_h1;
model TIME*prepubertal(0) = parent_dep_status child_sex parent_marital ses_class/ ties=efron;
assess PH / resample;
run;
ods graphics off;

*******************************
Comment: Based on the PH assumption assessment, we found that for parents depression status, 
among 1000 simulated paths, only 4.3% of them have extreme points that exceeded the most 
extreme points of the observed path. The p-value was produced by RESAMPLE option.
******************************;

/*Test the interaction*/

proc phreg data=midterm_h1;
model TIME*prepubertal(0) = parent_dep_status child_sex parent_marital 
ses_class parent_dep_time/ ties=efron;
parent_dep_time =  parent_dep_status * TIME;
title 'Cox model with time * parents_dep_status interaction';
run;
*************************************
Comment: The interaction is not significant with p-value = 0.9918 at 5% signifiacnce level.
Because parents' depression status is a key predictor of interest in this study, 
we chose to use the interaction method to test nonproportionality.
Since the interaction does not have a significant coefficient, 
then we can cnclude that the PH assumption is not violated 
for parents' depression status variable.
*************************************;
