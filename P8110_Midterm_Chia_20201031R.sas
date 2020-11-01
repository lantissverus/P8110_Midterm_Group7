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
	if child_dep_age < 13 and child_dep_status = 1 then prepubertal = 1;
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


/*table for each covariates used in hypothesis 1 */
proc tabulate data = midterm2 missing;
class parent_dep_status child_sex parent_marital ses_class prepubertal;
tables child_sex*(n colpctn) parent_marital*(n colpctn) ses_class*(n colpctn) prepubertal*(n colpctn), parent_dep_status;
keylabel pctn = "%";
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

/* K-M curves and log-rank test */

ods graphics on;
proc lifetest data=midterm_h1 method=km alpha=0.05 plots=survival;
time TIME*prepubertal(0);
strata parent_dep_status;
title 'Part 1 : Kaplan-Meier estimate by parent depression status';
run;
ods graphics off;


/*Examine the model */

proc phreg data = midterm_h1;
class parent_dep_status (ref = first) child_sex (ref = first) 
parent_marital (ref = first) ses_class (ref = last) /param= ref;
model TIME*prepubertal(0) = parent_dep_status child_sex parent_marital ses_class/ risklimits covb ties=efron;
title 'Adjusted model for demographic and social characteristics';
run;

/* Found two missing values here for ses_class =. */

ods graphics on;
proc phreg data = midterm_h1 plots(cl) = s;
class parent_dep_status (ref = first) child_sex (ref = first) 
parent_marital (ref = first) ses_class (ref = last) /param= ref;
model TIME*prepubertal(0) = parent_dep_status child_sex parent_marital ses_class/ ties=efron;
baseline out = a survival = s lower = lcl upper = ucl;
title 'Estimate S(t) for covariaes at the mean value or reference cell';
run;
ods graphics off;



/* Test the proportionality.*/

ods graphics on;
proc phreg data=midterm_h1;
model TIME*child_dep_status(0) = parent_dep_status child_sex parent_marital 
ses_class/ ties=efron;
assess PH / resample;
run;
ods graphics off;


*******************************
Comment: 
1)Based on the PH assumption assessment, we found that for parents depression status, 
among 1000 simulated paths, only <0.1% of them have extreme points that exceeded the most 
extreme points of the observed path. The p-value was produced by RESAMPLE option.

2)Based on the PH assumption assessment, we found that for Categorized social class of parent, 
among 1000 simulated paths, only <3.2% of them have extreme points that exceeded the most 
extreme points of the observed path. The p-value was produced by RESAMPLE option.
******************************;


/*Test the interaction of parents depression status and time*/

proc phreg data=midterm_h1;
model TIME*prepubertal(0) = parent_dep_status child_sex parent_marital 
ses_class parent_dep_time/ ties=efron;
parent_dep_time =  parent_dep_status * TIME;
title 'Cox model with time * parents_dep_status interaction';
run;

*************************************
Comment: The interaction is not significant with p-value = 0.9909 at 5% signifiacnce level.
Because parents' depression status is a key predictor of interest in this study, 
we chose to use the interaction method to test nonproportionality.
Since the interaction does not have a significant coefficient, 
then we can conclude that the PH assumption is not violated 
for parents' depression status variable.
*************************************;

/*Test for interaction term */
proc phreg data=midterm_h1;
model TIME*prepubertal(0) = parent_dep_status child_sex parent_marital 
ses_class ses_class_time/ ties=exact;
ses_class_time =  ses_class * TIME;
title 'Cox model with time * ses_class interaction';
run;

/*Stratify by ses_class */
proc phreg data=midterm_h1;
model TIME*prepubertal(0) = parent_dep_status child_sex parent_marital / ties=exact;
strata ses_class;
title 'Cox model with time * ses_class straification';
run;
*************************************
Comment: The interaction is not significant with p-value = 0.15 for interaction term 
and p-value = 0.0966 for the ses_class at 5% signifiacnce level.
Because ses_class is not a key predictor of interest in this study and it is categorical, 
we chose to use the stratification method to test nonproportionality.
Results of the model estimation are presented above. 
Convergence criterion is satisfied, ses_class – which is the suspected covariate in the stratified model 
– is significant at any acceptable level. 
As it can be noticed, hazard ratio for ses_class differs to a large extent as compared to the highest ses level and is equal to 7.97, 
which means that every class the risk of being ever depressed increases by 697% as compared with the highest class (ses_class = 5). 
*************************************;

/*Given the low number in ses_class 1 and 5 (both <= 20) , which may affect the power, 
we decided to create a new variable to combine them to 2 and 5 respectively, 
and examine the proportional hazard assumption again.*/

data midterm_ses;
set midterm_h1;
if ses_class in (1, 2) then ses_new = 1;
else if ses_class = 2  then ses_new = 2;
else if ses_class = 3 then ses_new = 3;
else if ses_class in (4, 5) then ses_new = 4;
run;

ods graphics on;
proc phreg data=midterm_ses;
model TIME*child_dep_status(0) = parent_dep_status child_sex parent_marital 
ses_new/ ties=efron;
assess PH / resample;
run;
ods graphics off;

*******************************************
Once we combine ses class 1 and 5 with 2 and 4 respectively, the new variable does not
seem to be significant anymore. Therefore, we would not go further for testing its interaction and
stratification. In this model, only parents depression status seems to be more significant, 
and needs to be tested for interaction because it is a key predictor of interest.
*****************************************; 

**************************
Test for Hypothesis 2
*************************;
data midterm2;
set midterm2;
if child_subs_status = 1 then surv2 = child_subs_age;
else if chid_subs_status = 1 and child_subs_age = . then surv2 =  child_interview_age;
else surv2 = child_interview_age;
run;

/* Create histograms to examine the frequency distribution fo variable */

ods graphics on;
proc freq data = midterm2;
tables parent_dep_status child_sex ses_class parent_marital child_subs_status / plots= freqplot;
run;
ods graphics off;

/* There are 2 missing value in ses_class. */

ods graphics on;
proc freq data = midterm2;
tables surv2 / plots= freqplot;
run;
ods graphics off;

/* From surv2, we learn that there are 11 missing values in this variables */

/* Fit a Cox model treat prior depression as a time-dependent variable */
/* Testing on the effect of prior depression in offspring, as well as the
effect of parent’s depression status, on the age of onset of substance abuse */
/* Adjusted for child biological sex, family SES and parent marital status*/
proc phreg data = midterm2;
model surv2*child_subs_status(0) = prior_dep parent_dep_status child_sex ses_class parent_marital
/ ties=efron;
/* Define prior depression status for substance use */
if (child_dep_age >= surv2) or (surv2 = child_interview_age)
then prior_dep = 0;
else prior_dep = 1;
title "Cox model with prior depression as a time-dependent variable";
run;

***************************************************
Comment:
There are total 13 missing values in this analysis. 

We can tell from this model that prior depression has no effect on the hazard of substance abuse,
with p-value = 0.0613 at 5% significance level, given demographic and social characteristics. 

However, parents depression status has effect on the hazard of substance abuse with p-value = 0.0475
at 5% significance level, given demographic and social characteristics.

The risk of substance abuse among children with ever depressed parents is 3.27 times 
the risk of substance abuse among children with never depressed parents adjusting for prior depression
, demographic and social characteristics.  
**************************************************;
