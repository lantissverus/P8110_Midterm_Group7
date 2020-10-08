*********************************************************;
*P8110 - Applied Regression 2							*;
*Maria Guzman - mgg2153								    *;
*Start date: 9/24/2020									*;
*Midterm Group Project									*;
*********************************************************;

proc import 
	out = work.midterm
	datafile = "C:\Users\MariaGuzman\Desktop\Columbia 20-21\Regression 2\MidtermProjectData.csv"
	dbms = csv replace;
	getnames = yes;
	datarow = 2;
run;

data midterm; set work.midterm; run;
proc contents data = midterm order = varnum; run;

************************************************;
*Results of the proc contents:				   *;
*1 PTSEX  									   *;
*2 PTAGE 									   *;	 
*3 DSMDEPHR                                    *; 
*4 DSMSUBHR  								   *;	
*5 SESCLASS *NEED TO REVERSE CODE*			   *;
*6 BEDEPON  								   *;
*7 BESUBON  								   *;
*8 ID  										   *;
*9 PARDEP 								       *;
*10 MSPARENT 								   *;
*of note: var10 is PARENTMS in documentation   *; 
************************************************;

proc freq data = midterm;
table SESCLASS; run;
/*SES class has 5 categories, where SESCLASS = 5 has 20 obervations. The documentation does not mention the existence 
of this 5th category, so our group will ensure that this category is accounted for*/

/*starting to recode missing variables*/
data midterm1; set midterm;
rename ID = child_id;
rename PARDEP = parent_dep;
rename DSMDEPHR = child_dep;
rename PTSEX = child_sex;
rename PTAGE = child_interview_age;
rename BEDEPON = child_onset_age;
rename DSMSUBHR = child_SA_status;
rename BESUBON = child_SA_age;
rename SESCLASS = ses_cat;
rename MSPARENT = parent_marital;
run;

proc contents data = midterm1; run; /*great, that worked and of note: all var = num*/

/*continuing to format the data; let's recode the missing to be . instead of -1*/
data midterm2; set midterm1;
   array change _numeric_;
   do over change;
   if change= -1 then change= .;
   end;
 run ;

 proc print data = midterm2; var child_onset_age; run;
 proc means n nmiss min max data = midterm2; var child_onset_age; run; /*n = 69, age range 4 - 23 years*/

 /*checking to make sure that it worked and it did!
proc print data = midterm2; var child_onset_age child_SA_age; run;
proc print data = midterm1; var child_onset_age child_SA_age; run;*/

 /*Now applying formats to the data*/
proc format;
value dep_status 1 = "Ever depressed" 0 = "Never depressed";
value gender 1 = "Male" 2 = "Female";	
value SA 1 = "Substance abuse" 0 = "No substance abuse";		 
value marital 1 = "Married with spouse" 2 = "Separated/Divorced" 3 = "Never Married";
run;

data midterm2; set midterm2;
format parent_dep dep_status. child_dep dep_status. child_sex gender. 
	child_SA_status SA. parent_marital marital.;
label parent_dep = "Depression status of index parent" child_dep = "Child depression status"
	 child_sex = "Sex of child" child_interview_age = "Age at interview"
	 child_onset_age = "Age of onset of depression" child_SA_status = "Child substance abuse status"
	 child_onset_age = "Age of onset of substance abuse" 
	 parent_marital = "Marital status of index parent" child_SA_age = "Age of onset of substance abuse in child"
	 child_id = "child's unique participant study ID";
run;

proc contents data = midterm2; run;

/*Last thing I want to do is reverse code and format ses_class*/
/*ignore
use an array??? lol too lazy to look this up
data midterm3; set midterm2;
ses_class = reverse(ses_cat);
put ses_class=;
run;
*/

data midterm3; set midterm2;
if ses_cat = 1 then ses_class = 5;
if ses_cat = 2 then ses_class = 4;
if ses_cat = 3 then ses_class = 3;
if ses_cat = 4 then ses_class = 2;
if ses_cat = 5 then ses_class = 1;
run;

/*ok great my quick and dirty method worked
proc print data = midterm3; var ses_cat ses_class; run;*/

data midterm4; set midterm3;
drop ses_cat;
label ses_class = "Social class of parent (higher value, higher SES status)";
run;

proc contents data = midterm4; run;

*********************************************************;
*P8110 - Homework 4 (midterm)							*;
*Maria Guzman - mgg2153								    *;
*Start date: 10/5/2020   								*;
*Midterm Group Project/HW								*;
*********************************************************;

/*"Investigators have hypothesized that offspring of a depressed parent are more likely to have
pre-pubertal onset ( < 13 years of age) depression than offspring of a non-depressed parent"
therefore we define our event as pre-pubertal onset, 1 vs. 0 which I will create below:*/

data q2; set midterm4;
if child_onset_age = . then prepub_onset = .;
if 13 > child_onset_age ge 4 then prepub_onset = 1;
if child_onset_age ge 13 then prepub_onset = 0;
run;

proc print data = q2; var child_onset_age prepub_onset; run;
