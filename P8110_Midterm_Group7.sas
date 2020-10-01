data Midterm;
infile'/home/u45121447/Myfolder/P8110/MidtermProjectData.csv'  firstobs=2 
delimiter= ',' MISSOVER DSD;
input PTSEX PTAGE DSMDEPHR 	DSMSUBHR SESCLASS BEDEPON BESUBON ID PARDEP MSPARENT; 
run;

proc format;
	value PARDEP 0="never depressed" 1="Ever depressed" ;
	value DSMDEPHR 0 ='Never depressed' 1 ='Ever depressed';
	value PTSEX 0 ='male' 1= 'female';
	value DSMSUBHR 0 = 'no substance abuse' 1= 'substance abuse';
	value PARENTMS 1 ='Married with spouse' 2 = 'Separated/Divorced' 3= 'Never Married';
	run;

data Midterm_t1;
retain ID PARDEP DSMDEPHR PTSEX PTAGE BEDEPON DSMSUBHR BESUBON SESCLASS PARENTMS;
set Midterm;
run;

