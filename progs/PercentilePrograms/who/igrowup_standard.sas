/***********************************************************************
****WHO Child Growth Standards                                 *********
****Department of Nutrition for Health and Development         *********
****World Health Organization                                  *********
****Last modified on 22/05/2007 versions 8.2 and above         *********
************************************************************************/

%macro igrowup_standard(label=, 
             ref_lib=, 
             data_lib=,
             data_in=,
             sex=, 
             age=, 
             age_unit=, 
             weight=, 
             lenhei=,
             headc=,
             armc=,
             triskin=,
             subskin=, 
             measure=, 
             oedema=,
             sw=,  
			 group=);

ods listing close;
options  orientation =  landscape;
title1 justify=left &label;
title2 justify=left "Standard analysis (see Readme.pdf for its definition)";

footnote1 justify=left
 "*Observations with missing age are included in the total group for weight-for-length/height";
footnote2 justify=left
 "related statistics.";

libname _datalib "&data_lib";
libname _reflib "&ref_lib";

/*******************************************************************
To generate the formats used in the exported report tables;
********************************************************************/

proc format;

 value _age  

           -1="0-60" 
           -1.3="3-60" 
            0="0-5"
			0.3="3-5"
            1="6-11"
			2="12-23"
            3="24-35"
			4="36-47"
			5="48-60";

 value _sex 
          0="Sexes combined" 
          1="Males" 
          2="Females"; 

 value _stat 
            0="N"
            1="<-3 SD"
            2="<-2 SD"
		    3=">+1 SD"
            4=">+2 SD"
			5=">+3 SD"
			6="Mean"
			7="SD";

 value _cat 
            0=" "
            1="Prevalence (95% CI)"
            2="z-scores";

 value _order 
      1="WEIGHT-FOR-AGE"
      2="LENGTH/HEIGHT-FOR-AGE"  
      3="WEIGHT-FOR-LENGTH/HEIGHT" 
      4="BMI-FOR-AGE"
      5="HEAD CIRCUMFERENCE(HC)-FOR-AGE"
      6="ARM CIRCUMFERENCE(AC)-FOR-AGE"
      7="TRICEPS SKINFOLD(TS)-FOR-AGE"
      8="SUBSCAPULAR SKINFOLD(SS)-FOR-AGE";
run;

/*******************************************************************
To set up the WHO Child Growth Standards;
********************************************************************/

data _refwaz;
 set _reflib.weianthro;
 rename sex=_sex age=_age_day l=_l m=_m s=_s;
run;
proc sort data=_refwaz;
 by _sex _age_day;
run;

data _refhaz;
 length loh $3.;
 set _reflib.lenanthro;
 rename sex=_sex age=_age_day l=_l m=_m s=_s loh=_loh;
run;
proc sort data=_refhaz;
 by _sex _age_day _loh;
run;

data _refwfl;
 length lorh $3.;
  set _reflib.wflanthro;
 rename  sex=_sex length=_lht_cm l=_l m=_m s=_s lorh=_loh;
run;

data _refwfh;
 length lorh $3.;
 set _reflib.wfhanthro;
 rename  sex=_sex height=_lht_cm l=_l m=_m s=_s lorh=_loh;
run;

/*************************************************************************
To calculate the slopes and intercepts of the lines between the tabulated  
points in the weight-for-length standard;
**************************************************************************/

data _refwfl;
 set  _refwfl;
 _lht_id=_lht_cm*10000; 
run;

proc sort data=_refwfl;
 by _sex _lht_id;
run;

data _templ;
 set _refwfl;
 by _sex _lht_id;
 if first._sex then delete;
 _lht_id=_lht_id-1000;
 keep _sex _lht_id _lht_cm _l  _m  _s;
 rename _lht_cm=_lht_cma  _l=_la  _m=_ma  _s=_sa;
run;

data _refwfl;
 merge _refwfl _templ;
 by _sex _lht_id;
run;

data _refwfl;
 set _refwfl;
 _intl=(_la*_lht_cm-_l*_lht_cma)/(_lht_cm-_lht_cma);
 _slpl=(_l-_la)/(_lht_cm-_lht_cma);

 _intm=(_ma*_lht_cm-_m*_lht_cma)/(_lht_cm-_lht_cma);
 _slpm=(_m-_ma)/(_lht_cm-_lht_cma);

 _ints=(_sa*_lht_cm-_s*_lht_cma)/(_lht_cm-_lht_cma);
 _slps=(_s-_sa)/(_lht_cm-_lht_cma);

 drop _lht_cm _lht_cma _la _ma _sa;
run;

/************************************************************************
To calculate the slopes and intercepts of the lines between the tabulated  
points in the weight-for-height standard;
*************************************************************************/

data _refwfh;
 set _refwfh;
 _lht_id=_lht_cm*10000; 
run;

proc sort data=_refwfh;
 by _sex _lht_id;
run;

data _temph;
 set _refwfh;
 by _sex _lht_id;
 if first._sex then delete;
 _lht_id=_lht_id-1000;
 keep _sex _lht_id _lht_cm _l _m _s _loh;
 rename _lht_cm=_lht_cma _l=_la  _m=_ma  _s=_sa;
run;

data _refwfh;
 merge _refwfh _temph;
 by _sex _lht_id;
run;

data _refwfh;
 set _refwfh;
 _intl=(_la*_lht_cm-_L*_lht_cma)/(_lht_cm-_lht_cma);
 _slpl=(_l-_la)/(_lht_cm-_lht_cma);

 _intm=(_ma*_lht_cm-_m*_lht_cma)/(_lht_cm-_lht_cma);
 _slpm=(_m-_ma)/(_lht_cm-_lht_cma);

 _ints=(_sa*_lht_cm-_s*_lht_cma)/(_lht_cm-_lht_cma);
 _slps=(_s-_sa)/(_lht_cm-_lht_cma);

 drop _lht_cm _lht_cma _la _ma _sa;
run;

data _refwlhz;
 set _refwfl _refwfh;
 _refwlhz=1;
run;

proc sort data=_refwlhz;
 by _loh _sex _lht_id;
run;

proc sql;
 drop table _refwfl, _refwfh, _templ, _temph;
quit;

data _refbaz;
 length loh $3.;
 set _reflib.bmianthro;
 rename sex=_sex age=_age_day l=_l m=_m s=_s loh=_loh;
run;

proc sort data=_refbaz;
 by _sex _age_day _loh;
run;

data _refhcaz;
 set _reflib.hcanthro;
 rename sex=_sex age=_age_day l=_l m=_m s=_s;
run;
proc sort data=_refhcaz;
 by _sex _age_day;
run;

data _refacaz;
 set _reflib.acanthro;
 rename sex=_sex age=_age_day l=_l m=_m s=_s;
run;
proc sort data=_refacaz;
 by _sex _age_day;
run;

data _reftsaz;
 set _reflib.tsanthro;
 rename sex=_sex age=_age_day l=_l m=_m s=_s;
run;
proc sort data=_reftsaz;
 by _sex _age_day;
run;

data _refssaz;
 set _reflib.ssanthro;
 rename sex=_sex age=_age_day l=_l m=_m s=_s;
run;
proc sort data=_refssaz;
 by _sex _age_day;
run;

/*******************************************************************
To decide if the default analyses are to be performed;
********************************************************************/
data _null_;

/*****************************************************************************************
The macro parameters for the second set of the standards, i.e. head circumference-for-age, 
arm circumference-for-age, triceps skinfold-for-age and subscapular skinfold-for-age, 
are optional.    
******************************************************************************************/

 _headc=symget('headc');
 if _headc=" " then do;
	call symput ('headc', "_XXHC");
	end;

 _armc=symget('armc');
 if _armc=" " then do;
	call symput ('armc', "_XXAC");
	end;

 _triskin=symget('triskin');
 if _triskin=" " then do;
	call symput ('triskin', "_XXTS");
	end;

 _subskin=symget('subskin');
 if _subskin=" " then do;
	call symput ('subskin', "_XXSS");
	end;

 _measure=symget('measure');
 if _measure=" " then do;
	call symput ('measure', "_XXME");
	end;

 _oedema=symget('oedema');
 	if _oedema=" " then do;
	call symput ('oedema', "_XXOE");
	end;

 _sw=symget('sw');
 	if _sw=" " then do;
	call symput ('sw', "_XXSW");
	end;
run;

/*******************************************************************
To set up the input data set;
********************************************************************/

data _data_in;
 length _loh $3.;
 set _datalib.&data_in;

  if compress(upcase(&sex)) in ("F", "2") then _sex=2;
  else if compress(upcase(&sex)) in ("M", "1") then _sex=1;

 %if %upcase(&age_unit)=MONTHS %then %do;
 	_age_day=round(&age*30.4375);
	_age_dyc=&age*30.4375;
   %end; 
   %else %if %upcase(&age_unit)=DAYS %then %do;
 	_age_day=round(&age);
 	_age_dyc=&age;
   %end; 

 _wgt_kg=&weight;

 _lht_cm=&lenhei;

  %if %upcase(&headc)^=_XXHC %then %do;
 	_hc_cm=&headc;
  %end;
  %else %do;
  	_hc_cm=.;
  %end;

  %if %upcase(&armc)^=_XXAC %then %do;
 	_ac_cm=&armc;
  %end;
  %else %do;
  	_ac_cm=.;
  %end;

  %if %upcase(&triskin)^=_XXTS %then %do;
 	_ts_mm=&triskin;
  %end;
  %else %do;
  	_ts_mm=.;
  %end;

  %if %upcase(&subskin)^=_XXSS %then %do;
 	_ss_mm=&subskin;
  %end;
  %else %do;
  	_ss_mm=.;
  %end;

 %if %upcase(&measure)^=_XXME %then %do;
 	_temp=&measure;
  %end;
  %else %do;
  	_temp=" ";
  %end;

 if compress(upcase(_temp))="L" then _loh="L";
   else if compress(upcase(_temp))="H" then _loh="H";
   else _loh=" ";

 if _loh=" " then do;
 	if _age_day=. then do;
    	if _lht_cm<0 then _loh=" ";
		  else if 0<=_lht_cm<87 then _loh="L";
		  else if _lht_cm>=87 then _loh="H";
	  end;

	  else if _age_day<731 then _loh="L";
      else if _age_day>=731 then _loh="H";
   end;	

 if _age_day=. then do;
 	_lht_cm=_lht_cm;
	_loh=_loh;
   end;

   else if 0<=_age_day<731 then do;
   		if _loh="H" then do;
     		_lht_cm=_lht_cm+0.7;
     		_loh="L";
   		  end;
	      else if _loh="L" then do;
    	  	_lht_cm=_lht_cm;
   	      end;
   end;

   else if _age_day>=731 then do;
   		if _loh="L" then do;
    		_lht_cm=_lht_cm-0.7;
     		_loh="H";
   		  end;
          else if _loh="H" then do;
    	  	_lht_cm=_lht_cm;
   		  end;
   end;

 if _lht_cm=. then _loh=" ";

 %if %upcase(&oedema)^=_XXOE %then %do;
 	_oedema=&oedema;
 	%end;
  %else %do;
  	_oedema="N";
 	%end;

 %if %upcase(&sw)^=_XXSW %then %do;
 	_sw=&sw;
 	%end;
  %else %do;
  	_sw=1;
 	%end;

 _id=_n_;
run;

proc sort data=_data_in;
 by _sex _age_day _loh;
run;

/*******************************************************************
To generate the footnotes indicating the number of subjects having 
missing age and/or oedema;
********************************************************************/
%let _agem=NO;
proc freq data=_data_in noprint;
 table _age_day/out=_agem;
 weight _sw;
run;
data _null_;
 set _agem;
 number=compress(trim(put(count, 8.0)));
 if _age_day=. and count>0 then do;
 	call symput('_agem', 'YES');
	call symput('_agemn', number);
   end;
run;

%let _oed=NO;
proc freq data=_data_in noprint;
 table _oedema/out=_oed;
 weight _sw;
 where _age_day<=1856;
run;
data _null_;
 set _oed;

 number=compress(trim(put(count, 8.0)));
 if compress(upcase(_oedema))="Y" and count>0 then do;
 	call symput('_oed', 'YES');
	call symput('_oedn', number);
   end;
run;

%let _ageoed=NO;
proc freq data=_data_in noprint;
 table _age_day*_oedema/out=_ageoed;
 weight _sw;
 where _age_day<=1856;
run;
data _null_;
 set _ageoed;
 number=compress(trim(put(count, 8.0)));
 if _age_day=. and compress(upcase(_oedema))="Y" and count>0 then do;
 	call symput('_ageoed', 'YES');
	call symput('_ageoedn', number);
   end;
run;

%let _XSWM=NO;
proc means data=_data_in noprint;
 var _sw;
 output out=_XSWM n=n;
 where _sw<0 and _sw^=.;
run;
data _null_;
 set _XSWM;
 if n>0 then  do;
 	call symput('_XSWM', 'YES');
	end;
run;

proc sql;
 drop table _oed, _agem, _ageoed,_XSWM;
run;

/*******************************************************************
To generate the footnotes indicating the number of subjects having 
missing age and/or oedema;
********************************************************************/

	%if &_agem=NO %then %do;
		%if &_oed=NO %then %do;
 			footnote3 justify=left
 			"There are no subjects with either missing age or oedema.";
	 	%end;
     	%else %if &_oed=YES %then %do;
     		footnote3 justify=left
 			"There are no subjects with missing age. Number of subjects 
with oedema: &_oedn.";
 	 	%end;
 	%end;

 	%else %if &_agem=YES %then %do;
		%if &_oed=NO %then %do;
			footnote3 justify=left
     		"Number of subjects with missing age: &_agemn.There are 
no subjects with oedema.";
	 	%end;
    	%else %if &_oed=YES %then %do;
	 		footnote3 justify=left
 			"Number of subjects with missing age: &_agemn. Number 
of subjects with oedema: &_oedn.";
			footnote4 justify=left
 			"Number of subjects with missing age and oedema: &_ageoedn.";
 	 	%end;
 	%end;

/*******************************************************************
To derive weight-for-age z-scores;
********************************************************************/
data _waz;
 merge _data_in(in=exist) _refwaz;
 by _sex _age_day;
 if exist;
run;

data _waz;
 set _waz;
 _sd2pos=_m*((1+2*_l*_s)**(1/_l));
 _sd3pos=_m*((1+3*_l*_s)**(1/_l));
 _sd2neg=_m*((1-2*_l*_s)**(1/_l));
 _sd3neg=_m*((1-3*_l*_s)**(1/_l));

 _z=((_wgt_kg/_m)**_l-1)/(_s*_l);

 if _z>3 then do;
	_z=3+(_wgt_kg-_sd3pos)/(_sd3pos-_sd2pos);
   end;

   else if _z<-3 and _z^=. then do;
	_z=-3-(_sd3neg-_wgt_kg)/(_sd2neg-_sd3neg);
   end;
 keep _id _z;
 rename _z=_ZWEI;
run;

proc sort data=_waz;
 by _id;
run;

proc sql;
 drop table _refwaz;
quit;

/*******************************************************************
To derive height-for-age z-scores;
********************************************************************/
data _haz;
 merge _data_in(in=exist) _refhaz;
 by _sex _age_day _loh;
 if exist;
run;

data _haz;
 set _haz;
 _z=((_lht_cm/_m)**_l-1)/(_s*_l);
 keep _id _z; 
 rename _z=_ZLEN;
run;

proc sort data=_haz;
 by _id;
run;

proc sql;
 drop table _refhaz;
quit;

/*******************************************************************
To derive weight-for-length/height z-scores;
********************************************************************/

data _wlhz;
 set _data_in;
 _wlhz=1;
run;

data _wlhz;
 set _wlhz;
 _lht_id=round(_lht_cm*10000);
run;
proc sort data=_wlhz;
 by _loh _sex _lht_id ;
run;

data _wlhz;
 merge _wlhz _refwlhz;
 by _loh _sex _lht_id;
 if _refwlhz=. then _refwlhz=0;
 if _wlhz=. then _wlhz=0;
run;

/*******************************************************************
To do linear interpolation for length/height between the tabulated 
points in order to derive their z-scores;
********************************************************************/

data _wlhz;
 retain _tintl _tintm _tints _tslpl _tslpm _tslps;
 set _wlhz;
 if _refwlhz=1 then do;
 	_tintl=_intl; 
  	_tintm=_intm; 
  	_tints=_ints; 
  	_tslpl=_slpl; 
  	_tslpm=_slpm; 
  	_tslps=_slps;
   end;

   else if _refwlhz=0 then do;
   	_intl=_tintl; 
  	_intm=_tintm; 
  	_ints=_tints; 
  	_slpl=_tslpl; 
  	_slpm=_tslpm;
  	_slps=_tslps;
   end;
run;

data _wlhz;
 set _wlhz;
 if _wlhz=1 and _refwlhz=0 then do;
 	_l=_intl+_slpl*_lht_cm;
   	_m=_intm+_slpm*_lht_cm;
   	_s=_ints+_slps*_lht_cm;
   end;
run;

data _wlhz;
 set _wlhz;
 if _wlhz=1;

 _sd2pos=_m*((1+2*_l*_s)**(1/_l));
 _sd3pos=_m*((1+3*_l*_s)**(1/_l));
 _sd2neg=_m*((1-2*_l*_s)**(1/_l));
 _sd3neg=_m*((1-3*_l*_s)**(1/_l));

 _z=((_wgt_kg/_m)**_l-1)/(_s*_l);

 if _z>3 then do;
  	_z=3+(_wgt_kg-_sd3pos)/(_sd3pos-_sd2pos);
   end;
   else if _z<-3 and _z^=. then do;
    	_z=-3-(_sd3neg-_wgt_kg)/(_sd2neg-_sd3neg);
   end;

   if _age_day>1856 then _z=.;

 keep _id _z; 
 rename _z=_ZWFL;
run;

proc sort data=_wlhz;
 by _id;
run;

proc sql;
 drop table _refwlhz;
quit;
run;

/*******************************************************************
To derive BMI-for-age z-score;
********************************************************************/
data _baz;
 merge _data_in(in=exist) _refbaz;
 by _sex _age_day _loh;
 if exist;
 _CBMI=_wgt_kg/((_lht_cm/100)**2);
run;

data _baz;
 set _baz;

 _sd2pos=_m*((1+2*_l*_s)**(1/_l));
 _sd3pos=_m*((1+3*_l*_s)**(1/_l));
 _sd2neg=_m*((1-2*_l*_s)**(1/_l));
 _sd3neg=_m*((1-3*_l*_s)**(1/_l));

 _z=((_CBMI/_m)**_l-1)/(_s*_l);

 if _z>3 then do;
 	_z=3+(_CBMI-_sd3pos)/(_sd3pos-_sd2pos);
   end;
   else if _z<-3 and _z^=. then do;
   	_z=-3-(_sd3neg-_CBMI)/(_sd2neg-_sd3neg);
   end;
 keep _id _CBMI _z; 
 rename _z=_ZBMI;
run;

proc sort data=_baz;
 by _id;
run;

proc sql;
 drop table _refbaz;
quit;

/*******************************************************************
To derive head circumference (HC)-for-age  z-scores;
********************************************************************/
data _hcaz;
 merge _data_in(in=exist) _refhcaz;
 by _sex _age_day;
 if exist;
run;

data _hcaz;
 set _hcaz;
 _z=((_HC_cm/_m)**_l-1)/(_s*_l);
 keep _id _z; 
 rename _z=_ZHC;
run;

proc sort data=_hcaz;
 by _id;
run;

proc sql;
 drop table _refhcaz;
quit;

/*******************************************************************
To derive arm circumference (AC)-for-age  z-scores;
********************************************************************/
data _acaz;
 merge _data_in(in=exist) _refacaz;
 by _sex _age_day;
 if exist;
run;

data _acaz;
 set _acaz;

 _sd2pos=_m*((1+2*_l*_s)**(1/_l));
 _sd3pos=_m*((1+3*_l*_s)**(1/_l));
 _sd2neg=_m*((1-2*_l*_s)**(1/_l));
 _sd3neg=_m*((1-3*_l*_s)**(1/_l));

 _z=((_AC_cm/_m)**_l-1)/(_s*_l);

  if _z>3 then do;
	_z=3+(_AC_cm-_sd3pos)/(_sd3pos-_sd2pos);
   end;

   else if _z<-3 and _z^=. then do;
	_z=-3-(_sd3neg-_AC_cm)/(_sd2neg-_sd3neg);
   end;

 keep _id _z; 
 rename _z=_ZAC;
run;

proc sort data=_acaz;
 by _id;
run;

proc sql;
 drop table _refacaz;
quit;

/*******************************************************************
To derive triceps skinfold (TS)-for-age  z-scores;
********************************************************************/
data _tsaz;
 merge _data_in(in=exist) _reftsaz;
 by _sex _age_day;
 if exist;
run;

data _tsaz;
 set _tsaz;

 _sd2pos=_m*((1+2*_l*_s)**(1/_l));
 _sd3pos=_m*((1+3*_l*_s)**(1/_l));
 _sd2neg=_m*((1-2*_l*_s)**(1/_l));
 _sd3neg=_m*((1-3*_l*_s)**(1/_l));

 _z=((_TS_mm/_m)**_l-1)/(_s*_l);

  if _z>3 then do;
	_z=3+(_TS_mm-_sd3pos)/(_sd3pos-_sd2pos);
   end;

   else if _z<-3 and _z^=. then do;
	_z=-3-(_sd3neg-_TS_mm)/(_sd2neg-_sd3neg);
   end;

 keep _id _z; 
 rename _z=_ZTS;
run;

proc sort data=_tsaz;
 by _id;
run;

proc sql;
 drop table _reftsaz;
quit;

/*******************************************************************
To derive subscapular skinfold (SS)-for-age  z-scores;
********************************************************************/
data _ssaz;
 merge _data_in(in=exist) _refssaz;
 by _sex _age_day;
 if exist;
run;
data _ssaz;
 set _ssaz;

 _sd2pos=_m*((1+2*_l*_s)**(1/_l));
 _sd3pos=_m*((1+3*_l*_s)**(1/_l));
 _sd2neg=_m*((1-2*_l*_s)**(1/_l));
 _sd3neg=_m*((1-3*_l*_s)**(1/_l));

 _z=((_SS_mm/_m)**_l-1)/(_s*_l);

 if _z>3 then do;
	_z=3+(_SS_mm-_sd3pos)/(_sd3pos-_sd2pos);
   end;

   else if _z<-3 and _z^=. then do;
	_z=-3-(_sd3neg-_SS_mm)/(_sd2neg-_sd3neg);
   end;

 keep _id _z; 
 rename _z=_ZSS;
run;

proc sort data=_ssaz;
 by _id;
run;

proc sql;
 drop table _refssaz;
quit;

/*******************************************************************
To flag the extreme z-scores and export the data with derived z scores. 
Weight-related z-scores for oedema are all set to missing. The flag 
value for the missing z-score is also missing.
********************************************************************/

data _flag;
 merge _waz _haz _wlhz _baz _hcaz _acaz _tsaz _ssaz;
 by _id; 

 _ZWEI=round(_ZWEI, 0.01);
 _ZLEN=round(_ZLEN, 0.01);
 _ZWFL=round(_ZWFL, 0.01);
 _ZBMI=round(_ZBMI, 0.01); 
 _ZHC=round(_ZHC, 0.01); 
 _ZAC=round(_ZAC, 0.01); 
 _ZTS=round(_ZTS, 0.01); 
 _ZSS=round(_ZSS, 0.01); 

run;

data _flag;
 set _flag;

 if _ZWEI=. then _FWEI=.;
  else if (_ZWEI<-6 or _ZWEI>+5) then _FWEI=1;
  else _FWEI=0;

 if _ZLEN=. then _FLEN=.;
  else if (_ZLEN<-6 or _ZLEN>+6) then _FLEN=1;
  else _FLEN=0;

 if _ZWFL=. then _FWFL=.;
  else if (_ZWFL<-5 or _ZWFL>+5) then _FWFL=1;
  else _FWFL=0;

 if _ZBMI=. then _FBMI=.;
  else if (_ZBMI<-5 or _ZBMI>+5) then _FBMI=1;
  else _FBMI=0;

 if _ZHC=. then _FHC=.;
  else if (_ZHC<-5 or _ZHC>+5) then _FHC=1;
  else _FHC=0;

 if _ZAC=. then _FAC=.;
  else if (_ZAC<-5 or _ZAC>+5) then _FAC=1;
  else _FAC=0;

 if _ZTS=. then _FTS=.;
  else if (_ZTS<-5 or _ZTS>+5) then _FTS=1;
  else _FTS=0;

 if _ZSS=. then _FSS=.;
  else if (_ZSS<-5 or _ZSS>+5) then _FSS=1;
  else _FSS=0;

run;

data _flag;
 retain _CBMI _ZWEI _ZLEN _ZWFL _ZBMI _ZHC _ZAC _ZTS _ZSS 
              _FWEI _FLEN _FWFL _FBMI _FHC _FAC _FTS _FSS;
 set _flag;
run;

proc sort data=_data_in;
 by _id;
run;

data _temp;
 merge _data_in _flag;
by _id;

 if compress(upcase(_oedema))="Y" then do;
	_ZWEI=.;
	_ZWFL=.;
	_ZBMI=.;
	_FWEI=.;
	_FWFL=.;
	_FBMI=.;
end;

drop _id;
run;

proc sql;
drop table _waz, _haz, _wlhz, _baz, _hcaz, _acaz, _tsaz, _ssaz, _flag, _data_in;
quit;

data _datalib.&data_in._z_st;
 set _temp;
 label
 _ZWEI="Weight-for-age z-score"
 _ZLEN="Length/height-for-age z-score"
 _ZWFL="Weight-for-length/height z-score"
 _ZBMI="BMI-for-age z-score"
 _ZHC="Head circumference-for-age z score"
 _ZAC="Arm circumference-for-age z score"
 _ZTS="Triceps Skinfold-for-age z score"
 _ZSS="Subscapular skinfold-for-age z score"

 _FWEI="Flag for _ZWEI<-6 or _ZWEI>5"
 _FLEN="Flag for _ZLEN<-6 or _ZLEN>6"
 _FWFL="Flag for _ZWFL<-5 or _ZWFL>5"
 _FBMI="Flag for _ZBMI<-5 or _ZBMI>5"
 _FHC="Flag for _ZHC<-5 or _ZHC>5"
 _FAC="Flag for _ZAC<-5 or _ZAC>5"
 _FTS="Flag for _ZTS<-5 or _ZTS>5"
 _FSS="Flag for _ZSS<-5 or _ZSS>5"

 _AGE_DAY="Calulated age in days for deriving z-score"
 _CBMI="Calulated BMI=weight / squared(_CLENHEI)"
 _lht_cm="Converted length/height (cm) for deriving z-score";
 drop _temp _age_dyc _sex _wgt_kg _sw _loh _oedema 
      _HC_cm _AC_cm _TS_mm _SS_mm;

  %if %upcase(&headc)=_XXHC %then %do;
 	drop _ZHC _FHC;  
	%end;

  %if %upcase(&armc)=_XXAC %then %do;
 	drop _ZAC _FAC;  
	%end;

  %if %upcase(&triskin)=_XXTS %then %do;
 	drop _ZTS _FTS;  
	%end;

  %if %upcase(&subskin)=_XXSS %then %do;
 	drop _ZSS _FSS;  
	%end;

 rename _age_day=_agedays _lht_cm=_CLENHEI;
run;

PROC EXPORT DATA=_datalib.&data_in._z_st 
          OUTFILE= "&data_lib\&data_in._z_st.csv" 
          DBMS=CSV REPLACE;
RUN;

%if &_XSWM=YES %then %do;
	%put "****Error message: the macro has encountered negative value(s) in sampling weight.";
    %put "****No prevalence tables will be produced.";
 %end;
 %else %do;

/*******************************************************************
To calculate the prevalences of under/over nutrition and summary 
statistics (mean and SD) of the z-scores;
********************************************************************/

data _temp;
 set _temp;
 if floor(_age_dyc/30.4375)<=60;
 if _age_dyc<0 then _mon_c=.;
  else if   0<=(_age_dyc/30.4375)<6   then _mon_c=0;
  else if   6<=(_age_dyc/30.4375)<12  then _mon_c=1;
  else if  12<=(_age_dyc/30.4375)<24  then _mon_c=2;
  else if  24<=(_age_dyc/30.4375)<36  then _mon_c=3;
  else if  36<=(_age_dyc/30.4375)<48  then _mon_c=4;
  else if  48<=(_age_dyc/30.4375)<61  then _mon_c=5;
run;

data _temp;
length _IND $8.;
 set _temp;
 _IND="WAZ";
 _outcom=_ZWEI;
 output;
 _IND="HAZ";
 _outcom=_ZLEN;
 output;
 _IND="WHZ";
 _outcom=_ZWFL;
 output;
 _IND="BAZ";
 _outcom=_ZBMI;
 output;
 _IND="ZHC";
 _outcom=_ZHC;
 output;
 _IND="ZAC";
 _outcom=_ZAC;
 output;
 _IND="ZTS";
 _outcom=_ZTS;
 output;
 _IND="ZSS";
 _outcom=_ZSS;
 output;

 run;

data _temp;
 set _temp; 

 if _outcom=. then _prev_p1=.;
   else if _outcom>1 then _prev_p1=1;
   else _prev_p1=0;

 if _outcom=. then _prev_p2=.;
   else if _outcom>2 then _prev_p2=1;
   else _prev_p2=0;

 if _outcom=. then _prev_p3=.;
   else if _outcom>3 then _prev_p3=1;
   else _prev_p3=0;

 if _outcom=. then _prev_n2=.;
   else if _outcom<-2 then _prev_n2=1;
   else _prev_n2=0;

 if _outcom=. then _prev_n3=.;
   else if _outcom<-3 then _prev_n3=1;
   else _prev_n3=0;

 if _outcom=. then _n=0;
   else _n=1;
run;

data _temp;
 set _temp;

 if _ind="WAZ"  then do;
 	if compress(upcase(_oedema))="Y" then do;
 		_FWEI=0;
 		_n=1; 
 		_prev_n2=1;
 		_prev_n3=1;

		_prev_p1=0;
 		_prev_p2=0;
 		_prev_p3=0;
	  end;
   end;

   else if _ind="WHZ"  then do;
 	if compress(upcase(_oedema))="Y" then do;
 		_FWFL=0;
 		_n=1; 
 		_prev_n2=1;
 		_prev_n3=1;

		_prev_p1=0;
 		_prev_p2=0;
 		_prev_p3=0;
	  end;
   end;

   else if _ind="BAZ"  then do;
 	if compress(upcase(_oedema))="Y" then do;
 		_FBMI=0;
 		_n=1; 
 		_prev_n2=1;
 		_prev_n3=1;

		_prev_p1=0;
 		_prev_p2=0;
 		_prev_p3=0;
	  end;
   end;
run;

data _temp;
 set _temp;

 	if _ind="WAZ" then do;
 		if _FWEI=0 then _select=1;
		 else _select=0;
      end;
 	  else if _ind="HAZ" then do;
 		if _FLEN=0 then _select=1;
         else _select=0;
      end;
 	  else if _ind="WHZ" then do;
 		if _FWFL=0 then _select=1;
		else _select=0;	
      end;
 	  else if _ind="BAZ" then do;
 		if _FBMI=0 then _select=1;
		else _select=0;
      end;

 	  else if _ind="ZHC" then do;
 		if _FHC=0 then _select=1;
         else _select=0;
      end;
      else if _ind="ZAC" then do;
 		if _FAC=0 then _select=1;
         else _select=0;
      end;
      else if _ind="ZTS" then do;
 		if _FTS=0 then _select=1;
         else _select=0;
      end;
      else if _ind="ZSS" then do;
 		if _FSS=0 then _select=1;
         else _select=0;
      end;

 if _select=1;
run;

data _temp;
 set _temp;
 output;
 _sex=0;
 output;
run;

data _temp;
 set _temp;
 output;
 _mon_c=-1;
 output;
run;

/*******************************************************************
To decide whether the stratified analysis by group is requested;
********************************************************************/
data _null_;
 _temp=symget('group');
 if _temp^=" " then do;
 	call symput ('_byclass', "YES");
 end;
 else if _temp=" " then do;
 	call symput ('_byclass', "NONE");
 end;
run;

%if %upcase(&_byclass)=YES %then %do;
    proc freq data=_temp noprint;
	 table &group/out=_grplev;
    run; 
    data _null_;
	 set _grplev;
	 by &group;
	 if last.&group;
 	 call symput ('_grpnum', _n_);
	run;
	data _temp;
 	 length &group $40.; 
 	set _temp;
	output;
 	 &group=" All groups combined";
 	output;
	 label &group="Group";
	run;

    proc freq data=_temp noprint;
	 table &group/out=_grplev;
    run; 
	data _grplev;
	 set _grplev;
	 _group=_n_;
	 keep _group &group;
	run;

	data _fmtgrp;
 	 length fmtname start end $8. label $40.;
	  set _grplev;
 	  label=&group;
 	  fmtname="_fmtgrp";
 	  start=put(_group,8.0);
 	  end=start;
 	  keep fmtname start end label &group;
	run;
	proc format cntlin=_fmtgrp;
    run;
 %end;

proc sort data=_temp;
 by &group _ind _sex _mon_c;
run;

proc means data=_temp sum mean std noprint vardef=WDF;
 var _n _prev_n3 _prev_n2 _prev_p1 _prev_p2 _prev_p3 _outcom;
 by &group _ind _sex _mon_c;
 weight _sw;
 output out=_stat 
        sum=_n1 _n2 _n3 _n4 _n5 _n6 _n7 
        mean=_temp _prev_n3 _prev_n2 _prev_p1 _prev_p2 _prev_p3 _mean_z 
        std=_std1 _std2 _std3 _std4 _std5 _std_6 _std_z;  
run;

proc transpose data=_stat out=_tab(rename=(col1=_value));
 by &group _ind _sex _mon_c;
 var _n1 _prev_n3 _prev_n2 _prev_p1 _prev_p2 _prev_p3 _mean_z _std_z;
run;

data _tab;
 set _tab;
 if _name_="_n1"      then _param=0;
 if _name_="_prev_n3" then _param=1;
 if _name_="_prev_n2" then _param=2;
 if _name_="_prev_p1" then _param=3;
 if _name_="_prev_p2" then _param=4;
 if _name_="_prev_p3" then _param=5;
 if _name_="_mean_z"  then _param=6;
 if _name_="_std_z"   then _param=7;

 if _IND="WAZ" then _order=1; 
  else if _IND="HAZ" then _order=2;
  else if _IND="WHZ" then _order=3;
  else if _IND="BAZ" then _order=4;
  else if _IND="ZHC" then _order=5;
  else if _IND="ZAC" then _order=6;
  else if _IND="ZTS" then _order=7;
  else if _IND="ZSS" then _order=8;

 rename _name_=_name;
 format _sex _sex. _mon_c _age. _param _stat. _order _order.;
run;

data _tab;
 set _tab;
 if _ind in ("WAZ", "HAZ") then do;
 	if 3<=_param<=5 then delete;
  end;
 run;

proc sql;
 drop table _stat, _temp;
quit;

proc sort data=_tab;
 by &group _sex _order _mon_c _param;
run;

proc transpose data=_tab out=_ci;
 by &group _sex _order _mon_c;
 var _value; 
 id _name;
run;

data _ci;
 set _ci;

 array prev{5} _prev_n3 _prev_n2 _prev_p1 _prev_p2 _prev_p3;
 array ci_l{5} _ci_l1-_ci_l5;
 array ci_u{5} _ci_u1-_ci_u5;

 do _i=1 to dim(prev);
    ci_l[_i]=round((prev[_i]-1.96*(prev[_i]*(1-prev[_i])/_n1)**0.5-1/(2*_n1))*100, 
                  0.1);
	ci_u[_i]=round((prev[_i]+1.96*(prev[_i]*(1-prev[_i])/_n1)**0.5+1/(2*_n1))*100,
                  0.1);
	if ci_l[_i]<0 then ci_l[_i]=0;
	if ci_u[_i]>100 then ci_u[_i]=1;
 end;

 drop _i;
run;

/*******************************************************************
To set up the structure of the exported table;
********************************************************************/
proc sort data=_ci;
 by  &group _sex _order _mon_c;
run;

data _frame;
 do _sex=0 to 2;
 	do _order=1 to 8;
 		do _mon_c=-1 to 5;	
        	%if %upcase(&_byclass)=YES %then %do;
			do _group=1 to %eval(&_grpnum);
			output;
 			end;
			%end;
			output;
  		 end;
  	 end;
  end;
run;

data _frame;
 set _frame;
 if _order=1 then _ind="WAZ";
 else if _order=2 then _ind="HAZ";
 else if _order=3 then _ind="WHZ";
 else if _order=4 then _ind="BAZ";
 else if _order=5 then _ind="ZHC";
 else if _order=6 then _ind="ZAC";
 else if _order=7 then _ind="ZTS";
 else if _order=8 then _ind="ZSS";

 %if %upcase(&_byclass)=YES %then %do;
 length &group $40.;
 &group=put(_group, _fmtgrp.);
 %end;
run;

proc sort data=_frame;
 by  &group _sex _order _mon_c;
run;

data _ci;
 merge _frame _ci;
 by  &group _sex _order _mon_c;
run; 

/*******************************************************************
To export report tables in Excel;
********************************************************************/

data _ext1;
 length B $11. C $13. D $10.;
 _order=0;
 _sex=0;
 output;
 _order=0;
 _sex=0;
 output;
 _line=1;
 _order=0;
 _sex=0;
 B="Set 1:";
 C="Sexes";
 D="combined";
 output;
run;

data _ext2_1  _ext2_2;
 length B $11. C $13. D E $10.;
 _line=2;
 _sex=0;
 _mon_c=-3;
 _order=1;
 B="Weight";
 C="-for-";
 D="age";
 E=" ";
 output _ext2_1;
 _line=2;
 _sex=0;
 _mon_c=-3;
 _order=2;
 B="Length";
 C="/height";
 D="-for-";
 E="age";
 output _ext2_1;
 _line=2;
 _sex=0;
 _mon_c=-3;
 _order=3;
 B="Weight";
 C="-for-";
 D="length";
 E="/height";
 output _ext2_1;
 _line=2;
 _sex=0;
 _mon_c=-3;
 _order=4;
 B="BMI";
 C="-for-";
 D="age";
 E=" ";
 output _ext2_1;

 _line=2;
 _sex=0;
 _mon_c=-3;
 _order=5;
 B="Head";
 C="Circumference";
 D="-for-";
 E="age";
 output _ext2_2;
 _line=2;
 _sex=0;
 _mon_c=-3;
 _order=6;
 B="MUAC";
 C="-for-";
 D="age";
 E=" ";
 output _ext2_2;
 _line=2;
 _sex=0;
 _mon_c=-3;
 _order=7;
 B="Triceps";
 C="skinfold";
 D="-for-";
 E="age";
 output _ext2_2;
 _line=2;
 _sex=0;
 _mon_c=-3;
 _order=8;
 B="Subscapular";
 C="skinfold";
 D="-for-";
 E="age";
 output _ext2_2;
run;

data _ext3;
 length A D E F G H I J $10. B $11. C $13.;
 _line=3;
 _sex=0;
 _mon_c=-2;
 A="Age";
 B="N";
 C="% <-3 SD";
 D="95%";
 E="C.I";
 F="% <-2 SD";
 G="95%";
 H="C.I";
 I="Mean";
 J="SD";
run;

data _ext3_1 _ext3_2;
 set _ext3;
 _order=1;
 output _ext3_1;
 _order=2;
 output _ext3_2;
run;

data _ext3_3 _ext3_4
     _ext3_5 _ext3_6 _ext3_7 _ext3_8;
 length K L M N O P Q R S $10.;
 set _ext3;
 I="% >+1 SD";
 J="95%";
 K="C.I.";
 L="% >+2 SD";
 M="95%";
 N="C.I.";
 O="% >+3 SD";
 P="95%";
 Q="C.I.";
 R="Mean";
 S="SD";
 _order=3;
 output _ext3_3;
 _order=4;
 output _ext3_4;
 _order=5;
 output _ext3_5;
 _order=6;
 output _ext3_6;
 _order=7;
 output _ext3_7;
 _order=8;
 output _ext3_8;

run; 

data _all_1;
 set _ext1 _ext2_1  
     _ext3_1 _ext3_2 _ext3_3 _ext3_4;
run;

data _male_1;
 set _all_1;
 _sex=1;
 if _line=1 then do;
    B="Set 2:";
 	C="Males";
	D=" ";
  end;
run;

data _female_1;
 set _all_1;
 _sex=2;
 if _line=1 then do;
    B="Set 3:";
 	C="Females";
	D=" ";
  end;
run;

data _all_2;
 set _ext1 _ext2_2  
     _ext3_5 _ext3_6 _ext3_7 _ext3_8;
run;

data _male_2;
 set _all_2;
 _sex=1;
 if _line=1 then do;
    B="Set 2:";
 	C="Males";
	D=" ";
  end;
run;

data _female_2;
 set _all_2;
 _sex=2;
 if _line=1 then do;
    B="Set 3:";
 	C="Females";
	D=" ";
  end;
run;

proc sql;
 drop table _ext1, _ext2_1, _ext2_2, _ext3,  
      _ext3_1, _ext3_2, _ext3_3, _ext3_4, _ext3_5, _ext3_6, _ext3_7,
      _ext3_8;
quit;

 %if %upcase(&_byclass)=YES %then %do;
	proc sql;
 	 drop table _fmtgrp, _grplev;
	quit;
 %end;

data _ci_dat;
 length A $18. D E F G H I J K L M N O P Q R S $10. B $11. C $13.;
 set _ci;
 if _mon_c^=.;

  	A=put(_mon_c, _age.);

	if _n1>0 then B=put(_n1, 8.0);
	else B="0";

	if _prev_n3^=. then  C=put(_prev_n3*100, 8.1);
    else C=" ";

 	if _ci_l1^=. then D=put(_ci_l1, 8.1);
	else D=" ";

 	if _ci_u1^=. then E=put(_ci_u1, 8.1);
	else E=" ";

 	if _prev_n2^=. then F=put(_prev_n2*100, 8.1);
	else F=" ";

 	if _ci_l2^=. then  G=put(_ci_l2, 8.1);
	else G=" ";

 	if _ci_u2^=. then H=put(_ci_u2, 8.1);
	else H=" ";

 	if _order in (1,2) then do;
 		if _mean_z^=. then I=put(_mean_z, 8.2);
		else I=" ";
 		if _std_z^=. then J=put(_std_z, 8.2);
		else J=" ";
  	end;

  	else if _order in (3,4,5,6,7,8) then do;

		if _prev_p1^=. then I=put(_prev_p1*100, 8.1);
		else I=" ";

  		if _ci_l3^=. then J=put(_ci_l3, 8.1);
		else J=" ";

 		if _ci_u3^=. then K=put(_ci_u3, 8.1);
		else K=" ";

 		if _prev_p2^=. then L=put(_prev_p2*100, 8.1);
		else L=" ";

		if _ci_l4^=. then M=put(_ci_l4, 8.1);
		else M=" ";

 		if _ci_u4^=. then N=put(_ci_u4, 8.1);
		else N=" ";

		if _prev_p3^=. then O=put(_prev_p3*100, 8.1);
		else O=" ";

 		if _ci_l5^=. then P=put(_ci_l5, 8.1);
		else P=" ";

 		if _ci_u5^=. then Q=put(_ci_u5, 8.1);
		else Q=" ";

 		if _mean_z^=. then R=put(_mean_z, 8.2);
		else R=" ";

 		if _std_z^=. then S=put(_std_z, 8.2);
		else S=" ";
   	end;
 _line=4;
run;

data _ci_dat1;
 set _ci_dat _all_1 _male_1 _female_1;
 if _order in (0,1,2,3,4);
run;
proc sort data=_ci_dat1;
 by  _sex _order _mon_c;
run;

data _ci_dat2;
 set _ci_dat _all_2 _male_2 _female_2;

 if _order in (1,2,3,4) then delete;
  
 _mhc=0; 
 _mac=0; 
 _mts=0; 
 _mss=0;

  %if %upcase(&headc)=_XXHC %then %do;
   if _order=5 then delete;
   _mhc=1;
  %end;

  %if %upcase(&armc)=_XXAC %then %do;
   if _order=6 then delete;
   _mac=1;
  %end;

  %if %upcase(&triskin)=_XXTS %then %do;
   if _order=7 then delete;
   _mts=1;
  %end;

  %if %upcase(&subskin)=_XXSS %then %do;
   if _order=8 then delete;
   _mss=1;
  %end;

 if _mhc=1 and _mac=1 and _mts=1 and _mss=1 then delete;
run;

proc sort data=_ci_dat2;
 by  _sex _order _mon_c;
run;

data _ci_dat;
 set _ci_dat1 _ci_dat2;
 if _order in (6,7,8) and A="0-5" then A="3-5";
 if _order in (6,7,8) and A="0-60" then A="3-60";

run; 

proc sql;
 drop table _tab, _frame, _all_1, _male_1, _female_1,
                          _all_2 ,_male_2, _female_2;
quit;

/*******************************************************************
To perform stratified analysis in the exported report tables in Excel;
********************************************************************/

%if %upcase(&_byclass)=YES %then %do;
 	data _all;
  	 set _ci_dat; 
   	 if &group^=" All groups combined" and _line=4 then delete;
	run;

	data _group;
  	 set _ci_dat; 
     if _sex=0;
   	 if &group=" All groups combined" and _line=4 then delete;
	 if _mon_c>=0 then delete;
	 
	 if _line=1 then do; 
     	B="Set 4: ";
	 	C="Overall";
     	D="Sexes";
     	E="combined";
     	F="by";
     	G="group";
	  end;
	  else if _line=3 then A="Group";
	  else if _line=4 then A=&group;
	 
	run;

    data _ci_dat;
     set _all _group;
    run; 
	
	proc sql;
	 drop table _all, _group;
	quit; 
%end;

data _ci_dat;
 retain A B C D E F G H I J K L M N O P Q R S;
 set _ci_dat; 
 keep A--S;
run;

	PROC EXPORT DATA= WORK._ci_dat                                                                                                             
            OUTFILE= "&data_lib\&data_in._prev_st.xls"
            DBMS=EXCEL4 REPLACE;                                                                                                        
	RUN;   

proc sql;
 drop table _ci_dat, _ci_dat1, _ci_dat2;
quit; 

/*******************************************************************
To export report tables in Word;
********************************************************************/

data _ci_tab;
 length _n _p1-_p5 $8. _ci_neg3 _ci_neg2 _ci_pos1 _ci_pos2 _ci_pos3 $38.;
 set _ci;
 array prev{5} _prev_n3 _prev_n2 _prev_p1 _prev_p2 _prev_p3;
 array p{5}    _p1-_p5;

 if _n1=. then _n="0";
  else if _n1^=. then do;

 	do _i=1 to dim(prev);
 		p[_i]=put(prev[_i]*100,8.1);
 	end; 

 	_n=put(_n1, 8.0);
 	_ci_neg3=compress(_p1)||" ("||compress(put(_ci_l1, 8.1))||", "
    	    ||compress(put(_ci_u1, 8.1))||")";
 	_ci_neg2=compress(_p2)||" ("||compress(put(_ci_l2, 8.1))||", "
          	||compress(put(_ci_u2, 8.1))||")";
 	_ci_pos1=compress(_p3)||" ("||compress(put(_ci_l3, 8.1))||", "
          	||compress(put(_ci_u3, 8.1))||")";
 	_ci_pos2=compress(_p4)||" ("||compress(put(_ci_l4, 8.1))||", "
        	||compress(put(_ci_u4, 8.1))||")";
 	_ci_pos3=compress(_p5)||" ("||compress(put(_ci_l5, 8.1))||", "
    	    ||compress(put(_ci_u5, 8.1))||")";
 	_mean=put(_mean_z, 8.2);
 	_std=put(_std_z, 8.2);

 end;

 if _order in (6,7,8) and _mon_c=-1 then _mon_c=-1.3;
 if _order in (6,7,8) and _mon_c=0 then  _mon_c=0.3;

 %if %upcase(&headc)=_XXHC %then %do;
 	if _order=5 then delete;
  %end;

  %if %upcase(&armc)=_XXAC %then %do;
 	if _order=6 then delete;
  %end;

  %if %upcase(&triskin)=_XXTS %then %do;
 	if _order=7 then delete;
  %end;

  %if %upcase(&subskin)=_XXSS %then %do;
 	if _order=8 then delete;
  %end;

 drop _name_;
run;

proc sort data=_ci_tab;
 by &group _sex _order _mon_c;
run;

proc transpose data=_ci_tab out=_ci_tab(rename=(col1=_value));
 var _n _ci_neg3  _ci_neg2  _ci_pos1  _ci_pos2  _ci_pos3 _mean _std; 
 by &group _sex _order _mon_c;
run;

data _ci_tab;
 set _ci_tab;

 if _name_="_n"      then do;
 	_param=0;
	_cat=0;
  end;

  else if _name_="_ci_neg3" then do;
  	_param=1;
	_cat=1;
  end;
  else if _name_="_ci_neg2" then do;
  	_param=2;
    _cat=1;
  end;
 
  else if _name_="_ci_pos1" then do;
  	_param=3;
	_cat=1;
  end;

  else if _name_="_ci_pos2" then do;
  	_param=4;
	_cat=1;
  end;

  else if _name_="_ci_pos3" then do;
  	_param=5;
	_cat=1;
  end;

  else if _name_="_mean"  then do;
  	_param=6;
	_cat=2;
  end;

  else if _name_="_std" then do;
  	_param=7;
	_cat=2;
  end;

 if _order in (1, 2) then do;
 	if _name_ in ("_ci_pos1", "_ci_pos2", "_ci_pos3") then delete;
   end; 

 format _param _stat. _cat _cat.;
run;

proc sort data=_ci_tab;
 by &group _sex _order _mon_c _param;
run;

data _ci_tab;
 set _ci_tab;
 _index=_n_;
run;

data _fmt;
 length fmtname start end $8. label $18.;
 set _ci_tab;
 label=_value;
 fmtname="_ci";
 start=put(_index,8.0);
 end=start;
 keep fmtname start end label _value;
run;
proc format cntlin=_fmt;
run;

	filename _XXtemp "&data_lib.\&data_in._prev_st.rtf";
	ods rtf file = _XXtemp;

/*******************************************************************
To perform stratified analysis in the exported report tables in Word;
********************************************************************/

%if %upcase(&_byclass)=YES %then %do;
proc tabulate data=_ci_tab;
 by &group;
 class _mon_c _sex _order _param _cat;
 var _index;
 table _sex=" "*_order=" ", _mon_c="Total*", _cat=" "*_param=" "*(_INDEX=" "*sum=" "*f=_ci.)
 /box="AGE GROUPS (Months)";
 where &group=" All groups combined" and _order in (1,2,3,4);
run;

proc tabulate data=_ci_tab;
 by &group;
 class _mon_c _sex _order _param _cat;
 var _index;
 table _sex=" "*_order=" ", _mon_c="Total*", _cat=" "*_param=" "*(_INDEX=" "*sum=" "*f=_ci.)
 /box="AGE GROUPS (Months)";
 where &group^=" All groups combined" and _order in (1,2,3,4);
run;

proc tabulate data=_ci_tab;
 by &group;
 class _mon_c _sex _order _param _cat;
 var _index;
 table _sex=" "*_order=" ", _mon_c="Total*", _cat=" "*_param=" "*(_INDEX=" "*sum=" "*f=_ci.)
 /box="AGE GROUPS (Months)";
 where &group=" All groups combined" and _order in (5,6,7,8);
run;

proc tabulate data=_ci_tab;
 by &group;
 class _mon_c _sex _order _param _cat;
 var _index;
 table _sex=" "*_order=" ", _mon_c="Total*", _cat=" "*_param=" "*(_INDEX=" "*sum=" "*f=_ci.)
 /box="AGE GROUPS (Months)";
 where &group^=" All groups combined" and _order in (5,6,7,8);
run;
%end;

%else %if %upcase(&_byclass)^=YES %then %do;
proc tabulate data=_ci_tab;
 class _mon_c _sex _order _param _cat;
 var _index;
 table _sex=" "*_order=" ", _mon_c="Total*", _cat=" "*_param=" "*(_INDEX=" "*sum=" "*f=_ci.)
 /box="AGE GROUPS (Months)";
 where _order in (1,2,3,4);
run;

proc tabulate data=_ci_tab;
 class _mon_c _sex _order _param _cat;
 var _index;
 table _sex=" "*_order=" ", _mon_c="Total*", _cat=" "*_param=" "*(_INDEX=" "*sum=" "*f=_ci.)
 /box="AGE GROUPS (Months)";
 where _order in (5,6,7,8);
run;
%end;

ods rtf close;

proc sql;
 drop table _ci, _ci_tab, _fmt;
quit;

%end;

title1;title2;
footnote1;footnote2;footnote3;footnote4;
ods listing;
%mend igrowup_standard;