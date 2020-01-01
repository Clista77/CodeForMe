proc import datafile = 'C:/Matching/A.xlsx'
out = A
dbms = xlsx
replace;
run;

proc import datafile = 'C:/Matching/B.xlsx'
out = B
dbms = xlsx
replace;
run;

data A;
set A;
keep SUBJECT DHLA_R AA_R EPA_R DHA_R;
run;

data B;
set B;
keep SUBJECT DHLA_R AA_R EPA_R DHA_R;
run;

proc sql;
  create table AB1 as
  select A.SUBJECT, A.DHLA_R, A.AA_R, A.EPA_R, A.DHA_R
  from A inner join B on A.SUBJECT=B.SUBJECT & A.DHLA_R=B.DHLA_R & A.AA_R=B.AA_R & A.EPA_R=B.EPA_R & A.DHA_R=B.DHA_R
  ;
quit;

proc sql;
  create table AB2 as
  select * from A
    except
  select * from AB1
  ;
quit;

proc sql;
  create table AB3 as
  select * from B
    except
  select * from AB1
  ;
quit;

filename export "C:/Matching/Matched.xlsx";
proc export
  data=AB1
  dbms=xlsx
  outfile=export
  replace;
run;

filename export "C:/Matching/NotMatched1.xlsx";
proc export
  data=AB2
  dbms=xlsx
  outfile=export
  replace;
run;

filename export "C:/Matching/NotMatched2.xlsx";
proc export
  data=AB3
  dbms=xlsx
  outfile=export
  replace;
run;
