(********************************************************************)
(*  THIS PROCEDURE RETURNS A STRING 11 CHARS LONG IN THEN FOLLOWING *)
(*     FORMAT MMM DD YYYY                                           *)
(* INPUT        : Month_Number, Day_Number, YearNumber              *)
(* INPUT/OUTPUT : NONE                                              *)
(* OUTPUT       : STAT = NO ERRS AND VALID OUTPUTS IF TRUE          *)
(*              : OUTDATE0 = STRING 11 CHARS LONG 'MMM DD YYYY'     *)
(* USES         : NO SIDE EFFECTS                                   *)
(* CALLS        : Valid_Date_Number                                 *)
(* ERRORS       : INVALID DATE NUMBERS                              *)
(* GLOBAL VARS  : NONE                                              *)
(* SUBJECT      : CONVERT, DATE NUMBERS TO DATE 0 STRING            *)
(* DOMAIN       : ARNO A. KARNER                      LEVEL : 001   *)
(********************************************************************)

PROCEDURE Date_Nums_To_Date0           (     MonthNum : Month_Number ;
                                             DayNum   : Day_Number   ;
                                             YearNum  : Year_Number  ;
                                         VAR OutDate0 : Date0       ;
                                         VAR Stat     : BOOLEAN   ) ;

(* GLOBAL VARS *)
(* MonthNames [1..12] OF PARAGRAPH *)

VAR
   TempDate0 : Date0 ;
   TempDate1 : Date0 ;

BEGIN (* PROC *)
   Stat := ValidDateNumber ( MonthNum , DayNum , YearNum ) ;
   IF NOT Stat THEN
           BEGIN (* THEN *)
              WRITELN ('ERR INVALID DATE NUMBERS ') ;
           END   (* THEN *)
      ELSE BEGIN (* ELSE *)
              TempDate0 := '' ;
              TempDate0 := COPY ( MonthNames [ MonthNum ] , 1 , 3 )
                           + ' ' ;
              STR ( DayNum , TempDate1 ) ;
              IF LENGTH (TempDate1)=1 THEN
                   TempDate1 := '0' + TempDate1 ;
              TempDate0 := TempDate0 + TempDate1 + ' ' ;
              STR ( YearNum :4 , TempDate1 ) ;
              TempDate0 := TempDate0 + TempDate1 ;
              OutDate0 := TempDate0 ;
      END ; (* ELSE *)
   IF NOT Stat THEN WRITELN ( 'ERROR IN PROC DATNDAT0.PRC' ) ;
END ; (* PROC *)
