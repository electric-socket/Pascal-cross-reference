(********************************************************************)
(* THIS FUNCTION WILL CHECK THE INPUT DATE NUMBERS TO SEE IF VAILD  *)
(* INPUT        : MONTH : MonthNumber , DAY : DayNumber ,           *)
(*              : YEAR : Year_Number                                *)
(* INPUT/OUTPUT : NONE                                              *)
(* OUTPUT       : BOOLEAN = TRUE THEN OK                            *)
(* USES         : NO SIDE EFFECTS                                   *)
(* CALLS        : NONE                                              *)
(* ERRORS       : NONE                                              *)
(* GLOBAL VARS  : NONE                                              *)
(* SUBJECT      : DATE NUMBERS, CHECK IF VALID                      *)
(* DOMAIN       : ARNO A. KARNER                      LEVEL : 000   *)
(********************************************************************)

FUNCTION ValidDateNumber                   ( MonthNum : Month_Number ;
                                             DayNum   : Day_Number   ;
                                             YearNum  : Year_Number )
                                             : BOOLEAN ;

VAR
   Stat : BOOLEAN ;

BEGIN (* FUNCTION *)
   Stat := TRUE ;
   IF MonthNum > 12
     THEN Stat := FALSE
     ELSE BEGIN (* ELSE 00 *)
             IF DayNum > MaxDaysPerMonth [ MonthNum ]
             THEN BEGIN (* THEN 01 *)
                     IF ( MonthNum <> 2 ) OR
                        ( YearNum MOD 4 > 0 ) OR
                        ( DayNum > 29 )
                        THEN Stat := FALSE ;
                  END ; (* THEN 01 *)
          END ; (* ELSE 00 *)
   ValidDateNumber := Stat ;
END ; (* FUNCTION *)