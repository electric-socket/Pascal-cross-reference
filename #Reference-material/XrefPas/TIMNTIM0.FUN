(********************************************************************)
(* THIS FUNCTION WILL RETURN A STRING IN TIME 0 FORMAT HH:MM:SS.hh  *)
(* INPUT        : Hour, Minute, Second, Hundredths of seconds       *)
(* INPUT/OUTPUT : NONE                                              *)
(* OUTPUT       : HOURS,MINUTES,SECONDS,HUNDREDTHS in a 11 char str *)
(* USES         : NO SIDE EFFECTS                                   *)
(* CALLS        : INT $1A                                           *)
(* ERRORS       : NONE CHECKED                                      *)
(* GLOBAL VARS  : NONE                                              *)
(* SUBJECT      : CONVERT, TIME NUMBERS TO TIME 0 STRING            *)
(* DOMAIN       : ARNO A. KARNER                      LEVEL : 001   *)
(********************************************************************)

FUNCTION  Tim_Num_Time0                  ( Hour      : Hour_Number   ;
                                           Minute    : Minute_Number ;
                                           Second    : Second_Number ;
                                           Hundredth : Hundredths )
                                           : Time0 ;

VAR
   Strng    : Time0 ;
   Strng1   : Time0 ;

BEGIN (* FUNC *)
   STR ( Hour : 2 , Strng ) ;
   Strng := Strng + ':' ;
   STR ( Minute : 2 , Strng1 ) ;
   Strng := Strng + Strng1 + ':' ;
   STR ( Second : 2 , Strng1 ) ;
   Strng := Strng + Strng1 + '.' ;
   STR ( Hundredth : 2 , Strng1 ) ;
   Strng := Strng + Strng1 ;
   IF Strng [  4 ] = ' ' THEN Strng [  4 ] := '0' ;
   IF Strng [  7 ] = ' ' THEN Strng [  7 ] := '0' ;
   IF Strng [ 10 ] = ' ' THEN Strng [ 10 ] := '0' ;
   Tim_Num_Time0 := Strng ;
END ; (* FUNC *)