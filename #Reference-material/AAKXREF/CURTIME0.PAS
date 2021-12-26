(********************************************************************)
(* THIS FUNCTION WILL RETURN THE CURRENT TIME IN TIME 0 FORMAT      *)
(* HH:MM:SS.HH                                                      *)
(* INPUT        : NONE USES REAL TIME CLOCK                         *)
(* INPUT/OUTPUT : NONE                                              *)
(* OUTPUT       : Cur_Time0                                         *)
(* USES         : NO SIDE EFFECTS                                   *)
(* CALLS        : Read_Time, Tim_Num_Time0                          *)
(* ERRORS       : NONE                                              *)
(* GLOBAL VARS  : NONE                                              *)
(* SUBJECT      : TIME, CURRENT TIME, TIME 0 FORMAT                 *)
(* DOMAIN       : ARNO A. KARNER\PUBLIC               LEVEL : 002   *)
(********************************************************************)

FUNCTION Cur_Time0 : Time0 ;

VAR
   H  : Hour_Number ;
   M  : Minute_Number ;
   S  : Second_Number ;
   Hu : Hundredths ;

BEGIN (* FUNC *)
   Read_Time ( H , M , S , Hu ) ;
   Cur_Time0 := Tim_Num_Time0 ( H , M , S , Hu ) ;
END ; (* FUNC *)
