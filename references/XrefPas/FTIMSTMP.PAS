(********************************************************************)
(* THIS FUNCTION WILL RETURN THE FILES DATE0 TIME0 TIME STAMP       *)
(* INPUT        : THE FILE VARIABLE                                 *)
(* INPUT/OUTPUT : NONE                                              *)
(* OUTPUT       : DATE0 TIME0 OF THE INPUT FILE IF OK               *)
(* USES         : NO SIDE EFFECTS                                   *)
(* CALLS        : File_Date_Time_Num ,Date_Nums_To_Date0,           *)
(*              : Tim_Num_Time0                                     *)
(* ERRORS       : FILE PROBLEMS                                     *)
(* GLOBAL VARS  : NONE                                              *)
(* SUBJECT      : FILES , DATE TIME , TIME STAMP                    *)
(* DOMAIN       : ARNO A. KARNER                      LEVEL : 002   *)
(********************************************************************)

FUNCTION File_Time_Stamp                       ( VAR The_File : TEXT )
                                               : String_40 ;

VAR
   M  : Month_Number ;
   D  : Day_Number ;
   D0 : Date0 ;
   Y  : Year_Number ;
   H  : Hour_Number ;
   Mi : Minute_Number ;
   S  : Second_Number ;
   Hu : Hundredths ;
   T  : String_40 ;
   OK : BOOLEAN ;

BEGIN (* FUNC *)
   Hu := 0 ;
   File_Date_Time_Num ( The_File , M , D , Y , H , Mi , S , OK ) ;
   Date_Nums_To_Date0 ( M , D , Y , D0 , OK ) ;
   T := D0 + '  ' + Tim_Num_Time0 ( H , Mi , S , Hu ) ;
   File_Time_Stamp := T ;
END ; (* FUNC *)

