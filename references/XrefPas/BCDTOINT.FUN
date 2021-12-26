(********************************************************************)
(* THIS FUNCTION WILL A FOUR DIGIT BCD INTEGER AND CONVERT IT TO A  *)
(* NORMAL INTEGER                                                   *)
(* INPUT        : BCD INTEGER                                       *)
(* INPUT/OUTPUT : NONE                                              *)
(* OUTPUT       : NORMAL INTEGER                                    *)
(* USES         : NO SIDE EFFECTS                                   *)
(* CALLS        : NONE                                              *)
(* ERRORS       : NONE                                              *)
(* GLOBAL VARS  : NONE                                              *)
(* SUBJECT      : BCD, CONVERT TO INTEGER                           *)
(* DOMAIN       : ARNO A. KARNER                      LEVEL : 000   *)
(********************************************************************)

FUNCTION  BcdToInteger              ( Bcd : INTEGER ) : INTEGER ;

BEGIN (* FUNCTION *)
   BcdToInteger := ( ( Bcd SHR 12 ) * 1000 +
                   ( ( Bcd AND $F00 ) SHR 8 ) * 100 +
                   ( ( BCD AND $F0 ) SHR 4 ) * 10 +
                   ( Bcd AND $F ) ) ;
END ; (* FUNCTION *)
