(********************************************************************)
(* THIS PROCEDURE WILL READ THE TIME IN THE REAL TIME CLOCK         *)
(* INPUT        : NONE                                              *)
(* INPUT/OUTPUT : NONE                                              *)
(* OUTPUT       : HOURS,MINUTES,SECONDS,HUNDREDTHS                  *)
(* USES         : NO SIDE EFFECTS                                   *)
(* CALLS        : INT $1A,BCD TO INTEGER                            *)
(* ERRORS       : NONE                                              *)
(* GLOBAL VARS  : NONE                                              *)
(* SUBJECT      : TIME, READ REAL TIME CLOCK                        *)
(* DOMAIN       : ARNO A. KARNER                      LEVEL : 001   *)
(********************************************************************)

PROCEDURE Read_Time                  ( VAR Hour      : Hour_Number   ;
                                       VAR Minute    : Minute_Number ;
                                       VAR Second    : Second_Number ;
                                       VAR Hundredth : Hundredths ) ;

VAR
   SecondBcd    : INTEGER ;
   HourBcd      : INTEGER ;
   MinuteBcd    : INTEGER ;
   HundredthBcd : INTEGER ;
   Regs         : Reg ;

BEGIN
   Regs.AH := 2 ;
   INTR ($1A,Regs) ;
   WITH Regs DO
   BEGIN (* WITH 01 *)
      HourBcd      := CH ;
      MinuteBcd    := CL ;
      SecondBcd    := DH ;
      HundredthBcd := DL ;
   END ;  (* WITH 01 REGS *)
   Hour      := BcdToInteger ( HourBcd ) ;
   Minute    := BcdToInteger ( MinuteBcd ) ;
   Second    := BcdTointeger ( SecondBcd ) ;
   Hundredth := BcdTointeger ( HundredthBcd ) ;
END ; (* END PROC *)
