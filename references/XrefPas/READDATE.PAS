(********************************************************************)
(* THIS PROCEDURE WILL PROCEDURE WILL READ THE DATE FROM THE REAL   *)
(*     TIME CLOCK                                                   *)
(* INPUT        : NONE                                              *)
(* INPUT/OUTPUT : NONE                                              *)
(* OUTPUT       : CURRENT MONTH NUMBER                              *)
(*              : CURRENT DAY NUMBER                                *)
(*              : CURRENT YEAR NUMBER                               *)
(* USES         : NO SIDE EFFECTS                                   *)
(* CALLS        : INTERUPT $1A , BCD_TO_INTEGER                     *)
(* ERRORS       : NONE                                              *)
(* GLOBAL VARS  : REG.TYPE                                          *)
(* SUBJECT      : DATE, READ REAL TIME CLOCK                        *)
(* DOMAIN       : ARNO A. KARNER\PUBLIC               LEVEL : 001   *)
(********************************************************************)

PROCEDURE Read_Date                      ( VAR Month : Month_Number  ;
                                           VAR Day   : Day_Number    ;
                                           VAR Year  : Year_Number ) ;


VAR
   CenturyBcd,
   MonthBcd,
   DayBcd     : INTEGER ;
   Regs       : Reg ;

BEGIN
   Regs.AH := 4 ;
   INTR ($1A,Regs) ;
   WITH Regs DO
       BEGIN
          MonthBcd   := DH ;
          DayBcd     := DL ;
          CenturyBcd := CX ;
       END ;  (* WITH REGS *)
   Month    := BcdToInteger ( MonthBcd ) ;
   Day      := BcdToInteger (DayBcd) ;
   Year     := BcdTointeger (CenturyBcd)
END ; (* END PROC *)
