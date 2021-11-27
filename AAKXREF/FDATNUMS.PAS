(********************************************************************)
(* THIS PROCEDURE WILL RETURN THE DATE AND TIME OF LAST WRITE TO    *)
(* THE INPUT FILE OR RETURN OK = FALSE                              *)
(* INPUT        : THE FILE                                          *)
(* INPUT/OUTPUT : NONE                                              *)
(* OUTPUT       : MONTH,DAY,YEAR,HOUR,MINUTE,SEC,OK                 *)
(* USES         : NO SIDE EFFECTS                                   *)
(* CALLS        : MSDOS $57                                         *)
(* ERRORS       : INVALID COMMAND, BAD HANDLE                       *)
(* GLOBAL VARS  : NONE                                              *)
(* SUBJECT      : FILE, DATE TIME, READ                             *)
(* DOMAIN       : ARNO A. KARNER\PUBLIC               LEVEL : 001   *)
(********************************************************************)

PROCEDURE  File_Date_Time_Num         ( VAR The_File : text ;
                                        VAR Month    : Month_Number ;
                                        VAR Day      : Day_Number ;
                                        VAR Year     : Year_Number ;
                                        VAR Hour     : Hour_Number ;
                                        VAR Minute   : Minute_Number ;
                                        VAR Second   : Second_Number ;
                                        VAR OK       : BOOLEAN ) ;

VAR
   Packed_Digits,
   Handle      : INTEGER ;
   Regs : reg ;

BEGIN (* PROC *)
   OK := TRUE ;
   Handle := MEMW [ SEG ( The_File ) : OFS ( The_File ) ] ;
   Regs.al := 0;
   Regs.AH := $57;
   Regs.BX := Handle;
   MSDOS ( Regs ) ;
   IF ( Regs.Flags AND 1 <> 0 )
   THEN OK := FALSE
   ELSE BEGIN (* OK *)
           Packed_Digits := Regs.dx;
           Year          := ( Packed_Digits SHR 9 ) + 1980 ;
           Month         := ( ( Packed_Digits SHR 5 ) and $000F ) ;
           Day           := ( Packed_Digits and $001F ) ;
           Packed_Digits := Regs.CX ;
           Second        := ( Packed_Digits AND $1F ) * 2 ;
           Minute        := ( Packed_Digits SHR 5 ) AND $3F ;
           Hour          := Packed_Digits SHR 11 ;
        END ; (* OK *)
END ; (* PROC *)
