(********************************************************************)
(* THIS PROCEDURE WILL CREATE AND INITIALIZE LINE RECORD            *)
(* INPUT        : NONE                                              *)
(* INPUT/OUTPUT : Line                                              *)
(* OUTPUT       : NONE                                              *)
(* USES         : NO SIDE EFFECTS                                   *)
(* CALLS        : Memory                                            *)
(* ERRORS       : OUT OF MEMORY, PROGRAM HALTED                     *)
(* GLOBAL VARS  : NONE                                              *)
(* SUBJECT      : PRINTER , NEW LINE RECORD                         *)
(* DOMAIN       : ARNO A. KARNER                      LEVEL : 001   *)
(********************************************************************)

PROCEDURE New_Line_Ptr                        ( VAR Lin : Line_Ptr ) ;

BEGIN (* PROC *)
   IF Memory > Min_Memory
   THEN BEGIN (* ENOUGH MEMORY *)
           NEW ( Lin ) ;
           WITH Lin^ DO
           BEGIN (* INIT PAGE *)
              Next  := NIL ;
              CPL := 0 ;
              Line  := '' ;
           END ; (* FIRST PAGE *)
        END   (* ENOUGH MEMORY *)
   ELSE BEGIN (* OUT OF MEMORY *)
           WRITELN ( 'Error out of memory. Program aborted.') ;
           HALT ;
        END ; (* OUT OF MEMORY *)
END ; (* PROC *)