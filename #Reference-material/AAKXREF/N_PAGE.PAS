(********************************************************************)
(* THIS PROCEDURE WILL CREATE AND INITALIZE A PAGE RECORD           *)
(* INPUT        : NONE                                              *)
(* INPUT/OUTPUT : First,Cur,Last                                    *)
(* OUTPUT       : NONE                                              *)
(* USES         : NO SIDE EFFECTS                                   *)
(* CALLS        : Memory                                            *)
(* ERRORS       : OUT OF MEMORY PROGRAM HALTS                       *)
(* GLOBAL VARS  : NONE                                              *)
(* SUBJECT      : PRINTER, NEW PAGE RECORD                          *)
(* DOMAIN       : ARNO A. KARNER                      LEVEL : 001   *)
(********************************************************************)

PROCEDURE New_Page_Ptr         ( VAR First , Cur , Last : Page_Ptr ) ;

BEGIN (* PROC *)
   IF Memory > Min_Memory
   THEN BEGIN (* ENOUGH MEMORY *)
           NEW ( Cur ) ;
           WITH Cur^ DO
           BEGIN (* INIT PAGE *)
              IF First = NIL
              THEN BEGIN (* FIRST PAGE *)
                      First := Cur ;
                      Last  := Cur ;
                      Prev  := NIL ;
                      Next  := NIL ;
                   END  (* FIRST PAGE *)
              ELSE BEGIN (* NEW LAST PAGE *)
                      Prev       := Last ;
                      Next       := NIL ;
                      Last^.Next := Cur ;
                      Last       := Cur ;
                   END ; (* NEW LAST PAGE *)
              First_Line := NIL ;
              Last_Line := NIL ;
              Cur_LPP := 0 ;
              Max_CPL := 0 ;
           END ; (* INIT PAGE *)
        END   (* ENOUGH MEMORY *)
   ELSE BEGIN (* OUT OF MEMORY *)
           WRITELN ( 'Error out of memory. Program aborted.') ;
           HALT ;
        END ; (* OUT OF MEMORY *)
END ; (* PROC *)