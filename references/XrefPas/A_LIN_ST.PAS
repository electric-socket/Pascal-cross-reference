(********************************************************************)
(* THIS PROCEDURE WILL ADD A STRING OF CHARS WITH PRINTABLE LEN     *)
(* INPUT        : Line_Ptr,St,Len                                   *)
(* INPUT/OUTPUT : NONE                                              *)
(* OUTPUT       : UPDATED LINE RECORD                               *)
(* USES         : NO SIDE EFFECTS                                   *)
(* CALLS        : NONE                                              *)
(* ERRORS       : ERROR JUST REPORTED                               *)
(* GLOBAL VARS  : NONE                                              *)
(* SUBJECT      : PRINTER, LINE OF TEXT                             *)
(* DOMAIN       : ARNO A. KARNER                      LEVEL : 000   *)
(********************************************************************)

PROCEDURE Add_Line_Str                           ( Lin : Line_Ptr ;
                                                   St  : Work_String ;
                                                   Len : INTEGER ) ;

BEGIN (* PROC *)
   WITH Lin^ DO
   BEGIN (* WITH *)
      IF LENGTH ( ST ) + LENGTH ( Line ) < 255
      THEN BEGIN (* SIZE OK *)
              Line := Line + St ;
              CPL := CPL + Len ;
           END
      ELSE BEGIN
              WRITELN ('Error in add line string > 255 ') ;
              WRITELN ( Line ) ;
              WRITELN ( St ) ;
              WRITELN ( Len ) ;
           END ;
   END ; (* WITH *)
END ; (* PROC *)