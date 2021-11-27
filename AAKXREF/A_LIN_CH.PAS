(********************************************************************)
(* THIS PROCEDURE WILL ADD A CHAR TO THE LINE RECORD ON THE END OF  *)
(* INPUT LINE POINTER. WILL ALSO UPDATE CPL BY 1                    *)
(* INPUT        : Line_Ptr,ch                                       *)
(* INPUT/OUTPUT : NONE                                              *)
(* OUTPUT       : UPDATED LINE RECORD                               *)
(* USES         : NO SIDE EFFECTS                                   *)
(* CALLS        : NONE                                              *)
(* ERRORS       : NONE CHECKED                                      *)
(* GLOBAL VARS  : NONE                                              *)
(* SUBJECT      : PRINTER, LINE OF TEXT                             *)
(* DOMAIN       : ARNO A. KARNER                      LEVEL : 000   *)
(********************************************************************)

PROCEDURE Add_Line_Ch ( Lin : Line_Ptr ;
                        Ch  : CHAR ) ;

BEGIN (* PROC *)
   WITH Lin^ DO
   BEGIN
      Line := Line + Ch ;
      CPL  := CPL + 1 ;
   END ;
END ; (* PROC *)