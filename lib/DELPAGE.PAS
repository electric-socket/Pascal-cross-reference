(********************************************************************)
(* THIS PROCEDURE WILL DELET THE INPUT PAGE RECORD AND UPDATE PTRS  *)
(* INPUT        : NONE                                              *)
(* INPUT/OUTPUT : First,P,Last                                      *)
(* OUTPUT       : NONE                                              *)
(* USES         : NO SIDE EFFECTS                                   *)
(* CALLS        : NONE                                              *)
(* ERRORS       : NONE                                              *)
(* GLOBAL VARS  : NONE                                              *)
(* SUBJECT      : PRINTER DEL PAGE                                  *)
(* DOMAIN       : ARNO A. KARNER                      LEVEL : 000   *)
(********************************************************************)

PROCEDURE Delete_Page ( Var First , P , Last : Page_Ptr ) ;

BEGIN (* PROC *)
   IF P^.Prev = NIL THEN First := P^.Next ELSE P^.Prev^.Next := P^.Next ;
   IF P^.Next = NIL THEN Last  := P^.Prev ELSE P^.Next^.Prev := P^.Prev ;
END ; (* PROC *)