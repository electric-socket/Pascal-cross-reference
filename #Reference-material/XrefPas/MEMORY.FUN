(********************************************************************)
(* THIS FUNCTION WILL RETURN THE VALUE OF FREE MEMORY IN K BYTES    *)
(* INPUT        : NONE                                              *)
(* INPUT/OUTPUT : NONE                                              *)
(* OUTPUT       : INTEGER : FREE MEMORY IN K BYTES                  *)
(* USES         : NO SIDE EFFECTS                                   *)
(* CALLS        : MAXAVIL                                           *)
(* ERRORS       : NONE                                              *)
(* GLOBAL VARS  : NONE                                              *)
(* SUBJECT      : MEMORY, FREE K BYTES                              *)
(* DOMAIN       : ARNO A. KARNER\PUBLIC               LEVEL : 000   *)
(********************************************************************)

FUNCTION Memory : INTEGER ;

VAR
   Memspace : real;

BEGIN (* FUNC *)
   memspace := maxavail;
   IF memspace < 0 then memspace := 65536.0 + memspace;
   memory := TRUNC ( ( memspace * 16.0 ) / 1024.0 ) ;
END ; (* FUNC *)
