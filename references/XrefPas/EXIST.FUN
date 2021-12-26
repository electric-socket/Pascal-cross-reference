(********************************************************************)
(* THIS FUNCTION WILL DETERMIN IF THE GIVEN FILE EXISTS             *)
(* INPUT        : FILE-SPEC + FILE-NAME + FILE-EXTENSION            *)
(* INPUT/OUTPUT : NONE                                              *)
(* OUTPUT       : BOOLEAN FILE EXISTS IF TRUE                       *)
(* USES         : NO SIDE EFFECTS                                   *)
(* CALLS        : ASSIGN , IOresult                                 *)
(* ERRORS       : NONE                                              *)
(* GLOBAL VARS  : NONE                                              *)
(* SUBJECT      : FILES, DOES IT EXIST YES OR NO                    *)
(* DOMAIN       : ARNO A. KARNER\PUBLIC               LEVEL : 000   *)
(********************************************************************)

FUNCTION Exist                                ( FileSpec : String_80 )
                                              : BOOLEAN ;
   VAR
      Fil : TEXT ;
   BEGIN
      IF IOresult <> 0 THEN WRITELN (' IO RESULT HOSED ALREADY ' ) ;
      ASSIGN ( Fil , FileSpec ) ;
{$I-}
      RESET ( Fil )
{$I+} ;
      IF ( IOresult = 0 ) THEN Exist := TRUE ELSE Exist := FALSE ;
      CLOSE (Fil) ;
   END ;
