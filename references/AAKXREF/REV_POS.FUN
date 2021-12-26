(********************************************************************)
(* THIS FUNCTION WILL RETURN THE POSITION OF THE SEARCH STRING IN   *)
(*   THE GIVEN STRING SEARCHING FROM BACK TO FRONT INSTEAD OF FRONT *)
(*   TO BACK                                                        *)
(* INPUT        : Search_Str,Pattern_Str                            *)
(* INPUT/OUTPUT : NONE                                              *)
(* OUTPUT       : INTEGER                                           *)
(* USES         : NO SIDE EFFECTS                                   *)
(* CALLS        : NONE                                              *)
(* ERRORS       : NONE                                              *)
(* GLOBAL VARS  : NONE                                              *)
(* SUBJECT      : STRINGS, CONTAININ                                *)
(* DOMAIN       : ARNO A. KARNER                      LEVEL : 000   *)
(********************************************************************)

FUNCTION Rev_Pos ( Pattern_Str, Search_Str : Work_String ) : INTEGER;

VAR
   No_Match        : BOOLEAN ;
   Slen , Plen , I : INTEGER ;

BEGIN (* FUNC *)
   Slen := LENGTH ( Search_Str ) ;
   Plen := LENGTH ( Pattern_Str ) ;
   IF ( Plen > Slen ) OR ( Plen = 0 )
   THEN BEGIN (* NO WAY HOSEA *)
           Rev_Pos := 0 ;
        END   (* NO WAY HOSEA *)
   ELSE BEGIN (* SAME LENGTH OR LESS *)
           IF Plen = Slen
           THEN BEGIN (* STRINGS SAME LENGTH *)
                  IF Search_Str = Pattern_Str
                  THEN Rev_Pos := 1
                  ELSE Rev_Pos := 0 ;
                END   (* STRINGS SAME LENGTH *)
           ELSE BEGIN (* SEARCH FOR PATTERN *)
                   I := Slen - Plen + 1 ;
                   No_Match := TRUE ;
                   WHILE ( I > 0 ) AND No_Match DO
                   BEGIN (* WHILE 00 *)
                      No_Match:=(Pattern_Str<>COPY(Search_Str,I,Plen));
                      IF No_Match THEN I := I -1 ;
                   END ;
                   IF NOT No_Match
                   THEN Rev_Pos := I (* PATTERN MATCHED AT THE Ith Pos *)
                   ELSE Rev_Pos := 0 ;
                END ; (* SEARCH FOR PATTERN *)
        END ; (* SAME LENGTH OR LESS *)
END ; (* FUNC *)