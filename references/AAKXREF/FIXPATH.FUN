(********************************************************************)
(* THIS FUNCTION WILL FIX PATH NAMES THAT LACK TRAILING SLASHES     *)
(*  IT WILL ALSO REMOVE ..\ AND TRAILING SPACES                     *)
(* INPUT        : Work_String                                       *)
(* INPUT/OUTPUT : NONE                                              *)
(* OUTPUT       : File_Str                                          *)
(* USES         : NO SIDE EFFECTS                                   *)
(* CALLS        : Rev_Pos                                           *)
(* ERRORS       : NONE                                              *)
(* GLOBAL VARS  : NONE                                              *)
(* SUBJECT      : PATH NAMES                                        *)
(* DOMAIN       : ARNO A. KARNER                      LEVEL : 001   *)
(********************************************************************)

FUNCTION Fix_Path_Str          ( Path_Str : Work_String ) : File_Str ;

VAR
   I,J : INTEGER ;
   Tem : Work_String ;

BEGIN (* FUNC *)
   (* DELETE ALL TRAILING SPACES *)
   WHILE Path_Str [ LENGTH ( Path_Str ) ] = ' ' DO
      Path_Str := COPY ( Path_Str , 1 , LENGTH ( Path_Str ) - 1 ) ;
   (* ADD SUB DIRECTORY SYMBOL IF NOT THERE *)
   IF (Rev_Pos ( '\', Path_Str ) > Rev_Pos ( '.', Path_Str ))
      AND ( Path_Str [ LENGTH ( Path_Str ) ] <> '\' )
   THEN Tem := Path_Str + '\'
   ELSE Tem := Path_Str ;
   I := POS ( '\..\', Tem ) ;
   IF I <> 0
   THEN J := Rev_Pos ( '\', Copy ( Tem, 1, I -1 ))
   ELSE J := 0 ;
   WHILE (I > 2) AND (J > 0) DO  (* loop while deletable directories*)
   BEGIN (* WHILE *)
      Tem := COPY ( Tem, 1 , J ) + COPY ( Tem, I + 4, 255 ) ;
      I := POS ( '\..\' , Tem ) ;
      IF I <> 0
      THEN J := Rev_Pos ( '\', Copy ( Tem, 1, I -1 ))
      ELSE J := 0 ;
   END ; (* WHILE *)
   Fix_Path_Str := COPY ( Tem , 1 , 64 ) ;
END ; (* FUNC *)
