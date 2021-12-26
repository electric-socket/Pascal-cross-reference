(********************************************************************)
(* THIS PROCEDURE WILL TAKE AN INPUT FILE PATH AND RETURN ITS       *)
(* COMPONETES Path, Name, Extension                                 *)
(* INPUT        : F_S ; PATH\FILE NAME . EXT                        *)
(* INPUT/OUTPUT : NONE                                              *)
(* OUTPUT       : File_Path,File_Name,File_Ext                      *)
(* USES         : NO SIDE EFFECTS                                   *)
(* CALLS        : NONE                                              *)
(* ERRORS       : NONE                                              *)
(* GLOBAL VARS  : NONE                                              *)
(* SUBJECT      : FILE NAMES, PARSS FILE SPEC                       *)
(* DOMAIN       : ARNO A. KARNER                      LEVEL : 000   *)
(********************************************************************)

PROCEDURE File_Separator               ( VAR F_S       : File_Str ;
                                         VAR File_Path : File_Str ;
                                         VAR File_Name : String_08 ;
                                         VAR File_Ext  : String_03 ) ;

VAR
   Tem       : File_Str ;
   I,J, Len  : INTEGER ;

BEGIN (* PROC *)
   File_Ext  := '' ;
   File_Name := '' ;
   File_Path := '' ;
   Len := Length ( F_S ) ;
   I   := Len ;
   WHILE (( I > 0 ) AND ( NOT ( F_S [ I ] = '\' ) )) DO I := I - 1 ;
   IF I <> 0
   THEN BEGIN (* FOUND PATH *)
           File_Path := COPY ( F_S , 1 , I ) ;
           Tem := COPY ( F_S , I + 1 , Len - I ) ;
        END  (* FOUND PATH *)
   ELSE Tem := F_S ;
   J := POS ( '.' , Tem ) ;
   Len := LENGTH ( Tem ) ;
   IF J <> 0
   THEN BEGIN (* FOUND EXT *)
           IF J <> Len
           THEN BEGIN (* NOT ZERO LENGTH EXT *)
                   File_Ext := COPY ( Tem , J + 1 , Len - J ) ;
                   File_Name := COPY ( Tem , 1 , J - 1 ) ;
                END
           ELSE File_Name := COPY ( Tem , 1 , Len - 1 ) ;
        END   (* FOUND EXT *)
   ELSE BEGIN (* GET FILE NAME *)
           File_Name := Tem ;
        END ; (* GET FILE NAME *)
END ; (* PROC *)