(********************************************************************)
(* THIS PROCEDURE WILL INSERT THE INPUT LINE INTO THE INPUT PAGE    *)
(* INPUT        : fv,File_Name,Title,Line_Len                       *)
(* INPUT/OUTPUT : Line,Line_Num                                     *)
(* OUTPUT       : NEXT LINE RECORD READY FOR DATA PREV ONE STORED   *)
(* USES         : NO SIDE EFFECTS                                   *)
(* CALLS        : New_Page                                          *)
(* ERRORS       : NONE                                              *)
(* GLOBAL VARS  : NONE                                              *)
(* SUBJECT      : PRINTER, NEW LINE                                 *)
(* DOMAIN       : ARNO A. KARNER                      LEVEL : 004   *)
(********************************************************************)

PROCEDURE New_Line                        ( VAR fv        : TEXT ;
                                                File_Name : File_Str ;
                                                Title     : TitleType ;
                                            VAR Line      : Line_Ptr ;
                                            VAR Line_Num  : INTEGER ) ;

VAR
   Tem : work_String ;

BEGIN (* PROC START NEW LINE OF TEXT *)
   IF Cur_Page^.Cur_LPP >= Lines_Per_Page
     THEN New_Page ( fv , File_Name , Title ) ;
   WITH Cur_Page^ DO
   BEGIN (* INSIDE PAGE REC *)
      Cur_LPP         := Cur_LPP + 1 ;
      Last_Line^.NEXT := Line ;
      Last_Line       := Line ;
      IF Line^.CPL > Max_CPL THEN Max_CPL := Line^.CPL ;
   END ; (* INSIDE PAGE REC *)
   New_Line_Ptr ( Line ) ;
   Line_Num := Line_Num + 1 ;
   STR ( Line_Num : Linenum_Size , Tem ) ;
   Tem := Tem + ' ' ;
   Line^.Line := Tem ;
   Line^.CPL  := Linenum_Size + 1 ;
END ; (* PROC START NEW LINE OF TEXT *)
