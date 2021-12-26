(********************************************************************)
(* THIS PROCEDURE WILL CREATE A NEW PAGE FOR THE OUTPUT STREAM      *)
(* INPUT        : fv, File_Name , Title                             *)
(* INPUT/OUTPUT : NONE                                              *)
(* OUTPUT       : NONE                                              *)
(* USES         : MODIFIES First_Page, Cur_Page, Last_Page,         *)
(*              : Page_Numb                                         *)
(* CALLS        : Cur_Date0,Cur_Time0,New_Page_Ptr,New_Line_Ptr     *)
(*              : Print_Page                                        *)
(* ERRORS       : NONE                                              *)
(* GLOBAL VARS  : NONE                                              *)
(* SUBJECT      : PRINTER , NEW PAGE DONE WITH PREVIOUS             *)
(* DOMAIN       : ARNO A. KARNER                      LEVEL : 003   *)
(********************************************************************)

PROCEDURE New_Page                     ( VAR fv        : TEXT ;
                                             File_Name : File_Str ;
                                             Title     : TitleType ) ;

VAR
   Squeezed    : BOOLEAN ;
   L           : Line_Ptr ;
   P           : Page_Ptr ;
   date        : Date0 ;
   Date_Stuff1, Date_Stuff, Tem : Work_String ;

BEGIN (* PROC *)
    (* CHECK IF PRINTER AND NOT FIRST PAGE *)
   Date_Stuff  := Title + ': ' + File_Name ;
   Date_Stuff1 := 'Listed ' + Cur_date0 + '  ' + Cur_Time0 +
                   '    Created ' + File_Time_Stamp ( fv ) ;
   New_Page_Ptr ( First_Page , Cur_Page , Last_Page ) ;
   WITH Cur_Page^ DO
   BEGIN (* WITH *)
      Out_Device := Output_Device ;
      New_Line_Ptr ( L ) ;
      First_Line := L ;
      Last_Line  := L ;
      New_Line_Ptr ( L ) ;
      Last_Line^.Next := L ;
      Last_Line  := L ;
      L^.Line := ( Date_Stuff ) ;
      New_Line_Ptr ( L ) ;
      Last_Line^.Next := L ;
      Last_Line  := L ;
      L^.Line := ( Date_Stuff1 ) ;
      New_Line_Ptr ( L ) ;
      Last_Line^.Next := L ;
      Last_Line  := L ;
      Max_CPL := 78 ;
      Cur_LPP := 4 ;
   END ; (* WITH *)
   IF ( First_Page <> Last_Page )
   THEN Print_Page ( First_Page , Last_Page , Page_Numb ) ;
END ; (* PROC *)

