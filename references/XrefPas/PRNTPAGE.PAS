(********************************************************************)
(* THIS PROCEDURE WILL PROCESS THE PAGES AND SEND ON TO OUTPUT IF   *)
(* THAT PAGE IS DONE BEING PROCESSED                                *)
(* INPUT        : NONE                                              *)
(* INPUT/OUTPUT : First,Last,No                                     *)
(* OUTPUT       : PAGE IF DONE PROCESSING                           *)
(* USES         : NO SIDE EFFECTS                                   *)
(* CALLS        : Print__Page,New_Line_Ptr,Delete_Page              *)
(* ERRORS       : NONE                                              *)
(* GLOBAL VARS  : NONE                                              *)
(* SUBJECT      : PRINTER, PROCESS PAGE                             *)
(* DOMAIN       : ARNO A. KARNER                      LEVEL : 002   *)
(********************************************************************)

PROCEDURE Print_Page                  ( VAR First , Last : Page_Ptr ;
                                        VAR No           : INTEGER ) ;

VAR
   Squeezed : BOOLEAN ;
   L        : Line_Ptr ;
   P        : Page_Ptr ;

BEGIN (* AT LEAST ONE OR MORE PAGES PRESENT *)
   { COMPRESS PAGES SWITCH }
   P := First^.Next ;
   IF ( 'C' IN Switches )
   THEN BEGIN (* TRY TO COMPRESS THE TWO PAGES *)
           IF ( First^.Next <> Last )
           THEN BEGIN (* THREE OR MORE PAGES PRESENT *)
                   WITH First^ DO
                   BEGIN (* INSIDE FIRST PAGE REC *)
                      IF ( P^.Cur_LPP + Cur_LPP + 1 < Lines_Per_Page )
                      THEN BEGIN (* COMBINE FIRST TWO PAGES INTO ONE *)
                              New_Line_Ptr    ( L ) ;
                              Last_Line^.Next := L ;
                              Last_Line       := L ;{INSERT BLANK LINE}
                              Last_Line^.Next := P^.First_Line ;
                              Last_Line       := P^.Last_Line ;
                              Squeezed        := TRUE;
                              Cur_LPP         := Cur_LPP + P^.Cur_LPP+1;
                              IF Max_CPL < P^.Max_CPL
                                 THEN Max_CPL := P^.Max_CPL ;
                           END   (* COMBINE FIRST TWO PAGES INTO ONE *)
                      ELSE BEGIN
                              Print__Page ( First , Last , Page_Numb ) ;
                              Squeezed := FALSE ;
                           END ;
                   END ; (* INSIDE FIRST PAGE REC *)
                   IF squeezed THEN Delete_Page ( First , P , Last ) ;
                END ; (* THREE OR MORE PAGES PRESENT *)
        END   (* TRY TO COMPRESS THE TWO PAGES *)
   ELSE BEGIN (* PRINT PAGE *)
           Print__Page ( First , Last , Page_Numb ) ;
        END ; (* PRINT PAGE *)
END ; (* TWO OR MORE PAGES PRESENT *)
