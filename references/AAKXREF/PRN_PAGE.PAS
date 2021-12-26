(********************************************************************)
(* THIS PROCEDURE WILL OUTPUT PAGE TO THE OUT_DEVICE CONTAINED      *)
(* THIS PAGE RECORD                                                 *)
(* INPUT        : NONE                                              *)
(* INPUT/OUTPUT : First,Last,No                                     *)
(* OUTPUT       : OUTPUTED PAGE OF INFORMATION                      *)
(* USES         : NO SIDE EFFECTS                                   *)
(* CALLS        : Print_Out                                         *)
(* ERRORS       : NONE                                              *)
(* GLOBAL VARS  : NONE                                              *)
(* SUBJECT      : PRINTER, PAGE OUTPUT                              *)
(* DOMAIN       : ARNO A. KARNER                      LEVEL : 001   *)
(********************************************************************)

PROCEDURE Print__Page                 ( VAR First , Last : Page_Ptr ;
                                        VAR No           : INTEGER ) ;

VAR
   P        : Page_Ptr ;
   L        : Line_Ptr ;
   Tem      : String_10 ;
   T, Setup : Work_String ;

BEGIN (* PROC *)
   Setup := '';
   No := No + 1 ;
   STR ( No : Linenum_Size , Tem ) ;
   P := First ;
   First := First^.Next ;
   IF ('6' IN Switches )
   THEN BEGIN (* 66 LINES PER INCH *)
           Setup          := Setup + LPI06 ;
        END
   ELSE BEGIN (* 88 LINES PER PAGE *)
           IF ('9' IN Switches )
           THEN BEGIN (* 9 LINES PER INCH  *)
                   Setup          := Setup + LPI09 ;
                END   (* 9 LINES PER INCH  *)
           ELSE BEGIN (* DEFAULT 8 LINES PER INCH *)
                   Setup          := Setup + LPI08 ;
                END ; (* DEFAULT 8 LINES PER INCH *)
        END ;
   WITH P^ DO
   BEGIN (* WITH THIS PAGE *)
      IF Max_CPL <= 80
      THEN BEGIN
              Setup := Setup + CPL80 ;
              Chars_Per_Line := 80 ;
           END
      ELSE IF Max_CPL <= 96
           THEN BEGIN
                   Setup := Setup + CPL96 ;
                   Chars_Per_Line := 96 ;
                END
           ELSE BEGIN
                   Setup := Setup + CPL132 ;
                   Chars_Per_Line := 136 ;
                END ;
      IF ('D' IN Switches ) THEN Setup := Setup + Double_Strike_On
           ELSE Setup := Setup + Double_Strike_Off ;
      IF First_Line <> NIL
      THEN BEGIN (* PUT PAGE NUMBER IN *)
              IF ODD ( No )
              THEN BEGIN (* ODD PAGE *)
                      STR ( Max_CPL : 6 , T ) ;
                      Setup := Setup + T ;
                      STR ( Cur_LPP : 6 , T ) ;
                      WHILE LENGTH ( T ) < Chars_Per_Line - 27 DO
                            T := T + ' ' ;
                      T := T + 'Page ' + Tem ;
                      First_Line^.Line := Setup + T ;
                      IF ('O' IN Switches)
                      THEN Out_Device := Odd_Pages
                      ELSE IF ('P' IN Switches )
                           THEN Out_Device := Printer
                           ELSE IF ('S' IN Switches )
                                THEN Out_Device := CRT ;
                   END   (* ODD PAGE *)
              ELSE BEGIN (* EVEN PAGE *)
                      STR ( Max_CPL : 6 , T ) ;
                      Setup := Setup + T ;
                      STR ( Cur_LPP : 6 , T ) ;
                      Setup := Setup + T + '  ' ;
                      First_Line^.Line := Setup + ' Page ' + Tem ;
                      IF ('E' IN Switches)
                      THEN Out_Device := Even_Pages
                      ELSE IF ('P' IN Switches )
                           THEN Out_Device := Printer
                           ELSE IF ('S' IN Switches )
                                THEN Out_Device := CRT ;
                   END ; (* EVEN PAGE *)
           END ; (* PUT PAGE NUMBER IN *)
      WHILE First_Line <> NIL DO
      BEGIN (* PRINT LINES *)
         L          := First_Line ;
         First_Line := First_Line^.Next ;
         Print_Out ( Out_Device , L^.Line ) ;
         DISPOSE ( L ) ;
      END ; (* PRINT LINES *)
      IF ( 'P' IN Switches )
      THEN Print_Out ( Out_Device , Form_Feed ) ;
   END ; (* WITH THIS PAGE *)
   DISPOSE (P) ;
END ; (* PROC *)
