(********************************************************************)
(* THIS PROCEDURE WILL PRINT OUT CROSS REFRENCE LISTING TO THE      *)
(* OUTPUT STREEM CONTROLED BY THE SWITCHES                          *)
(* INPUT        : NONE                                              *)
(* INPUT/OUTPUT :                                                   *)
(* OUTPUT       :                                                   *)
(* USES         : NO SIDE EFFECTS                                   *)
(* CALLS        :                                                   *)
(* ERRORS       :                                                   *)
(* GLOBAL VARS  : NONE                                              *)
(* SUBJECT      :                                                   *)
(* DOMAIN       : ARNO A. KARNER                      LEVEL : 00?   *)
(********************************************************************)

PROCEDURE printtree ( w : Word_Ptr ) ;

   PROCEDURE Print_Word ( w : Word ) ;

   VAR
      l   : INTEGER ;
      x   : Item_Ptr;
      Tem : Work_String ;

   BEGIN (* NESTED PRINT WORD *)
      Tem := W.Key ;
      WHILE LENGTH ( Tem ) < Ch_Per_Word DO Tem := Tem + ' ' ;
      Tem := Tem + ' : ' ;
      Add_Line_Str ( Line , Tem , LENGTH ( Tem ) ) ;
      x := w.first;
      l := 0 ;
      repeat
         IF l = Ref_Per_Line THEN
         BEGIN (* NEW LINE *)
            New_Line ( fv , File_Name , Title , Line , Line_Numb ) ;
            Tem := '' ;
            WHILE LENGTH ( Tem ) < Ch_Per_Word DO Tem := Tem + ' ' ;
            Tem := Tem + ' : ' ;
            Add_Line_Str ( Line , Tem , LENGTH ( Tem ) ) ;
            IF NOT ( 'S' IN Switches ) THEN scrn_update(false);
            l := 0
         END ; (* NEW LINE *)
         l := l + 1 ;
         STR ( X^.Lno : LineNum_Size , Tem ) ;
         Add_Line_Str ( Line, Tem , LENGTH ( Tem ) ) ;
         x := x^.next
      UNTIL X = NIL ;
      New_Line ( fv , File_Name , Title , Line , Line_Numb ) ;
      IF NOT ( 'S' IN Switches ) THEN scrn_update ( false )
   END ; (* NESTED PRINT WORD *)

BEGIN (* PRINT TREE *)
   IF w <> NIL THEN
   BEGIN
      printtree ( w^.left ) ;
      Print_Word ( w^ ) ;
      printtree ( w^. right ) ;
   END ;
END ; (* PRINT TREE *)
