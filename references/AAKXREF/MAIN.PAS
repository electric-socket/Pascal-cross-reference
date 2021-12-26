BEGIN {*** main ***}
   (***************** INIT ***********************)
   lastf :=' ';  { to prevent an error; see CASE scan of com2,pcom2x }
   Switches     := [];
   get_info;
   empty_keyboard;
   IF ( 'S' IN Switches )
   THEN Output_Device := CRT
   ELSE IF ( 'F' IN Switches )
        THEN Output_Device := Disk
        ELSE Output_Device := Printer ;
   (* INIT VARS *)
   First_Page   := NIL ;
   Last_Page    := NIL ;
   Cur_Page     := NIL ;
   Page_Numb    := 0 ;
   New_Line_Ptr ( Line ) ;
   Line_Numb    := 1 ;
   STR          ( Line_Numb : Linenum_Size , ID ) ;
   Id           := Id + ' ' ;
   Add_Line_Str ( Line , Id , LENGTH ( Id ) ) ;
   Taken_Careof := False ;
   Inc_Root     := NIL ;
   Inc_Last     := NIL ;
   Root         := nil;
   scan         := none;
   title        := 'Main';

   (************************** INIT OUTPUT DEVICE *********************)
   IF NOT ('S' in switches) THEN
   BEGIN
      WRITELN ;
      WRITELN ('Listing main file ',File_Name);
      IF 'F' in switches THEN
      BEGIN
         WRITELN ( 'All data to file ', outname ) ;
         ASSIGN  ( OutF , OutName ) ;
         REWRITE ( OutF ) ;
      END ;
      IF 'O' IN Switches THEN
      BEGIN
         File_Separator ( File_Name , File_Path , File_Nam , File_Ext);
         Out_Odd_Name := File_Path + File_Nam + '.ODD' ;
         WRITELN ( File_Path , '//',File_Nam ,'//', File_Ext ) ;
         WRITELN ( 'Odd Pages To File ' , Out_Odd_Name ) ;
         ASSIGN  ( OddF , Out_Odd_Name ) ;
         REWRITE ( OddF ) ;
         Switches := Switches + ['P'] ;
      END ;
      IF 'E' IN Switches THEN
      BEGIN
         File_Separator ( File_Name , File_Path , File_Nam , File_Ext);
         Out_Even_Name := File_Path + File_Nam + '.EVE' ;
         WRITELN ( 'Even Pages to file ' , Out_Even_Name ) ;
         ASSIGN  ( EveF , Out_Even_Name ) ;
         REWRITE ( EveF ) ;
         Switches := Switches + ['P'] ;
      END ;
   END;
   IF ('P' IN Switches )
   THEN BEGIN
           WRITE ( LST , Pre_Set_Skip + ^B ); { PERF SKIP = 2 }
           IF '6' IN Switches THEN Lines_Per_Page := 64
           ELSE IF '8' IN Switches THEN Lines_Per_Page := 84
                ELSE Lines_Per_Page := 94;
        END;
   NEW ( Inc_Last ) ;
   WITH Inc_Last^ DO
   BEGIN (* WITH *)
      Prev := Inc_Root ;
      Inc_File_Name := File_Name ;
      Inc_Title := 'Main' ;
   END ; (* WITH *)
   Inc_Root := Inc_Last ;
   WHILE Inc_Root <> NIL DO
   BEGIN (* PRINT FILES *)
      Inc_Last := Inc_Root ;
      WITH Inc_Last^ DO
      BEGIN (* WITH *)
         Inc_Root := Prev ;
         title    := Inc_Title ;
         File_Name := Inc_File_Name ;
      END ; (* WITH *)
      DISPOSE ( Inc_Last ) ;
      Inc_Last := NIL ;
      File_Separator ( File_Name , File_Path , File_Nam , File_Ext ) ;
      IF ( EXIST ( File_Path + File_Nam + '.BAK' ) )
      THEN BEGIN (* BACKUP FILE EXISTS *)
              IF ( 'B' IN Switches )
              THEN BEGIN (* DELETE BACKUP *)
                      IF NOT ( 'S' IN Switches ) THEN
                      BEGIN (* MESSAGE *)
                         WRITELN ;
                         WRITELN ( 'Deleting ' , File_Path , File_Nam ,
                                   '.BAK' ) ;
                         WRITE   ( 'Processing line # ' ) ;
                      END ; (* MESSAGE *)
                      ASSIGN ( Tem_File , File_Path + File_Nam +
                              '.BAK' ) ;
                      ERASE  ( Tem_File ) ;
                      IF ( 'M' IN Switches )
                      THEN BEGIN (* YES OUTPUT *)
                              IF ( 'S' IN Switches )
                              THEN Output_Device := CRT ;
                              IF ( 'P' IN Switches )
                              THEN BEGIN
                                      Output_Device := Printer ;
                                   END ;
                           END ; (* YES OUTPUT *)
                   END ; (* DELETE BACKUP *)
           END   (* BACKUP FILE EXISTS *)
      ELSE BEGIN (* NO BACKUP FILE EXISTS *)
              IF NOT ( 'M' IN Switches )
              THEN BEGIN (* YES OUTPUT *)
                      IF ( 'S' IN Switches )
                      THEN Output_Device := CRT ;
                      IF ( 'P' IN Switches )
                      THEN Output_Device := Printer ;
                   END   (* YES OUTPUT *)
              ELSE Output_Device := Null ;
           END ; (* NO BACKUP FILE EXISTS *)
      ASSIGN ( fv , File_Name ) ;
      Reset  ( Fv ) ;
      Taken_Careof := False ;
      do_listing ( fv , title , File_Name , none ) ;
   END ; (* PRINT FILES *)
(*************************** CROSS REFRENCE ***********************)
   IF ('N' in switches) THEN
   BEGIN (* PRODUCE CROSS REF *)
      IF NOT ('S' in switches) THEN
      BEGIN
         WRITELN;
         WRITE('Listing cross reference of ',Main_File_Name);
         IF 'F' in switches THEN WRITELN(' to file ',outname)
         ELSE WRITELN;
         WRITE('Processing line #')
      END;
      title := 'Xref';
      ASSIGN ( fv , Main_File_Name ) ;
      Reset  ( fv ) ;
      New_Page ( fv , Main_File_Name , Title ) ;
      printtree(root);
      CLOSE ( fv ) ;
   END ;
   (************************* PRINT TRAILING PAGES ******************)
   WHILE First_Page <> NIL DO
     Print__Page ( First_Page , Last_Page , Page_Numb ) ;
   (************************* CLOSE FILES AND EXIT *****************)
   IF 'O' IN Switches THEN CLOSE ( Oddf ) ;
   IF 'E' IN Switches THEN CLOSE ( EveF ) ;
   IF 'F' IN Switches THEN CLOSE ( OutF ) ;
END.
