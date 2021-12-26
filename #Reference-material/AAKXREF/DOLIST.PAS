PROCEDURE do_listing           ( VAR fv    : TEXT ;
                                     title : titletype ;
                                     fn    : file_Str ;
                                     mode  : state ) ;

VAR
   Path : File_Str ;
   Name : String_08 ;
   Ext  : String_03 ;

   PROCEDURE bugout;

   BEGIN
    parsing_for_dollars := false;
    itsan_include := false;
    itsa_directive := false
   END;


   PROCEDURE isitan_include;

   BEGIN
      while f = ' ' do
      BEGIN
         Add_Line_Ch ( Line , f ) ;
         read ( fv , f )
      END ;
      incname:='';
      REPEAT
         Add_Line_Ch ( Line , f ) ;
         incname :=incname + f;
         read(fv,f);
      UNTIL NOT (f in ['.','\',':','A'..'Z','a'..'z','_','0'..'9']);
      File_Separator ( IncName, Path, Name, Ext ) ;
      IF Ext = '' THEN incname := incname + '.PAS';
      IncName := Fix_Path_Str ( Main_Path + IncName ) ;
      IF NOT ( 'T' IN Switches )
      THEN BEGIN (* REGULAR INCLUDED THIS SUCKS *)
              IF Exist ( Incname )
              THEN BEGIN (* INCLUDED EXISTS *)
                      assign ( iv , incname);
                      Reset ( iv ) ;
                      IF NOT ('S' in switches) THEN
                      BEGIN
                         WRITELN ;
                         WRITELN;
                         WRITE ('  Listing include file ',incname);
                         IF 'F' in switches THEN WRITELN(' to file ',outname)
                         ELSE WRITELN;
                         WRITE('  Processing line #')
                      END;
                      taken_careof := False ;
                      do_listing ( iv , 'Include' , incname , none ) ;
                      New_Page ( fv , fn , title ) ;
                      taken_careof := true;
                      IF NOT ('S' in switches) THEN
                      BEGIN
                         WRITELN;
                         WRITELN;
                         WRITE ('Listing main file ',File_Name);
                         IF 'F' in switches THEN WRITELN(' to file ',outname)
                         ELSE WRITELN;
                         WRITE ('Processing line #')
                      END;
                  END  (* INCLUDED EXISTS *)
             ELSE WRITELN ('Error included File : ',Incname ,' does not exist ' ) ;
          END
     ELSE BEGIN (* SAVE FILE NAMES *)
              IF Memory > Min_Memory THEN
              BEGIN (* ENOUGH MEMORY *)
                 NEW ( Inc_Last ) ;
                 WITH Inc_Last^ DO
                 BEGIN (* WITH *)
                    Prev := Inc_Root ;
                    Inc_File_Name := IncName ;
                    Inc_Title := 'Included' ;
                 END ; (* WITH *)
                 Inc_Root := Inc_Last ;
              END ; (* ENOUGH MEMORY *)
           END ; (* SAVE FILE NAMES *)
      parsing_for_dollars := false;
      itsa_directive      := false;
      itsan_include       := false;
   END ; (* NESTED ITS AN INCLUDED *)


   PROCEDURE Symbol_Parsing ;
   BEGIN (* NESTED SYMBOL PARSING *)
      IF f in ['.','a'..'z','A'..'Z','0'..'9','_']
      THEN BEGIN
              id := id + f;
           END
      ELSE BEGIN (* END NAME *)
              WRITEid;
              Add_Line_Ch ( Line , f ) ;
              IF f = '''' THEN scan := quote
              ELSE BEGIN (* NOT QUOTE *)
                      IF f = '{'
                      THEN BEGIN
                              scan := com1;
                              IF 'I' in switches THEN parsing_for_dollars := true
                           END
                      ELSE IF f = '(' THEN scan := pcom2
                           ELSE  scan := none ;
                   END ; (* NOT QUOTE *)
           END ; (* END NAME *)
   END ; (* NESTED SYMBOL PARSING *)

   PROCEDURE Com1_Parsing ;
   BEGIN (* NESTED COM1 PARSING *)
      Add_Line_Ch ( Line , f ) ;
      IF ( f = '+' ) or ( f = '-' ) THEN bugout;
      IF itsan_include THEN
      BEGIN
         isitan_include ;
      END;
      IF itsa_directive THEN
      BEGIN (* IT IS A DERECTIVE *)
         IF ( f = 'I' ) or ( f = 'i' )
         THEN BEGIN
                 itsan_include  := true;
                 itsa_directive := false
              END
         ELSE itsa_directive := false;
      END ; (* IT IS A DIRECTIVE *)
      IF parsing_for_dollars THEN
      BEGIN (* PARSING FOR DOLLARS *)
         IF f = '$'
         THEN BEGIN
                 parsing_for_dollars :=false;
                 itsa_directive := true
              END
         ELSE parsing_for_dollars := false;
      END ; (* PARSING FOR DOLLARS *)
      IF f = '}' THEN
      BEGIN (* END OF COMMENT *)
         parsing_for_dollars := false;
         itsa_directive := false;
         itsan_include := false;
         scan := none
      END ; (* END OF COMMENT *)
   END ; (* NESTED COM1 PARSING *)

   PROCEDURE Pcom2_Parsing ;
   BEGIN (* PCOM 2 PARSING *)
      IF f in['a'..'z','A'..'Z','_']
      THEN BEGIN
              id := f;
              scan := symbol
           END
      ELSE BEGIN
              Add_Line_Ch ( Line , f ) ;
              IF f = '''' THEN scan := quote
              ELSE BEGIN
                      IF f = '{'
                      THEN BEGIN
                              scan := com1;
                              IF 'I' in switches THEN parsing_for_dollars := true ;
                           END
                      ELSE BEGIN
                              IF f = '(' THEN scan := pcom2
                              ELSE BEGIN
                                      IF f = '*'
                                      THEN BEGIN
                                              scan := com2;
                                              IF 'I' in switches THEN parsing_for_dollars := true ;
                                           END
                                      ELSE  scan := none
                                   END ;
                           END ;
                   END ;
           END ;
   END ; (* PCOM 2 PARSING *)

   PROCEDURE Com2_Parsing ;
   BEGIN
      Add_Line_Ch ( Line , f ) ;
      IF (f='+') or (f='-')  THEN bugout;
      IF itsan_include THEN
      BEGIN
         isitan_include;
      END;
      IF itsa_directive  AND  ( ( f = 'I' ) OR ( f = 'i' ) )
      THEN BEGIN (* IT IS A DIRECTIVE *)
              itsan_include := true;
              itsa_directive := false
           END
      ELSE itsa_directive := false;
      IF parsing_for_dollars AND ( F = '$' )
      THEN BEGIN
              itsa_directive := true;
              parsing_for_dollars := false
           END
      ELSE    parsing_for_dollars := false;
      IF f = '*' THEN scan := pcom2x
      ELSE BEGIN
              IF (f = ')') and (lastf='*') THEN
              BEGIN
                 parsing_for_dollars := false;
                 itsa_directive := false;
                 itsan_include := false;
                 scan := none
              END ;
           END ;
   END ; (* COM2 PARSING *)


BEGIN  (* DO LISTING *)
   Fn := Fix_Path_Str ( Fn ) ;
   scan                := mode ;
   parsing_for_dollars := false;
   itsa_directive      := false;
   itsan_include       := false;
   WHILE NOT EOF ( fv ) DO
   BEGIN (* NOT END OF FILE *)
   IF NOT Taken_Careof
   THEN BEGIN (* First page title of current file *)
           New_Page ( fv , fn , title ) ; (* File must be open *)
           Taken_Careof := TRUE ;
        END;
   IF NOT ('S' in switches) THEN  scrn_update(title='Include');
      while ( NOT EOLN (fv) ) AND ( NOT EOF (fv) ) DO
      BEGIN
         IF keypressed THEN dealwithuser;
         read ( fv , f);
         case scan of
            none:   Start_Parsing ;
            symbol: Symbol_Parsing ;
            quote:  BEGIN
                       Add_Line_Ch ( Line , f ) ;
                       IF f = '''' THEN scan := none ;
                    END;
            com1:   Com1_Parsing ;
            pcom2:  Pcom2_Parsing ;
            com2:   Com2_Parsing ;
            pcom2x: BEGIN
                       Add_Line_Ch ( Line , f ) ;
                       IF (f = ')') THEN scan := none
                       ELSE BEGIN
                               scan := com2;
                               lastf:=f
                            END
                    END;
         END ; (* CASE *)
      END ; (* NOT END OF LINE *)
      IF scan = symbol THEN
      BEGIN
         WRITEid;
         scan := none
      END;
      New_Line ( fv , File_Name , Title , Line , Line_Numb ) ;
      readln (  fv ) ;
   END ; (* NOT EOF *)
   CLOSE ( Fv ) ;
END;

