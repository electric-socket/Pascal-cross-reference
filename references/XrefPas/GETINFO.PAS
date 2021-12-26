procedure get_info;

var
   n,m,i : integer;
   Dir,
   Path : File_Str ;
   Name : String_08 ;
   Ext  : String_03 ;
   parameters : string[127] absolute cseg:$0080;
   workparams : string[127];
   The_File : TEXT ;

   procedure query_File_Name;

   begin
      File_Name := '' ;
      write('Enter name of file to be listed [.PAS] : ');
      readln ( File_Name ) ;
      IF LENGTH ( File_Name ) = 0 THEN HALT ;
      File_Separator ( File_Name , Path , Name , ext ) ;
      WRITELN ( Path , '//' , Name , '//' , Ext ) ;
      IF ( Ext = '' )
      THEN File_Name := Path + Name + '.PAS' ;
   end;

   procedure get_File_Name;

   begin
      M := 0;
      repeat
         M := M+1
      until (M > length(workparams)) or (workparams[M] <> ' ');
      N:=M;
      REPEAT
         N:=N+1
      UNTIL (N>length(workparams)) OR (workparams[N]='/');
      File_Name := copy ( workparams , m , ( n - m ) ) ;
      File_Separator ( File_Name , Path , Name , ext ) ;
      IF ( Ext = '' ) AND ( File_Name [ LENGTH ( File_Name ) ] <> '.' )
      THEN File_Name := Path + Name + '.PAS' ;
      IF ( LENGTH ( File_Name ) = 0 ) OR ( not Exist ( File_Name ) )
      THEN BEGIN
              repeat
                 writeln ( 'File ' , File_Name , ' not found.' ) ;
                 query_File_Name;
              until Exist (File_Name);
           END ;
   END ; (* NESTED PROC *)

   procedure waytogo_user;  {* File_Name and switches on command line*}
   begin
      n := pos('/',workparams) + 1;
      While n<=length(workparams) do
      begin
         if upcase( workparams [ n ] ) IN
            ['6','8','9','B','C','D','E','F','I','N','M','O','P','S','T']
         then switches := switches + [upcase(workparams[n])];
         n:=n+1
      end ;
   end;

   PROCEDURE Switch_Menu ;
   VAR
      Answer, Answer1 : char;
   BEGIN (* NESTED *)
      write('Output to file, screen, or printer (F,S,P) ? ');
      answer := get_choices('f','s','p');
      If answer = 'P'
      THEN BEGIN (* PRINTER OPTIONS *)
              Switches := Switches + ['P'] ;
              write('Printer output in Double strike mode (Y,N) ? ');
              if get_answer('y','n') = 'Y'
              then switches := switches + ['D'] ;
              WRITE ('Spool even pages to disk file (Y,N) ? ');
              if get_answer('y','n') = 'Y'
              then switches := switches + ['E'];
              WRITE ('Spool odd pages to disk file (Y,N) ? ');
              if get_answer('y','n') = 'Y'
              then switches := switches + ['O'];
              WRITE ('Compress Procedures on page (Y,N) ? ');
              if get_answer('y','n') = 'Y'
              then switches := switches + ['C'];
              WRITE ('Lines per inch (6,8,9) ? ');
              Answer1 := Get_Choices ('6','8','9') ;
              IF Answer1 = '6' THEN BEGIN
                                       Switches := Switches + ['6'] ;
                                    END
              ELSE IF Answer1 = '8'
                   THEN BEGIN
                           Switches := Switches + ['8'] ;
                        END
                   ELSE BEGIN
                           switches := switches + ['9'] ;
                        END ;
           END
      else
         if answer='S'
         then switches := switches + ['S']
         else begin
                 switches := switches + ['F'];
                 write('Enter name of output file [',copy(File_Name,1,
                        pos('.',File_Name)-1),'.','LST]');
                 readln(outname);
                 if outname=''
                 then outname := copy(File_Name,1,pos('.',File_Name)-1)
                              + '.' + 'LST'
              end;
      write('List Include files within the Main listing (Y,N) ? ');
      if get_answer('y','n') = 'Y'
      then BEGIN
              switches := switches + ['I'];
              WRITE ('Produce top down program listing (Y,N) ? ');
              if get_answer('y','n') = 'Y'
              then switches := switches + ['T'];
        END ;
      write('Produce cross reference of user-defined Vars (Y,N) ? ');
      if get_answer('y','n') = 'Y'
      then switches := switches + ['N'];
      WRITE ('Delete back up files as listed (Y,N) ? ');
      if get_answer('y','n') = 'Y'
      then switches := switches + ['B'];
      WRITE ('Output only modified files (Y,N) ? ') ;
      IF Get_Answer ( 'y' , 'n' ) = 'Y'
      THEN Switches := Switches + ['M'] ;
   end;

begin  (* PROC *)
   workparams := parameters;
   If ( POS ( '/' , workparams ) > 0 ) AND
      ( POS ( '/' , WorkParams ) < LENGTH ( WorkParams ) )
   THEN BEGIN (* SWITCHES ON COMMAND LINE *)
           get_File_Name;
           WayToGo_User ;
        END
   ELSE BEGIN (* 03 INPUT DATA SUPLIED *)
           Get_File_Name ;
           Switch_Menu ;
        END ;
   while File_Name [ LENGTH ( File_Name ) ] = #0 DO
        delete ( File_Name , length ( File_Name ) , 1 ) ;
   WRITELN ('FILE NAME ' , File_Name ) ;
   I := LENGTH ( File_Name ) ;
   WHILE ( I > 0 ) AND ( File_Name [ I ] <> '.' ) DO I := I - 1 ;
   IF ( I <> 0 ) AND ( '.BAK' = Copy ( File_Name , I , 4 ) ) THEN
   BEGIN (* DEL BAK RENAME FILE *)
      WRITE ( 'Deleting backup file' ) ;
      ASSIGN ( The_File , File_Name ) ;
      ERASE ( The_File ) ;
      File_Name := Copy ( File_Name , 1 , I ) + 'PAS' ;
   END ; (* DEL BAK RENAME FILE *)
   GetDir ( 0, Dir ) ;
   Dir := Fix_Path_Str ( Dir ) ;
   File_Name := Dir + File_Name ;
   File_Separator ( File_Name , Main_Path, Name, Ext ) ;
   File_Name := Fix_Path_Str ( File_Name ) ;
{$V-} { don't care about string size }
   Upper_Case ( File_Name ) ;
{$V+}
   Main_File_Name := File_Name ;
END ; (* PROC *)
