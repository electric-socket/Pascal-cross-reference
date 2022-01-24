{$I CrossrefPrefixCode.inc}  // STD definitions
// Crossref - Pascal Cross reference
// Version 0.01
// January 9, 2021
// Copyright 2021 Paul Robinson
// Licensed under GPL v2

// Scan.pas - Scan unit
// Scan the source file, processing it for keywords and identifiers,
// responding to both, then copying the file to the output file
unit scan; 
interface
uses Sysutils, Tables;
var



        LocalListing,          // Separate Text listing of unit
        LocalHTML,             // Separate HTML list of unit
        LocalPDF,              // Separate PDF of unit
        LocalCREF,             // Local Ctoss-reference of unit
        NoLocalBTL,            // Don't cross-reference implementation items
        NoLocalProc,           // Don't cross-reference procedure idenmtifiers
        NoGlobalProcCREF,      // don't include procedure-level identifiers
                               // in Global cross-reference
        NoGlobalBTLCREF,       // dont include implementation items in
                               // global cross-reference
        GlobalCREF,            // Cross-reference of project
        GlobalListing,         // Text listing of project
        GlobalHTML,            // HTML listing of project
        GlobalPDF: Boolean;    // PDF of complete project



   Procedure TestMode;
   procedure SplitPath(const Path: UnicodeString; var Folder, Name, Ext: UnicodeString);
   // open a file where all we have is the name w/o path or extension
   Procedure OpenUnitFile(FName:UnicodeString);
   // this time we have a full file name
   procedure OpenInputFile(FileName:UnicodeString);
   Procedure GetLine;
   Procedure AllocateBuffer;
   Procedure InitPrintLine;                // process printing



implementation


procedure SplitPath(const Path: UnicodeString; var Folder, Name, Ext: UnicodeString);
var
    DotPos, SlashPos, i: Integer;
begin
    Folder := '';
    Name := Path;
    Ext := '';

    DotPos := 0;
    SlashPos := 0;

    for i := Length(Path) downto 1 do
        if (Path[i] = '.') and (DotPos = 0) then
            DotPos := i
        else if (Path[i] = '\') and (SlashPos = 0) then
            SlashPos := i;

    if DotPos > 0 then
    begin
        Name := Copy(Path, 1, DotPos - 1);
        Ext  := Copy(Path, DotPos, Length(Path) - DotPos + 1);
    end;

    if SlashPos > 0 then
    begin
        Folder := Copy(Path, 1, SlashPos);
        Name   := Copy(Path, SlashPos + 1, Length(Name) - SlashPos);
    end;

end;


// we have a whole file name, try to open
procedure OpenInputFile(FileName:UnicodeString);
Var

     FolderIndex: NameRange;
    NoFolder: boolean;

begin


    // First search the source folder, then the folders specified in $UNITPATH
    // or other specified places

    FolderIndex := 1;
    FileMode := 0; // force read-only in case file is read only

    with Buffer^ do
    repeat
         if TopFolder = 0 then // can't read if none there
            NoFolder := TRUE
         Else
            Folder := FolderTable[FolderIndex];

         If Pos(Slashchar,Name )>0 then // if it's altrady marked with a path
         begin                          // don't add one
             FullName :=  FileName;
             NoFolder := TRUE;
         end
         else
         begin
             if Folder <> '' then   // add path
                Folder := Folder + SlashChar;
             FullName := Folder + FileName ;
         end;
         Assign(F,FullName );
         {$I-} Reset(F); {$I+}
         Error :=  IOResult;  // IOResult is lost once read, so save it
         if  Error = 0 then Exit;
         {$ifndef mswindows}
         // Additional attemptss for non-windows

         // repeat using all lower case
         FullName := LowerCase(FullName);
         Assign(F, FullName);
         {$I-} Reset(F); {$I+}
         Error :=  IOResult;  // IOResult is lost once read, so save it
         if  Error = 0 then Exit;

         // repeat using all Upper Case
         Buffer^.FullName := UpperCase(Buffer^.FullName);
         Assign(Buffer^.F, Buffer^.FullName);
         {$I-} Reset(Buffer^.F); {$I+}
         Error :=  IOResult;
         if  Error = 0 then Exit;
         {$endif}
         if nofolder then exit;
         Inc(FolderIndex);
         Folder := FolderTable[FolderIndex];
     until FolderIndex < TopFolder ;
end;

// we're handed a unit name, try matching it to a file
Procedure OpenUnitFile(FName:UnicodeString);
Var
    ExtensionIndex,
    FolderIndex: NameRange;


begin
with Buffer^ do
    begin
    // First search the source folder, then the folders specified in $UNITPATH
    // or other specified places

    FolderIndex := 1;
    ExtensionIndex := 1;
    FileMode := 0; // force read-only in case file is read only

        repeat
            Folder := FolderTable[FolderIndex];
            Name := FName;

        // Do not use a FOR loop; we need to have extensiions exceed count
        // to know to move to the next folder
            repeat
                Ext := Extensions[ExtensionIndex];
                FullName :=  Folder;
                if FullName <> '' then
                   FullName := FullName + SlashChar;
                FullName := FullName + Name + '.' + Ext;
                Assign(F, FullName );
                {$I-} Reset(F); {$I+}
                Error :=  IOResult;  // IOResult is lost once read, so save it
                if Error = 0 then exit;
                {$ifdef mswindows}
                Inc(ExtensionIndex);
                {$else}
                // Additional attempts for non-windows

                // repeat using all lower case
                Folder := LowerCase(Folder);
                Ext    := LowerCase(Ext);
                Name   := LowerCase(Name);
                FullName := LowerCase(FullName);
                Assign(F, FullName);
                {$I-} Reset(F); {$I+}
                Error := IOResult;
                if Error = 0 then Exit;

                // repeat using all Upper Case
                Folder := UpperCase(Folder);
                Ext    := UpperCase(Ext);
                Name   := UpperCase(Name);
                FullName := UpperCase(FullName);
                Assign(F, FullName);
                {$I-} Reset(F); {$I+}
                Error := IOResult;
                if Error = 0 then Exit;

            // We're out of ideas, try again
                Inc(ExtensionIndex);
                {$endif}
            // try the next extension
        until ExtensionIndex > TopExtension;
        If ExtensionIndex <= TopExtension then EXIT;
        // try the next folder
        inc(FolderIndex);
        Folder := FolderTable[FolderIndex];
        until FolderIndex > TopFolder;
    end
end;


    Procedure InitUnit;    //   Initialize this unit
    Begin //*Procedure InitUnit
        With Buffer^ do
        begin
     //      General initializarion


     //  as desired, open local
     //    listing file
     //    HTML file
     //    PDF file
     // Increment Master page numbers and set line numbers to 1
     // eject page on each
     // If we are doing a complete listing, prepare to
     //    Start new page in master output listing file
     //    Start new "page" in master HTML, if paging desired
     //    start new page in master PDF
     //  Set up page headers

        end;
    end;

    Procedure InitPrintLine;
    Begin //*Procedure InitPrintLine
    // Convert the text in Buffer^.TextLine into output
    // if necessary, fold lines that won't fit, and output to
    // various file formats

    end; // Procedure Print

    Procedure CompletePrintLine;
    begin
        // finish moving the rest of the line to output
        With Buffer^ do
        begin

        end;
    end;

    Procedure SkipSpaces;
    Begin        // bypass whitespace
        With Buffer^ Do
        Begin
            While (Inindex <= Len) and (TextUC[InIndex] =' ') do
                Inc(Inindex);
        end;
    end;

    Procedure HandleCompilerDirective;
    begin
        // finish moving the rest of the line to output
        With Buffer^ do
        begin

        end;
    end;

    // follow through to end of block comment then resume
    PROCEDURE CheckComment(C:UnicodeString; Len:integer);
    Var
        Done: Boolean = False;

    begin
       With Buffer^ do
       begin
           If TextUC[Inindex] = '$' then
              HandleCompilerDirective
           else
           repeat
               Inc(InIndex);
               if InIndex > Len then
               begin  // comment continues on next line
                   CompletePrintLine;
                   GetLine;
                   if EOF(F) then exit;

               end;
               if TextUC[Inindex] = C[1] then
                 if Len = 1 then
                 begin
                     Inc(InIndex);
                     exit;  // step over closure
                 end
                 else
                 begin  // we don't advance now in case of something
                        // like (* **) the first * does match, but the
                        // second does not, if we had advanced here, we'd
                        // skip two chars, thus missing the actual
                        // closure
                     if TextUC[Inindex+1] = C[2] then
                     begin
                         Inc(InIndex,2);
                         exit;  // step over closure
                     end;
                 end;

           until done or eof(F);
       end;
    end;

    Procedure GetLine;
    begin
       With Buffer^ do
       begin
       Repeat
         Inc(LineNumber);
         Inindex := 0;
         If EOF(F) then exit;
         {$I-} Readln(F,TextLine); {$I+}
         Error := IOResult;
         if Error<> 0 then
         begin
             Writeln('?Error reading file ',FullName,' at line ',LineNumber);
             Exit;
         end;
         InitPrintLine;                  // start of printing
         Len := Length(TextLine);
         if trim(TextLine)='' then   // Don't bother scanning
            Len := 0;                // all blank lines
         If Len < 1  then
             CompletePrintLine;          // finish line
         Until len > 0 ;             // keep skipping blank lines
       InIndex := 1;                     // start scanning here
       TextLine := TextLine + ' '+#0; // ensure line has 2 extra chars
       TextUC := UpperCase(TextLine);

       // at this point we know there is at least one non-blank char
       While (TextUC[Inindex] = ' ') do // move to it
           Inc(Inindex);
       end
    end;

    Procedure EatNum; // discard a number, decimal, or hex
    begin
        With Buffer^ do
        begin
        Inc(InIndex);
        While TextUC[Inindex] in ['0','1','2','3','4','5',
              '6','7','8','9','A','B','C','D','E','F',
              '+','-','.'] DO
            INC(InIndex);
        end;
    end;

    // eject page on ^L if ok
    Procedure NewPage;
    begin
        with buffer^ do
        begin

        end;
    end;

    Procedure GetToken(Var Token: TokenType; Ident: UnicodeString);
    // Scan the source code, looking for symbols, keywords or identifiers
    Var
         Done: Boolean = False;
         CH,
         NextCH: WideChar;

         Procedure NewToken(T: TokenType);
         begin
             Token := T;
             Done := True;

         end;

         
     // collect an identifier
     Procedure MakeIdent;
     begin
         With Buffer^ do
         begin
            Ident := textUC[InIndex];
            Inc(InIndex);
             while TextUC[Inindex] in ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L',
                  'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X',
                  'Y', 'Z','_','0','1','2','3','4','5','6','7','8','9']  do
             begin
                 Ident := Ident + TextUC[Inindex];
                 Inc(InIndex);
             end;
         end
     end;



    Begin //*Procedure GetToken
     With Buffer^ do
     Repeat
         // when going thru line looking for a token,
         // we might run out of line, so we'd need to
         // read in the next line
         if InIndex > Len then
         begin
            CompletePrintLine;
            GetLine;
         end;
         CH := TextUC[InIndex];
         NextCh := TextUC[InIndex+1];
         IF CH in ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L',
                  'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X',
                  'Y', 'Z','_'] then
            begin
                Token := IDENTTOK;
                MakeIdent;
                done := True;
            end
         else
        // just scan for chars we're interested in
          Case CH of
            ' ', #0..#11,#13..#31: Continue;
            #12:  NewPage;  // Form Feed
            ')' : NewToken( CPARENTOK );
          //  '*’ : NewToken( MULTOK );
          //  '+' : NewToken( PLUSTOK );
            ',' : NewToken( COMMATOK );
          //  '-' : NewToken( MINUSTOK );
            '&': // octal number or protected ident
                  if Nextch in ['0','1','2','3','4','5','6','7'] then
                    begin
                        Inc(InIndex); // Step over &
                        EatNum;
                    end
                    else
                     if  Nextch in ['A', 'B', 'C', 'D', 'E', 'F',
                                    'G', 'H', 'I', 'J', 'K', 'L',
                                    'M', 'N', 'O', 'P', 'Q', 'R',
                                    'S', 'T', 'U', 'V', 'W', 'X',
                                    'Y', 'Z','_'] then
                     begin
                          Inc(InIndex); // Step over &
                          MakeIdent;    // collect ident
                          Token := NONKEYWORDTOK;  // protect from mistake as kw
                          Done := TRUE;      // exit loop
                     end;
            '/' : if NextCH='/' then // ignore / unless //
                     InIndex := Len; // Skip to end
            '#','0','1','2','3','4','5','6','7','8','9',
               '$' :  EatNum;
            ':' : IF NextCH <> '=' then
                     NewToken( COLONTOK )         //  :
                  ELSE
                     begin
                         Inc(InIndex);
                         NewToken( BECOMESTOK );  // :=
                     end;
            ';' : NewToken( SEMICOLONTOK );
        //  '@' : NewToken( ADDRESSTOK );
        //  '[' : NewToken( OBRACKETTOK );
        //  ']' : NewToken( CBRACKETTOK );
            '^' : NewToken( DEREFERENCETOK );
            '=' : NewToken( EQTOK );
            '(' : begin
                      if NextCH='.' then
                            Inc(InIndex)    // (. is old school [ ; ignore
                      else
                      if NextCH='*' then // opening of (* comment
                      begin
                           Inc(InIndex);
                           CheckComment('*)',2);
                      end
                      else
                           NewToken( OPARENTOK )
                  end;
            '.' : if NextCH='.' then
                      // begin
                     Inc(InIndex)   // skip first dot and ignore ..
                      // NewToken( RANGETOK );
                      // end
                  else
                     if NextCH=')' then  // .) is old school ]
                         Inc(InIndex)   // skip first dot and ignore
                     else
                         NewToken( PERIODTOK );

             '{' : begin
                       Inc(InIndex);
                           CheckComment('}',1);
                   end;
             '<' :  // swallow other symbols
                  if (NextCH = '>') or    // GT
                     (NextCH = '<') or       // SHL
                     (NextCH = '=') then     // LE
                          Inc(InIndex)
                  else
                     NewToken( LTTOK );

             '>' :  if  (NextCH = '>') or // SHR
                        (NextCH = '=') then // GE
                          Inc(InIndex)
                  else
                      NewToken( GTTOK );
        end; // end case
        Inc(InIndex);    // skip to next char

     until done or eof(F);
    end;  //*Procedure GetToken


   Procedure CloseUnit;
   Var
       Temp: InPtr;

   Begin //*Procedure CloseUnit
       with Buffer^ do
       begin

           Close(F);     // Close input file

            // close page in local lisring
            // close page in HTML
            // close page in PDF
            // if dezired, emit unit statistics
            // if desired, close page after statistics
            // if desired, begin local cross-reference
            // close local listing, HTML, PDF files

            // Finally, release Buffer and rollback
           Temp := Buffer;
           Buffer := Buffer^.Next;
           Dispose(Temp);


       end; // Procedure CloseUnit
   end;

   Procedure AllocateBuffer;
Var
    Temp: InPtr;

begin  //*Procedure AllocateBuffer
    If Buffer = NIL then // if this is the first unit
    begin
       New(Buffer);
       Buffer^.Next := NIL;
    end
    else
    begin // this is another unit
        New(Temp);
        Temp^.Next := Buffer;  // save the prior Buffer
        Buffer := Temp;
    end;
end;

// Given a unit name, begin processing
Function ProcessUnit(UnitName: UnicodeString):integer;
begin  //*Function ProcessUnit;

    Result := 0; // presume success
    AllocateBuffer;
    OpenUnitFile(UnitName);

    if Buffer^.Error <> 0 then
    begin
        Writeln('? Unit ',UnitName,' not found');
        Exit(2);     // caller will release buffer
    end
    else
    With Buffer^ do
    begin // Start the process
        If EOF( F ) then
        begin   // empty file
            Writeln('? Unit ',UnitName,' empty');
            Close(F);
            Exit(1);
        end;
        InitUnit;    //   Initialize this unit
    end;
end ;

   Procedure TestMode;
   VAR
        Tok: TokenType;
         Id: UnicodeString;
          I: Integer;

   begin
       Buffer := NIL;
       AllocateBuffer;
       ID := '';
    with buffer^ do
     begin

       OpenInputFile('testtest.pas');

       if error <>0 then
       begin
           Writeln('?Open Error ',error);
           Write('*36 Press Enter: '); readln;

           Exit;
       end;
       LineNumber := 0;
       I := 0;
       inindex := 1;
       While Not EOF(F) do
       begin
           inc(I);
           GetToken(Tok,ID);
           Write(Tok,' ');
           if ID<>'' then write('ID=',ID,' ');
           if I>15 then
           begin
               writeln;
               write(Linenumber:5,' ');
               I := 0;
           end;

       end;
       Writeln(' Completed, read ',LineNumber, ' lines');
       write('*33 ?'); readln;
       close(F);
    end;

   end;


end.

