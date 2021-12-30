{$I CrossrefPrefixCode.inc}
{$ifdef mswindows}{$apptype console}{$endif}
program pullcond;


  // Simple, quick program to scan pascal programs for
  // compiler declarations and conditional statrments
  // any line containing either {$ or (*$ is captured
  //
  // Paul Robinson 2021-12-30

  {$mode ObjFPC}{$H+}
  uses
     windows, SysUtils;

  Const

      // These are the extensions we search
      Extensions: Array[1 .. 5] of String =(
      'pp',
      'pas',
      'inc',
      '',
      ''
      );

     // Standard months of year
     Months: array[1..12] of string[9]=
              ('January',   'February','March',    'April',
                'May',      'June',    'July',     'August',
                'September','October', 'November', 'December');

      // Standard days of week
      Days: Array[0..6] of string[9]=
              ('Sunday','Monday','Tuesday','Wednesday',
              'Thursday','Friday','Saturday');

      path = '*';             //< file names to pick up
      Attr = faAnyFile  ;     //< take any file name
      SlashChar = '\';        //< directory separator




  Type
  {$IFDEF Bits64}
      LargeInt = Int64;
  {$ELSE}
  {$IFDEF Bits32}
      LargeInt = Integer;
  {$ELSE}
  {$PATAL Must define BITS64 or BITS32}
  {$ENDIF}
  {$ENDIF}
  Var

      foundlines,       //< Total sentinel lines written
      IR,               //< IOResult save
      GlobalFileCount,  //< Files processed for running count
      FoundCount,       //< Number of files found with sentinel
      DirCount:Integer; //< Directories Found

      Infile,           //< Source file
      Outfile: text;    //< Output File

      EndTS,             //< Completion Time Stamp
      TS: SystemTime;    //< Start time stamp
      TimeStamp: String; //< Text of time stamp



      // Converts a file name into directory, name, extension.
      // This removes the dot in the extension. To keep it,
      // remove the // in front of Ext := and insert it in
      // the line after it
      procedure SplitPath(const Path: UnicodeString; var Folder,
                                                         Name,
                                                         Ext: UnicodeString);
      var

              i,                //< Loop counter for location of separators
          DotPos,               //< Posiyion of . in filename
          SlashPos: Integer;    //< Pos. of directory separator

      begin
          Folder := '';
          Name := Path;
          Ext := '';

          DotPos := 0;
          SlashPos := 0;

          for i := Length(Path) downto 1 do
              if (Path[i] = '.') and (DotPos = 0) then
                  DotPos := i
              else if (Path[i] = SlashChar) and (SlashPos = 0) then
                  SlashPos := i;

          if DotPos > 0 then
          // The file name contains a period
          begin
              Name := Copy(Path, 1, DotPos - 1);
      //        Ext  := LowerCase(Copy(Path, DotPos, Length(Path) - DotPos + 1));
              Ext  := LowerCase(Copy(Path, DotPos+1, Length(Path) - DotPos + 1));
          end;

          if SlashPos > 0 then
          // The file name contains a directory separator character
          begin
              Folder := Copy(Path, 1, SlashPos);
              Name   := Copy(Path, SlashPos + 1, Length(Name) - SlashPos);
          end;

      end;

  // creates a text timestamp
    Function CTS(Const CTime:SystemTime): AnsiString;
  begin
     Result := Days[CTime.dayOfWeek]+
               ' '+Months[CTime.month]+
               ' '+IntToStr(CTime.day)+
               ', '+IntToStr(CTime.year)+
               ' '+IntToStr(CTime.Hour)+
               ':';
     if CTime.Minute < 10 then Result := Result+'0';
     Result := Result+ IntToStr(CTime.Minute)+':';
     if CTime.Second < 10 then Result := Result+'0';
     Result := Result+ IntToStr(CTime.Second);
  end;





  Function Comma(K:LargeInt):string;
  var
     i:integer;
     s: string;
  begin
      S := IntToStr(K);
      i := length(s)-3;
      while i>0 do
      begin
          S := Copy(S,1,i) +','+copy(s,i+1,length(s));
          I := I-3;
      end;
      Result := S;
  end;

  Function Plural(N:Integer; Plu:String; Sng: String): string;
  Var
     s:String;
  Begin
      S := IntToStr(N);
      S := ' '+S+' ';
      If n<>1 Then
          Result:= S+ Plu
       Else
          Result := S + Sng;
  End;  // Function Plural





    // Recursively scan directories
    Procedure ScanFiles(
                Const Prefix: UnicodeString);  //< from where are we searching?
    var
        WasFound: Boolean; // sentinel found in file
       Rslt: TUnicodeSearchRec;    //< since this proc is recursive,
                                   //< this must be local


        i,          //< Loop counter for # of extensions processed
       LineCount: Integer;
       FullName,     //< Full name of input file

       Line,         //< A line read from the input file
       TheFilePath,  //< File name split into path
       TheNameOnly,  //< File Name w/o extenion
       TheExtension: UnicodeString; //< File Name extension

       // Determine if a file is a directory
       Function isDirectory: boolean;
       begin
         result := (rslt.Attr And faDirectory) = faDirectory;
       end; // Function isDirectory


    // the real "meat" of this program. Recursively scan all directories
    // to find the files having the extensions we use
    begin //< procedure scanfiles

        // Each time we enter this procedure, we have found another directory
        Inc(DirCount);

        // Open the directory and get first file
        // this will probably be . or ..
        If FindFirst(Prefix+Path,Attr,rslt) = 0 Then
        // If there are any files, pick them
        Repeat
            // skip parent directory and self
            If (rslt.Name = '.') Or  (rslt.Name = '..') Then
                continue;

            if isDirectory then     //< don't collect directory
                                    //< but do scan it
            begin
                   ScanFiles(prefix+rslt.Name+SlashChar)  //< recursive search
            end
            else    // NOT a directory
            begin   // split the file name into components
               SplitPath(rslt.name,TheFilePath,TheNameOnly,TheExtension);

               // search the array of preferred extensions
               For I := 1 to 5 do
                 if Extensions[I]<>'' then   //< check this extension
                   If theExtension = UnicodeString( Extensions[I] ) then  //< found it
                   begin           //< We do want this one
                       WasFound := False;
                       Linecount := 0;
                       FullName := Prefix+rslt.name;   //< Get the original name
                       FileMode := 0; // open input file read only
                       // if we are here, start reading and potentially copying
                       Assign(Infile,Fullname); // the file
                       {$I-} Reset(Infile); {$I+}
                       IR := IOResult;
                       if IR<>0 then        // then there's an error
                       Begin
                            writeln;writeln('?Err opening "',fullname,'", skipped');
                            break;
                       end;


                       // now we can read the file
                       while not eof(infile) do
                       begin
                           Readln(Infile, Line);
                           inc(LineCount);
                           if (Pos('{$',Line)>0) or
                              (Pos('(*$',Line)>0) then
                           begin
                               If not wasfound then
                               begin
                                   Inc(foundcount);
                       Writeln(Outfile); // blank line before each file
                                   Writeln(Outfile,Fullname,':');
                                   wasfound := true;
                               end;
                               Inc(FoundLines);
                               Writeln(outfile,Comma(linecount):8,' ',Line);

                           end;
                       end;
                       Close(Infile);
                       inc(GlobalFileCount);
                       Write('           ',#13,globalFileCount,#13);
                       break
                   end;  // If TheExtension

            end;
         // get the next file
         Until FindNext(rslt) <> 0;   // If, Repeat
        FindClose(rslt);   // close directory
     end;  //< procedure scanfiles

  Procedure Banner;
  begin

     Writeln('Pullcond - Examine Pascal source files for compiler conditionals');
     writeln('Preparing to read all .pas. .pp, and .inc files in this ');
     Writeln('directory and all subdirectories for a $ comment.');
     Writeln('Output sent to Pullcond.lst');
     writeln('Started: ',TimeStamp,', please wait...');
     Assign(Outfile,'pullcond.lst');
     Rewrite(Outfile);
     Writeln(OutFile,'Pullcond scan of Pascal files in ',GetCurrentDir);
     Writeln(OutFile,' as of: ',TimeStamp);


  end;  // rocedure Banner

  Procedure Elapsed(CONST StartTime,EndTime: SystemTime);
  Var
     H,M,S,MS: Integer;
     TimeString: String;

  Begin
      // Now tell them how long it took
      // Presumably this program did not run for days...

      H :=  EndTime.Hour;
      if StartTime.Hour < EndTime.Hour  then
          h:=H + 24;
      h := h - StartTime.Hour;
      M := EndTime.Minute ;
      if M < StartTime.minute then
      begin
          H := H-1;
          M := M+60;
      end;
      M := M - StartTime.minute;
      S := EndTime.second  ;
      if S < StartTime.second then
      BEGIN
          M := M-1;
          S := S+60;
      END;
      S := S-StartTime.second;
      MS := EndTime.MilliSecond;
      IF MS < StartTime.MilliSecond then
      begin
          MS := MS+1000;
          S := S-1;
      end;
      MS := MS-StartTime.MilliSecond;

  // we won't bother with days,
  // nobody is going to process something taking that long (famous last words)

      TimeString := '';   // Make sure it has nothing left over
      If H >0 then
          Timestring := Plural(H,'hours','hour')+' ';
      If M >0 then
          Timestring := TimeString + Plural(M,'minutes','minute')+' ';
      if timestring <> '' then
          Timestring := Timestring +' and ';
      Timestring := TimeString + IntToStr(S)+'.' + IntToStr(MS)+' seconds.';
      Writeln('Elapsed time: ',TimeString)
  end;  // Procedure Elapsed


  begin   // .Main.
        TS.Year:=0; EndTS.Month:= 0 ;      //< silence compiler warning
                                           //< about uninitialized variables
        GetLocalTime(TS);
        TimeStamp := CTS(TS);
        Banner;
        writeln;
        ScanFiles(''); // Start Here

        writeln;
        Close(OutFile);
        GetLocalTime(EndTS);
        TimeStamp := CTS(EndTS);
        Writeln('Completed ',TimeStamp);
        writeln('Processed ',Comma(GlobalFileCount),' files.');
        Writeln('Detected: ',Comma(FoundCount),' files with sentinel');
        writeln('Found: ',Comma(FoundLines),' lines.');
        Elapsed(TS,EndTS);
        writeln;



  end.     // .Main.

