// Examine a file and trace every proc or func
{$I CrossrefPrefixCode.inc}
{$ifdef mswindows}{$apptype console}{$endif}
{$mode ObjFPC}{$H+}
program ProcTrace;
uses
   windows, SysUtils;

Const
        // Standard months of year
     Months: array[1..12] of string[9]=
              ('January',   'February','March',    'April',
                'May',      'June',    'July',     'August',
                'September','October', 'November', 'December');

      // Standard days of week
      Days: Array[0..6] of string[9]=
              ('Sunday','Monday','Tuesday','Wednesday',
              'Thursday','Friday','Saturday');
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

     TracePtr = ^TraceTable;
     TraceTable = record
          Next: TracePtr;           // ptr to next entry
          ProcName: String;         // identifier of p/f
          InRecord: Boolean;        // Inside of record (we ignore case stmt)
          NestCount,                // how many nested procs
          BeginCount: Integer;      // how many begin stmts
     end;

VAR
      Infile,           //< Source file
      Outfile: text;    //< Output File

      EndTS,              //< Completion Time Stamp
      TS: SystemTime;     //< Start time stamp
      TimeStamp: String;  //< Text of time stamp  
      CH:WideChar;        //< char we're looking at
      InInterface,        //< are we in interface of unit
      ProcFlag: Boolean;  //< true if we just saw a proc/func header

      Ident,              //< Identifier


      Prefix,             //< Part of line before insertion
      Suffix,             //< Part of line adter insertion
      UCL: UnicodeString; //< Uppercase of text line

      Trace: TracePtr;

      FullName,    //< Bame of input file
      Backup,      //< Name of backup file
      Line:        //< A line read from the input file
      UnicodeString;

     LineNumber,   //< # of lines read
     Index,        //< currrent char in line
     Len: Integer; //< Length of input line
     IR: byte;     //< IOResult save






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

   Procedure Banner;
  begin

     Writeln('Proctrace - Examine Pascal source file for ',
                      'procedutes and functions and');
     writeln('mark them for tracing');
     writeln('Started: ',TimeStamp,', please wait...');
     If (Paramcount <1) or (Paramcount >2) or
         ((Paramcount =2) and (UpperCase(Paramstr(1))<>'-D')) then
         begin
             writeln;
             writeln('Usage:');
             writeln('proctrace [-d] <file.pas>');
             writeln;
             writeln('optional param -d to remove tracecode');
             writeln('(switch is not case sensitive)');
             writeln;
             write('Press enter to end program: ');
             readln;
             halt(99);
         end;
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

// try to put file back if there was am error
procedure putback;
begin // try to put file back
   close(infile);
   if renameFile(backup,FullName) then // we did
       Writeln('?Error ',IR,' Unable to read file "',
               FullName,'" file skipped')
   else                   // can't put it back
       Writeln('Unable to restore file "',
                FullName,'", renamed to ',Backup,
               '" file skipped');
end;  // procedure putback;

Procedure ReadLine;
begin
    WriteLn(OutFile,Line);
    if eof(Infile) then exit;
    Readln(Infile, Line);
    Inc(LineNumber);
    len := Length(Line);
    Index :=1;
    UCL := UpperCase(Line);
end;

Procedure HandleProcName;

begin
    if Trace = Nil then // it should have been set
    begin
        Writeln('Internal error 100: Trace Nil in Handleprocname');
        Writeln;
        writeln('Please report the above error.');
        writeln('Program cannot continue - Press Enter to end program.: ');
        readln;
        Halt(100);
    end;
    Trace^.ProcName := Trace^.ProcName+Ident;
end;

Procedure HandleInterface;

Begin
    InInterface := TRUE;

End;

Procedure  HandleImplementation;

Begin
      InInterface := FALSE;
End;

// procedure or funcrion header found
Procedure Procedural;
Var
    TraceHold: Traceptr;

Begin
    // Proc/func header in Interface is implidit forward
    // so ignore that one
    If not InInterface then
    begin
        ProcFlag := True;
        TraceHold := Trace;
        New(Trace);
        Trace^.Next := TraceHold;
        Trace^.BeginCount:= 0;
        Trace^.NestCount:= 0;
        Trace^.InRecord:= FALSE;
        Trace^.ProcName:= Ident +' '; // save 'procedure' or 'function'
    end;
End;

// forward or external mens this procedure or funcrion
// is not defined hertr, just declared, so ignore it
Procedure  HandleForExt;
// we don't count this header
Var
    TraceHold: Traceptr;

Begin
    TraceHold := Trace;
    iF Trace<>Nil then
    begin
      Trace := Trace^.Next;
      Dispose(Tracehold);
    end;

End;


Procedure  HandleBegin;

Begin

End;

Procedure  Structure;

Begin

End;

Procedure SpecialCase;

Begin

End;

Procedure  HandleEnd;

Begin

End;

Procedure Comment(Const C:string;const Size:Integer);
begin
    Inc(index);
    if index>Len then
        readLine;

    repeat  // find end of comment or end of line
      CH :=  UCL[Index];
      if (CH = C[1]) and (Size=1) then
      begin
          inc(index);
          exit;
      end;

      if (CH=C[1]) and (Index<Len) and
          (UCL[Index+1]=C[2]) then
      begin
          inc(index,2);
          exit;
      end;
      Inc(Index);
    until EOF(InFile);


end;


procedure ProcessFile;
var

   NumArgs: byte;
   DeleteMarks: boolean;

begin
    NumArgs := ParamCount;
    FullName := ParamStr(NumArgs);   //< Get the original name
    DeleteMarks := numArgs = 2;
    Backup   := FullName + '.bak';  //< Get the nackup name
    // rename to Fullname + .bak, e.g pascal.pas.bak
    if FileExists(Backup) then
    // erase old backup
       If not DeleteFile(Backup) then
       // Error deleting previous backup
       begin
           Writeln('Unable to delete backup of "',
           FullName,'"');
           writeln;
           write('Press enter to end program: ');
           readln;
           halt(2);
       end;
       // If we can't rename the file to the baxkup
       // name, that's an error too
       if not renameFile(FullName,backup) then
       begin
           // tell them cam't backup
           Writeln('Unable to backup file "',
                   FullName,'" ');
           writeln;
           write('Press enter to end program: ');
           readln;
           halt(4);
       end;
       // Open the source for reading
       Assign(Infile,Backup);
       FileMode := 0; // open input file read only
       {$I-} Reset(Infile); {$I+}
       IR := IOResult;  //< Check if error
       if IR<>0 then    //< sorry, error
       begin // try to put it back
           putback;
           writeln;
           write('Press enter to end program: ');
           readln;
           halt(10);
       end;
       // if we are here, start copying
       Assign(Outfile,Fullname); // name replacement
       {$I-} Rewrite(Outfile); {$I+}  // create replacement
       IR := IOResult;
       if IR<>0 then        // then there's an error
       begin // try to put it back
            putback;
            write('Press enter to end program: ');
            readln;
            halt(20);
       end;
       while not eof(infile) do
       begin
           readline;

           Index := 1;
           repeat
               CH := UCL[Index];
               If  (CH = '_') or
                  ((CH>='A') and (CH<='Z')) then
               begin                // identifier
                   Ident := '';
                   while  ((CH = '_') or
                   ((CH>='A') and (CH<='Z'))) and
                   (Index<=Len) do
                   begin
                       Ident := Ident + CH;
                       Inc(Index);
                       If Index<=Len then CH := UCL[Index];
                   end;
                   if ProcFlag then
                       HandleProcName
                   else
                   CASE ident OF
                       'INTERFACE': HandleInterface;
                       'FORWARD', 'EXTERNAL': HandleForExt;
                       'IMPLEMENTATION': HandleImplementation;
                       'PROCEDURE','FUNCTION' : Procedural;
                       'BEGIN': HandleBegin;
                       'RECORD': Structure;
                       'CASE': SpecialCase;
                       'END': HandleEnd;
                   end // case
               end
               else if CH='''' then     // quoted string
               begin
               // continue until end of string (or end of line)
               // which we treat as end of string
                   while  ((CH<>'''')  and
                           (Index<=Len)) do
                       Inc(Index);
                   inc(Index); // either step over ' or go past end of line
               end
               else if CH ='{' then Comment('}',1)  // Check for brace
               else if CH ='(' then  // check for block comment
               begin
                 If Index<Len then
                     If UCL[Index+1]='*' then
                     begin
                         Inc(Index);  // step over *
                         Comment('*)',2)
                     end
               end
               else if CH ='/' then   // check line or special block
               begin   // in case a line ends with . such as if a
                       // formula is broken over two lines
                   If Index<Len then
                      // stop if // found
                      If UCL[Index+1]='/' then
                          Index := Len      // skip rest of line
                      else if UCL[Index+1]='*' then
                      begin
                          Inc(Index);  // step over *
                          Comment('*/',2) // if C-style comments allowed
                      end
               end
               else If  (CH = '$') or
                  ((CH>='0') and (CH<='9')) then
               begin       // handle numbers
                   Inc(Index);
                   CH := UCL[Index];
                   while (((CH>='0') and (CH<='9')) or
                          ((CH>='A') and (CH<='F')) or // Hex
                           (CH='+')  or  (CH='-') or  // sign or decimal point
                           (CH='.')) and (Index<=Len) do
                       Inc(Index);
               end;




               inc(Index);
          until Index>Len;
          Readline;


       end;// while not eof
end;

begin  // .main.

    GetLocalTime(TS);
    TimeStamp := CTS(TS);

    Banner;
    Trace := NIL;
    ProcessFile;

    Close(OutFile);
    Close(InFile);
    writeln;
    GetLocalTime(EndTS);
    TimeStamp := CTS(EndTS);
    Writeln('Completed ',TimeStamp);
    Elapsed(TS,EndTS);
    writeln;
end.


