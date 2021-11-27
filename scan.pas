// Crossref - Pascal Cross reference
// Version 0.01
// January 9, 2021
// Copyright 2021 Paul Robinson

// Licensed under GPL v3 or at your option any later version

// Scan unit
// Scan the source file, copying it to the output file



unit scan;

interface
type
       String2= String[2];

var

        TimeString,
        ProgramPath,
        ProgramFolder,
        ProgramName,
        ProgramExt,
        PasPath,
        PasFolder,
        PasName,
        PasExt:       String;

   Procedure Catastrophic(Msg:String);
   Procedure Init;
   procedure SplitPath(const Path: String; var Folder, Name, Ext: String);
   Procedure OpenFile(Name:String);
   procedure ScanFile;
   Procedure Writefile;


implementation
Const
    BufferSize = 102400; // 100K

type
    PCharacter = ^Char;
    TBuffer = record
       inPtr,
       OutPtr: PCharacter;
       InFile,
       OutFile: String;
       Ch: Char;
       Line,
       InPosition,
       OutPosition,
       InSize,
       OutSize,
       inPos,
       OutPos: Integer;
       EndOfFile: Boolean;
    end;



var
  F: File;
  CrLf: String[2];
  buffer: TBuffer;


  Procedure Catastrophic(Msg:String);
  begin
      Writeln;
      Writeln(Msg);
      halt(16);
  end;



  Procedure WriteChar(C:Char);
  begin
     with buffer do
     begin
         PCharacter(Integer(outPtr) + outPos)^ := C;
         inc(outPos);
     end;
  end;

  Procedure WriteString(S:string);
  Var L: Integer;

  Begin
     with buffer do
     For L := 1 to Length(S) do
     begin
         PCharacter(Integer(outPtr) + outPos)^ := S[L];
         inc(outPos);
     end;
  end;


  procedure NewLine;
  begin
      WriteString( CrLf);
  end;

  Procedure NextCh;
  begin
      with Buffer do
      begin
          if ch=#10 then
          begin
              Inc(line);
              NewLine;
              InPosition  := 0;
              OutPosition := 0;
          end;
          ch := #0;
          if inPos <= inSize then
          begin
              ch := PCharacter(Integer(inPtr) + inPos)^;
              Inc(inPos);
              Inc(InPosition);
              if (Ch=#13) then exit;
              PCharacter(Integer(outPtr) + outPos)^ := Ch;
              inc(outPos);
              inc(OutPosition);
          end
       else
          EndOfFile:= TRUE;
          if outposition <>0 then
            newline;
        end;
    end;


  Procedure OpenFile(Name:String);
  VAR
      E,
      ActualSize: Integer;
      Found: Boolean;


  begin
       Assign(F, Name );
       Reset(F, 1);
       if IOResult <> 0 then
           Catastrophic('Fatal: Unable to open source file "' + Name+'".');
       with Buffer do
       begin
           InSize := FileSize(F);
           Line := 1;
           inPosition := 0;
           OutPosition:= 0;
           inPos := 0;
           OutPos := 0;
           GetMem(inPtr, inSize );
           OutSize := Insize*2;
           ActualSize := 0;
           BlockRead(F, inPtr^, inSize , ActualSize);
           Close(F);
           if ActualSize <> inSize then
              Catastrophic('Fatal: Unable to read source file ' + Name);
           // Since we don't know how big the output file will be,
           // just use double the size of the input file
           GetMem(OutPtr,OutSize);
           E := GetLastError;
           if e<>0 then
           Writeln(' Win Err ',E);
           ch  := ' ';
           EndOfFile := FALSE;
      end;
  end;

  Procedure Writefile;
  Var
      K,W: Integer;
  begin
     Writeln('Writing: ',PasFolder+PasName+'.lst');
     Assign(F,PasFolder+PasName+'.lst');
     Rewrite(F);
     K := IOResult;
     W := WinIOResult;
     if (k<>0) or (W<>0)  then
      begin
      writeln('Error ',K,' or ',W);
      halt(99);
      end;

     with buffer do
          BlockWrite(F, OutPtr^,OutPos);
     Close(F);
     Writeln('Completed.');
  end;



procedure SplitPath(const Path: String; var Folder, Name, Ext: String);
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




  procedure ScanFile;
  begin
       repeat
            NextCh;
       until Buffer.EndOfFile;
  end;


  Procedure Init;
  begin
      CrLf := #13;
      CrLf := CrLf+#10;
      Buffer.Ch := ' ';
      ProgramPath := ParamStr(0);
      SplitPath(ProgramPath, ProgramFolder, ProgramName, ProgramExt);


  end;



end.

