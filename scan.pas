// Crossref - Pascal Cross reference
// Version 0.01
// January 9, 2021
// Copyright 2021 Paul Robinson

// Licensed under GPL v2

// Scan unit
// Scan the source file, processing it for keywords and identifiers,
// responding to both, then copying the file to the output file



unit scan;

interface
type
       String2= String[2];

var

        StartTime,                      // time when program began
        EndTime:String;
        // file descriptor for this program
        ProgramPath,                    // FQFN of nsme up to folder
        ProgramFolder,                  // folser it's in, null if root
        ProgramName,                    // name excluding extension
        ProgramExt,                     // This program's extension if any
        PasPath,                        // filename being processed
        PasFolder,
        PasName,
        PasExt,
        ErrorMessage: UnicodeString;    // error message text

   Procedure Catastrophic(Msg:UnicodeString);
   procedure SplitPath(const Path: UnicodeString; var Folder, Name, Ext: UnicodeString);
//   Procedure OpenFile(Name:String);
//   procedure ScanFile;



implementation
Const
   FileLimit = 20;   // maximum number of simultaneously open files



type

    TBuffer = record
       Line,                       // line being read
       FileName: UnicodeString;    // name of this file
       LineNumbet,                 // line number we're at
       LinePosition: Integer;      // current position on line
       Size: Int64;                // file size
       F: Text;                    // file pointer
    end;



var
  CrLf: String[2];
  buffer: Array[1..filelimit] of TBuffer;
  CurrentFile: 1..Filelimit ;


  Procedure Catastrophic(Msg:UnicodeString);
  begin
      Writeln;
      Writeln(Msg);
      halt(16);
  end;

{
  Procedure OpenFile(Name:String);
  VAR
      E,
      IR,
      ActualSize: Integer;
      Found: Boolean;

  begin // we need to use file folders per unitpath
      if currentFile < Filelimit then
      begin
// this needs to be replacwd with a folder search
          inc(CurrentFile);
          Assign(Buffer[CurrentFile].F, Name );
          {$I-}
          Reset(Buffer[CurrentFile].F);
          {$I+}
          IR := IOResult
          if IR <> 0 then
          begin
//     here, we'll move to next folder unless we run out of folders
//     ro search
          end;
// if we still have a failure, bsil
          if IR <>9 then
          begin
              Writeln('?Cannot open ',Name);
              writeln('** Skipping file');
              Dec(CurrentFile);
              Exit;
          end;
          with Buffer[Currentfile) do
          begin
              Size := FileSize(F);
              if size = 0 then
              begin
                  Writeln('** Skipping empty file ',Name);
                  Close(F);
                  Dec(CurrentFile);
                  exit;
              end;
              LineNumber := 1;
              LinePosition := 1;
              Line := '';
         end;
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

  }

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


end.

