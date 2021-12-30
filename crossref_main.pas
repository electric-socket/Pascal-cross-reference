{$I CrossrefPrefixCode.inc}  // STD definitions
// Crossref - Pascal Cross reference
// Version 0.01
// January 9, 2021
// Copyright 2021 Paul Robinson
// Licensed under GPL v2

// Ctossref_Main - Defacto main program used for modularization
// Main Program - starts everything
unit Crossref_Main;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Notice, Command, scan, setup, tables;



  // Main toutine of program
  Procedure Main;


implementation

Procedure GetArgs;
var
    I: Integer;
    Param,
    Param1,
    ParAm2,
    Test1,
    Test2: UnicodeString;

begin

     if ParamCount = 0 then
     begin
            Param := '--help';
            Passthru "= TRUE;
     end;
     For I := 1 to Paramcount do
       begin
          if not passthru  then
          begin  // bypass first argument only
                 Param   := UnicodeString( ParamStr(I));  // collect each argument
                 PassThru := FALSE;
          end;
          Test1   := Copy(Param,1,1);  // used for all other switches
          Param1  := Copy(Param,2,Length(Param));
          Test2   := Copy(Param,1,2);  // used for -- switches
          Param2  := Copy(Param,3,Length(Param));

          If (Test1 in ['-','/','@']  then
          Case Test1 of
          '/',
          '-':  begin
                    If test2<>'--' then
                        ProcessSingleArgument(Param1)  // handle /x or -x
                    else
                        ProcessDoubleArgument(Param2);     // handle --x
                    continue;
                end;
          '@':  begin
                    ProcessCommandFile(Param1);
                    continue;
                end;
          end
          else
               ProcessFile(Param);
     end
end;

Procedure Main;
begin
    ProgramPath := UnicodeString(ParamStr(0));
    SplitPath(ProgramPath,ProgramFolder,ProgramName,ProgramExt);
    writeln('ProgramPath=', ProgramPath);
    writeln('ProgramFolder=', ProgramFolder,' ProgramName=',ProgramName,' ProgramExt=',ProgramExt);

    Banner;
    // check for switches ot commands

    GetArgs;
    // if the user issued a command that disallows processing, don't do it!
    if  not WillExit then
    begin
    Init;

{    OpenFile(PasPath);
    ScanFile;
    WriteFile;
}   end
{$IFDEF Test}
      ;
write('Press rnter: ');
     readln;
{$ELSE}
      else
      begin
          Writeln;
          Write('Press Enter key to end program;

      end;
{$ENDIF}
end;


end.

