unit Crossref_Main;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Windows, scan, setup;



  // Main toutine of program
  Procedure Main;


implementation
var

    StartTime,               //< time program started
    EndTime: SystemTime;     //< for elapsed time


Procedure Usage;
begin
  if ParamCount < 1 then
  begin
      Writeln('Crossref ',Version,' - Create cross-reference listing of a Pascal program');
      Writeln(Copyright,' - Licensed under GPLv2.');
      Writeln;
      Writeln('   Usage: ',PasPath,' <options> ');
      Writeln;
      WriteLn('   Examples:');
      Writeln('Usage: ',Programname,' [/switch | -switch | --switch ] ',
              '<file1>| <file2> etc]');
      Writeln('      Optional switches must come before any file nams)');
      Writeln('      <file1>|<file2> etc]');
      writeln('      If you want to use a configuration file, ',
              'prefix the name with @');
      writeln('      @configurationfile, if used, must appear first, ',
              'after any switch(es), if used');
      Writeln('      Any source or template files must come after the ',
              'configuration file, if used');
      Writeln;
      Writeln('      If you want to include units not used by a main program ',
              'or units that');
      Writeln('      the main program uses, directly or indirectly, list ',
              'them first.');
              Writeln('      The main prograam (or primary/controlling unit ',
              'if no main program');
      Writeln('is specified) should be last');
      Writeln;
      Writeln('      The configuration file will declare options for the ',
              'program and any independent units.');
      Writeln('      if the configuration file includes or invokes the main ',
              'program (and any independent units),');
      Writeln('      then the source file need not be specified ',
              'on the command line.');
      Writeln;
      Writeln('For a list of switches, use /sw -sw or --switches');
      writeln('switches are not case senitive.');
      Halt(1)
  end
end;

Procedure Banner;
Begin
    Writeln(ProgramName,' - Create cross-reference listing of ',
                          'a Pascal program', Version);
    Writeln(Copyright,' - Released under GPL ver. 2');
    Starttime.Year:=0; EndTime.Year:=0;    // eliminate uninitialized warning
    GetLocalTime(StartTime);
    Write('Good ');
    If StartTime.Hour <12 then
        Write('morning')
    else if StartTime.Hour <18 then
        Write('afternoon')
    else
        Write('evening');
{ temp dike
    writeln(', it is now ', CTS(StartTime));
    Writeln('Current directory ',GetCurrentDir);
    Writeln;
}
end;

// handle /x or -x
Procedure ProcessSingleArgument(const Arg:UnicodeString);
Var
    UCArg:UnicodeString;

begin
    UCArg := Uppercase(Arg);
    if (UCarg='H') then Usage;


end;



// handle --xxx-xxx or --xx=
Procedure ProcessDoubleArgument(const Arg:UnicodeString);
Var
   UCArg:UnicodeString;

begin
    UCArg := Uppercase(Arg);
    if (UCArg='HELP') then usage;


end;


Function ElapsedTime(Const StartTime, EndTime:SystemTime):String;
VAR
    H,M,S,MS: Integer;

Begin
   // Now tell them how long it took

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
// nobody is going to process something taking that long

    If H >0 then
        Result  := Plural(H,'hours','hour')+' ';
    If M >0 then

        Result  := Result  + Plural(M,'minutes','minute')+' ';
    if Result  <> '' then
        Result  := Result  +' and';
    Result  := Result  + IntToStr(S)+'.' + IntToStr(MS)+' seconds.';

end;

Procedure GetArgs;
var
    I: Integer;
    Param,
    Parm1,
    Test1,
    Test2: AnsiString;

begin

     For I := 1 to Paramcount do
       begin
          Param := ParamStr(I);  // collect each argument
          Test1 := Copy(Param,1,1);  // used for all other switches
          Test2 := Copy(Paspath,1,2);  // used for -- switches

          If (Test1 ='-') or (Test1 ='/') then
          begin
              If test2<>'--' then
              Begin
                  ProcessSingleArgument(Test1); // handle /x or -x
                  ProcessDoubleArgument(Test2);     // handle --x
              end;
              // not a switch, collect file name

          end
     end
end;

Procedure Main;
begin
    PasPath := ParamStr(0);
    SplitPath(PasPath,PasFolder,PasName,PasExt);

{
    Banner;
    // check for switches ot commands
    if paramcount <1 then
        Usage;  // does not return
    GetArgs;
}
    Init;

{    OpenFile(PasPath);
    ScanFile;
    WriteFile;
}
     write('Press rnter: ');
     readln;

end;


end.

