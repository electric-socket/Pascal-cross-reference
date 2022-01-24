{$I CrossrefPrefixCode.inc}  // STD definitions
// Crossref - Pascal Cross reference
// Version 0.06
// January 9, 2021
// Copyright 2021 Paul Robinson
// Licensed under GPL v2

// Command.pas - Commans File Processor
unit Command;

{$mode ObjFPC}{$H+}

interface

uses   Notice, Tables,SysUtils, Scan;
Var
    Quit: Boolean = False;
    WillExit: Boolean = False;         //< Exit without acting
    WillHold: Boolean = False;         //< pause before exit
    WillDryRun: Boolean = False;         //< test w/o acting
    WillDumpStats: Boolean = False;       //< dump statistics




// handle -x or -x= commands
Procedure ProcessSingleArgument(const S:UnicodeString);
// handle --x or  --x= commands
Procedure ProcessDoubleArgument(const S:UnicodeString);
// handle @command file
Procedure ProcessCommandFile(Const S: UnicodeString);
// handle source file
Procedure ProcessSourceFile(Const S: UnicodeString);
// Message if no args
Procedure Usage;


implementation


Procedure Usage;
begin
  if ParamCount < 1 then
  begin
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
      writeln('      File names containing spaces must be quoted, @',
               QuoteChar,'like this',QuoteChar,' or -u=',QuoteChar,
               'this way',QuoteChar);
      Writeln;
      Writeln('      If you want to include units not used by a main program ',
              'or units that');
      Writeln('      the main program uses, directly or indirectly, list ',
              'them first.');
      Writeln('      The main prograam (or primary/controlling unit ',
              'if no main program');
      Writeln('      is specified) should be the last file name on ',
              ' the command line.');
      Writeln;
      Writeln('      The configuration file will declare options for the ',
              'program and any independent units.');
      Writeln('      if the configuration file includes or invokes the main ',
              'program (and any independent units),');
      Writeln('      then the source file need not be specified ',
              'on the command line.');
      Writeln;
      Writeln('For a list of switches, use the /sw -sw or --switches command.');
      writeln('Switches are not case senitive.');
      {$IFDEF test}
      writeln;write('** Press Enter; '); Readln;
      {$ENDIF}
      exit;

          // The following DEAD CODE is INTRNTIONALLY used to eliminate
          // spurious compiler warning messages
      Starttime.Year:=0;
      EndTime.Year:=0;
      Starttime.Year := EndTime.Year;
      EndTime.Year := Starttime.Year ;

  end
end;


// Temporary proc to test --dryrun
Procedure DryRun;
begin
    Writeln('Run in dry (no action) mode');
    WillDryRun := TRUE;
    WillDumpStats := TRUE;
end;


// handle /x  -x /x= -x= /sa /sa= -sa -sa=
Procedure ProcessSingleArgument(const S:UnicodeString);
Var
    Len,
    Eq: Integer;
    Val,
    Cmd,
    UCArg:UnicodeString;

begin
    UCArg := Uppercase(S);
    Len := Length(S);
    Eq := Pos('=',UCArg);
    If Eq > 0 then
    begin
       Val := Copy(S,Eq+1,Len);
       Cmd := Copy(UCArg,1,Eq-1);
    end
    else
    begin
      Val := '';
      Cmd :=UCArg;
    end;

    CASE UCArg of
       'H','HELP','INFO': begin usage; WillExit := TRUE; end;
       'XYXXY': Writeln('Nothing happens.');
       'HOLD', 'STOP': WillHold := TRUE;
       'DRY','DRYRUN','DRY-RUN' : Dryrun;
    else
       Writeln('?Imvalid argument "',S,'" ignoted');
    end

end;


// handle --xxx --xxx-x --xxx-x= or --xx=
Procedure ProcessDoubleArgument(const S:UnicodeString);
Var
    Len,                  //< param (S) length
    Eq: Integer;          //< location of = in param
    Val,                  //< if = in command, value after it
    Cmd,                  //< command as given
    UCArg:UnicodeString;  //< command in UPPER CASE

begin
    UCArg := Uppercase(S);
    Len := Length(S);
    Eq := Pos('=',UCArg);
    If Eq > 0 then
    begin
       Val := Copy(S,Eq+1,Len);
       Cmd := Copy(UCArg,1,Eq-1);
    end
    else
    begin
      Val := '';
      Cmd :=UCArg;
    end;

    CASE UCArg of
        'H','HELP','INFO': begin usage; WillExit := TRUE; end;
        'XYXXY': Writeln('Nothing happens.');
        'HOLD','STOP' : WillHold := TRUE;
        'DRY','DRYRUN','DRY-RUN' : Dryrun;
    else
       Writeln('?Imvalid argument "',S,'" ignored');
    end

end;

// handle @command file
Procedure ProcessCommandFile(Const S: UnicodeString);
var
     Temp: InPtr;

begin
    If Buffer = NIL then // if this is the first unit
        New(Buffer)
    else
    begin // this is another file
        New(Temp);
        Temp^.Next := Buffer;  // save the prior Buffer
        Buffer := Temp;
    end;


end;

// handle source file
Procedure ProcessSourceFile(Const S: UnicodeString);
begin


end;


END.

