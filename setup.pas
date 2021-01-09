unit setup;


interface

Uses Scan;

    Procedure Usage;

implementation


Procedure Usage;
begin
  if ParamCount < 1 then
  begin
      Writeln(ProgramName,' - Create cross-reference listing of a Pascal program');
      Writeln('Copyright 2021 Paul Robinson - Licensed under GPLv3.');
      Writeln;
      Writeln('   Usage:'+Programname+' <file1.pas> [<file2.pas> etc]');
      Writeln('      If you want to list units not used by a main program or units that ');
      Writeln('      it uses, directly or indirectly, list them first. The main prograam');
      Writeln('      should be last');
      Writeln('    OR');
      Writeln('   Usage: '+Programname+'  @<configurationfile.txt> [<file2.pas> etc]');
      Writeln('      The configuration file will declare options for the program and any independent units.');
      Writeln('      if the configuration file includes the main program (and any independent units),');
      Writeln('      then the source file need not be specified.');
      Halt(1);
  end;

  PasPath := ParamStr(1);
  SplitPath(PasPath,PasFolder,PasName,PasExt);

end;


end.

