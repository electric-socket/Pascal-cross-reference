program pascref;

uses SysUtils, Scan;


begin
    Usage;
    ProgramPath := ParamStr(0);
    SplitPath(ProgramPath, ProgramFolder, ProgramName, ProgramExt);

    ScanInit;


end.

