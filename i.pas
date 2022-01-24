
// Examine a file and trace every proc or func
{$I CrossrefPrefixCode.inc}
{$ifdef mswindows}{$apptype console}{$endif}
{$mode ObjFPC}{$H+}
program ProcTrace;
uses
   windows, SysUtils;

Const
        // Standard months of year
     Months: arr