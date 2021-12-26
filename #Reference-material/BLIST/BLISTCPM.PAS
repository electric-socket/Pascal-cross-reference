PROGRAM BList;

{ TURBO PASCAL SOURCE CODE LISTER AND BEGIN-END COUNTER PROGRAM }

{ Prints a listing to console or printer of a TURBO PASCAL source code with
  optional display of comment counter and begin/end counter, and also optional
  display skip of paper perforations.  Accepts file name passed by CP/M or
  from operator input of file to list. }

{ This version of the code is specific to CP/M-80 because the GET_IN_FILE
  procedure looks for a parameter passed by CP/M at absolute location $80. The
  procedure could be modified for other operating systems, or not to accept
  passed parameters at all. }

{ I declare that this code is released to the PUBLIC DOMAIN as of July 1, 1984
                                       Phillip M. Nickell                     }

{ Modified Sept. 1, 1984 by Marvin Landis
  Record/end combination is now handled correctly. }

VAR Buff1: STRING[135];                       { Input line buffer }
    ListFil: TEXT;                            { Fib for LST: or CON: output }
    InFile: TEXT;                             { Fib for input file }
    BCount,KCount,LineCt: INTEGER;            { Counters }
    Count_Be,PerfSkip: BOOLEAN;               { Count begin/end and skip }

CONST First: BOOLEAN = TRUE;                  { True when program is run }

{ To customize code for your printer and desires - adjust the next two items }

      MaxLine = 60;      { max # of lines on page when in PERFSKIP mode }
      SkipLine = 2;      { # of lines to skip at top of form }

      CR = #13;
      LF = #10;
      FF = #12;

Procedure Clean;      { Clears screen and positions cursor }
BEGIN
  ClrScr;
  GoToXY(1,10);
END;

Procedure Lines(X: Integer);   { Puts X amount of blank lines to output file }
Var N: Integer;
BEGIN
  For N:= 1 To X Do
    Writeln(ListFil);
END;

{ GET_IN_FILE PROCEDURE : When program is first run, it will check for a file
  name passed by CP/M and will try to open that file.  If no name is passed,
  it will ask operator for a file name to open.  Proc will tell operator if
  file doesn't exist and will allow multiple retrys.  On second and later
  executions, proc will not check for CP/M passed file name.  In all cases
  proc will assume a file type of .PAS if file type is not specified.  Exit
  from the program occurs when a null string is entered in response to a file
  name request. }

Procedure Get_In_File;                 { Gets input file name }
Var FNam: String[14];                  { Input file name }
    Parm: String[14] Absolute $81;     { Passed file name if any }
    ParmLth: Byte Absolute $80;        { CP/M passed length of Parm }
    Existing: Boolean;
BEGIN
  Repeat                               { Until file exists }
    If (ParmLth In [1..14]) And First Then
      FNam:= Copy(Parm,1,ParmLth - 1)
    Else Begin
      Clean;
      Write('Enter file name to list or <CR> to exit: ');
      Readln(FNam);
    End;
    If FNam = '' Then Halt;
    If Pos('.',FNam) = 0 Then
      FNam:= Concat(FNam,'.PAS');      { File default to .PAS type }
    First:= False;
    Assign(InFile,FNam);
    {$I-}
    Reset(InFile);
    {$I+}
    Existing:= (IOResult = 0);
    If Not Existing Then Begin
      Clean;
      Writeln('File does not exist.');
      Delay(700);
    End;
  Until Existing;
END;   { Get_In_File }

{ GET_OUT_FILE procedure : Asks operator to select output to console device
  or list device, and then assigns and resets a file control block to the
  appropriate device.  'C' or 'P' are the only correct responses, and
  multiple retrys are allowed. }

Procedure Get_Out_File;
Var C: Char;
BEGIN
  Repeat                            { Until good selection }
    Clean;
    Write('Output listing to (C)onsole or (P)rinter? ');
    C:= UpCase(Chr(BDos(1)));
  Until C In ['C','P'];
  Writeln;
  If C = 'C' Then
    Assign(ListFil,'CON:')
  Else
    Assign(ListFil,'LST:');
  Reset(ListFil);
END;  { Get_Out_File }

{ GET_OPTIONS procedure : Asks operator if count of begin/end pairs is desired,
  and also if skip over paper perforations is desired.  Proc will set or clear
  the Count_Be flag and the PerfSkip flag. }

Procedure Get_Options;
Var C: Char;
BEGIN
  Repeat
    Clean;
    Write('Count of BEGIN/END pairs (Y/N)? ');
    C:= UpCase(CHR(BDOS(1)));
  Until C In ['Y','N'];
  If C = 'Y' Then Count_Be:= True
  Else Count_Be:= False;
  Repeat
    Clean;
    Write('Skip printer perforations (Y/N)? ');
    C:= UpCase(Chr(BDOS(1)));
  Until C In ['Y','N'];
  If C = 'Y' Then PerfSkip:= True
  Else PerfSkip:= False;
END;  { Get_Options }

{ SCAN_LINE procedure : Scans one line of Turbo Pascal source code looking
  for begin/end pairs, case/end pairs, literal fields and comment fields.
  BCount is begin/end and case/end counter.  KCount is comment counter.
  Begin/case/ends are only valid outside of comment fields and literal
  constant fields (KCount = 0 and NOT LITERAL).  Some of the code in this
  procedure appears at first glance to be repetitive and/or redundant, but
  was added to speed up the process of scanning each line of source code.
  The program now spits out listings much faster than a 160 cps printer. }

Procedure Scan_Line;
Var Literal: Boolean;               { True if in literal field }
    Tmp: String[8];                 { Temp work area }
    Buff2: String[135];             { Working line buffer }
    I: Integer;
BEGIN
  Literal:= False;
  Buff2[0]:= Buff1[0];      { Copy input buffer into working buffer }
  For I:= 1 to Length(Buff1) Do
    Buff2[I]:= UpCase(Buff1[I]);
  Buff2:= Concat(' ',Buff2,'       ');   { Add on some working space }
  For I:= 1 to Length(Buff2) - 7 Do Begin
    Tmp:= Copy(Buff2,I,8);
    If Not Literal Then Begin
      If Tmp[1] In ['{','}','(','*'] Then Begin
        If (Tmp[1] = '{') Or (Copy(Tmp,1,2) = '(*') Then
          KCount:= Succ(KCount);
        If (Tmp[1] = '}') Or (Copy(Tmp,1,2) = '*)') then
          KCount:= Pred(KCount);
      End;
    End;
    If KCount = 0 Then Begin
      If Tmp[1] = Chr(39) Then Literal:= Not Literal;
      If Not Literal And (Tmp[2] In ['B','C','E','R']) Then Begin
        If (Copy(Tmp,1,7) = ' BEGIN ') Or (Copy(Tmp,1,6) = ' CASE ') Or
        (Tmp = ' RECORD ') Then Begin
          BCount:= Succ(BCount);
          I:= I + 5;
        End;
        If (Copy(Tmp,1,4) = ' END') And (Tmp[5] In ['.',' ',';']) Then Begin
          BCount:= Pred(BCount);
          I:= I + 4;
        End;
      End;
    End;
  End;
END;  { Scan_Line }

BEGIN
  Repeat                     { Forever }
    Get_In_File;
    Get_Out_File;
    Get_Options;
    Lines(1);
    Linect:= 1;
    If Count_Be Then Begin
      KCount:= 0;
      BCount:= 0;
      Writeln(ListFil,' C  B');
    End;
    While Not EOF(InFile) Do Begin
      Readln(InFile,Buff1);
      If Count_Be Then Begin
        Scan_Line;
        Writeln(ListFil,KCount:2,BCount:3,'  ',Buff1);
      End Else
        Writeln(ListFil,Buff1);
      If PerfSkip Then Begin
        LineCt:= Succ(LineCt);
        If LineCt > MaxLine Then Begin
          Write(ListFil,FF);
          Lines(SkipLine);
          LineCt:= 1;
          If Count_Be Then Writeln(ListFil,' C  B');
        End;
      End;
    End;
    Write(CR,LF,'Hit any key to continue...');
    BCount:= BDOS(1);
  Until False;     { Exit is in Get_In_File procedure }
END.

