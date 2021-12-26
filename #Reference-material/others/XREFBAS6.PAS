PROGRAM Xrefbas6;

 {  Cross-reference a BASIC program saved in ASCII }
 {  Original BASIC-80 version by Advanced Informatics, 1980 }
 {  MODIFIED FOR IBM PC BY STEVE NOSSEN V1.10 1/13/82  }
 {  MODIFIED BY Buzz Hamilton V1.2 12/2/82       }
 {  TRANSLATED to Turbo Pascal by David W. Carroll, V2.0, 6/7/86 }
 {  Copyright 1986 by David W. Carroll. }
 {  Released for NON-COMMERCIAL use only, all other rights reserved. }

 {  This and over 1500 other Turbo Pascal programs are available }
 {  on the High Sierra RBBS-PC 24 hours a day at 209/296-3534    }

 {RESERVED WORDS}

  CONST
    Linewidth = 79;
    Spc = ' ';
    Maxreswords = 157;
    Reswords: ARRAY [1..Maxreswords] OF STRING [8] =
    ('ABS', 'AND', 'APPEND', 'ASC', 'AS', 'ATN', 'AUTO', 'BEEP',
    'BLOAD', 'BSAVE', 'CALL', 'CDBL', 'CHAIN', 'CHR$', 'CINT', 'CIRCLE',
    'CLEAR', 'CLOSE', 'CLS', 'COLOR ', 'COMMON', 'COM', 'CONT', 'COS', 'CSNC',
    'CSRLIN', 'CVD', 'CVI', 'CVS', 'DATA', 'DATE$', 'DEFDBL', 'DEFINT',
    'DEFSNG', 'DEFSTR', 'DEFUSR', 'DEF', 'DELETE', 'DIM', 'DRAW', 'EDIT',
    'ELSE', 'END', 'EOF', 'EQV', 'ERASE', 'ERL', 'ERROR', 'ERR', 'EXP',
    'FIELD', 'FILES', 'FIX', 'FOR', 'FRE', 'GET', 'GOSUB', 'GOTO', 'HEX$',
    'IF', 'IMP', 'INKEY$', 'INPUT$', 'INPUT', 'INP', 'INSTR', 'INT', 'KEY',
    'KILL', 'LEFT$', 'LEN', 'LET', 'LINE', 'LIST', 'LLIST', 'LOAD', 'LOCATE',
    'LOC', 'LOF', 'LOG', 'LPOS', 'LPRINT', 'LSET', 'MERGE', 'MID$', 'MKD$',
    'MKI$', 'MKS$', 'MOD', 'MOTOR', 'NAME', 'NEW', 'NEXT', 'NOT', 'OCT$',
    'OFF', 'ON', 'OPEN', 'OPTION', 'OR', 'OUT', 'PAINT', 'PEEK', 'PEN', 'PLAY',
    'POINT', 'POKE', 'POS', 'PRESET', 'PRINT', 'PSET', 'PUT', 'RANDOMIZE',
    'READ', 'REM', 'RENUM', 'RESET', 'RESTORE', 'RESUME', 'RETURN', 'RIGHT$',
    'RND', 'RSET', 'RUN', 'SAVE', 'SCREEN', 'SGN', 'SIN', 'SOUND', 'SPACE$',
    'SPC(', 'SQR', 'STEP', 'STICK', 'STOP', 'STR$', 'STRIG', 'STRING$', 'SWAP',
    'SYSTEM', 'TAB(', 'TAN', 'THEN', 'TIME$', 'TO', 'TROFF', 'TRON', 'USING',
    'USR', 'VAL', 'VARPTR', 'WAIT', 'WEND', 'WHILE', 'WIDTH', 'WRITE', 'XOR');

  TYPE
    Longstr = STRING [132];
    Datstr = STRING [25];

  VAR
    Infile, Outfile: Text;
    Infname, Outfname: Datstr;
    Nocnt, Q, Err, V, P, C, Value, M, I, J: Integer;
    Ls: Longstr;
    Ers, Rword, Vsx: Longstr;
    Ch1: STRING [1];
    Ch: Char;
    Ok: Boolean;
    Lineno, Lc, Bc: Real;
    Il, Rz, Lp, Brnch, Lg, Lz, X, Y, Pz, Vc, Rc: Integer;

    Pt: ARRAY [0..25] OF Integer;
    Vnxt: ARRAY [0..490] OF Integer;
    Vs: ARRAY [0..490] OF STRING [15];
    Frst, Last: ARRAY [0..400] OF Integer;
    Rfl, Nxt: ARRAY [0..2000] OF Integer;


  PROCEDURE Uppercase(VAR Str: Datstr);

    VAR
      Indx, Len: Integer;

    BEGIN
      Len := Length(Str);
      FOR Indx := 1 TO Len DO
        Str[Indx] := Upcase(Str[Indx])
    END;


  FUNCTION Instr(Start: Integer;
                 A, B: Longstr): Integer;

    VAR
      Loc: Integer;
    BEGIN
      Loc := Pos(B, Copy(A, Start, Length(A)));
      IF Loc > 0 THEN
        Loc := Loc + (Start - 1);
      Instr := Loc;
    END;


  PROCEDURE Newpage;
    BEGIN
      IF (Pz > 0) OR (M > 1) THEN
        Writeln(Outfile, Chr(12));
      Pz := Pz + 1;
      Writeln(Outfile, Infname, Spc: 55, 'PAGE # ', Pz);
      Writeln(Outfile);
      Writeln(Outfile);
      Lz := 3;
    END;


  PROCEDURE PrintHeader;

    VAR
      I: Integer;
    BEGIN
      Newpage;
      Writeln(Outfile, 'SYMBOL', Spc: 16, 'REFERENCE LINE');
      FOR I := 1 TO 40 DO
        Write(Outfile, '-');
      Writeln(Outfile);
      Writeln(Outfile);
      Lz := Lz + 1;
    END;

  { Print Program Listing to Outfile }


  PROCEDURE PrintListing;
    BEGIN
      X := 1;
      IF (Lz > 60) THEN
        Newpage;
      WHILE X <= Length(Ls) DO
        BEGIN
          Writeln(Outfile, Copy(Ls, X, Linewidth));
          Lz := Lz + 1;
          X := X + Linewidth;
        END;
    END;

  { END VARIABLE }


  PROCEDURE EndVar;

    LABEL
      1260, 1280, 1300, 1310;

    VAR
      Temp: STRING [15];
    BEGIN
      IF Vsx = '' THEN
        Exit;
      IF Vsx >= 'A' THEN
        BEGIN
          Vsx := Vsx + Ers;
          C := Ord(Vsx[1]) + 1;
        END
      ELSE IF Vsx >= '' THEN
        BEGIN
          WHILE Length(Vsx) < 5 DO
            Vsx := ' ' + Vsx;
          Val(Copy(Vsx, 1, 2), C, Err);
        END
      ELSE
        GOTO 1310;
      Il := - 1;
      I := C;
    1260:
      IF Vsx > Vs[I] THEN
        BEGIN
          Il := I;
          I := Vnxt[I];
          IF I > 0 THEN
            GOTO 1260
          ELSE
            GOTO 1280;
        END;
      IF Vsx = Vs[I] THEN
        BEGIN
          J := Last[I - 91];
          IF Rfl[J] = Lineno THEN
            GOTO 1310
          ELSE
            BEGIN
              Rc := Rc + 1;
              Nxt[J] := Rc;
              GOTO 1300;
            END;
        END;
    1280:
      Vc := Vc + 1;
      IF Il >= 0 THEN
        Vnxt[Il] := Vc;
      Vs[Vc] := Vsx;
      Vnxt[Vc] := I;
      Rc := Rc + 1;
      Frst[Vc - 91] := Rc;
      I := Vc;
    1300:
      Rfl[Rc] := Trunc(Lineno);
      Nxt[Rc] := - 1;
      Last[I - 91] := Rc;
    1310:
      Vsx := '';
    END;

  { LIST VARIABLES }


  PROCEDURE ListXrf;

    LABEL
      1480;

    VAR
      Q, Vlen: Integer;

    BEGIN
      IF M <> 2 THEN
        BEGIN
          Pz := 0;
          Lz := 60;
          FOR J := 0 TO 91 DO
            BEGIN
              V := J;
              V := Vnxt[V];
              WHILE V >= 0 DO
                BEGIN
                  IF Lz > 56 THEN
                    PrintHeader;
                  Rz := 0;
                  I := Frst[V - 91];
                  Writeln(Outfile);
                  Write(Outfile, Vs[V]);
                  Vlen := Length(Vs[V]);
                1480:
                  IF Rz = 0 THEN
                    Write(Outfile, Spc: (21 - Vlen));
                  Lineno := Rfl[I];
                  IF Lineno < 0 THEN
                    Lineno := Lineno + 65536.0;
                  Write(Outfile, Lineno: 5: 0, '  ');
                  Rz := Rz + 1;
                  IF Rz > 6 THEN
                    BEGIN
                      Rz := 0;
                      Writeln(Outfile);
                      Vlen := 0;
                      Lz := Lz + 1;
                      IF Lz > 56 THEN
                        PrintHeader;
                    END;
                  I := Nxt[I];
                  IF I > 0 THEN
                    GOTO 1480;
                  IF Rz > 0 THEN
                    BEGIN
                      Writeln(Outfile);
                      Lz := Lz + 1;
                    END;
                  V := Vnxt[V];
                END;
            END;

          FOR Q := 1 TO 79 DO
            Write(Outfile, '=');
          Writeln(Outfile);
          Write(Outfile, 'LINES: ', Lc: 5: 0, '    BYTES: ', Bc: 6: 0);
          Writeln(Outfile, '     SYMBOLS: ', Vc - 91, '    REFERENCES: ',
                  Rc + 1);
          Writeln;
          Writeln;
          Write('LINES: ', Lc: 5: 0, '    BYTES: ', Bc: 6: 0);
          Writeln('     SYMBOLS: ', Vc - 91, '    REFERENCES: ', Rc + 1);

          Writeln;
          Writeln('- XREF DONE - ');
        END
      ELSE
        BEGIN
          Writeln;
          Writeln('- LISTING DONE - ');
          Writeln(Outfile);
          Writeln(Outfile, '    - End Of File -');
        END;

      Writeln(Outfile);
      Close(Outfile);
      Lz := Lz + 2;
    END;


  FUNCTION Openfiles: Boolean;

    CONST
      Bell = 07;

    VAR
      Ch: Char;
      Goodfile, Quit: Boolean;

    BEGIN
      REPEAT
        Quit := False;
        Write('Enter input file: ');
        Readln(Infname);
        Uppercase(Infname);
        Assign(Infile, Infname);
        {$I-}
        Reset(Infile) {$I+} ;
        Goodfile := (Ioresult = 0);
        IF NOT Goodfile THEN
          BEGIN
            Write(Chr(Bell));
            Writeln('FILE ', Infname, ' NOT FOUND');
            Write('Quit? (Y/N) ');
            REPEAT
              Read(Kbd, Ch);
              Ch := Upcase(Ch);
            UNTIL Ch IN ['Y', 'N'];
            Writeln(Ch);
            Quit := Ch = 'Y';
          END;
      UNTIL Goodfile OR Quit;
      IF NOT Quit THEN
        BEGIN
          REPEAT
            Quit := False;
            Write('Enter output file: ');
            Readln(Outfname);
            Uppercase(Outfname);
            Assign(Outfile, Outfname);
            {$I-}
            Reset(Outfile) {$I+} ;
            Goodfile := (Ioresult <> 0);
            IF NOT Goodfile THEN
              BEGIN
                Write(Chr(Bell));
                Write('FILE ', Outfname, ' EXISTS, OVERWRITE? (y/n) ');
                REPEAT
                  Read(Kbd, Ch);
                  Ch := Upcase(Ch);
                UNTIL Ch IN ['Y', 'N'];
                Writeln(Ch);
                Goodfile := Ch = 'Y';
                IF NOT Goodfile THEN
                  BEGIN
                    Write('Quit? (Y/N) ');
                    REPEAT
                      Read(Kbd, Ch);
                      Ch := Upcase(Ch);
                    UNTIL Ch IN ['Y', 'N'];
                    Writeln(Ch);
                    Quit := Ch = 'Y';
                  END;
              END;
          UNTIL Goodfile OR Quit;
          IF NOT Quit THEN
            Rewrite(Outfile);
        END;
      Openfiles := NOT Quit;
    END; {open_files}

  PROCEDURE Getfiles;
   BEGIN
    CASE Paramcount OF
      0:
        IF NOT Openfiles THEN
          Halt;
      1:
        BEGIN
          Infname := Paramstr(1);
          Uppercase(Infname);
          IF Pos('.', Infname) = 0 THEN
            BEGIN
              Outfname := Infname + '.XRF';
              Infname := Infname + '.ASC';
            END
          ELSE
            Outfname := Copy(Infname, 1, Pos('.', Infname) - 1) + '.XRF';
          Assign(Infile, Infname);
          {$I-}
          Reset(Infile);
          {$I+}
          Ok := Ioresult = 0;
          IF NOT Ok THEN
            BEGIN
              Writeln('File ', Infname, ' not found.');
              IF NOT Openfiles THEN
                Halt;
            END
          ELSE
            BEGIN
              Assign(Outfile, Outfname);
              Rewrite(Outfile);
            END;
        END;
      2:
        BEGIN
          Infname := Paramstr(1);
          Uppercase(Infname);
          IF Pos('.', Infname) = 0 THEN
            Infname := Infname + '.ASC';
          Outfname := Paramstr(2);
          Uppercase(Outfname);
          IF Pos('.', Outfname) = 0 THEN
            Outfname := Outfname + '.XRF';
          Assign(Infile, Infname);
          {$I-}
          Reset(Infile);
          {$I+}
          Ok := Ioresult = 0;
          IF NOT Ok THEN
            BEGIN
              Writeln('File ', Infname, ' not found.');
              IF NOT Openfiles THEN
                Halt;
            END
          ELSE
            BEGIN
              Assign(Outfile, Outfname);
              Rewrite(Outfile);
            END;
        END;
      ELSE
        Writeln('Parameter error - ');
        Writeln('Use:  XREFBAS INFILE[.ASC] [OUTFILE.XRF] ');
        Writeln('Halting...');
        Halt;
      END;
   END;

  PROCEDURE Process;

    LABEL
      880, 960, 1100, 1110, 1370;
    BEGIN

      {INITIALIZE FOR CROSS REFERENCE}

      Lc := 0;
      Bc := 0;
      Pz := 0;
      P := 1;
      C := 0;
      Vsx := '';
      Ch1 := '';
      Vc := 91;
      Nocnt := 0;
      Rc := - 1;
      FOR I := 0 TO 91 DO
        Vnxt[I] := - 1;
      IF M > 1 THEN
        Newpage;

      { INPUT LINE AND EXTRACT LINE NUMBER }

    880:
      IF Eof(Infile) THEN
        BEGIN
          Close(Infile);
          ListXrf;
          Halt;
        END;
      Readln(Infile, Ls);
      IF M > 1 THEN
        BEGIN
          PrintListing;
          IF M = 2 THEN
            GOTO 880;
        END;
      Lg := Length(Ls);
      Brnch := 0;

      Ers := '';
      Nocnt := Nocnt + 1;
      Lc := Lc + 1;
      Bc := Bc + Lg;
      Lp := Pos(' ', Ls);
      Val(Copy(Ls, 1, Lp) + '.0', Lineno, Err);
      Write(Lineno: 5: 0, '  ');
      IF Lineno > 32767 THEN
        Lineno := Lineno - 65536.0;
      IF Nocnt > 9 THEN
        BEGIN
          Nocnt := 0;
          Writeln;
        END;

      { PARSE REST OF LINE }

    960:
      Lp := Lp + 1;
      IF Lp > Lg THEN
        BEGIN
          EndVar;
          GOTO 880;
        END;
      Ch1 := Copy(Ls, Lp, 1);
      IF (Ch1 >= 'A') AND (Ch1 <= 'Z') THEN
        GOTO 1100
      ELSE IF ((Ch1 >= '0') AND (Ch1 <= '9')) OR (Ch1 = '.') THEN
        GOTO 1370;
      IF Ch1 = ' ' THEN
        BEGIN
          EndVar;
          GOTO 960;
        END
      ELSE IF Ch1 <> ',' THEN
        Brnch := 0;
      IF Ch1 = Chr(34) THEN
        BEGIN
          EndVar;
          Lp := Instr(Lp + 1, Ls, Ch1);
          IF Lp > 0 THEN
            GOTO 960
          ELSE
            GOTO 880;
        END;
      IF Ch1 = '''' THEN
        BEGIN
          EndVar;
          GOTO 880;
        END;
      IF Ch1 = '&' THEN
        BEGIN
          EndVar;
          Vsx := Ch1;
          GOTO 880;
        END;
      Ch := Ch1;
      IF (Ch IN ['$', '!', '%', '#']) THEN
        BEGIN
          IF Vsx <> '' THEN
            Vsx := Vsx + Ch1;
          GOTO 960;
        END;
      IF Ch1 = '(' THEN
        BEGIN
          IF Vsx <> '' THEN
            Vsx := Vsx + Ch1;
        END;
      EndVar;

      IF Ch1 <> ',' THEN
        Ers := '';
      GOTO 960;

      {TEST FOR COMMAND}

    1100:
      IF Vsx > '' THEN
        BEGIN
          Vsx := Vsx + Ch1;
          GOTO 960;
        END
      ELSE
        BEGIN
          C := Ord(Ch1);
          P := Pt[C - Ord('A')];
          Brnch := 0;
        END;
    1110:
      IF C < Ord(Reswords[P][1]) THEN
        BEGIN
          Vsx := Vsx + Ch1;
          GOTO 960;
        END;
      IF Instr(Lp, Ls, Reswords[P]) <> Lp THEN
        BEGIN
          P := P + 1;
          GOTO 1110;
        END;
      EndVar;
      Rword := Reswords[P];
      IF Rword = 'DATA' THEN
        BEGIN
          Lp := Instr(Lp, Ls, ':');
          IF Lp > 0 THEN
            GOTO 960
          ELSE
            GOTO 880;
        END;
      IF Rword = 'REM' THEN
        GOTO 880;
      IF (Rword = 'GOTO') OR (Rword = 'GOSUB') OR (Rword = 'THEN') THEN
        Brnch := 1;
      IF (Rword = 'ELSE') OR (Rword = 'RESUME') THEN
        Brnch := 1;
      IF Rword = 'ERASE' THEN
        Ers := '('
      ELSE
        Ers := '';
      Lp := Lp + Length(Rword) - 1;
      GOTO 960;
    1370:
      IF (Vsx = '') AND (Brnch = 0) THEN
        GOTO 960;
      Vsx := Vsx + Ch1;
      GOTO 960;

    END;  { PROCESS }

  BEGIN   { MAIN }
    Clrscr;
    Writeln('BASIC CROSS-REFERENCE ');
    Writeln('COPYRIGHT (C) 1986 BY David W. Carroll');
    Writeln('V2.0 6/7/86');

    { Initialize arrays }

    FOR I := 0 TO 25 DO
      Pt[I] := 0;

    FOR I := 0 TO 490 DO
      Vnxt[I] := 0;

    FOR I := 0 TO 400 DO
      BEGIN
        Frst[I] := 0;
        Last[I] := 0;
      END;

    FOR I := 0 TO 2000 DO
      BEGIN
        Rfl[I] := 0;
        Nxt[I] := 0;
      END;

    FOR I := 0 TO 490 DO
      Vs[I] := '';

    FOR I := 1 TO Maxreswords DO
      BEGIN
        Value := Ord(Reswords[I][1]) - Ord('A');
        IF Pt[Value] = 0 THEN
          Pt[Value] := I;
      END;

    FOR I := 0 TO 25 DO
      IF Pt[I] = 0 THEN
        Pt[I] := Maxreswords;

    { GET FILE NAMES }
    Getfiles;

    Writeln;
    Writeln;
    Writeln('Input file:  ', Infname);
    Writeln('Output file: ', Outfname);
    Writeln;
    Writeln;
    Writeln('         MENU');
    Writeln('========================');
    Writeln('  1.  Crossref Program');
    Writeln('  2.  List Program');
    Writeln('  3.  Do Both');
    Writeln('  4.  Quit');
    Writeln;
    REPEAT
      Write('  Enter choice: ');
      Readln(M);
    UNTIL M IN [1..4];
    IF M = 4 THEN
      Halt;
    Clrscr;
    Writeln('Processing program ', Infname, ' ... ');
    Writeln;
    Process;

  END.
