PROGRAM w_xref;

{$N-  Don't use the numeric coprocessor.}

{    N.WIRTH  10.2.76
     Cross Reference Generator for Pascal Programs,  using  the
     Quadratic Quotient hash method.

     Note:  This program was received with the  PASCAL 6000-3.4
            compiler for the Control Data 6600 computer written
            by Urs Ammann  of  the  Institut  fuer  Informatik,
            ETH-Zurich, Switzerland 1972-1974.

     Updated for Turbo Pascal by Harry M. Murphy,  4 June 1986.

     Updated for Turbo Pascal Version 4.0 on 23 November 1987.

     Keylength expanded from 10 to 15 and keywords expanded to
     include Borland's  Turbo Pascal  keywords  by  H.M.M.  on
     17 January 1987.  }

USES
      CRT;

CONST
      DGPN   =  6;       { No. of digits per number }
      EMPTY  = '               ';  { KLN blanks.    }
      KLN    = 15;       { Keylength                }
      LPPG   = 55;       { No. of lines per page    }
      LLDF   = 78;       { Default line length      }
      P      = 1499;     { Size of hash table       }
      NK     = 213;      { No. of keywords          }
      VERSION = '23 November 1987.';

TYPE
     ALFA     = ARRAY [1..KLN] OF CHAR;
     FILESPEC = STRING[65];
     FILESTAT = RECORD
                  FILECH: CHAR;
                  FILEEF: BOOLEAN;
                  FILEEL: BOOLEAN
                END;
     INDEX    = 0..P;
     REF      = ^ITEM;
     WORD     = RECORD
                  KEY: ALFA;
                  FIRST: REF;
                END;
     ITEM     = RECORD
                  LNO: 0..32767;
                  NEXT: REF
                END;

VAR
    CC     : INTEGER;     { POSITION OF CHARACTER IN LINE }
    FF     : CHAR;        { FORM-FEED CHARACTER }
    I      : INDEX;
    INP    : TEXT;        { INPUT FILE }
    INPCH  : CHAR;
    INPEF  : BOOLEAN;
    INPEL  : BOOLEAN;
    INPNAM : FILESPEC;    { INPUT FILE NAME }
    INPSTAT: FILESTAT;
    LL1    : INTEGER;     { LENGTH OF LISTING LINE }
    LL2    : INTEGER;     { LENGTH OF TABLE LINE   }
    K      : INTEGER;
    M      : INTEGER;     { NO. OF LINES ON PAGE }
    N      : INTEGER;     { CURRENT LINE NUMBER }
    NOPL   : INTEGER;     { NO. OF LINE-NUMBERS PER LINE }
    OUT    : TEXT;        { OUTPUT FILE }
    OUTNAM : FILESPEC;    { OUTPUT FILE NAME }
    QUOTE  : CHAR;        { SINGLE QUOTE MARK }
    ID     : RECORD
               CASE BOOLEAN OF
                 FALSE: (A: ALFA);
                 TRUE:  (ORD: INTEGER)
               END;
    T      : ARRAY [INDEX] OF WORD;    { HASH TABLE }
    KEY    : ARRAY [1..NK] OF ALFA;


PROCEDURE GETINP;

{  This routine emulates the GET(INP) file primitive,  which  puts
   the next input character in INP^, and which in not available in
   Borland's Turbo Pascal.

   Following a call to GETINP, INPCH contains the next input char-
   acter,  INPEF contains the EOF(INP) status and  INPEL  contains
   the EOLN(INP) status.

   Routine by Harry M. Murphy,  4 June 1986.  }

BEGIN
  WITH INPSTAT DO
    BEGIN
      INPCH := FILECH;
      INPEF := FILEEF;
      INPEL := FILEEL;
      FILECH := ' ';
      FILEEF := EOF(INP);
      FILEEL := EOLN(INP);
      IF NOT FILEEF
        THEN
          IF FILEEL
            THEN
              READLN(INP)
            ELSE
              READ(INP,FILECH)
    END
END  { Procedure GETINP };


PROCEDURE GETINPFIL(VAR INPNAME: FILESPEC);

{  This file gets an input file, either as the first parameter
   on the command line or by requesting it from the user.

   Procedure by Harry M. Murphy,  22 February 1986.  }

  VAR
      L: INTEGER;

  BEGIN
    IF PARAMCOUNT = 0
      THEN
        BEGIN
          WRITE('Input  file: ');
          READLN(INPNAME)
        END
      ELSE
        INPNAME := PARAMSTR(1);
    FOR L:=1 TO LENGTH(INPNAME) DO INPNAME[L] := UPCASE(INPNAME[L]);
    ASSIGN(INP,INPNAME);
    {$I-} RESET(INP) {$I+};
    IF IORESULT <> 0
      THEN
        BEGIN
          WRITELN('ERROR!  Can''t open file ',INPNAME,'!');
          HALT
        END;
  END {Procedure GETINPFIL};


PROCEDURE GETOUTFIL(VAR OUTNAME: FILESPEC);

{  This file gets an output file, either as the second parameter
   on the command line or by requesting it from the user.

   Procedure by Harry M. Murphy,  22 February 1986.  }

 VAR
     L: INTEGER;

  BEGIN
    IF PARAMCOUNT < 2
      THEN
        BEGIN
          WRITE('Output file: ');
          READLN(OUTNAME)
        END
      ELSE
        OUTNAME := PARAMSTR(2);
    FOR L:=1 TO LENGTH(OUTNAME) DO OUTNAME[L] := UPCASE(OUTNAME[L]);
    ASSIGN(OUT,OUTNAME);
    {$I-} REWRITE(OUT) {$I-};
    IF IORESULT <> 0
      THEN
        BEGIN
          WRITELN('ERROR!  Can''t open ',OUTNAME,'!');
          HALT
        END
  END {Procedure GETOUTFIL};


  FUNCTION NOKEY: BOOLEAN;

  VAR
      I,J,K: INTEGER;

   BEGIN
     I := 1;
     J := NK;
      REPEAT
        K := (I+J) DIV 2;
        IF KEY[K] <= ID.A
          THEN
            I := SUCC(K)
          ELSE
            J := K-1
      UNTIL I > J;
     IF J = 0
       THEN
         NOKEY := TRUE
       ELSE
         NOKEY := KEY[J] <> ID.A
   END { NOKEY };


  PROCEDURE EPILOG;

  BEGIN
    WRITELN(OUT);
    WRITELN(OUT,
    'This is a Turbo Pascal version of Niklaus Wirth''s Cross Reference'
    );
    WRITELN(OUT,
     'Generator for Pascal Programs,  using the Quadratic Quotient Hash'
    );
    WRITELN(OUT,'Method, dated 10.2.76.  This version date is ',VERSION)
 END { Procedure EPILOG };


  PROCEDURE COUNTLINE;

   BEGIN
     IF M = LPPG
       THEN
         BEGIN
           WRITELN(OUT,FF);
           M := 0
         END;
     M := M+1
   END { COUNTLINE };

  PROCEDURE OPENLINE;

   BEGIN
     COUNTLINE;
     N := N+1;
     WRITE(OUT,N:6,' ');
     CC := 0
   END { OPENLINE };

  PROCEDURE ADVANCE;

   BEGIN
     WRITE(OUT,INPCH);
     GETINP;
     CC := SUCC(CC);
     IF CC = LL1
       THEN
         WHILE NOT INPEL DO
           BEGIN
             WRITE(OUT,INPCH);
             GETINP
           END
   END { ADVANCE };

  PROCEDURE CLOSELINE;

   BEGIN
     WRITELN(OUT);
     GETINP
   END { CLOSELINE };

  PROCEDURE SEARCH;

  VAR
      H,D,I: INDEX;
      X: REF;
      F: BOOLEAN;

   BEGIN
     H := (ID.ORD DIV 4096) MOD P;
     F := FALSE;
     D := 1;
     NEW(X);
     X^.LNO := N;
      REPEAT
        IF T[H].KEY = ID.A
          THEN
            BEGIN { FOUND }
              F := TRUE;
              X^.NEXT := T[H].FIRST;
              T[H].FIRST := X;
            END
          ELSE
            IF T[H].KEY = EMPTY
              THEN
                BEGIN { NEW ENTRY }
                  F := TRUE;
                  T[H].KEY := ID.A;
                  T[H].FIRST := X;
                  X^.NEXT := NIL
                END
              ELSE
                BEGIN { COLLISION }
                  H := H+D;
                  D := D+2;
                  IF H >= P THEN H := H-P;
                  IF D = P
                    THEN
                      BEGIN
                        WRITELN(OUT);
                        WRITELN(OUT,' TABLE FULL');
                        HALT
                      END
                END
      UNTIL F
   END { SEARCH };

  PROCEDURE SORT(L,R: INTEGER);   { QUICKSORT }

  VAR
      I,J: INTEGER;
      X: ALFA;
      W: WORD;

   BEGIN
     I := L;
     J := R;
     X := T[(I+J) DIV 2].KEY;
      REPEAT
        WHILE T[I].KEY < X DO I := SUCC(I);
        WHILE X < T[J].KEY DO J := PRED(J);
        IF I <= J
          THEN
            BEGIN
              W := T[I];
              T[I] := T[J];
              T[J] := W;
              I := I+1;
              J := J-1
            END
      UNTIL I > J;
     IF L < J THEN SORT(L,J);
     IF I < R THEN SORT(I,R)
   END { SORT };

  PROCEDURE PRINTWORD(W: WORD);

  VAR
      L     : INTEGER;
      X,Y,Z : REF;

   BEGIN
     COUNTLINE;
     WRITE(OUT,' ',W.KEY);
     X := W.FIRST;
     Y := X^.NEXT;
     X^.NEXT := NIL;
     WHILE Y <> NIL DO
       BEGIN
         Z := Y^.NEXT;
         Y^.NEXT := X;
         X := Y;
         Y := Z
       END;
     L := 0;
      REPEAT
        IF L = NOPL
          THEN
            BEGIN
              L := 0;
              WRITELN(OUT);
              COUNTLINE;
              WRITE(OUT,' ',EMPTY)
            END;
        L := L+1;
        WRITE(OUT,X^.LNO:DGPN);
        X := X^.NEXT
      UNTIL X = NIL;
     WRITELN(OUT)
   END { PRINTWORD };

  PROCEDURE PRINTTABLE;

  VAR
      I,M: INDEX;

   BEGIN
     M := 0;    { COMPRESS TABLE }
     FOR I:=0 TO P-1 DO
       IF T[I].KEY <> EMPTY
         THEN
           BEGIN
             T[M] := T[I];
             M := M+1
           END;
     IF M > 0 THEN SORT(0,M-1);
     NOPL := (LL2-KLN-1) DIV DGPN;
     FOR I:=0 TO M-1 DO PRINTWORD(T[I])
   END { PRINTTABLE };

 BEGIN
   WRITELN('Wirth''s - Pascal Cross-Reference.  (10.2.76)');
   WRITELN('Turbo Pascal Version of ',VERSION);
   WRITELN;
   GETINPFIL(INPNAM);
   GETOUTFIL(OUTNAM);

   WITH INPSTAT DO
     BEGIN
       FILECH := ' ';
       FILEEF := FALSE;
       FILEEL := FALSE
     END;
   GETINP;
   GETINP;

   WRITELN('XREF now processing ',INPNAM);

   FOR I:=0 TO P-1 DO T[I].KEY := EMPTY;
   KEY[  1] := 'ABS            ';
   KEY[  2] := 'ABSOLUTE       ';
   KEY[  3] := 'ADDR           ';
   KEY[  4] := 'AND            ';
   KEY[  5] := 'ARC            ';
   KEY[  6] := 'ARCTAN         ';
   KEY[  7] := 'ARRAY          ';
   KEY[  8] := 'ASSIGN         ';
   KEY[  9] := 'AUX            ';
   KEY[ 10] := 'AUXINPTR       ';
   KEY[ 11] := 'AUXOUTPTR      ';
   KEY[ 12] := 'BACK           ';
   KEY[ 13] := 'BEGIN          ';
   KEY[ 14] := 'BLOCKREAD      ';
   KEY[ 15] := 'BLOCKWRITE     ';
   KEY[ 16] := 'BOOLEAN        ';
   KEY[ 17] := 'BYTE           ';
   KEY[ 18] := 'CASE           ';
   KEY[ 19] := 'CHAIN          ';
   KEY[ 20] := 'CHAR           ';
   KEY[ 21] := 'CHDIR          ';
   KEY[ 22] := 'CHR            ';
   KEY[ 23] := 'CIRCLE         ';
   KEY[ 24] := 'CLEARSCREEN    ';
   KEY[ 25] := 'CLOSE          ';
   KEY[ 26] := 'CLREOL         ';
   KEY[ 27] := 'CLRSCR         ';
   KEY[ 28] := 'COLORTABLE     ';
   KEY[ 29] := 'CON            ';
   KEY[ 30] := 'CONCAT         ';
   KEY[ 31] := 'CONINPTR       ';
   KEY[ 32] := 'CONOUTPTR      ';
   KEY[ 33] := 'CONST          ';
   KEY[ 34] := 'CONSTPTR       ';
   KEY[ 35] := 'COPY           ';
   KEY[ 36] := 'COS            ';
   KEY[ 37] := 'CRTEXIT        ';
   KEY[ 38] := 'CRTINIT        ';
   KEY[ 39] := 'CSEG           ';
   KEY[ 40] := 'DELAY          ';
   KEY[ 41] := 'DELETE         ';
   KEY[ 42] := 'DELLINE        ';
   KEY[ 43] := 'DISPOSE        ';
   KEY[ 44] := 'DIV            ';
   KEY[ 45] := 'DO             ';
   KEY[ 46] := 'DOWNTO         ';
   KEY[ 47] := 'DRAW           ';
   KEY[ 48] := 'ELSE           ';
   KEY[ 49] := 'END            ';
   KEY[ 50] := 'EOF            ';
   KEY[ 51] := 'EOLN           ';
   KEY[ 52] := 'ERASE          ';
   KEY[ 53] := 'EXECUTE        ';
   KEY[ 54] := 'EXP            ';
   KEY[ 55] := 'EXTERNAL       ';
   KEY[ 56] := 'FALSE          ';
   KEY[ 57] := 'FILE           ';
   KEY[ 58] := 'FILEPOS        ';
   KEY[ 59] := 'FILESIZE       ';
   KEY[ 60] := 'FILLCHAR       ';
   KEY[ 61] := 'FILLPATTERN    ';
   KEY[ 62] := 'FILLSCREEN     ';
   KEY[ 63] := 'FILLSHAPE      ';
   KEY[ 64] := 'FLUSH          ';
   KEY[ 65] := 'FOR            ';
   KEY[ 66] := 'FORWARD        ';
   KEY[ 67] := 'FRAC           ';
   KEY[ 68] := 'FREEMEM        ';
   KEY[ 69] := 'FUNCTION       ';
   KEY[ 70] := 'GET            ';
   KEY[ 71] := 'GETDIR         ';
   KEY[ 72] := 'GETDOT         ';
   KEY[ 73] := 'GETMEM         ';
   KEY[ 74] := 'GETPIC         ';
   KEY[ 75] := 'GOTO           ';
   KEY[ 76] := 'GOTOXY         ';
   KEY[ 77] := 'GRAPHBACKGROUND';
   KEY[ 78] := 'GRAPHCOLORMODE ';
   KEY[ 79] := 'GRAPHMODE      ';
   KEY[ 80] := 'GRAPHWINDOW    ';
   KEY[ 81] := 'HALT           ';
   KEY[ 82] := 'HEADING        ';
   KEY[ 83] := 'HEAPPTR        ';
   KEY[ 84] := 'HI             ';
   KEY[ 85] := 'HIDETURTLE     ';
   KEY[ 86] := 'HIRES          ';
   KEY[ 87] := 'HIRESCOLOR     ';
   KEY[ 88] := 'HOME           ';
   KEY[ 89] := 'IF             ';
   KEY[ 90] := 'IN             ';
   KEY[ 91] := 'INLINE         ';
   KEY[ 92] := 'INPUT          ';
   KEY[ 93] := 'INSERT         ';
   KEY[ 94] := 'INSLINE        ';
   KEY[ 95] := 'INT            ';
   KEY[ 96] := 'INTEGER        ';
   KEY[ 97] := 'INTR           ';
   KEY[ 98] := 'IORESULT       ';
   KEY[ 99] := 'KBD            ';
   KEY[100] := 'KEYPRESSED     ';
   KEY[101] := 'LABEL          ';
   KEY[102] := 'LENGTH         ';
   KEY[103] := 'LN             ';
   KEY[104] := 'LO             ';
   KEY[105] := 'LOWVIDEO       ';
   KEY[106] := 'LST            ';
   KEY[107] := 'LSTOUTPTR      ';
   KEY[108] := 'MARK           ';
   KEY[109] := 'MAXAVAIL       ';
   KEY[110] := 'MAXINT         ';
   KEY[111] := 'MEMAVAIL       ';
   KEY[112] := 'MEMW           ';
   KEY[113] := 'MKDIR          ';
   KEY[114] := 'MOD            ';
   KEY[115] := 'MOVE           ';
   KEY[116] := 'MSDOS          ';
   KEY[117] := 'NEW            ';
   KEY[118] := 'NIL            ';
   KEY[119] := 'NORMVIDEO      ';
   KEY[120] := 'NOSOUND        ';
   KEY[121] := 'NOT            ';
   KEY[122] := 'ODD            ';
   KEY[123] := 'OF             ';
   KEY[124] := 'OFS            ';
   KEY[125] := 'OR             ';
   KEY[126] := 'ORD            ';
   KEY[127] := 'OUTPUT         ';
   KEY[128] := 'OVERLAY        ';
   KEY[129] := 'PACK           ';
   KEY[130] := 'PACKED         ';
   KEY[131] := 'PAGE           ';
   KEY[132] := 'PALETTE        ';
   KEY[133] := 'PARAMCOUNT     ';
   KEY[134] := 'PARAMSTR       ';
   KEY[135] := 'PATTERN        ';
   KEY[136] := 'PENDOWN        ';
   KEY[137] := 'PENUP          ';
   KEY[138] := 'PI             ';
   KEY[139] := 'PLOT           ';
   KEY[140] := 'PORT           ';
   KEY[141] := 'POS            ';
   KEY[142] := 'PRED           ';
   KEY[143] := 'PROCEDURE      ';
   KEY[144] := 'PROGRAM        ';
   KEY[145] := 'PTR            ';
   KEY[146] := 'PUT            ';
   KEY[147] := 'PUTPIC         ';
   KEY[148] := 'RANDOM         ';
   KEY[149] := 'RANDOMIZE      ';
   KEY[150] := 'READ           ';
   KEY[151] := 'READLN         ';
   KEY[152] := 'REAL           ';
   KEY[153] := 'RECORD         ';
   KEY[154] := 'RELEASE        ';
   KEY[155] := 'RENAME         ';
   KEY[156] := 'REPEAT         ';
   KEY[157] := 'RESET          ';
   KEY[158] := 'REWRITE        ';
   KEY[159] := 'RMDIR          ';
   KEY[160] := 'ROUND          ';
   KEY[161] := 'SEEK           ';
   KEY[162] := 'SEEKEOF        ';
   KEY[163] := 'SEEKEOLN       ';
   KEY[164] := 'SEG            ';
   KEY[165] := 'SET            ';
   KEY[166] := 'SETHEADING     ';
   KEY[167] := 'SETPENCOLOR    ';
   KEY[168] := 'SETPOSITION    ';
   KEY[169] := 'SHL            ';
   KEY[170] := 'SHOWTURTLE     ';
   KEY[171] := 'SHR            ';
   KEY[172] := 'SIN            ';
   KEY[173] := 'SIZEOF         ';
   KEY[174] := 'SOUND          ';
   KEY[175] := 'SQR            ';
   KEY[176] := 'SQRT           ';
   KEY[177] := 'STR            ';
   KEY[178] := 'STRING         ';
   KEY[179] := 'SUCC           ';
   KEY[180] := 'SWAP           ';
   KEY[181] := 'TEXT           ';
   KEY[182] := 'TEXTBACKGROUND ';
   KEY[183] := 'TEXTCOLOR      ';
   KEY[184] := 'TEXTMODE       ';
   KEY[185] := 'THEN           ';
   KEY[186] := 'TO             ';
   KEY[187] := 'TRM            ';
   KEY[188] := 'TRUE           ';
   KEY[189] := 'TRUNC          ';
   KEY[190] := 'TURNLEFT       ';
   KEY[191] := 'TURNRIGHT      ';
   KEY[192] := 'TURTLETHERE    ';
   KEY[193] := 'TURTLEWINDOW   ';
   KEY[194] := 'TYPE           ';
   KEY[195] := 'UNPACK         ';
   KEY[196] := 'UNTIL          ';
   KEY[197] := 'UPCASE         ';
   KEY[198] := 'USR            ';
   KEY[199] := 'USRINPTR       ';
   KEY[200] := 'USROUTPTR      ';
   KEY[201] := 'VAL            ';
   KEY[202] := 'VAR            ';
   KEY[203] := 'WHEREX         ';
   KEY[204] := 'WHEREY         ';
   KEY[205] := 'WHILE          ';
   KEY[206] := 'WINDOW         ';
   KEY[207] := 'WITH           ';
   KEY[208] := 'WRAP           ';
   KEY[209] := 'WRITE          ';
   KEY[210] := 'WRITELN        ';
   KEY[211] := 'XCOR           ';
   KEY[212] := 'XOR            ';
   KEY[213] := 'YCOR           ';
   FF := CHR(12);
   QUOTE := CHR(39);

   { SCAN INPUT FOR IDENTIFIERS }
   N := 0;
   M := 0;
   LL1 := LLDF;
   LL2 := LLDF;
   WHILE NOT INPEF DO
     BEGIN
       OPENLINE;
       WHILE NOT INPEL DO
         BEGIN
           IF INPCH = ' '
             THEN
               ADVANCE
             ELSE
               IF INPCH IN ['A'..'Z']
                 THEN
                   BEGIN
                     K := 0;
                     ID.A := EMPTY;
                     REPEAT
                       IF K < KLN
                         THEN
                           BEGIN
                             K := K+1;
                             IF INPCH IN ['a'..'z']
                               THEN
                                 ID.A[K] := CHR(ORD(INPCH)-32)
                               ELSE
                                 ID.A[K] := INPCH
                           END;
                       ADVANCE
                     UNTIL NOT(INPCH IN ['A'..'Z', 'a'..'z', '0'..'9']);
                     IF NOKEY THEN SEARCH
                   END
                 ELSE
                   IF INPCH IN ['0'..'9']
                     THEN
                       REPEAT
                         ADVANCE
                       UNTIL NOT (INPCH IN ['B','0'..'9'])
                     ELSE
                       IF INPCH = QUOTE
                         THEN
                           BEGIN { STRING }
                             REPEAT
                               ADVANCE
                             UNTIL (INPCH = QUOTE) OR INPEL;
                             IF NOT INPEL THEN ADVANCE
                           END
                         ELSE
                           IF INPCH = '{'
                             THEN
                               BEGIN { COMMENT }
                                 ADVANCE;
                                 WHILE INPCH <> '}' DO
                                   BEGIN
                                     ADVANCE;
                                     WHILE INPEL DO
                                      BEGIN
                                       CLOSELINE;
                                       OPENLINE
                                      END
                                   END;
                                 ADVANCE
                               END
                             ELSE
                               IF INPCH = '('
                                 THEN
                                   BEGIN
                                     ADVANCE;
                                     IF INPCH = '*'
                                      THEN
                                       BEGIN { COMMENT }
                                        ADVANCE;
                                        REPEAT
                                         WHILE INPCH <> '*' DO
                                          BEGIN
                                           IF INPEL
                                            THEN
                                             BEGIN
                                              CLOSELINE;
                                              OPENLINE
                                             END
                                            ELSE
                                             ADVANCE
                                          END;
                                         ADVANCE
                                        UNTIL INPCH = ')';
                                        ADVANCE
                                       END
                                   END
                                 ELSE
                                   ADVANCE
         END;
       CLOSELINE
     END;
   CLOSE(INP);
   WRITELN(OUT,FF);
   M := 0;
   PRINTTABLE;
   IF (LPPG-M) > 4 THEN EPILOG;
   IF OUTNAM = 'PRN' THEN WRITELN(OUT,FF);
   CLOSE(OUT);
   WRITELN;
   WRITELN('XREF processing of ',INPNAM,' is complete.');
   WRITELN('The cross reference output file is ',OUTNAM)
 END.
