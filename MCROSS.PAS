program Mcross;
// {$L+,C-,R-,T+}

// PROGRAM CROSS ( TTY, INPUT, OUTPUT );

(*PROGRAM FOR CREATING A CROSSREFERENCE LIST WITH SIMULTANEOUS
    FORMAT OF A PASCAL PROGRAM. WRITTEN BY MANUEL MALL.
  PROGRAMM ZUR ERSTELLUNG EINER CROSSREFERENZLISTE MIT GLEICHZEITIGEM
    FORMATIEREN EINES PASCALPROGRAMMS.
    GESCHRIEBEN VON MANUEL MALL.*)

// Updated 2022-01-04 Paul Robinson for Free Pascal Compiler

(* MOVED FROM DEC-10  1977-10-01 BY SEVED TORSTENDAHL
  CHANGED   CAUSE
  1977-11-22   1   HT & ESC CHAR'S GIVE SYNTAX ERROR
  1978-11-22   2   FILENAME VARIABLE TOO SHORT
  1978-11-22   3   FILENAME EXTENSION & VERSION GIVEN =>
                   SPURIOUS ERRORS
  1979-10-26   4   FORMFEED SHOULD REMAIN IN SOURCE FILE
  1979-10-26   5   FORMFEED AND $P+ SHOULD GIVE NEW PAGE IN LIST FILE
  1979-10-26   6   PAGE HEADER MORE LIKE LME STANDARD
  1979-10-26   7   FILENAME SHOULD ALLOW DEVICE AND UIC

  2021-01-04   8   Fix translation issues
                   % changed to { and \ changed to }
                   # is changed to <>
                   LOOP / EXIT IF ... / END changed to
                     LOOP := while true do begin
                     EXIT IF ... := IF ... then break
                   Octal constants changed from nnnB; to &nnn;
                   Comments translated to enflisg but with
                     original comment retained
                   Changed label / goto statements used to
                      exit procedure early to exit amd ro
                      end program to halt
 *)
// LABEL
// 99;
// This is being used for EXIT procedure

CONST
VERSION = 'CROSS VERSION OF OCT -79';       {DATE OF LAST MODIFICATION
                                             DATUM DER LETZTEN AENDERUNG}
FEED = 2;                             {CHARACTER FEED FOR PROCEDURES AND BLOCKS
                                       ZEICHENVORSCHUB BEI PROZEDUREN
                                         UND BLOECKEN}
MAXCH = 72;                          {MAXIMUM NUMBER OF CHARACTERS
                                        PER PRINT LINE
                                      MAXIMALE ZAHL VON ZEICHEN PRO DRUCKZEILE}
BACKFEED = 1;                         {CHARACTER PRETENSION AT
                                         'PROCEDURE','BEGIN', ETC.
                                       ZEICHENVORRUECKUNG BEI
                                         'PROCEDURE','BEGIN' ETC.}
MAXLINE = 40;                         {MAXIMUM NUMBER OF LINES PER PRINT PAGE
                                       MAXIMALE ZAHL VON ZEILEN PRO DRUCKSEITE}
HT = &11;                             {ASCII HORIZONTAL TAB}
LF = &12;                             {ASCII LINE FEED}
FF = &14;                             {ASCII FORM FEED}
CR = &15;                             {ASCII CARRIAGE RETURN}

TYPE
LINEPTRTY = ^LINE;
LISTPTRTY = ^LIST;
PROCCALLTY = ^PROCCALL;
PROCSTRUCTY = ^PROCSTRUC;
LINENRTY = 0..&7777;
PAGENRTY = 0..&77;
STR5 = PACKED ARRAY [1..5] OF CHAR;
WORD    = PACKED ARRAY [1..10] OF CHAR;
SYMBOL = (LABELSY,CONSTSY,TYPESY,VARSY,                       {DECSYM}
          FUNCTIONSY,PROCEDURESY,INITPROCSY,                  {PROSYM}
          ENDSY,UNTILSY,ELSESY,THENSY,EXITSY,OFSY,DOSY,EOBSY, {ENDSYMBOLS}
          BEGINSY,CASESY,LOOPSY,REPEATSY,IFSY,                {BEGSYM}
          RECORDSY,FORWARDSY,OTHERSY,INTCONST,IDENT,STRGCONST,EXTERNSY,
          RPARENT,SEMICOLON,POINT,LPARENT,COLON,LBRACK,OTHERSSY{DELIMITER});


LINE = PACKED RECORD                          {DESCRIPTION OF LINE NUMBERS
                                               BESCHREIBUNG DER ZEILENNUMMERN}
      	  LINENR : LINENRTY;            {Line number
                                         ZEILENNUMMER}
      	  PAGENR : PAGENRTY;            {Page number
                                         SEITENNUMMER}
      	  CONTLINK : LINEPTRTY          {NEXT LINE NUMBER RECORD
                                         NAECHSTER ZEILENNUMMERNRECORD}
      	END;

LIST = PACKED RECORD                          {DESCRIPTION OF IDENTIFIERS
                                               BESCHREIBUNG VON IDENTIFIERN}
      	  NAME : WORD;                  {Name of identifier
                                         NAME DES IDENTIFIERS}
      	  LLINK ,                       {LEFT SUCCESSOR IN TREE
                                         LINKER NACHFOLGER IN BAUM}
      	  RLINK : LISTPTRTY;            {Right successor in tree
                                         RECHTER NACHFOLGER IM BAUM}
      	  FIRST ,                       {POINTER TO FIRST LINE NUMBER RECORD
                                         ZEIGER AUF ERSTEN ZEILENNUMMERNRECORD}
      	  LAST  : LINEPTRTY;            {Pointer to last line number record
                                         ZEIGER AUF LETZTEN ZEILENNUMMERNRECORD}
      	  PROCVAR : 0..2;               {0=NO PROCEDURE/ 1=PROCEDURE/
                                           2=FUNCTION
                                         0=KEINE PROZEDUR/ 1=PROZEDUR/
                                           2=FUNKTION}
      	  CALLED,                       {POINTER TO THE FIRST PROCEDURE
                                         CALLED BY IT
                                         ZEIGER AUF DIE ERSTE PROZEDUR DIE
                                           VON DIESER GERUFEN WIRD}
      	  CALLEDBY : PROCCALLTY         {POINTER TO FIRST CALLING PROCEDURE
                                         ZEIGER AUF ERSTE RUFENDE PROZEDUR}
      	END;

PROCCALL = PACKED RECORD                   {DESCRIPTION OF PROCEDURE CALLS
                                            BESCHREIBUNG VON PROZEDURAUFRUFEN}
      	      PROCNAME : LISTPTRTY;     {POINTER TO THE CORRESPONDING
                                           IDENTIFIER RECORD
                                         ZEIGER AUF DEN ZUGEHOERIGEN
                                           IDENTIFIERRECORD}
      	      NEXTPROC : PROCCALLTY;    {POINTER TO THE NEXT PROCEDURE
                                         ZEIGER AUF DIE NAECHSTE PROZEDUR}
      	      FIRST,                    {LINE NUMBERCORD FOR THE FIRST CALL
                                         ZEILENNUMMERNRECORD FUER DEN
                                           ERSTEN AUFRUF}
      	      LAST : LINEPTRTY          {LINE NUMBEER RCORD FOR THE LAST CALL
                                         ZEILENNUMMERNRECORD FUER DEN
                                           LETZTEN AUFRUF}
      	    END;

DOUBLEDEC = PACKED RECORD                     {PROCEDURES THAT ARE ALSO
                                                 DEFINED AS NORMAL ID.
                                               PROZEDUREN DIE AUCH ALS
                                                 NORMALE ID. DEFINIERT WURDEN}
      	       PROCORT : LISTPTRTY;     {POINTER TO THE PROCEDURE
                                         ZEIGER AUF DIE PROZEDUR}
      	       NEXTPROC: ^DOUBLEDEC     {NEXT DOUBLE-DECLARED PROCEDURE
                                         NAECHSTE DOPPELT DEKLARIERTE PROZEDUR}
      	     END;

PROCSTRUC = PACKED RECORD                     {DESCRIPTION OF PROCEDURE NESTING
                                               BESCHREIBUNG DER
                                               PROZEDURVERSCHACHTELUNG}
      	       PROCNAME : LISTPTRTY;    {POINTER TO THE ASSOCIATED IDENTIFIER
                                         ZEIGER AUF DEN ZUGERHOERIGEN
                                           IDENTIFIER}
      	       NEXTPROC : PROCSTRUCTY;  {POINTER TO THE PROCEDURE DECLARED NEXT
                                         ZEIGER AUF DIE NAECHSTD
                                           DEKLARIERTE PROZEDUR}
      	       LINENR : LINENRTY;       {PROCEDURE DEFINITION LINE NUMBER
                                         ZEILENNUMMER DER PROZEDURDEFINITION}
      	       PAGENR ,                 {PROCEDURE DEFINITION PAGE NUMBER
                                         SEITENNUMMER DER PROZEDURDEFINITION}
      	       PROCLEVEL: PAGENRTY      {NESTING DEPTH OF PROCEDURE
                                         VERSCHACHTELUNGSTIEFE DER PROZEDUR}
      	     END;

VAR
PIX,
I,                                    {LOOP VARIABLE
                                       SCHLEIFENVARIABLE}
BUFFLEN,                              {LENGTH OF THE DESCRIBED PART OF
                                         THE INPUT BUFFER
                                       LAENGE DES BESCHRIEBENEN TEILS
                                         DES EINGABEPUFFERS}
BUFFMARK,                             {LENGTH OF THE ALREADY PRINTED
                                         PART OF THE BUFFER
                                       LAENGE DES SCHON GEDRUCKTEN
                                         TEIL DES PUFFERS}
BUFFERPTR,                            {POINTER TO THE NEXT CHARACTER
                                         TO READ IN THE BUFFER
                                       ZEIGER AUF DAS NAECHSTE ZU
                                         LESENDE ZEICHEN IM PUFFER}
BUFFINDEX,                            {POINTERS IN BUFF'S ARRAY
                                       ZEIGER IM ARRAY VON BUFF}
BMARKNR,                              {NUMBER TO BE PRINTED FOR MARKING
                                         'BEGIN', 'LOOP' ETC.
                                       ZU DRUCKENDE NUMMER FUER MARKIERUNG
                                         VON 'BEGIN', 'LOOP' ETC.}
EMARKNR,                              {NUMBER TO BE PRINTED FOR MARKING
                                        'END', 'UNTIL' ETC.
                                       ZU DRUCKENDE NUMMER FUER MARKIERUNG
                                         VON 'END', 'UNTIL' ETC.}
SPACES,                               {CHARACTER FEED FOR FORMATTING
                                       ZEICHENVORSCHUB FUER DIE FORMATIERUNG}
LASTSPACES,                           {LAST CHARACTER FEED USED
                                       LETZTER BENUTZTER ZEICHENVORSCHUB}
SYLENG,                               {LENGTH OF THE LAST IDENTIFIER READ
                                       LAENGE DES LETZTEN GELESENEN
                                         BEZEICHNERS}
CHCNT,                                {NUMBER OF RELEVANT CHARACTERS IN
                                         THE LAST IDENTIFIER
                                       ANZAHL DER RELEVANTEN ZEICHEN IM
                                         LETZTEN BEZEICHNER}
LEVEL,                                {NESTING DEPTH OF THE CURRENT PROCEDURE
                                       VERSCHACHTELUNGSTIEFE DER
                                         AKTUELLEN PROZEDUR}
BLOCKNR,                              {INCLUDES THE MARKED STATEMENTS
                                       ZAEHLT DIE GEKENNZEICHNETEN STATEMENTS}
PROCDEC,                              {SET AT PROCEDURE DECLARATION
                                         1=PROCEDURE 2=FUNCTION
                                       GESETZT BEI PROZEDUR DEKLARATION
                                         1=PROCEDURE 2=FUNCTION}
PAGECNT,                              {COUNTS THE NUMBER OF SOS PAGES
                                       ZAEHLT DIE SOS-SEITEN}
PAGECNT2,                             {COUNTS PRINT PAGES PER SOS PAGE
                                       ZAEHLT DIE DRUCKSEITEN PRO SOS-SEITE}
INCREMENT,                            {PARAMETERS FOR INCREASING LINE NUMBERS
                                       PARAMETER FUER DIE ERHOEHUNG
                                         DER ZEILENNUMMERN}
MAXINC,                               {LARGEST ALLOWED LINE NUMBER
                                       GROESSTE ERLAUBTE ZEILENNUMMER}
REALLINCNT,                           {COUNT OF LINES PER PRINT PAGE
                                       ZAEHLT DIE ZEILEN PRO DRUCKSEITE}
LINECNT : INTEGER;                    {COUNTS THE ROWS PER SOS PAGE
                                       ZAEHLT DIE ZEILEN PRO SOS-SEITE}
INPUTFILE,                            {DESCRIPTION OF THE INPUT FILE
                                       BESCHREIBUNG DES EINGABEFILES}
OUTPUTFILE : RECORD                   {DESCRIPTION OF THE OUTPUT FILE
                                       BESCHREIBUNG DES AUSGABEFILES}
// these 3 are only used by DEC timesharing operating systems
      	 DEV,UIC,FILENAME : PACKED ARRAY [1..20] OF CHAR    (*** C7 ***)
             END;
// file extensions: pascal, listing, cross-reference
PAS,NUE,CRL: PACKED ARRAY [0..3] OF CHAR;

// IOS only used on DEC 10
// IOS: SET OF IOSPEC;
// used as argument to rewrite
BUFFER  : ARRAY [1..147] OF CHAR;     {INPUT BUFFER (147 CHARACTERS =
                                         MAX. LENGTH SOS LINE)
                                       EINGABEPUFFER (147 ZEICHEN =
                                         MAX. LAENGE SOS-ZEILE)}
LINENB : PACKED ARRAY [1..5] OF CHAR; {SOS LINE NUMBER
                                       SOS-ZEILENNUMMER}
TIMEANDDAY : PACKED ARRAY [1..24] OF CHAR;           {DATE AND TIME FOR
                                                        PRINT PAGE HEAD
                                                      DATUM UND UHRZEIT
                                                        FUER DRUCKSEITENKOPF}
SY      : WORD;                       {LETZTER GELESENER BEZEICHNER}
SYTY    : SYMBOL;                     {TYP DES LETZTEN GELESENEN ZEICHENS}
PAGEEJECT,                            {MARKER FOR PAGE SHIFT}   (*** C5 ***)
ERRFLAG,                              {FEHLERMARKE}
OLDSPACES,                            {GESETZT WENN LASTSPACES BENUTZT WERDEN SOLL}
EOB     : BOOLEAN;                    {EOF-MARKE}
CH,                                   {LETZTES GELESENES ZEICHEN}
BMARKTEXT,                            {TEXT ZUR MARKIERUNG VON 'BEGIN' ETC.}
EMARKTEXT: CHAR;                      {TEXT ZUR MARKIERUNG VON 'END' ETC.}
DELSY : ARRAY [' '..'_'] OF SYMBOL;   {TYPENARRAY FUER DELIMITERZEICHEN}
RESNUM  : ARRAY [1..11] OF INTEGER;   {STARTADRESSEN FUER DIE RESERVIERTEN WORTE BESTIMMTER LAENGE}
RESLIST : ARRAY [1..43] OF WORD;      {LISTE DER RESERVIERTEN WORTE}
RESSY   : ARRAY [1..43] OF SYMBOL;    {TYPENLISTE DER RESERVIERTEN WORTE}
ALPHANUM,                             {ZEICHEN VON 0..9 UND A..Z}
DIGITS,                               {ZEICHEN VON 0..9}
LETTERS : SET OF CHAR;                {ZEICHEN VON A..Z}
RELEVANTSYM,                          {STARTSYMBOLE FUER STATEMENTS UND PROCEDURES}
PROSYM,                               {ALLE SYMBOLE DIE DEN BEGINN EINER PROZEDUR KENNZEICHNEN}
DECSYM,                               {ALLE SYMBOLE DIE DEN BEGINN VON DEKLARATIONEN KENNZEICHNEN}
BEGSYM,                               {ALLE SYMBOLE DIE DEN BEGINN EINES STATEMENTS KENNZEICHNEN}
ENDSYM  : SET OF SYMBOL;              {ALLE SYMBOLE DIE STATEMENTS ODER PROZEDUREN TERMINIEREN}
LISTPTR : LISTPTRTY;                  {ZEIGER IM BINAERBAUM DER DEKLARIETEN BEZEICHNER}
FIRSTNAME : ARRAY ['A'..'Z'] OF LISTPTRTY;    {ZEIGER AUF DIE WURZELN DES BAUMES}
PROCSTRUCF,                           {ZEIGER AUF DAS ERSTE ELEMENT DER PROZEDURENLISTE}
PROCSTRUCL : PROCSTRUCTY;             {ZEIGER AUF DAS LETZTE ELEMENT DER PROZEDURENLISTE}
//NEWFIL : FILE OF CHAR;                {AUSGABEFILE AUF DEM DAS NEUFORMATIERTE PROGRAMM STEHT}
TTY,
NEWFIL: text;
MESSAGE : PACKED ARRAY [1..23] OF CHAR;       {ARRAY ZUR AUSGABE DER SCHLUSSMELDUNG}

PROCEDURE INIT;
 BEGIN

   BEGIN
    RESNUM[1] := 1;
    RESNUM[2] := 1;
    RESNUM[3] := 7;
    RESNUM[4] :=15;
    RESNUM[5] :=26;
    RESNUM[6] :=32;
    RESNUM[7] :=38;
    RESNUM[8] :=41;
    RESNUM[9] :=42;
    RESNUM[10]:=43;
    RESNUM[11]:=44;
    RESLIST[ 1] :='IF   ';      RESSY [ 1] := IFSY;      RESLIST[ 2] :='TO   ';      RESSY [ 2] := OTHERSY;
    RESLIST[ 3] :='OF   ';      RESSY [ 3] := OFSY;      RESLIST[ 4] :='IN   ';      RESSY [ 4] := OTHERSY;
    RESLIST[ 5] :='DO   ';      RESSY [ 5] := DOSY;      RESLIST[ 6] :='OR   ';      RESSY [ 6] := OTHERSY;
    RESLIST[ 7] :='END  ';      RESSY [ 7] := ENDSY;     RESLIST[ 8] :='FOR  ';      RESSY [ 8] := OTHERSY;
    RESLIST[ 9] :='SET  ';      RESSY [ 9] := OTHERSY;   RESLIST[10] :='AND  ';      RESSY [10] := OTHERSY;
    RESLIST[11] :='NOT  ';      RESSY [11] := OTHERSY;   RESLIST[12] :='VAR  ';      RESSY [12] := VARSY;
    RESLIST[13] :='NIL  ';      RESSY [13] := OTHERSY;   RESLIST[14] :='DIV  ';      RESSY [14] := OTHERSY;
    RESLIST[15] :='LOOP ';      RESSY [15] := LOOPSY;    RESLIST[16] :='CHAR ';      RESSY [16] := OTHERSY;
    RESLIST[17] :='GOTO ';      RESSY [17] := OTHERSY;   RESLIST[18] :='THEN ';      RESSY [18] := THENSY;
    RESLIST[19] :='ELSE ';      RESSY [19] := ELSESY;    RESLIST[20] :='WITH ';      RESSY [20] := OTHERSY;
    RESLIST[21] :='CASE ';      RESSY [21] := CASESY;    RESLIST[22] :='REAL ';      RESSY [22] := OTHERSY;
    RESLIST[23] :='FILE ';      RESSY [23] := OTHERSY;   RESLIST[24] :='TYPE ';      RESSY [24] := TYPESY;
    RESLIST[25] :='EXIT ';      RESSY [25] := EXITSY;    RESLIST[26] :='BEGIN';      RESSY [26] := BEGINSY;
    RESLIST[27] :='ARRAY';      RESSY [27] := OTHERSY;   RESLIST[28] :='WHILE';      RESSY [28] := OTHERSY;
    RESLIST[29] :='CONST';      RESSY [29] := CONSTSY;   RESLIST[30] :='LABEL';      RESSY [30] := LABELSY;
    RESLIST[31] :='UNTIL';      RESSY [31] := UNTILSY;   RESLIST[32] :='RECORD';     RESSY [32] := RECORDSY;
    RESLIST[33] :='REPEAT';     RESSY [33] := REPEATSY;  RESLIST[34] :='DOWNTO';     RESSY [34] := OTHERSY;
    RESLIST[35] :='PACKED';     RESSY [35] := OTHERSY;   RESLIST[36] :='OTHERS';     RESSY [36] := OTHERSSY;
    RESLIST[37] :='EXTERN';     RESSY [37] := EXTERNSY;  RESLIST[38] :='INTEGER';    RESSY [38] := OTHERSY;
    RESLIST[39] :='BOOLEAN';    RESSY [39] := OTHERSY;   RESLIST[40] :='FORWARD';    RESSY [40] := FORWARDSY;
    RESLIST[41] :='FUNCTION';   RESSY [41] := FUNCTIONSY;RESLIST[42] :='PROCEDURE';  RESSY [42] := PROCEDURESY;
    RESLIST[43] :='INITPROCED'; RESSY [43] := INITPROCSY;
   END;
  

   BEGIN
    I := 0;
    BUFFLEN := 0;
    BUFFMARK := 0;
    BUFFERPTR := 2;
    BUFFINDEX := 0;
    REALLINCNT:= 0;
    LINECNT :=0;
    BLOCKNR := 0;
    LEVEL := 0;
    PAGECNT := 1;
    PAGECNT2 := 0;
    INCREMENT := 1;
    EOB  := FALSE;
    ERRFLAG := FALSE;
    OLDSPACES := FALSE;
    CH := ' ';
    BMARKTEXT := ' ';
    EMARKTEXT := ' ';
    SY := '          ';
    MESSAGE := 'ERROR IN BLOCKSTRUCTURE';
    TIMEANDDAY := '                  :  :  ';
    DIGITS := ['0'..'9'];
    LETTERS := ['A'..'Z'];
    ALPHANUM := ['0'..'9','A'..'Z'] {LETTERS OR DIGITS};
    DECSYM := [LABELSY..VARSY];
    PROSYM := [FUNCTIONSY..INITPROCSY];
    ENDSYM := [FUNCTIONSY..EOBSY];      {PROSYM OR ENDSYMBOLS}
    BEGSYM := [BEGINSY..IFSY];
    RELEVANTSYM := [LABELSY..INITPROCSY {DECSYM OR PROSYM} ,BEGINSY,FORWARDSY,EXTERNSY,EOBSY];
    PAS:='.PAS';  NUE:='.PAS';  CRL:='.CRL';
   END;
  

   BEGIN (*INIT*)
    FOR CH := 'A' TO 'Z' DO FIRSTNAME [CH] := NIL;
    FOR CH := ' ' TO '_' DO DELSY [CH] := OTHERSY;
    DELSY ['('] := LPARENT;
    DELSY [')'] := RPARENT;
    DELSY ['['] := LPARENT;
    DELSY [']'] := RPARENT;
    DELSY [';'] := SEMICOLON;
    DELSY ['.'] := POINT;
    DELSY [':'] := COLON;
    FOR I := 1 TO 147 DO BUFFER [I] := ' ';
    I := 0;
    NEW (FIRSTNAME['M']);
    LISTPTR := FIRSTNAME ['M'];
    WITH FIRSTNAME ['M']^ DO
     BEGIN
      NAME := 'MAIN.     ';
      LLINK := NIL;
      RLINK := NIL;
      NEW (FIRST);
      LAST := FIRST;
      PROCVAR := 1;
      WITH LAST^ DO
       BEGIN
        LINENR := LINECNT;
        CONTLINK := NIL;
       END;
      NEW (CALLED);
      WITH CALLED^ DO
       BEGIN
        PROCNAME := FIRSTNAME ['M'];
        NEXTPROC := NIL;
        NEW (FIRST);
        FIRST^.LINENR := 0;
        FIRST^.CONTLINK := NIL;
        LAST := FIRST;
       END;
      NEW (CALLEDBY);
      WITH CALLEDBY^ DO
       BEGIN
        PROCNAME := FIRSTNAME ['M'];
        NEXTPROC := NIL;
        NEW (FIRST);
        FIRST^.LINENR := 0;
        FIRST^.CONTLINK := NIL;
        LAST := FIRST;
       END;
     END;
    NEW (PROCSTRUCF);
    WITH PROCSTRUCF^ DO
     BEGIN
      PROCNAME := FIRSTNAME ['M'];
      NEXTPROC := NIL;
      LINENR   := 0;
      PROCLEVEL:= 0;
     END;
    PROCSTRUCL := PROCSTRUCF;
   END {INIT} ;
 END;

PROCEDURE GETLINENR( FLIN: STR5 );

VAR
  I,L,FAC: INTEGER;
 BEGIN
  FAC:=10000;  L:=LINECNT+1;
  FOR I:=1 TO 5 DO
   BEGIN
    FLIN[I] := CHR( L DIV FAC + &60 );
    L := L MOD FAC;   FAC := FAC DIV 10;
   END
 END;


PROCEDURE WRITECH (FCH : CHAR);

 BEGIN {WRITECH}
  WRITE(NEWFIL,FCH);
 END {WRITECH};

PROCEDURE WRITELIN;

 BEGIN {WRITELIN}
  WRITELN(NEWFIL);
 END {WRITELIN};

PROCEDURE WRITEPAGE;

 BEGIN {WRITEPAGE}
  WRITELN(NEWFIL,CHR(FF));
 END {WRITEPAGE};

PROCEDURE WRITELINNR;

VAR
  I, LLINECNT : INTEGER;

 BEGIN {WRITELINNR}
  LLINECNT := LINECNT * INCREMENT;
  (*
   I := 10000;
   FOR BUFFINDEX := 1 TO 4 DO
   BEGIN
   WRITE( NEWFIL, CHR (LLINECNT DIV I + 60B));
   LLINECNT :=  LLINECNT MOD I;
   I := I DIV 10
   END;
   WRITE( NEWFIL, CHR(LLINECNT+61B),CHR(HT));
   *)
 END {WRITELINNR};

{
PROCEDURE DATUM;
VAR
  DATUM,TIM : PACKED ARRAY [1..10] OF CHAR;
  I : INTEGER;

 BEGIN
  DATE( DATUM );   TIME( TIM );
  FOR I := 1 TO 10 DO TIMEANDDAY[I] := DATUM[I];
  FOR I:= 1 TO 8 DO TIMEANDDAY[I+16] := TIM[I];
 END;
 }
 // These will be replaced; until then
PROCEDURE datum; begin end;
PROCEDURE page; begin end;


PROCEDURE HEADER;

 BEGIN {HEADER}
  PAGECNT2 := PAGECNT2 + 1;
  REALLINCNT := 0;
  PAGE;
  WRITELN ('TELEFON AB LM ERICSSON          ',TIMEANDDAY,' ':10,
           'PAGE',PAGECNT2:4 );         (*** C6 ***)
  WRITELN ('PASCAL - ',VERSION,' ':10,OUTPUTFILE.FILENAME);     (*** C6 ***)
  WRITELN;   WRITELN;           (*** C6 ***)
  PAGEEJECT := FALSE;            (*** C5 ***)
 END {HEADER} ;


PROCEDURE NEWPAGE;

 BEGIN {NEWPAGE}
  WRITEPAGE;
  HEADER;
  IF EOLN (INPUT)
  THEN READLN;
 END {NEWPAGE} ;

PROCEDURE WRITELINE (POSITION {LETZTES ZU DRUCKENDES ZEICHEN IM PUFFER} : INTEGER);

VAR
  I, TABCNT, LSPACES : INTEGER;    {MARKIERT ERSTES ZU DRUCKENDES ZEICHEN}

 BEGIN {WRITELINE}
  POSITION := POSITION - 2;
  IF POSITION > 0
  THEN
   BEGIN
    I := BUFFMARK + 1;
    WHILE (BUFFER [I] = ' ') AND (I <= POSITION) DO I := I + 1;
    BUFFMARK := POSITION;
    WHILE (BUFFER [POSITION] = ' ') AND (I < POSITION) DO POSITION := POSITION - 1;
    IF I <= POSITION
    THEN
     BEGIN
      IF REALLINCNT = MAXLINE
      THEN HEADER;
      LINECNT := LINECNT + 1;
      REALLINCNT := REALLINCNT + 1;
      IF BMARKTEXT <> ' '
      THEN
       BEGIN
        WRITE (BMARKTEXT, BMARKNR : 4, '       ');
        BMARKTEXT := ' ';
       END
      ELSE
       IF EMARKTEXT <> ' '
       THEN
         BEGIN
          WRITE ('      ',EMARKTEXT,EMARKNR : 4,' ');
          EMARKTEXT := ' ';
         END
       ELSE WRITE ('            ');
      WRITE (LINECNT * INCREMENT : 5,' ');
      WRITELINNR;
      IF NOT OLDSPACES
      THEN LASTSPACES := SPACES;
      WRITE (' ' : LASTSPACES);
      TABCNT := LASTSPACES DIV 8;
      LSPACES := LASTSPACES MOD 8;
      FOR TABCNT := TABCNT DOWNTO 1 DO WRITECH (CHR(HT));
      FOR LSPACES := LSPACES DOWNTO 1 DO WRITECH (' ');
      IF (POSITION - I + LASTSPACES + 1) > MAXCH
      THEN
       BEGIN
        IF REALLINCNT = MAXLINE
        THEN
         BEGIN
          FOR I := I TO MAXCH + I - LASTSPACES - 1 DO
           BEGIN
            WRITE (BUFFER[I]);
            WRITECH (BUFFER[I]);
            BUFFER [I] := ' ';
           END;
          WRITELN;
          HEADER;
         END;
        REALLINCNT := REALLINCNT + 1;
       END;
      FOR I := I TO POSITION DO
       BEGIN
        WRITE (BUFFER [I]);
        WRITECH (BUFFER[I]);
        BUFFER [I] := ' ';
       END;
      WRITELIN;
      WRITELN;
      IF ((LINENB = '     ') AND (POSITION = BUFFLEN)) OR (MAXINC = LINECNT)
      THEN NEWPAGE;
      IF PAGEEJECT THEN HEADER;      (*** C5 ***)
     END;
   END;
  LASTSPACES := SPACES;
  OLDSPACES := FALSE;
 END {WRITELINE} ;

PROCEDURE BLOCK;

VAR
  DOUBLEDECF,                 {ZEIGER AUF ERSTE UND LETZTE VARIABLE DIE ALS PROCEDURE}
  DOUBLEDECL : ^DOUBLEDEC;    {IN DIESEM BLOCK DOPPELT DEKLARIERT WURDEN}
  CURPROC : LISTPTRTY;        {ZEIGER AUF DIE PROZEDUR IN DEREN ANWEISUNGSTEIL DAS PROGRAMM SICH BEFINDET}

  PROCEDURE INSYMBOL ;
  LABEL
    1;

  VAR
    OLDSPACESMARK,            {ALTER ZEICHENVORSCHUB BEI FORMATIERUNG VON KOMMENTAREN}
    I       : INTEGER;

    PROCEDURE READBUFFER;


      PROCEDURE READLINE;

      VAR
        CH      : CHAR;

       BEGIN {READLINE}
         REPEAT
          WHILE EOLN (INPUT) AND NOT (EOF (INPUT)) DO
           BEGIN
            GETLINENR (LINENB);
            READLN;
             BEGIN
      	IF REALLINCNT = MAXLINE
      	THEN HEADER;
      	LINECNT := LINECNT + 1;
      	REALLINCNT := REALLINCNT + 1;
      	WRITELN (' ' : 12,LINECNT * INCREMENT : 5);
      	WRITELINNR;
      	WRITELIN;
      	IF MAXINC = LINECNT
      	THEN NEWPAGE;
             END;
           END;
          READ (CH);
          IF CH = CHR(FF) THEN                 (*** C5 ***)
           BEGIN   NEWPAGE;
      	IF EOLN(INPUT) THEN BEGIN  READLN; READ(CH)  END;
           END;
          IF CH < ' '			(*** C 1 ***)
          THEN CH := ' ';
         UNTIL (CH <> ' ') OR (EOF (INPUT));
        BUFFLEN := 0;
//         LOOP
        while true do begin
          BUFFLEN := BUFFLEN + 1;
          BUFFER [BUFFLEN] := CH;
//         EXIT IF (EOLN (INPUT) OR (BUFFLEN = 147));
          IF (EOLN (INPUT) OR (BUFFLEN = 147)) then break;
          READ (CH);
          IF CH < ' ' THEN  CH := ' ';		(*** C 1 ***)
         END;
        IF NOT (EOLN (INPUT))
        THEN
         BEGIN
          WRITELN (TTY);
          WRITELN (TTY,'LINE ',(LINECNT+1)*INCREMENT : 5,'TOO LONG');
          WRITELN (' ' : 17,' **** NEXT LINE TOO LONG ****');
         END
        ELSE
         IF NOT (EOF (INPUT))
         THEN
           BEGIN
            GETLINENR (LINENB);
            READLN;
           END;
        BUFFERPTR := 1;
        BUFFMARK := 0;
       END {READLINE} ;
      
     BEGIN {READBUFFER}
      IF BUFFERPTR = BUFFLEN + 2
      THEN
       BEGIN
        WRITELINE (BUFFERPTR);
        CH := ' ';
        IF EOF (INPUT)
        THEN EOB := TRUE
        ELSE READLINE;
       END
      ELSE
       BEGIN
        CH := BUFFER [BUFFERPTR];
        BUFFERPTR := BUFFERPTR + 1;
       END;
     END {READBUFFER} ;


    PROCEDURE LPARENTHESE;

    VAR
      OLDSPACESMARK : INTEGER;        {OLD CHARACTER FEED WHEN FORMATTING
                                       PARENTHESES
                                       ALTER ZEICHENVORSCHUB BEI FORMATIERUNG
                                         VON KLAMMERN}

     BEGIN {PARENTHESE}
      OLDSPACESMARK := SPACES;
      IF OLDSPACES
      THEN SPACES := LASTSPACES + BUFFERPTR - 2
      ELSE
       BEGIN
        LASTSPACES := SPACES;
        SPACES := SPACES + BUFFERPTR - 2;
        OLDSPACES := TRUE;
       END;
       REPEAT
        INSYMBOL
       UNTIL SYTY IN [RPARENT,EOBSY];
      SPACES := OLDSPACESMARK;
      OLDSPACES := TRUE;
      INSYMBOL;
     END {PARENTHESE} ;


    FUNCTION RESWORD: BOOLEAN ;
    LABEL
      1;

    VAR
      I       : INTEGER;

     BEGIN {RESWORD}
      RESWORD:= FALSE;
      FOR I:=RESNUM[CHCNT] TO RESNUM [CHCNT + 1] -1 DO
      IF RESLIST[ I ] = SY
      THEN
       BEGIN
        RESWORD := TRUE;
        SYTY := RESSY [I];
        GOTO 1;
       END;
1:
     END {RESWORD} ;
    
    PROCEDURE FINDNAME;
    LABEL
      1;

    VAR
      PROCPTR : PROCCALLTY;   {POINTER TO CALLING OR CALLED PROCEDURE
                                 WHEN CONCATENATING
                               ZEIGER AUF RUFENDE BZW. GERUFENE PROZEDUR
                                 BEI DEREN VERKETTUNG}
      LPTR: LISTPTRTY;        {POINTER TO THE LEADER IN THE TREE
                               ZEIGER AUF DEN VORGAENGER IM BAUM}
      ZPTR : LINEPTRTY;       {POINTER TO THE PENULTIMATE LINE NUMBER IN
                                 A CHAIN
                               ZEIGER AUF DIE VORLETZTE ZEILENNUMMER IN
                                 EINER KETTE}
      RIGHT: BOOLEAN;         {MEMORY VARIABLE FOR BRANCHING IN THE TREE
                               MERKVARIABLE FUER DIE VERZWEIGUNG IM BAUM}
      INDEXCH : CHAR;         {INDEX VARIABLE FOR THE START POINTER
                                 FIELD (FIRSTNAME)
                               INDEXVARIABLE FUER DAS FELD DER
                                 STARTZEIGER (FIRSTNAME)}


      PROCEDURE FINDPROC (COMP : LISTPTRTY);

      VAR
        PROCCALLPTR : PROCCALLTY;     {MERK SICH LETZTE PROZEDUR FALLS EINE NEUE ERZEUGT WERDEN MUSS}

       BEGIN {FINDPROC}
        WHILE (PROCPTR^.PROCNAME <> COMP) AND (PROCPTR^.NEXTPROC <> NIL) DO
        PROCPTR := PROCPTR^.NEXTPROC;
        IF PROCPTR^.PROCNAME = COMP
        THEN
         BEGIN
          ZPTR := PROCPTR^.LAST;
          NEW (PROCPTR^.LAST);
          WITH PROCPTR^.LAST^ DO
           BEGIN
            LINENR := LINECNT + 1;
            PAGENR := PAGECNT;
            CONTLINK := NIL;
           END;
          ZPTR^.CONTLINK := PROCPTR^.LAST;
         END
        ELSE
         BEGIN
          PROCCALLPTR := PROCPTR;
          NEW (PROCPTR);
          WITH PROCPTR^ DO
           BEGIN
            PROCNAME := COMP;
            NEXTPROC := NIL;
            ZPTR := FIRST;
            NEW (FIRST);
            WITH FIRST^ DO
             BEGIN
      	LINENR := LINECNT + 1;
      	PAGENR := PAGECNT;
      	CONTLINK := NIL;
             END;
            ZPTR^.CONTLINK := FIRST;
            LAST := FIRST;
           END;
          PROCCALLPTR^.NEXTPROC := PROCPTR;
         END;
       END {FINDPROC} ;
      
      PROCEDURE NEWPROCEDURE;

       BEGIN {NEWPROCEDURE}
        WITH LISTPTR^ DO
         BEGIN
          PROCVAR := PROCDEC;
          NEW (CALLEDBY);
          WITH CALLEDBY^ DO
           BEGIN
            PROCNAME := CURPROC;
            NEXTPROC := NIL;
            ZPTR := FIRST;
            NEW (FIRST);
            WITH FIRST^ DO
             BEGIN
      	LINENR := LINECNT + 1;
      	PAGENR := PAGECNT;
      	CONTLINK := NIL;
             END;
            ZPTR^.CONTLINK := FIRST;          // $$** CRASHPOINT ** $$$
            LAST := FIRST;
           END;
          NEW (CALLED);
          WITH CALLED^ DO
           BEGIN
            PROCNAME := FIRSTNAME ['M'];
            NEXTPROC := NIL;
            ZPTR := FIRST;
            NEW (FIRST);
            WITH FIRST^ DO
             BEGIN
      	LINENR := LINECNT + 1;
      	PAGENR := PAGECNT;
      	CONTLINK := NIL;
             END;
            ZPTR^.CONTLINK := FIRST;
            LAST := FIRST;
           END;
         END;
        NEW (PROCSTRUCL^.NEXTPROC);
        PROCSTRUCL := PROCSTRUCL^.NEXTPROC;
        WITH PROCSTRUCL^ DO
         BEGIN
          PROCNAME := LISTPTR;
          NEXTPROC := NIL;
          LINENR := LINECNT + 1;
          PAGENR := PAGECNT;
          PROCLEVEL := LEVEL;
         END;
       END {NEWPROCEDURE} ;
      
     BEGIN {FINDNAME}
      INDEXCH := SY [1];
      LISTPTR := FIRSTNAME [INDEXCH];
      WHILE LISTPTR <> NIL DO
       BEGIN
        LPTR:= LISTPTR;
        IF SY = LISTPTR^.NAME
        THEN
         BEGIN
          ZPTR := LISTPTR^.LAST;
          NEW (LISTPTR^.LAST);
          WITH LISTPTR^.LAST^ DO
           BEGIN
            LINENR := LINECNT + 1;
            PAGENR := PAGECNT;
            CONTLINK := NIL;
           END;
          ZPTR^.CONTLINK := LISTPTR^.LAST;
          IF LISTPTR^.PROCVAR <> 0
          THEN
           BEGIN
            IF LISTPTR^.PROCVAR = 2
            THEN WHILE CH = ' ' DO
             BEGIN
      	SYLENG := SYLENG + 1;
      	READBUFFER;
             END;
            IF (CH <> ':') OR (LISTPTR^.PROCVAR = 1)
            THEN
             BEGIN
      	PROCPTR := LISTPTR^.CALLEDBY;
      	FINDPROC (CURPROC);
      	PROCPTR := CURPROC^.CALLED;
      	FINDPROC (LISTPTR);
             END
           END
          ELSE
           IF PROCDEC <> 0
           THEN
             BEGIN
      	IF DOUBLEDECF = NIL
      	THEN
      	 BEGIN
      	  NEW (DOUBLEDECF);
      	  DOUBLEDECL := DOUBLEDECF;
      	 END
      	ELSE
      	 BEGIN
      	  NEW (DOUBLEDECL^.NEXTPROC);
      	  DOUBLEDECL := DOUBLEDECL^.NEXTPROC;
      	 END;
      	DOUBLEDECL^.NEXTPROC := NIL;
      	DOUBLEDECL^.PROCORT := LISTPTR;
      	NEWPROCEDURE;
             END;
          GOTO 1;
         END
        ELSE
         IF SY > LISTPTR^.NAME
         THEN
           BEGIN
            LISTPTR:= LISTPTR^.RLINK;
            RIGHT:= TRUE;
           END
         ELSE
           BEGIN
            LISTPTR:= LISTPTR^.LLINK;
            RIGHT:= FALSE;
           END;
       END;
      NEW (LISTPTR);
      WITH LISTPTR^ DO
       BEGIN
        NAME := SY;
        LLINK := NIL;
        RLINK := NIL;
       END;
      IF FIRSTNAME [INDEXCH] = NIL
      THEN FIRSTNAME [INDEXCH] := LISTPTR
      ELSE
       IF RIGHT
       THEN LPTR^.RLINK := LISTPTR
       ELSE LPTR^.LLINK := LISTPTR;
      WITH LISTPTR^ DO
       BEGIN
        NEW (FIRST);
        WITH FIRST^ DO
         BEGIN
          LINENR := LINECNT + 1;
          PAGENR := PAGECNT;
          CONTLINK := NIL;
         END;
        LAST := FIRST ;
        IF PROCDEC = 0
        THEN
         BEGIN
          PROCVAR := 0;
          CALLED := NIL;
          CALLEDBY := NIL;
         END
        ELSE NEWPROCEDURE;
       END;
1:
      PROCDEC := 0;
     END {FINDNAME} ;
    
   BEGIN {INSYMBOL}
    SYLENG := 0;
    WHILE (CH IN ['_','(',' ','{','$','?','}','!','@'])
    AND NOT EOB AND (CH <= '_')  DO
     BEGIN
      IF (CH = '{') OR (CH = '(') AND (BUFFER[BUFFERPTR] = '*')
      THEN
       BEGIN
        OLDSPACESMARK := SPACES;
        IF OLDSPACES
        THEN SPACES := LASTSPACES
        ELSE  LASTSPACES := SPACES;
        SPACES := SPACES + BUFFERPTR - 1;
        OLDSPACES := TRUE;
        IF CH = '{'
        THEN
         REPEAT
          READBUFFER;
         UNTIL (CH = '}') OR EOB
        ELSE
         BEGIN
          PAGEEJECT := (BUFFER[BUFFERPTR+1]='$') AND (BUFFER[BUFFERPTR+2]='P')
      		  AND   (BUFFER[BUFFERPTR+3]='+');     (*** C5 ***)
          REPEAT
           READBUFFER
          UNTIL (CH = ')') AND (BUFFER[BUFFERPTR-2] = '*') OR EOB;
         END;
        SPACES := OLDSPACESMARK;
        OLDSPACES := TRUE;
       END
      ELSE
       IF CH = '('
       THEN GOTO 1;
      READBUFFER;
     END;
    IF CH = ''''
    THEN
     BEGIN
      SYTY := STRGCONST;
       REPEAT
        READBUFFER;
       UNTIL (CH = '''') OR EOB;
      READBUFFER;
     END
    ELSE
     IF (CH IN LETTERS) OR (ORD(CH) > &137)
     THEN
       BEGIN
        SYLENG := 0;
         REPEAT
          SYLENG := SYLENG + 1;
          IF ORD(CH) > &137
          THEN CH := CHR(ORD(CH)-32);
          IF SYLENG <= 10
          THEN SY [SYLENG] := CH;
          READBUFFER;
//         UNTIL NOT ((CH IN (ALPHANUM OR ['_'])) OR (CH > '_'));
         UNTIL NOT ((CH IN (ALPHANUM + ['_'])) OR (CH > '_'));
        FOR I := SYLENG + 1 TO 10 DO SY [I] := ' ';
        IF SYLENG > 10
        THEN CHCNT := 10
        ELSE CHCNT := SYLENG;
        IF NOT RESWORD
        THEN
         BEGIN
          SYTY := IDENT ;
          FINDNAME;
         END
       END
     ELSE
       IF CH IN DIGITS
       THEN
         BEGIN
           REPEAT
            READBUFFER;
           UNTIL NOT (CH IN DIGITS);
          SYTY := INTCONST;
          IF CH = 'B'
          THEN READBUFFER
          ELSE
           BEGIN
            IF CH = '.'
            THEN
             BEGIN
      	 REPEAT
      	  READBUFFER
      	 UNTIL NOT (CH IN DIGITS);
      	SYTY := OTHERSY;
             END;
            IF CH = 'E'
            THEN
             BEGIN
      	READBUFFER;
      	IF CH IN ['+','-']
      	THEN READBUFFER;
      	WHILE CH IN DIGITS DO READBUFFER;
      	SYTY := OTHERSY;
             END;
           END;
         END
       ELSE
         IF CH = '"'
         THEN
           BEGIN
             REPEAT
      	READBUFFER
//             UNTIL NOT (CH IN  (DIGITS OR ['A'..'F']));
             UNTIL NOT (CH IN  (DIGITS + ['A'..'F']));
            SYTY := INTCONST;
           END
         ELSE
           IF CH <> ' '
           THEN
             BEGIN
      	1
:
      	SYTY := DELSY [CH];
      	READBUFFER;
      	IF SYTY = LPARENT
      	THEN LPARENTHESE
      	ELSE
      	 IF (SYTY = COLON) AND (CH = '=')
      	 THEN
      	   BEGIN
      	    SYTY := OTHERSY;
      	    READBUFFER;
      	   END;
             END
           ELSE SYTY := EOBSY;
   END {INSYMBOL} ;
  
  PROCEDURE RECDEF;

  VAR
    OLDSPACESMARK  : INTEGER;         {ALTER ZEICHENVORSCHUB BEI FORMATIERUNG VON RECORDS}


    PROCEDURE CASEDEF;

    VAR
      OLDSPACESMARK  : INTEGER;       {ALTER ZEICHENVORSCHUB BEI FORMATIERUNG VON VARIANT PARTS}


      PROCEDURE PARENTHESE;

      VAR
        OLDSPACESMARK : INTEGER;      {ALTER ZEICHENVORSCHUB BEI FORMATIERUNG VON KLAMMERN INNERHALB VON VARIANT PARTS}

       BEGIN {PARENTHESE}
        OLDSPACESMARK := SPACES;
        IF OLDSPACES
        THEN SPACES := LASTSPACES
        ELSE LASTSPACES := SPACES;
        SPACES := SPACES + BUFFERPTR - 2;
        OLDSPACES := TRUE;
         REPEAT
          INSYMBOL;
           CASE SYTY OF
            LBRACK : PARENTHESE;
            CASESY : CASEDEF;
            RECORDSY : RECDEF
           END;
         UNTIL SYTY IN [RPARENT,EOBSY];
        SPACES := OLDSPACESMARK;
        OLDSPACES := TRUE;
        INSYMBOL;
       END {PARENTHESE} ;


     BEGIN {CASEDEF}
      DELSY ['('] := LBRACK;
      OLDSPACESMARK := SPACES;
      IF OLDSPACES
      THEN SPACES := LASTSPACES
      ELSE LASTSPACES := SPACES;
      SPACES := BUFFERPTR - BUFFMARK + SPACES - SYLENG + 3;
      OLDSPACES := TRUE;
       REPEAT
        INSYMBOL ;
         CASE SYTY OF
          LBRACK : PARENTHESE;
          CASESY : CASEDEF;
          RECORDSY: RECDEF
         END;
       UNTIL SYTY IN [ENDSY,RPARENT,EOBSY];
      SPACES := OLDSPACESMARK;
      DELSY ['('] := LPARENT;
     END {CASEDEF} ;

   BEGIN {RECDEF}
    OLDSPACESMARK := SPACES;
    SPACES := BUFFERPTR - BUFFMARK + SPACES - SYLENG - 2 + FEED;
    OLDSPACES := TRUE;
    INSYMBOL;
    WRITELINE ( BUFFERPTR-SYLENG);
     REPEAT
       CASE SYTY OF
        CASESY : CASEDEF;
        RECORDSY : RECDEF;
        OTHERSY  : INSYMBOL
       END;
     UNTIL SYTY IN [ENDSY,EOBSY];
    WRITELINE (BUFFERPTR-SYLENG);
    OLDSPACES := TRUE;
    LASTSPACES := SPACES - FEED;
    SPACES := OLDSPACESMARK;
    INSYMBOL;
   END {RECDEF} ;
  
  PROCEDURE ERROR (ERRNR : INTEGER);

   BEGIN {ERROR}
    ERRFLAG := TRUE;
    WRITELINE (BUFFERPTR);
    WRITE (' ':17,' **** ');
     CASE ERRNR OF
      1 : WRITELN (SY,' ? ? ? ',MESSAGE);
      2 : WRITELN ('MISSING ''END'' OR ''UNTIL'' NUMBER ',EMARKNR : 4);
      3 : WRITELN ('MISSING ''THEN'' NUMBER ',EMARKNR : 4);
      4 : WRITELN ('MISSING ''OF'' TO ''CASE'' NUMBER ',BMARKNR : 4);
      5 : WRITELN (' ONLY ONE ''EXIT'' ALLOWED');
      6 : WRITELN ('MISSING ''EXIT'' IN ''LOOP'' ',EMARKNR : 4)
     END;
   END {ERROR} ;

  PROCEDURE STATEMENT (IFFLAG : BOOLEAN);

  VAR
    CURBLOCKNR : INTEGER;     {AKTUELLE BLOCKNUMMER}



    PROCEDURE COMPSTAT;

     BEGIN {COMPSTAT}
      BMARKTEXT := 'B';
      OLDSPACES := TRUE;
      LASTSPACES := SPACES - BACKFEED;
      INSYMBOL;
      WRITELINE (BUFFERPTR-SYLENG);
//       LOOP
       while true do begin
         REPEAT
          STATEMENT (FALSE);
         UNTIL SYTY IN ENDSYM;
//       EXIT IF SYTY IN [ENDSY,EOBSY,PROCEDURESY,FUNCTIONSY];
       IF SYTY IN [ENDSY,EOBSY,PROCEDURESY,FUNCTIONSY] then break;
        ERROR (1);
        INSYMBOL ;
       END;
      WRITELINE (BUFFERPTR-SYLENG);
      EMARKTEXT := 'E';
      EMARKNR := CURBLOCKNR;
      LASTSPACES := SPACES-BACKFEED;
      OLDSPACES := TRUE;
      IF SYTY = ENDSY
      THEN
       BEGIN
        INSYMBOL ;
        WRITELINE (BUFFERPTR-SYLENG);
       END
      ELSE ERROR (2);
     END {COMPSTAT} ;
    
    PROCEDURE CASESTAT;

    VAR
      OLDSPACESMARK : INTEGER;        {ALTER ZEICHENVORSCHUB BEI FORMATIERUNG VON CASE-STATEMENTS}

     BEGIN {CASESTAT}
      BMARKTEXT := 'C';
      OLDSPACES := TRUE;
      LASTSPACES := SPACES-BACKFEED;
      INSYMBOL;
      STATEMENT (FALSE);
      IF SYTY = OFSY
      THEN WRITELINE (BUFFERPTR)
      ELSE ERROR (3);
//       LOOP
        while true do begin
         REPEAT
           REPEAT
            INSYMBOL ;
//           UNTIL SYTY IN (ENDSYM OR [COLON]);
           UNTIL SYTY IN (ENDSYM + [COLON]);
          IF SYTY = COLON
          THEN
           BEGIN
            OLDSPACESMARK := SPACES;
            LASTSPACES := SPACES;
            SPACES := BUFFERPTR - BUFFMARK + SPACES - 2;
            OLDSPACES := TRUE;
            INSYMBOL;
            STATEMENT (FALSE);
            SPACES := OLDSPACESMARK;
           END;
         UNTIL SYTY IN ENDSYM;
//       EXIT IF SYTY IN [ENDSY,EOBSY,PROCEDURESY,FUNCTIONSY];
       IF SYTY IN [ENDSY,EOBSY,PROCEDURESY,FUNCTIONSY] then break;
        ERROR (1);
       END;
      WRITELINE (BUFFERPTR-SYLENG);
      EMARKTEXT := 'E';
      EMARKNR := CURBLOCKNR;
      LASTSPACES := SPACES-BACKFEED;
      OLDSPACES := TRUE;
      IF SYTY = ENDSY
      THEN
       BEGIN
        INSYMBOL ;
        WRITELINE (BUFFERPTR-SYLENG);
       END
      ELSE ERROR (2);
     END {CASESTAT} ;
    
    PROCEDURE LOOPSTAT;

    VAR
      LOOPFLAG : BOOLEAN;     {GESETZT BEIM AUFTRETEN VON EXIT-STATEMENTS}

     BEGIN {LOOPSTAT}
      BMARKTEXT := 'L';
      OLDSPACES := TRUE;
      LASTSPACES := SPACES - BACKFEED;
      INSYMBOL;
      WRITELINE (BUFFERPTR-SYLENG);
      LOOPFLAG := FALSE;
//      LOOP
      while true do begin
         REPEAT
          STATEMENT (FALSE);
          IF SYTY = EXITSY
          THEN
           BEGIN
            WRITELINE (BUFFERPTR-SYLENG);
            IF LOOPFLAG
            THEN ERROR (5);
            OLDSPACES := TRUE;
            LASTSPACES := SPACES-BACKFEED;
            LOOPFLAG := TRUE;
            EMARKTEXT := 'X';
            EMARKNR := CURBLOCKNR;
            INSYMBOL; INSYMBOL;
           END;
         UNTIL SYTY IN ENDSYM;
//       EXIT IF SYTY IN [ENDSY,EOBSY,PROCEDURESY,FUNCTIONSY];
       IF SYTY IN [ENDSY,EOBSY,PROCEDURESY,FUNCTIONSY] then break;
        ERROR (1);
        INSYMBOL ;
       END;
      WRITELINE (BUFFERPTR-SYLENG);
      EMARKTEXT := 'E';
      EMARKNR := CURBLOCKNR;
      LASTSPACES := SPACES-BACKFEED;
      OLDSPACES := TRUE;
      IF SYTY = ENDSY
      THEN
       BEGIN
        INSYMBOL ;
        WRITELINE (BUFFERPTR-SYLENG);
       END
      ELSE ERROR (2);
      IF NOT LOOPFLAG
      THEN ERROR (6);
     END {LOOPSTAT} ;
    
    PROCEDURE IFSTAT (IFVAR : BOOLEAN);

     BEGIN {IFSTAT}
      BMARKTEXT := 'I';
      IF NOT IFVAR
      THEN
       BEGIN
        SPACES := SPACES - FEED; LASTSPACES := SPACES
       END
      ELSE LASTSPACES := SPACES - BACKFEED;
      OLDSPACES := TRUE;
      INSYMBOL;
      STATEMENT (FALSE);
      IF SYTY = THENSY
      THEN
       BEGIN
        WRITELINE (BUFFERPTR-SYLENG);
        IF IFVAR
        THEN LASTSPACES := SPACES - BACKFEED
        ELSE LASTSPACES := SPACES;
        OLDSPACES := TRUE;
        EMARKTEXT := 'T';
        EMARKNR := CURBLOCKNR;
        INSYMBOL;
        STATEMENT (TRUE);
       END
      ELSE ERROR (4);
      IF SYTY = ELSESY
      THEN
       BEGIN
        WRITELINE (BUFFERPTR-SYLENG);
        EMARKTEXT := 'S';
        EMARKNR := CURBLOCKNR;
        IF IFVAR
        THEN LASTSPACES := SPACES - BACKFEED
        ELSE LASTSPACES := SPACES;
        OLDSPACES := TRUE;
        INSYMBOL;
        STATEMENT (TRUE);
       END;
      IF NOT IFVAR
      THEN SPACES := SPACES + FEED
     END {IFSTAT} ;


    PROCEDURE LABELSTAT;

     BEGIN {LABELSTAT}
      LASTSPACES := 0;
      OLDSPACES := TRUE;
      INSYMBOL;
      WRITELINE (BUFFERPTR-SYLENG);
     END {LABELSTAT} ;
    
    PROCEDURE REPEATSTAT;

     BEGIN {REPEATSTAT}
      BMARKTEXT := 'R';
      OLDSPACES := TRUE;
      LASTSPACES := SPACES - BACKFEED;
      INSYMBOL ;
      WRITELINE (BUFFERPTR-SYLENG);
//       LOOP
       while true do begin
         REPEAT
          STATEMENT (FALSE);
         UNTIL SYTY IN ENDSYM;
//       EXIT IF SYTY IN [UNTILSY,EOBSY,PROCEDURESY,FUNCTIONSY];
        IF SYTY IN [UNTILSY,EOBSY,PROCEDURESY,FUNCTIONSY] then break;
        ERROR (1);
        INSYMBOL ;
       END;
      WRITELINE (BUFFERPTR-SYLENG);
      EMARKTEXT := 'U';
      EMARKNR := CURBLOCKNR;
      OLDSPACES := TRUE;
      LASTSPACES := SPACES-BACKFEED;
      IF SYTY = UNTILSY
      THEN
       BEGIN
        INSYMBOL;
        STATEMENT (FALSE);
       END
      ELSE ERROR (2);
     END {REPEATSTAT} ;


   BEGIN {STATEMENT}
    IF SYTY = INTCONST
    THEN
     BEGIN
      INSYMBOL;
      IF SYTY = COLON
      THEN LABELSTAT;
     END;
    IF SYTY IN BEGSYM
    THEN
     BEGIN
      BLOCKNR := BLOCKNR + 1;
      CURBLOCKNR := BLOCKNR;
      BMARKNR := CURBLOCKNR;
      WRITELINE (BUFFERPTR-SYLENG);
      SPACES := SPACES + FEED;
       CASE SYTY OF
        BEGINSY : COMPSTAT;
        LOOPSY  : LOOPSTAT;
        CASESY  : CASESTAT;
        IFSY    : IFSTAT (IFFLAG);
        REPEATSY : REPEATSTAT
       END;
      SPACES := SPACES - FEED;
     END
//    ELSE WHILE NOT (SYTY IN ([SEMICOLON] OR ENDSYM)) DO INSYMBOL;

      ELSE
      WHILE NOT (SYTY IN ([SEMICOLON] + ENDSYM)) DO
         INSYMBOL;

    IF SYTY = SEMICOLON
    THEN INSYMBOL
    ELSE
     IF SYTY = DOSY
     THEN
       BEGIN
        INSYMBOL;
        STATEMENT (FALSE);
       END;
   END {STATEMENT} ;
  
 BEGIN {BLOCK}
  DOUBLEDECF := NIL;
  LEVEL := LEVEL + 1;
  CURPROC := LISTPTR;
  SPACES := LEVEL * FEED;
   REPEAT
    INSYMBOL
   UNTIL (SYTY IN RELEVANTSYM);
  WHILE SYTY IN DECSYM DO
   BEGIN
    WRITELINE (BUFFERPTR-SYLENG);
    SPACES := SPACES - FEED;
    WRITELINE (BUFFERPTR);
    SPACES := SPACES + FEED;
     REPEAT
      INSYMBOL ;
      IF SYTY = RECORDSY
      THEN RECDEF;
     UNTIL SYTY IN RELEVANTSYM;
   END;
  WHILE SYTY IN PROSYM DO
   BEGIN
    WRITELINE (BUFFERPTR-SYLENG);
    OLDSPACES := TRUE;
    IF SYTY <> INITPROCSY
    THEN
     BEGIN
      IF SYTY = PROCEDURESY
      THEN PROCDEC := 1              // Procedure declaration
      ELSE PROCDEC := 2;             // Function declaration
      INSYMBOL;
     END;
    BLOCK;
    IF SYTY = SEMICOLON
    THEN INSYMBOL;
   END;
  LEVEL := LEVEL - 1;
  SPACES := LEVEL * FEED;
  IF NOT (SYTY IN [BEGINSY,FORWARDSY,EXTERNSY])
  THEN
   BEGIN
    ERROR (1);
    WHILE NOT (SYTY IN [BEGINSY,FORWARDSY,EXTERNSY,EOBSY]) DO INSYMBOL
   END;
  IF SYTY = BEGINSY
  THEN STATEMENT (FALSE)
  ELSE INSYMBOL;
  IF DOUBLEDECF <> NIL
  THEN
   REPEAT
    DOUBLEDECF^.PROCORT^.PROCVAR := 0;
    DOUBLEDECF := DOUBLEDECF^.NEXTPROC;
   UNTIL  DOUBLEDECF = NIL;
  IF LEVEL = 0
  THEN
   BEGIN
    IF SYTY <> POINT
    THEN
     BEGIN
      WRITELN (TTY,'MISSING POINT AT PROGRAM END');
      WRITELN (TTY);
      WRITELN (' ' : 17, ' **** MISSING POINT AT PROGRAM END ****');
      INSYMBOL;
     END;
    IF SYTY <> EOBSY
    THEN
     REPEAT
      INSYMBOL
     UNTIL SYTY = EOBSY;
   END;
 END {BLOCK} ;

PROCEDURE PRINTLISTE;

VAR
  FIRSTPROC,LASTPROC, {ZEIGER ZUM DURCHHANGELN DURCH DIE BAEUME UND LISTEN BEIM AUSDRUCKEN}
  PRED : LISTPTRTY;
  INDEXCH : CHAR;     {LAUFVARIABLE FUER DAS FELD 'FIRSTNAME' ZUM AUSDRUCKEN}



  PROCEDURE WRITELINENR (SPACES : INTEGER);

  VAR
    LINK : LINEPTRTY; {ZEIGER ZUM DURCHHANGELN DURCH DIE VERKETTUNG DER ZEILENNUMMERN}
    COUNT : INTEGER;  {ZAEHLT DIE GEDRUCKTEN ZEILENNUMMERN PRO ZEILE}

   BEGIN {WRITELINENR}
    COUNT := 0;
    LINK := LISTPTR^.FIRST;
     REPEAT
      IF COUNT > (MAXCH - SPACES) DIV 6
      THEN
       BEGIN
        WRITELN;
        WRITE (' ' : SPACES);
        COUNT := 0;
       END;
      COUNT := COUNT + 1;
      WRITE (LINK^.LINENR * INCREMENT : 6);
      LINK := LINK^.CONTLINK;
     UNTIL LINK = NIL;
   END {WRITELINENR} ;
  
 BEGIN {PRINTLISTE}
  FIRSTPROC := NIL;
  LASTPROC := NIL;
  WITH FIRSTNAME ['M']^ DO
  IF RLINK = NIL
  THEN FIRSTNAME ['M'] := LLINK
  ELSE
   BEGIN
    LISTPTR := RLINK;
    WHILE LISTPTR^.LLINK <> NIL DO LISTPTR := LISTPTR^.LLINK;
    LISTPTR^.LLINK := LLINK;
    FIRSTNAME ['M'] := RLINK;
   END;
  INDEXCH := 'A';
  WHILE (INDEXCH < 'Z') AND (FIRSTNAME [INDEXCH] = NIL) DO INDEXCH := SUCC (INDEXCH);
  IF FIRSTNAME [INDEXCH] <> NIL
  THEN
   BEGIN
    PAGE;
    WRITELN ('CROSS REFERENCE LIST OF IDENTIFIERS');
    WRITELN ('***********************************');
    FOR INDEXCH := INDEXCH TO 'Z' DO
    WHILE FIRSTNAME [INDEXCH] <> NIL DO
     BEGIN
      LISTPTR := FIRSTNAME [INDEXCH];
      WHILE LISTPTR^.LLINK <> NIL DO
       BEGIN
        PRED := LISTPTR;
        LISTPTR := LISTPTR^.LLINK;
       END;
      IF LISTPTR = FIRSTNAME [INDEXCH]
      THEN FIRSTNAME [INDEXCH] := LISTPTR^.RLINK
      ELSE PRED^.LLINK := LISTPTR^.RLINK;
      IF LISTPTR^.CALLED <> NIL
      THEN
       BEGIN
        IF FIRSTPROC = NIL
        THEN
         BEGIN
          FIRSTPROC := LISTPTR;
          LASTPROC := FIRSTPROC;
          LASTPROC^.CALLED^.PROCNAME := NIL;
         END
        ELSE
         BEGIN
          LASTPROC^.CALLED^.PROCNAME := LISTPTR;
          LASTPROC := LISTPTR;
         END;
       END;
      WRITELN;
      WRITE (LISTPTR^.NAME : 11);
      WRITELINENR (11);
     END;
    IF FIRSTPROC <> NIL
    THEN
     BEGIN
      PAGE;
      WRITELN ('LIST OF PROCEDURE CALLS');
      WRITELN ('***********************');
      LASTPROC^.CALLED^.PROCNAME := NIL;
      LASTPROC := FIRSTPROC;
      WHILE LASTPROC <> NIL DO
       BEGIN
        LISTPTR :=LASTPROC;
        WRITELN;WRITELN;
        WRITE (LASTPROC^.NAME:11, ' IS CALLED FROM :');
        WITH LASTPROC^ DO
         REPEAT
          WRITELN;
          WRITE (' ' : 11,CALLEDBY^.PROCNAME^.NAME:11);
          LISTPTR^.FIRST := CALLEDBY^.FIRST;
          WRITELINENR (22);
          CALLEDBY := CALLEDBY^.NEXTPROC;
         UNTIL CALLEDBY = NIL;
        WRITELN; WRITELN;
        IF LASTPROC^.CALLED^.NEXTPROC <> NIL
        THEN
         BEGIN
          WRITE (' ' : 11, ' AND CALLS :');
          WITH LASTPROC^.CALLED^ DO
           REPEAT
            WRITELN;
            WRITE (' ' : 11,NEXTPROC^.PROCNAME^.NAME:11);
            LISTPTR^.FIRST := NEXTPROC^.FIRST;
            WRITELINENR (22);
            NEXTPROC := NEXTPROC^.NEXTPROC;
           UNTIL NEXTPROC = NIL;
         END;
        LASTPROC := LASTPROC^.CALLED^.PROCNAME;
       END;
      PAGE;
      WRITELN ('LIST OF PROCEDURE NESTINGS');
      WRITELN ('**************************');
      PROCSTRUCL := PROCSTRUCF;
       REPEAT
        WRITELN;
        WITH PROCSTRUCL^ DO
        WRITE (' ':PROCLEVEL*3,PROCNAME^.NAME : 11,LINENR * INCREMENT : 6 );
        PROCSTRUCL := PROCSTRUCL^.NEXTPROC;
       UNTIL PROCSTRUCL = NIL;
      WRITELN;
     END;
   END;
 END {PRINTLISTE} ;

PROCEDURE READFILENAME;

// LABEL 999;
// this is being used for break

TYPE   LINETYP = PACKED ARRAY [1..80] OF CHAR;

VAR
  LEGALCHAR : SET OF CHAR;    {MENGE DER LEGALEN EINGABEZEICHEN}
  MAXINDEX : INTEGER;         {MAXIMALER INDEX FUER DIE FUELLUNG DES FELDES 'FILENAME'}
  LINE: LINETYP;
  LEN,IL: INTEGER;


//  PROCEDURE  GCML ( VAR S: LINETYP;  VAR LEN: INTEGER );   EXTERN;
// This is equivalent to ParamCount and ParamStr

 // This procedure parses a file name into [Proj,Prog]File.Nam as
 // used bt the PDP 11; it has been superceded

 BEGIN {READFILENAME}

 // most of this procedure is unnecessary

  // IOS := [SPOOL];
  IL := 5;       EOB := TRUE;

 // GCML ( LINE, LEN );
   len := 0;
 {
  IF LEN < 3 THEN
      BEGIN}
          WRITE('file: ');
 //         BREAK;
 // This means to stop buffering terminal output
 // for interactive communications
 //         READLN(TTY);
          readln ( LINE );

//          IF EOF(TTY) THEN exit; // GOTO 999;
         if line='' THEN exit; // GOTO 999;
//          READ ( TTY, LINE );
{
          LEN := 80;
          IL := 1;
          EOB := FALSE;
      END;
   PIX := 0;
   I := 1;
   WITH INPUTFILE DO
   BEGIN
       FILENAME := '                    ';
       DEV := FILENAME;
       UIC := DEV
   END;
   OUTPUTFILE := INPUTFILE;
   WHILE LINE[IL] = ' ' DO
      IL := IL + 1;
   WITH INPUTFILE DO
      WHILE (I <=20) AND (IL <= LEN) DO
      BEGIN
          CH := LINE[IL];
          IL := IL + 1;
          IF CH = '.'  THEN
              PIX:=I;
          IF ((CH=' ') OR (CH=';')) AND (PIX=0) THEN
          BEGIN
              FOR PIX:=0 TO 3 DO
                 FILENAME[PIX+I]:=PAS[PIX];
              PIX:=I;
              I:=I+4;
          END;
       IF CH = '/' THEN
        BEGIN
            IF (LINE[IL]='-') AND (LINE[IL+1]='S') AND (LINE[IL+2]='P') THEN
             BEGIN
              // IOS := [];
                 IL := 81;
             END;
        END
       ELSE
        BEGIN
            FILENAME[I]:=CH;
            I := I + 1;
        END;
        IF CH = ':' THEN
          BEGIN
             DEV := FILENAME;
             FILENAME := UIC;
             PIX := 0;
             I := 1;
          END;
        IF (CH = ']') OR (CH = '>') THEN
         BEGIN
             UIC := FILENAME;
             FILENAME := '                    ';
             PIX := 0;
             I := 1;
         END;
      END;
   IF PIX = 0 THEN
    BEGIN
        PIX := I;
        WITH INPUTFILE DO
         FOR I:=0 TO 3 DO
           FILENAME[PIX+I] := PAS[I];
   END;
   OUTPUTFILE.FILENAME := INPUTFILE.FILENAME;
   WITH OUTPUTFILE DO
    BEGIN
        FOR I:=0 TO 3 DO
            FILENAME[PIX+I] := NUE[I];
         FOR I:=PIX+4 TO 20 DO
            FILENAME[I]:=' ';
    END;
// 999:
}
 END {READFILENAME} ;

BEGIN {MAIN}
INIT;
write('File: ');
readln (inputfile.FileName);
assign(input,inputfile.filename);
Reset(input);
Assign(TTY,'CON:'); rewrite(TTY);
(*
 WITH INPUTFILE DO
// LOOP
while true do begin
  READFILENAME;
  IF EOF(TTY) THEN HALT; // GOTO 99;
  // RESET (INPUT,FILENAME,UIC,DEV);

    Assign(Input,FileName);
      {$I-} reset(INPUT); {$I+}
    if IOResult =0 then break;
    Writeln;
    Writeln(FileName,' not found.');
    WRITE ('Give new ');

//  IF EOB AND EOF(INPUT) THEN HALT; // GOTO 99;
// EXIT IF NOT (EOF (INPUT));
{ IF NOT (EOF (INPUT)) then break;
  WRITELN (TTY);
  WRITE(TTY,FILENAME );
  WRITELN (TTY,' NOT FOUND');
  WRITE (TTY,'GIVE NEW ');
  }
 END;
 *)

WRITELN (TTY);
WRITELN (TTY,VERSION);
WRITELN (TTY);
EOB := FALSE;
MAXINC := 29999 DIV INCREMENT ;
WITH OUTPUTFILE DO
 BEGIN
 // REWRITE (NEWFIL,FILENAME);
  { I'll tuen this off until I'm certain I want it
    to write files
   Assign(NEWFIL,FILENAME);
  }
   Assign(NEWFIL,'NUL:');
   Rewrite(Newfil);
   FOR I:=0 TO 3 DO
      FILENAME[PIX+I] := CRL[I];
//  REWRITE (OUTPUT,FILENAME,,,IOS);
{ Same thing, we'll see about it
   Assign(OUTPUT,FILENAME);
   Rewrite(Output);
   }
 END;
CH := ' ';
DATUM;
HEADER;
BLOCK;
WRITELINE (BUFFLEN+2);
IF NOT ERRFLAG
THEN WRITE (TTY,'NO ');
WRITELN (TTY,MESSAGE);
PRINTLISTE;
// 99:
   WRITELN;
END {MAIN} .

