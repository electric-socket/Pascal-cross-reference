
Program Mcross;
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
  2021-01-05   9   Ran program through PTOP to reformat and to
                      make program easier to understand
 *)
// LABEL
// 99;
// This is being used for EXIT procedure

Const 
  VERSION = 'CROSS VERSION OF OCT -79';      {DATE OF LAST MODIFICATION
                                               DATUM DER LETZTEN AENDERUNG}
  FEED = 2;                                  {CHARACTER FEED FOR PROCEDURES
                                               AND BLOCKS
                                              ZEICHENVORSCHUB BEI PROZEDUREN
                                                UND BLOECKEN}
  MAXCH = 72;                                {MAXIMUM NUMBER OF CHARACTERS
                                                PER PRINT LINE
                                              MAXIMALE ZAHL VON ZEICHEN
                                                PRO DRUCKZEILE}
  BACKFEED = 1;                              {CHARACTER PRETENSION AT
                                               'PROCEDURE','BEGIN', ETC.
                                              ZEICHENVORRUECKUNG BEI
                                               'PROCEDURE','BEGIN' ETC.}
  MAXLINE = 40;                       {MAXIMUM NUMBER OF LINES PER PRINT PAGE
                                       MAXIMALE ZAHL VON ZEILEN PRO DRUCKSEITE}
  HT = &11;                             {ASCII HORIZONTAL TAB}
  LF = &12;                             {ASCII LINE FEED}
  FF = &14;                             {ASCII FORM FEED}
  CR = &15;                             {ASCII CARRIAGE RETURN}


  // Reserved Words
Const
    MaxResSize  =  14; // 2..MaxResSize+1 letters in identifiers
    MaxResWords = 100; // Max possiblr keywords
    MaxModifiers= 300; // modifiers are only keywords in some circumstances


Type 
  LINEPTRTY = ^LINE;
  LISTPTRTY = ^LIST;
  PROCCALLTY = ^PROCCALL;
  PROCSTRUCTY = ^PROCSTRUC;
  LINENRTY = 0..&7777;
  PAGENRTY = 0..&77;
  STR5 = PACKED ARRAY [1..5] Of CHAR;

  LINE = PACKED Record                        {DESCRIPTION OF LINE NUMBERS
                                               BESCHREIBUNG DER ZEILENNUMMERN}
    LINENR : LINENRTY;                        {Line number
                                               ZEILENNUMMER}
    PAGENR : PAGENRTY;                        {Page number
                                               SEITENNUMMER}
    CONTLINK : LINEPTRTY                      {NEXT LINE NUMBER RECORD
                                               NAECHSTER ZEILENNUMMERNRECORD}
  End;

    LIST = PACKED Record                        {DESCRIPTION OF IDENTIFIERS
                                               BESCHREIBUNG VON IDENTIFIERN}
    NAME : String;                              {Name of identifier
                                               NAME DES IDENTIFIERS}
    LLINK ,                                   {LEFT SUCCESSOR IN TREE
                                               LINKER NACHFOLGER IN BAUM}
    RLINK : LISTPTRTY;                        {Right successor in tree
                                               RECHTER NACHFOLGER IM BAUM}
    FIRST ,                                   {POINTER TO FIRST LINE
                                                 NUMBER RECORD
                                               ZEIGER AUF ERSTEN
                                                 ZEILENNUMMERNRECORD}
    LAST  : LINEPTRTY;                        {Pointer to last line number
                                                 record
                                               ZEIGER AUF LETZTEN
                                                 ZEILENNUMMERNRECORD}
    PROCVAR : 0..2;                           {0=NO PROCEDURE/ 1=PROCEDURE/
                                               2=FUNCTION
                                               0=KEINE PROZEDUR/ 1=PROZEDUR/
                                               2=FUNKTION}
    CALLED,                                   {POINTER TO THE FIRST PROCEDURE
                                                 CALLED BY IT
                                               ZEIGER AUF DIE ERSTE PROZEDUR DIE
                                                 VON DIESER GERUFEN WIRD}
    CALLEDBY : PROCCALLTY                     {POINTER TO FIRST CALLING
                                                 PROCEDURE
                                               ZEIGER AUF ERSTE RUFENDE
                                                 PROZEDUR}
  End;

      PROCCALL = PACKED Record                  {DESCRIPTION OF PROCEDURE CALLS
                                             BESCHREIBUNG VON PROZEDURAUFRUFEN}
    PROCNAME : LISTPTRTY;                   {POINTER TO THE CORRESPONDING
                                               IDENTIFIER RECORD
                                             ZEIGER AUF DEN ZUGEHOERIGEN
                                               IDENTIFIERRECORD}
    NEXTPROC : PROCCALLTY;                  {POINTER TO THE NEXT PROCEDURE
                                             ZEIGER AUF DIE NAECHSTE PROZEDUR}
    FIRST,                                  {LINE NUMBER RECORD FOR THE
                                               FIRST CALL
                                             ZEILENNUMMERNRECORD FUER DEN
                                               ERSTEN AUFRUF}
    LAST : LINEPTRTY                        {LINE NUMBEER RCORD FOR THE
                                               LAST CALL
                                             ZEILENNUMMERNRECORD FUER DEN
                                               LETZTEN AUFRUF}
  End;
  
  PROCSTRUC = PACKED Record                  {DESCRIPTION OF PROCEDURE NESTING
                                              BESCHREIBUNG DER
                                                PROZEDURVERSCHACHTELUNG}
    PROCNAME : LISTPTRTY;                    {POINTER TO THE ASSOCIATED
                                                IDENTIFIER
                                              ZEIGER AUF DEN ZUGERHOERIGEN
                                                IDENTIFIER}
    NEXTPROC : PROCSTRUCTY;                  {POINTER TO THE PROCEDURE
                                                DECLARED NEXT
                                              ZEIGER AUF DIE NAECHSTD
                                                DEKLARIERTE PROZEDUR}
    LINENR : LINENRTY;                       {PROCEDURE DEFINITION
                                                LINE NUMBER
                                              ZEILENNUMMER DER
                                                PROZEDURDEFINITION}
    PAGENR ,                             {PROCEDURE DEFINITION PAGE NUMBER
                                          SEITENNUMMER DER PROZEDURDEFINITION}
    PROCLEVEL: PAGENRTY                  {NESTING DEPTH OF PROCEDURE
                                         VERSCHACHTELUNGSTIEFE DER PROZEDUR}
  End;



  SYMBOL = (LABELSY,CONSTSY,TYPESY,VARSY,UNITSY,USESSY,                       {DECSYM}
            FUNCTIONSY,PROCEDURESY,INITPROCSY,                  {PROSYM}
            ENDSY,UNTILSY,ELSESY,THENSY, EXITSY,OFSY,DOSY,EOBSY, {ENDSYMBOLS}
            BEGINSY,CASESY,LOOPSY,REPEATSY,IFSY,                {BEGSYM}
            RECORDSY,FORWARDSY,OTHERSY,INTCONST,IDENT,STRGCONST,EXTERNSY,
            RPARENT,SEMICOLON,POINT,LPARENT,COLON,LBRACK,
            CLASSSY,OBJECTSY,PROGRAMSY,INTERFACESY,DESTRUCTORSY,
            CONSTRUCTORSY,IMPLEMENTATIONSY,
            OTHERSSY{DELIMITER});


      FullString =  UnicodeString;
      // these two will be fleshed out later
      affecttype = (noeffect, consteffect, beforeprocedural,afterprocedural,
                   other);
      modifier = (virtualmod,propertymod);

      ModEntry = Record
               Affects: set of affecttype;
               Which: Modifier;
               End;

      DOUBLEDEC = PACKED Record                  {PROCEDURES THAT ARE ALSO
                                                    DEFINED AS NORMAL ID.
                                                  PROZEDUREN DIE AUCH ALS
                                                    NORMALE ID. DEFINIERT WURDEN}
        PROCORT : LISTPTRTY;                     {POINTER TO THE PROCEDURE
                                                  ZEIGER AUF DIE PROZEDUR}
        NEXTPROC: ^DOUBLEDEC                     {NEXT DOUBLE-DECLARED PROCEDURE
                                                  NAECHSTE DOPPELT DEKLARIERTE
                                                    PROZEDUR}
      End;

    Var

      RESNUM  : ARRAY [1..MaxResSize] Of INTEGER;
                                      {num. of letters -1 in reserve words
                                       STARTADRESSEN FUER DIE RESERVIERTEN
                                         WORTE BESTIMMTER LAENGE}
      RESLIST : ARRAY [1..MaxResWords] Of FullString;
                                          {List of reserved (key)words
                                           LISTE DER RESERVIERTEN WORTE}
      RESSY: ARRAY [1..MaxResWords] Of SYMBOL;    {Symbol different
                                                   reserved words means
                                                   TYPENLISTE DER
                                                     RESERVIERTEN WORTE}

  PIX,
  CommentNest,                           {Nesting level of comments }
  I,                                     {Loop variable
                                          SCHLEIFENVARIABLE}
  BUFFLEN,                               {LENGTH OF THE DESCRIBED PART OF
                                            THE INPUT BUFFER
                                          LAENGE DES BESCHRIEBENEN TEILS
                                            DES EINGABEPUFFERS}
  BUFFMARK,                              {LENGTH OF THE ALREADY PRINTED
                                            PART OF THE BUFFER
                                          LAENGE DES SCHON GEDRUCKTEN
                                            TEIL DES PUFFERS}
  BUFFERPTR,                             {POINTER TO THE NEXT CHARACTER
                                            TO READ IN THE BUFFER
                                          ZEIGER AUF DAS NAECHSTE ZU
                                            LESENDE ZEICHEN IM PUFFER}
  BMARKNR,                               {NUMBER TO BE PRINTED FOR MARKING
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
  PAGECNT2,                            {COUNTS PRINT PAGES PER SOS PAGE
                                         ZAEHLT DIE DRUCKSEITEN PRO SOS-SEITE}
  INCREMENT,                           {PARAMETERS FOR INCREASING LINE NUMBERS
                                        PARAMETER FUER DIE ERHOEHUNG
                                          DER ZEILENNUMMERN}
  MAXINC,                              {LARGEST ALLOWED LINE NUMBER
                                          GROESSTE ERLAUBTE ZEILENNUMMER}
  REALLINCNT,                          {COUNT OF LINES PER PRINT PAGE
                                        ZAEHLT DIE ZEILEN PRO DRUCKSEITE}
  LINECNT : INTEGER;                   {COUNTS THE LINES PRINTED THIS PAGE
                                        ZAEHLT DIE ZEILEN PRO SOS-SEITE}
  INPUTFILE,                           {DESCRIPTION OF THE INPUT FILE
                                        BESCHREIBUNG DES EINGABEFILES}
  OUTPUTFILE : Record                  {DESCRIPTION OF THE OUTPUT FILE
                                        BESCHREIBUNG DES AUSGABEFILES}
    // these 2 are only used by DEC timesharing operating systems
{    DEV,UIC: : PACKED ARRAY [1..20] Of CHAR}    (*** C7 ***)
    FILENAME: FullString;
  End;
  // file extensions: pascal, listing, cross-reference
  PAS,NUE,CRL:string[3];

  // IOS only used on DEC 10
{  /IOS: SET OF IOSPEC; }
  // used as argument to rewrite
  BUFFER: FullString;        {INPUT BUFFER (147 CHARACTERS =
                                              MAX. LENGTH SOS LINE)
                              EINGABEPUFFER (147 ZEICHEN =
                                              MAX. LAENGE SOS-ZEILE)}
  LINENB : string[5];    {SOS LINE NUMBER
                                            SOS-ZEILENNUMMER}
  TIMEANDDAY : fullstring;  {DATE AND TIME FOR PRINT PAGE HEAD
                             DATUM UND UHRZEIT FUER DRUCKSEITENKOPF}
  SY      : FullString;     {Current identifier
                             LETZTER GELESENER BEZEICHNER}
  SYTY    : SYMBOL;         {Symbol type
                             TYP DES LETZTEN GELESENEN ZEICHENS}
  PAGEEJECT,                {
                             MARKER FOR PAGE SHIFT}   (*** C5 ***)
  ERRFLAG,                  {
                             FEHLERMARKE}
  OLDSPACES,                {
                             GESETZT WENN LASTSPACES BENUTZT WERDEN SOLL}
  EOB     : BOOLEAN;        {Indicates end of file
                             EOF-MARKE}
  CH,                       {Current character
                             LETZTES GELESENES ZEICHEN}
  BMARKTEXT,                {
                             TEXT ZUR MARKIERUNG VON 'BEGIN' ETC.}
  EMARKTEXT: CHAR;          {
                             TEXT ZUR MARKIERUNG VON 'END' ETC.}
  DELSY : ARRAY [' '..'_'] Of SYMBOL;   {Delimiters
                                         TYPENARRAY FUER DELIMITERZEICHEN}
  ALPHANUM,                  {Legal chatacters for second and later chats
                                in an identifier
                              ZEICHEN VON 0..9 UND A..Z}
  DIGITS,                    {'0'..'9'
                              ZEICHEN VON 0..9}
  LETTERS : SET Of CHAR;     {Legal as first char in identifier
                              ZEICHEN VON A..Z}
  RELEVANTSYM,               {
                              STARTSYMBOLE FUER STATEMENTS UND PROCEDURES}
  PROSYM,                    {
                              ALLE SYMBOLE DIE DEN BEGINN EINER PROZEDUR KENNZEICHNEN}
  DECSYM,                    {
                              ALLE SYMBOLE DIE DEN BEGINN VON DEKLARATIONEN KENNZEICHNEN}
  BEGSYM,                    {
                              ALLE SYMBOLE DIE DEN BEGINN EINES STATEMENTS KENNZEICHNEN}
  ENDSYM  : SET Of SYMBOL;   {
                              ALLE SYMBOLE DIE STATEMENTS ODER PROZEDUREN TERMINIEREN}
  LISTPTR : LISTPTRTY;       {
                              ZEIGER IM BINAERBAUM DER DEKLARIETEN BEZEICHNER}
  FIRSTNAME : ARRAY ['A'..'Z'] Of LISTPTRTY;
                             {
                              ZEIGER AUF DIE WURZELN DES BAUMES}
  PROCSTRUCF,                {
                              ZEIGER AUF DAS ERSTE ELEMENT DER PROZEDURENLISTE}
  PROCSTRUCL : PROCSTRUCTY;  {
                              ZEIGER AUF DAS LETZTE ELEMENT DER PROZEDURENLISTE}
  NEWFIL,                    {
                              AUSGABEFILE AUF DEM DAS NEUFORMATIERTE PROGRAMM STEHT}
  InFile,
  CrefFile,
  TTY: text;             // originally used as output filr, now use OUTPUT

  MESSAGE : string[23];      {Error message text
                              ARRAY ZUR AUSGABE DER SCHLUSSMELDUNG}

  
Procedure INIT;
var RC: Integer;
Begin

  Begin
 {   RESNUM[1] := 1;     // Location in array of first reserved word with
    RESNUM[2] := 1;     // having two characters, and each additional one ia
    RESNUM[3] := 7;     // for words with one less character. There should be
    RESNUM[4] := 15;    // s way to do this automatically. And there is...
    RESNUM[5] := 26;
    RESNUM[6] := 32;
    RESNUM[7] := 38;
    RESNUM[8] := 41;
    RESNUM[9] := 42;
    RESNUM[10] := 43;
    RESNUM[11] := 44;   }

    // Automatic initialization; no counting, always right, and adding new
    // keywords is trivial!

    RC := 0;
    // two-letter keywords
    Inc(RC); RESLIST[RC] := 'IF';        RESSY[RC] := IFSY;    RESNUM[1] := RC;
    Inc(RC); RESLIST[RC] := 'TO';        RESSY[RC] := OTHERSY;
    Inc(RC); RESLIST[RC] := 'OF';        RESSY[RC] := OFSY;
    Inc(RC); RESLIST[RC] := 'IN';        RESSY[RC] := OTHERSY;
    Inc(RC); RESLIST[RC] := 'DO';        RESSY[RC] := DOSY;
    Inc(RC); RESLIST[RC] := 'OR';        RESSY[RC] := OTHERSY;
    // three-letter keywords
    Inc(RC); RESLIST[RC] := 'END';       RESSY[RC] := ENDSY;   RESNUM[2] := RC;
    Inc(RC); RESLIST[RC] := 'FOR';       RESSY[RC] := OTHERSY;
    Inc(RC); RESLIST[RC] := 'SET';       RESSY[RC] := OTHERSY;
    Inc(RC); RESLIST[RC] := 'AND';       RESSY[RC] := OTHERSY;
    Inc(RC); RESLIST[RC] := 'NOT';       RESSY[RC] := OTHERSY;
    Inc(RC); RESLIST[RC] := 'VAR';       RESSY[RC] := VARSY;
    Inc(RC); RESLIST[RC] := 'NIL';       RESSY[RC] := OTHERSY;
    Inc(RC); RESLIST[RC] := 'DIV';       RESSY[RC] := OTHERSY;
    // fout-letter keywords
    Inc(RC); RESLIST[RC] := 'LOOP';      RESSY[RC] := LOOPSY;  RESNUM[3] := RC;
    Inc(RC); RESLIST[RC] := 'CHAR';      RESSY[RC] := OTHERSY;
    Inc(RC); RESLIST[RC] := 'GOTO';      RESSY[RC] := OTHERSY;
    Inc(RC); RESLIST[RC] := 'THEN';      RESSY[RC] := THENSY;
    Inc(RC); RESLIST[RC] := 'ELSE';      RESSY[RC] := ELSESY;
    Inc(RC); RESLIST[RC] := 'WITH';      RESSY[RC] := OTHERSY;
    Inc(RC); RESLIST[RC] := 'CASE';      RESSY[RC] := CASESY;
    Inc(RC); RESLIST[RC] := 'REAL';      RESSY[RC] := OTHERSY;
    Inc(RC); RESLIST[RC] := 'FILE';      RESSY[RC] := OTHERSY;
    Inc(RC); RESLIST[RC] := 'TYPE';      RESSY[RC] := TYPESY;
    Inc(RC); RESLIST[RC] := 'EXIT';      RESSY[RC] := USESSY;
    Inc(RC); RESLIST[RC] := 'UNIT';      RESSY[RC] := UNITSY;
    Inc(RC); RESLIST[RC] := 'USES';      RESSY[RC] := USESSY;
    // five-letter keywords
    Inc(RC); RESLIST[RC] := 'BEGIN';     RESSY[RC] := BEGINSY; RESNUM[4] := RC;
    Inc(RC); RESLIST[RC] := 'ARRAY';     RESSY[RC] := OTHERSY;
    Inc(RC); RESLIST[RC] := 'WHILE';     RESSY[RC] := OTHERSY;
    Inc(RC); RESLIST[RC] := 'BREAK';     RESSY[RC] := OTHERSY;
    Inc(RC); RESLIST[RC] := 'CONST';     RESSY[RC] := CONSTSY;
    Inc(RC); RESLIST[RC] := 'LABEL';     RESSY[RC] := LABELSY;
    Inc(RC); RESLIST[RC] := 'UNTIL';     RESSY[RC] := UNTILSY;
    Inc(RC); RESLIST[RC] := 'CLASS';     RESSY[RC] := CLASSSY;
    // six-letter keywords
    Inc(RC); RESLIST[RC] := 'RECORD';    RESSY[RC] := RECORDSY; RESNUM[5] := RC;
    Inc(RC); RESLIST[RC] := 'REPEAT';    RESSY[RC] := REPEATSY;
    Inc(RC); RESLIST[RC] := 'DOWNTO';    RESSY[RC] := OTHERSY;
    Inc(RC); RESLIST[RC] := 'PACKED';    RESSY[RC] := OTHERSY;
    Inc(RC); RESLIST[RC] := 'OTHERS';    RESSY[RC] := OTHERSSY;
    Inc(RC); RESLIST[RC] := 'EXTERN';    RESSY[RC] := EXTERNSY;
    Inc(RC); RESLIST[RC] := 'OBJECT';    RESSY[RC] := OBJECTSY;
    // seven-letter keywords
    Inc(RC); RESLIST[RC] := 'INTEGER';   RESSY[RC] := OTHERSY; RESNUM[6] := RC;
    Inc(RC); RESLIST[RC] := 'BOOLEAN';   RESSY[RC] := OTHERSY;
    Inc(RC); RESLIST[RC] := 'FORWARD';   RESSY[RC] := FORWARDSY;
    Inc(RC); RESLIST[RC] := 'PROGRAM';   RESSY[RC] := PROGRAMSY;
    Inc(RC); RESLIST[RC] := 'VIRTUAL';    RESSY[RC] := OTHERSY;
    // eight-letter keywords
    Inc(RC); RESLIST[RC] := 'FUNCTION';  RESSY[RC] := FUNCTIONSY; RESNUM[7] := RC;
    Inc(RC); RESLIST[RC] := 'OPERATOR';  RESSY[RC] := OTHERSY;
    Inc(RC); RESLIST[RC] := 'CONTINUE';  RESSY[RC] := OTHERSY;
    Inc(RC); RESLIST[RC] := 'OVERRIDE';  RESSY[RC] := OTHERSY;
    // nine-letter keywords
    Inc(RC); RESLIST[RC] := 'PROCEDURE'; RESSY[RC] := PROCEDURESY;RESNUM[8] := RC;
    Inc(RC); RESLIST[RC] := 'INTERFACE'; RESSY[RC] := INTERFACESY;
    Inc(RC); RESLIST[RC] := 'OTHERWISE'; RESSY[RC] := ELSESY;
    // ten-letter keywords
    Inc(RC); RESLIST[RC] := 'DESTRUCTOR'; RESSY[RC] := DESTRUCTORSY;Resnum[9] := RC;
    // eleven-letter keywords
    Inc(RC); RESLIST[RC] := 'CONSTRUCTOR'; RESSY[RC] := CONSTRUCTORSY;  Resnum[10]:= RC;
    // twelve- and thirteen-letter keywords
    Resnum[11]:= RC; Resnum[12]:= RC;
    // fourteen-letter keywords
    Inc(RC); RESLIST[RC] := 'IMPLEMENTATION'; RESSY[RC] := IMPLEMENTATIONSY;    Resnum[13]:= RC;
    // fifteen-letter keywords
    Resnum[14]:= RC;
    // note: the highest number used here must be at least one higher than
    // the length of the longest keyword
  End;
  
  Begin
    I := 0;
    BUFFLEN := 0;
    BUFFMARK := 0;
    BUFFERPTR := 2;
    REALLINCNT := 0;
    LINECNT := 0;
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
    LETTERS := ['A'..'Z','_'];
    ALPHANUM := LETTERS+DIGITS;
    DECSYM := [LABELSY..VARSY];
    PROSYM := [FUNCTIONSY..INITPROCSY];
    ENDSYM := [FUNCTIONSY..EOBSY];      {PROSYM OR ENDSYMBOLS}
    BEGSYM := [BEGINSY..IFSY];
    RELEVANTSYM := [LABELSY..INITPROCSY {DECSYM OR PROSYM} ,BEGINSY,FORWARDSY,EXTERNSY,EOBSY];
    PAS := '.PAS';
    NUE := '.PAS';
    CRL := '.CRL';
  End;
  

  Begin  (*INIT*)
    For CH := 'A' To 'Z' Do  FIRSTNAME [CH] := Nil;
    For CH := ' ' To '_' Do  DELSY [CH] := OTHERSY;
    DELSY ['('] := LPARENT;
    DELSY [')'] := RPARENT;
    DELSY ['['] := LPARENT;
    DELSY [']'] := RPARENT;
    DELSY [';'] := SEMICOLON;
    DELSY ['.'] := POINT;
    DELSY [':'] := COLON;
    BUFFER  := '';
    I := 0;
    NEW (FIRSTNAME['M']);
    LISTPTR := FIRSTNAME ['M'];
    With FIRSTNAME ['M']^ Do
      Begin
        NAME := 'MAIN.     ';
        LLINK := Nil;
        RLINK := Nil;
        NEW (FIRST);
        LAST := FIRST;
        PROCVAR := 1;
        With LAST^ Do
          Begin
            LINENR := LINECNT;
            CONTLINK := Nil;
          End;
        NEW (CALLED);
        With CALLED^ Do
          Begin
            PROCNAME := FIRSTNAME ['M'];
            NEXTPROC := Nil;
            NEW (FIRST);
            FIRST^.LINENR := 0;
            FIRST^.CONTLINK := Nil;
            LAST := FIRST;
          End;
        NEW (CALLEDBY);
        With CALLEDBY^ Do
          Begin
            PROCNAME := FIRSTNAME ['M'];
            NEXTPROC := Nil;
            NEW (FIRST);
            FIRST^.LINENR := 0;
            FIRST^.CONTLINK := Nil;
            LAST := FIRST;
          End;
      End;
    NEW (PROCSTRUCF);
    With PROCSTRUCF^ Do
      Begin
        PROCNAME := FIRSTNAME ['M'];
        NEXTPROC := Nil;
        LINENR   := 0;
        PROCLEVEL := 0;
      End;
    PROCSTRUCL := PROCSTRUCF;
  End {INIT} ;
End;

Procedure GETLINENR( FLIN: STR5 );
Var 
  I,L,FAC: INTEGER;

Begin
  FAC := 10000;
  L := LINECNT+1;
  For I:=1 To 5 Do
    Begin
      FLIN[I] := CHR( L Div FAC + &60 );
      L := L Mod FAC;
      FAC := FAC Div 10;
    End
End;


Procedure WRITECH (FCH : CHAR);

Begin {WRITECH}
  WRITE (NEWFIL,FCH);
End {WRITECH};

Procedure WRITELIN;

Begin {WRITELIN}
  WRITELN (NEWFIL);
End {WRITELIN};

Procedure WRITEPAGE;

Begin {WRITEPAGE}
  WRITELN (NEWFIL,CHR(FF));
End {WRITEPAGE};

Procedure WRITELINNR;

Var 
  I, LLINECNT : INTEGER;

Begin {WRITELINNR}
  LLINECNT := LINECNT * INCREMENT;

(*
   I := 10000;
   FOR BUFFINDEX := 1 TO 4 DO
   BEGIN
   WRITE ( NEWFIL, CHR (LLINECNT DIV I + 60B));
   LLINECNT :=  LLINECNT MOD I;
   I := I DIV 10
   END;
   WRITE ( NEWFIL, CHR(LLINECNT+61B),CHR(HT));
   *)
End {WRITELINNR};


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
Procedure datum;
Begin
End;
Procedure page;
Begin
End;


Procedure HEADER;

Begin {HEADER}
  PAGECNT2 := PAGECNT2 + 1;
  REALLINCNT := 0;
  PAGE;
  WRITELN (CrefFile,'TELEFON AB LM ERICSSON          ',TIMEANDDAY,' ':10,
           'PAGE',PAGECNT2:4 );
(*** C6 ***)
  WRITELN (crefFile,'PASCAL - ',VERSION,' ':10,OUTPUTFILE.FILENAME);
(*** C6 ***)
  WRITELN (crefFile);
  WRITELN (crefFile);
(*** C6 ***)
  PAGEEJECT := FALSE;
(*** C5 ***)
End {HEADER} ;


Procedure NEWPAGE;

Begin {NEWPAGE}
  WRITEPAGE;
  HEADER;

End {NEWPAGE} ;

Procedure WRITELINE (POSITION {LETZTES ZU DRUCKENDES ZEICHEN IM PUFFER} : INTEGER);

Var 
  I, TABCNT, LSPACES : INTEGER;    {MARKIERT ERSTES ZU DRUCKENDES ZEICHEN}

Begin {WRITELINE}
  POSITION := POSITION - 2;
  If POSITION > 0
    Then
    Begin
      I := BUFFMARK + 1;
      While (BUFFER [I] = ' ') And (I <= POSITION) Do
        I := I + 1;
      BUFFMARK := POSITION;
      While (BUFFER [POSITION] = ' ') And (I < POSITION) Do
        POSITION := POSITION - 1;
      If I <= POSITION
        Then
        Begin
          If REALLINCNT = MAXLINE
            Then HEADER;
          LINECNT := LINECNT + 1;
          REALLINCNT := REALLINCNT + 1;
          If BMARKTEXT <> ' '
            Then
            Begin
              WRITE (crefFile,BMARKTEXT, BMARKNR : 4, '       ');
              BMARKTEXT := ' ';
            End
          Else
            If EMARKTEXT <> ' '
              Then
              Begin
                WRITE (crefFile,'      ',EMARKTEXT,EMARKNR : 4,' ');
                EMARKTEXT := ' ';
              End
          Else WRITE (crefFile,'            ');
          WRITE (crefFile,LINECNT * INCREMENT : 5,' ');
          WRITELINNR;
          If Not OLDSPACES
            Then LASTSPACES := SPACES;
          WRITE (' ' : LASTSPACES);
          TABCNT := LASTSPACES Div 8;
          LSPACES := LASTSPACES Mod 8;
          For TABCNT := TABCNT Downto 1 Do
            WRITECH (CHR(HT));
          For LSPACES := LSPACES Downto 1 Do
            WRITECH (' ');
          If (POSITION - I + LASTSPACES + 1) > MAXCH
            Then
            Begin
              If REALLINCNT = MAXLINE
                Then
                Begin
                  For I := I To MAXCH + I - LASTSPACES - 1 Do
                    Begin
                      WRITE (BUFFER[I]);
                      WRITECH (BUFFER[I]);
                      BUFFER [I] := ' ';
                    End;
                  WRITELN;
                  HEADER;
                End;
              REALLINCNT := REALLINCNT + 1;
            End;
          For I := I To POSITION Do
            Begin
              WRITE (BUFFER [I]);
              WRITECH (BUFFER[I]);
              BUFFER [I] := ' ';
            End;
          WRITELIN;
          WRITELN;
          If ((LINENB = '     ') And (POSITION = BUFFLEN)) Or (MAXINC = LINECNT)
            Then NEWPAGE;
          If PAGEEJECT Then HEADER;
(*** C5 ***)
        End;
    End;
  LASTSPACES := SPACES;
  OLDSPACES := FALSE;
End {WRITELINE} ;

Procedure READLINE;
Var
  CH      : WideCHAR;

Begin {READLINE}

  Repeat
     Readln(buffer);
     BufferPtr := 1;
     bufflen := Length(Buffer);
     // handle blank lines directly
    While (bufflen=0) And Not (EOF (INPUT)) Do
      Begin
        GETLINENR (LINENB);

          If REALLINCNT = MAXLINE
            Then HEADER;
          LINECNT := LINECNT + 1;
          REALLINCNT := REALLINCNT + 1;
          WRITELN (' ' : 12,LINECNT * INCREMENT : 5);
          WRITELINNR;
          WRITELIN;
          If MAXINC = LINECNT
            Then NEWPAGE;
          Readln(Buffer);
          bufflen := Length(Buffer);
      End;
    CH := Buffer[Bufferptr];
    inc(BufferPtr);
    If CH = CHR(FF) Then                 (*** C5 ***)
        NEWPAGE;
        If CH < ' '   (*** C 1 ***)
      Then CH := ' ';
  Until (CH <> ' ') Or (EOF (INPUT));

(*** C 1 ***)

    If Not (EOF (INPUT)) Then
        GETLINENR (LINENB);
    BUFFMARK := 0;
End {READLINE} ;



{I suspect someone cribbed part of an actual
 Pascal compiler to construct this... }

Procedure BLOCK;

Var 
  DOUBLEDECF,                 {
                               ZEIGER AUF ERSTE UND LETZTE VARIABLE
                                 DIE ALS PROCEDURE}
  DOUBLEDECL : ^DOUBLEDEC;    {
                               IN DIESEM BLOCK DOPPELT DEKLARIERT WURDEN}
  CURPROC : LISTPTRTY;        {
                               ZEIGER AUF DIE PROZEDUR IN DEREN
                                 ANWEISUNGSTEIL DAS PROGRAMM SICH BEFINDET}

Procedure INSYMBOL ;
Label
  1;

Var 
  OLDSPACESMARK,            {
                             ALTER ZEICHENVORSCHUB BEI FORMATIERUNG
                               VON KOMMENTAREN}
  I       : INTEGER;        {Index variable}

Procedure READBUFFER;



Begin {READBUFFER}
  If BUFFERPTR >= BuffLen  Then
    Begin
      WRITELINE (BUFFERPTR);
      CH := ' ';
      If EOF (INPUT) Then
          EOB := TRUE
      Else READLINE;
    End
  Else
    Begin
      BUFFERPTR := BUFFERPTR + 1;
      CH := BUFFER [BUFFERPTR];

    End;
End {READBUFFER} ;


Procedure LPARENTHESE;

Var 
  OLDSPACESMARK : INTEGER;    {OLD CHARACTER FEED WHEN FORMATTING PARENTHESES
                               ALTER ZEICHENVORSCHUB BEI FORMATIERUNG
                                 VON KLAMMERN}

Begin {PARENTHESE}
  OLDSPACESMARK := SPACES;
  If OLDSPACES
    Then SPACES := LASTSPACES + BUFFERPTR - 2
  Else
    Begin
      LASTSPACES := SPACES;
      SPACES := SPACES + BUFFERPTR - 2;
      OLDSPACES := TRUE;
    End;
  Repeat
    INSYMBOL
  Until SYTY In [RPARENT,EOBSY];
  SPACES := OLDSPACESMARK;
  OLDSPACES := TRUE;
  INSYMBOL;
End {PARENTHESE} ;


Function RESWORD: BOOLEAN ;

// Label  1;

Var
  Chcnt,
  I       : INTEGER;

Begin {RESWORD}
  Chcnt := Length(SY);
  RESWORD := FALSE;
  If (Chcnt<2) or (Chcnt > MaxResSize) then exit;
  For I:=RESNUM[CHCNT] To RESNUM [CHCNT + 1] -1 Do
    If RESLIST[ I ] = SY
      Then
      Begin
        RESWORD := TRUE;
        SYTY := RESSY [I];
        Break; // GOTO 1;
      End;
  // 1:
End {RESWORD} ;

{$NOTE Remember to implrment HandleSwitches}
// process any {$ / (*$ switch
Procedure HandleSwitches(Closure: FullString);
Begin end; // This will be handled later


// handles 1 or 2 char comment blocks
// also handles nested comments
procedure ProcessComments(Closure: String = '}');
VAR
    Len: Byte;
    NextCh,
    Ch: Widechar;
    Closedby: string;

begin
    // Assumption: BufferPtr is at char after start of comment, e.g.
    // if comment is {X or (*X, then X marks the spot we should be at.
    ClosedBy := Closure;
    Inc(CommentNest);
    Len := Length(ClosedBy);
    Ch := Buffer[BufferPtr];
    NextCh := Buffer[BufferPtr+1];
    if (CH = '$') and (CommentNest = 1) then
    // check for compiler switch, but not inside a nested comment
    begin
        HandleSwitches(Closedby); // it will eat the rest of the comment
        if CommentNest < 1 then
            Exit;
    end;

    NextCh := Buffer[BufferPtr+1];
    // see if we reached closure of this comment
    while ((Len=1) and (CH<>Closedby[1])) or
          ((Len=2) and (CH<>Closedby[1]) and
                       (NextCh<>Closedby[2])) do
    Begin
        // see if we just started a nested comment
        if Ch = '{' then
            ProcessComments('}')
        else if (Ch = '(') and (NextCh = '*') then
            ProcessComments('*)');
        // Continue through this comment
        Inc(BufferPtr);
        if BufferPtr > BuffLen then
             ReadLine;
        // Get the buffer chars again
        Ch := Buffer[BufferPtr];
        NextCh := Buffer[BufferPtr+1];
    end;
    if ((Len=1) and (CH=Closedby[1])) or
       ((Len=2) and (CH=Closedby[1]) and
               (NextCh=Closedby[2]))  then
    begin
        Dec(  CommentNest);
        break;
    end;
    until Eof(Infile); // Ptrsumably, we exit before this
end;

Procedure FINDNAME;

Label 
  1;

Var 
  PROCPTR : PROCCALLTY;   {POINTER TO CALLING OR CALLED PROCEDURE
                             WHEN CONCATENATING
                           ZEIGER AUF RUFENDE BZW. GERUFENE PROZEDUR
                             BEI DEREN VERKETTUNG}
  LPTR: LISTPTRTY;        {POINTER TO THE LEADER IN THE TREE
                           ZEIGER AUF DEN VORGAENGER IM BAUM}
  ZPTR : LINEPTRTY;       {POINTER TO THE PENULTIMATE LINE NUMBER IN A CHAIN
                           ZEIGER AUF DIE VORLETZTE ZEILENNUMMER IN EINER KETTE}
  RIGHT: BOOLEAN;         {MEMORY VARIABLE FOR BRANCHING IN THE TREE
                           MERKVARIABLE FUER DIE VERZWEIGUNG IM BAUM}
  INDEXCH : CHAR;         {INDEX VARIABLE FOR THE START POINTER
                             FIELD (FIRSTNAME)
                           INDEXVARIABLE FUER DAS FELD DER
                             STARTZEIGER (FIRSTNAME)}


Procedure FINDPROC (COMP : LISTPTRTY);

Var 
  PROCCALLPTR : PROCCALLTY;     {       AAA
                                 MERK SICH LETZTE PROZEDUR FALLS EINE
                                   NEUE ERZEUGT WERDEN MUSS}

Begin {FINDPROC}
  While (PROCPTR^.PROCNAME <> COMP) And (PROCPTR^.NEXTPROC <> Nil) Do
    PROCPTR := PROCPTR^.NEXTPROC;
  If PROCPTR^.PROCNAME = COMP
    Then
    Begin
      ZPTR := PROCPTR^.LAST;
      NEW (PROCPTR^.LAST);
      With PROCPTR^.LAST^ Do
        Begin
          LINENR := LINECNT + 1;
          PAGENR := PAGECNT;
          CONTLINK := Nil;
        End;
      ZPTR^.CONTLINK := PROCPTR^.LAST;
    End
  Else
    Begin
      PROCCALLPTR := PROCPTR;
      NEW (PROCPTR);
      With PROCPTR^ Do
        Begin
          PROCNAME := COMP;
          NEXTPROC := Nil;
          ZPTR := FIRST;
          NEW (FIRST);
          With FIRST^ Do
            Begin
              LINENR := LINECNT + 1;
              PAGENR := PAGECNT;
              CONTLINK := Nil;
            End;
          ZPTR^.CONTLINK := FIRST;
          LAST := FIRST;
        End;
      PROCCALLPTR^.NEXTPROC := PROCPTR;
    End;
End {FINDPROC} ;

Procedure NEWPROCEDURE;

Begin {NEWPROCEDURE}
  With LISTPTR^ Do
    Begin
      PROCVAR := PROCDEC;
      NEW (CALLEDBY);
      With CALLEDBY^ Do
        Begin
          PROCNAME := CURPROC;
          NEXTPROC := Nil;
          ZPTR := FIRST;
          NEW (FIRST);
          With FIRST^ Do
            Begin
              LINENR := LINECNT + 1;
              PAGENR := PAGECNT;
              CONTLINK := Nil;
            End;
          ZPTR^.CONTLINK := FIRST;
          // $$** CRASHPOINT ** $$$
          LAST := FIRST;
        End;
      NEW (CALLED);
      With CALLED^ Do
        Begin
          PROCNAME := FIRSTNAME ['M'];
          NEXTPROC := Nil;
          ZPTR := FIRST;
          NEW (FIRST);
          With FIRST^ Do
            Begin
              LINENR := LINECNT + 1;
              PAGENR := PAGECNT;
              CONTLINK := Nil;
            End;
          ZPTR^.CONTLINK := FIRST;
          LAST := FIRST;
        End;
    End;
  NEW (PROCSTRUCL^.NEXTPROC);
  PROCSTRUCL := PROCSTRUCL^.NEXTPROC;
  With PROCSTRUCL^ Do
    Begin
      PROCNAME := LISTPTR;
      NEXTPROC := Nil;
      LINENR := LINECNT + 1;
      PAGENR := PAGECNT;
      PROCLEVEL := LEVEL;
    End;
End {NEWPROCEDURE} ;

Begin {FINDNAME}
  INDEXCH := SY [1];
  LISTPTR := FIRSTNAME [INDEXCH];
  While LISTPTR <> Nil Do
    Begin
      LPTR := LISTPTR;
      If SY = LISTPTR^.NAME
        Then
        Begin
          ZPTR := LISTPTR^.LAST;
          NEW (LISTPTR^.LAST);
          With LISTPTR^.LAST^ Do
            Begin
              LINENR := LINECNT + 1;
              PAGENR := PAGECNT;
              CONTLINK := Nil;
            End;
          ZPTR^.CONTLINK := LISTPTR^.LAST;
          If LISTPTR^.PROCVAR <> 0
            Then
            Begin
              If LISTPTR^.PROCVAR = 2
                Then While CH = ' ' Do
                       Begin
                         SYLENG := SYLENG + 1;
                         READBUFFER;
                       End;
              If (CH <> ':') Or (LISTPTR^.PROCVAR = 1)
                Then
                Begin
                  PROCPTR := LISTPTR^.CALLEDBY;
                  FINDPROC (CURPROC);
                  PROCPTR := CURPROC^.CALLED;
                  FINDPROC (LISTPTR);
                End
            End
          Else
            If PROCDEC <> 0
              Then
              Begin
                If DOUBLEDECF = Nil
                  Then
                  Begin
                    NEW (DOUBLEDECF);
                    DOUBLEDECL := DOUBLEDECF;
                  End
                Else
                  Begin
                    NEW (DOUBLEDECL^.NEXTPROC);
                    DOUBLEDECL := DOUBLEDECL^.NEXTPROC;
                  End;
                DOUBLEDECL^.NEXTPROC := Nil;
                DOUBLEDECL^.PROCORT := LISTPTR;
                NEWPROCEDURE;
              End;
          GOTO 1;
        End
      Else
        If SY > LISTPTR^.NAME
          Then
          Begin
            LISTPTR := LISTPTR^.RLINK;
            RIGHT := TRUE;
          End
      Else
        Begin
          LISTPTR := LISTPTR^.LLINK;
          RIGHT := FALSE;
        End;
    End;
  NEW (LISTPTR);
  With LISTPTR^ Do
    Begin
      NAME := SY;
      LLINK := Nil;
      RLINK := Nil;
    End;
  If FIRSTNAME [INDEXCH] = Nil
    Then FIRSTNAME [INDEXCH] := LISTPTR
  Else
    If RIGHT
      Then LPTR^.RLINK := LISTPTR
  Else LPTR^.LLINK := LISTPTR;
  With LISTPTR^ Do
    Begin
      NEW (FIRST);
      With FIRST^ Do
        Begin
          LINENR := LINECNT + 1;
          PAGENR := PAGECNT;
          CONTLINK := Nil;
        End;
      LAST := FIRST ;
      If PROCDEC = 0
        Then
        Begin
          PROCVAR := 0;
          CALLED := Nil;
          CALLEDBY := Nil;
        End
      Else NEWPROCEDURE;
    End;
  1:
     PROCDEC := 0;
End {FINDNAME} ;

Begin {INSYMBOL}
  SYLENG := 0;
  While (CH In ['(',' ','{','$','?','}','!','@'])
        And Not EOB And (CH <= '_')  Do
    Begin
      If (CH = '{') Or ((CH = '(') And (BUFFER[BUFFERPTR] = '*')) Then
        Begin
          OLDSPACESMARK := SPACES;
          If OLDSPACES
            Then SPACES := LASTSPACES
          Else  LASTSPACES := SPACES;
          SPACES := SPACES + BUFFERPTR - 1;
          OLDSPACES := TRUE;
          If CH = '{'
            Then
            Repeat
              READBUFFER;
            Until (CH = '}') Or EOB
          Else
            Begin
              PAGEEJECT := (BUFFER[BUFFERPTR+1]='$') And (BUFFER[BUFFERPTR+2]='P')
                           And   (BUFFER[BUFFERPTR+3]='+');
(*** C5 ***)
              Repeat
                READBUFFER
              Until (CH = ')') And (BUFFER[BUFFERPTR-2] = '*') Or EOB;
            End;
          SPACES := OLDSPACESMARK;
          OLDSPACES := TRUE;
        End
      Else
        If CH = '('
          Then GOTO 1;
      READBUFFER;
    End;
  If CH = ''''
    Then
    Begin
      SYTY := STRGCONST;
      Repeat
        READBUFFER;
      Until (CH = '''') Or EOB;
      READBUFFER;
    End
  Else
    If (CH In LETTERS) Or (ORD(CH) > &137)
      Then
      Begin
        SY := '';
        Repeat
{  // don't need all this: String management is automatic
          SYLENG := SYLENG + 1;
          If ORD(CH) > &137
            Then CH := CHR(ORD(CH)-32);
          If SYLENG <= 10 Then
}
          SY := SY + CH;

          READBUFFER;
//               UNTIL NOT ((CH IN (ALPHANUM OR ['_'])) OR (CH > '_'));
          Until (Not (CH In ALPHANUM)) Or (CH > '_');
{ // don't need this, either
{        For I := SYLENG + 1 To 10 Do

          SY [I] := ' '; }
        If SYLENG > 10
          Then CHCNT := 10
        Else CHCNT := SYLENG;   }
        If Not RESWORD
          Then
          Begin
            SYTY := IDENT ;
            FINDNAME;
          End
      End
  Else
    If CH In DIGITS
      Then
      Begin
        Repeat
          READBUFFER;
        Until Not (CH In DIGITS);
        SYTY := INTCONST;
        If CH = 'B'
          Then READBUFFER
        Else
          Begin
            If CH = '.'
              Then
              Begin
                Repeat
                  READBUFFER
                Until Not (CH In DIGITS);
                SYTY := OTHERSY;
              End;
            If CH = 'E'
              Then
              Begin
                READBUFFER;
                If CH In ['+','-']
                  Then READBUFFER;
                While CH In DIGITS Do
                  READBUFFER;
                SYTY := OTHERSY;
              End;
          End;
      End
  Else
    If CH = '"'
      Then
      Begin
        Repeat
          READBUFFER
          //             UNTIL NOT (CH IN  (DIGITS OR ['A'..'F']));
        Until Not (CH In  (DIGITS + ['A'..'F']));
        SYTY := INTCONST;
      End
  Else
    If CH <> ' '
      Then
      Begin
        1
        :
          SYTY := DELSY [CH];
        READBUFFER;
        If SYTY = LPARENT
          Then LPARENTHESE
        Else
          If (SYTY = COLON) And (CH = '=')
            Then
            Begin
              SYTY := OTHERSY;
              READBUFFER;
            End;
      End
  Else SYTY := EOBSY;
End {INSYMBOL} ;

Procedure RECDEF;

Var 
  OLDSPACESMARK  : INTEGER;         {ALTER ZEICHENVORSCHUB BEI FORMATIERUNG VON RECORDS}


Procedure CASEDEF;

Var 
  OLDSPACESMARK  : INTEGER;       {ALTER ZEICHENVORSCHUB BEI FORMATIERUNG VON VARIANT PARTS}


Procedure PARENTHESE;

Var 
  OLDSPACESMARK : INTEGER;
  {ALTER ZEICHENVORSCHUB BEI FORMATIERUNG VON KLAMMERN INNERHALB VON VARIANT PARTS}

Begin {PARENTHESE}
  OLDSPACESMARK := SPACES;
  If OLDSPACES
    Then SPACES := LASTSPACES
  Else LASTSPACES := SPACES;
  SPACES := SPACES + BUFFERPTR - 2;
  OLDSPACES := TRUE;
  Repeat
    INSYMBOL;
    Case SYTY Of 
      LBRACK : PARENTHESE;
      CASESY : CASEDEF;
      RECORDSY : RECDEF
    End;
  Until SYTY In [RPARENT,EOBSY];
  SPACES := OLDSPACESMARK;
  OLDSPACES := TRUE;
  INSYMBOL;
End {PARENTHESE} ;


Begin {CASEDEF}
  DELSY ['('] := LBRACK;
  OLDSPACESMARK := SPACES;
  If OLDSPACES
    Then SPACES := LASTSPACES
  Else LASTSPACES := SPACES;
  SPACES := BUFFERPTR - BUFFMARK + SPACES - SYLENG + 3;
  OLDSPACES := TRUE;
  Repeat
    INSYMBOL ;
    Case SYTY Of 
      LBRACK : PARENTHESE;
      CASESY : CASEDEF;
      RECORDSY: RECDEF
    End;
  Until SYTY In [ENDSY,RPARENT,EOBSY];
  SPACES := OLDSPACESMARK;
  DELSY ['('] := LPARENT;
End {CASEDEF} ;

Begin {RECDEF}
  OLDSPACESMARK := SPACES;
  SPACES := BUFFERPTR - BUFFMARK + SPACES - SYLENG - 2 + FEED;
  OLDSPACES := TRUE;
  INSYMBOL;
  WRITELINE ( BUFFERPTR-SYLENG);
  Repeat
    Case SYTY Of 
      CASESY : CASEDEF;
      RECORDSY : RECDEF;
      OTHERSY  : INSYMBOL
    End;
  Until SYTY In [ENDSY,EOBSY];
  WRITELINE (BUFFERPTR-SYLENG);
  OLDSPACES := TRUE;
  LASTSPACES := SPACES - FEED;
  SPACES := OLDSPACESMARK;
  INSYMBOL;
End {RECDEF} ;

Procedure ERROR (ERRNR : INTEGER);

Begin {ERROR}
  ERRFLAG := TRUE;
  WRITELINE (BUFFERPTR);
  WRITE (' ':17,' **** ');
  Case ERRNR Of 
    1 : WRITELN (SY,' ? ? ? ',MESSAGE);
    2 : WRITELN ('MISSING ''END'' OR ''UNTIL'' NUMBER ',EMARKNR : 4);
    3 : WRITELN ('MISSING ''THEN'' NUMBER ',EMARKNR : 4);
    4 : WRITELN ('MISSING ''OF'' TO ''CASE'' NUMBER ',BMARKNR : 4);
    5 : WRITELN (' ONLY ONE ''EXIT'' ALLOWED');
    6 : WRITELN ('MISSING ''EXIT'' IN ''LOOP'' ',EMARKNR : 4)
  End;
End {ERROR} ;

Procedure STATEMENT (IFFLAG : BOOLEAN);

Var 
  CURBLOCKNR : INTEGER;     {AKTUELLE BLOCKNUMMER}



Procedure COMPSTAT;

Begin {COMPSTAT}
  BMARKTEXT := 'B';
  OLDSPACES := TRUE;
  LASTSPACES := SPACES - BACKFEED;
  INSYMBOL;
  WRITELINE (BUFFERPTR-SYLENG);
  //       LOOP
  While true Do
    Begin
      Repeat
        STATEMENT (FALSE);
      Until SYTY In ENDSYM;
      //       EXIT IF SYTY IN [ENDSY,EOBSY,PROCEDURESY,FUNCTIONSY];
      If SYTY In [ENDSY,EOBSY,PROCEDURESY,FUNCTIONSY] Then break;
      ERROR (1);
      INSYMBOL ;
    End;
  WRITELINE (BUFFERPTR-SYLENG);
  EMARKTEXT := 'E';
  EMARKNR := CURBLOCKNR;
  LASTSPACES := SPACES-BACKFEED;
  OLDSPACES := TRUE;
  If SYTY = ENDSY
    Then
    Begin
      INSYMBOL ;
      WRITELINE (BUFFERPTR-SYLENG);
    End
  Else ERROR (2);
End {COMPSTAT} ;

Procedure CASESTAT;

Var 
  OLDSPACESMARK : INTEGER;        {ALTER ZEICHENVORSCHUB BEI FORMATIERUNG VON CASE-STATEMENTS}

Begin {CASESTAT}
  BMARKTEXT := 'C';
  OLDSPACES := TRUE;
  LASTSPACES := SPACES-BACKFEED;
  INSYMBOL;
  STATEMENT (FALSE);
  If SYTY = OFSY
    Then WRITELINE (BUFFERPTR)
  Else ERROR (3);
  //       LOOP
  While true Do
    Begin
      Repeat
        Repeat
          INSYMBOL ;
          //           UNTIL SYTY IN (ENDSYM OR [COLON]);
        Until SYTY In (ENDSYM + [COLON]);
        If SYTY = COLON
          Then
          Begin
            OLDSPACESMARK := SPACES;
            LASTSPACES := SPACES;
            SPACES := BUFFERPTR - BUFFMARK + SPACES - 2;
            OLDSPACES := TRUE;
            INSYMBOL;
            STATEMENT (FALSE);
            SPACES := OLDSPACESMARK;
          End;
      Until SYTY In ENDSYM;
      //       EXIT IF SYTY IN [ENDSY,EOBSY,PROCEDURESY,FUNCTIONSY];
      If SYTY In [ENDSY,EOBSY,PROCEDURESY,FUNCTIONSY] Then break;
      ERROR (1);
    End;
  WRITELINE (BUFFERPTR-SYLENG);
  EMARKTEXT := 'E';
  EMARKNR := CURBLOCKNR;
  LASTSPACES := SPACES-BACKFEED;
  OLDSPACES := TRUE;
  If SYTY = ENDSY
    Then
    Begin
      INSYMBOL ;
      WRITELINE (BUFFERPTR-SYLENG);
    End
  Else ERROR (2);
End {CASESTAT} ;

Procedure LOOPSTAT;

Var 
  LOOPFLAG : BOOLEAN;     {GESETZT BEIM AUFTRETEN VON EXIT-STATEMENTS}

Begin {LOOPSTAT}
  BMARKTEXT := 'L';
  OLDSPACES := TRUE;
  LASTSPACES := SPACES - BACKFEED;
  INSYMBOL;
  WRITELINE (BUFFERPTR-SYLENG);
  LOOPFLAG := FALSE;
  //      LOOP
  While true Do
    Begin
      Repeat
        STATEMENT (FALSE);
        If SYTY = EXITSY
          Then
          Begin
            WRITELINE (BUFFERPTR-SYLENG);
            If LOOPFLAG
              Then ERROR (5);
            OLDSPACES := TRUE;
            LASTSPACES := SPACES-BACKFEED;
            LOOPFLAG := TRUE;
            EMARKTEXT := 'X';
            EMARKNR := CURBLOCKNR;
            INSYMBOL;
            INSYMBOL;
          End;
      Until SYTY In ENDSYM;
      //       EXIT IF SYTY IN [ENDSY,EOBSY,PROCEDURESY,FUNCTIONSY];
      If SYTY In [ENDSY,EOBSY,PROCEDURESY,FUNCTIONSY] Then break;
      ERROR (1);
      INSYMBOL ;
    End;
  WRITELINE (BUFFERPTR-SYLENG);
  EMARKTEXT := 'E';
  EMARKNR := CURBLOCKNR;
  LASTSPACES := SPACES-BACKFEED;
  OLDSPACES := TRUE;
  If SYTY = ENDSY
    Then
    Begin
      INSYMBOL ;
      WRITELINE (BUFFERPTR-SYLENG);
    End
  Else ERROR (2);
  If Not LOOPFLAG
    Then ERROR (6);
End {LOOPSTAT} ;

Procedure IFSTAT (IFVAR : BOOLEAN);

Begin {IFSTAT}
  BMARKTEXT := 'I';
  If Not IFVAR
    Then
    Begin
      SPACES := SPACES - FEED;
      LASTSPACES := SPACES
    End
  Else LASTSPACES := SPACES - BACKFEED;
  OLDSPACES := TRUE;
  INSYMBOL;
  STATEMENT (FALSE);
  If SYTY = THENSY
    Then
    Begin
      WRITELINE (BUFFERPTR-SYLENG);
      If IFVAR
        Then LASTSPACES := SPACES - BACKFEED
      Else LASTSPACES := SPACES;
      OLDSPACES := TRUE;
      EMARKTEXT := 'T';
      EMARKNR := CURBLOCKNR;
      INSYMBOL;
      STATEMENT (TRUE);
    End
  Else ERROR (4);
  If SYTY = ELSESY
    Then
    Begin
      WRITELINE (BUFFERPTR-SYLENG);
      EMARKTEXT := 'S';
      EMARKNR := CURBLOCKNR;
      If IFVAR
        Then LASTSPACES := SPACES - BACKFEED
      Else LASTSPACES := SPACES;
      OLDSPACES := TRUE;
      INSYMBOL;
      STATEMENT (TRUE);
    End;
  If Not IFVAR
    Then SPACES := SPACES + FEED
End {IFSTAT} ;


Procedure LABELSTAT;

Begin {LABELSTAT}
  LASTSPACES := 0;
  OLDSPACES := TRUE;
  INSYMBOL;
  WRITELINE (BUFFERPTR-SYLENG);
End {LABELSTAT} ;

Procedure REPEATSTAT;

Begin {REPEATSTAT}
  BMARKTEXT := 'R';
  OLDSPACES := TRUE;
  LASTSPACES := SPACES - BACKFEED;
  INSYMBOL ;
  WRITELINE (BUFFERPTR-SYLENG);
  //       LOOP
  While true Do
    Begin
      Repeat
        STATEMENT (FALSE);
      Until SYTY In ENDSYM;
      //       EXIT IF SYTY IN [UNTILSY,EOBSY,PROCEDURESY,FUNCTIONSY];
      If SYTY In [UNTILSY,EOBSY,PROCEDURESY,FUNCTIONSY] Then break;
      ERROR (1);
      INSYMBOL ;
    End;
  WRITELINE (BUFFERPTR-SYLENG);
  EMARKTEXT := 'U';
  EMARKNR := CURBLOCKNR;
  OLDSPACES := TRUE;
  LASTSPACES := SPACES-BACKFEED;
  If SYTY = UNTILSY
    Then
    Begin
      INSYMBOL;
      STATEMENT (FALSE);
    End
  Else ERROR (2);
End {REPEATSTAT} ;


Begin {STATEMENT}
  If SYTY = INTCONST
    Then
    Begin
      INSYMBOL;
      If SYTY = COLON
        Then LABELSTAT;
    End;
  If SYTY In BEGSYM
    Then
    Begin
      BLOCKNR := BLOCKNR + 1;
      CURBLOCKNR := BLOCKNR;
      BMARKNR := CURBLOCKNR;
      WRITELINE (BUFFERPTR-SYLENG);
      SPACES := SPACES + FEED;
      Case SYTY Of 
        BEGINSY : COMPSTAT;
        LOOPSY  : LOOPSTAT;
        CASESY  : CASESTAT;
        IFSY    : IFSTAT (IFFLAG);
        REPEATSY : REPEATSTAT
      End;
      SPACES := SPACES - FEED;
    End
    //    ELSE WHILE NOT (SYTY IN ([SEMICOLON] OR ENDSYM)) DO INSYMBOL;

  Else
    While Not (SYTY In ([SEMICOLON] + ENDSYM)) Do
      INSYMBOL;

  If SYTY = SEMICOLON
    Then INSYMBOL
  Else
    If SYTY = DOSY
      Then
      Begin
        INSYMBOL;
        STATEMENT (FALSE);
      End;
End {STATEMENT} ;

Begin {BLOCK}
  DOUBLEDECF := Nil;
  LEVEL := LEVEL + 1;
  CURPROC := LISTPTR;
  SPACES := LEVEL * FEED;
  Repeat
    INSYMBOL
  Until (SYTY In RELEVANTSYM);
  While SYTY In DECSYM Do
    Begin
      WRITELINE (BUFFERPTR-SYLENG);
      SPACES := SPACES - FEED;
      WRITELINE (BUFFERPTR);
      SPACES := SPACES + FEED;
      Repeat
        INSYMBOL ;
        If SYTY = RECORDSY
          Then RECDEF;
      Until SYTY In RELEVANTSYM;
    End;
  While SYTY In PROSYM Do
    Begin
      WRITELINE (BUFFERPTR-SYLENG);
      OLDSPACES := TRUE;
      If SYTY <> INITPROCSY
        Then
        Begin
          If SYTY = PROCEDURESY
            Then PROCDEC := 1              // Procedure declaration
          Else PROCDEC := 2;
          // Function declaration
          INSYMBOL;
        End;
      BLOCK;
      If SYTY = SEMICOLON
        Then INSYMBOL;
    End;
  LEVEL := LEVEL - 1;
  SPACES := LEVEL * FEED;
  If Not (SYTY In [BEGINSY,FORWARDSY,EXTERNSY])
    Then
    Begin
      ERROR (1);
      While Not (SYTY In [BEGINSY,FORWARDSY,EXTERNSY,EOBSY]) Do
        INSYMBOL
    End;
  If SYTY = BEGINSY
    Then STATEMENT (FALSE)
  Else INSYMBOL;
  If DOUBLEDECF <> Nil
    Then
    Repeat
      DOUBLEDECF^.PROCORT^.PROCVAR := 0;
      DOUBLEDECF := DOUBLEDECF^.NEXTPROC;
    Until  DOUBLEDECF = Nil;
  If LEVEL = 0
    Then
    Begin
      If SYTY <> POINT
        Then
        Begin
          WRITELN (TTY,'MISSING POINT AT PROGRAM END');
          WRITELN (TTY);
          WRITELN (' ' : 17, ' **** MISSING POINT AT PROGRAM END ****');
          INSYMBOL;
        End;
      If SYTY <> EOBSY
        Then
        Repeat
          INSYMBOL
        Until SYTY = EOBSY;
    End;
End {BLOCK} ;

Procedure PRINTLISTE;

Var 
  FIRSTPROC,LASTPROC, {ZEIGER ZUM DURCHHANGELN DURCH DIE BAEUME UND LISTEN BEIM AUSDRUCKEN}
  PRED : LISTPTRTY;
  INDEXCH : CHAR;     {LAUFVARIABLE FUER DAS FELD 'FIRSTNAME' ZUM AUSDRUCKEN}



Procedure WRITELINENR (SPACES : INTEGER);

Var 
  LINK : LINEPTRTY; {ZEIGER ZUM DURCHHANGELN DURCH DIE VERKETTUNG DER ZEILENNUMMERN}
  COUNT : INTEGER;  {ZAEHLT DIE GEDRUCKTEN ZEILENNUMMERN PRO ZEILE}

Begin {WRITELINENR}
  COUNT := 0;
  LINK := LISTPTR^.FIRST;
  Repeat
    If COUNT > (MAXCH - SPACES) Div 6
      Then
      Begin
        WRITELN;
        WRITE (' ' : SPACES);
        COUNT := 0;
      End;
    COUNT := COUNT + 1;
    WRITE (LINK^.LINENR * INCREMENT : 6);
    LINK := LINK^.CONTLINK;
  Until LINK = Nil;
End {WRITELINENR} ;

Begin {PRINTLISTE}
  FIRSTPROC := Nil;
  LASTPROC := Nil;
  With FIRSTNAME ['M']^ Do
    If RLINK = Nil
      Then FIRSTNAME ['M'] := LLINK
    Else
      Begin
        LISTPTR := RLINK;
        While LISTPTR^.LLINK <> Nil Do
          LISTPTR := LISTPTR^.LLINK;
        LISTPTR^.LLINK := LLINK;
        FIRSTNAME ['M'] := RLINK;
      End;
  INDEXCH := 'A';
  While (INDEXCH < 'Z') And (FIRSTNAME [INDEXCH] = Nil) Do
    INDEXCH := SUCC (INDEXCH);
  If FIRSTNAME [INDEXCH] <> Nil
    Then
    Begin
      PAGE;
      WRITELN ('CROSS REFERENCE LIST OF IDENTIFIERS');
      WRITELN ('***********************************');
      For INDEXCH := INDEXCH To 'Z' Do
        While FIRSTNAME [INDEXCH] <> Nil Do
          Begin
            LISTPTR := FIRSTNAME [INDEXCH];
            While LISTPTR^.LLINK <> Nil Do
              Begin
                PRED := LISTPTR;
                LISTPTR := LISTPTR^.LLINK;
              End;
            If LISTPTR = FIRSTNAME [INDEXCH]
              Then FIRSTNAME [INDEXCH] := LISTPTR^.RLINK
            Else PRED^.LLINK := LISTPTR^.RLINK;
            If LISTPTR^.CALLED <> Nil
              Then
              Begin
                If FIRSTPROC = Nil
                  Then
                  Begin
                    FIRSTPROC := LISTPTR;
                    LASTPROC := FIRSTPROC;
                    LASTPROC^.CALLED^.PROCNAME := Nil;
                  End
                Else
                  Begin
                    LASTPROC^.CALLED^.PROCNAME := LISTPTR;
                    LASTPROC := LISTPTR;
                  End;
              End;
            WRITELN;
            WRITE (LISTPTR^.NAME : 11);
            WRITELINENR (11);
          End;
      If FIRSTPROC <> Nil
        Then
        Begin
          PAGE;
          WRITELN ('LIST OF PROCEDURE CALLS');
          WRITELN ('***********************');
          LASTPROC^.CALLED^.PROCNAME := Nil;
          LASTPROC := FIRSTPROC;
          While LASTPROC <> Nil Do
            Begin
              LISTPTR := LASTPROC;
              WRITELN;
              WRITELN;
              WRITE (LASTPROC^.NAME:11, ' IS CALLED FROM :');
              With LASTPROC^ Do
                Repeat
                  WRITELN;
                  WRITE (' ' : 11,CALLEDBY^.PROCNAME^.NAME:11);
                  LISTPTR^.FIRST := CALLEDBY^.FIRST;
                  WRITELINENR (22);
                  CALLEDBY := CALLEDBY^.NEXTPROC;
                Until CALLEDBY = Nil;
              WRITELN;
              WRITELN;
              If LASTPROC^.CALLED^.NEXTPROC <> Nil
                Then
                Begin
                  WRITE (' ' : 11, ' AND CALLS :');
                  With LASTPROC^.CALLED^ Do
                    Repeat
                      WRITELN;
                      WRITE (' ' : 11,NEXTPROC^.PROCNAME^.NAME:11);
                      LISTPTR^.FIRST := NEXTPROC^.FIRST;
                      WRITELINENR (22);
                      NEXTPROC := NEXTPROC^.NEXTPROC;
                    Until NEXTPROC = Nil;
                End;
              LASTPROC := LASTPROC^.CALLED^.PROCNAME;
            End;
          PAGE;
          WRITELN ('LIST OF PROCEDURE NESTINGS');
          WRITELN ('**************************');
          PROCSTRUCL := PROCSTRUCF;
          Repeat
            WRITELN;
            With PROCSTRUCL^ Do
              WRITE (' ':PROCLEVEL*3,PROCNAME^.NAME : 11,LINENR * INCREMENT : 6 );
            PROCSTRUCL := PROCSTRUCL^.NEXTPROC;
          Until PROCSTRUCL = Nil;
          WRITELN;
        End;
    End;
End {PRINTLISTE} ;

Procedure READFILENAME;

// LABEL 999;
// this is being used for break

Type   LINETYP = PACKED ARRAY [1..80] Of CHAR;

Var 
  LEGALCHAR : SET Of CHAR;    {MENGE DER LEGALEN EINGABEZEICHEN}
  MAXINDEX : INTEGER;         {MAXIMALER INDEX FUER DIE FUELLUNG DES FELDES 'FILENAME'}
  LINE: LINETYP;
  LEN,IL: INTEGER;


  //  PROCEDURE  GCML ( VAR S: LINETYP;  VAR LEN: INTEGER );   EXTERN;
  // This is equivalent to ParamCount and ParamStr

  // This procedure parses a file name into [Proj,Prog]File.Nam as
  // used bt the PDP 11; it has been superceded

Begin {READFILENAME}

  // most of this procedure is unnecessary

  // IOS := [SPOOL];
  IL := 5;
  EOB := TRUE;

  // GCML ( LINE, LEN );
  len := 0;
 {
  IF LEN < 3 THEN
      BEGIN}
  WRITE ('file: ');
  //         BREAK;
  // This means to stop buffering terminal output
  // for interactive communications
  //         Readln(TTY);
  Readln ( LINE );

  //          IF EOF(TTY) THEN exit; // GOTO 999;
  If line='' Then exit;
  // GOTO 999;
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
End {READFILENAME} ;

Begin {MAIN}
  INIT;
  write ('File: ');
  Readln (inputfile.FileName);
  assign(input,inputfile.filename);
  Reset(input);
  Assign(TTY,'CON:');
  rewrite(TTY);

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
    Writeln (FileName,' not found.');
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
  MAXINC := 29999 Div INCREMENT ;
  With OUTPUTFILE Do
    Begin
      // REWRITE (NEWFIL,FILENAME);

 { I'll tuen this off until I'm certain I want it
    to write source files
   Assign(NEWFIL,FILENAME);
  }
      Assign(NEWFIL,'NUL:');
      Rewrite(Newfil);
      For I:=0 To 3 Do
        FILENAME[PIX+I] := CRL[I];
      //  REWRITE (OUTPUT,FILENAME,,,IOS);
{ Same thing, we'll see about it
   Assign(OUTPUT,FILENAME);
   Rewrite(Output);
   }
    End;
  CH := ' ';
  DATUM;
  HEADER;
  BLOCK;
  WRITELINE (BUFFLEN+2);
  If Not ERRFLAG
    Then WRITE (TTY,'NO ');
  WRITELN (TTY,MESSAGE);
  PRINTLISTE;
  // 99:
  WRITELN;
  Close(CrefFile);
End {MAIN} .
