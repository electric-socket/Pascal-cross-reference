PROGRAM NewPcross(input, output) ;
(* $N+       ***********
PCROSS is a cross-referencing program for PASCAL source files,
useful also to reformat programs (indentation, upper-lower case)
and to find omitted or excessive BEGINS and ENDS.

comments, gripes and bugs to A.ARMANDO

subtopics:
        INPUT AND OUTPUT
        HOW TO USE IT
        SWITCHES
        READING THE OUTPUT
<INPUT AND OUTPUT>
INPUT:
-----
through the file OLDSOURCE, a PASCAL source program file.

OUTPUT:
------
through NEWSOURCE:
        a copy of your input file, unnumbered, prettyprinted
        (proper indentation according to the statement nesting,
         newline on standard places in the program, management of upper
         and lower case according to the list of reserved words, etc.)

and trough  CROSSLIST:
        (a) a prettyprinted version of the program, showing in the left
            margin the relations between BEGIN..END pairs,THEN..ELSE
            pairs, etc.
        (b) a cross reference of all the identifiers
        (c) a report of procedure and function declaration nesting.

By default, PCROSS will change all reserved words and strings to upper
case, comments and everything else to lower case. There are switches
available to change this condition.
<HOW TO USE IT>
You can:
        (a) use the /CREF switch when @EXECUTEing your program
             or calling @PASCAL, or
        (b) call it directly by typing @PCROSS.

In the first case, PCROSS will be called immediately after compilation;
    You will have to type the file names, though. We are working to fix
    that detail soon. Two sample calls are:

        @pcross                         AND     @exec myprog/cref
        OLDSOURCE= myprog.pas/nocross             (*COMPILER STUFF HERE*)
        NEWSOURCE= myprog.pas                   OLDSOURCE= myprog.pas
        CROSSLIST=                              NEWSOURCE=
                                                CROSSLIST=

The left one calls it directly. The effect is the prettyprinting of the
program file itself. The right one is a call through the compiler, which
will use the default options.

The program parameters all have dafault values, but they can be modified
if desired, by the use of switches. See the subtopic SWITCHES.
See the subtopic SWITCHES.
<SWITCHES>
                                        Brackets indicate optional.
                                        <n> stands for an integer number.
                                        <L> stands for a letter.

   Switch          Meaning                                      Default.

        FILES.
                 <n> is the sum of:
                       1   source program listing
                       2   listing of identifiers
                       4   listing of proc-func
                           declaration nesting.
                       8   listing of proc-func call nesting.

        PAGE AND LINE FORMAT
                margin every fifth line                        on

        STATEMENT FORMAT
                  begin..end block is indented n spaces further.
                if it is there, the block will not be indented,
                  but the begin and end statements will be
                  exdented n spaces.                           0
                 after begin, end, then, else, repeat, etc.)   off
                 comment from old standards to
                 '('-'*' and '*'-')'                           on

        UPPER AND LOWER CASE
                       note: the possible values for <l> are:
                               u means upper case
                               l means lower case.

<READING THE OUTPUT>

The cross-reference file CROSSLIST contains 4 parts:

1. THE PROGRAM LISTING:
        the letters and numbers in the left margin indicate the presence
        of reserved words which have to match. (BEGIN-END, etc.) The two
        words that match accorde with the scope rules will have the
        same number. The first one (e. g., BEGIN) will appear at left, and
        the second one (e.g., END) at right, inside the margin. The meaning
        of the letters is:
                B  Begin        E end
                I  If           T  Then         S Else
                L  Loop         X  Exit         E End
                C  Case         E  End
                R  Repeat       U  Until

2. THE CROSS REFERENCE LISTING OF IDENTIFIERS:
        It is ordered alphabetically. For each identifier it contains:
            a. A 'P' or 'F' if it is a procedure or function, respectively.
            b. The name of the identifier, up to 10 symbols.
            c. The line numbers in which it occur. Those lines in which
                  it is declared are marked with a D; those in which it
                  occurs more than once are marked with an M (Multiple).

3. THE NESTING OF PROCEDURE-FUNCTION DECLARATION:
        It describes the static links. The scope is shown by indentation.
        Each line describes a procedure or function and contains:
            a. The name.
            b. an (M), (P) or (F) if it is the main program, a procedure
                  or a function, respectively.
            c. If there are more than one procedure-function with the same
                  name (in different scopes, which is perfectly valid, but
                  a horrible thing to do), a D  (for DOUBLE) appears next,
                  and the rest of the information in this and the next part
                  of the listing contains data on both (or all of them, if
                  there are more than one.).
            d. If it is an external procedure, an E will appear next.
            e. The line number where the header appeared.
            f. The line number of the BEGIN statement.
            g. The line number of the END statement.

4. THE NESTING OF PROCEDURE-FUNCTION CALLS:
        It describes the dynamic links. Calling depth is indicated by
        indenting. The format of each line is the same as in the previous
        report, except for the following:
            a. Lines are numbered.
            b. A procedure can be called from more than one place. To avoid
                repeating the list of proc-funcs called by it,an asterisk
                is printed after the name to indicate that it has already
                been 'described'. Instead of the line numbers for its
                appearence in the program, that for its appearence in this
                part of the listing is given, so that you can refer to it.
*************)



(******************************************************************************)



(* $T-,R100   *)
(*PROGRAM WHICH CREATES A CROSS REFERENCE LISTING WITH SIMULTANEOUS
 FORMATTING OF A PASCAL PROGRAM.       WRITTEN BY MANUEL MALL.*)




(*          INDEX           *)



(**) (*DECLARATIONS*)
(**)     (*INITPROCEDURES*)
(**)     (*CHECKOPTIONS[*) (*SETSWITCH*)
(**)     (*PAGE AND LINE CONTROL*) (*HEADER*) (*NEWPAGE*) (*NEWLINE*)
(**)     (*BLOCK[*) (*OUTPUT PROCEDURES:*) (*ERROR*) (*WRITELINE[*) (*USETABS*)
(*]*) (*SETLASTSPACES*)
(**)        (*INPUT PROCEDURES:*) (*INSYMBOL[*) (*READBUFFER*)
(**)            (*RESWORD*) (*FINDNAME*) (*PARENTHESE*)
(**)            (*DOCOMMENT*) (*]INSYMBOL*)
(**)        (*"PARSING" PROCEDURES:*) (*RECDEF[*) (*CASEDEF*) (*PARENTHESE*) (*]
*)
(**)        (*STATEMENT[*) (*AND ITS PARTS*) (*]*)
(**)     (*]BLOCK*)
(**)     (*PRINT_XREF_LIST[*) (*CHECKPAGE*) (*WRITEPROCNAME*) (*WRITELINENR*) (*
DUMPCALL*)
(**)     (*]PRINT_XREF_LIST*)
(**) (*MAIN PROGRAM*)
(*DECLARATIONS*)

    (**********************************************************************
     *
     *
     *       PROGRAM WHICH CREATES A CROSS REFERENCE LISTING
     *       AND A NEW, REFORMATTED VERSION OF A PASCAL PROGRAM.
     *
     *       INPUT:  PASCAL SOURCE FILE.
     *       OUTPUT: NEW REFORMATTED SOURCE FILE AND
     *               CROSS-REFERENCE LISTING.
     *
     *       AUTHOR: MANUEL MALL (1974).
     *
     *       MODIFIED AT STANFORD UNIVERSITY BY LARRY PAULSON.
     *                       + NOT AS MANY FORCED NEWLINES.
     *                       + THE REPORT ON PROCEDURE CALLS WAS CANCELLED.
     *
     *       MODIFIED AT STANFORD UNIVERSITY BY ARMANDO R. RODRIGUEZ. 24-MAR-78.
     *                       + A NEW SET OF SWITCH OPTIONS.
     *                       + SOME NEW ERRORS ARE REPORTED.
     *
     *       SEE THE PROCEDURE CHECKOPTIONS FOR THE AVAILABLE SWITCHES.
     *
     *       MODIFIED AT STANFORD UNIVERSITY BY ARMANDO R. RODRIGUEZ. 6-JUL-88.
     *               + ACCEPT COMMENTS BETWEEN '%' AND BACKLASH  AND BETWEEN '/*
' AND '*/'.
     *               + MARK 'P' OR 'F' IN FRONT OF THE PROC-FUNC NAMES.
     *               + MARK 'D' FOR DECLARATIONS AND 'M' FOR MULTIPLE OCCURRENCE
S.
     *               + SWITCH CLEAN/NOCLEAN TO STANDARIZE COMMENTS.
     *               + LISTING OF PROC-FUNC CALL NESTING.
     *               + REPORT THE LINE NUMBERS OF BEGIN AND END OF BODY OF PROCE
DURES.
     *          THINGS TO BE FIXED, OR DOCUMENTED:
     *                  + IF THERE ARE TWO PROCS WITH ONE NAME, IT MIXES THEM.
     *                  + IF A PROC NAME IS USED AS A VAR LATER, IT WILL BE SEEN
     *                      AS A PROC FOR CALL-NESTING.
     *                  + MAKE IT SMART ENOUGH TO AVOID CREATING STRUCTURES
     *                      THAT WON'T BE USED, WHEN CROSS IS NOT 15.
     *                  + MAKE IT RUN FASTER IN GENERAL. (TRY SPECIFICALLY I/O).
     *
     *
     ***********************************************************************)




{**********************************************************************
 *                                                                    *
 *                                                                    *
 *   PCROSS is now  modified  to  run  under  the  Stanford  PASCAL   *
 *   system.   The  various  option switches for the program should   *
 *   now be specified in special comments of the following form:      *
 *                                                                    *
 *     (*%X+,F+,N-,R+,I-,C+,... other comments *)                     *
 *                                                                    *
 *     where:                                                         *
 *                                                                    *
 *     X+/- --> generate/do not generate a cross reference table,     *
 *     F+/- --> generate/do not generate a reformatted listing,       *
 *     N+/- --> allow/do not allow nesing of comments,                *
 *     R+/- --> use upper/lower case in printing the reserved words , *
 *     I+/- --> use upper/lower case in printing the identifieres,    *
 *     C+/- --> use upper/lower case in printing comments.            *
 *                                                                    *
 *                                                                    *
 *                                                                    *
 *   PCROSS in general, and its reformating function in particular,   *
 *   can be fairly time consuming and it  is  known  to  have  some   *
 *   problems.                                                        *
 *                                                                    *
 *                                                                    *
 *                                                                    *
 *                               Sassan Hazeghi                       *
 *                                                                    *
 *                               Stanford Linear Accelerator Center   *
 *                               P. O. Box 4349                       *
 *                               Stanfor, CA 94305.                   *
 *                                                                    *
 *                               Jan. 22 1979.                        *
 *                                                                    *
 *                                                                    *
 **********************************************************************}




    LABEL  444;

CONST
    version ='PCROSS: VERSION OF 4-aug-78 AT STANFORD UNIVERSITY';
    maxline = 55;                         (*MAXIMUM NUMBER OF LINES PER PAGE, IG
NORING HEADER*)
    oldwidth =200;                        (*MAXIMUM LENGTH OF INPUT LINES*)
    max_line_count = 7777(*B*);           (*LIMIT OF LINES/EDIT-PAGE*)
    max_page_count = 77(*B*);             (*LIMIT OF EDIT-PAGES*)
    (*          MAX_LINE_COUNT AND MAX_PAGE_COUNT SHOULD NOT NEED MORE THAN 18 B
ITS TOTAL*)
    ht = 9 (*11B*);                      (*ASCII HORIZONTAL TAB*)
    ff = 12 (*14B*);                     (*ASCII FORM FEED*)
    cr = 13 (*15B*);                     (*ASCII CARRIAGE RETURN*)
    blanks = '          ';               (*FOR EDITING PURPOSES*)
    dots = '  .   .   .   .   .   .   .   .   .   .';
        (*'  .   .   .   .   .   .   .   .   .   .   .   .   .   .'; *)

TYPE
    pack6 = PACKED ARRAY[1..6] OF char;
    pack9 = PACKED ARRAY[1..9] OF char;
{P} alfa = string;

    errkinds = (begerrinblkstr,enderrinblkstr,missgenduntil,missgthen,missgof,
missgexit,
                missgrpar,missgquote,linetoolong,missgmain,missgpoint);
    lineptrty = ^line;
    listptrty = ^list;
    procstructy = ^procstruc;
    calledty = ^called;

    linenrty = 0..max_line_count;
    pagenrty = 0..max_page_count;

    symbol = (labelsy,constsy,typesy,varsy,programsy,             (*DECSYM*)
              functionsy,proceduresy,initprocsy,                  (*PROSYM*)
              endsy,untilsy,elsesy,thensy,exitsy,ofsy,dosy,eobsy, (*ENDSYMBOLS*)
              beginsy,casesy,loopsy,repeatsy,ifsy,                (*BEGSYM*)
              recordsy,forwardsy,gotosy,othersy,intconst,ident,strgconst,
externsy,langsy,forsy,whilesy,
              rbracket,rparent,semicolon,point,lparent,lbracket,colon,eqlsy,
otherssy(*DELIMITER*),
              letterup, letterlo, digit,quotech, dquotech, spacech,
              dollarch, undsch, skipch ) ;

    line = PACKED RECORD
                      (*DESCRIPTION OF THE LINE NUMBER*)
                      linenr : linenrty;            (*LINE NUMBER*)
                      pagenr : pagenrty;            (*PAGE NUMBER*)
                      contlink : lineptrty;         (*NEXT LINE NUMBER RECORD*)
                      declflag: char;               (*'D' IF DECLARATION, 'M' IF
 MULTIPLE OCCURRENCE, BLANK OTHERWISE*)
                  END;

    list = PACKED RECORD
                      (*DESCRIPTION OF IDENTIFIERS*)
                      name : alfa;                  (*NAME OF THE IDENTIFIER*)
                      llink ,                       (*LEFT SUCCESSOR IN TREE*)
                      rlink : listptrty;            (*RIGHT SUCCESSOR IN TREE*)
                      first ,                       (*POINTER TO FIRST LINE NUMB
ER RECORD*)
                      last  : lineptrty;            (*POINTER TO LAST LINE NUMBE
R RECORD*)
                      externflag: char;             (*'E' IF EXTERNAL, 'F' IF FO
RWARD,
                                                     'D' IF TWO PROCS WITH THE S
AME NAME, BLANK OTHERWISE*)
                      profunflag : char;            (*'P' IF PROCEDURE NAME, 'F'
 IF FUNCTION, BLANK OTHERWISE*)
                      procdata: procstructy;
                  END;


    procstruc = PACKED RECORD
                           (*DESCRIPTION OF THE PROCEDURE NESTING*)
                           procname : listptrty;    (*POINTER TO THE APPROPRIATE
 IDENTIFIER*)
                           nextproc : procstructy;  (*POINTER TO THE NEXT ELEMEN
T*)
                           linenr,                  (*LINE NUMBER OF THE PROCEDU
RE DEFINITION*)
                           begline,                 (*LINE NUMBER OF THE BEGIN S
TATEMENT*)
                           endline: linenrty;       (*LINENUMBER OF THE END STAT
EMENT*)
                           pagenr ,                 (*PAGE NUMBER OF THE PROCEDU
RE DEFINITION*)
                           begpage,                 (*PAGE NUMBER OF THE BEGIN S
TATEMENT*)
                           endpage,                 (*PAGE NUMBER OF THE END STA
TEMENT*)
                           proclevel: pagenrty;     (*NESTING DEPTH OF THE PROCE
DURE*)
                           firstcall: calledty;     (*LIST OF PROCEDURES CALLED
BY THIS ONE*)
                           printed: boolean;        (*TO AVOID LOOPS IN THE CALL
-NEST LIST*)
                       END;

    called = PACKED RECORD
                        nextcall : calledty;
                        whom : procstructy;
                    END;

VAR
    (*                    INPUT CONTROL*)
    i,                                    (*INDEX VARIABLE*)
    bufflen,                              (*LENGTH OF THE CURRENT LINE IN THE IN
PUT BUFFER*)
    buffmark,                             (*LENGTH OF THE ALREADY PRINTED PART O
F THE BUFFER*)
    bufferptr,                            (*POINTER TO THE NEXT CHARACTER IN THE
 BUFFER*)
    syleng,                               (*LENGTH OF THE LAST READ IDENTIFIER O
R LABEL*)

    (*                    NESTING AND MATCHING CONTROL*)
    bmarknr,                              (*NUMBER FOR MARKING OF 'BEGIN', 'LOOP
' ETC.*)
    emarknr,                              (*NUMBER FOR MARKING OF 'END', 'UNTIL'
 ETC.*)
    level,                                (*NESTING DEPTH OF THE CURRENT PROCEDU
RE*)
    variant_level,                        (*NESTING DEPTH OF VARIANTS*)
    blocknr,                              (*COUNTS THE STATEMENTS 'BEGIN', 'CASE
', 'LOOP', 'REPEAT', 'IF'*)
    errcount,                              (*COUNTS THE ERRORS ENCOUNTERED*)

    (*                    FORMATTING*)
    maxch,                                (*PAGE WIDTH IN COLS FOR CROSSLIST*)
    increment,                            (*LINE NUMBER INCREMENT*)
    indentbegin,                          (*INDENTATION AFTER A BEGIN*)
    begexd,                               (*EXDENTATION FOR BEGIN-END PAIRS*)
    feed,                                 (*INDENTATION BY PROCEDURES AND BLOCKS
*)
    spaces,                               (*INDENTATION FOR THE FORMATTING*)
    lastspaces,                           (*ONE-TIME OVERRIDING VALUE FOR SPACES
*)
    goodversion,                          (*keeps the value of the version optio
n*)
    pagecnt,                              (*COUNTS THE FILE PAGES*)
    pagecnt2,                             (*COUNTS THE PRINT PAGES PER FILE PAGE
*)
    maxinc,                               (*GREATEST ALLOWABLE LINE NUMBER*)
    reallincnt,                           (*COUNTS THE LINES  PER PRINT PAGE*)
    linecnt : integer;                    (*COUNTS THE LINES  PER FILE PAGE*)

    rtime: ARRAY[0..3] OF integer;        (*CPU TIME REPORT*)

    procstrucdata : RECORD
                        (*NEXT PROCEDURE TO BE PUT IN NESTING LIST*)
                        exists : boolean;
                        item : procstruc;
                    END;

    flagger : ARRAY [-1..202] OF boolean; (*INDICATOR FOR UPPER AND LOWER CASE P
RINTING*)
    buffer  : ARRAY [-1..202] OF char;    (*INPUT BUFFER*)
    (*          BUFFER HAS 2 EXTRA POSITIONS ON THE LEFT AND ONE ON THE RIGHT*)

    tabs: ARRAY [1..17] OF (*ascii*) char;   (*A STRING OF TABS FOR FORMATTING*)

    linenb : PACKED ARRAY [1..5] OF char; (*SOS-LINE NUMBER*)
   {date_text,time_text: alfa;}           (*HEADING DATE AND TIME*)
    prog_name,                            (*NAME OF CURRENT PROGRAM*)
    sy      : alfa;                       (*LAST SYMBOL READ*)
    syty    : symbol;                     (*TYPE OF THE LAST SYMBOL READ*)

    (*                    SWITCHES*)
    renewing,                             (*SET IF THE NEWLSOURCE FILE IS BEING
WRITTEN*)
    crossing,                             (*SET IF THE CROSSLIST FILE IS BEING W
RITEN*)
    refing,                               (*SET IF THE REFERENCES WILL BE PRINTE
D*)
    decnesting,                           (*SET IF THE PRO-FUNC DECLARATION LIST
ING WILL BE PRINTED*)
    callnesting,                          (*SET IF THE PRO-FUNC CALL NESTING WIL
L BE PRINTED*)
    doting,                               (*SET IF DOTED LINES WILL BE PRINTED A
T LEFT MARGIN*)
    forcing,                              (*SET IF THEN, ELSE, DO, REPEAT WILL F
ORCE NEWLINE*)
nestcomments,                             (*ACCEPT NESTED COMMENTS*)
    cleaning,                             (*SET IF COMMENTS WILL BE STANDARIZD*)
    rescase,                              (*SET IF RESERVED WORDS WILL UPSHIFT*)
    nonrcase,                             (*SET IF NONRESERVED WORDS WILL UPSHIF
T*)
    comcase,                              (*SET IF COMMENTS WILL UPSHIFT*)
    strcase,                              (*SET IF STRINGS WILL UPSHIFT*)
    thendo,                               (*SET WHENEVER 'SPACES := SPACES+DOFEE
D' IS EXECUTED*)
    anyversion,                           (*set if goodversion > 9*)

    (*                    OTHER CONTROLS*)
    fwddecl,                              (*SET TRUE BY BLOCK AFTER 'FORWARD', '
EXTERN'*)
    oldspaces,                            (*SET WHEN LASTSPACES SHOULD BE USED*)
    eoline,                               (*SET AT END ON INPUT LINE*)
    gotoinline,                           (*SET IF A HORRENDOUS GOTO STATEMENT I
N THIS LINE*)
    declaring,                            (*SET WHILE PARSING DECLARATIONS*)
    programpresent,                       (*SET AFTER PROGRAM ENCOUNTERED*)
    nobody,                               (*SET IF NO MAIN BODY IS FOUND*)
    eob     : boolean;                    (*EOF-FLAG*)

                        (**)
    ch,                                   (*LAST READ CHARACTER*)
    bmarktext,                            (*CHARACTER FOR MARKING OF 'BEGIN' ETC
.*)
    emarktext: char;                      (*CHARACTER FOR MARKING 'END' ETC.*)

    (*                    SETS*)
    delsy : ARRAY [CHAR{' '..'_'}] OF symbol; (*TYPE ARRAY FOR DELIMITER CHARS*)
    resnum: ARRAY[char] OF integer;       (*INDEX OF THE FIRST KEYWORD BEGINNING
 WITH THE INDEXED LETTER*)
    reslist : ARRAY [1..46] OF alfa;      (*LIST OF THE RESERVED WORDS*)
    ressy   : ARRAY [1..46] OF symbol;    (*TYPE ARRAY OF THE RESERVED WORDS*)
   {alphanum,                             (*CHARACTERS FROM 0..9 AND A..Z*)
    digits,                               (*CHARACTERS FROM 0..9*)
    letters : SET OF char; }              (*CHARACTERS FROM A..Z*)
    relevantsym,                          (*START SYMBOLS FOR STATEMENTS AND PRO
CEDURES*)
    prosym,                               (*ALL SYMBOLS WHICH BEGIN A PROC.*)
    decsym,                               (*ALL SYMBOLS WHICH BEGIN DECLARATIONS
*)
    begsym,                               (*ALL SYMBOLS WHICH BEGIN COMPOUND STA
TEMENTS*)
    endsym  : SET OF symbol;              (*ALL SYMBOLS WHICH TERMINATE  STATEME
NTS OR PROCEDURES*)

    (*                    POINTERS AND FILES*)
    listptr, heapmark : listptrty;        (*POINTER INTO THE BINARY TREE OF THE
IDENTIFIER*)
    firstname : ARRAY [char    ] OF listptrty;    (*POINTER TO THE ROOTS OF THE
TREE*)
    procstrucf,                           (*POINTER TO THE FIRST ELEMENT OF THE
PROCEDURE CALLS LIST*)
    procstrucl : procstructy;             (*POINTER TO THE LAST ELEMENT OF THE P
ROCEDURE CALLS LIST*)
    workcall: calledty;
    link_name,
    new_name, cross_name: pack9;          (*USED TO GET THE PARAMETER FILES*)
    old_dev,link_device,
    new_dev,cross_dev:pack6;
    old_prot,old_ppn,
    new_prot,new_ppn,cross_prot,cross_ppn: integer;
    programname,oldfileid,newfileid,crossfileid: alfa;
    TTY,
    OLDSOURCE,NEWSOURCE,CROSSLIST: text;  (*FILES PROCESSED BY THIS PROGRAM*)

    (*INITPROCEDURES*)

  PROCEDURE init0; {INITPROCEDURE;}
    BEGIN (*CONSTANTS*)
    eob := false;
    maxch:=114;
    increment:= {100} 1;
    feed:=4;
    indentbegin:=0;
    nestcomments := true ;
    cleaning := true ;
    begexd:=0;
    rescase:=true;
    nonrcase:=false;
    comcase:=false;
    strcase:=true;
    renewing:=true;
    crossing:=true;
    refing:= true (*false*);
    decnesting:= true (*false*);
    callnesting:=true  (*false*);
    doting:=false (*true*);
    nobody := false;
    anyversion := false;
    goodversion := -1;
    new_name:='         ';
    cross_name:='         ';
    programname:='PCROSS    ';
    oldfileid:='OLDSOURCE ';
    newfileid:='NEWSOURCE ';
    crossfileid:='CROSSLIST ';
    END (*CONSTANTS*);


   procedure init1; {INITPROCEDURE;}
   Var
        RC : Integer;

    BEGIN (*RESERVED WORDS*)
    RC :=  0;
    resnum['B'] :=  3;    resnum['C'] :=  4;
    resnum['D'] :=  6;    resnum['E'] :=  9;    resnum['F'] := 13;
    resnum['G'] := 18;    resnum['H'] := 19;    resnum['I'] := 19;
    resnum[succ('I')] := 22;
    resnum['J'] := 22;    resnum['K'] := 22;    resnum['L'] := 22;
    resnum['M'] := 24;    resnum['N'] := 25;    resnum['O'] := 27;
    resnum['P'] := 30;    resnum['Q'] := 33;    resnum['R'] := 33;
    resnum[succ('R')] := 35 ;
    resnum['S'] := 35;    resnum['T'] := 36;    resnum['U'] := 39;
    resnum['V'] := 40;    resnum['W'] := 41;    resnum['X'] := 43;
    resnum['Y'] := 43;    resnum['Z'] := 43;    resnum[succ('Z')] := 43;





    inc(rc); reslist[ 1] :='AND';        ressy [ 1] := othersy;      resnum['A'] :=  RC;
    inc(rc); reslist[ 2] :='ARRAY';      ressy [ 2] := othersy;
    inc(rc); reslist[ 3] :='BEGIN';      ressy [ 3] := beginsy;
    inc(rc); reslist[ 4] :='CASE';       ressy [ 4] := casesy;
    inc(rc); reslist[ 5] :='CONST';      ressy [ 5] := constsy;
    inc(rc); reslist[ 6] :='DO';         ressy [ 6] := dosy;
    inc(rc); reslist[ 7] :='DIV';        ressy [ 7] := othersy;
    inc(rc); reslist[ 8] :='DOWNTO';     ressy [ 8] := othersy;
    inc(rc); reslist[ 9] :='END';        ressy [ 9] := endsy;
    inc(rc); reslist[10] :='ELSE';       ressy [10] := elsesy;

    inc(rc); reslist[11] :='EXIT';       ressy [11] := exitsy;
    inc(rc); reslist[12] :='EXTERN';     ressy [12] := externsy;
    inc(rc); reslist[13] :='FOR';        ressy [13] := forsy;
    inc(rc); reslist[14] :='FILE';       ressy [14] := othersy;
    inc(rc); reslist[15] :='FORWARD';    ressy [15] := forwardsy;
    inc(rc); reslist[16] :='FUNCTION';   ressy [16] := functionsy;
    inc(rc); reslist[17] :='FORTRAN';    ressy [17] := langsy;
    inc(rc); reslist[18] :='GOTO';       ressy [18] := gotosy;
    inc(rc); reslist[19] :='IF';         ressy [19] := ifsy;
    inc(rc); reslist[20] :='IN';         ressy [20] := othersy;

    inc(rc); reslist[21] :='INITPROCED'; ressy [21] := initprocsy;
    inc(rc); reslist[22] :='LOOP';       ressy [22] := loopsy;
    inc(rc); reslist[23] :='LABEL';      ressy [23] := labelsy;
    inc(rc); reslist[24] :='MOD';        ressy [24] := othersy;
    inc(rc); reslist[25] :='NOT';        ressy [25] := othersy;
    inc(rc); reslist[26] :='NIL';        ressy [26] := othersy;
    inc(rc); reslist[27] :='OR ';        ressy [27] := othersy;
    inc(rc); reslist[28] :='OF ';        ressy [28] := ofsy;
    inc(rc); reslist[29] :='OTHERS';     ressy [29] := otherssy;
    inc(rc); reslist[30] :='PACKED';     ressy [30] := othersy;

    inc(rc); reslist[31] :='PROCEDURE';  ressy [31] := proceduresy;
    inc(rc); reslist[32] :='PROGRAM';    ressy [32] := programsy;
    inc(rc); reslist[33] :='RECORD';     ressy [33] := recordsy;
    inc(rc); reslist[34] :='REPEAT';     ressy [34] := repeatsy;
    inc(rc); reslist[35] :='SET';        ressy [35] := othersy;
    inc(rc); reslist[36] :='THEN';       ressy [36] := thensy;
    inc(rc); reslist[37] :='TO ';        ressy [37] := othersy;
    inc(rc); reslist[38] :='TYPE';       ressy [38] := typesy;
    inc(rc); reslist[39] :='UNTIL';      ressy [39] := untilsy;
    inc(rc); reslist[40] :='VAR';        ressy [40] := varsy;

    inc(rc); reslist[41] :='WHILE';      ressy [41] := whilesy;
    inc(rc); reslist[42] :='WITH';       ressy [42] := othersy;
    END;


  procedure init2; {INITPROCEDURE;}
    BEGIN (*SETS*)
   {digits := ['0'..'9'];
    letters := ['A'..'Z'];
    alphanum := ['0'..'9','A'..'Z'] (*LETTERS OR DIGITS*); }
    decsym := [labelsy,constsy,typesy,varsy,programsy];
    prosym := [functionsy..initprocsy];
    endsym := [functionsy..eobsy];      (*PROSYM OR ENDSYMBOLS*)
    begsym := [beginsy..ifsy];
    relevantsym := [labelsy..initprocsy (*DECSYM OR PROSYM*),beginsy,forwardsy,
externsy,eobsy];
    END (*SETS*);


PROCEDURE init;
    BEGIN
    init0 ;
    init1 ;
    init2 ;
{P Time
    rtime[0]:=clock(1);
}
    new(heapmark);    (*THE HEAP IS DEALLOCATED AFTER EACH PROGRAM*)
    workcall := NIL;
    i := 0;
    bufflen := 0;
    buffmark := 0;
    bufferptr := 2;
    variant_level := 0;
    reallincnt:= 0;
    linecnt :=0;
    blocknr := 0;
    level := 0;
    pagecnt := 1;
    pagecnt2 := 0;
    errcount := 0;
    eoline := true;
    gotoinline := false;
    programpresent := false;
    procstrucdata.exists := false;
    oldspaces := false;
    declaring := false;
    bmarktext := ' ';
    emarktext := ' ';
    sy := blanks;   prog_name := blanks;
   {date(date_text);  time(time_text);}
    FOR ch := chr(0) to chr(255) DO
        firstname [ch] := NIL;
    FOR ch := ' ' TO 'Z' DO
        delsy [ch] := othersy;
      FOR ch := 'A' TO 'I' DO    delsy[ch] := letterup ;
      FOR ch := 'J' TO 'R' DO    delsy[ch] := letterup ;
      FOR ch := 'S' TO 'Z' DO    delsy[ch] := letterup ;
      FOR ch := 'a' TO 'i' DO    delsy[ch] := letterlo ;
      FOR ch := 'j' TO 'r' DO    delsy[ch] := letterlo ;
      FOR ch := 's' TO 'z' DO    delsy[ch] := letterlo ;
      FOR ch := '0' TO '9' DO    delsy[ch] := digit ;
      delsy['"']  := dquotech;
      delsy['#']  := skipch ;
      delsy['$']  := dollarch ;
      delsy[''''] := quotech ;
      delsy['_']  := undsch ;
    delsy [' '] := spacech;
    delsy ['('] := lparent;
    delsy [')'] := rparent;
    delsy ['['] := lbracket;
    delsy [']'] := rbracket;
    delsy [';'] := semicolon;
    delsy ['.'] := point;
    delsy [':'] := colon;
    delsy ['='] := eqlsy;
    FOR i := -1 TO 201 DO
        buffer [i] := ' ';
    i := 0;
    new (firstname['M']);
    listptr := firstname ['M'];
    WITH firstname ['M']^ DO
        BEGIN
        name := 'MAIN.     ';
        llink := NIL;
        rlink := NIL;
        profunflag := 'M';
        new (first);
        last := first;
        WITH last^ DO
            BEGIN
            linenr := 1;
            pagenr:=1;
            contlink := NIL;
            END;
        END;
    new (procstrucf);
    WITH procstrucf^ DO
        BEGIN
        procname := firstname ['M'];
        nextproc := NIL;
        linenr   := 1;
        pagenr:=1;
        proclevel:= 0;
        firstcall := NIL;
        END;
    procstrucl := procstrucf;
    ch := ' ';
    FOR i := 1 TO 17 DO
        tabs [i] := {chr (ht)} ' ';
    linenb := '-----' ;
    END (*INIT*);
    (*CHECKOPTIONS[*) (*SETSWITCH*)

    (*---------------------------------------------------------------------
     !  CHECKS THE PRESENCE OF SWITCHES WITH THE FILE NAMES.
     !
     !  VALID SWITCHES ARE:                     BRACKETS INDICATE OPTIONAL.
     !                                          <N> STANDS FOR AN INTEGER NUMBER
.
     !                                          <L> STANDS FOR A LETTER.
     !
     !  SWITCH          MEANING                                         DEFAULT.
     !
     !           FILES.
     !   /[NO]NEW        WRITTING OF THE NEWSOURCE FILE                 ON
     !   /[NO]CROSS[:<N>]  WRITTING OF THE CROSSLIST FILE.              ON,15
     !                    <N> IS THE SUM OF:
     !                          1   SOURCE PROGRAM LISTING
     !                          2   LISTING OF IDENTIFIERS
     !                          4   LISTING OF PROC-FUNC
     !                              DECLARATION NESTING.
     !                          8   LISTING OF PROC-FUNC CALL NESTING.
     !   /version:<n>    behave as if conditionally compiling %<n>
     !                     comments.                                    -1
     !
     !           PAGE AND LINE FORMAT
     !   /WIDTH:<N>      PAGE WIDTH FOR CROSSLIST. (MINIMUM: 40)        132
     !   /INDENT:<N>     INDENTATION BETWEEN LEVELS.                    4
     !   /INCREMENT:<N>  LINE NUMBER INCREMENT                          100
     !   /[NO]DOTS       PUT AS A GUIDE A DOTTED LINE AT THE LEFT
     !                   MARGIN EVERY FIFTH LINE                        ON
     !
     !           STATEMENT FORMAT
     !   /BEGIN:[-]<N>   IF THE [-] IS NOT THERE, THE CONTENTS OF A
     !                     BEGIN..END BLOCK IS INDENTED N SPACES FURTHER.
     !                   IF IT IS THERE, THE BLOCK WILL NOT BE INDENTED,
     !                     BUT THE BEGIN AND END STATEMENTS WILL BE
     !                     EXDENTED N SPACES.                           0
     !   /[NO]FORCE      FORCES NEWLINE IN STANDARD PLACES. (BEFORE AND
     !                    AFTER BEGIN, END, THEN, ELSE, REPEAT, ETC.)   OFF
     !   /[NO]CLEAN      CONVERTS THE SYMBOLS FOR BEGIN AND END OF
     !                    COMMENT FROM OLD STANDARDS TO '('-'*' AND '*'-')'
ON
     !
     !           UPPER AND LOWER CASE
     !                          NOTE: THE POSSIBLE VALUES FOR <L> ARE:
     !                                  U MEANS UPPER CASE
     !                                  L MEANS LOWER CASE.
     !
     !   /RES:<L>        CASE USED FOR RESERVED WORDS.                  U
     !   /NONRES:<L>     SAME FOR NON-RESERVED WORDS.                   L
     !   /COMM:<L>       SAME FOR COMMENTS.                             L
     !   /STR:<L>        SAME FOR STRINGS.                              U
     !   /CASE:<L>       RESETS ALL THE DEFAULTS TO <L>.
OFF
     !
     +--------------------------------------------------------------------*)

// dumkmy stuv function
{P} FUNCTION Option(const Opt:alfa):Boolean;
    begin

        result := false
    end;


PROCEDURE checkoptions;
    VAR    ThisTry: integer;

    PROCEDURE setswitch(opt:alfa;VAR switch:boolean);
        VAR
            i: integer;
        BEGIN (*SETSWITCH*)
  (*    getoption(opt,i);   *)  i := ord('l') ;
        IF i=ord('L') THEN
            switch:=false
        ELSE
            IF i=ord('U') THEN
                switch:=true;
        END (*SETSWITCH*);

    PROCEDURE showthemall (i: integer);
        BEGIN
        writeln (tty,'(',i:3,')',oldfileid,',',programname);
        writeln (tty, new_name,',',new_prot,',',new_ppn,',',new_dev,',',
newfileid,',',programname);
        writeln (tty, cross_name,',',cross_prot,',',cross_ppn,',',cross_dev,',',
crossfileid,',',programname);
        writeln (tty);
         (* break(tty); *)
        END;

    BEGIN (*CHECKOPTIONS*)
               (*##############
    getparameter(oldsource,oldfileid,programname,true);
    IF NOT option('NONEW     ') THEN
      parnameget(new_name,new_prot,new_ppn,new_dev,newfileid,programname,false);
    IF NOT option('NOCROSS   ') THEN
        parnameget(cross_name,cross_prot,cross_ppn,cross_dev,crossfileid,program
name,false);

    IF NOT option('NONEW     ') THEN
        begin
        IF (new_name = '         ') AND (new_dev = 'dsk   ') THEN
            BEGIN
            getstatus(oldsource, new_name,old_prot,old_ppn,old_dev);
            new_name[7]:='N';
            new_name[8]:='E';
            new_name[9]:='W';
            END;
        startfile(newsource,new_name,new_prot,new_ppn,new_dev,false,newfileid);
        end;

    IF NOT option('NOCROSS   ') THEN
        begin
        IF (cross_name = '         ') AND (cross_dev = 'dsk   ') THEN
            BEGIN
            getstatus(oldsource, cross_name,old_prot,old_ppn,old_dev);
            cross_name[7]:='C';
            cross_name[8]:='R';
            cross_name[9]:='L';
            END;
        startfile(crosslist,cross_name,cross_prot,cross_ppn,cross_dev,false,cros
sfileid);
        end;
 #####################*)

    renewing:= true {NOT option('NONEW     ')} ;

    crossing:= true {NOT option('NOCROSS   ')} ;
    IF crossing THEN
        BEGIN
  (*    getoption('CROSS     ',ThisTry);     *) ThisTry  := 0 ;
        IF ThisTry = 0 THEN
            ThisTry:=15;
        callnesting:=ThisTry > 7;
        decnesting:=(ThisTry MOD 8) > 3;
        refing:= (ThisTry MOD 4) > 1;
        crossing:=(ThisTry MOD 2) = 1;
        END;

  (*if option('version   ') then    *)
        begin
  (*    getoption('version   ',goodversion);  *)  goodversion := 15 ;
        if goodversion > 9 then
            begin
            goodversion := -1;
            anyversion := true;
            end;
        end;

  (*IF option('WIDTH     ') THEN  *)
        BEGIN
  (*    getoption('WIDTH     ',maxch);  *)  maxch := 90 ;
        IF maxch < 40 THEN
            maxch:=40;
        maxch:=maxch-18;
        END;

                 (* #################
    IF option('INDENT    ') THEN
        BEGIN
  //    getoption('INDENT    ',feed);                 *)
        IF feed < 0 THEN
            feed:=4;
//{P}        END;

    IF option('INCREMENT ') THEN
        BEGIN
  (*    getoption('INCREMENT ',increment);            *)
        IF increment < 0 THEN
            increment:= {100} 1;
        END;

    doting:=NOT option('NODOTS    ');

    IF option('BEGIN     ') THEN
        BEGIN
  (*    getoption('BEGIN     ',indentbegin);          *)
        IF indentbegin < 0 THEN
            BEGIN
            begexd:=-indentbegin;
            indentbegin:=0;
            END;
        END;

    forcing:=option('FORCE     ');

    cleaning := NOT option('NOCLEAN   ');

    IF option('CASE      ') THEN
        BEGIN
        setswitch('CASE      ',rescase);
        nonrcase:=rescase;
        comcase:=rescase;
        strcase:=rescase;
        END;

    setswitch('RES       ',rescase);
    setswitch('NONRES    ',nonrcase);
    setswitch('COMM      ',comcase);
    setswitch('STR       ',strcase);

    END (*CHECKOPTIONS*);
    (*PAGE AND LINE CONTROL*) (*HEADER*) (*NEWPAGE*) (*NEWLINE*)

{P}  Procedure Page(Var F:Text);
     begin
         Writeln(F,Chr(12));
     end;

PROCEDURE header (name: alfa);
    (*PRINT TOP OF FORM AND HEADER ON LIST OUTPUT*)
    VAR
        position: integer;

    BEGIN (*HEADER*)
    pagecnt2 := pagecnt2 + 1;
    reallincnt := 0;
    IF crossing THEN
        BEGIN
        page(crosslist); { writeln(crosslist) ;}
        write (crosslist, 'Page':9, pagecnt:4, '-', pagecnt2:3 {, ' ':15});
        position := 84;
        IF maxch < 84 THEN
            BEGIN
            reallincnt:=1;
            writeln(crosslist);
            position := 42;
            END;
//{P}        write(crosslist,'   [', prog_name,']', time{_text}:50, date{_text});
{P}     write(crosslist,'   [', prog_name,']');
        IF (name <> blanks) AND (maxch < position + 25) THEN
            BEGIN
            reallincnt := reallincnt + 1;
            writeln (crosslist);
            position := 0;
            END;
        writeln (crosslist, ' ': maxch - position, name);
        writeln(crosslist);
        END;
    END (*HEADER*);


PROCEDURE newpage;
    BEGIN (*NEWPAGE*)
    pagecnt2 := 0;
    pagecnt := pagecnt + 1;
    IF renewing THEN
      (*write(newsource, chr(cr), chr(ff));*)    writeln(newsource) ;
    header (blanks);
    IF eoln (oldsource) THEN
        readln(oldsource);
    linecnt := 0;
    reallincnt := 0;
    IF prog_name <> blanks  THEN
        write(tty, pagecnt:3,'..');
     (* break(tty); *)
    END (*NEWPAGE*);


PROCEDURE newline;
    BEGIN
    IF reallincnt = maxline THEN
        header (blanks);
    linecnt := linecnt + 1;
    reallincnt := reallincnt + 1;
    if crossing then write(crosslist, ' ':5);
    if renewing then write(newsource, ' ':5) ;
    END;
    (*BLOCK[*) (*OUTPUT PROCEDURES:*) (*ERROR*) (*WRITELINE[*) (*USETABS*) (*]*)
 (*SETLASTSPACES*)


PROCEDURE block;
    VAR
        curproc : listptrty;        (*ZEIGER AUF DIE PROZEDUR IN DEREN ANWEISUNG
STEIL DAS PROGRAMM SICH BEFINDET*)
        itisaproc : boolean;        (*TRUE WHEN THE WORD PROCEDURE IS FOUND*)
        locprocstl: procstructy;


    PROCEDURE error (errnr : errkinds);
        BEGIN (*ERROR*)
        errcount := errcount+1;
        reallincnt := reallincnt + 1; (*COUNT THE LINE OF THE ERROR MESSAGE ON T
HE LPT: FILE*)
        IF crossing THEN
            BEGIN
            write (crosslist, ' ':17,' *??* ');
            CASE errnr OF
                begerrinblkstr   : write(crosslist, sy,' ? ? ? : ERROR IN BLOCK'
,'STRUCTURE. POSSIBLY A MISSING BEGIN.');
                enderrinblkstr   : write(crosslist, sy,' ? ? ? : ERROR IN BLOCK'
,'STRUCTURE. POSSIBLY A MISSING END.');
                missgenduntil : write(crosslist, 'MISSING ''END'' OR ''UNTIL'' '
,'NUMBER ',emarknr : 4);
                missgthen     : write(crosslist, 'MISSING ''THEN'' NUMBER ',
emarknr : 4);
                missgof       : write(crosslist, 'MISSING ''OF'' TO ''CASE'' '
,'NUMBER ',bmarknr : 4);
                missgexit     : write(crosslist, 'MISSING ''EXIT'' IN ''LOOP'' '
,emarknr : 4);
                missgrpar     : write(crosslist, 'MISSING RIGHT PARENTHESIS OR '
,'BRACKET');
                missgquote    : write(crosslist, 'MISSING CLOSING QUOTE ON THIS'
,' LINE');
                linetoolong:
                          IF renewing THEN
                              write(crosslist,'NEXT LINE IS TOO LONG. IT WILL '
,'BE BROKEN AT COLUMN',
                                    oldwidth:4,' IN NEWSOURCE.');
                missgmain     : write(crosslist, 'WARNING: THIS FILE HAS NO '
,'MAIN BODY');
                missgpoint    : write(crosslist,'MISSING CLOSING POINT AT END '
,'OF PROGRAM.');
                END;
            writeln(crosslist,' *??*');
            END;
        writeln(tty);
        write(tty, ' ERROR AT ', linecnt*increment: 5, '/', pagecnt:2,
                    ord(errnr):12);
        END (*ERROR*) ;


    PROCEDURE writeline (position (*LETZTES ZU DRUCKENDES ZEICHEN IM PUFFER*):
integer);
        VAR
            i, j, tabcnt, maxchar: integer;    (*MARKIERT ERSTES ZU DRUCKENDES Z
EICHEN*)


        PROCEDURE usetabs(lastspaces: integer;crossing,renewing:boolean);
            VAR
                lspaces: integer;
            BEGIN (*USETABS*)
            (*USE TABS AND SPACES TO MAKE INDENTATION*)
            tabcnt := lastspaces DIV 8;
            lspaces := lastspaces MOD 8;
            IF renewing THEN
                BEGIN
                write(newsource, tabs:tabcnt*8);
                write(newsource, ' ': lspaces);
                END;
            IF crossing THEN
                IF doting AND ((reallincnt MOD 5) = 4) THEN
                    BEGIN
                    write(crosslist, ' ', dots: lastspaces - 1);
                    IF lastspaces > 0 THEN
                        write (crosslist, ' ');
                    END
                ELSE
                    BEGIN
                    write(crosslist, tabs: tabcnt*8);
                    write (crosslist, ' ': lspaces);
             (*+++  IF lastspaces > 7 THEN              (*COMPENSATE FOR THE FIR
ST TAB*)
                        write (crosslist, '  ');    +++*)
                    END;
            END (*USETABS*);

        BEGIN (*WRITELINE*)
        position := position - 2;
        IF position > 0 THEN
            BEGIN
            i := buffmark + 1;
            WHILE (buffer [i] = ' ') AND (i <= position) DO
                i := i + 1;
            buffmark := position;
            WHILE (buffer [position] = ' ') AND (i < position) DO
                position := position - 1;
            IF i <= position THEN
                BEGIN
                IF NOT programpresent THEN
                    BEGIN
                    programpresent := true;
                    header (blanks);
                    END;
                newline;
                IF crossing THEN
                    BEGIN
                    IF gotoinline THEN
                        BEGIN
                        write(crosslist, '****GOTO****');
                        gotoinline := false;
                        bmarktext:=' ';
                        emarktext:=' ';
                        END
                    ELSE
                        BEGIN
                        IF bmarktext <> ' ' THEN
                            BEGIN
                            write (crosslist, bmarktext, bmarknr : 4, ' ');
                            bmarktext := ' ';
                            END
                        ELSE
                            write(crosslist,'      ');
                        IF emarktext <> ' ' THEN
                            BEGIN
                            write (crosslist,emarktext,emarknr : 4,' ');
                            emarktext := ' ';
                            END
                        ELSE
                            write (crosslist,'      ');
                        END;
                    write (crosslist, linecnt * increment : 5,' ');
                    END;
                IF NOT oldspaces THEN
                    lastspaces := spaces;
                usetabs(lastspaces,crossing,renewing);
                maxchar:=maxch+i-lastspaces-1;
                FOR j := i TO position DO
                    BEGIN
                    IF crossing AND (j > maxchar) THEN
                        BEGIN
                        writeln(crosslist);
                        IF reallincnt = maxline THEN
                            header (blanks);
                        reallincnt:=reallincnt+1;
                        usetabs(lastspaces+18+feed,crossing,false);
                        maxchar:=maxch+j-lastspaces-1;
                        END;
                    IF (NOT flagger[j]) AND (* ('A' <= buffer[j]) AND
                     (buffer[j] <= 'Z') *) (delsy[buffer[j]] = letterup) THEN
                        BEGIN
                        IF crossing THEN
                            write(crosslist,chr(ord(buffer[j])-64(*+40B*)));
                        IF renewing THEN
                            write(newsource,chr(ord(buffer[j])-64(*+40B*)));
                        END
                    ELSE
                    IF flagger[j] AND (delsy[buffer[j]] = letterlo)  then

                        BEGIN
                        IF crossing THEN
                            write(crosslist,chr(ord(buffer[j])+64(*+40B*)));
                        IF renewing THEN
                            write(newsource,chr(ord(buffer[j])+64(*+40B*)));
                        END
                    ELSE
                        BEGIN
                        IF crossing THEN
                            write (crosslist, buffer [j]);
                        IF renewing THEN
                            write(newsource, buffer[j]);
                        END;
                    END;
                IF crossing THEN
                    writeln(crosslist);
                IF renewing THEN
                    writeln(newsource);
                IF ((linenb = '     ') AND (position = bufflen)) OR (maxinc <=
linecnt) THEN
                    newpage;
                END;
            END;
        lastspaces := spaces;
        oldspaces := false;
        thendo := false;
        END (*WRITELINE*) ;


    PROCEDURE setlastspaces(i: integer);
        BEGIN
        oldspaces := true;
        lastspaces := 0 ;  if i > 0 then  lastspaces := i ;   (*max(0,i);*)
        END;
        (*INPUT PROCEDURES:*) (*INSYMBOL[*) (*READBUFFER*)

    PROCEDURE insymbol ;
        LABEL 1;
        VAR
            oldspacesmark,            (*ALTER ZEICHENVORSCHUB BEI FORMATIERUNG V
ON KOMMENTAREN*)
            i: integer;



        PROCEDURE readbuffer;
            (*READS A CHARACTER FROM THE INPUT BUFFER*)


            PROCEDURE readline;
                (*HANDLES LEADING BLANKS AND BLANK LINES, READS NEXT NONBLANK LI
NE
                 (WITHOUT LEADING BLANKS) INTO BUFFER*)
//{P}                LABEL 111 ;
                VAR
                    ch : char;
                BEGIN (*READLINE*)
                (*ENTERED AT THE BEGINNING OF A LINE*)
                REPEAT
                    WHILE eoln (oldsource) AND NOT eof (oldsource) DO
                        BEGIN
                        (*IS THIS A PAGE MARK?*)
                       {getlinenr (oldsource,linenb);}
                        readln(oldsource);
                        IF (linenb = '     ') AND programpresent THEN
                            newpage
                        ELSE
                            IF programpresent THEN
                                BEGIN
                                (*HANDLE BLANK LINE*)
                                newline;
                                IF crossing THEN
                                    writeln (crosslist,{chr(ht)}' ':12,linecnt *
 increment : 5);
                                IF renewing THEN
                                    writeln(newsource);
                                IF maxinc <= linecnt THEN
                                    newpage;
                                END;
                        END;
                    read(oldsource, ch);
                UNTIL (ch <> ' ') OR (eof (oldsource));
                bufflen := 0;
                (*READ IN THE LINE*)
              (*LOOP *)  repeat
                    bufflen := bufflen + 1;
                    buffer [bufflen] := ch;
                    flagger[bufflen]:=nonrcase;
              (*EXIT*) IF (eoln (oldsource) OR (bufflen = oldwidth))
//{P}                         THEN GOTO 111 ;
{P}              THEN break  ;
                    read(oldsource, ch);
                  (*END*) UNTIL FALSE ;
//{P}              111:
                buffer[bufflen+1] := ' '; (*SO WE CAN ALWAYS BE ONE CHAR AHEAD*)
                IF NOT eoln (oldsource) THEN
                    error(linetoolong)
                ELSE
                    IF NOT eof (oldsource) THEN
                        BEGIN
                       {getlinenr (linenb);}
                        readln(oldsource);
                        END;
                bufferptr := 1;
                buffmark := 0;
                END (*READLINE*) ;

            BEGIN (*READBUFFER*)
            (*IF READING PAST THE EXTRA BLANK ON THE END, GET A NEW LINE*)
            IF eoline THEN
                BEGIN
                writeline (bufferptr);
                ch := ' ';
                IF eof (oldsource) THEN
                    eob := true
                ELSE
                    readline;
                END
            ELSE
                BEGIN
                ch := buffer [bufferptr];
                bufferptr := bufferptr + 1;
                END;
            eoline := bufferptr = bufflen + 2;
            END (*READBUFFER*) ;

            (*RESWORD*) (*FINDNAME*) (*PARENTHESE*)

        FUNCTION resword: boolean ;
            (*DETERMINES IF THE CURRENT IDENTIFIER IS A RESERVED WORD*)
//{P}           LABEL 222 ;
            VAR
                i,j: integer;  lid : alfa ;
            BEGIN (*RESWORD*)
            resword:= false; lid := sy ;
            for i := 1 to 10 do
              if delsy[lid[i]] = letterlo then lid[i] := chr( ord(lid[i]) +64) ;
            FOR i:=resnum[lid[1]] TO resnum[succ(lid[1])] - 1
            DO
                IF reslist[ i ] = lid THEN
                    BEGIN
                    resword := true;
                    syty := ressy [i];
                 (* i:=resnum[succ(sy[1])]; ** BUG 1 **   *)
                    FOR j:=bufferptr-syleng-1 TO bufferptr-2 DO
                        flagger[j]:=rescase;
//{P}                     GOTO 222;
{P}                     break;
                    END;
//{P}            222:
            END (*RESWORD*) ;


        PROCEDURE findname {(curproc: listptrty)} (*does not seem to be used*);
            VAR
                lptr: listptrty;        (*ZEIGER AUF DEN VORGAENGER IM BAUM*)
                bptr,
                zptr : lineptrty;       (*ZEIGER AUF DIE VORLETZTE ZEILENNUMMER
IN EINER KETTE*)
                found,                  (*SET AFTER IDENTIFIER IS FOUND*)
                right: boolean;         (*MERKVARIABLE FUER DIE VERZWEIGUNG IM B
AUM*)
                indexch : char;         (*INDEXVARIABLE FUER DAS FELD DER STARTZ
EIGER (FIRSTNAME)*)

            BEGIN (*FINDNAME*)
            indexch := sy [1];
            listptr := firstname [indexch];
            (*SEARCH IN THE TREE FOR THE IDENTIFIER*)
            found := false;
            WHILE NOT found AND (listptr <> NIL) DO
                BEGIN
                lptr:= listptr;
                IF sy = listptr^.name THEN
                    BEGIN
                    found := true;
                    IF{(listptr^.profunflag IN ['P', 'F'])}(NOT declaring) AND
                      ((listptr^.profunflag = 'P')OR(listptr^.profunflag = 'F'))
                      THEN
                        BEGIN
                        new (workcall);
                        workcall^.whom := listptr^.procdata;
                        workcall^.nextcall := NIL;
                        END;
                    zptr := listptr^.last;
                    IF (zptr^.linenr <> linecnt+1) OR (zptr^.pagenr <> pagecnt)
THEN
                        BEGIN
                        new (listptr^.last);
                        WITH listptr^.last^ DO
                            BEGIN
                            linenr := linecnt + 1;
                            pagenr := pagecnt;
                            contlink := NIL;
                            IF declaring THEN
                                declflag := 'D'
                            ELSE
                                declflag := ' ';
                            END;
                        zptr^.contlink := listptr^.last;
                        END
                    ELSE
                        zptr^.declflag := 'M';
                    END
                ELSE
                    IF sy > listptr^.name THEN
                        BEGIN
                        listptr:= listptr^.rlink;
                        right:= true;
                        END
                    ELSE
                        BEGIN
                        listptr:= listptr^.llink;
                        right:= false;
                        END;
                END;
            IF NOT found THEN
                BEGIN (*UNKNOWN IDENTIFIER*)
                new (listptr);
                WITH listptr^ DO
                    BEGIN
                    name := sy;
                    llink := NIL;
                    rlink := NIL;
                    profunflag := ' ';
                    externflag := ' ';
                    procdata := NIL;
                    END;
                IF firstname [indexch] = NIL THEN
                    firstname [indexch] := listptr
                ELSE
                    IF right THEN
                        lptr^.rlink := listptr
                    ELSE
                        lptr^.llink := listptr;
                WITH listptr^ DO
                    BEGIN
                    new (first);
                    WITH first^ DO
                        BEGIN
                        linenr := linecnt + 1;
                        pagenr := pagecnt;
                        contlink := NIL;
                        IF declaring THEN
                            declflag := 'D'
                        ELSE
                            declflag := ' ';
                        END;
                    last := first ;
                    END;
                END;
            END (*FINDNAME*) ;

        PROCEDURE insertcall;
            VAR
                thiscall: calledty;
                repeated,               (*SET IF SY IS A PROC-NAME AND IS ALREAD
Y IN THE CALL SEQUENCE*)
                finished: boolean;      (*SET WHEN DONE CHECKING THE CALL SEQUEN
CE*)

            BEGIN (*INSERTCALL*)
            IF locprocstl^.firstcall = NIL THEN
                locprocstl^.firstcall := workcall
            ELSE
                BEGIN
                thiscall := locprocstl^.firstcall;
                repeated := false;
                finished := false;
                WHILE (NOT finished) AND (NOT repeated) DO
                    IF thiscall^.whom^.procname^.name = workcall^.whom^.procname
^.name THEN
                        repeated := true
                    ELSE
                        IF thiscall^.nextcall = NIL THEN
                            finished := true
                        ELSE
                            thiscall := thiscall^.nextcall;
                IF NOT repeated THEN
                    thiscall^.nextcall := workcall;
                END;
            workcall := NIL;
            END (*INSERTCALL*);


        PROCEDURE parenthese (which: symbol);
            (*HANDLES THE FORMATTING OF PARENTHESES, EXCEPT THOSE IN VARIANT PAR
TS OF RECORDS*)
            VAR
                oldspacesmark : integer;        (*ALTER ZEICHENVORSCHUB BEI FORM
ATIERUNG VON KLAMMERN*)
            BEGIN (*PARENTHESE*)
            IF variant_level = 0 THEN
                BEGIN
                oldspacesmark := spaces;
                IF NOT oldspaces THEN
                    setlastspaces (spaces);
                spaces := lastspaces + bufferptr - buffmark - 2;
                (*SKIP STUFF UNTIL WE SEEM TO BE OUT OF THE EXPRESSION*)
                IF declaring THEN
                    REPEAT
                        insymbol;
                        CASE syty OF
                            colon: declaring := false;
                            semicolon: declaring := true;
                            END;
                    UNTIL syty IN [externsy..rparent,labelsy..typesy,initprocsy
..exitsy,dosy..forwardsy]
                ELSE
                    REPEAT
                        insymbol
                    UNTIL syty IN [externsy..rparent,labelsy..typesy,initprocsy
..exitsy,dosy..forwardsy];
                spaces := oldspacesmark;
                oldspaces := true;
                IF syty = which THEN
                    insymbol
                ELSE
                    error(missgrpar);
                END;
            END (*PARENTHESE*) ;
            (*DOCOMMENT*) (*]INSYMBOL*)


        PROCEDURE docomment (dellength: integer; firstch, secondch: char;
cleaning: boolean);
          var temposp : boolean ;

            PROCEDURE expand (here: integer; firstch, secondch: char);
                VAR
                    i: integer;

                BEGIN (*EXPAND*)
                bufferptr := here + 2;
                bufflen := bufflen + 1;
                FOR i := bufflen + 1 DOWNTO here + 2 DO
                    buffer [i] := buffer [i-1];
                buffer [here] := firstch;
                buffer [here + 1] := secondch;
                END (*EXPAND*);
            PROCEDURE dumpit;
                VAR i: integer;
                BEGIN
                write (tty, '<');
                FOR i := 1 TO bufflen DO
                    write (tty, buffer[i]);
                writeln (tty);
                writeln (tty,'<',' ':bufferptr -1, '@');
                END;

            BEGIN (* DOCOMMENT *)
            oldspacesmark := spaces;
            IF oldspaces THEN
                spaces := lastspaces
            ELSE
                lastspaces := spaces;
            spaces := spaces + bufferptr - 1;
            oldspaces := true;
            IF dellength = 2 THEN
                BEGIN
                IF cleaning THEN BEGIN
                    buffer [bufferptr - 1] := '(';
                    buffer [bufferptr] := '*';
                    END;
                readbuffer ;  flagger[bufferptr] := comcase ; readbuffer ;
                (* skip over comment beginning bracket *)
                if ch = '%' then
                REPEAT  readbuffer;  flagger[bufferptr] := comcase;
                  IF ch <> '*' THEN
                    BEGIN
                    IF ch = 'X' THEN
                      BEGIN readbuffer;  crossing := ch = '+'  END
                    ELSE IF ch = 'F' THEN
                      BEGIN readbuffer; renewing := ch = '+';  END
                    ELSE IF ch = 'R' THEN
                      BEGIN readbuffer; rescase := ch = '+' END
                    ELSE IF ch = 'C' THEN
                      BEGIN  readbuffer ;  comcase := ch = '+' ;  END
                    ELSE IF ch = 'N' THEN
                      BEGIN  readbuffer ;  nestcomments := ch ='+'  END
                    ELSE IF ch='I' THEN
                      BEGIN  readbuffer ;  nonrcase := ch = '+' END
                    (*ELSE IF ch = 'S' THEN
                        BEGIN  readbuffer ;  SAVEREGS := ch <> '-'  END
                        ELSE IF ch = 'F' THEN
                        BEGIN readbuffer ;  SAVEFPRS := ch <> '-' ;
                        END  *) ;
                    readbuffer
                  END
                UNTIL ch <> ',' ;

            (*  REPEAT   *)
                WHILE NOT((ch = secondch) AND (buffer[bufferptr-2] = firstch)
                          OR eob)  DO
                    begin
                    if nestcomments then
                      if (ch = '(') and (buffer[bufferptr] = '*') then
                        begin
                        temposp := oldspaces ;
                        docomment(2, '*', ')', false) ;
                        oldspaces := temposp;
                        readbuffer;
                        end;

                    flagger[bufferptr]:=comcase;
                    readbuffer;

                    end (* while .. *) ;
            (*  UNTIL (ch = secondch) AND (buffer[bufferptr-2] = firstch) OR eob
;           *)
                IF cleaning THEN
                    BEGIN
                    buffer [bufferptr - 2] := '*';
                    buffer [bufferptr - 1] := ')';
                    END;
                END
            ELSE
                BEGIN
                IF cleaning THEN
                    expand (bufferptr - 1,'(','*');
                REPEAT
                    flagger[bufferptr]:=comcase;
                    readbuffer;
                UNTIL (ch = firstch) OR eob;
                IF cleaning THEN
                    expand (bufferptr - 1, '*', ')');
                END;
            spaces := oldspacesmark;
            oldspaces := true;
            END (*DOCOMMENT*);

        BEGIN (*INSYMBOL*)
        syleng := 0;
       {WHILE (ch IN ['_', '(', ' ', '$', '?', '!', '@', '%', '/', '\']) AND NOT
 eob  DO}
        WHILE ((delsy[ch] in [spacech, dquotech, skipch, lparent])
              AND (NOT eob)) DO
     (* WHILE  ( (ch = '_') OR (ch = '(') OR (ch = ' ') OR (ch = '$') OR
                (ch = '?') OR (ch = '!') OR (ch = '@') OR (ch = '%') OR
                (ch = '/') OR (ch = '\') OR (ch = '#') OR (ch = '"') )
                AND (NOT eob)  DO  *)
            BEGIN
            IF (ch = '(') AND (buffer[bufferptr] = '*') THEN
                docomment (2,'*',')', false)
            ELSE
             (* IF (ch = '/') AND (buffer[bufferptr] = '*') THEN
                    docomment (2,'*','/',cleaning)
                ELSE   *)
                    IF ch = '"' then  docomment(1, '"', ' ', cleaning)
                    ELSE  IF ch = '#' THEN buffer[bufferptr-1] := ' '
                    (*ELSE
                    IF ch = '%' THEN
                      IF   (buffer[bufferptr] >= '0') AND
                           (buffer[bufferptr] <= '9') then
                            if anyversion OR (ord(buffer[bufferptr]) - ord ('0')
 = goodversion) then
                                bufferptr := bufferptr + 1
                            else
                                docomment (1,'\','\',cleaning)
                        else
                        docomment (1,'\','\',cleaning)    *)
                    ELSE  IF ch = '#' THEN buffer[bufferptr-1] := ' '
                    ELSE
                        IF (ch = '(') (* OR (ch = '/') *) THEN
                            GOTO 1;
            readbuffer;
            END;
      { CASE ch OF
            'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I',
            'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q',
            'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y',
            'Z':}
     (*  IF (ch >= 'A') AND (ch <= 'Z')  THEN  *)

       CASE delsy[ch] OF

       letterup, letterlo :
              BEGIN
              syleng := 0;
              sy := '          ';
              REPEAT
                  syleng := syleng + 1;
                  IF syleng <= 10 THEN
                      sy [syleng] := ch;
                  readbuffer;
         (*   UNTIL NOT (((ch >= '0') AND (CH <= '9')) OR
                        ((ch >= 'A') AND (ch <= 'Z')) OR (ch = '_'));*)
              UNTIL NOT(delsy[ch] IN [letterup, letterlo, digit,
                                      dollarch, undsch]) ;
              IF NOT resword THEN
                  BEGIN
                  syty := ident ;
                  findname{(curproc)};
                  END
              END ;
      (* ELSE IF  (ch >= '0') AND (ch <= '9') THEN *)
          { '0', '1', '2', '3', '4', '5', '6', '7', '8',
            '9':}

            digit :
              BEGIN
              REPEAT
                  syleng := syleng + 1;
                  readbuffer;
              UNTIL NOT (('0' <= ch) AND (ch <= '9'));
              syty := intconst;
              IF ch = 'B' THEN
                  BEGIN
                  flagger[bufferptr-1]:=true;
                  readbuffer;
                  END
              ELSE
                  BEGIN
                  IF ch = '.' THEN
                      BEGIN
                      REPEAT
                          readbuffer
                      UNTIL NOT (('0' <= ch) AND (ch <= '9'));
                      syty := othersy; syleng := 0; (*REALS CAN'T BE LABELS*)
                      END;
                  IF ch = 'E' THEN
                      BEGIN
                      readbuffer;
                      IF (ch = '+') OR (ch = '-') THEN
                          readbuffer;
                      WHILE ('0' <= ch) AND (ch <= '9') DO
                          readbuffer;
                      syty := othersy; syleng := 0; (*REALS CAN'T BE LABELS*)
                      END;
                  END;
              END  ;
       (* ELSE IF ch = ''''  THEN  *)
          quotech :
               BEGIN
               syty := strgconst;
               REPEAT
                (* flagger[bufferptr]:=strcase; *)
                   (*this ensures that Strings will not be up/low shifted*)
                   flagger[bufferptr]:= delsy[buffer[bufferptr]] = letterup ;
                   readbuffer;
               UNTIL (ch = '''') OR eob OR eoline;
               IF ch <> '''' THEN
                   error(missgquote);
               readbuffer;
               END ;
      (*  ELSE IF ch = '"' THEN
              BEGIN
              REPEAT
                  readbuffer
              UNTIL NOT ((ch >= '0') AND (ch <= '9')) {(digits + ['A'..'F']))};
              syty := intconst;
              END     *)
       (* ELSE IF ch = ' ' THEN *)

       spacech : syty := eobsy ;   (*END OF FILE*)
       (* ELSE   *)  (*OTHERS*)

       othersy, lparent, rparent, lbracket, rbracket,
       semicolon, point, colon, eqlsy :
                 BEGIN
        1:
                 syty := delsy [ch];
                 readbuffer;
                 IF (syty = colon) AND (ch = '=') THEN
                     BEGIN
                     workcall := NIL;
                     syty := othersy;
                     readbuffer;
                     END
                 ELSE
                     IF syty IN [lparent, lbracket] THEN
                         IF syty = lparent THEN
                             parenthese (rparent)
                         ELSE
                             parenthese (rbracket);
                 END ;
              END;  (* CASE delsy[ch] OF *)
        IF workcall <> NIL THEN
            insertcall;
        END (*INSYMBOL*) ;

        (*"PARSING" PROCEDURES:*) (*RECDEF[*) (*CASEDEF*) (*PARENTHESE*) (*]*)

    PROCEDURE recdef;
        VAR
            oldspacesmark  : integer;         (*ALTER ZEICHENVORSCHUB BEI FORMAT
IERUNG VON RECORDS*)


        PROCEDURE casedef;
            VAR
                oldspacesmark  : integer;       (*ALTER ZEICHENVORSCHUB BEI FORM
ATIERUNG VON VARIANT PARTS*)


            PROCEDURE parenthese;
                (*HANDLES THE FORMATTING OF PARENTHESES INSIDE VARIANT PARTS*)
                VAR
                    oldspacesmark : integer;      (*SAVED VALUE OF 'SPACES'*)
                BEGIN (*PARENTHESE*)
                oldspacesmark := spaces;
                IF NOT oldspaces THEN
                    setlastspaces (spaces);
                spaces := spaces + bufferptr - 2;
                declaring := true;
                insymbol;
                REPEAT
                IF  NOT (syty IN [casesy, recordsy, semicolon, lparent,
                             eqlsy, colon])  THEN  insymbol
                ELSE
                    CASE syty OF
                        casesy  : casedef;
                        recordsy : recdef;
                        semicolon, lparent:
                                         BEGIN
                                         declaring := true;
                                         insymbol;
                                         END;
                        eqlsy, colon:
                                   BEGIN
                                   declaring := false;
                                   insymbol;
                                   END;
                       {OTHERS:  insymbol}
                        END;
                    (*UNTIL WE APPARENTLY LEAVE THE DECLARATION*)
                UNTIL syty IN [strgconst..whilesy,rparent,labelsy..exitsy,dosy..
beginsy,
                               loopsy..forwardsy];
                spaces := oldspacesmark;
                oldspaces := true;
                IF syty = rparent THEN
                    insymbol
                ELSE
                    error(missgrpar);
                END (*PARENTHESE*) ;

            BEGIN (*CASEDEF*)
            variant_level := variant_level+1;
            oldspacesmark := spaces;
            IF NOT oldspaces THEN
                setlastspaces (spaces);
            spaces := bufferptr - buffmark + lastspaces - syleng + 3;
            declaring := true;
            insymbol;
            declaring := false;
            REPEAT
                IF syty = lparent THEN
                    parenthese
                ELSE
                    insymbol
            UNTIL syty IN [untilsy..exitsy,labelsy..endsy,rparent,dosy..beginsy]
;
            spaces := oldspacesmark;
            variant_level := variant_level-1;
            END (*CASEDEF*) ;

        BEGIN (*RECDEF*)
        oldspacesmark := spaces;
        setlastspaces(spaces);
        spaces := bufferptr - buffmark + spaces - syleng - 2 + feed;
        declaring := true;
        insymbol;
        REPEAT
            IF  NOT(syty IN [casesy, recordsy, semicolon, lparent,
                         eqlsy, colon])  THEN  insymbol
            ELSE
            CASE syty OF
                casesy   : casedef;
                recordsy : recdef;
                semicolon, lparent:
                                 BEGIN
                                 declaring := true;
                                 insymbol;
                                 END;
                eqlsy, colon:
                           BEGIN
                           declaring := false;
                           insymbol;
                           END;
              { OTHERS   : insymbol }
                END;
        UNTIL syty IN [untilsy..exitsy,labelsy..endsy,dosy..beginsy];
        oldspaces := true;
        lastspaces := spaces - feed;
        spaces := oldspacesmark;
        IF syty = endsy THEN
            begin
            declaring := true;
            insymbol;
            end
        ELSE
            error(missgenduntil);
        END (*RECDEF*) ;
        (*STATEMENT[*) (*AND ITS PARTS*) (*]*)


    PROCEDURE statement;
        VAR
            oldspacesmark,           (*SPACES AT ENTRY OF THIS PROCEDURE*)
            curblocknr : integer;     (*CURRENT BLOCKNUMBER*)


        PROCEDURE endedstatseq(endsym: symbol;  markch: char);
            BEGIN
            statement;
            WHILE syty = semicolon DO
                BEGIN
                insymbol;
                statement;
                END;
            WHILE NOT (syty IN [endsym,eobsy,proceduresy,functionsy]) DO
                BEGIN
                error(enderrinblkstr);
                IF NOT (syty IN begsym) THEN
                    insymbol;
                statement;
                WHILE syty = semicolon DO
                    BEGIN
                    insymbol;
                    statement;
                    END;
                END;
            IF forcing THEN
                writeline(bufferptr-syleng);
            emarktext := markch;
            emarknr := curblocknr;
            IF (endsym = endsy) THEN
                IF indentbegin = 0 THEN
                    setlastspaces(spaces-begexd)
                ELSE
                    setlastspaces(spaces-indentbegin)
            ELSE
                setlastspaces(spaces - feed);
            IF syty <> endsym THEN
                error(missgenduntil);
            END (*ENDEDSTATSEQ*);


        PROCEDURE compstat;
            BEGIN (*COMPSTAT*)
            IF indentbegin = 0 THEN
                IF NOT oldspaces THEN
                    setlastspaces (spaces-begexd)
                ELSE
                    IF NOT oldspaces THEN
                        setlastspaces (spaces - indentbegin);
            bmarktext := 'B';
            insymbol;
            IF forcing THEN
                writeline(bufferptr-syleng);
            endedstatseq(endsy, 'E');
            IF syty = endsy THEN
                BEGIN
                insymbol ;
                writeline(bufferptr-syleng);
                END;
            END (*COMPSTAT*) ;


        PROCEDURE casestat;
//{P}            LABEL 333 ;
            VAR
                oldspacesmark : integer;        (*SAVED VALUE OF 'SPACES'*)

            BEGIN (*CASESTAT*)
            bmarktext := 'C';
            IF NOT oldspaces THEN
                setlastspaces (spaces-feed);
            insymbol;
            statement;
            IF syty = ofsy THEN
                writeline (bufferptr)
            ELSE
                error (missgof);
          { LOOP } REPEAT
                REPEAT
                    REPEAT
                        insymbol
                    UNTIL syty IN [colon,functionsy..eobsy];
                    IF syty = colon THEN
                        BEGIN
                        oldspacesmark := spaces;
                        lastspaces := spaces;
                        spaces := bufferptr - buffmark + spaces - 4;
                        oldspaces := true;
                        thendo := true;
                        insymbol;
                        statement;
                        spaces := oldspacesmark;
                        END;
                UNTIL syty IN endsym;
//{P}          { EXIT}IF syty IN [endsy,eobsy,proceduresy,functionsy] THEN GOTO 333;
{P}               IF syty IN [endsy,eobsy,proceduresy,functionsy] THEN break;
                error (enderrinblkstr);
                UNTIL FALSE {END};
//{P}          333:
            writeline(bufferptr-syleng);
            emarktext := 'E';
            emarknr := curblocknr;
            IF syty = endsy THEN
                BEGIN
                insymbol ;
                writeline(bufferptr-syleng);
                END
            ELSE
                error (missgenduntil);
            END (*CASESTAT*) ;


        PROCEDURE loopstat;
            BEGIN (*LOOPSTAT*)
            bmarktext := 'L';
            IF NOT oldspaces THEN
                setlastspaces (spaces - feed);
            insymbol;
            writeline(bufferptr-syleng);
            statement;
            WHILE syty = semicolon DO
                BEGIN
                insymbol;
                statement;
                END;
            IF syty = exitsy THEN
                BEGIN
                writeline(bufferptr-syleng);
                oldspaces := true;
                lastspaces := spaces-feed;
                emarktext := 'X';
                emarknr := curblocknr;
                insymbol; insymbol;
                END
            ELSE
                error(missgexit);
            endedstatseq(endsy, 'E');
            IF syty = endsy THEN
                BEGIN
                insymbol ;
                writeline(bufferptr-syleng);
                END;
            END (*LOOPSTAT*) ;


        PROCEDURE ifstat;
            VAR
                oldspacesmark: integer;

            BEGIN
            oldspacesmark := spaces;
            bmarktext := 'I';
            IF NOT oldspaces THEN
                setlastspaces (spaces - feed);
            (*DON'T INDENT THE 'IF'*)
            (*MAKE 'THEN' AND 'ELSE' LINE UP WITH 'IF' UNLESS ON SAME LINE*)
            spaces := lastspaces + bufferptr - buffmark + feed - 4;
            insymbol;
            statement; (*WILL EAT THE EXPRESSION AND STOP ON A KEYWORD*)
            IF syty = thensy THEN
                BEGIN
                IF NOT oldspaces THEN
                    setlastspaces (spaces-feed);
                emarktext := 'T';
                emarknr := curblocknr;
                IF forcing THEN
                    writeline(bufferptr)
                ELSE
                    thendo := true;
                (*SUPPRESS FURTHER INDENTATION FROM A 'DO'*)
                insymbol;
                statement;
                END
            ELSE
                error (missgthen);
            IF syty = elsesy THEN
                BEGIN
                writeline(bufferptr-syleng);
                emarktext := 'S';
                emarknr := curblocknr;
                IF NOT oldspaces THEN
                    setlastspaces (spaces-feed);
                IF forcing THEN
                    writeline(bufferptr)
                ELSE
                    thendo := true;
                insymbol;
                statement;
                END;
            oldspaces := true; (*PRESERVE INDENTATION OF STATEMENT*)
            writeline(bufferptr-syleng);
            spaces := oldspacesmark;
            END (*IFSTAT*) ;


        PROCEDURE labelstat;
            BEGIN (*LABELSTAT*)
            lastspaces := level * feed;
            oldspaces := true;
            insymbol;
            writeline(bufferptr-syleng);
            END (*LABELSTAT*) ;


        PROCEDURE repeatstat;
            BEGIN
            bmarktext := 'R';
            IF NOT oldspaces THEN
                setlastspaces (spaces - feed);
            insymbol;
            endedstatseq(untilsy, 'U');
            IF syty = untilsy THEN
                BEGIN
                insymbol;
                statement;
                writeline(bufferptr-syleng);
                END;
            END (*REPEATSTAT*) ;

        BEGIN (*STATEMENT*)
        oldspacesmark := spaces; (*SAVE THE INCOMING VALUE OF SPACES TO BE ABLE
TO RESTORE  IT*)
        IF syty = intconst THEN
            BEGIN
            insymbol;
            IF syty = colon THEN
                labelstat;
            END;
        IF syty IN begsym THEN
            BEGIN
            blocknr := blocknr + 1;
            curblocknr := blocknr;
            bmarknr := curblocknr;
            IF NOT thendo THEN
                BEGIN
                writeline(bufferptr-syleng);
                IF (syty <> beginsy) THEN
                    spaces := spaces + feed
                ELSE
                    spaces:=spaces + indentbegin;
                END;
            CASE syty OF
                beginsy : compstat;
                loopsy  : loopstat;
                casesy  : casestat;
                ifsy    : ifstat;
                repeatsy: repeatstat
                END;
            END
        ELSE
            BEGIN
            IF forcing THEN
                IF syty IN [forsy,whilesy] THEN
                    writeline(bufferptr-syleng);
            IF syty = gotosy THEN
                gotoinline:=true;
            WHILE NOT (syty IN [semicolon,functionsy..recordsy]) DO
                insymbol;
            IF syty = dosy THEN
                BEGIN
                IF NOT thendo THEN
                    BEGIN
                    setlastspaces(spaces);
                    spaces := spaces + feed;
                    IF NOT forcing THEN
                        thendo := true;
                    END;
                insymbol;
                statement;
                writeline(bufferptr-syleng);
                END;
            END;
        spaces := oldspacesmark;
        END (*STATEMENT*) ;

        (*]BLOCK*)

    BEGIN (*BLOCK*)
    REPEAT
        insymbol
    UNTIL syty IN relevantsym;
    level := level + 1;
    curproc := listptr;
    spaces := level * feed;
    (*HANDLE NESTING LIST*)
    locprocstl := procstrucf;
  IF procstrucdata.exists then      (*** BUG 2 ***)
    WITH procstrucdata, item, procname^ DO
      (*IF exists THEN*)
            BEGIN
            IF procdata <> NIL THEN
                BEGIN
                IF externflag = 'F' THEN
                    procdata := NIL
                ELSE
                    IF externflag = ' ' THEN
                        externflag := 'D';
                locprocstl := procdata;
                END;
            IF procdata = NIL THEN
                BEGIN
                IF (syty IN [forwardsy,externsy]) THEN
                    IF syty = externsy THEN
                        externflag := 'E'
                    ELSE
                        externflag := 'F';
                new(procstrucl^.nextproc);
                procstrucl := procstrucl^.nextproc;
                procdata := procstrucl;
                procstrucl^ := item;
                locprocstl := procstrucl;
                END;
            procstrucdata.exists := false
            END;
    REPEAT
        fwddecl := false;
        WHILE syty IN decsym DO
            BEGIN
            writeline(bufferptr-syleng);
            setlastspaces(spaces-feed);
            IF syty = programsy THEN
                BEGIN
                insymbol;
                prog_name := sy;
                procstrucf^.procname := listptr;
                listptr^.procdata := procstrucf;
                listptr^.profunflag := 'M';
                writeln(tty);
                write(tty,'1', version:8,new_name:7,' [',prog_name,']  1..');
                IF pagecnt > 1 THEN
                    FOR i := 2 TO pagecnt DO
                        write (tty, i:3,'..');
                 (* break(tty); *)
                declaring := false;
                END
            ELSE
                BEGIN
                declaring := true;
                IF forcing THEN
                    writeline(bufferptr);
                END;
            REPEAT
                insymbol;
                CASE syty OF
                    semicolon, lparent : declaring := true;
                    eqlsy, colon : declaring := false;
                    recordsy: recdef;
                    END;
            UNTIL syty IN relevantsym;
            END;
        WHILE syty IN prosym DO
            BEGIN
            writeline(bufferptr-syleng);
            setlastspaces(spaces-feed);
            IF syty <> initprocsy THEN
                BEGIN
                itisaproc := syty = proceduresy;
                declaring := true;
                insymbol;
                IF itisaproc THEN
                    listptr^.profunflag := 'P'
                ELSE
                    listptr^.profunflag := 'F';
                WITH procstrucdata, item DO
                    BEGIN
                    exists := true;
                    procname := listptr;
                    nextproc := NIL;
                    linenr := linecnt+1;
                    pagenr := pagecnt;
                    proclevel := level;
                    printed := false;
                    firstcall := NIL;
                    END;
                END;
            block;
            IF syty = semicolon THEN
                insymbol;
            END;
        (*FORWARD AND EXTERNAL DECLARATIONS MAY COME BEFORE 'VAR', ETC.*)
    UNTIL NOT fwddecl;
    IF forcing THEN
        writeline(bufferptr-syleng);
    level := level - 1;
    spaces := level * feed;
    IF NOT (syty IN [beginsy,forwardsy,externsy,eobsy,langsy]) THEN
        BEGIN
        IF (level = 0) AND (syty = point) THEN
            nobody := true
        ELSE
            error (begerrinblkstr);
        WHILE NOT (syty IN [beginsy,forwardsy,externsy,eobsy,langsy,point]) DO
            insymbol
        END;
    IF syty = beginsy THEN
        BEGIN
        declaring := false;
        locprocstl^.begline := linecnt + 1;
        locprocstl^.begpage := pagecnt;
        statement;
        locprocstl^.endline := linecnt + 1;
        locprocstl^.endpage := pagecnt;
        END
    ELSE
        IF NOT nobody THEN
            BEGIN
            fwddecl := true;
            insymbol;
            IF syty = langsy THEN
                insymbol
            END;
    IF programpresent AND (level = 0) THEN
        BEGIN
        IF nobody THEN
            BEGIN
            error (missgmain);
            errcount := errcount - 1;
            END;
        IF syty <> point THEN
            BEGIN
            error(missgpoint);
            REPEAT (*SKIP TEXT UNTIL END OF FILE OR END OF PROGRAM HIT*)
                REPEAT
                insymbol UNTIL (syty = endsy) OR (syty = eobsy);
                IF syty = endsy THEN
                    insymbol;
            UNTIL (syty = point) OR (syty = eobsy);
            END;
        writeline(bufflen+2);
        writeln(tty);
        writeln(tty);
        writeln (tty,errcount:4,' ERROR(S) DETECTED');    (* break(tty); *)
        END;
    END (*BLOCK*) ;

PROCEDURE print_xref_list;
    VAR
        pred : listptrty;
        indexch : char;         (*LAUFVARIABLE FUER DAS FELD 'FIRSTNAME' ZUM AUS
DRUCKEN*)
        listpgnr : boolean;     (*TRUE IF THE SOURCE CONTAINS A PAGE MARK*)
        itemlen: integer;        (*LENGTH OF A PRINTED LINENUMBER, 9 OR 12*)
        thiscall : calledty;
        oldcrossing: boolean;


    PROCEDURE checkpage(heading: boolean);
        BEGIN
        IF reallincnt = maxline THEN
            BEGIN
            IF heading THEN
                header (listptr^.name)
            ELSE
                header (blanks);
            END;
        reallincnt:=reallincnt+1;
        END(*CHECKPAGE*);

    PROCEDURE writeprocname (procstrucl: procstructy; depth: integer; mark: char
; numbering: boolean);
        BEGIN (*WRITEPROCNAME*)
        writeln(crosslist);
        checkpage(false);
        WITH procstrucl^, procname^ DO
            BEGIN
            IF numbering THEN
                write (crosslist, linecnt * increment:6, '  ')
            ELSE write(crosslist, linenr*increment:6, '  ') ;
         (* IF depth > 2 THEN
                write (crosslist, '. ',dots:depth-2)
            ELSE
                write (crosslist, '.':depth);  *)
            write  (crosslist, ' ': depth*3, name: 10, ' ':25-depth*3,
                    ' (', profunflag, ')', mark:2, externflag:2
                    {,chr(ht), linenr * increment : 8});
            IF numbering then  write(crosslist, linenr*increment:10) ;
            IF listpgnr OR (pagenr > 1) THEN
                write(crosslist, ' / ',pagenr : 1);
            IF (mark = ' ') AND NOT ((externflag = 'E') OR
                                     (externflag = 'F')) THEN
                BEGIN
                write (crosslist, begline * increment: 10);
                IF listpgnr THEN
                    write (crosslist, ' / ', begpage: 1);
                write (crosslist, endline * increment: 7);
                IF listpgnr THEN
                    write (crosslist, ' / ', endpage:1);
                END;
            END;
        END (*WRITEPROCNAME*);

    PROCEDURE writelinenr (spaces : integer);

        VAR
            link : lineptrty; (*ZEIGER ZUM DURCHHANGELN DURCH DIE VERKETTUNG DER
 ZEILENNUMMERN*)
            maxcnt,             (*MAXIMUM ALLOWABLE VALUE OF COUNT*)
            count : integer;  (*ZAEHLT DIE GEDRUCKTEN ZEILENNUMMERN PRO ZEILE*)
        BEGIN (*WRITELINENR*)
        count := 0;
        maxcnt := (maxch+20 - spaces) DIV itemlen; (*ITEMS ARE ITEMLEN CHARS EAC
H*)
        link := listptr^.first;
        REPEAT
            IF count = maxcnt THEN
                BEGIN
                writeln(crosslist);
                checkpage(true);
                write (crosslist, ' ' : spaces);
                count := 0;
                END;
            count := count + 1;
            write (crosslist, link^.linenr * increment : 6);
            IF listpgnr THEN
                write(crosslist, '/',link^.pagenr : 2);
            write (crosslist,link^.declflag,'  ');
            link := link^.contlink;
        UNTIL link = NIL;
        END (*WRITELINENR*) ;

    PROCEDURE dumpcall (thisproc: procstructy; depth: integer);
        VAR
            thiscall: calledty;

        BEGIN (*DUMPCALL*)
        linecnt := linecnt + 1;
        WITH thisproc^ DO
            IF printed THEN
                writeprocname (thisproc, depth,'*', true)
            ELSE
                BEGIN
                writeprocname (thisproc, depth, ' ', true);
                printed := true;
                linenr := linecnt;
                pagenr := pagecnt;
                thiscall := firstcall;
                WHILE thiscall <> NIL DO
                    BEGIN
                    dumpcall (thiscall^.whom, depth + {4}1);
                    thiscall := thiscall^.nextcall;
                    END;
                END;
        END (*DUMPCALL*);
        (*]PRINT_XREF_LIST*)

    BEGIN (*PRINT_XREF_LIST*)
    oldcrossing := crossing;
    crossing := true;
    listpgnr := pagecnt > 1;
    IF listpgnr THEN
        itemlen := 12
    ELSE
        itemlen := 9;
    WITH firstname ['M']^ DO  (*DELETE 'MAIN'*)
        IF rlink = NIL THEN
            firstname ['M'] := llink
        ELSE
            BEGIN
            listptr := rlink;
            WHILE listptr^.llink <> NIL DO
                listptr := listptr^.llink;
            listptr^.llink := llink;
            firstname ['M'] := rlink;
            END;
    indexch := 'a';
    WHILE (indexch < 'Z') AND (firstname [indexch] = NIL) DO
        indexch := succ (indexch);
    IF firstname [indexch] <> NIL THEN
        BEGIN
        IF refing THEN
            BEGIN
            pagecnt := pagecnt + 1;
            pagecnt2 := 0;
            header (blanks);
            writeln (crosslist,
                     ' CROSS REFERENCE LISTING OF IDENTIFIERS   ':46) ;
            writeln (crosslist,
                     ' **************************************   ':46) ;
            reallincnt:= reallincnt + 3;
            FOR indexch := indexch TO 'Z' DO
                WHILE firstname [indexch] <> NIL DO
                    BEGIN
                    listptr := firstname [indexch];
                    WHILE listptr^.llink <> NIL DO
                        BEGIN
                        pred := listptr;
                        listptr := listptr^.llink;
                        END;
                    IF listptr = firstname [indexch] THEN
                        firstname [indexch] := listptr^.rlink
                    ELSE
                        pred^.llink := listptr^.rlink;
                    writeln(crosslist);
                    checkpage(true);
                    write (crosslist, listptr^.profunflag, listptr^.name : 11);
                    writelinenr (12);
                    END;
            writeln(crosslist) ;
            END;

        IF procstrucl <> procstrucf THEN
            BEGIN
            IF decnesting THEN
                BEGIN
                pagecnt := pagecnt + 1;
                pagecnt2 := 0;
                writeln (crosslist);
                header (blanks);
                writeln (crosslist,
                         ' NESTING OF PROCEDURE-FUNCTION DECLARATION':46) ;
                writeln (crosslist,
                         ' *****************************************':46) ;
                writeln (crosslist,
                         ' HEADERLINE, NAME, P/F, BEGINLINE, ENDLINE':46) ;
                reallincnt:= reallincnt + 4;
                procstrucl := procstrucf;
                REPEAT
                    writeprocname (procstrucl, procstrucl^.proclevel{* 2},' ',
false);
                    procstrucl := procstrucl^.nextproc;
                UNTIL procstrucl = NIL;
                END;
            IF callnesting THEN
                BEGIN
                pagecnt := pagecnt + 1;
                pagecnt2 := 0;
                writeln (crosslist);
                header (blanks);
                writeln (crosslist,
                         ' NESTING OF PROCEDURE-FUNCTION CALLS      ':46);
                writeln (crosslist,
                         ' ***********************************      ':46);
                writeln (crosslist,
                         ' NAME, P/F, HEADLINE, BEGINLINE, ENDLINE  ':46) ;
                reallincnt := reallincnt + 4;
                linecnt := 0;
                procstrucl := procstrucf;
                WHILE procstrucl <> NIL DO
                    BEGIN
                    IF NOT procstrucl^.printed THEN
                        dumpcall (procstrucl, 0);
                    procstrucl := procstrucl^.nextproc;
                    END;
                END;
            writeln(crosslist) ;
            END;
        END;
    crossing := oldcrossing;
    END (*PRINT_XREF_LIST*) ;

VAR
{P}    SFN,
       OFN,
       CRL:String;
       IR,
       fm: Integer; // file mode


    (*MAIN PROGRAM*)

BEGIN
{P} Write('Source? ');
    Readln(SFN);
    ASSIGN(OldSource,SFN);
    FM:= FileMode;
    FileMode := 0;


    {$I-} RESET(OLDSOURCE) ; {$I+}
    IR := IOResult;
    if IR<>0 then
    begin
       Writeln('?Error ',IR,' teading file',SFN);
       write('Press Enter to quit: ');
       readln;
       Halt(1);
    end;

  {P} Write('Output? ');
      Readln(OFN);
      ASSIGN(NewSource,OFN);
      FileMode := FM; // restore

    {$I-} REWRITE(NEWSOURCE); {$I+}
      IR := IOResult;
      if IR<>0 then
      begin
          Writeln('?Error ',IR,' writing file',OFN);
          write('Press Enter to quit: ');
          readln;
          Halt(2);
      end;


      FM:= FileMode;
      FileMode := 0;



      wRITE('Listing output? ');
      Readln(CRL);
      ASSIGN(CROSSLIST,CRL);
      ASSIGN(TTY,'TTY.TXT');


      {$I-} REWRITE(CROSSLIST) ; {$I+}
      IR := IOResult;
      if IR<>0 then
      begin
          Writeln('?Error ',IR,' writing file',CRL);
          write('Press Enter to quit: ');
          readln;
          Halt(3);
      end;

      {$I-}  REWRITE(TTY) ;  {$I+}
      IR := IOResult;
      if IR<>0 then
      begin
          Writeln('?Error ',IR,' writing file TTY.TXT');
          write('Press Enter to quit: ');
          readln;
          Halt(3);
      end;

      checkoptions;
  {getstatus(oldsource,new_name,new_prot,new_ppn,new_dev);}

(*FIND MAX POSSIBLE LINE NUMBER WITH THIS INCREMENT*)
  increment := 1 ;
maxinc := (99999 DIV increment);
IF maxinc > 4000 THEN
    maxinc := 4000;

  {LOOP} REPEAT
    init;
    block;
//{P}  {EXIT} IF NOT programpresent OR (syty = eobsy) THEN GOTO 444;
{P}        IF NOT programpresent OR (syty = eobsy) THEN break;
    IF refing OR decnesting OR callnesting THEN
        print_xref_list;
    {dispose(heapmark);}    (*RELEASE THE ENTIRE HEAP*)  
//{P}    rtime[0]:=clock(1)-rtime[0];

    rtime[1]:=rtime[0] DIV 60000;
    rtime[2]:=(rtime[0] MOD 60000) DIV 1000;
    rtime[3]:=rtime[0] MOD 1000;
    writeln(tty);
    writeln(tty,' RUNTIME:',rtime[1]:3,':',rtime[2]:2,'.',rtime[3]:3);
     (* break(tty); *)
    UNTIL FALSE {END};
//{P}    444:
     (* writeln(tty, bel);   break(tty); *)

    {getnextcall (link_name, link_device);
     IF link_name <> '         ' THEN
       call (link_name, link_device); }
END (*PCROSS*).
