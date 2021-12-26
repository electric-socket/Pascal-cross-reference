PROGRAM pcross(input, output) ;
(*$N+       ***********
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



(*$T-,R100   *)
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




"**********************************************************************
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
 **********************************************************************"




##  LABEL  444;

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

    errkinds = (begerrinblkstr,enderrinblkstr,missgenduntil,missgthen,missgof,
missgexit,
                missgrpar,missgquote,linetoolong,missgmain,missgpoint);
    lineptrty = @line;
    listptrty = @list;
    procstructy = @procstruc;
    calledty = @called;

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
###           letterup, letterlo, digit,quotech, dquotech, spacech,
###           dollarch, undsch, skipch ) ;

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

##  tabs: ARRAY [1..17] OF (*ascii*) char;   (*A STRING OF TABS FOR FORMATTING*)

    linenb : PACKED ARRAY [1..5] OF char; (*SOS-LINE NUMBER*)
## "date_text,time_text: alfa;"           (*HEADING DATE AND TIME*)
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
    delsy : ARRAY [CHAR"' '..'_'"] OF symbol; (*TYPE ARRAY FOR DELIMITER CHARS*)
#   resnum: ARRAY[char] OF integer;       (*INDEX OF THE FIRST KEYWORD BEGINNING
 WITH THE INDEXED LETTER*)
    reslist : ARRAY [1..46] OF alfa;      (*LIST OF THE RESERVED WORDS*)
    ressy   : ARRAY [1..46] OF symbol;    (*TYPE ARRAY OF THE RESERVED WORDS*)
## "alphanum,                             (*CHARACTERS FROM 0..9 AND A..Z*)
    digits,                               (*CHARACTERS FROM 0..9*)
##  letters : SET OF char; "              (*CHARACTERS FROM A..Z*)
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
##  TTY,
    OLDSOURCE,NEWSOURCE,CROSSLIST: text;  (*FILES PROCESSED BY THIS PROGRAM*)

    (*INITPROCEDURES*)

# PROCEDURE init0; "INITPROCEDURE;"
    BEGIN (*CONSTANTS*)
    eob := false;
    maxch:=114;
##  increment:= "100" 1;
    feed:=4;
    indentbegin:=0;
##  nestcomments := true ;
##  cleaning := true ;
    begexd:=0;
    rescase:=true;
    nonrcase:=false;
    comcase:=false;
    strcase:=true;
    renewing:=true;
    crossing:=true;
##  refing:= true (*false*);
##  decnesting:= true (*false*);
##  callnesting:=true  (*false*);
##  doting:=false (*true*);
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


#  procedure init1; "INITPROCEDURE;"
    BEGIN (*RESERVED WORDS*)
    resnum['A'] :=  1;    resnum['B'] :=  3;    resnum['C'] :=  4;
    resnum['D'] :=  6;    resnum['E'] :=  9;    resnum['F'] := 13;
    resnum['G'] := 18;    resnum['H'] := 19;    resnum['I'] := 19;
##  resnum[succ('I')] := 22;
    resnum['J'] := 22;    resnum['K'] := 22;    resnum['L'] := 22;
    resnum['M'] := 24;    resnum['N'] := 25;    resnum['O'] := 27;
    resnum['P'] := 30;    resnum['Q'] := 33;    resnum['R'] := 33;
##  resnum[succ('R')] := 35 ;
    resnum['S'] := 35;    resnum['T'] := 36;    resnum['U'] := 39;
    resnum['V'] := 40;    resnum['W'] := 41;    resnum['X'] := 43;
    resnum['Y'] := 43;    resnum['Z'] := 43;    resnum[succ('Z')] := 43;

    reslist[ 1] :='AND       '; ressy [ 1] := othersy;
    reslist[ 2] :='ARRAY     '; ressy [ 2] := othersy;
    reslist[ 3] :='BEGIN     '; ressy [ 3] := beginsy;
    reslist[ 4] :='CASE      '; ressy [ 4] := casesy;
    reslist[ 5] :='CONST     '; ressy [ 5] := constsy;
    reslist[ 6] :='DO        '; ressy [ 6] := dosy;
    reslist[ 7] :='DIV       '; ressy [ 7] := othersy;
    reslist[ 8] :='DOWNTO    '; ressy [ 8] := othersy;
    reslist[ 9] :='END       '; ressy [ 9] := endsy;
    reslist[10] :='ELSE      '; ressy [10] := elsesy;

##  reslist[11] :='EXIT____  '; ressy [11] := exitsy;
    reslist[12] :='EXTERN    '; ressy [12] := externsy;
    reslist[13] :='FOR       '; ressy [13] := forsy;
    reslist[14] :='FILE      '; ressy [14] := othersy;
    reslist[15] :='FORWARD   '; ressy [15] := forwardsy;
    reslist[16] :='FUNCTION  '; ressy [16] := functionsy;
    reslist[17] :='FORTRAN   '; ressy [17] := langsy;
    reslist[18] :='GOTO      '; ressy [18] := gotosy;
    reslist[19] :='IF        '; ressy [19] := ifsy;
    reslist[20] :='IN        '; ressy [20] := othersy;

    reslist[21] :='INITPROCED'; ressy [21] := initprocsy;
    reslist[22] :='LOOP____  '; ressy [22] := loopsy;
    reslist[23] :='LABEL     '; ressy [23] := labelsy;
    reslist[24] :='MOD       '; ressy [24] := othersy;
    reslist[25] :='NOT       '; ressy [25] := othersy;
    reslist[26] :='NIL       '; ressy [26] := othersy;
    reslist[27] :='OR        '; ressy [27] := othersy;
    reslist[28] :='OF        '; ressy [28] := ofsy;
    reslist[29] :='OTHERS    '; ressy [29] := otherssy;
    reslist[30] :='PACKED    '; ressy [30] := othersy;

    reslist[31] :='PROCEDURE '; ressy [31] := proceduresy;
    reslist[32] :='PROGRAM   '; ressy [32] := programsy;
    reslist[33] :='RECORD    '; ressy [33] := recordsy;
    reslist[34] :='REPEAT    '; ressy [34] := repeatsy;
    reslist[35] :='SET       '; ressy [35] := othersy;
    reslist[36] :='THEN      '; ressy [36] := thensy;
    reslist[37] :='TO        '; ressy [37] := othersy;
    reslist[38] :='TYPE      '; ressy [38] := typesy;
    reslist[39] :='UNTIL     '; ressy [39] := untilsy;
    reslist[40] :='VAR       '; ressy [40] := varsy;

    reslist[41] :='WHILE     '; ressy [41] := whilesy;
    reslist[42] :='WITH      '; ressy [42] := othersy;
    END;


# procedure init2; "INITPROCEDURE;"
    BEGIN (*SETS*)
## "digits := ['0'..'9'];
    letters := ['A'..'Z'];
##  alphanum := ['0'..'9','A'..'Z'] (*LETTERS OR DIGITS*); "
    decsym := [labelsy,constsy,typesy,varsy,programsy];
    prosym := [functionsy..initprocsy];
    endsym := [functionsy..eobsy];      (*PROSYM OR ENDSYMBOLS*)
    begsym := [beginsy..ifsy];
    relevantsym := [labelsy..initprocsy (*DECSYM OR PROSYM*),beginsy,forwardsy,
externsy,eobsy];
    END (*SETS*);


PROCEDURE init;
    BEGIN
#   init0 ;
#   init1 ;
#   init2 ;
#   rtime[0]:=clock(1);
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
#  "date(date_text);  time(time_text);"
    FOR ch := chr(0) to chr(255) DO
        firstname [ch] := NIL;
    FOR ch := ' ' TO 'Z' DO
        delsy [ch] := othersy;
###   FOR ch := 'A' TO 'I' DO    delsy[ch] := letterup ;
###   FOR ch := 'J' TO 'R' DO    delsy[ch] := letterup ;
###   FOR ch := 'S' TO 'Z' DO    delsy[ch] := letterup ;
###   FOR ch := 'a' TO 'i' DO    delsy[ch] := letterlo ;
###   FOR ch := 'j' TO 'r' DO    delsy[ch] := letterlo ;
###   FOR ch := 's' TO 'z' DO    delsy[ch] := letterlo ;
###   FOR ch := '0' TO '9' DO    delsy[ch] := digit ;
###   delsy['"']  := dquotech;
###   delsy['#']  := skipch ;
###   delsy['$']  := dollarch ;
###   delsy[''''] := quotech ;
###   delsy['_']  := undsch ;
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
    WITH firstname ['M']@ DO
        BEGIN
        name := 'MAIN.     ';
        llink := NIL;
        rlink := NIL;
        profunflag := 'M';
        new (first);
        last := first;
        WITH last@ DO
            BEGIN
            linenr := 1;
            pagenr:=1;
            contlink := NIL;
            END;
        END;
    new (procstrucf);
    WITH procstrucf@ DO
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
##      tabs [i] := "chr (ht)" ' ';
#   linenb := '-----' ;
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

PROCEDURE checkoptions;
    VAR
        try: integer;

    PROCEDURE setswitch(opt:alfa;VAR switch:boolean);
        VAR
            i: integer;
        BEGIN (*SETSWITCH*)
##(*    getoption(opt,i);   *)  i := ord('l') ;
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
##       (* break(tty); *)
        END;

    BEGIN (*CHECKOPTIONS*)
    ###########(*##############
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
######################*)##############

##  renewing:= true "NOT option('NONEW     ')" ;

##  crossing:= true "NOT option('NOCROSS   ')" ;
    IF crossing THEN
        BEGIN
##(*    getoption('CROSS     ',try);     *) try  := 0 ;
        IF try = 0 THEN
            try:=15;
        callnesting:=try > 7;
        decnesting:=(try MOD 8) > 3;
        refing:= (try MOD 4) > 1;
        crossing:=(try MOD 2) = 1;
        END;

##(*if option('version   ') then    *)
        begin
##(*    getoption('version   ',goodversion);  *)  goodversion := 15 ;
        if goodversion > 9 then
            begin
            goodversion := -1;
            anyversion := true;
            end;
        end;

##(*IF option('WIDTH     ') THEN  *)
        BEGIN
##(*    getoption('WIDTH     ',maxch);  *)  maxch := 90 ;
        IF maxch < 40 THEN
            maxch:=40;
        maxch:=maxch-18;
        END;

#################(*##################
    IF option('INDENT    ') THEN
        BEGIN
##(*    getoption('INDENT    ',feed);                 *)
        IF feed < 0 THEN
            feed:=4;
        END;

    IF option('INCREMENT ') THEN
        BEGIN
##(*    getoption('INCREMENT ',increment);            *)
        IF increment < 0 THEN
##          increment:= "100" 1;
        END;

    doting:=NOT option('NODOTS    ');

    IF option('BEGIN     ') THEN
        BEGIN
##(*    getoption('BEGIN     ',indentbegin);          *)
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
#################*)##################
    END (*CHECKOPTIONS*);
    (*PAGE AND LINE CONTROL*) (*HEADER*) (*NEWPAGE*) (*NEWLINE*)

PROCEDURE header (name: alfa);
    (*PRINT TOP OF FORM AND HEADER ON LIST OUTPUT*)
    VAR
        position: integer;

    BEGIN (*HEADER*)
    pagecnt2 := pagecnt2 + 1;
    reallincnt := 0;
    IF crossing THEN
        BEGIN
        page(crosslist); " writeln(crosslist) ;"
##      write (crosslist, 'Page':9, pagecnt:4, '-', pagecnt2:3 ", ' ':15");
        position := 84;
        IF maxch < 84 THEN
            BEGIN
            reallincnt:=1;
            writeln(crosslist);
            position := 42;
            END;
        write(crosslist,'   [', prog_name,']', time"_text":50, date"_text");
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
##    (*write(newsource, chr(cr), chr(ff));*)    writeln(newsource) ;
    header (blanks);
    IF eoln (oldsource) THEN
        readln(oldsource);
    linecnt := 0;
    reallincnt := 0;
    IF prog_name <> blanks  THEN
        write(tty, pagecnt:3,'..');
##   (* break(tty); *)
    END (*NEWPAGE*);


PROCEDURE newline;
    BEGIN
    IF reallincnt = maxline THEN
        header (blanks);
    linecnt := linecnt + 1;
    reallincnt := reallincnt + 1;
##  if crossing then write(crosslist, ' ':5);
##  if renewing then write(newsource, ' ':5) ;
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
##                  write(crosslist, ' ', dots: lastspaces - 1);
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
##                  IF (NOT flagger[j]) AND (* ('A' <= buffer[j]) AND
###                  (buffer[j] <= 'Z') *) (delsy[buffer[j]] = letterup) THEN
                        BEGIN
                        IF crossing THEN
###                         write(crosslist,chr(ord(buffer[j])-64(*+40B*)));
                        IF renewing THEN
###                         write(newsource,chr(ord(buffer[j])-64(*+40B*)));
                        END
                    ELSE
##                  IF flagger[j] AND (delsy[buffer[j]] = letterlo)  then

                        BEGIN
                        IF crossing THEN
###                         write(crosslist,chr(ord(buffer[j])+64(*+40B*)));
                        IF renewing THEN
###                         write(newsource,chr(ord(buffer[j])+64(*+40B*)));
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
##      lastspaces := 0 ;  if i > 0 then  lastspaces := i ;   (*max(0,i);*)
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
                LABEL 111 ;
                VAR
                    ch : char;
                BEGIN (*READLINE*)
                (*ENTERED AT THE BEGINNING OF A LINE*)
                REPEAT
                    WHILE eoln (oldsource) AND NOT eof (oldsource) DO
                        BEGIN
                        (*IS THIS A PAGE MARK?*)
##                     "getlinenr (oldsource,linenb);"
                        readln(oldsource);
                        IF (linenb = '     ') AND programpresent THEN
                            newpage
                        ELSE
                            IF programpresent THEN
                                BEGIN
                                (*HANDLE BLANK LINE*)
                                newline;
                                IF crossing THEN
                                    writeln (crosslist,"chr(ht)"' ':12,linecnt *
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
##            (*LOOP *)  repeat
                    bufflen := bufflen + 1;
                    buffer [bufflen] := ch;
                    flagger[bufflen]:=nonrcase;
##            (*EXIT*) IF (eoln (oldsource) OR (bufflen = oldwidth))
                         THEN GOTO 111 ;
                    read(oldsource, ch);
                  (*END*) UNTIL FALSE ;
              111:
                buffer[bufflen+1] := ' '; (*SO WE CAN ALWAYS BE ONE CHAR AHEAD*)
                IF NOT eoln (oldsource) THEN
                    error(linetoolong)
                ELSE
                    IF NOT eof (oldsource) THEN
                        BEGIN
##                     "getlinenr (linenb);"
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
##          LABEL 222 ;
            VAR
                i,j: integer;  lid : alfa ;
            BEGIN (*RESWORD*)
##          resword:= false; lid := sy ;
##          for i := 1 to 10 do
##            if delsy[lid[i]] = letterlo then lid[i] := chr( ord(lid[i]) +64) ;
##          FOR i:=resnum[lid[1]] TO resnum[succ(lid[1])] - 1
            DO
##              IF reslist[ i ] = lid THEN
                    BEGIN
                    resword := true;
                    syty := ressy [i];
##               (* i:=resnum[succ(sy[1])]; ** BUG 1 **   *)
                    FOR j:=bufferptr-syleng-1 TO bufferptr-2 DO
                        flagger[j]:=rescase;
##                   GOTO 222;
                    END;
##          222:
            END (*RESWORD*) ;


##      PROCEDURE findname "(curproc: listptrty)" (*does not seem to be used*);
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
                IF sy = listptr@.name THEN
                    BEGIN
                    found := true;
##                  IF"(listptr@.profunflag IN ['P', 'F'])"(NOT declaring) AND
##                    ((listptr@.profunflag = 'P')OR(listptr@.profunflag = 'F'))
                      THEN
                        BEGIN
                        new (workcall);
                        workcall@.whom := listptr@.procdata;
                        workcall@.nextcall := NIL;
                        END;
                    zptr := listptr@.last;
                    IF (zptr@.linenr <> linecnt+1) OR (zptr@.pagenr <> pagecnt)
THEN
                        BEGIN
                        new (listptr@.last);
                        WITH listptr@.last@ DO
                            BEGIN
                            linenr := linecnt + 1;
                            pagenr := pagecnt;
                            contlink := NIL;
                            IF declaring THEN
                                declflag := 'D'
                            ELSE
                                declflag := ' ';
                            END;
                        zptr@.contlink := listptr@.last;
                        END
                    ELSE
                        zptr@.declflag := 'M';
                    END
                ELSE
                    IF sy > listptr@.name THEN
                        BEGIN
                        listptr:= listptr@.rlink;
                        right:= true;
                        END
                    ELSE
                        BEGIN
                        listptr:= listptr@.llink;
                        right:= false;
                        END;
                END;
            IF NOT found THEN
                BEGIN (*UNKNOWN IDENTIFIER*)
                new (listptr);
                WITH listptr@ DO
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
                        lptr@.rlink := listptr
                    ELSE
                        lptr@.llink := listptr;
                WITH listptr@ DO
                    BEGIN
                    new (first);
                    WITH first@ DO
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
            IF locprocstl@.firstcall = NIL THEN
                locprocstl@.firstcall := workcall
            ELSE
                BEGIN
                thiscall := locprocstl@.firstcall;
                repeated := false;
                finished := false;
                WHILE (NOT finished) AND (NOT repeated) DO
                    IF thiscall@.whom@.procname@.name = workcall@.whom@.procname
@.name THEN
                        repeated := true
                    ELSE
                        IF thiscall@.nextcall = NIL THEN
                            finished := true
                        ELSE
                            thiscall := thiscall@.nextcall;
                IF NOT repeated THEN
                    thiscall@.nextcall := workcall;
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
##        var temposp : boolean ;

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
##              readbuffer ;  flagger[bufferptr] := comcase ; readbuffer ;
                (* skip over comment beginning bracket *)
##              if ch = '%' then
##              REPEAT  readbuffer;  flagger[bufferptr] := comcase;
##                IF ch <> '*' THEN
##                  BEGIN
##                  IF ch = 'X' THEN
##                    BEGIN readbuffer;  crossing := ch = '+'  END
##                  ELSE IF ch = 'F' THEN
##                    BEGIN readbuffer; renewing := ch = '+';  END
##                  ELSE IF ch = 'R' THEN
##                    BEGIN readbuffer; rescase := ch = '+' END
##                  ELSE IF ch = 'C' THEN
##                    BEGIN  readbuffer ;  comcase := ch = '+' ;  END
##                  ELSE IF ch = 'N' THEN
##                    BEGIN  readbuffer ;  nestcomments := ch ='+'  END
##                  ELSE IF ch='I' THEN
##                    BEGIN  readbuffer ;  nonrcase := ch = '+' END
##                  (*ELSE IF ch = 'S' THEN
##                      BEGIN  readbuffer ;  SAVEREGS := ch <> '-'  END
##                      ELSE IF ch = 'F' THEN
##                      BEGIN readbuffer ;  SAVEFPRS := ch <> '-' ;
##                      END  *) ;
##                  readbuffer
##                END
##              UNTIL ch <> ',' ;
##
##          (*  REPEAT   *)
##              WHILE NOT((ch = secondch) AND (buffer[bufferptr-2] = firstch)
##                        OR eob)  DO
##                  begin
##                  if nestcomments then
##                    if (ch = '(') and (buffer[bufferptr] = '*') then
##                      begin
##                      temposp := oldspaces ;
##                      docomment(2, '*', ')', false) ;
##                      oldspaces := temposp;
##                      readbuffer;
##                      end;
##
                    flagger[bufferptr]:=comcase;
                    readbuffer;
##
##                  end (* while .. *) ;
##          (*  UNTIL (ch = secondch) AND (buffer[bufferptr-2] = firstch) OR eob
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
       "WHILE (ch IN ['_', '(', ' ', '$', '?', '!', '@', '%', '/', '\']) AND NOT
 eob  DO"
###     WHILE ((delsy[ch] in [spacech, dquotech, skipch, lparent])
###           AND (NOT eob)) DO
##   (* WHILE  ( (ch = '_') OR (ch = '(') OR (ch = ' ') OR (ch = '$') OR
##              (ch = '?') OR (ch = '!') OR (ch = '@') OR (ch = '%') OR
##              (ch = '/') OR (ch = '\') OR (ch = '#') OR (ch = '"') )
##              AND (NOT eob)  DO  *)
            BEGIN
            IF (ch = '(') AND (buffer[bufferptr] = '*') THEN
                docomment (2,'*',')', false)
            ELSE
##           (* IF (ch = '/') AND (buffer[bufferptr] = '*') THEN
                    docomment (2,'*','/',cleaning)
##              ELSE   *)
##                  IF ch = '"' then  docomment(1, '"', ' ', cleaning)
##                  ELSE  IF ch = '#' THEN buffer[bufferptr-1] := ' '
##                  (*ELSE
                    IF ch = '%' THEN
##                    IF   (buffer[bufferptr] >= '0') AND
##                         (buffer[bufferptr] <= '9') then
                            if anyversion OR (ord(buffer[bufferptr]) - ord ('0')
 = goodversion) then
                                bufferptr := bufferptr + 1
                            else
                                docomment (1,'\','\',cleaning)
                        else
##                      docomment (1,'\','\',cleaning)    *)
##                  ELSE  IF ch = '#' THEN buffer[bufferptr-1] := ' '
                    ELSE
##                      IF (ch = '(') (* OR (ch = '/') *) THEN
                            GOTO 1;
            readbuffer;
            END;
##    " CASE ch OF
            'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I',
            'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q',
            'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y',
##          'Z':"
##   (*  IF (ch >= 'A') AND (ch <= 'Z')  THEN  *)
###
###    CASE delsy[ch] OF
###
###    letterup, letterlo :
              BEGIN
              syleng := 0;
              sy := '          ';
              REPEAT
                  syleng := syleng + 1;
                  IF syleng <= 10 THEN
                      sy [syleng] := ch;
                  readbuffer;
##       (*   UNTIL NOT (((ch >= '0') AND (CH <= '9')) OR
##                      ((ch >= 'A') AND (ch <= 'Z')) OR (ch = '_'));*)
###           UNTIL NOT(delsy[ch] IN [letterup, letterlo, digit,
###                                   dollarch, undsch]) ;
              IF NOT resword THEN
                  BEGIN
                  syty := ident ;
##                findname"(curproc)";
                  END
##            END ;
##    (* ELSE IF  (ch >= '0') AND (ch <= '9') THEN *)
##        " '0', '1', '2', '3', '4', '5', '6', '7', '8',
            '9':"

##          digit :
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
##            END  ;
##     (* ELSE IF ch = ''''  THEN  *)
###       quotech :
               BEGIN
               syty := strgconst;
               REPEAT
###             (* flagger[bufferptr]:=strcase; *)
###                (*this ensures that Strings will not be up/low shifted*)
###                flagger[bufferptr]:= delsy[buffer[bufferptr]] = letterup ;
                   readbuffer;
               UNTIL (ch = '''') OR eob OR eoline;
               IF ch <> '''' THEN
                   error(missgquote);
               readbuffer;
##             END ;
##    (*  ELSE IF ch = '"' THEN
              BEGIN
              REPEAT
                  readbuffer
##            UNTIL NOT ((ch >= '0') AND (ch <= '9')) "(digits + ['A'..'F']))";
              syty := intconst;
##            END     *)
##     (* ELSE IF ch = ' ' THEN *)

###    spacech : syty := eobsy ;   (*END OF FILE*)
##     (* ELSE   *)  (*OTHERS*)

###    othersy, lparent, rparent, lbracket, rbracket,
###    semicolon, point, colon, eqlsy :
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
##            END;  (* CASE delsy[ch] OF *)
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
##              IF  NOT (syty IN [casesy, recordsy, semicolon, lparent,
##                           eqlsy, colon])  THEN  insymbol
##              ELSE
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
##                     "OTHERS:  insymbol"
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
##          IF  NOT(syty IN [casesy, recordsy, semicolon, lparent,
##                       eqlsy, colon])  THEN  insymbol
##          ELSE
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
##            " OTHERS   : insymbol "
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
            LABEL 333 ;
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
##        " LOOP " REPEAT
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
##        " EXIT"IF syty IN [endsy,eobsy,proceduresy,functionsy] THEN GOTO 333;
                error (enderrinblkstr);
##              UNTIL FALSE "END";
##        333:
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
##IF procstrucdata.exists then      (*** BUG 2 ***)
    WITH procstrucdata, item, procname@ DO
##    (*IF exists THEN*)
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
                new(procstrucl@.nextproc);
                procstrucl := procstrucl@.nextproc;
                procdata := procstrucl;
                procstrucl@ := item;
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
                procstrucf@.procname := listptr;
                listptr@.procdata := procstrucf;
                listptr@.profunflag := 'M';
                writeln(tty);
                write(tty,'1', version:8,new_name:7,' [',prog_name,']  1..');
                IF pagecnt > 1 THEN
                    FOR i := 2 TO pagecnt DO
                        write (tty, i:3,'..');
##               (* break(tty); *)
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
                    listptr@.profunflag := 'P'
                ELSE
                    listptr@.profunflag := 'F';
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
        locprocstl@.begline := linecnt + 1;
        locprocstl@.begpage := pagecnt;
        statement;
        locprocstl@.endline := linecnt + 1;
        locprocstl@.endpage := pagecnt;
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
##      writeln (tty,errcount:4,' ERROR(S) DETECTED');    (* break(tty); *)
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
                header (listptr@.name)
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
        WITH procstrucl@, procname@ DO
            BEGIN
            IF numbering THEN
##              write (crosslist, linecnt * increment:6, '  ')
##          ELSE write(crosslist, linenr*increment:6, '  ') ;
##       (* IF depth > 2 THEN
                write (crosslist, '. ',dots:depth-2)
            ELSE
##              write (crosslist, '.':depth);  *)
##          write  (crosslist, ' ': depth*3, name: 10, ' ':25-depth*3,
                    ' (', profunflag, ')', mark:2, externflag:2
##                  ",chr(ht), linenr * increment : 8");
##          IF numbering then  write(crosslist, linenr*increment:10) ;
            IF listpgnr OR (pagenr > 1) THEN
                write(crosslist, ' / ',pagenr : 1);
            IF (mark = ' ') AND NOT ((externflag = 'E') OR
##                                   (externflag = 'F')) THEN
                BEGIN
                write (crosslist, begline * increment: 10);
                IF listpgnr THEN
                    write (crosslist, ' / ', begpage: 1);
##              write (crosslist, endline * increment: 7);
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
        link := listptr@.first;
        REPEAT
            IF count = maxcnt THEN
                BEGIN
                writeln(crosslist);
                checkpage(true);
                write (crosslist, ' ' : spaces);
                count := 0;
                END;
            count := count + 1;
            write (crosslist, link@.linenr * increment : 6);
            IF listpgnr THEN
                write(crosslist, '/',link@.pagenr : 2);
            write (crosslist,link@.declflag,'  ');
            link := link@.contlink;
        UNTIL link = NIL;
        END (*WRITELINENR*) ;

    PROCEDURE dumpcall (thisproc: procstructy; depth: integer);
        VAR
            thiscall: calledty;

        BEGIN (*DUMPCALL*)
        linecnt := linecnt + 1;
        WITH thisproc@ DO
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
##                  dumpcall (thiscall@.whom, depth + "4"1);
                    thiscall := thiscall@.nextcall;
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
    WITH firstname ['M']@ DO  (*DELETE 'MAIN'*)
        IF rlink = NIL THEN
            firstname ['M'] := llink
        ELSE
            BEGIN
            listptr := rlink;
            WHILE listptr@.llink <> NIL DO
                listptr := listptr@.llink;
            listptr@.llink := llink;
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
                    WHILE listptr@.llink <> NIL DO
                        BEGIN
                        pred := listptr;
                        listptr := listptr@.llink;
                        END;
                    IF listptr = firstname [indexch] THEN
                        firstname [indexch] := listptr@.rlink
                    ELSE
                        pred@.llink := listptr@.rlink;
                    writeln(crosslist);
                    checkpage(true);
                    write (crosslist, listptr@.profunflag, listptr@.name : 11);
                    writelinenr (12);
                    END;
##          writeln(crosslist) ;
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
##                  writeprocname (procstrucl, procstrucl@.proclevel"* 2",' ',
false);
                    procstrucl := procstrucl@.nextproc;
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
                    IF NOT procstrucl@.printed THEN
                        dumpcall (procstrucl, 0);
                    procstrucl := procstrucl@.nextproc;
                    END;
                END;
##          writeln(crosslist) ;
            END;
        END;
    crossing := oldcrossing;
    END (*PRINT_XREF_LIST*) ;

    (*MAIN PROGRAM*)

BEGIN
# RESET(OLDSOURCE) ;
# REWRITE(NEWSOURCE);
# REWRITE(CROSSLIST) ;
# REWRITE(TTY) ;
checkoptions;
##"getstatus(oldsource,new_name,new_prot,new_ppn,new_dev);"

(*FIND MAX POSSIBLE LINE NUMBER WITH THIS INCREMENT*)
##increment := 1 ;
maxinc := (99999 DIV increment);
IF maxinc > 4000 THEN
    maxinc := 4000;

##"LOOP" REPEAT
    init;
    block;
##"EXIT" IF NOT programpresent OR (syty = eobsy) THEN GOTO 444;
    IF refing OR decnesting OR callnesting THEN
        print_xref_list;
##  "dispose(heapmark);"    (*RELEASE THE ENTIRE HEAP*)
    rtime[0]:=clock(1)-rtime[0];
    rtime[1]:=rtime[0] DIV 60000;
    rtime[2]:=(rtime[0] MOD 60000) DIV 1000;
    rtime[3]:=rtime[0] MOD 1000;
    writeln(tty);
    writeln(tty,' RUNTIME:',rtime[1]:3,':',rtime[2]:2,'.',rtime[3]:3);
##   (* break(tty); *)
##  UNTIL FALSE "END";
##  444:
##   (* writeln(tty, bel);   break(tty); *)

##  "getnextcall (link_name, link_device);
##   IF link_name <> '         ' THEN
##     call (link_name, link_device); "
END (*PCROSS*).
***end#
  (*************************************************************
  *                                                           *
  *      P A S C A L   P R O G R A M   F O R M A T T E R      *
  *      -----------------------------------------------      *
  *                                                           *
  *             COPYRIGHT MICHAEL N. CONDICT 1975             *
  *                                                           *
  *************************************************************)
 PROGRAM FORMAT( (*PROG,*) OUTPUT, INPUT );

 CONST
    LASTPSYMBOLNAME = 40;
    BUFFERSIZE = 160;
    BUFFERSZP1 = 161;
    BUFFSZDIV10 = 16;
    ALFLEN = 10;

 TYPE
    chset = 0..63;
    STATMNTTYPES =
       (FORWITHWHILESTATEMENT, REPEATSTATEMENT, IFSTATEMENT,
        CASESTATEMENT, COMPOUNDSTATEMENT, OTHERSTATEMENT);
    SYMBOLS =
       (PROGRAMSYMBOL, COMMENT, BEGINSYMBOL, ENDSYMBOL,
        SEMICOLON, CONSTSYMBOL, TYPESYMBOL, RECORDSYMBOL,
        COLONSYMBOL, EQUALSYMBOL, PERIODSYMBOL, RANGE,
        CASESYMBOL, OTHERSYMBOL, IFSYMBOL, THENSYMBOL,
        ELSESYMBOL, DOSYMBOL, OFSYMBOL, FORSYMBOL, WITHSYMBOL,
        WHILESYMBOL, REPEATSYMBOL, UNTILSYMBOL, IDENTIFIER,
        VARSYMBOL, VALUESYMBOL, PROCEDSYMBOL, FUNCTIONSYMBOL,
        LEFTBRACKET, RIGHTBRACKET, COMMASYMBOL, LABELSYMBOL,
        LEFTPAREN, RIGHTPARENTH, UPARROW, ALPHAOPERATOR);
    WIDTH = 0 .. BUFFERSIZE;
    CHARTYPES =
       (ALPHANUMERIC, ENDOFLINE, BLANK, RIGHTARROW, SLASH,
        LFTPAREN, COLON, PERIOD, STRING, LESSTHAN, GREATERTHAN,
"SZH"   STAR, DQUOTE, OTHER);
    MARGINS = - 100 .. BUFFERSIZE;
    SYMBOLSET = SET OF SYMBOLS;
    OPTIONSIZE = - 99 .. 99;
    COMMENTTEXT = ARRAY [1 .. BUFFSZDIV10] OF ALFA;
    SYMBOLSTRING = ARRAY [WIDTH] OF CHAR;

 VAR
    READINGFORMATOPTIONS, PROGISPASCAL2: BOOLEAN;
    PROG: TEXT;
    I: INTEGER;
 (*USED AS FOR LOOP INDEX*)
    CHARACTER: CHAR;
    READCOLUMN, READCOL2: 0 .. 1000;
    OUTPUTCOL, WRITECOLUMN, LEFTMARGIN, ACTUALLEFTMARGIN:
       MARGINS;
    READCOL1, WRITECOL1, WRITECOL2: MARGINS;
    DISPLAYISON, PROCEDNAMESWANTED, ENDCOMMENTSWANTED,
       PACKERISOFF, SAVEDCOMPRESS, COMPRESSWANTED, NOFORMATTING,
       DISPLAYWANTED, CROSSREFWANTED: BOOLEAN;
    LINENUMBER, INCREMENT: - 999 .. 999;
    INDENTINDEX, LONGLINEINDENT, SYMBOLGAP, DECLARALIGNMENT,
       STATMTSEPARATION, PROCEDSEPARATION: OPTIONSIZE;
    LASTSYMBOL, SYMBOLNAME: SYMBOLS;
    ALPHASYMBOLS, ENDLABEL, ENDCONST, ENDTYPE, ENDVAR, ENDVALUE:
       SYMBOLSET;
    SYMBOL: SYMBOLSTRING;
    LENGTH: WIDTH;
"SZH"  FIRSTDQUOTE,
    SYMBOLISNUMBER, LASTPROGPARTWASBODY: BOOLEAN;
    DIGITS: SET OF CHSET;
    OLDEST: WIDTH;
    CHARCOUNT: INTEGER (* USED AS A TOTAL CHARACTERS COUNT*);
    MAIN: COMMENTTEXT;
    MAINNMLENGTH: WIDTH;
    BLANKS, ZEROES: ALFA;
    UNWRITTEN: ARRAY [WIDTH] OF RECORD
                                   CH: CHAR;
                                   CHISENDLINE: BOOLEAN;
                                   INDENTAFTEREOL: MARGINS;
                                END;
    TYPEOF: ARRAY [CHAR] OF CHARTYPES;
    PASCALSYMBOL: ARRAY [1 .. LASTPSYMBOLNAME] OF ALFA;
    PSYMBOLNAME: ARRAY [1 .. LASTPSYMBOLNAME] OF SYMBOLS;
    NAMEOF: ARRAY [CHAR] OF SYMBOLS;
    STATEMENTTYPEOF: ARRAY [SYMBOLS] OF STATMNTTYPES;
"szh"  CHMAP   : ARRAY [CHAR] OF 0..63  ;  (*CHAR CODE CONVERSION           *)
"szh"  PQ      : INTEGER;


"szh"procedure init_chmap;
"szh"
"szh"   (* this routine initializes the ebcdic to 'CCDC' display code MAP *)
"szh"   (* chmap : array[char] of 0..63                                   *)
"szh"
"szh"   const ordmaxch= 255;
"szh"
"szh"   var ch: char;
"szh"
"szh"   BEGIN
"szh"
"szh"    FOR ch := chr(0) TO chr(ORDMAXCH) DO CHMAP[ch] := 0;
"szh"    FOR CH := 'A' TO 'I' DO  CHMAP[CH] := ORD(CH)-192 ;
"szh"    FOR CH := 'J' TO 'R' DO  CHMAP[CH] := ORD(CH)-199 ;
"szh"    FOR CH := 'S' TO 'Z' DO  CHMAP[CH] := ORD(CH)-207 ;
"szh"    FOR CH := 'a' TO 'i' DO  CHMAP[CH] := ORD(CH)-128 ;
"szh"    FOR CH := 'j' TO 'r' DO  CHMAP[CH] := ORD(CH)-135 ;
"szh"    FOR CH := 's' TO 'z' DO  CHMAP[CH] := ORD(CH)-143 ;
"szh"    FOR CH := '0' TO '9' DO  CHMAP[CH] := ORD(CH)-213 ;
"szh"    CHMAP['+']  :=  37 ;
"szh"    CHMAP['-']  :=  38 ;
"szh"    CHMAP['*']  :=  39 ;
"szh"    CHMAP['/']  :=  40 ;
"szh"    CHMAP['(']  :=  41 ;
"szh"    CHMAP[')']  :=  42 ;
"szh"    CHMAP['$']  :=  43 ;
"szh"    CHMAP['=']  :=  44 ;
"szh"    CHMAP[' ']  :=  45 ;
"szh"    CHMAP[',']  :=  46 ;
"szh"    CHMAP['.']  :=  47 ;
"szh"    CHMAP['#']  :=  48 ;
"szh"    CHMAP['[']  :=  49 ;
"szh"    CHMAP[']']  :=  50 ;
"szh"    CHMAP['%']  :=  51 ;
"szh"    CHMAP['"']  :=  52 ;
"szh"    CHMAP['_']  :=  53 ;
"szh"    CHMAP['¯']  :=  54 ;      (* SHOULD BE '!' *)
"szh"    CHMAP['&']  :=  55 ;
"szh"    CHMAP[''''] :=  56 ;
"szh"    CHMAP['ò']  :=  57 ;
"szh"    CHMAP['<']  :=  58 ;
"szh"    CHMAP['>']  :=  59 ;
"szh"    CHMAP['@']  :=  60 ;
"szh"    CHMAP[':']  :=  61 ;       (* SHOULD BE '0', 61 IS BACK SLASH *)
"szh"    CHMAP['Ö']  :=  62 ;
"szh"    CHMAP[';']  :=  63 ;
"szh"
"szh"   END (*init_chmap*);
"szh"
 PROCEDURE CONSTANTSINITIALIZATION;

    BEGIN
"SZH"  FIRSTDQUOTE := FALSE;
       MAIN[1] := 'MAIN      ';   MAINNMLENGTH := 4;
       BLANKS := '          ';   ZEROES := '0000000000';
       FOR I := 0 TO BUFFERSIZE DO
          WITH UNWRITTEN[I] DO
             BEGIN
                CH := 'A';   CHISENDLINE := FALSE;
                INDENTAFTEREOL := 0;
             END;
       FOR CHARACTER := CHR(0) TO CHR(255) DO
          TYPEOF[CHARACTER] := OTHER;
       TYPEOF[CHR(0)] := BLANK;
       FOR CHARACTER := 'A' TO 'I' DO
          TYPEOF[CHARACTER] := ALPHANUMERIC;
       FOR CHARACTER := 'J' TO 'R' DO
          TYPEOF[CHARACTER] := ALPHANUMERIC;
       FOR CHARACTER := 'S' TO 'Z' DO
          TYPEOF[CHARACTER] := ALPHANUMERIC;
       FOR CHARACTER := '0' TO '9' DO
          TYPEOF[CHARACTER] := ALPHANUMERIC;
       TYPEOF['*'] := STAR;         TYPEOF['/'] := SLASH;
       TYPEOF['('] := LFTPAREN;     TYPEOF[' '] := BLANK;
       TYPEOF['.'] := PERIOD;       TYPEOF[''''] := STRING;
       TYPEOF[':'] := COLON;
"SZH"  TYPEOF['¯'] := RIGHTARROW;   TYPEOF['"'] := DQUOTE;
"SZH"  TYPEOF['$'] := ALPHANUMERIC; TYPEOF['_'] := ALPHANUMERIC;
"SZH"  TYPEOF['#'] := BLANK;
       TYPEOF['<'] := LESSTHAN;     TYPEOF['>'] := GREATERTHAN;
"SZH"  FOR CHARACTER := CHR(0) TO CHR(255) DO
          NAMEOF[CHARACTER] := OTHERSYMBOL;
       NAMEOF['('] := LEFTPAREN;
       NAMEOF[')'] := RIGHTPARENTH;
       NAMEOF['='] := EQUALSYMBOL;   NAMEOF[','] := COMMASYMBOL;
       NAMEOF['.'] := PERIODSYMBOL;
       NAMEOF['['] := LEFTBRACKET;
       NAMEOF[']'] := RIGHTBRACKET;
       NAMEOF[':'] := COLONSYMBOL;   NAMEOF['@'] := UPARROW;
"SZH"  NAMEOF['ò'] := COMMENT;   NAMEOF['<'] := EQUALSYMBOL;
"SZH"  NAMEOF['"'] := COMMENT;
       NAMEOF['>'] := EQUALSYMBOL;   NAMEOF[';'] := SEMICOLON;
       PASCALSYMBOL[1] := 'PROGRAM   ';
       PASCALSYMBOL[21] := 'FUNCTION  ';
       PASCALSYMBOL[2] := 'BEGIN     ';
       PASCALSYMBOL[22] := 'LABEL     ';
       PASCALSYMBOL[3] := 'END       ';
       PASCALSYMBOL[23] := 'IN        ';
       PASCALSYMBOL[4] := 'CONST     ';
       PASCALSYMBOL[24] := 'MOD       ';
       PASCALSYMBOL[5] := 'TYPE      ';
       PASCALSYMBOL[25] := 'DIV       ';
       PASCALSYMBOL[6] := 'VAR       ';
       PASCALSYMBOL[26] := 'AND       ';
       PASCALSYMBOL[7] := 'VALUE     ';
       PASCALSYMBOL[27] := 'OR        ';
       PASCALSYMBOL[8] := 'RECORD    ';
       PASCALSYMBOL[28] := 'NOT       ';
       PASCALSYMBOL[9] := 'CASE      ';
       PASCALSYMBOL[29] := 'LT        ';
       PASCALSYMBOL[10] := 'IF        ';
       PASCALSYMBOL[30] := 'LE        ';
       PASCALSYMBOL[11] := 'THEN      ';
       PASCALSYMBOL[31] := 'IS        ';
       PASCALSYMBOL[12] := 'ELSE      ';
       PASCALSYMBOL[32] := 'ISNT      ';
       PASCALSYMBOL[13] := 'DO        ';
       PASCALSYMBOL[33] := 'EQ        ';
       PASCALSYMBOL[14] := 'OF        ';
       PASCALSYMBOL[34] := 'NE        ';
       PASCALSYMBOL[15] := 'FOR       ';
       PASCALSYMBOL[35] := 'GE        ';
       PASCALSYMBOL[16] := 'WHILE     ';
       PASCALSYMBOL[36] := 'GT        ';
       PASCALSYMBOL[17] := 'WITH      ';
       PASCALSYMBOL[37] := 'JOIN      ';
       PASCALSYMBOL[18] := 'REPEAT    ';
       PASCALSYMBOL[38] := 'MEET      ';
       PASCALSYMBOL[19] := 'UNTIL     ';
       PASCALSYMBOL[39] := 'ARRAY     ';
       PASCALSYMBOL[20] := 'PROCEDURE ';
       PASCALSYMBOL[40] := 'NOSYMBOL  ';
       PSYMBOLNAME[1] := PROGRAMSYMBOL;
       PSYMBOLNAME[21] := FUNCTIONSYMBOL;
       PSYMBOLNAME[2] := BEGINSYMBOL;
       PSYMBOLNAME[22] := LABELSYMBOL;
       PSYMBOLNAME[3] := ENDSYMBOL;
       PSYMBOLNAME[4] := CONSTSYMBOL;
       PSYMBOLNAME[5] := TYPESYMBOL;
       PSYMBOLNAME[6] := VARSYMBOL;
       PSYMBOLNAME[7] := VALUESYMBOL;
       PSYMBOLNAME[8] := RECORDSYMBOL;
       PSYMBOLNAME[9] := CASESYMBOL;
       PSYMBOLNAME[10] := IFSYMBOL;
       PSYMBOLNAME[11] := THENSYMBOL;
       PSYMBOLNAME[12] := ELSESYMBOL;
       PSYMBOLNAME[13] := DOSYMBOL;   PSYMBOLNAME[14] := OFSYMBOL;
       PSYMBOLNAME[15] := FORSYMBOL;
       PSYMBOLNAME[16] := WHILESYMBOL;
       PSYMBOLNAME[17] := WITHSYMBOL;
       PSYMBOLNAME[18] := REPEATSYMBOL;
       PSYMBOLNAME[19] := UNTILSYMBOL;
       PSYMBOLNAME[20] := PROCEDSYMBOL;
       PSYMBOLNAME[40] := IDENTIFIER;
       FOR I := 23 TO 39 DO PSYMBOLNAME[I] := ALPHAOPERATOR;
       FOR SYMBOLNAME := PROGRAMSYMBOL TO ALPHAOPERATOR DO
          STATEMENTTYPEOF[SYMBOLNAME] := OTHERSTATEMENT;
       STATEMENTTYPEOF[BEGINSYMBOL] := COMPOUNDSTATEMENT;
       STATEMENTTYPEOF[CASESYMBOL] := CASESTATEMENT;
       STATEMENTTYPEOF[IFSYMBOL] := IFSTATEMENT;
       STATEMENTTYPEOF[FORSYMBOL] := FORWITHWHILESTATEMENT;
       STATEMENTTYPEOF[WHILESYMBOL] := FORWITHWHILESTATEMENT;
       STATEMENTTYPEOF[WITHSYMBOL] := FORWITHWHILESTATEMENT;
       STATEMENTTYPEOF[REPEATSYMBOL] := REPEATSTATEMENT;
    END (*CONSTANTSINITIALIZATION*);

 PROCEDURE WRITEA(CHARACTER: CHAR);

    VAR
       I: WIDTH;
       TESTNO: INTEGER;

    BEGIN
       CHARCOUNT := CHARCOUNT + 1;
       OLDEST := CHARCOUNT MOD BUFFERSIZE;
       WITH UNWRITTEN[OLDEST] DO
          BEGIN
             IF CHARCOUNT > BUFFERSZP1
             THEN
                BEGIN
                   IF CHISENDLINE
                   THEN
                      BEGIN
                         IF INDENTAFTEREOL < 0
                         THEN
                            BEGIN
                               WRITE(OUTPUT, BLANKS: - INDENTAFTEREOL);
                               OUTPUTCOL := OUTPUTCOL -
                                  INDENTAFTEREOL;
                            END
                         ELSE
                            BEGIN
                               IF INCREMENT < 0
                               THEN
                                  BEGIN
                                     I := WRITECOL2 - OUTPUTCOL
                                        + 1;
                                     IF I > 0
                                     THEN WRITE(OUTPUT, BLANKS: I);
                                     TESTNO := LINENUMBER;
                                     I := 0;
                                     REPEAT
                                        TESTNO := TESTNO DIV 10;
                                        I := I + 1;
                                     UNTIL TESTNO = 0;
                                     WRITE(OUTPUT, ZEROES: (6 - I),
                                        LINENUMBER: I);
                                     LINENUMBER := LINENUMBER -
                                        INCREMENT;
                                     IF LINENUMBER > 999999
                                     THEN
                                        LINENUMBER := LINENUMBER
                                           - 1000000;
                                     WRITELN(OUTPUT);  WRITE(OUTPUT, ' ');
                                  END
                               ELSE
                                  BEGIN
                                     WRITELN(OUTPUT, ' '); WRITE(OUTPUT, ' ');
                                     IF INCREMENT > 0 THEN
                                        BEGIN
                                           WRITE(OUTPUT, LINENUMBER: (
                                              WRITECOL1 - 2));
                                           INDENTAFTEREOL :=
                                              INDENTAFTEREOL -
                                              WRITECOL1 + 2;
                                           LINENUMBER :=
                                              LINENUMBER +
                                              INCREMENT;
                                        END
                                  END;
                               IF INDENTAFTEREOL > 0 THEN
                                  WRITE(OUTPUT, BLANKS: INDENTAFTEREOL);
                               OUTPUTCOL := INDENTAFTEREOL + 1;
                            END;
                         CHISENDLINE := FALSE;
                      END (*IF CHISENDLINE*)
                   ELSE
                      BEGIN
                         WRITE(OUTPUT, CH);
                         OUTPUTCOL := OUTPUTCOL + 1;
                      END (*ELSE*);
                END (*IF CHARCOUNT > *);
             CH := CHARACTER;   WRITECOLUMN := WRITECOLUMN + 1;
          END (*WITH*);
    END (*WRITEA*);

 PROCEDURE WRITEANEOLWITHNOINDENT;

    BEGIN
       WRITEA(' ');
       WITH UNWRITTEN[OLDEST] DO
          BEGIN
             CHISENDLINE := TRUE;
             INDENTAFTEREOL := WRITECOL1 - 1;
          END;
       WRITECOLUMN := WRITECOL1;
    END (*WRITEANEOLWITHNOINDENT*);

 PROCEDURE STARTNEWLINEANDINDENT;

    LABEL
       1, 2;

    VAR
       I: OPTIONSIZE;

    BEGIN
       IF PACKERISOFF AND DISPLAYISON
       THEN
          BEGIN
          2: WRITEA(' ');   1: LASTSYMBOL := PERIODSYMBOL;
             WITH UNWRITTEN[OLDEST] DO
                BEGIN
                   CHISENDLINE := TRUE;
                   INDENTAFTEREOL := WRITECOL1 + LEFTMARGIN - 1;
                END;
             WRITECOLUMN := WRITECOL1 + LEFTMARGIN;
          END (*IF PACKERISOFF*);
    END (*STARTNEWLINE*);

 PROCEDURE READACHARACTER;

    BEGIN
       IF READCOLUMN > READCOL2
       THEN
          BEGIN
             GET(PROG);
             IF READCOL2 < 999
             THEN BEGIN WHILE NOT EOLN(PROG) DO GET(PROG); END
             ELSE READCOLUMN := 2;
          END
       ELSE
          IF READCOLUMN <= 1
          THEN
             BEGIN
                IF READCOLUMN = 1   THEN GET(PROG)
                ELSE READCOLUMN := 1;
                WHILE READCOLUMN < READCOL1 DO
                   BEGIN
                      IF EOLN(PROG)   THEN READCOLUMN := 1
                      ELSE READCOLUMN := READCOLUMN + 1;
                      GET(PROG);
                   END;
             END
          ELSE GET(PROG);
       IF EOLN(PROG)
       THEN
          BEGIN
             CHARACTER := ' ';   READCOLUMN := 1;
             IF NOFORMATTING   THEN WRITEANEOLWITHNOINDENT;
          END
       ELSE
          BEGIN
             CHARACTER := PROG@;   READCOLUMN := READCOLUMN + 1;
             IF NOFORMATTING   THEN WRITEA(CHARACTER);
          END;

"SZH" IF CHARACTER = '"' THEN
"SZH"    IF FIRSTDQUOTE THEN
"SZH"       BEGIN  CHARACTER := 'ò';
"SZH"       FIRSTDQUOTE := FALSE;
"SZH"       END ;

    END (*READACHARACTER*);

 PROCEDURE WRITESYMBOL;

    VAR
       I: WIDTH;
       NUMBERBLANKSTOWRITE: OPTIONSIZE;
       WRITEWIDTH, TOTALINDENT: WIDTH;

    BEGIN
       IF DISPLAYISON
       THEN
          BEGIN
             NUMBERBLANKSTOWRITE := SYMBOLGAP;
             IF (LASTSYMBOL IN [LEFTPAREN, LEFTBRACKET,
                PERIODSYMBOL]) OR (SYMBOLNAME IN [SEMICOLON,
                RIGHTPARENTH, RIGHTBRACKET, COMMASYMBOL,
                PERIODSYMBOL, COLONSYMBOL]) OR (SYMBOLNAME IN [
                LEFTBRACKET, LEFTPAREN, UPARROW]) AND (
                LASTSYMBOL = IDENTIFIER)
             THEN NUMBERBLANKSTOWRITE := 0
             ELSE
                IF (SYMBOLNAME IN ALPHASYMBOLS) AND (LASTSYMBOL
                   IN ALPHASYMBOLS)
                THEN
                   IF WRITECOLUMN <= WRITECOL2 THEN
                      BEGIN
                         WRITEA(' ');
                         IF SYMBOLGAP > 0 THEN
                            NUMBERBLANKSTOWRITE := SYMBOLGAP - 1
                         ;
                      END;
             IF WRITECOLUMN + LENGTH + NUMBERBLANKSTOWRITE - 1 >
                WRITECOL2
             THEN
                BEGIN
                   WRITEA(' ');
                   WITH UNWRITTEN[OLDEST] DO
                      BEGIN
                         CHISENDLINE := TRUE;
                         IF PACKERISOFF
                         THEN
                            BEGIN
                               WRITEWIDTH := WRITECOL2 -
                                  WRITECOL1 + 1;
                               TOTALINDENT := LEFTMARGIN +
                                  LONGLINEINDENT;
                               IF LENGTH <= WRITEWIDTH -
                                  TOTALINDENT
                               THEN
                                  INDENTAFTEREOL := WRITECOL1 -
                                     1 + TOTALINDENT
                               ELSE
                                  IF LENGTH <= WRITEWIDTH
                                  THEN
                                     INDENTAFTEREOL := WRITECOL2
                                        - LENGTH
                                  ELSE
                                     BEGIN
                                        LENGTH := WRITEWIDTH;
                                        INDENTAFTEREOL :=
                                           WRITECOL1 - 1;
                                        IF SYMBOL[1] = ''''
                                        THEN
                                           SYMBOL[LENGTH] :=
                                              ''''
                                     END;
                               WRITECOLUMN := INDENTAFTEREOL + 1
                               ;
                            END
                         ELSE
                            BEGIN
                               IF LENGTH > WRITECOL2 - WRITECOL1
                                  + 1
                               THEN
                                  LENGTH := WRITECOL2 -
                                     WRITECOL1 + 1;
                               INDENTAFTEREOL := WRITECOL1 - 1;
                               WRITECOLUMN := WRITECOL1;
                            END;
                      END (*WITH*);
                END
             ELSE
                FOR I := 1 TO NUMBERBLANKSTOWRITE DO
                   WRITEA(' ');
             FOR I := 1 TO LENGTH DO WRITEA(SYMBOL[I]);
          END (*IF DISPLAYISON*);
       LASTSYMBOL := SYMBOLNAME;
    END (*WRITESYMBOL*);

 PROCEDURE READSYMBOL;

    CONST
       READNEXTCH = TRUE;
       DONTREADNEXTCH = FALSE;

    VAR
       TESTSYMBOL: ALFA;
       CHARNUMBER: WIDTH;
       I: WIDTH;

    PROCEDURE DOCOMMENT(TERMINATOR: CHAR);

       VAR
          I: OPTIONSIZE;
          SECONDTERMINATOR: CHAR;
          SAVEDSYMBGP: OPTIONSIZE;

       PROCEDURE COPYACHARACTER;

          LABEL
             10;

          BEGIN
             IF READINGFORMATOPTIONS
             THEN BEGIN CHARACTER := INPUT@;   GET(INPUT); END
             ELSE
                BEGIN
                   IF DISPLAYISON
                   THEN
                      BEGIN
                         IF WRITECOLUMN > WRITECOL2
                         THEN
                            BEGIN
                               IF (CHARACTER = ' ') AND
                                  NOT EOLN(PROG)
                               THEN
                                  BEGIN
                                     REPEAT READACHARACTER;
                                     UNTIL (CHARACTER <> ' ') OR
                                        EOLN(PROG);
                                     IF CHARACTER = TERMINATOR
                                     THEN GOTO 10;
                                  END;
                               IF NOT EOLN(PROG)
                               THEN
                                  BEGIN
                                     WRITEA(' ');
                                     WITH UNWRITTEN[OLDEST] DO
                                        BEGIN
                                           CHISENDLINE := TRUE;
                                           INDENTAFTEREOL :=
                                              WRITECOL1 +
                                              LEFTMARGIN - 1;
                                        END;
                                     WRITECOLUMN := WRITECOL1 +
                                        LEFTMARGIN;
                                  END;
                            END;
                         IF EOLN(PROG)
                         THEN WRITEANEOLWITHNOINDENT
                         ELSE WRITEA(CHARACTER);
                      END;
                   READACHARACTER;
                END;
         10:
          END (*COPYACHARACTER*);

       PROCEDURE COMPILERDIRECTIVES;

          BEGIN
             REPEAT
                REPEAT COPYACHARACTER;
"SZH"          UNTIL CHMAP[CHARACTER] IN
"SZH"                [CHMAP['E'], CHMAP['U'], CHMAP[' '], CHMAP['['],
"SZH"                 CHMAP[TERMINATOR] ];
             UNTIL NOT ((CHARACTER = 'E') AND PROGISPASCAL2);
             IF (CHARACTER = 'E') OR (CHARACTER = 'U')
             THEN
                BEGIN
                   COPYACHARACTER;
                   IF (CHARACTER = '+') OR (CHARACTER = '-') THEN
                      BEGIN
                         IF CHARACTER = '+'
                         THEN READCOL2 := 72
                         ELSE READCOL2 := 999;
                         IF (WRITECOL2 > 72)
                         THEN CHARACTER := '-';
                      END;
                END;
"SZH"       WHILE NOT (CHMAP[CHARACTER] IN [ CHMAP['['], CHMAP[TERMINATOR]]) DO
                COPYACHARACTER;
          END (*COMPILERDIRECTIVES*);

       PROCEDURE FORMATTERDIRECTIVES;

          CONST
             INVALID = - 1;

          TYPE
             PARAMCOUNT = 1 .. 2;
             PARAMS = ARRAY [PARAMCOUNT] OF MARGINS;

          VAR
             SPECIFICATION: PARAMS;
             FORMATOPTION: CHAR;
             PREVDISPLAY: BOOLEAN;
             ENDDIRECTV: SET OF CHSET;

          PROCEDURE READIN(N: PARAMCOUNT; VAR SPECIFICATION:
             PARAMS);

             VAR
                I: PARAMCOUNT;

             BEGIN
                FOR I := 1 TO N DO
                   BEGIN
"SZH"                 WHILE NOT (CHMAP[CHARACTER] IN (DIGITS +
                         ENDDIRECTV)) DO
                         COPYACHARACTER;
                      SPECIFICATION[I] := 0;
"SZH"                 IF NOT (CHMAP[CHARACTER] IN ENDDIRECTV)
                      THEN
                         REPEAT
                            SPECIFICATION[I] := 10 *
                               SPECIFICATION[I] + ORD(CHARACTER)
                               - ORD('0');
                            COPYACHARACTER;
"SZH"                    UNTIL NOT (CHMAP[CHARACTER] IN DIGITS)
                      ELSE SPECIFICATION[I] := INVALID;
                   END (*FOR*);
             END (*READIN*);

          BEGIN (*FORMATTERDIRECTIVES*)
"SZH"        ENDDIRECTV := [CHMAP[TERMINATOR], CHMAP[']']];
             REPEAT
"SZH"          IF ( CHMAP[CHARACTER] IN
"SZH"             [ CHMAP['A'], CHMAP['B'], CHMAP['C'], CHMAP['D'],
"SZH"               CHMAP['E'], CHMAP['G'], CHMAP['I'], CHMAP['L'],
"SZH"               CHMAP['N'], CHMAP['P'], CHMAP['R'], CHMAP['S'],
"SZH"               CHMAP['W'], CHMAP['F'], CHMAP['X'] ])
                THEN
                   BEGIN
                      FORMATOPTION := CHARACTER;
                      CASE FORMATOPTION OF
                         'A', 'E', 'I', 'G', 'P', 'L', 'S':
                            BEGIN
                               READIN(1, SPECIFICATION);
                               IF (SPECIFICATION[1] < WRITECOL2
                                  - WRITECOL1 - 9) OR (
                                  FORMATOPTION = 'P')
                               THEN
                                  CASE FORMATOPTION OF
                                     'A':
                                        DECLARALIGNMENT :=
                                           SPECIFICATION[1];
                                     'E':
                                        IF SPECIFICATION[1] < 4
                                        THEN
                                           BEGIN
                                              PROCEDNAMESWANTED
                                                 :=
                                                 SPECIFICATION[1
                                                 ] > 1;
                                              ENDCOMMENTSWANTED
                                                 := ODD(
                                                 SPECIFICATION[1]
                                                 );
                                           END;
                                     'G':
                                        SYMBOLGAP :=
                                           SPECIFICATION[1];
                                     'I':
                                        INDENTINDEX :=
                                           SPECIFICATION[1];
                                     'L':
                                        LONGLINEINDENT :=
                                           SPECIFICATION[1];
                                     'P':
                                        PROCEDSEPARATION :=
                                           SPECIFICATION[1];
                                     'S':
                                        STATMTSEPARATION :=
                                           SPECIFICATION[1];
                                  END (*CASE*);
                            END (*SINGLE PARAMETERS*);
                         'W', 'R', 'N':
                            BEGIN
                               READIN(2, SPECIFICATION);
                               IF SPECIFICATION[2] <> INVALID
                               THEN
                                  CASE FORMATOPTION OF
                                     'W':
                                        IF (SPECIFICATION[1] > 0
                                           ) AND (SPECIFICATION[
                                           2] < BUFFERSIZE - 2)
                                           AND (SPECIFICATION[2]
                                           - SPECIFICATION[1] >
                                           8)
                                        THEN
                                           BEGIN
                                              WRITECOL1 :=
                                                 SPECIFICATION[1
                                                 ];
                                              WRITECOL2 :=
                                                 SPECIFICATION[2
                                                 ];
                                           END;
                                     'R':
                                        IF (SPECIFICATION[1] > 0
                                           ) AND (SPECIFICATION[
                                           2] - SPECIFICATION[1]
                                           > 8)
                                        THEN
                                           BEGIN
                                              READCOL1 :=
                                                 SPECIFICATION[1
                                                 ];
                                              READCOL2 :=
                                                 SPECIFICATION[2
                                                 ];
                                           END;
                                     'N':
                                        BEGIN
                                           LINENUMBER :=
                                              SPECIFICATION[1];
                                           INCREMENT :=
                                              SPECIFICATION[2];
"SZH"                                     WHILE NOT ( CHMAP[CHARACTER]
"SZH"                                           IN ([CHMAP['+'], CHMAP['-'] ]+
                                              ENDDIRECTV)) DO
                                              COPYACHARACTER;
                                           IF CHARACTER = '-'
                                           THEN
                                              INCREMENT := -
                                                 INCREMENT
                                           ELSE
                                              IF WRITECOL1 < 3
                                              THEN
                                                 WRITECOL1 := 3;
                                        END;
                                  END (*CASE*);
                            END (*DOUBLE PARAMETERS*);
                         'B', 'C', 'D', 'F', 'X':
                            BEGIN
                               REPEAT COPYACHARACTER;
"SZH"                         UNTIL CHMAP[CHARACTER] IN
"SZH"                               ([ CHMAP['+'], CHMAP['-'] ] +
                                     ENDDIRECTV);
                               IF (CHARACTER = '+') OR (CHARACTER = '-')
                               THEN
                                  CASE FORMATOPTION OF
                                     'B':
                                        PACKERISOFF := CHARACTER
                                           = '-';
                                     'C':
                                        IF DISPLAYISON THEN
                                           COMPRESSWANTED :=
                                              CHARACTER = '+';
                                     'D':
                                        BEGIN
                                           PREVDISPLAY :=
                                              DISPLAYWANTED;
                                           DISPLAYWANTED :=
                                              CHARACTER = '+';
                                           IF PREVDISPLAY AND
                                              NOT DISPLAYWANTED
                                           THEN
                                              BEGIN
                                                 WRITEA(
                                                    TERMINATOR);
                                                 IF TERMINATOR =
                                                    '*'
                                                 THEN
                                                   IF
                                                   PROGISPASCAL2
                                                   THEN
                                                   WRITEA('(')
                                                   ELSE
                                                   WRITEA('/');
                                                 SAVEDCOMPRESS
                                                    :=
                                                  COMPRESSWANTED
                                                 ;
                                                 COMPRESSWANTED
                                                    := FALSE;
                                              END
                                           ELSE
                                              IF NOT PREVDISPLAY
                                                 AND
                                                 DISPLAYWANTED
                                              THEN
                                                 BEGIN

                                           STARTNEWLINEANDINDENT
                                                    ;
                                                   IF TERMINATOR
                                                      = '*'
                                                   THEN
                                                   BEGIN
                                                   IF PROGISPASCAL2
                                                   THEN
                                                   WRITEA('(')
                                                   ELSE
                                                   WRITEA('/');
                                                   WRITEA('*');
                                                   END
                                                   ELSE
                                                   WRITEA('_');

                                                  COMPRESSWANTED
                                                      :=
                                                   SAVEDCOMPRESS
                                                    ;
                                                 END (*IF NOT PR
                                                 EV*);
                                        END (* 'D': *);
                                     'F':
                                        BEGIN
                                           PREVDISPLAY :=
                                              NOFORMATTING;
                                           NOFORMATTING :=
                                              CHARACTER = '-';
                                           IF PREVDISPLAY AND
                                              NOT NOFORMATTING
                                           THEN READACHARACTER;
                                           IF NOT PREVDISPLAY
                                              AND NOFORMATTING
                                           THEN WRITEA('-');
                                        END;
                                     'X':
                                        CROSSREFWANTED :=
                                           CHARACTER = '+';
                                  END (*CASE*);
                               DISPLAYISON := DISPLAYWANTED AND
                                  NOT NOFORMATTING;
                            END (*BOOLEAN PARAMETERS*);
                      END (*CASE*)
                   END (*THEN*)
                ELSE
"SZH"              IF NOT (CHMAP[CHARACTER] IN ENDDIRECTV)
                   THEN COPYACHARACTER;
"SZH"        UNTIL CHMAP[CHARACTER] IN ENDDIRECTV;
             IF CHARACTER = ']'   THEN COPYACHARACTER;
          END (*FORMATTERDIRECTIVES*);

       BEGIN (*DOCOMMENT*)
          IF READINGFORMATOPTIONS   THEN FORMATTERDIRECTIVES
          ELSE
             BEGIN
                IF PROGISPASCAL2   THEN SECONDTERMINATOR := ')'
                ELSE SECONDTERMINATOR := '/';
                IF LASTSYMBOL IN [COMMENT, SEMICOLON] THEN
                   BEGIN
                      LEFTMARGIN := 0;   STARTNEWLINEANDINDENT;
                      LEFTMARGIN := ACTUALLEFTMARGIN;
                   END;
                WRITESYMBOL;
                IF CHARACTER = '$'   THEN COMPILERDIRECTIVES;
                IF CHARACTER = '['   THEN FORMATTERDIRECTIVES;
                SAVEDSYMBGP := SYMBOLGAP;   SYMBOLGAP := 0;
                REPEAT

                   WHILE CHARACTER <> TERMINATOR DO
                      BEGIN  COPYACHARACTER;
                      END (*WHILE*);

                   READSYMBOL;   WRITESYMBOL;
                UNTIL SYMBOLNAME = COMMENT;
                SYMBOLGAP := SAVEDSYMBGP;   READSYMBOL;
             END;
       END (*DOCOMMENT*);

    PROCEDURE CHECKFOR(SECONDCHAR: CHAR; TWOCHARSYMBOL: SYMBOLS;
       READALLOWED: BOOLEAN);

       BEGIN
          IF READALLOWED THEN
             BEGIN
                LENGTH := 1;   SYMBOL[1] := CHARACTER;
                SYMBOLNAME := NAMEOF[CHARACTER];
                READACHARACTER;
             END;
          IF CHARACTER = SECONDCHAR THEN
             BEGIN
                SYMBOL[2] := CHARACTER;   LENGTH := 2;
                SYMBOLNAME := TWOCHARSYMBOL;   READACHARACTER;
             END;
       END (*CHECKFOR*);

    BEGIN (*READSYMBOL*)
"SZH"
"SZH"  IF CHARACTER = '#' THEN CHARACTER := ' '
"SZH"  ELSE
"SZH"     IF CHARACTER = '"' THEN
"SZH"         IF FIRSTDQUOTE THEN
"SZH"            BEGIN CHARACTER := 'ò';  FIRSTDQUOTE := FALSE END
"SZH"         ELSE  BEGIN  CHARACTER := '¯';  FIRSTDQUOTE := TRUE  END;
"SZH"
       IF READINGFORMATOPTIONS   THEN DOCOMMENT('*')
       ELSE
          CASE TYPEOF[CHARACTER] OF
             STAR:
                IF PROGISPASCAL2
                THEN CHECKFOR(')', COMMENT, READNEXTCH)
                ELSE CHECKFOR('/', COMMENT, READNEXTCH);
             RIGHTARROW:
                BEGIN
                   SYMBOLNAME := COMMENT;   SYMBOL[1] := '¯';
                   LENGTH := 1;   READACHARACTER;
                   DOCOMMENT('ò');
                END;
"SZH"    (*  DQUOTE:
"SZH"           BEGIN
"SZH"              SYMBOLNAME := COMMENT;   SYMBOL[1] := '"';
"SZH"              LENGTH := 1;   READACHARACTER;
"SZH"              DOCOMMENT('"');
"SZH"           END;   *)
             SLASH:
                BEGIN
                   CHECKFOR('*', COMMENT, READNEXTCH);
                   IF (SYMBOLNAME = COMMENT) THEN
                      BEGIN
                         PROGISPASCAL2 := FALSE;
                         DOCOMMENT('*')
                      END;
                END;
             LFTPAREN:
                BEGIN
                   CHECKFOR('*', COMMENT, READNEXTCH);
                   IF (SYMBOLNAME = COMMENT) THEN
                      BEGIN
                         PROGISPASCAL2 := TRUE;
                         DOCOMMENT('*')
                      END;
                END;
             ALPHANUMERIC:
                BEGIN
"SZH"              SYMBOLISNUMBER := CHMAP[CHARACTER] IN DIGITS;
                   CHARNUMBER := 1;
                   REPEAT
                      SYMBOL[CHARNUMBER] := CHARACTER;
                      READACHARACTER;
                      CHARNUMBER := CHARNUMBER + 1
                   UNTIL NOT (TYPEOF[CHARACTER] = ALPHANUMERIC);
                   IF SYMBOLISNUMBER AND (SYMBOL[CHARNUMBER - 1]
                      = 'E')
                   THEN
                      REPEAT
                         SYMBOL[CHARNUMBER] := CHARACTER;
                         READACHARACTER;
                         CHARNUMBER := CHARNUMBER + 1;
"SZH"                 UNTIL NOT (CHMAP[CHARACTER] IN DIGITS);
                   LENGTH := CHARNUMBER - 1;
                   IF SYMBOLISNUMBR
                   THEN SYMBOLNAME := IDENTIFIER
                   ELSE
                      BEGIN
                         FOR CHARNUMBER := CHARNUMBER TO 10 DO
                            SYMBOL[CHARNUMBER] := ' ';
"""                      PACK(SYMBOL, 1, TESTSYMBOL);   """  I := 1;
"SZH"                    FOR PQ := 1 TO ALFLEN DO TESTSYMBOL[PQ] := SYMBOL[PQ];
                         PASCALSYMBOL[LASTPSYMBOLNAME] :=
                            TESTSYMBOL;
                         WHILE TESTSYMBOL <> PASCALSYMBOL[I] DO
                            I := I + 1;
                         SYMBOLNAME := PSYMBOLNAME[I];
                      END (*ELSE*);
                END (*ALPHANUMERIC*);
             BLANK, ENDOFLINE:
                BEGIN
                   REPEAT READACHARACTER
                   UNTIL NOT (TYPEOF[CHARACTER] IN [BLANK,
                      ENDOFLINE]);
                   READSYMBOL
                END;
             GREATERTHAN, COLON:
                CHECKFOR('=', OTHERSYMBOL, READNEXTCH);
             LESSTHAN:
                BEGIN
                   CHECKFOR('=', OTHERSYMBOL, READNEXTCH);
                   IF SYMBOLNAME <> OTHERSYMBOL THEN
                      CHECKFOR('>', OTHERSYMBOL, DONTREADNEXTCH)
                   ;
                END;
             PERIOD:
                IF LASTSYMBOL <> ENDSYMBOL
                THEN CHECKFOR('.', RANGE, READNEXTCH)
                ELSE SYMBOLNAME := PERIODSYMBOL;
             STRING:
                BEGIN
                   CHARNUMBER := 1;
                   REPEAT
                      REPEAT
                         SYMBOL[CHARNUMBER] := CHARACTER;
                         CHARNUMBER := CHARNUMBER + 1;
                         READACHARACTER;
                      UNTIL CHARACTER = '''';
                      SYMBOL[CHARNUMBER] := CHARACTER;
                      CHARNUMBER := CHARNUMBER + 1;
                      READACHARACTER;
                   UNTIL CHARACTER <> '''';
                   LENGTH := CHARNUMBER - 1;
                   SYMBOLNAME := OTHERSYMBOL;
                END (*STRING*);
             OTHER:
                BEGIN
                   SYMBOL[1] := CHARACTER;
                   SYMBOLNAME := NAMEOF[CHARACTER];
                   LENGTH := 1;   READACHARACTER;
                END;
          END (*CASE*);
    END (*READSYMBOL*);

 PROCEDURE CHANGEMARGINTO(NEWLEFTMARGIN: MARGINS);

    BEGIN
       ACTUALLEFTMARGIN := NEWLEFTMARGIN;
       LEFTMARGIN := NEWLEFTMARGIN;
       IF LEFTMARGIN < 0   THEN LEFTMARGIN := 0
       ELSE
          IF LEFTMARGIN > WRITECOL2 - WRITECOL1 - 9 -
             LONGLINEINDENT
          THEN
             LEFTMARGIN := WRITECOL2 - WRITECOL1 - 9 -
                LONGLINEINDENT;
    END (*CHANGEMARGINTO*);

 PROCEDURE DODECLARATIONUNTIL(ENDDECLARATION: SYMBOLSET);

    PROCEDURE DOPARENTHESES;

       VAR
          SAVEDLGLNID: OPTIONSIZE;

       BEGIN
          SAVEDLGLNID := LONGLINEINDENT;
          IF DECLARALIGNMENT > 0
          THEN
             BEGIN
                LONGLINEINDENT := WRITECOLUMN + SYMBOLGAP + 1 -
                   LEFTMARGIN - WRITECOL1;
                REPEAT WRITESYMBOL;   READSYMBOL;
                UNTIL SYMBOLNAME = RIGHTPARENTH;
                WRITESYMBOL;   READSYMBOL;
             END
          ELSE
             BEGIN
                LONGLINEINDENT := 1;
                CHANGEMARGINTO(ACTUALLEFTMARGIN + INDENTINDEX);
                STARTNEWLINEANDINDENT;
                REPEAT WRITESYMBOL;   READSYMBOL
                UNTIL SYMBOLNAME = RIGHTPARENTH;
                WRITESYMBOL;   READSYMBOL;
                CHANGEMARGINTO(ACTUALLEFTMARGIN - INDENTINDEX);
             END (*ELSE*);
          LONGLINEINDENT := SAVEDLGLNID;
       END (*DOPARENTHESES*);

    PROCEDURE DOFIELDLISTUNTIL(ENDFIELDLIST: SYMBOLSET);

       VAR
          LASTEOL: MARGINS;
          ALIGNCOLUMN: WIDTH;

       PROCEDURE DORECORD;

          VAR
             SAVEDLEFTMARGIN: WIDTH;

          BEGIN
             SAVEDLEFTMARGIN := ACTUALLEFTMARGIN;   WRITESYMBOL;
             READSYMBOL;
             CHANGEMARGINTO(WRITECOLUMN - 6 + INDENTINDEX -
                WRITECOL1);
             STARTNEWLINEANDINDENT;
             DOFIELDLISTUNTIL([ENDSYMBOL]);
             CHANGEMARGINTO(ACTUALLEFTMARGIN - INDENTINDEX);
             STARTNEWLINEANDINDENT;   WRITESYMBOL;   READSYMBOL;
             CHANGEMARGINTO(SAVEDLEFTMARGIN);
          END (*DORECORD*);

       PROCEDURE DOVARIANTRECORDPART;

          VAR
             SAVEDLEFTMARGIN, OTHERSAVEDMARGIN: MARGINS;

          BEGIN
             OTHERSAVEDMARGIN := ACTUALLEFTMARGIN;
             IF DECLARALIGNMENT > 0
             THEN
                BEGIN
                   REPEAT WRITESYMBOL;   READSYMBOL;
                   UNTIL SYMBOLNAME = COLONSYMBOL;
                   WRITESYMBOL;   READSYMBOL;
                   WITH UNWRITTEN[LASTEOL] DO
                      BEGIN
                         INDENTAFTEREOL := INDENTAFTEREOL +
                            ALIGNCOLUMN - WRITECOLUMN;
                         IF INDENTAFTEREOL < 0
                         THEN INDENTAFTEREOL := 0;
                      END;
                   WRITECOLUMN := ALIGNCOLUMN;
                   CHANGEMARGINTO(ACTUALLEFTMARGIN + ALIGNCOLUMN -
                      WRITECOLUMN);
                END;
             REPEAT WRITESYMBOL;   READSYMBOL;
             UNTIL SYMBOLNAME = OFSYMBOL;
             CHANGEMARGINTO(ACTUALLEFTMARGIN + INDENTINDEX);
             REPEAT
                WRITESYMBOL;   READSYMBOL;
                IF SYMBOLNAME <> ENDSYMBOL
                THEN
                   BEGIN
                      STARTNEWLINEANDINDENT;
                      REPEAT WRITESYMBOL;   READSYMBOL;
                      UNTIL SYMBOLNAME IN [LEFTPAREN, SEMICOLON
                         , ENDSYMBOL];
                      IF SYMBOLNAME = LEFTPAREN
                      THEN
                         BEGIN
                            WRITESYMBOL;   READSYMBOL;
                            SAVEDLEFTMARGIN := ACTUALLEFTMARGIN;
                            CHANGEMARGINTO(WRITECOLUMN -
                               WRITECOL1);
                            DOFIELDLISTUNTIL([RIGHTPARENTH]);
                            WRITESYMBOL;   READSYMBOL;
                            CHANGEMARGINTO(SAVEDLEFTMARGIN);
                         END;
                   END;
             UNTIL SYMBOLNAME <> SEMICOLON;
             CHANGEMARGINTO(OTHERSAVEDMARGIN);
          END (*DOVARIANTRECORDPART*);

       BEGIN (*DOFIELDLISTUNTIL*)
          LASTEOL := OLDEST;
          IF LASTSYMBOL = LEFTPAREN THEN
             FOR I := 1 TO DECLARALIGNMENT - LENGTH DO WRITEA(' ');
          ALIGNCOLUMN := LEFTMARGIN + WRITECOL1 + DECLARALIGNMENT +
             1;
          WHILE NOT (SYMBOLNAME IN ENDFIELDLIST) DO
             BEGIN
                IF LASTSYMBOL IN [SEMICOLON, COMMENT] THEN
                   IF SYMBOLNAME <> SEMICOLON THEN
                      BEGIN
                         STARTNEWLINEANDINDENT;
                         LASTEOL := OLDEST
                      END;
                IF SYMBOLNAME IN [RECORDSYMBOL, CASESYMBOL,
                   LEFTPAREN, COMMASYMBOL, COLONSYMBOL,
                   EQUALSYMBOL]
                THEN
                   CASE SYMBOLNAME OF
                      RECORDSYMBOL: DORECORD;
                      CASESYMBOL: DOVARIANTRECORDPART;
                      LEFTPAREN: DOPARENTHESES;
                      COMMASYMBOL, COLONSYMBOL, EQUALSYMBOL:
                         BEGIN
                            WRITESYMBOL;
                            IF DECLARALIGNMENT > 0
                            THEN
                               IF ENDFIELDLIST <> ENDLABEL
                               THEN
                                  BEGIN
                                     WITH UNWRITTEN[LASTEOL] DO
                                        BEGIN
                                           INDENTAFTEREOL :=
                                              INDENTAFTEREOL +
                                              ALIGNCOLUMN -
                                              WRITECOLUMN;
                                           IF INDENTAFTEREOL < 0
                                           THEN
                                              INDENTAFTEREOL :=
                                                 0;
                                           WRITECOLUMN :=
                                              ALIGNCOLUMN;
                                        END;
                                     IF SYMBOLNAME = COMMASYMBOL
                                     THEN
                                        BEGIN
                                           STARTNEWLINEANDINDENT
                                           ;
                                           LASTEOL := OLDEST;
                                        END;
                                  END (*IF DECLARALIGN*);
                            READSYMBOL;
                         END (*  ,   :   = *)
                   END (*CASE*)
                ELSE BEGIN WRITESYMBOL;   READSYMBOL END;
             END (*WHILE*);
       END (*DOFIELDLISTUNTIL*);

    BEGIN (*DODECLARATIONUNTIL*)
       STARTNEWLINEANDINDENT;   WRITESYMBOL;
       CHANGEMARGINTO(ACTUALLEFTMARGIN + INDENTINDEX);
       STARTNEWLINEANDINDENT;   READSYMBOL;
       DOFIELDLISTUNTIL(ENDDECLARATION);
       STARTNEWLINEANDINDENT;
       CHANGEMARGINTO(ACTUALLEFTMARGIN - INDENTINDEX);
    END (*DOTYPES*);

 PROCEDURE BLOCK(BLOCKNAME: COMMENTTEXT; BLOCKNMLENGTH: WIDTH);

    VAR
       I: WIDTH;
       IFTHENCOMPRESSNEEDED: BOOLEAN;
       ATPROCEDBEGINNING: BOOLEAN;

    PROCEDURE PROCEDURES;

       VAR
          I: 0 .. 20;
          PROCEDNAME: COMMENTTEXT;
          PROCEDNMLENGTH: WIDTH;

       BEGIN
          FOR I := 2 TO PROCEDSEPARATION DO
             STARTNEWLINEANDINDENT;
          STARTNEWLINEANDINDENT;   WRITESYMBOL;   READSYMBOL;
          FOR I := 0 TO(LENGTH - 1) DIV 10 DO
"""          PACK(SYMBOL, I * 10 + 1, PROCEDNAME[I + 1]); """
"SZH"        FOR PQ := 1 TO ALFLEN DO PROCEDNAME[I+1,PQ] := SYMBOL[I*10+PQ];
          PROCEDNMLENGTH := LENGTH;   WRITESYMBOL;   READSYMBOL;
          IF SYMBOLNAME = LEFTPAREN THEN
             BEGIN
                WRITESYMBOL;
                REPEAT READSYMBOL;   WRITESYMBOL
                UNTIL SYMBOLNAME = RIGHTPARENTH;
                READSYMBOL;
             END;
          IF SYMBOLNAME = COLONSYMBOL THEN
             REPEAT WRITESYMBOL;   READSYMBOL;
             UNTIL SYMBOLNAME = SEMICOLON;
          WRITESYMBOL;   READSYMBOL;
          CHANGEMARGINTO(ACTUALLEFTMARGIN + INDENTINDEX);
          STARTNEWLINEANDINDENT;   LASTPROGPARTWASBODY := FALSE;
          BLOCK(PROCEDNAME, PROCEDNMLENGTH);
          LASTPROGPARTWASBODY := TRUE;
          CHANGEMARGINTO(ACTUALLEFTMARGIN - INDENTINDEX);
          WRITESYMBOL;   READSYMBOL;
 (* WRITE ";"*)
          STARTNEWLINEANDINDENT;
       END (*PROCEDURES*);

    PROCEDURE DOSTATEMENT(VAR ADDEDBLANKS: WIDTH; STATMTSYMBOL:
       COMMENTTEXT; STMTSYMLENGTH: WIDTH);

       VAR
          I: WIDTH;
          STATMTBEGINNING: INTEGER;
          STATMTPART: ARRAY [1 .. 4] OF INTEGER;
          BLKSONCURRNTLINE, BLKSADDEDBYTHISSTMT: INTEGER;
          SUCCESSFUL: BOOLEAN;

       PROCEDURE COMPRESS(BEGINNING, BREAKPT, ENDING: INTEGER;
          STATMTSEPARATION: OPTIONSIZE);

          BEGIN
             IF COMPRESSWANTED OR IFTHENCOMPRESSNEEDED
             THEN
                BEGIN
                   IF STATMTSEPARATION < 1
                   THEN STATMTSEPARATION := 1;
                   BLKSONCURRNTLINE := BLKSONCURRNTLINE +
                      STATMTSEPARATION - 1;
                   SUCCESSFUL := ((ENDING - BEGINNING +
                      BLKSONCURRNTLINE + UNWRITTEN[BEGINNING MOD
                      BUFFERSIZE].INDENTAFTEREOL) < WRITECOL2)
                      AND (CHARCOUNT - BEGINNING < BUFFERSIZE);
                   IF SUCCESSFUL THEN
                      BEGIN
                         BLKSADDEDBYTHISST := BLKSADDEDBYTHISST +
                            STATMTSEPARATION - 1;
                         UNWRITTEN[BREAKPT MOD BUFFERSIZE].
                            INDENTAFTEREOL := - STATMTSEPARATION
                         ;
                      END;
                END;
          END (*COMPRESS*);

       PROCEDURE WRITECOMMENT;

          VAR
             I: 0 .. BUFFSZDIV10;
             SAVEDLENGTH: WIDTH;
             SAVEDSYMBOLNAME: SYMBOLS;
             SAVEDCHARS: SYMBOLSTRING;

          BEGIN
             SAVEDSYMBOLNAME := SYMBOLNAME;
             FOR I := 1 TO LENGTH DO SAVEDCHARS[I] := SYMBOL[I];
             SAVEDLENGTH := LENGTH;   SYMBOLNAME := OTHERSYMBOL;
             IF PROGISPASCAL2   THEN SYMBOL[1] := '('
             ELSE SYMBOL[1] := '/';
             SYMBOL[2] := '*';   LENGTH := 2;   WRITESYMBOL;
             FOR I := 0 TO(STMTSYMLENGTH - 1) DIV 10 DO
            """ UNPACK(STATMTSYMBOL[I + 1], SYMBOL, (I * 10 + 1)
                   );"""
                FOR PQ := 1 TO ALFLEN DO  SYMBOL[I*10+PQ]:=STATMTSYMBOL[I+1,PQ];
             LENGTH := STMTSYMLENGTH;
             SYMBOLNAME := PERIODSYMBOL;
             LASTSYMBOL := PERIODSYMBOL;   WRITESYMBOL;
             SYMBOL[1] := '*';
             IF PROGISPASCAL2   THEN SYMBOL[2] := ')'
             ELSE SYMBOL[2] := '/';
             LENGTH := 2;   WRITESYMBOL;
             SYMBOLNAME := SAVEDSYMBOLNAME;
             LENGTH := SAVEDLENGTH;
             FOR I := 1 TO LENGTH DO SYMBOL[I] := SAVEDCHARS[I];
          END (*WRITECOMMENT*);

       PROCEDURE DOSTATMTLIST(ENDLIST: SYMBOLS);

          VAR
             BLKSAFTERPRT2: WIDTH;
             ATPROCEDEND: BOOLEAN;

          BEGIN
             ATPROCEDEND := ATPROCEDBEGINNING;   WRITESYMBOL;
             READSYMBOL;   STATMTPART[1] := CHARCOUNT + 1;
             STATMTPART[2] := STATMTPART[1];
             IF SYMBOLNAME <> ENDLIST
             THEN
                BEGIN
                   IF PROCEDNAMESWANTED THEN
                      IF ATPROCEDBEGINNING THEN
                         IF LASTPROGPARTWASBODY THEN
                            IF LASTSYMBOL = BEGINSYMBOL
                            THEN WRITECOMMENT;
                   ATPROCEDBEGINNING := FALSE;
                   DOSTATEMENT(ADDEDBLANKS, STATMTSYMBOL,
                      STMTSYMLENGTH);
                   BLKSAFTERPRT2 := ADDEDBLANKS;
                   BLKSADDEDBYTHISSTMT := BLKSADDEDBYTHISSTMT +
                      ADDEDBLANKS;
                   WHILE SYMBOLNAME <> ENDLIST DO
                      BEGIN
                         WRITESYMBOL;   READSYMBOL;
                         IF SYMBOLNAME <> ENDLIST
                         THEN
                            BEGIN
                               STATMTPART[3] := CHARCOUNT + 1;
                               DOSTATEMENT(ADDEDBLANKS,
                                  STATMTSYMBOL, STMTSYMLENGTH);
                               BLKSONCURRNTLINE := ADDEDBLANKS +
                                  BLKSAFTERPRT2;
                               BLKSADDEDBYTHSTMT :=
                                  BLKSADDEDBYTHSTMT +
                                  ADDEDBLANKS;
                               COMPRESS(STATMTPART[2],
                                  STATMTPART[3], CHARCOUNT,
                                  STATMTSEPARATION);
                               IF NOT SUCCESSFUL
                               THEN
                                  BEGIN
                                     BLKSAFTERPRT2 :=
                                        ADDEDBLANKS;
                                     STATMTPART[2] := STATMTPART
                                        [3];
                                  END
                               ELSE
                                  BLKSAFTERPRT2 :=
                                     BLKSONCURRNTLINE;
                            END;
                      END (*WHILE SYMBOLNAME <> ENDLIST*);
                END (*IF SYMBOLNAME <> ENDLIST*);
             BLKSONCURRNTLINE := BLKSADDEDBYTHISSTMT;
             COMPRESS(STATMTBEGINNING, STATMTPART[1], CHARCOUNT,
                SYMBOLGAP);
             STARTNEWLINEANDINDENT;
             STATMTPART[1] := CHARCOUNT;
             REPEAT WRITESYMBOL;   READSYMBOL;
             UNTIL SYMBOLNAME IN [SEMICOLON, UNTILSYMBOL,
                ENDSYMBOL, ELSESYMBOL, PERIODSYMBOL];
             IF SUCCESSFUL
             THEN
                BEGIN
                   IF ENDLIST = UNTILSYMBOL
                   THEN STATMTPART[4] := STATMTSEPARATION
                   ELSE STATMTPART[4] := SYMBOLGAP;
                   COMPRESS(STATMTBEGINNING, STATMTPART[1],
                      CHARCOUNT, STATMTPART[4]);
                END (*IF SUCCESSFUL*);
             IF NOT (SUCCESSFUL AND COMPRESSWANTED)
             THEN
                IF ENDLIST = ENDSYMBOL THEN
                   IF LASTSYMBOL = ENDSYMBOL THEN
                      IF ATPROCEDEND AND PROCEDNAMESWANTED
                      THEN WRITECOMMENT
                      ELSE
                         IF ENDCOMMENTSWANTED
                         THEN WRITECOMMENT;
          END (*DOSTATMTLIST*);

       BEGIN (*DOSTATEMENT*)
          BLKSONCURRNTLINE := 0;
          BLKSADDEDBYTHISST := 0;
          CHANGEMARGINTO(ACTUALLEFTMARGIN + INDENTINDEX);
          STARTNEWLINEANDINDENT;   STATMTBEGINNING := CHARCOUNT;
          IF SYMBOLISNUMBER
          THEN
             BEGIN
                WITH UNWRITTEN[OLDEST] DO
                   BEGIN
                      INDENTAFTEREOL := INDENTAFTEREOL - 1 -
                         LENGTH - SYMBOLGAP;
                      IF INDENTAFTEREOL < 0
                      THEN INDENTAFTEREOL := 0;
                   END;
                WRITESYMBOL;   READSYMBOL (*WRITE LABEL*);
                WRITESYMBOL;   READSYMBOL (*WRITE COLON*);
             END;
          CASE STATEMENTTYPEOF[SYMBOLNAME] OF
             FORWITHWHILESTATEMENT:
                BEGIN
"""                PACK(SYMBOL, 1, STATMTSYMBOL[1]);    """
"SZH"              FOR PQ := 1 TO ALFLEN DO STATMTSYMBOL[1,PQ] := SYMBOL[PQ];
                   STMTSYMLENGTH := LENGTH;
                   REPEAT WRITESYMBOL;   READSYMBOL
                   UNTIL SYMBOLNAME = DOSYMBOL;
                   WRITESYMBOL;   READSYMBOL;
                   STATMTPART[1] := CHARCOUNT + 1;
                   DOSTATEMENT(ADDEDBLANKS, STATMTSYMBOL,
                      STMTSYMLENGTH);
                   BLKSONCURRNTLINE := BLKSONCURRNTLINE +
                      ADDEDBLANKS;
                   BLKSADDEDBYTHISSTMT := BLKSADDEDBYTHISSTMT +
                      ADDEDBLANKS;
                   COMPRESS(STATMTBEGINNING, STATMTPART[1],
                      CHARCOUNT, SYMBOLGAP);
                END;
             REPEATSTATEMENT: DOSTATMTLIST(UNTILSYMBOL);
             IFSTATEMENT:
                BEGIN
"""                PACK(SYMBOL, 1, STATMTSYMBOL[1]);   """
"SZH"              FOR PQ := 1 TO ALFLEN DO STATMTSYMBOL[1,PQ] := SYMBOL[PQ];
                   STMTSYMLENGTH := LENGTH;
                   REPEAT WRITESYMBOL;   READSYMBOL
                   UNTIL SYMBOLNAME = THENSYMBOL;
                   STARTNEWLINEANDINDENT;
                   STATMTPART[1] := CHARCOUNT;   WRITESYMBOL;
                   READSYMBOL;   STATMTPART[2] := CHARCOUNT + 1;
                   DOSTATEMENT(ADDEDBLANKS, STATMTSYMBOL,
                      STMTSYMLENGTH);
                   BLKSONCURRNTLINE := ADDEDBLANKS;
                   BLKSADDEDBYTHISSTMT := ADDEDBLANKS;
                   COMPRESS(STATMTPART[1], STATMTPART[2],
                      CHARCOUNT, SYMBOLGAP);
                   IF SUCCESSFUL
                   THEN
                      COMPRESS(STATMTBEGINNING, STATMTPART[1],
                         CHARCOUNT, STATMTSEPARATION)
                   ELSE IFTHENCOMPRESSNEEDED := TRUE;
                   IF SYMBOLNAME = ELSESYMBOL
                   THEN
                      BEGIN
"""                      PACK(SYMBOL, 1, STATMTSYMBOL[1]);          """
"SZH"                    FOR PQ := 1 TO ALFLEN DO
"SZH"                        STATMTSYMBOL[1,PQ] := SYMBOL[PQ];
                         STMTSYMLENGTH := LENGTH;
                         IFTHENCOMPRESSNEEDED := FALSE;
                         STARTNEWLINEANDINDENT;
                         STATMTPART[3] := CHARCOUNT;
                         WRITESYMBOL;   READSYMBOL;
                         STATMTPART[4] := CHARCOUNT + 1;
                         DOSTATEMENT(ADDEDBLANKS, STATMTSYMBOL,
                            STMTSYMLENGTH);
                         BLKSONCURRNTLINE := ADDEDBLANKS;
                         BLKSADDEDBYTHISSTMT :=
                            BLKSADDEDBYTHISSTMT + ADDEDBLANKS;
                         COMPRESS(STATMTPART[3], STATMTPART[4],
                            CHARCOUNT, SYMBOLGAP);
                         BLKSONCURRNTLINE := BLKSADDEDBYTHISSTMT
                         ;
                         IF SUCCESSFUL THEN
                            COMPRESS(STATMTBEGINNING, STATMTPART[3],
                               CHARCOUNT, STATMTSEPARATION);
                      END
                   ELSE
                      IF (CHARCOUNT - STATMTBEGINNING) <
                         BUFFERSIZE
                      THEN
                         BEGIN
                            COMPRESSWANTED := NOT COMPRESSWANTED
                            ;
                            BLKSONCURRNTLINE := 0;
                            COMPRESS(STATMTBEGINNING, STATMTPART
                               [1], STATMTPART[2], SYMBOLGAP);
                            COMPRESSWANTED := NOT COMPRESSWANTED
                            ;
                         END;
                   IFTHENCOMPRESSNEEDED := FALSE;
                END (*IFSTATEMENT*);
             CASESTATEMENT:
                BEGIN
                   REPEAT WRITESYMBOL;   READSYMBOL
                   UNTIL SYMBOLNAME = OFSYMBOL;
                   WRITESYMBOL;   READSYMBOL;
                   CHANGEMARGINTO(ACTUALLEFTMARGIN + INDENTINDEX
                      );
                   WHILE SYMBOLNAME <> ENDSYMBOL DO
                      BEGIN
                         STARTNEWLINEANDINDENT;
                         STATMTPART[1] := CHARCOUNT;
                         FOR I := 0 TO(LENGTH - 1) DIV 10 DO
"""                         PACK(SYMBOL, (I * 10 + 1), STATMTSYMBOL[I + 1]);"""
"SZH"                       FOR PQ := 1 TO ALFLEN DO
                              STATMTSYMBOL[I+1, PQ] := SYMBOL[I*10 +PQ];
                         STMTSYMLENGTH := LENGTH;
                         REPEAT WRITESYMBOL;   READSYMBOL
                         UNTIL SYMBOLNAME = COLONSYMBOL;
                         WRITESYMBOL;   READSYMBOL;
 (*WRITE COLON*)
                         IF NOT (SYMBOLNAME IN [SEMICOLON,
                            ENDSYMBOL])
                         THEN
                            BEGIN
                               STATMTPART[2] := CHARCOUNT + 1;
                               DOSTATEMENT(ADDEDBLANKS,
                                  STATMTSYMBOL, STMTSYMLENGTH);
                               BLKSONCURRNTLINE := ADDEDBLANKS;
                               BLKSADDEDBYTHISSTMT :=
                                  BLKSADDEDBYTHISSTMT +
                                  ADDEDBLANKS;
                               COMPRESS(STATMTPART[1],
                                  STATMTPART[2], CHARCOUNT,
                                  SYMBOLGAP);
                            END (*IF NOT(SYMBOLNAME...)*);
                         IF SYMBOLNAME = SEMICOLON THEN
                            BEGIN WRITESYMBOL;   READSYMBOL;
                            END;
                      END;
                   CHANGEMARGINTO(ACTUALLEFTMARGIN - INDENTINDEX
                      );
                   STARTNEWLINEANDINDENT;   WRITESYMBOL;
                   READSYMBOL;
                   IF ENDCOMMENTSWANTED AND (LASTSYMBOL =
                      ENDSYMBOL)
                   THEN
                      BEGIN
                         STATMTSYMBOL[1] := 'CASE      ';
                         STMTSYMLENGTH := 4;   WRITECOMMENT;
                      END;
                END (*CASESTATEMENT*);
             OTHERSTATEMENT:
                BEGIN
                   WHILE NOT (SYMBOLNAME IN [SEMICOLON,
                      UNTILSYMBOL, ENDSYMBOL, ELSESYMBOL]) DO
                      BEGIN WRITESYMBOL;   READSYMBOL END;
                END (*OTHER*);
             COMPOUNDSTATEMENT: DOSTATMTLIST(ENDSYMBOL);
          END (*CASE*);
          ADDEDBLANKS := BLKSADDEDBYTHISSTMT;
          CHANGEMARGINTO(ACTUALLEFTMARGIN - INDENTINDEX);
       END (*DOSTATEMENT*);

    BEGIN (*BLOCK*)
       LASTPROGPARTWASBODY := LASTPROGPARTWASBODY AND (
          SYMBOLNAME = BEGINSYMBOL);
       IF SYMBOLNAME = LABELSYMBOL
       THEN DODECLARATIONUNTIL(ENDLABEL);
       IF SYMBOLNAME = CONSTSYMBOL
       THEN DODECLARATIONUNTIL(ENDCONST);
       IF SYMBOLNAME = TYPESYMBOL
       THEN DODECLARATIONUNTIL(ENDTYPE);
       IF SYMBOLNAME = VARSYMBOL
       THEN DODECLARATIONUNTIL(ENDVAR);
       IF SYMBOLNAME = VALUESYMBOL
       THEN DODECLARATIONUNTIL(ENDVALUE);
       WHILE SYMBOLNAME IN [FUNCTIONSYMBOL, PROCEDSYMBOL] DO
          PROCEDURES;
       IF SYMBOLNAME = BEGINSYMBOL
       THEN
          BEGIN
             IF LASTPROGPARTWASBODY THEN
                FOR I := 2 TO PROCEDSEPARATION DO
                   STARTNEWLINEANDINDENT;
             IFTHENCOMPRESSNEEDED := FALSE;
             ATPROCEDBEGINNING := TRUE;
             CHANGEMARGINTO(ACTUALLEFTMARGIN - INDENTINDEX);
             DOSTATEMENT(I, BLOCKNAME, BLOCKNMLENGTH);
 (*I = DUMMY PARAMETER*)
             LASTPROGPARTWASBODY := TRUE;
             CHANGEMARGINTO(ACTUALLEFTMARGIN + INDENTINDEX);
          END
       ELSE
          BEGIN WRITESYMBOL;   READSYMBOL;
 (*WRITE "FORWARD"*)
          END;
    END (*BLOCK*);

 PROCEDURE INITIALIZE;

    VAR
       I: WIDTH;

    BEGIN
"SZH" DIGITS := [ CHMAP['0'], CHMAP['1'], CHMAP['2'], CHMAP['3'],
"SZH"             CHMAP['4'], CHMAP['5'], CHMAP['6'], CHMAP['7'],
"SZH"             CHMAP['8'], CHMAP['9'] ];
       ALPHASYMBOLS := [PROGRAMSYMBOL, BEGINSYMBOL, ENDSYMBOL,
          CONSTSYMBOL, TYPESYMBOL, RECORDSYMBOL, CASESYMBOL,
          IFSYMBOL, THENSYMBOL, ELSESYMBOL, DOSYMBOL, OFSYMBOL,
          FORSYMBOL, WITHSYMBOL, WHILESYMBOL, REPEATSYMBOL,
          UNTILSYMBOL, IDENTIFIER, VARSYMBOL, VALUESYMBOL,
          PROCEDSYMBOL, FUNCTIONSYMBOL, LABELSYMBOL,
          ALPHAOPERATOR];
       ENDLABEL := [CONSTSYMBOL, TYPESYMBOL, VARSYMBOL,
          VALUESYMBOL, PROCEDSYMBOL, FUNCTIONSYMBOL, BEGINSYMBOL
          ];
       ENDCONST := ENDLABEL - [CONSTSYMBOL];
       ENDTYPE := ENDCONST - [TYPESYMBOL];
       ENDVAR := ENDTYPE - [VARSYMBOL];
       ENDVALUE := ENDVAR - [VALUESYMBOL];   WRITECOLUMN := 1;
       LEFTMARGIN := 0;   ACTUALLEFTMARGIN := 0;
       OUTPUTCOL := 1;   READCOL1 := 1;   READCOL2 := 999;
       WRITECOL1 := 1;   WRITECOL2 := 72;   OLDEST := 1;
       CHARCOUNT := 1;   LINENUMBER := 0;   INCREMENT := 0;
       PACKERISOFF := TRUE;   COMPRESSWANTED := FALSE;
       DISPLAYISON := TRUE;   DISPLAYWANTED := TRUE;
       NOFORMATTING := FALSE;   CROSSREFWANTED := FALSE;
       PROCEDNAMESWNTD := TRUE;   ENDCOMMENTSWNTD := FALSE;
       INDENTINDEX := 3;   LONGLINEINDENT := 3;
       PROCEDSEPARATION := 3;   SYMBOLGAP := 1;
       STATMTSEPARATION := 3;   DECLARALIGNMENT := 0;
       READCOLUMN := 0;   LASTSYMBOL := PERIODSYMBOL;
       LASTPROGPARTWASBODY := FALSE;
       READINGFORMATOPTIONS := FALSE;   PROGISPASCAL2 := FALSE;
       RESET(PROG);
    END (*INITIALIZE*);

 PROCEDURE READFORMATOPTIONS;

    BEGIN
       IF NOT EOF (*EOS*) (INPUT) THEN
          BEGIN
             READINGFORMATOPTIONS := TRUE;   READSYMBOL;
             READINGFORMATOPTIONS := FALSE;
          END;
    END (*READFORMATOPTIONS*);

 BEGIN (*MAINPROGRAM*)
"szh" init_chmap;
    (*MESSAGE*)
    WRITELN(OUTPUT, ' >>>> PASCAL PROGRAM FORMATTER VERS. 1/5/76 <<<<');
    (*MESSAGE*)
    WRITELN(OUTPUT, ' -----------------------------------------------');
    WRITELN(OUTPUT);

""" LINELIMIT(OUTPUT, - 1);    """
 (* UNLIMITED OUTPUT IS ALLOWED*)
    CONSTANTSINITIALIZATION;   INITIALIZE;   READFORMATOPTIONS;
    READACHARACTER;   WRITEA(' ');   READSYMBOL;
    PROGISPASCAL2 := SYMBOLNAME = PROGRAMSYMBOL;
    IF PROGISPASCAL2
    THEN
       BEGIN
          STARTNEWLINEANDINDENT;   WRITESYMBOL;   READSYMBOL;
          FOR I := 0 TO(LENGTH - 1) DIV 10 DO
"""          PACK(SYMBOL, (I * 10 + 1), MAIN[I + 1]);      """
"SZH"        FOR PQ := 1 TO ALFLEN DO MAIN[I+1,PQ] := SYMBOL[I*10+PQ];
          MAINNMLENGTH := LENGTH;
          REPEAT WRITESYMBOL;   READSYMBOL;
          UNTIL SYMBOLNAME = SEMICOLON;
          WRITESYMBOL;   READSYMBOL;   STARTNEWLINEANDINDENT;
          PSYMBOLNAME[7] := IDENTIFIER;
          PSYMBOLNAME[31] := IDENTIFIER;
          PSYMBOLNAME[32] := IDENTIFIER;
       END (*IF PROGISPASCAL2*);
    BLOCK(MAIN, MAINNMLENGTH);   WRITEA('.');   WRITEA(' ');
    WITH UNWRITTEN[OLDEST] DO
       BEGIN CHISENDLINE := TRUE;   INDENTAFTEREOL := 0; END;
    WRITECOLUMN := 0;   FOR I := 0 TO 159 DO WRITEA(' ');
    WRITELN(OUTPUT);

    IF SYMBOLNAME <> PERIODSYMBOL
    THEN (*MESSAGE*)  WRITELN(OUTPUT, ' >>>> ERROR(S) IN FORMATTING.');
    (*MESSAGE*)  WRITELN(OUTPUT, ' >>>> END FORMATTING');
 END (*MAINPROGRAM*).
PROGRAM PASCREF(INPUT,OUTPUT);      (*$D- N.WIRTH  2.7.75*)
(*CROSS REFERENCE GENERATOR FOR PASCAL PROGRAMS*)
(*QUADRATIC QUOTIENT HASH METHOD*)
(*MODIFIED SLIGHTLY BY A. MICKEL 75/12/08 AND D. LALIBERTE
  78/03/15 TO PRODUCE PROCEDURE LIST AND SKIP COMPILER TITLE*)
(*$T-,P-,R-,B4 TESTS OFF, PMD OFF, DYNAMIC STORAGE, BIG BUFFERS.*)
LABEL 99;
CONST P = 1499;     (*SIZE OF HASH TABLE*)
  NK = 33;          (*NO. OF KEYWORDS*)
  KLN = 10;         (*KEYLENGTH*)
  LPPG = 55;        (*NO. OF LINES PER PAGE*)
  LLMAX = 132;      (*LINE LENGTH DEFAULT MAX*)
  LLMIN = 72;       (*LINE LENGTH MINIMUM*)
  MAXN = 10000;     (*MAX NO. OF LINES*)
  DGPN =  6;        (*NO. OF DIGITS PER NUMBER*)
  LITL = 3;         (*NUMBER OF LINES IN COMPILER TITLE*)
  ADDRWIDTH = 6;    (*NUMBER OF DIGITS IN CODE ADDRESS*)
  EMPTY = '          ';
  STARS = ' *****';
TYPE INDEX = 0..P;
  ALFA = PACKED ARRAY [1..KLN] OF CHAR;
  REF = @ITEM;
  WORD = RECORD KEY: ALFA;
           FIRST: REF;
         END ;
  ITEM = PACKED RECORD
           LNO: 0..MAXN;
           NEXT: REF
         END ;
  PROCREF = @PROC;  (*PROCEDURE OR FUNCTION REFERENCE*)
  PROC = PACKED RECORD
           NAME: ALFA;
           LNO: 0..MAXN;
           NEXT: PROCREF
         END ;
VAR I: INDEX;
  K: INTEGER;
  M: INTEGER;       (*NO. OF LINES ON PAGE*)
  N: INTEGER;       (*NO. OF LINES INPUT*)
  LN: INTEGER;      (*CURRENT LINE NUMBER*)
  LLNGOUT: INTEGER; (*LINE LENGTH FOR OUTPUT*)
  LLNGIN: INTEGER;  (*LINE LENGTH FOR INPUT*)
  CCOUNT: INTEGER;  (*CHARACTER COUNT IN LINE*)
  NOPL: INTEGER;    (*NO. OF LINE-NUMBERS PER LINE*)
  ID: RECORD CASE BOOLEAN OF
             FALSE: (A: ALFA);
             TRUE:  (ORD: INTEGER)
      END ;
  T: ARRAY [INDEX] OF WORD;    (*HASH TABLE*)
  KEY: ARRAY [1..NK] OF ALFA;
  PROCORFUNC,
  COMPILERLISTING,
  LINENUMBERS: BOOLEAN;
  FIRSTPROC,
  PROCPTR: PROCREF; (*POINTERS TO CHAIN OF PROCEDURES*)


   FUNCTION LETTER(C: CHAR): BOOLEAN;

     BEGIN
     LETTER := (('A' <= C) AND (C <= 'Z')) OR
               (('a' <= C) AND (C <= 'i')) OR
               (('j' <= C) AND (C <= 'r')) OR
               (('s' <= C) AND (C <= 'z')) ;
     END ;

   FUNCTION DIGIT(C: CHAR): BOOLEAN ;

     BEGIN
     DIGIT := ('0' <= C) AND (C <= '9') ;
     END ;

   FUNCTION SPECIAL(C: CHAR): BOOLEAN;
     BEGIN  SPECIAL := (C = '$') OR (C = '_')  END ;

FUNCTION NOKEY: BOOLEAN;
   VAR I,J,K: INTEGER;
BEGIN I := 1; J := NK;   (*BINARY SEARCH*)
  REPEAT K := (I+J) DIV 2;
    IF KEY[K] <= ID.A THEN I := K+1 ELSE J := K-1
  UNTIL I > J;
  IF J = 0 THEN NOKEY := TRUE ELSE
    NOKEY := KEY[J] <> ID.A
END (*NOKEY*) ;

PROCEDURE COUNTLINE;
BEGIN
  IF M = LPPG THEN
    BEGIN PAGE(OUTPUT); WRITELN(OUTPUT);    WRITELN(OUTPUT);
      M := 0
    END;
  M := M + 1
END (*COUNTLINE*) ;

PROCEDURE ADVANCE;
BEGIN
  WRITE(OUTPUT,INPUT@); GET(INPUT);
  CCOUNT := CCOUNT + 1;
  IF CCOUNT = LLNGIN THEN
    WHILE NOT EOLN(INPUT) DO
      BEGIN WRITE(OUTPUT,INPUT@); GET(INPUT);
      END
END (*ADVANCE*);

PROCEDURE SPACE(J: INTEGER);
BEGIN
  REPEAT J := J-1; WRITELN(OUTPUT); COUNTLINE
  UNTIL J = 0
END (*SPACE*) ;

PROCEDURE NEWLINE;
BEGIN CCOUNT := 0;
  IF N < MAXN THEN
  BEGIN COUNTLINE;  N := N + 1;
    IF COMPILERLISTING THEN
      BEGIN IF NOT EOLN THEN
        BEGIN ADVANCE;
        IF NOT (INPUT@ IN ['0'..'9']) THEN  (* ERRORS *)
        IF NOT DIGIT(INPUT@) THEN  (* ERRORS *)
          WHILE NOT EOLN DO
            ADVANCE
        ELSE BEGIN
          FOR I := 1 TO ADDRWIDTH + 1  DO
            ADVANCE;
          WHILE (INPUT@ = ' ') AND NOT EOLN DO
            ADVANCE
          END
        END
      END
    ELSE WRITE(OUTPUT,' ');
    IF LINENUMBERS THEN
      BEGIN LN := 0;
      WHILE DIGIT(INPUT@) DO
      WHILE INPUT@ IN ['0'..'9'] DO
        BEGIN LN := 10*LN + ORD(INPUT@) - ORD('0');
          ADVANCE
        END
      END
    ELSE BEGIN
      LN := N;  WRITE(OUTPUT,LN:6, ' ')
      END
    END
  ELSE BEGIN
    WRITELN(STARS, ' TEXT TOO LONG', STARS);
     GOTO 99;  EXIT(99);
    END
END (*NEWLINE*) ;

PROCEDURE SEARCH;   (*MODULO P HASH SEARCH*)
  VAR H,D: INDEX;
      X: REF; F: BOOLEAN;
      K: INTEGER;
BEGIN  I := ABS(ID.ORD);  H := I MOD P;
  F := FALSE; D := 1;
  NEW(X); X@.LNO := LN;
  REPEAT
    IF T[H].KEY = ID.A THEN
    BEGIN (*FOUND*) F := TRUE;
      X@.NEXT := T[H].FIRST; T[H].FIRST := X;
    END ELSE
    IF T[H].KEY = EMPTY THEN
    BEGIN (*NEW ENTRY*) F := TRUE;
      T[H].KEY := ID.A;
      T[H].FIRST := X; X@.NEXT := NIL
    END ELSE
    BEGIN (*COLLISION*) H := H+D; D := D+2;
      IF H >= P THEN H := H-P;
      IF D = P THEN
        BEGIN WRITELN(OUTPUT); WRITELN(STARS,' TABLE FULL',STARS);    GOTO 99
        EXIT(99);
        END
    END
  UNTIL F
END (*SEARCH*) ;

PROCEDURE SORT(MIN, MAX: INTEGER);

(* QUICKSORT WITH BOUNDED RECURSION DEPTH *)
(* REQUIRES MIN <= MAX *)

   VAR
         LOW,
        HIGH: INDEX;
      MIDKEY: ALFA;
        TEMP: WORD;

   BEGIN
      REPEAT (*PICK SPLIT POINT*)
         MIDKEY := T[(MIN + MAX) DIV 2].KEY;
         LOW := MIN;
         HIGH := MAX;
         REPEAT (*PARTITION*)
            WHILE T[LOW].KEY < MIDKEY DO
               LOW := LOW + 1;
            WHILE T[HIGH].KEY > MIDKEY DO
               HIGH := HIGH - 1;
            IF LOW <= HIGH THEN
               BEGIN
                  TEMP := T[LOW];
                  T[LOW] := T[HIGH];
                  T[HIGH] := TEMP;
                  LOW := LOW + 1;
                  HIGH := HIGH - 1
               END;
         UNTIL LOW > HIGH;

         (*RECURSIVELY SORT SHORTER SUB-SEGMENT*)    (*A NOTE *)
         IF HIGH - MIN < MAX - LOW
         THEN    ¯ ANOTHER NOTEò  ¯ A FORTH ONE ò
            BEGIN
               IF MIN < HIGH THEN
                  SORT(MIN, HIGH);
  ¯THIS ONE ò  MIN := LOW
            END
" "      ELSE
            BEGIN
               IF LOW < MAX THEN
                  SORT(LOW, MAX);
               MAX := HIGH
               END
        UNTIL MAX <= MIN
   END " SORT" """(*SORT*)";


PROCEDURE " HERE " NOTEPROC;   (*NOTE INSTANCE OF PROCEDURE OR FUNCTION*)
  VAR P: PROCREF;
BEGIN PROCORFUNC := FALSE;
  NEW(P); PROCPTR@.NEXT := P;
  P@.NAME := ID.A; P@.LNO := LN; P@.NEXT := NIL;
  PROCPTR := P
END (*NOTEPROC*) ;

PROCEDURE PRINTWORD(W: WORD);
  VAR L: INTEGER; X,Y,Z: REF;
BEGIN COUNTLINE; WRITE(OUTPUT,' ', W.KEY);
  X := W.FIRST; Y := X@.NEXT; X@.NEXT := NIL;
  WHILE Y <> NIL DO
    BEGIN Z := Y@.NEXT; Y@.NEXT := X; X := Y; Y := Z
    END ;
  L := 0;
  REPEAT
    IF L = NOPL THEN
      BEGIN L := 0; WRITELN(OUTPUT); COUNTLINE; WRITE(OUTPUT,' ', EMPTY)
      END;
    L := L+1; WRITE(OUTPUT,X@.LNO: DGPN); X := X@.NEXT
  UNTIL X = NIL;
  WRITELN(OUTPUT);
END (*PRINTWORD*) ;

PROCEDURE PRINTTABLE;
  VAR I,M: INDEX;
BEGIN M := 0;    (*COMPRESS TABLE*)
  FOR I := 0 TO P-1 DO
    IF T[I].KEY <> EMPTY THEN
      BEGIN T[M] := T[I]; M := M+1
      END ;
  IF M > 0 THEN SORT(0,M-1);
  NOPL := (LLNGOUT-KLN-1) DIV DGPN;
  SPACE(2); WRITELN(' CROSS REFERENCE OF IDENTIFIERS,',
            ' LABEL DECLARATIONS AND GOTO STATEMENTS:');
  COUNTLINE; SPACE(1);
  FOR I := 0 TO M-1 DO PRINTWORD(T[I])
END (*PRINTTABLE*) ;

PROCEDURE PRINTPROCS;
BEGIN SPACE(1); COUNTLINE;
  WRITELN(' LIST OF PROCEDURES AND FUNCTIONS:');
  COUNTLINE; SPACE(1);
  PROCPTR := FIRSTPROC@.NEXT;
  WHILE PROCPTR <> NIL DO
    BEGIN WITH PROCPTR@ DO WRITELN(NAME:24,LNO:10);
      COUNTLINE; PROCPTR := PROCPTR@.NEXT
    END
END (*PRINTPROCS*) ;

PROCEDURE INITIALIZE;
  TYPE SETTING = PACKED RECORD
                   CASE SWITCH: BOOLEAN OF
                     TRUE: (ONOFF: CHAR);
                     FALSE: (SIZE: 0..999999)
                   END;
  VAR S: SETTING;
  FUNCTION OPTION(NAME: CHAR; VAR S: SETTING): BOOLEAN;
    EXTERNAL;
BEGIN N := 0; M := 0;
  LLNGIN := LLMAX; LLNGOUT := LLMAX;
   IF OPTION('U',S) THEN
    IF S.SWITCH AND (S.ONOFF = '+')
      THEN LLNGIN := LLMIN;
  IF OPTION('W',S) THEN
    IF S.SWITCH AND (S.ONOFF = '+')
      THEN LLNGOUT := LLMIN;
  FOR I := 0 TO P-1 DO T[I].KEY := EMPTY;
  NEW(PROCPTR); FIRSTPROC := PROCPTR; PROCPTR@.NEXT := NIL;
  PROCORFUNC := TRUE;   (*TO GET P R O G R A M NAME IN PROCEDURE INDEX*)
  KEY[ 1] := 'AND       '; KEY[ 2] := 'ARRAY     ';
  KEY[ 3] := 'BEGIN     '; KEY[ 4] := 'CASE      ';
  KEY[ 5] := 'CONST     '; KEY[ 6] := 'DIV       ';
  KEY[ 7] := 'DOWNTO    '; KEY[ 8] := 'DO        ';
  KEY[ 9] := 'ELSE      '; KEY[10] := 'END       ';
  KEY[11] := 'FILE      '; KEY[12] := 'FOR       ';
  KEY[13] := 'FUNCTION  '; KEY[14] := 'IF        ';
  KEY[15] := 'IN        '; KEY[16] := 'MOD       ';
  KEY[17] := 'NIL       '; KEY[18] := 'NOT       ';
  KEY[19] := 'OF        '; KEY[20] := 'OR        ';
  KEY[21] := 'PACKED    '; KEY[22] := 'PROCEDURE ';
  KEY[23] := 'PROGRAM   '; KEY[24] := 'RECORD    ';
  KEY[25] := 'REPEAT    '; KEY[26] := 'SET       ';
  KEY[27] := 'THEN      '; KEY[28] := 'TO        ';
  KEY[29] := 'TYPE      '; KEY[30] := 'UNTIL     ';
  KEY[31] := 'VAR       '; KEY[32] := 'WHILE     ';
  KEY[33] := 'WITH      '
END (*INITIALIZE*) ;

PROCEDURE SCANANDLISTINPUT;
BEGIN
  WHILE NOT EOF(INPUT) DO
  BEGIN NEWLINE;
    WHILE NOT EOLN(INPUT) DO
    CASE INPUT@ OF
     'a','b','c','d','e','f','g','h','i','j','k','l','m',
     'n','o','p','q','r','s','t','u','v','w','x','y','z',
     'A','B','C','D','E','F','G','H','I','J','K','L','M',
     'N','O','P','Q','R','S','T','U','V','W','X','Y','Z':
      BEGIN K := 0; ID.A := EMPTY;
        REPEAT
          IF K < KLN THEN
            BEGIN K := K+1; ID.A[K] := INPUT@
            END;
          ADVANCE
        UNTIL NOT(INPUT@ IN ['A'..'Z', '0'..'9']);
        UNTIL NOT(LETTER(INPUT@) OR DIGIT(INPUT@) OR SPECIAL(INPUT@));
        IF NOKEY THEN
        BEGIN SEARCH;
          IF PROCORFUNC THEN NOTEPROC
        END ELSE
        IF (ID.A = 'PROCEDURE ') OR (ID.A = 'FUNCTION  ') THEN
          PROCORFUNC := TRUE
      END;
     '0','1','2','3','4','5','6','7','8','9':
        REPEAT ADVANCE;
        UNTIL NOT (INPUT@ IN ['B','E','0'..'9']);
        UNTIL NOT DIGIT(INPUT@) ;
     '''':
      BEGIN (*STRING*)
        REPEAT ADVANCE;
        UNTIL (INPUT@ = '''') OR EOLN(INPUT);
        IF NOT EOLN(INPUT) THEN
          ADVANCE
      END;
#    '"':
      BEGIN (*COMMENT*)
        REPEAT ADVANCE;
          WHILE EOLN(INPUT) DO
            BEGIN WRITELN(OUTPUT); GET(INPUT); NEWLINE
            END
        UNTIL INPUT@ = '"';
        ADVANCE
      END;
     '(':
      BEGIN ADVANCE;
        IF INPUT@ = '*' THEN
        BEGIN (*COMMENT*) ADVANCE;
          REPEAT
            WHILE INPUT@ <> '*' DO
            BEGIN
              IF EOLN(INPUT) THEN
                BEGIN GET(INPUT); WRITELN(OUTPUT); NEWLINE
                END ELSE
                ADVANCE
            END ;
            ADVANCE
          UNTIL INPUT@ = ')';
          ADVANCE
        END
      END;
     '+','-','*','/',')','$','=',' ',',','.','[',']',
     ':','!','×','&','@','?','<','>','Ö','\','^',';','#','_','%','ò':
      ADVANCE
    END (*CASE*) ;
    WRITELN(OUTPUT); GET(INPUT)
  END ;
END (*SCANANDLISTINPUT*) ;

PROCEDURE SKIPCOMPILERTITLE;
  VAR I: INTEGER;
BEGIN
  COMPILERLISTING := INPUT@ = '1';
  IF COMPILERLISTING THEN
  BEGIN I := 0; GET(INPUT);
    WHILE I < LITL DO
      BEGIN I := I + 1;
        WHILE NOT EOLN(INPUT) DO
          ADVANCE;
        READLN; WRITELN(OUTPUT);
      END;
    COUNTLINE;
    LINENUMBERS := TRUE
  END ELSE
  BEGIN WRITELN(OUTPUT); WRITELN(OUTPUT);
    LINENUMBERS :=    INPUT@ IN ['0'..'9']    DIGIT(INPUT@)
  END
END (*SKIPCOMPILERTITLE*) ;

BEGIN (*CROSSREF*)
   LINELIMIT(OUTPUT, MAXN);     PAGE(OUTPUT); INITIALIZE;
  IF NOT EOF(INPUT) THEN
  BEGIN SKIPCOMPILERTITLE;
    SCANANDLISTINPUT;    LINELIMIT(OUTPUT, MAXN);
    PRINTTABLE; PRINTPROCS
  END ELSE WRITELN(STARS,' NO PROGRAM FOUND TO CROSS REFERENCE',STARS);
99:END .
