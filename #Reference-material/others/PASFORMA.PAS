
 PROGRAM pascalformatter;
{
| **                Pascal Program Formatter                       **
| **                                                               **
| **   by J. E. Crider, Shell Oil Company, Houston, Texas 77025    **
| **                                                               **
| **   Copyright (c) 1980 by Shell Oil Company.  Permission to     **
| **   copy, modify, and distribute, but not for profit, is        **
| **   hereby granted, provided that this note is included.        **
|
| Changes:
|   The program has been updated to replace keywords according to
|   the TURBO Pascal conventions.
|
|   This portable program formats Pascal programs and acceptable
|   program fragments according to structured formatting principles
|   [SIGPLAN Notices, Vol. 13, No. 11, Nov. 1978, pp. 15-22].
|   The actions of the program are as follows:
|
|   PREPARATION:  For each structured statement that controls a
|      structured statement, the program converts the controlled
|      statement into a compound statement.  The inserted BEGIN/END
|      pair are in capital letters.  A null statement (with semicolon)
|      is inserted before the last END symbol of each program/
|      procedure/function, if needed.  The semicolon forces the END
|      symbol to appear on a line by itself.
|
|   FORMATTING:  Each structured statement that controls a simple
|      statement is placed on a single line, as if it were a simple
|      statement.  Otherwise, each structured statement is formatted
|      in the following pattern (with indentation "indent"):
|
|            XXXXXX header XXXXXXXX
|               XXXXXXXXXXXXXXXXXX
|               XXXXX body XXXXXX
|               XXXXXXXXXXXXXXXXXX
|
|      where the header is one of:
|
|            while <expression> do begin
|            for <control variable> := <for list> do begin
|            with <record variable list> do begin
|            repeat
|            if <expression> then begin
|            else if <expression> then begin
|            else begin
|            case <expression> of
|            <case label list>: begin
|
|      and the last line either begins with UNTIL or ends with END.
|      Other program parts are formatted similarly.  The headers are:
|
|            <program/procedure/function heading>;
|            label
|            const
|            type
|            var
|            begin
|            (various for records and record variants)
|
|   COMMENTS:  Each comment that starts before or on a specified
|      column on an input line (program constant "commthresh") is
|      copied without shifting or reformatting.  Each comment that
|      starts after "commthresh" is reformatted and left-justified
|      following the aligned comment base column ("alcommbase").
|
|   LABELS:  Each statement label is justified to the left margin and
|      is placed on a line by itself.
|
|   SPACES AND BLANK LINES:  Spaces not at line breaks are copied from
|      the input.  Blank lines are copied from the input if they appear
|      between statements (or appropriate declaration units).  A blank
|      line is inserted above each significant part of each program/
|      procedure/function if one is not already there.
|
|   CONTINUATION:  Lines that are too long for an output line are
|      continued with additional indentation ("contindent").
|
|   INPUT FORM:  The program expects as input a program or program
|      fragment in Standard Pascal.  A program fragment is acceptable
|      if it consists of a sequence of (one or more) properly ordered
|      program parts; examples are:  a statement part (that is, a
|      compound statement), or a TYPE part and a VAR part followed by
|      procedure declarations.  If the program fragment is in serious
|      error, then the program may copy the remainder of the input file
|      to the output file without significant modification.  Error
|      messages may be inserted into the output file as comments.
|}

    CONST
       maxrwlen = 10;              { size of reserved word strings }
       ordminchar = 32;            { ord of lowest char in char set }
       ordmaxchar = 126;           { ord of highest char in char set }
                                   {  Although this program uses the ASCII
                                      character set, conversion to most other
                                      character sets should be straightforward.
                                      }

 {  The following parameters may be adjusted for the installation: }
       maxinlen = 255;             { maximum width of input line + 1 }
       maxoutlen = 80;             { maximum width of output line }
       initmargin = 1;             { initial value of output margin }
       commthresh = 4;             { column threshhold in input for comments to
                                      be aligned }
       alcommbase = 35;            { aligned comments in output start AFTER this
                                      column }
       indent = 3;                 { RECOMMENDED indentation increment }
       contindent = 5;             { continuation indentation, >indent }
       endspaces = 3;              { number of spaces to precede 'END' }
       commindent = 3;             { comment continuation indentation }
       line_number : INTEGER = 0;

    TYPE
       natural = 0..MaxInt;
       inrange = 0..maxinlen;
       outrange = 0..maxoutlen;

       errortype = (longline, noendcomm, notquote, longword, notdo, notof,
            notend, notthen, notbegin, notuntil, notsemicolon, notcolon,
            notparen, noeof);

       chartype = (illegal, special, chapostrophe, chleftparen, chrightparen,
            chperiod, digit, chcolon, chsemicolon, chlessthan, chgreaterthan,
            letter, chleftbrace);

                                   { for reserved word recognition }
       resword = (                 { reserved words ordered by length }
            rwif, rwdo, rwof, rwto, rwin, rwor,
                                   { length: 2 }
            rwend, rwfor, rwvar, rwdiv, rwmod, rwset, rwand, rwnot, rwnil,
                                   { length: 3 }
            rwthen, rwelse, rwwith, rwgoto, rwcase, rwtype, rwfile, rwuses,
            rwunit,                { length: 4 }
            rwbegin, rwuntil, rwwhile, rwarray, rwconst, rwlabel, rwvalue,
                                   { length: 5 }
            rwrepeat, rwrecord, rwdownto, rwpacked,rwmodule,
                                   { length: 6 }
            rwprogram,             { length: 7 }
            rwfunction,            { length: 8 }
            rwotherwise,rwprocedure,
                                   { length: 9 }
            rwx);                  { length: 10 for table sentinel }
       rwstring = PACKED ARRAY [1..maxrwlen] OF CHAR;

       firstclass = (              { class of word if on new line }
            newclause,             { start of new clause }
            continue,              { continuation of clause }
            alcomm,                { start of aligned comment }
            contalcomm,            { continuation of aligned comment }
            uncomm,                { start of unaligned comment }
            contuncomm,            { continuation of unaligned comment }
            stmtlabel);            { statement label }
       wordtype = RECORD           { data record for word }
          whenfirst: firstclass;   { class of word if on new line }
          puncfollows: BOOLEAN;    { to reduce dangling punctuation }
          blanklncount: natural;   { number of preceding blank lines }
          spaces: INTEGER;         { number of spaces preceding word }
          base: -9..maxinlen;      { inlinexx.buf[base] precedes word }
          size: inrange   END;     { length of word in inlinexx.buf }

       symboltype = (              { symbols for syntax analysis }
            semicolon, sybegin, syend,
                                   { three insertable symbols first }
            syif, sydo, syof, sythen, syelse, sygoto, sycase, syuntil, syrepeat,
            syrecord, forwhilewith, progprocfunc, declarator, otherword,
            othersym, leftparen, rightparen, period, syotherwise, sysubrange,
            intconst, colon, ident, comment, syeof);
       inserttype = semicolon..syend;
       symbolset = SET OF symboltype;
                                   { *** NOTE: set size of 0..26 REQUIRED for
                                      symbolset! }

    VAR
       Input,Output : TEXT[$800];
       response : STRING[10];
       no_error_output : BOOLEAN;
       infilename,outfilename : STRING[80];
       inlinexx: RECORD            { input line data }
          endoffile: BOOLEAN;      { end of file on input? }
          ch: CHAR;                { current char, buf[index] }
          index: inrange;          { subscript of current char }
          len: natural;            { length of input line in buf }
                                   { string ';BEGINEND' in buf[-8..0] }
          buf: ARRAY [-8..maxinlen] OF CHAR   END;
       outline: RECORD             { output line data }
          blanklns: natural;       { number of preceding blank lines }
          len: outrange;           { number of chars in buf }
          buf: ARRAY [1..maxoutlen] OF CHAR   END;
       WORD: wordtype;             { current word }
       margin: outrange;           { left margin }
       lnpending: BOOLEAN;         { new line before next symbol? }
       symbol: symboltype;         { current symbol }

  { Structured Constants }
       headersyms: symbolset;      { headers for program parts }
       strucsyms: symbolset;       { symbols that begin structured statements }
       stmtbeginsyms: symbolset;   { symbols that begin statements }
       stmtendsyms: symbolset;     { symbols that follow statements }
       stopsyms: symbolset;        { symbols that stop expression scan }
       recendsyms: symbolset;      { symbols that stop record scan }
       datawords: symbolset;       { to reduce dangling punctuation }
       newword: ARRAY [inserttype] OF wordtype;
       instring: PACKED ARRAY [1..9] OF CHAR;
       firstrw: ARRAY [1..maxrwlen] OF resword;
       rwword: ARRAY [rwif..rwprocedure] OF rwstring;
       rwsy: ARRAY [rwif..rwprocedure] OF symboltype;
       charclass: ARRAY [CHAR] OF chartype;
                                   { above is portable form; possible ASCII form
                                      is: }
                                   {    charclass: array [' '..'~'] of chartype;
                                      }
       symbolclass: ARRAY [chartype] OF symboltype;

    PROCEDURE strucconsts;         { establish values of structured constants }

       VAR
          i: ordminchar..ordmaxchar;
                                   { loop index }
          ch: CHAR;                { loop index }

       PROCEDURE buildinsert (symbol: inserttype;
            inclass: firstclass;
            inpuncfollows: BOOLEAN;
            inspaces, inbase: INTEGER;
            insize: inrange);

          BEGIN
             WITH newword[symbol] DO BEGIN
                whenfirst := inclass;
                puncfollows := inpuncfollows;
                blanklncount := 0;
                spaces := inspaces;
                base := inbase;
                size := insize   END;
             END;                  { buildinsert }

       PROCEDURE buildrw (rw: resword;
            symword: rwstring;
            symbol: symboltype);

          BEGIN
             rwword[rw] := symword;{ reserved word string }
             rwsy[rw] := symbol;   { map to symbol }
             END;                  { buildrw }

       BEGIN                       { strucconsts }
                                   { symbol sets for syntax analysis }
          headersyms := [progprocfunc, declarator, sybegin, syeof];
          strucsyms := [sycase, syrepeat, syif, forwhilewith];
          stmtbeginsyms := strucsyms + [sybegin, ident, sygoto, syotherwise];
          stmtendsyms := [semicolon, syend, syuntil, syelse, syeof];
          stopsyms := headersyms + strucsyms + stmtendsyms + [sygoto];
          recendsyms := [rightparen, syend, syeof];

          datawords := [otherword, intconst, ident, syend];

                                   { words for insertable symbols }
          buildinsert (semicolon, continue, FALSE, 0, -9, 1);
          buildinsert (sybegin, continue, FALSE, 1, -8, 5);
          buildinsert (syend, newclause, TRUE, endspaces, -3, 3);
          instring := ';        '; {';BEGINEND'}

                                   { constants for recognizing reserved words }
          firstrw[1] := rwif;      { length: 1 }
          firstrw[2] := rwif;      { length: 2 }
          buildrw (rwif, 'IF        ', syif);
          buildrw (rwdo, 'DO        ', sydo);
          buildrw (rwof, 'OF        ', syof);
          buildrw (rwto, 'TO        ', othersym);
          buildrw (rwin, 'IN        ', othersym);
          buildrw (rwor, 'OR        ', othersym);
          firstrw[3] := rwend;     { length: 3 }
          buildrw (rwend, 'END       ', syend);
          buildrw (rwfor, 'FOR       ', forwhilewith);
          buildrw (rwvar, 'VAR       ', declarator);
          buildrw (rwdiv, 'DIV       ', othersym);
          buildrw (rwmod, 'MOD       ', othersym);
          buildrw (rwset, 'SET       ', othersym);
          buildrw (rwand, 'AND       ', othersym);
          buildrw (rwnot, 'NOT       ', othersym);
          buildrw (rwnil, 'NIL       ', otherword);
          firstrw[4] := rwthen;    { length: 4 }
          buildrw (rwthen, 'THEN      ', sythen);
          buildrw (rwelse, 'ELSE      ', syelse);
          buildrw (rwwith, 'WITH      ', forwhilewith);
          buildrw (rwgoto, 'GOTO      ', sygoto);
          buildrw (rwcase, 'CASE      ', sycase);
          buildrw (rwtype, 'TYPE      ', declarator);
          buildrw (rwfile, 'FILE      ', othersym);
          buildrw (rwuses, 'USES      ', declarator);
          buildrw (rwunit, 'UNIT      ', declarator);
          firstrw[5] := rwbegin;   { length: 5 }
          buildrw (rwbegin, 'BEGIN     ', sybegin);
          buildrw (rwuntil, 'UNTIL     ', syuntil);
          buildrw (rwwhile, 'WHILE     ', forwhilewith);
          buildrw (rwarray, 'ARRAY     ', othersym);
          buildrw (rwconst, 'CONST     ', declarator);
          buildrw (rwlabel, 'LABEL     ', declarator);
          buildrw (rwvalue, 'VALUE     ', declarator);
          firstrw[6] := rwrepeat;  { length: 6 }
          buildrw (rwrepeat, 'REPEAT    ', syrepeat);
          buildrw (rwrecord, 'RECORD    ', syrecord);
          buildrw (rwdownto, 'DOWNTO    ', othersym);
          buildrw (rwpacked, 'PACKED    ', othersym);
          buildrw (rwmodule, 'MODULE    ',progprocfunc);
          firstrw[7] := rwprogram; { length: 7 }
          buildrw (rwprogram, 'PROGRAM   ', progprocfunc);
          firstrw[8] := rwfunction;{ length: 8 }
          buildrw (rwfunction, 'FUNCTION  ', progprocfunc);
          firstrw[9] := rwotherwise;
                                   { length: 9 }
          buildrw (rwotherwise, 'OTHERWISE ', syotherwise);
          buildrw (rwprocedure, 'PROCEDURE ', progprocfunc);
          firstrw[10] := rwx;      { length: 10 for table sentinel }

                                   { constants for lexical scan }
          FOR i := ordminchar TO ordmaxchar DO BEGIN
             charclass[Chr (i)] := illegal   END;
          FOR ch := 'a' TO 'z' DO BEGIN
                                   { !!! implementation-dependent!  (but can be
                                      replaced with 52 explicit assignments) }
             charclass[ch] := letter;
             charclass[UpCase(ch)] := letter   END;
          charclass['_'] := letter;
          charclass['#'] := letter;
          FOR ch := '0' TO '9' DO charclass[ch] := digit;
          charclass[' '] := special;
          charclass['$'] := special;
          charclass[''''] := chapostrophe;
          charclass['('] := chleftparen;
          charclass[')'] := chrightparen;
          charclass['*'] := special;
          charclass['+'] := special;
          charclass['-'] := special;
          charclass['.'] := chperiod;
          charclass['/'] := special;
          charclass[':'] := chcolon;
          charclass[';'] := chsemicolon;
          charclass['<'] := chlessthan;
          charclass['='] := special;
          charclass['>'] := chgreaterthan;
          charclass['@'] := special;
          charclass['['] := special;
          charclass[']'] := special;
          charclass['^'] := special;
          charclass['{'] := chleftbrace;
          symbolclass[illegal] := othersym;
          symbolclass[special] := othersym;
          symbolclass[chapostrophe] := otherword;
          symbolclass[chleftparen] := leftparen;
          symbolclass[chrightparen] := rightparen;
          symbolclass[chperiod] := period;
          symbolclass[digit] := intconst;
          symbolclass[chcolon] := colon;
          symbolclass[chsemicolon] := semicolon;
          symbolclass[chlessthan] := othersym;
          symbolclass[chgreaterthan] := othersym;
          symbolclass[letter] := ident;
          symbolclass[chleftbrace] := comment;

          END;                     { strucconsts }

{ writeline/writeerror/readline convert between files and lines. }

    PROCEDURE writeline;           { write buffer into output file }

       VAR
          i: outrange;             { loop index }

       BEGIN
          WITH outline DO BEGIN
             WHILE blanklns > 0 DO BEGIN
                Writeln (Output);
                blanklns := blanklns - 1   END;
             IF len > 0 THEN BEGIN
                FOR i := 1 TO len DO Write (Output, buf[i]);
                Writeln (Output);
                len := 0   END   END;
          END;                     { writeline }

    PROCEDURE writeerror (error: errortype);
                                   { report error to output }

       VAR
          i, ix: inrange;          { loop index, limit }

       BEGIN
          IF NOT no_error_output THEN BEGIN
             writeline;
             Write (Output, ' (*  !!! error, ');
             CASE error OF
                longline:     Write (Output, 'shorter line');
                noendcomm:    Write (Output, 'end of comment');
                notquote:     Write (Output, 'final "''" on line');
                longword:     Write (Output, 'shorter word');
                notdo:        Write (Output, '"do"');
                notof:        Write (Output, '"of"');
                notend:       Write (Output, '"end"');
                notthen:      Write (Output, '"then"');
                notbegin:     Write (Output, '"begin"');
                notuntil:     Write (Output, '"until"');
                notsemicolon: Write (Output, '";"');
                notcolon:     Write (Output, '":"');
                notparen:     Write (Output, '")"');
                noeof:        Write (Output, 'end of file')   END;
             Write (Output, ' expected');
             IF error >= longword THEN BEGIN
                Write (Output, ', not "');
                WITH inlinexx, WORD DO BEGIN
                   IF size > maxrwlen THEN ix := maxrwlen
                   ELSE ix := size;
                   FOR i := 1 TO ix DO Write (Output, buf[base + i])   END;
                Write (Output, '"')   END;
             IF error = noeof THEN Write (Output, ', FORMATTING STOPS');
             Writeln (Output, ' !!!  *)');
             END
          ELSE BEGIN
             Write (Con,line_number, ' (*  !!! error, ');
             CASE error OF
                longline:     Write (Con, 'shorter line');
                noendcomm:    Write (Con, 'end of comment');
                notquote:     Write (Con, 'final "''" on line');
                longword:     Write (Con, 'shorter word');
                notdo:        Write (Con, '"do"');
                notof:        Write (Con, '"of"');
                notend:       Write (Con, '"end"');
                notthen:      Write (Con, '"then"');
                notbegin:     Write (Con, '"begin"');
                notuntil:     Write (Con, '"until"');
                notsemicolon: Write (Con, '";"');
                notcolon:     Write (Con, '":"');
                notparen:     Write (Con, '")"');
                noeof:        Write (Con, 'end of file')   END;
             Write (Con, ' expected');
             IF error >= longword THEN BEGIN
                Write (Con, ', not "');
                WITH inlinexx, WORD DO BEGIN
                   IF size > maxrwlen THEN ix := maxrwlen
                   ELSE ix := size;
                   FOR i := 1 TO ix DO Write (Con, buf[base + i])   END;
                Write (Con, '"')   END;
             IF error = noeof THEN Write (Con, ', FORMATTING STOPS');
             Writeln (Con, ' !!!  *)');
             END;

          END;                     { writeerror }

    PROCEDURE readline;            { read line into input buffer }

       VAR
          c: CHAR;                 { input character }
          nonblank: BOOLEAN;       { is char other than space? }

       BEGIN
          WITH inlinexx DO BEGIN
             len := 0;
             IF Eof (Input) THEN endoffile := TRUE
             ELSE BEGIN            { get next line }
                WHILE NOT Eoln (Input) DO BEGIN
                   Read (Input, c);
                   IF c < ' ' THEN BEGIN
                                   { convert ASCII control chars (except leading
                                      form feed) to spaces }
                      IF c = Chr (9) THEN BEGIN
                                   { ASCII tab char }
                         c := ' '; { add last space at end }
                         WHILE len MOD 8 <> 7 DO BEGIN
                            len := len + 1;
                            IF len < maxinlen THEN buf[len] := c   END;
                         END       { end tab handling }
                      ELSE IF (c <> Chr (12)) OR (len > 0) THEN c := ' ';
                      END;         { end ASCII control char conversion }
                   len := len + 1;
                   IF len < maxinlen THEN buf[len] := c   END;
                Readln (Input);
                line_number := line_number+1;
                IF len >= maxinlen THEN BEGIN
                                   { input line too long }
                   writeerror (longline);
                   len := maxinlen - 1   END;
                nonblank := FALSE;
                REPEAT             { trim line }
                   IF len = 0 THEN nonblank := TRUE
                   ELSE IF buf[len] <> ' ' THEN nonblank := TRUE
                   ELSE len := len - 1
                   UNTIL nonblank   END;
             len := len + 1;       { add exactly ONE trailing blank }
             buf[len] := ' ';
             index := 0   END;
          END;                     { readline }

{ startword/finishword/copyword convert between lines and words.
   auxiliary procedures getchar/nextchar precede. }

    PROCEDURE getchar;             { get next char from input buffer }

       BEGIN
          WITH inlinexx DO BEGIN
             index := index + 1;
             ch := buf[index]   END;
          END;                     { getchar }

    FUNCTION nextchar: CHAR;       { look at next char in input buffer }

       BEGIN
          WITH inlinexx DO nextchar := buf[index + 1];
          END;                     { nextchar }

    PROCEDURE startword (startclass: firstclass);
                                   { note beginning of word, and count preceding
                                      lines and spaces }

       VAR
          first: BOOLEAN;          { is word the first on input line? }

       BEGIN
          first := FALSE;
          WITH inlinexx, WORD DO BEGIN
             whenfirst := startclass;
             blanklncount := 0;
             WHILE (index >= len) AND NOT endoffile DO BEGIN
                IF len = 1 THEN blanklncount := blanklncount + 1;
                IF startclass = contuncomm THEN writeline
                ELSE first := TRUE;
                readline;          { with exactly ONE trailing blank }
                getchar;           { ASCII:        if ch = chr (12) then begin [
                                      ASCII form feed char ] writeline; writeln
                                      (output, chr (12)); blanklncount := 0;
                                      getchar   end;  [ end ASCII form feed
                                      handling }
                END;
             spaces := 0;          { count leading spaces }
             IF NOT endoffile THEN BEGIN
                WHILE ch = ' ' DO BEGIN
                   spaces := spaces + 1;
                   getchar   END   END;
             IF first THEN spaces := 1;
             base := index - 1   END;
          END;                     { startword }

    PROCEDURE finishword;          { note end of word }

       BEGIN
          WITH inlinexx, WORD DO BEGIN
             puncfollows := (symbol IN datawords) AND (ch <> ' ');
             size := index - base - 1   END;
          END;                     { finishword }

    PROCEDURE copyword (newline: BOOLEAN;
         WORD: wordtype);          { copy word from input buffer into output
                                      buffer }

       VAR
          i: INTEGER;              { outline.len excess, loop index }

       BEGIN
          WITH WORD, outline DO BEGIN
             i := maxoutlen - len - spaces - size;
             IF newline OR (i < 0) OR ((i = 0) AND puncfollows) THEN writeline;
             IF len = 0 THEN BEGIN { first word on output line }
                blanklns := blanklncount;
                CASE whenfirst OF  { update LOCAL word.spaces }
                   newclause:  spaces := margin;
                   continue:   spaces := margin + contindent;
                   alcomm:     spaces := alcommbase;
                   contalcomm: spaces := alcommbase + commindent;
                   uncomm:     spaces := base;
                   contuncomm: ;   { spaces := spaces }
                   stmtlabel:  spaces := initmargin   END;
                IF spaces + size > maxoutlen THEN BEGIN
                   spaces := maxoutlen - size;
                                   { reduce spaces }
                   IF spaces < 0 THEN BEGIN
                      writeerror (longword);
                      size := maxoutlen;
                      spaces := 0   END   END   END;
             FOR i := 1 TO spaces DO BEGIN
                                   { put out spaces }
                len := len + 1;
                buf[len] := ' '   END;
             FOR i := 1 TO size DO BEGIN
                                   { copy actual word }
                len := len + 1;
                buf[len] := inlinexx.buf[base + i]   END   END;
          END;                     { copyword }

{ docomment/copysymbol/insert/getsymbol/findsymbol convert between
   words and symbols. }

    PROCEDURE docomment;           { copy aligned or unaligned comment }

       PROCEDURE copycomment (commclass: firstclass;
            commbase: inrange);    { copy words of comment }

          VAR
             endcomment: BOOLEAN;  { end of comment? }

          BEGIN
             WITH WORD DO BEGIN    { copy comment begin symbol }
                whenfirst := commclass;
                spaces := commbase - outline.len;
                copyword ((spaces < 0) OR (blanklncount > 0), WORD)   END;
             commclass := Succ (commclass);
             WITH inlinexx DO BEGIN
                REPEAT             { loop for successive words }
                   startword (commclass);
                   endcomment := endoffile;
                                   { premature end? }
                   IF endcomment THEN writeerror (noendcomm)
                   ELSE BEGIN
                      REPEAT
                         IF ch = '*' THEN BEGIN
                            getchar;
                            IF ch = ')' THEN BEGIN
                               endcomment := TRUE;
                               getchar   END   END
                         ELSE IF ch = '}' THEN BEGIN
                            endcomment := TRUE;
                            getchar   END
                         ELSE getchar
                         UNTIL (ch = ' ') OR endcomment   END;
                   finishword;
                   copyword (FALSE, WORD)
                   UNTIL endcomment   END;
             END;                  { copycomment }

       BEGIN                       { docomment }
          IF WORD.base < commthresh THEN BEGIN
                                   { copy comment without alignment }
             copycomment (uncomm, WORD.base)   END
          ELSE BEGIN               { align and format comment }
             copycomment (alcomm, alcommbase)   END;
          END;                     { docomment }

    PROCEDURE copysymbol (symbol: symboltype;
         WORD: wordtype);          { copy word(s) of symbol }

       BEGIN
          IF symbol = comment THEN BEGIN
             docomment;            { NOTE: docomment uses global word! }
             lnpending := TRUE   END
          ELSE IF symbol = semicolon THEN BEGIN
             copyword (FALSE, WORD);
             lnpending := TRUE   END
          ELSE BEGIN
             copyword (lnpending, WORD);
             lnpending := FALSE   END;
          END;                     { copysymbol }

    PROCEDURE Insert (newsymbol: inserttype);
                                   { copy word for inserted symbol into output
                                      buffer }

       BEGIN
          copysymbol (newsymbol, newword[newsymbol]);
          END;                     { insert }

    PROCEDURE getsymbol;           { get next non-comment symbol }

       PROCEDURE findsymbol;       { find next symbol in input buffer }

          VAR
             chclass: chartype;    { classification of leading char }

          PROCEDURE checkresword;  { check if current identifier is reserved
                                      word/symbol }

             CONST
                keyword_size = 226;
                keyword_len = 15;
                keyword : ARRAY[1..keyword_size] OF ARRAY[1..2] OF STRING[
                     keyword_len] = ( ('ABORT','Abort'),('ABSOLUTE','Absolute'),
                     ('ADDR','Addr'), ('ADR',''),('ADRMEM','AdrMem'),('ADS',''),
                     ('ADSMEM','AdsMem'), ('AND',''), ('APPEND','Append'), (
                     'ARCTAN','Arctan'), ('ARRAY',''), ('ASSIGN', 'Assign'), (
                     'AUX','Aux'), ('AUXINPTR','AuxInPtr'), ( 'AUXOUTPTR',
                     'AuxOutPtr'), ('BEGIN',''), ('BLOCKREAD', 'BlockRead'), (
                     'BLOCKWRITE','BlockWrite'), ('BOOLEAN',''), ('BREAK',''),
                     ('BUFLEN','BufLen'), ('BYTE',''), ('BYWORD','ByWord'), (
                     'CASE',''), ( 'CHAIN','Chain'), ('CHAR',''), ('CHDIR',
                     'ChDir'), ('CHR','Chr'), ('CLOSE', 'Close'), ('CLREOL',
                     'ClrEol'), ('CLRSCR','ClrScr'), ('CON', 'Con'), ('CONCAT',
                     'Concat'), ('CONINPTR','ConInPtr'), ( 'CONOUTPTR',
                     'ConOutPtr'), ('CONST',''), ('CONSTPTR', 'ConstPtr'), (
                     'COPY','Copy'), ('COPYLST','CopyLst'),('COPYSTR','CopyStr')
                     , ('COS','Cos'), ('CRTEXIT', 'CrtExit'), ('CRTINIT',
                     'CrtInit'), ('CSEG','CSeg'), ('CYCLE',''),('DECODE',
                     'Decode'), ( 'DELAY','Delay'), ('DELETE','Delete'), (
                     'DELLINE', 'DelLine'), ('DISPOSE','Dispose'), ('DIV',''), (
                     'DO',''), ('DOWNTO',''), ( 'DRAW','Draw'), ('DSEG','DSeg'),
                     ('ELSE',''), ('ENCODE','Encode'), ('END',''), ( 'EOF',
                     'Eof'), ('EOLN','Eoln'), ('ERASE','Erase'), ('EVAL','Eval')
                     , ('EXECUTE', 'Execute'), ('EXP','Exp'), ('EXTERN',''), (
                     'EXTERNAL',''), ('FALSE',''), ( 'FILE',''), ('FILEPOS',
                     'FilePos'), ('FILESIZE','FileSize'), ('FILLC','FillC'), (
                     'FILLCHAR','FillChar'), ('FILLSC','FillSC'), ('FLUSH',
                     'Flush'), ('FOR',''), ( 'FORWARD',''), ('FRAC','Frac'), (
                     'FREEMEM','FreeMem'), ( 'FUNCTION',''), ('GETDIR','GetDir')
                     , ('GETMEM','GetMem'), ('GOTO',''), ( 'GOTOXY','GotoXY'), (
                     'GRAPHBACKGROUND','GraphBackGround'), ('GRAPHCOLORMODE',
                     'GraphColorMode'), ('GRAPHMODE', 'GraphMode'), (
                     'GRAPHWINDOW','GraphWindow'), ('HALT', 'Halt'), ('HEAPSTR',
                     'HeapStr'), ('HI','Hi'), ('HIBYTE','HiByte'),
                     ('HIRES', 'HiRes'), ('HIRESCOLOR',
                     'HiResColor'), ('IF',''), ('IN','') , ('INLINE','InLine'),
                     ('INPUT','Input'), ('INSERT', 'Insert'), ('INSLINE',
                     'InsLine'), ('INT',''), ('INTEGER', ''), ('INTR','Intr'), (
                     'IORESULT','IOResult'), ('KBD', 'Kbd'), ('KEYPRESSED',
                     'KeyPressed'), ('LABEL',''), ( 'LENGTH','Length'), ('LN',
                     'Ln'), ('LO','Lo'), ( 'LONGFILEPOS','LongFilePos'), (
                     'LONGFILESIZE', 'LongFileSize'), ('LONGSEEK','LongSeek'),
                     ('LOBYTE','LoByte'),('LOWER','Lower'),
                     ('LOWVIDEO', 'LowVideo'), ('LST','Lst'),
                     ('LSTOUTPTR','LstOutPtr'), ('LSTRING',''), ( 'MARK','Mark')
                     , ('MAXAVAIL','MaxAvail'), ('MAXINT', 'MaxInt'), ('MEM',
                     'Mem'), ('MEMAVAIL','MemAvail'), ('MEMW', 'MemW'), (
                     'MKDIR','MkDir'), ('MOD',''), ('MODULE',''), ('MOVE',
                     'Move'), ('MOVEL','MoveL'),('MOVER','MoveR'), ('MOVESL',
                     'MoveSL'),('MOVESR','MoveSR'), ('MSDOS','MSDos'), ('NEW',
                     'New'), ('NIL',''), ('NORMVIDEO','NormVideo'), ( 'NOSOUND',
                     'NoSound'), ('NOT',''), ('NULL',''),
                     ('ODD','Odd'), ('OF',''), ('OFS',
                     'Ofs'), ('OR',''), ('ORD','Ord'), ('OTHERWISE',''),
                     ('OUTPUT','Output'), (
                     'OVRPATH','OvrPath'), ('PACKED',''), ('PALETTE','Palette'),
                     ('PARAMCOUNT','ParamCount'), ('PARAMSTR','ParamStr'), (
                     'PI','Pi'), ('PLOT', 'Plot'), ('PORT','Port'), ('PORTW',
                     'PortW'), ('POS','Pos'), ('POSITN','Positn'), ('PRED',''),
                     ('PROCEDURE',''), ('PROGRAM',''), ('PTR', 'Ptr'), (
                     'PUBLIC',''), ('RANDOM','Random'), ('RANDOMIZE',
                     'Randomize'), ( 'READ','Read'), ('READLN','Readln'), (
                     'REAL',''), ( 'RECORD',''), ('RELEASE','Release'), (
                     'RENAME','Rename'), ( 'REPEAT',''), ('RESET','Reset'), (
                     'RETURN',''), ('REWRITE','Rewrite'), ('RMDIR','RmDir'), (
                     'ROUND','Round'), ('SCANEQ','ScanEQ'),('SCANNE','ScanNE'),
                     ('SEEK','Seek'), ('SEG','Seg'), ('SET', ''), ('SHL','ShL'),
                     ('SHR','ShR'), ('SIN','Sin'), ( 'SIZEOF','SizeOf'), (
                     'SOUND','Sound'), ('SQR','Sqr'), ( 'SQRT','Sqrt'), ('SSEG',
                     'SSeg'), ('STATIC',''), ('STR','Str'), ('STRING', ''), (
                     'SUCC','Succ'),('SUPER',''),
                     ('SWAP','Swap'), ('TEXT',''), (
                     'TEXTBACKGROUND','TextBackGround'), ('TEXTCOLOR',
                     'TextColor'), ('TEXTMODE','TextMode'), ('THEN',''), ('TO',
                     ''), ('TRM','Trm'), ('TRUE',''), ('TRUNC','Trunc'), (
                     'TRUNCATE','Truncate'), ( 'TYPE',''), ('UNTIL',''), (
                     'UPCASE','UpCase'), ('UPPER','Upper'),('USES',''), ('USR',
                     'Usr'), ('USRINPTR','UsrInPtr'), ('USROUTPTR','UsrOutPtr'),
                     ('VAL','Val'), ('VALUE',''), ('VAR',''), ('WHEREX',
                     'WhereX'), ('WHEREY', 'WhereY'), ('WHILE',''), ('WINDOW',
                     'Window'), ('WITH',''), ('WORD',''),('WRD','Wrd'), (
                     'WRITE','Write'), ('WRITELN','Writeln'), ('XOR',''));

             LABEL
                bypass;

             VAR
                rw, rwbeyond: resword;
                                   { loop index, limit }
                symword: rwstring; { copy of symbol word }
                i: 1..maxrwlen;    { loop index }
                high_index,low_index,key_index,select,key_size : INTEGER;
                test_keyword : STRING[keyword_len];

             BEGIN
                WITH WORD, inlinexx DO BEGIN
                   size := index - base - 1;
                   IF size < maxrwlen THEN BEGIN
                      symword := '          ';
                      FOR i := 1 TO size DO symword[i] := UpCase(buf[ base + i]
                           );
                      rw := firstrw[size];
                      rwbeyond := firstrw[size + 1];
                      symbol := semicolon;
                      REPEAT
                         IF rw >= rwbeyond THEN symbol := ident
                         ELSE IF symword = rwword[rw] THEN symbol := rwsy[rw]
                         ELSE rw := Succ (rw)
                         UNTIL symbol <> semicolon;
                      IF symbol = syend THEN BEGIN
                         IF spaces < endspaces THEN spaces := endspaces;
                         whenfirst := newclause   END   END;
                                   {goto bypass;}
                   IF size <= keyword_len THEN BEGIN
                      FOR key_size := 1 TO size DO test_keyword[key_size] :=
                           UpCase(buf[base+key_size]);
                      test_keyword[0] := Chr(size);
                      low_index := 1;
                      high_index := keyword_size;
                      WHILE low_index <= high_index DO BEGIN
                         key_index := (high_index + low_index) DIV 2;
                         IF keyword[key_index,1] = test_keyword THEN BEGIN
                            IF keyword[key_index,2] = '' THEN select := 1
                            ELSE select := 2;
                            FOR key_size := 1 TO size DO buf[base+key_size] :=
                                 keyword[key_index,select][key_size];
                            low_index := high_index+1;
                                   {terminate the loop}
                            END
                         ELSE IF keyword[key_index,1] > test_keyword THEN
                              high_index := key_index - 1
                         ELSE low_index := key_index + 1;
                         END;
                      END;
                   bypass:;
                   END;
                END;               { checkresword }

          PROCEDURE getname;

             BEGIN
                WHILE charclass[inlinexx.ch] IN [letter, digit] DO getchar;
                checkresword;
                END;               { getname }

          PROCEDURE getnumber;

             BEGIN
                WITH inlinexx DO BEGIN
                   WHILE charclass[ch] = digit DO getchar;
                   IF ch = '.' THEN BEGIN
                                   { thanks to A.H.J.Sale, watch for '..' }
                      IF charclass[nextchar] = digit THEN BEGIN
                                   { NOTE: nextchar is a function! }
                         symbol := otherword;
                         getchar;
                         WHILE charclass[ch] = digit DO getchar   END   END;
                   IF UpCase (ch) = 'E' THEN BEGIN
                      symbol := otherword;
                      getchar;
                      IF (ch = '+') OR (ch = '-') THEN getchar;
                      WHILE charclass[ch] = digit DO getchar   END   END;
                END;               { getnumber }

          PROCEDURE getstringliteral;

             VAR
                endstring: BOOLEAN;{ end of string literal? }

             BEGIN
                WITH inlinexx DO BEGIN
                   endstring := FALSE;
                   REPEAT
                      IF ch = '''' THEN BEGIN
                         getchar;
                         IF ch = '''' THEN getchar
                         ELSE endstring := TRUE   END
                      ELSE IF index >= len THEN BEGIN
                                   { error, final "'" not on line }
                         writeerror (notquote);
                         symbol := syeof;
                         endstring := TRUE   END
                      ELSE getchar
                      UNTIL endstring   END;
                END;               { getstringliteral }

          BEGIN                    { findsymbol }
             startword (continue);
             WITH inlinexx DO BEGIN
                IF endoffile THEN symbol := syeof
                ELSE BEGIN
                   chclass := charclass[ch];
                   symbol := symbolclass[chclass];
                   getchar;        { second char }
                   CASE chclass OF
                      chsemicolon, chrightparen, chleftbrace, special, illegal:
                           ;
                      letter:  getname;
                      digit:  getnumber;
                      chapostrophe:  getstringliteral;
                      chcolon:  BEGIN
                         IF ch = '=' THEN BEGIN
                            symbol := othersym;
                            getchar   END   END;
                      chlessthan:  BEGIN
                         IF (ch = '=') OR (ch = '>') THEN getchar   END;
                      chgreaterthan:  BEGIN
                         IF ch = '=' THEN getchar   END;
                      chleftparen:  BEGIN
                         IF ch = '*' THEN BEGIN
                            symbol := comment;
                            getchar   END   END;
                      chperiod:  BEGIN
                         IF ch = '.' THEN BEGIN
                            symbol := sysubrange;
                            getchar   END   END   END   END   END;
             finishword;
             END;                  { findsymbol }

       BEGIN                       { getsymbol }
          REPEAT
             copysymbol (symbol, WORD);
                                   { copy word for symbol to output }
             findsymbol            { get next symbol }
             UNTIL symbol <> comment;
          END;                     { getsymbol }

{ block performs recursive-descent syntax analysis with symbols,
   adjusting margin, lnpending, word.whenfirst, and
   word.blanklncount.  auxiliary procedures precede. }

    PROCEDURE startclause;         { (this may be a simple clause, or the start
                                      of a header) }

       BEGIN
          WORD.whenfirst := newclause;
          lnpending := TRUE;
          END;                     { startclause }

    PROCEDURE passsemicolons;      { pass consecutive semicolons }

       BEGIN
          WHILE symbol = semicolon DO BEGIN
             getsymbol;
             startclause   END;    { new line after ';' }
          END;                     { passsemicolons }

    PROCEDURE startpart;           { start program part }

       BEGIN
          WITH WORD DO BEGIN
             IF blanklncount = 0 THEN blanklncount := 1   END;
          startclause;
          END;                     { startpart }

    PROCEDURE startbody;           { finish header, start body of structure }

       BEGIN
          passsemicolons;
          margin := margin + indent;
          startclause;
          END;                     { startbody }

    PROCEDURE finishbody;

       BEGIN
          margin := margin - indent;
          END;                     { finishbody }

    PROCEDURE passphrase (finalsymbol: symboltype);
                                   { process symbols until significant symbol
                                      encountered }

       VAR
          endsyms: symbolset;      { complete set of stopping symbols }

       BEGIN
          IF symbol <> syeof THEN BEGIN
             endsyms := stopsyms + [finalsymbol];
             REPEAT
                getsymbol
                UNTIL symbol IN endsyms   END;
          END;                     { passphrase }

    PROCEDURE expect (expectedsym: symboltype;
         error: errortype;
         syms: symbolset);

       BEGIN
          IF symbol = expectedsym THEN getsymbol
          ELSE BEGIN
             writeerror (error);
             WHILE NOT (symbol IN [expectedsym] + syms) DO getsymbol;
             IF symbol = expectedsym THEN getsymbol   END;
          END;                     { expect }

    PROCEDURE dolabel;             { process statement label }

       VAR
          nextfirst: firstclass;   { (pass whenfirst to statement) }

       BEGIN
          WITH WORD DO BEGIN
             nextfirst := whenfirst;
             whenfirst := stmtlabel;
             lnpending := TRUE;
             getsymbol;
             expect (colon, notcolon, stopsyms);
             whenfirst := nextfirst;
             lnpending := TRUE   END;
          END;                     { dolabel }

    PROCEDURE block;               { process block }

       PROCEDURE heading;          { process heading for program, procedure, or
                                      function }

          PROCEDURE matchparens;   { process parentheses in heading }

             BEGIN
                getsymbol;
                WHILE NOT (symbol IN recendsyms) DO BEGIN
                   IF symbol = leftparen THEN matchparens
                   ELSE getsymbol   END;
                expect (rightparen, notparen, stopsyms + recendsyms);
                END;               { matchparens }

          BEGIN                    { heading }
             getsymbol;
             passphrase (leftparen);
             IF symbol = leftparen THEN matchparens;
             IF symbol = colon THEN passphrase (semicolon);
             IF symbol = othersym THEN BEGIN
                                   {'['}
                passphrase(semicolon);
                IF symbol = othersym THEN passphrase(semicolon);
                                   {']'}
                END;
             expect (semicolon, notsemicolon, stopsyms);
             END;                  { heading }

       PROCEDURE statement;        { process statement }

          FORWARD;

       PROCEDURE stmtlist;         { process sequence of statements }

          BEGIN
             REPEAT
                statement;
                passsemicolons
                UNTIL symbol IN stmtendsyms;
             END;                  { stmtlist }

       PROCEDURE compoundstmt (    { process compound statement }
            stmtpart: BOOLEAN);    { statement part of block? }

          BEGIN
             getsymbol;
             startbody;            { new line, indent after 'BEGIN' }
             stmtlist;
             IF stmtpart AND NOT lnpending THEN Insert (semicolon);
             expect (syend, notend, stmtendsyms);
             finishbody;           { left-indent after 'END' }
             END;                  { compoundstmt }

       PROCEDURE statement;        { process statement }

          PROCEDURE checkcompound; { if structured then force compound }

             BEGIN
                IF symbol = intconst THEN dolabel;
                IF symbol IN strucsyms THEN BEGIN
                                   { force compound }
                                   {insert (sybegin);}
                   startbody;      { new line, indent after 'BEGIN' }
                   statement;      {insert (syend);}
                   finishbody   END{ left-indent after 'END' }
                ELSE statement;
                END;               { checkcompound }

          PROCEDURE ifstmt;        { process if statement }

             BEGIN
                passphrase (sythen);
                expect (sythen, notthen, stopsyms);
                checkcompound;
                IF symbol = syelse THEN BEGIN
                   startclause;    { new line before 'ELSE' }
                   getsymbol;
                   IF symbol = syif THEN ifstmt
                   ELSE checkcompound   END;
                END;               { ifstmt }

          PROCEDURE repeatstmt;    { process repeat statement }

             BEGIN
                getsymbol;
                startbody;         { new line, indent after 'REPEAT' }
                stmtlist;
                startclause;       { new line before 'UNTIL' }
                expect (syuntil, notuntil, stmtendsyms);
                passphrase (semicolon);
                finishbody;        { left-ident after 'UNTIL' }
                END;               { repeatstmt }

          PROCEDURE fwwstmt;       { process for, while, or with statement }

             BEGIN
                passphrase (sydo);
                expect (sydo, notdo, stopsyms);
                checkcompound;
                END;               { fwwstmt }

          PROCEDURE casestmt;      { process case statement }

             BEGIN
                passphrase (syof);
                expect (syof, notof, stopsyms);
                startbody;         { new line, indent after 'OF' }
                REPEAT
                   IF symbol = syelse THEN symbol := syotherwise;
                   IF symbol <> syotherwise THEN BEGIN
                      passphrase (colon);
                      expect (colon, notcolon, stopsyms);
                      END;
                   checkcompound;
                   passsemicolons
                   UNTIL symbol IN (stopsyms - [syelse]);
                expect (syend, notend, stmtendsyms);
                finishbody;        { left-indent after 'END' }
                END;               { casestmt }

          BEGIN                    { statement }
             IF symbol = intconst THEN dolabel;
             IF symbol IN stmtbeginsyms THEN BEGIN
                CASE symbol OF
                   sybegin:       compoundstmt (FALSE);
                   sycase:        casestmt;
                   syif:          ifstmt;
                   syrepeat:      repeatstmt;
                   forwhilewith:  fwwstmt;
                   syotherwise:   BEGIN
                      getsymbol;
                      startbody;
                      stmtlist;
                      finishbody;
                      END;
                   ident, sygoto: passphrase (semicolon)   END   END;
             IF NOT (symbol IN stmtendsyms) THEN BEGIN
                writeerror (notsemicolon);
                                   { ';' expected }
                passphrase (semicolon)   END;
             END;                  { statement }

       PROCEDURE passfields (forvariant: BOOLEAN);

          FORWARD;

       PROCEDURE dorecord;         { process record declaration }

          BEGIN
             getsymbol;
             startbody;
             passfields (FALSE);
             expect (syend, notend, recendsyms);
             finishbody;
             END;                  { dorecord }

       PROCEDURE dovariant;        { process (case) variant part }

          BEGIN
             passphrase (syof);
             expect (syof, notof, stopsyms);
             startbody;
             passfields (TRUE);
             finishbody;
             END;                  { dovariant }

       PROCEDURE doparens (forvariant: BOOLEAN);
                                   { process parentheses in record }

          BEGIN
             getsymbol;
             IF forvariant THEN startbody;
             passfields (FALSE);
             lnpending := FALSE;   { for empty field list }
             expect (rightparen, notparen, recendsyms);
             IF forvariant THEN finishbody;
             END;                  { doparens }

       PROCEDURE passfields;       { process declarations }
                                   {     procedure passfields (forvariant:
                                      boolean); }

          BEGIN                    { passfields }
             WHILE NOT (symbol IN recendsyms) DO BEGIN
                IF symbol = semicolon THEN passsemicolons
                ELSE IF symbol = syrecord THEN dorecord
                ELSE IF symbol = sycase THEN dovariant
                ELSE IF symbol = leftparen THEN doparens (forvariant)
                ELSE getsymbol   END;
             END;                  { passfields }

       BEGIN                       { block }
          WHILE symbol = declarator DO BEGIN
             startpart;            { label, const, type, var }
             getsymbol;
             startbody;
             REPEAT
                passphrase (syrecord);
                IF symbol = syrecord THEN dorecord;
                IF symbol = semicolon THEN passsemicolons
                UNTIL symbol IN headersyms;
             finishbody   END;
          WHILE symbol = progprocfunc DO BEGIN
             startpart;            { program, procedure, function }
             heading;
             startbody;
             IF symbol IN headersyms THEN block
             ELSE IF symbol = ident THEN BEGIN
                startpart;         { directive: forward, etc. }
                passphrase (semicolon);
                passsemicolons   END
             ELSE writeerror (notbegin);
             finishbody   END;
          IF symbol = sybegin THEN BEGIN
             startpart;            { statement part }
             compoundstmt (TRUE);
             IF symbol IN [sysubrange, period] THEN symbol := semicolon;
                                   { treat final period as semicolon }
             passsemicolons   END;
          END;                     { block }

    PROCEDURE copyrem;             { copy remainder of input }

       BEGIN
          writeerror (noeof);
          WITH inlinexx DO BEGIN
             REPEAT
                copyword (FALSE, WORD);
                startword (contuncomm);
                IF NOT endoffile THEN BEGIN
                   REPEAT
                      getchar
                      UNTIL ch = ' '   END;
                finishword;
                UNTIL endoffile   END;
          END;                     { copyrem }

    PROCEDURE initialize;          { initialize global variables }

       VAR
          i: 1..9;                 { loop index }

       BEGIN
          WITH inlinexx DO BEGIN
             FOR i := 1 TO 9 DO buf[i - 9] := instring[i];
                                   { string ';BEGINEND' in buf[-8..0] }
             endoffile := FALSE;
             ch := ' ';
             index := 0;
             len := 0   END;
          WITH outline DO BEGIN
             blanklns := 0;
             len := 0   END;
          WITH WORD DO BEGIN
             whenfirst := contuncomm;
             puncfollows := FALSE;
             blanklncount := 0;
             spaces := 0;
             base := 0;
             size := 0   END;
          margin := initmargin;
          lnpending := FALSE;
          symbol := othersym;
          END;                     { initialize }

    BEGIN                          { pascalformatter }
       IF (ParamCount<2) OR (ParamCount>3) THEN BEGIN
          Writeln('Incorrect # of parameters');
          Halt;
          END;
       IF ParamCount = 3 THEN no_error_output := FALSE
       ELSE no_error_output := TRUE;
       Assign(Input,ParamStr(1));
       Reset(Input);
       Assign(Output,ParamStr(2));
       Rewrite(Output);
       strucconsts;
       initialize;                 {  ***************  Files may be opened here.
                                      }
       getsymbol;
       block;
       IF NOT inlinexx.endoffile THEN copyrem;
       writeline;
       Write(Output,Chr(26));      {put EOF character}
       Close(Output);
       END                         { pascalformatter } .
