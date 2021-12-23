program cref;
{
	Cross-reference utility for programs written in Delphi
	or Pascal. It uses a file PASCAL.LTG with reserved
	keywords that can be modified by the user to include
	his favorite keywords (up to 100).
	Usage:
		cref [<input_file>] [<output_file>]
	If the extension is omitted in <input_file>, it is
	assumed to be .PAS.
	Both parameters are optional.
	If no parameters are given, the program asks for them.
	If only one parameter is given, the program asks the
	second one. If it is omitted, it is assumed to be
	"out.txt".
	The output of the program is a text file containing a
	source listing with line numbers and the cross-reference
	itself.
	The tab characters in the source file are expanded to
	four spaces. This can be modified by altering the
	constant NTAB.
	The base algorithm was taken from the book by Niklaus
	Wirth, "Data+algorithm=programs", written in Standard
	Pascal. Some idiosyncrasies of the program derive from
	this origin.
	Do not compile this program with the Delphi GUI,
	use the command line compiler instead.
}

const
    c1 =   25; {length of words}
    c2 =    9; {numbers per line}
    c3 =    6; {digits per number}
    c4 = 9999; {max line number}
    c5 =  100; {number of keywords}
    NTAB =  4; {number of spaces per tabulation}

type
    filename = string[64];
    {a word by its own}
    alpha = packed array[1..c1] of char;
    {a word in the tree}
    wordref = ^word;
    itemref = ^item;
    word = record
        key: alpha;
        first, last: itemref;
        left, right: wordref;
    end;
    {a reference to a word}
    item = record
        lno: 0..c4;
        next: itemref;
    end;

var
    root: wordref; {tree root}
    k1: integer; {counts characters}
    n: integer; {current line number}
    id: alpha; {token}
    finput, gout: text; {input, output}
    a: alpha; {collects characters}
    ch: char; {an input character}
    source, dest: filename; {file labels}
    first_letter, valid_char, comms_char: set of char;
    brackets: set of char;
    h: text; {keyword file and array}
    reserved: array[1..c5] of alpha;
    i, j: integer; {counters}
    feol: boolean; {end of line signal}
    maxKeyNumb: integer; {number of keywords}
    p: string[c1]; {a keyword}
    skipping: boolean; {open comment signal}
    brack: char; {open quote signal}

{
	***** File management *****
}
{ Opens for reading a file of name <name> and assigns its handle
to a variable <fp>. Returns TRUE if successful or FALSE if it fails.}

function Open(var fp: text; name: filename): boolean;
begin
    Assign(fp, name);
{$I-}
    Reset(fp);
{$I+}
    if Ioresult <> 0 then
    begin
        Open := FALSE;
    end
    else
        Open := TRUE;
end;

{ Open input and output files }

procedure OpenFiles;
begin
    if Paramcount = 0 then begin
        write('Input file? ');
        Readln(source);
        if pos('.', source) = 0 then source := concat(source, '.PAS');
        if not Open(finput, source) then begin
            writeln('ERROR - File access');
            Halt;
        end;
        write('Destination file? ');
        Readln(dest);
        if dest = '' then
            dest := 'out.txt';
    end
    else begin {at least one parameter}
        source := Paramstr(1);
        if pos('.', source) = 0 then source := concat(source, '.PAS');
        if not Open(finput, source) then begin
            writeln('ERROR - File access');
            Halt;
        end;
        if Paramcount > 1 then
            dest := Paramstr(2)
        else begin
            write('Destination file? ');
            Readln(dest);
            if dest = '' then
                dest := 'out.txt';
        end;
    end;
    read(finput, ch); {gets a character from the buffer}
    feol := Eoln(finput);
    Assign(gout, dest);
    Rewrite(gout);
end;

{
    ***** Data structure management *****
}
{ Looks up in the tree pointed by "w1", the word "id" with line
  number "n". If it is not there, it is inserted. Returns in "w1"
  a pointer to the word in the tree. }

procedure Search(var w1: wordref);
var
    w: wordref;
    x: itemref;

begin
    w := w1;
    if w = nil then
        {the tree is empty or the word is not there}
    begin
        New(w); {create a word}
        New(x); {create a reference}
        with w^ do
        begin
            key := id;
            left := nil;
            right := nil;
            first := x;
            last := x;
        end;
        x^.lno := n;
        x^.next := nil;
        w1 := w; {returns a pointer to the new word}
    end {empty tree, etc.}
    else
        {the tree is *not* empty}
        if id < w^.key then
            Search(w^.left) {if it is smaller, look to the left}
        else if id > w^.key then
            Search(w^.right) {if it is greater, look to the right}
        else
            {I found it}
        begin
            New(x); {create a new reference}
            x^.lno := n;
            x^.next := nil;
            w^.last^.next := x; {the last one is the new one}
            w^.last := x;
        end;
end; {Search}

{ Prints the tree pointed by "w". }

procedure PrintTree(w: wordref);

    procedure PrintWord(w: word);
    var
        l: integer;
        x: itemref;

    begin
        write(gout, ' ', w.key);
        x := w.first; l := 0;
        repeat
            if l = c2 then
                {the line is over}
            begin
                writeln(gout); {carriage return}
                l := 0;
                write(gout, ' ': c1 + 1); {indent number}
            end;
            l := l + 1; {increment}
            write(gout, x^.lno: c3); {print and go on}
            x := x^.next;
        until x = nil;
        writeln(gout);
    end; {PrintWord}

begin
    if w <> nil then
    begin
        PrintTree(w^.left);
        PrintWord(w^);
        PrintTree(w^.right);
    end;
end; {PrintTree}

{
    ***** Reading and printing the source file *****
}
{ Returns FALSE if a word "p" is a reserved word }

function IsNotReserved(p: alpha): boolean;
var i: integer;
begin
    IsNotReserved := TRUE;
    for i := 1 to maxKeyNumb do
        if p = reserved[i] then IsNotReserved := FALSE;
end;

{ Writes a character with tab expansion. }

procedure WriteWithExp(var f: text);
var i: integer;
begin
    {Convert tabulation to NTAB spaces}
    if ch = #9 then
        for i := 1 to NTAB do
            write(f, ' ')
    else
        write(f, ch);
end;

{ Writes a character to output and reads another one from
input. If the line ends, turn on the "feol" signal. }

procedure WriteAndRead;
begin
    WriteWithExp(gout);
    if not feol then
    begin
        read(finput, ch);
        feol := Eoln(finput);
    end;
end;

{ Writes and reads without processing until the end of
  line. If the line ends, turn on the "feol" signal. }

procedure ReadToEnd;
begin
    while not feol do
    begin
        WriteWithExp(gout);
        read(finput, ch);
        if Eoln(finput) then begin
            feol := true;
            WriteWithExp(gout);
        end;
    end;
end;

procedure ReadComment;
begin
    WriteWithExp(gout);
    while (not feol) and (ch <> '}') do begin
        read(finput, ch);
        feol := Eoln(finput);
        WriteWithExp(gout);
    end;
    if ch = '}' then
        skipping := false;
end;

{ Analyzes a line. Places the word in the tree if it is not a reserved
  one. Skips comments initiated by //, those enclosed in curly
  brackets, and quoted literals. }

procedure NormalLine;
var k: integer;
begin
    repeat
        {the first character is already read}
        if Upcase(ch) in first_letter then
        begin {is it a word?}
            k := 0; {counts characters}
            {collect the word's characters}
            while (Upcase(ch) in valid_char) and not feol do
            begin
                if k < c1 then begin
                    k := k + 1;
                    a[k] := Upcase(ch);
                end;
                write(gout, ch); {write character}
                read(finput, ch); {get another one}
                feol := Eoln(finput);
            end;
            if feol then begin
                {don't lose the last one}
                write(gout, ch);
                if Upcase(ch) in valid_char then
                    if k < c1 then begin
                        k := k + 1;
                        a[k] := Upcase(ch);
                    end;
            end;
            {fill with spaces}
            k1 := c1;
            if k < c1 then
                repeat
                    a[k1] := ' '; k1 := k1 - 1;
                until k1 = k;
            id := a;
            {place in the tree}
            if IsNotReserved(id) then
                Search(root);
        end {normal word}
        else
            {it is not a word - is it a comment?}
            if ch in comms_char then
            begin
                case ch of
                    '{': begin
                            {first bracket}
                            skipping := true;
                            ReadComment;
                        end;
                    '}': begin
                            skipping := false;
                            write(gout, ch);
                            if not feol then begin
                                read(finput, ch);
                                feol := Eoln(finput);
                            end;
                        end;
                    '/': begin
                            {first slash}
                            write(gout, ch);
                            if not feol then begin
                                read(finput, ch);
                                feol := Eoln(finput);
                                if ch in comms_char then
                                	ReadToEnd; {second /}
                            end
                        end
                end
            end {comment}
            else
            begin
                {it is not a comment - is it a quoted literal?}
                if ch in brackets then
                begin
                    {it is a quotation mark}
                    brack := ch;
                    repeat
                        WriteAndRead
                    until ((ch = brack) or feol);
                    if not feol then
                        WriteAndRead; {get next character}
                end
                else
                    {it is not word, comment, or literal -- go on}
                    WriteAndRead;
                if feol then
                begin
                    {we reached the end of the line with a character
					still not processed}
                    write(gout, ch);
                    if (Upcase(ch) in first_letter) then
                    begin
                        id[1] := Upcase(ch);
                        for k := 2 to c1 do id[k] := ' ';
                        if IsNotReserved(id) then
                            Search(root);
                    end;
                end; // is eoln
            end // not a comment else
    until feol; {repeat}
end; {NormalLine}

begin {main}
    {----- Initialization ---}
    maxKeyNumb := c5;
    first_letter := ['A'..'Z', '_'];
    valid_char := first_letter + ['0'..'9'];
    comms_char := ['/', '{', '}'];
    brackets := [#34, #39]; // double and single quotes
    root := nil;
    n := 0; // line number
    skipping := false;
    feol := false;

    {----- Process PASCAL.LTG -----}
    {read keywords}
    Assign(h, 'PASCAL.LTG');
    Reset(h);
    i := 0;
    while (not Eof(h) and (i < maxKeyNumb)) do
    begin
        i := i + 1;
          Readln(h, p);
        for j := 1 to c1 do
            reserved[i][j] := ' ';
        for j := 1 to Length(p) do
            reserved[i][j] := p[j];
    end;
    maxKeyNumb := i;

    {----- Open input and output files -----}
    OpenFiles;

    {----- Process -----}
    while not Eof(finput) do
    begin
        if n = c4 then
            {we ran out of line numbers}
            n := 0;
        n := n + 1; {another line}
        write(gout, n: c3); {write line number}
        write(gout, ' ');
        {***processing the 'n' line***}
        {is it a comment continuation?}
        if skipping then
        begin
            ReadComment;
            read(finput, ch); { read CR }
            read(finput, ch); { read LF }
            writeln(gout); { terminate output line }
        end
        else
        begin
            {process only non-empty lines}
            if ch <> #13 then
            begin
                {skip beginning spaces and tab characters}
                while ((ch = ' ') or (ord(ch) = 9)) and not feol do
                    WriteAndRead;
                {process nth line}
                NormalLine;
                {get next character}
                read(finput, ch); { read CR }
                read(finput, ch); { read LF }
                writeln(gout); { terminate output line }
            end
            else // an empty line
            begin
                read(finput, ch); { read LF }
                writeln(gout); { terminate output line }
            end;
        end;
        { --- begin next line --- }
        feol := false;
        read(finput, ch);
        feol := Eoln(finput);
    end; {while not eof}

    {----- Output -----}
    writeln(gout);
    for i := 1 to 60 do
        write(gout, '=');
    writeln(gout);
    writeln(gout, 'Pascal Cross-reference ', source);
    writeln(gout);
    PrintTree(root);

    {----- File closing -----}
    Close(gout);
    Close(finput);
end.
