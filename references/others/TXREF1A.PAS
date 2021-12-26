program TXREF;
{$V-}
{$R+}

{  Program TXREF - Produce a Listing and Cross Reference for a Turbo Pascal
   source file.

   You must have Turbo Toolbox from Borland International, Inc. in order to
   compile this program.

   As written, this program assumes that you have an Epson FX-80 printer. It
   may work on other printers if they are compatible enough.

   By Michael Quinlan
      Version 1.0.0
      12/1/84

  Known bugs:

    1. This program doesn't correctly handle certain types of constants;
       the 'E' in a floating point constant will be considered a name as
       will some hex constants. The procedure CopyTillAlpha needs to be
       re-written to handle these things better.

    2. Numeric labels are not included in the cross reference.

    3. Names longer than 79 bytes may mess up the page alignment while
       printing the cross reference.

ADDITIONS: APRIL 19,1985 BY GUY GALLO
I've adapted TXREF to take advantage of Turbo 3.0's paramater parsing.
And added a choice of printers.  D)ot matrix will set the program to use
epson compatible printers (thinkjet, oki, ibm) - program will be listed in
compressed print, with reserved words underlined L)etter Quality will print
underlining only if you have sufficient buffering.  The codes for letter
quality are those for Diablo 630/C.Itoh F10.  You can change to suit in the
Printer Procedures below}

const
  LinesPerPage = 60;

Type
  Str = String[127];
  XrefRec = record
              Name : Str;
              Page : Integer;
              Line : Integer;
            end;

var
  XrefVar       : XrefRec;
  NumOnLine     : Integer;
  CurLine       : Integer;
  CurPage       : Integer;
  SortResult    : Integer;
  InFileName    : Str;
  InFile        : Text;
  Line          : Str;
  CurPosn       : Integer;
  CommentStatus : (NoComment, CurlyBracket, ParenStar);
  InsideString  : Boolean;
  ptype         : Char;

const
 NumReservedWords = 44;
 BiggestReservedWord = 9;
 ReservedWordList : array [1..NumReservedWords] of String[BiggestReservedWord]
   = (
  'ABSOLUTE', 'AND'     , 'ARRAY' , 'BEGIN', 'CASE'    , 'CONST' , 'DIV',
  'DO'      , 'DOWNTO'  , 'ELSE'  , 'END'  , 'EXTERNAL', 'FILE'  , 'FOR',
  'FORWARD' , 'FUNCTION', 'GOTO'  , 'IF'   , 'IN'      , 'INLINE', 'LABEL',
  'MOD'     , 'NIL'     , 'NOT'   , 'OF'   , 'OR'      , 'PACKED', 'PROCEDURE',
  'PROGRAM' , 'RECORD'  , 'REPEAT', 'SET'  , 'SHL'     , 'SHR'   , 'STRING',
  'THEN'    , 'TO'      , 'TYPE'  , 'UNTIL', 'VAR'     , 'WHILE' , 'WITH',
  'XOR'     , 'OVERLAY');

var
  ReservedWordHashTable : array [1..NumReservedWords] of
                            record
                              WordPtr : Integer;
                              NextPtr : Integer
                            end;

 {$IA:SORT.BOX}  { Include the sort routines from Turbo ToolBox }

{=======================================================================}
{  Printer Routines                                                     }
{=======================================================================}

procedure Printer_Init;
{ Init the printer to 132 column mode }
begin
  if ptype = 'D' then  Write(Lst, #15)
end;

procedure Printer_Reset;
{ reset printer back to 80 column mode }
begin
  if ptype = 'D' then Write(Lst, #18)  { turn compressed mode off }
end;

procedure Printer_Underscore;
{ Turn on underlines }
begin
  if ptype = 'D' then
  Write(Lst, #27'-1')  { turn on underlines : dot matrix}
  else
  Write(Lst,#27#95);   {letter quality}
end;

procedure Printer_NoUnderscore;
{ Turn off underlines }
begin
  if ptype = 'D' then
  Write(Lst, #27'-0')  { turn off underlines : dot matrix}
  else
  Write(Lst,#27#82);   {letter quality}
end;

procedure Printer_Eject;
{ Eject to a new page }
begin
  Write(Lst, #12)
end;

{======================================================================}
{ Procedures for handling the hash table; this is used to speed up     }
{ checking for reserved words.                                         }
{======================================================================}

function ReservedWordHash(var w : Str) : Integer;
var
  c : char;
  h : integer;
  i : integer;
  n : integer;
begin
  h := 0;
  n := 1;
  for i := 1 to (length(w) div 2) do
    begin
      h := h xor ((Ord(w[n]) shl 8) or Ord(w[n+1]));
      n := n + 2
    end;
  if n = length(w) then
    h := h xor Ord(w[n]);
  ReservedWordHash := ((h and $7FFF) mod NumReservedWords) + 1
end;

procedure SetUpReservedWordHashTable;
var
  h : integer;
  i : integer;
  NewH : integer;

  function FindFreeEntry(h : integer) : integer;
  begin
    repeat
      if h >= NumReservedWords then h := 1
      else h := h + 1
    until ReservedWordHashTable[h].WordPtr = 0;
    FindFreeEntry := h
  end;

begin
  for i := 1 to NumReservedWords do
    begin
      ReservedWordHashTable[i].WordPtr := 0;
      ReservedWordHashTable[i].NextPtr := 0
    end;
  for i := 1 to NumReservedWords do
    begin
      h := ReservedWordHash(ReservedWordList[i]);
      if ReservedWordHashTable[h].WordPtr = 0 then
        ReservedWordHashTable[h].WordPtr := i
      else
        begin { handle collisions }
          { first find the end of the chain }
          while ReservedWordHashTable[h].NextPtr <> 0 do
            h := ReservedWordHashTable[h].NextPtr;
          { now attach the new entry onto the end of the chain }
          NewH := FindFreeEntry(h);
          ReservedWordHashTable[h].NextPtr := Newh;
          ReservedWordHashTable[NewH].WordPtr := i
        end
    end;
end;

{======================================================================}
{  Procedures to set up the input file.                                }
{======================================================================}

procedure UpStr(var s : Str);
var
  i : integer;
begin
  for i := 1 to length(s) do s[i] := UpCase(s[i])
end;

function AskFileName : Str;
var
  f : Str;
begin
  Write('Name of file to cross reference: ');
  Readln(f);
  if f = '' then halt;  { provide an exit for the user }
  AskFileName := f
end;

function OpenInFile : boolean;
begin
  UpStr(InFileName);  { convert file name to upper case }
  if Pos('.', InFileName) = 0 then InFileName := InFileName + '.PAS';
  Assign(InFile, InFileName);
  {$I-} Reset(InFile); {$I+}
  OpenInFile := (IOResult = 0)
end;

procedure GetInFile;
begin
{ on entry, InFileName may already have the file name }
  if InFileName = '' then InFileName := AskFileName;
  while not OpenInFile do
    begin
      Writeln('Cannot open ', InFileName);
      InFileName := AskFileName
    end
end;

procedure NewPage;
begin
  if CurPage = 0 then
    begin
      Writeln('Make sure printer is lined up at the top of the page and powered on.');
      Write('Press Enter when ready... ');
      readln;
      Printer_Init  { set printer in 132 column mode }
    end
  else
    Printer_Eject;
  CurPage := CurPage + 1;
  CurLine := 1;
  Writeln(Lst, 'Page ', CurPage:5, 'Listing of ':60, InFileName);
  Writeln(Lst)
end;

procedure ReadLine;
begin
  Readln(InFile, Line);
  if CurLine >= LinesPerPage then NewPage
  else CurLine := CurLine + 1;
  CurPosn := 1;
  InsideString := FALSE;
  Write(Lst, CurLine:2, ': ')
end;

{======================================================================}
{  Procedures to process the input file.                               }
{======================================================================}

procedure CopyTillAlpha;
{ copy chars from Line to the printer until the start of a name is found }
begin
  while (CurPosn <= length(Line)) and
         (not (Line[CurPosn] in ['A'..'Z','a'..'z','_']) or InsideString or
          (CommentStatus <> NoComment)) do
    begin
      if CommentStatus = NoComment then
        begin
          if Line[CurPosn] = '''' then InsideString := not InsideString
        end;
      if not InsideString then
        case CommentStatus of
          NoComment : begin
                        if Line[CurPosn] = '{' then CommentStatus := CurlyBracket
                        else if CurPosn < length(Line) then
                               begin
                                 if Copy(Line, CurPosn, 2) = '(*' then
                                   CommentStatus := ParenStar
                               end
                      end;
          CurlyBracket : if Line[CurPosn] = '}' then CommentStatus := NoComment;
          ParenStar    : if CurPosn < length(Line) then
                           begin
                             if Copy(Line, CurPosn, 2) = '*)' then
                               CommentStatus := NoComment
                           end
        end; { Case }
      Write(Lst, Line[CurPosn]);
      CurPosn := CurPosn + 1
    end
end;

function Reserved(var w : Str) : boolean;
var
  h : integer;
  r : (DontKnow, IsReserved, NotReserved);
begin
  h := ReservedWordHash(w);
  r := DontKnow;
  repeat
    if w = ReservedWordList[ReservedWordHashTable[h].WordPtr] then
      r := IsReserved
    else if ReservedWordHashTable[h].NextPtr = 0 then
      r := NotReserved
    else h := ReservedWordHashTable[h].NextPtr
  until r <> DontKnow;
  Reserved := (r = IsReserved)
end;

procedure WriteReserved(var w : Str);
begin
  Printer_Underscore;  { turn on underscores }
  write(Lst, w);
  Printer_NoUnderscore { turn off underscores }
end;

procedure WriteWord(var Word, CapWord : Str);
begin
  XrefVar.Name := CapWord;
  XrefVar.Page := CurPage;
  XrefVar.Line := CurLine;
  SortRelease(XrefVar);
  write(Lst, Word)
end;

procedure DoWord;
var
  wstart  : integer;
  Word    : Str;
  CapWord : Str;
begin
  wstart := CurPosn;
  repeat
    CurPosn := CurPosn + 1
  until (CurPosn > length(Line)) or not (Line[CurPosn] in ['A'..'Z','a'..'z','_','0'..'9']);
  Word := Copy(Line, wstart, CurPosn - wstart);
  CapWord := Word;
  UpStr(CapWord);  { Upper case version of the word }
  if Reserved(CapWord) then
    WriteReserved(Word)
  else
    WriteWord(Word, CapWord)
end;

procedure Inp;
begin
  GetInFile;
  CurLine := 1000;  { force page break on first line }
  CurPage := 0;
  CommentStatus := NoComment;
  while not EOF(InFile) do
    begin
      ReadLine;
      while CurPosn <= length(Line) do
        begin
          CopyTillAlpha;
          if CurPosn <= length(Line) then DoWord
        end;
      Writeln(Lst)
    end
end;

{======================================================================}
{  Procedure called by TurboSort to order the cross reference entries  }
{======================================================================}

function Less;
var
  FirstRec  : XrefRec absolute X;
  SecondRec : XrefRec absolute Y;
begin
  if FirstRec.Name = SecondRec.Name then
    begin
      if FirstRec.Page = SecondRec.Page then
        Less := FirstRec.Line < SecondRec.Line
      else
        Less := FirstRec.Page < SecondRec.Page
    end
  else
    Less := FirstRec.Name < SecondRec.Name
end;

{======================================================================}
{  Procedures to print the sorted cross reference                      }
{======================================================================}

procedure Xref_NewPage;
begin
  Printer_Eject;
  Writeln(Lst, 'C R O S S   R E F E R E N C E':54);
  Writeln(Lst, 'Entries are PAGE:LINE':50);
  Writeln(Lst);
  CurLine := 0
end;

procedure Xref_NewLine;
begin
  Writeln(Lst);
  if CurLine >= LinesPerPage then Xref_NewPage
  else CurLine := CurLine + 1;
  NumOnLine := 0
end;

procedure Xref_Write_Number(n, count : integer);
{ write "n" to Lst with "count" digits (add leading zeros) }
var
  s : Str;
  i : integer;
begin
  for i := count downto 1 do
    begin
      s[i] := Chr((n mod 10) + Ord('0'));
      n := n div 10
    end;
  s[0] := Chr(count);  { set correct string length }
  write(Lst, s)
end;

procedure Xref_Write;
begin
  if NumOnLine >= 8 then Xref_NewLine;
  if NumOnLine = 0 then Write(Lst, '   ');
  Write(Lst, ' ');
  Xref_Write_Number(XrefVar.Page, 5);
  Write(Lst, ':');
  Xref_Write_Number(XrefVar.Line, 2);
  NumOnLine := NumOnLine + 1
end;

procedure Xref_NewName;
begin
  if (CurLine + 2) >= LinesPerPage then Xref_NewPage;
  Write(Lst, XrefVar.Name);
  Xref_NewLine
end;

procedure Outp;
var
  CurName : Str;
begin
  Printer_Reset;  { put printer back into 80 column mode }
  Xref_NewPage;
  SortReturn(XrefVar);
  CurName := XrefVar.Name;
  Xref_NewName;
  Xref_Write;
  while not SortEOS do
    begin
      SortReturn(XrefVar);
      if CurName <> XrefVar.Name then
        begin
          Xref_NewLine;
          CurName := XrefVar.Name;
          Xref_NewName
        end;
      Xref_Write
    end;
  Writeln(Lst);
  Printer_Eject
end;

{======================================================================}
{  Main Program                                                        }
{======================================================================}

begin
  ClrScr;
  Writeln('Pascal Source Listing and Cross Reference Program V1.0.A');
  Writeln('        By Michael Quinlan - revised by G.G.');
  Writeln;
  Write('Type of Printer - D)ot matrix   L)etter Quality =>  ');
  read(kbd,ptype);
  ptype := upcase(ptype);
  repeat until ptype in ['D','L'];
  writeln(ptype);
  SetUpReservedWordHashTable;
  InFileName := paramstr(1);
  while (length(InFileName)>0) and (InFileName[1] = ' ') do
    delete(InFileName, 1, 1);
  SortResult := TurboSort(SizeOf(XrefRec));
  writeln;
  case SortResult of
     0 : Writeln('Program Completed OK');
     3 : Writeln('Insufficient Memory for Sort');
     8 : Writeln('Illegal Item Length for Sort (Program Logic Error)');
     9 : Writeln('More Than ', MaxInt, ' Items to be Sorted');
    10 : Writeln('Sort Error, Disk Error or Disk Full?');
    11 : Writeln('Write Error During Sort, Bad Disk?');
    12 : Writeln('File Creation Error During Sort')
  else
    Writeln('Unknown Error ', SortResult, ' From Sort')
  end; { Case }
  if SortResult <> 0 then
    Writeln('*** Sort Failed; Cross Reference Invalid or Incomplete')
end.
