{$U+,R+}
program nre_scan;

(* 
                        Version 1.5 

                       for PC/MS-DOS


  Written by: Michael Roberts
              3103 Glenview
              Royal Oak, Mi 48073
              Compuserve : 74226,3045

            This program is built on a listing program by Rick Schaeffer.


  Modified by: Earl Hall
               5619 N. Spaulding #3
               Chicago, IL  60659
               CompuServe : 72746,3244

            Please let me know if you run into any problems or have any
            suggestions.  This program is currently going through major
            revisions (see revision history below), so stay tuned!

                  ========    Revision History   =======

    9/18/85 -  Changes due to the use of DOS file handles in Turbo 3.x:
   (ver 1.1)     1.   Changed WhenCreated procedure to use DOS function 57h
                      to get date/time from file handle.
                      (Probably means that it won't work with older
                        versions of the Turbo compiler. Oh, well.)
                 2.   Added file close to ListIt procedure.
                      Otherwise, program would run out of file handles.

            -  Fixed problem where program was printing the last line of
               an included file twice.
            -  Include REPEATs in the count of block levels ('B' column).
            -  Changed filename of reserved words to TURBOPRT.RES.
            -  Include blank lines, and those with supported psuedo
               operations ($I,$L+,$L-,.PAGE), in line count (like Turbo's
               editor and compiler do).  ($L+,$L-, and .PAGE lines are
               still not printed.)
            -  Fixed PrintLine procedure; was failing to print if heading
               had to be printed first.
            -  Added check for position on paper to avoid {.PAGE} directive
               causing an extra skip to top of form.

    9/19/85 -  Add printer control codes for Gemini 10x printer.  Original
   (ver 1.2)   printer codes remain (commented out).  "Compressed" mode
               on Gemini means to use Elite (12 cpi) font.
            -  Clean-up of printer forms positioning.  Actions specified by
               "maxline" and "top_margin" constants are exact.  In all
               cases, lines are printed and then linect is compared to
               maxline.  Deleted extra formfeeds.  Program now assumes
               that paper is initially positioned at top-of-form and will
               skip to top-of-form at end of listing.
            -  Fixed Cross-Reference problem which caused 2nd and subsequent
               print lines of line numbers for an entry to list 11
               line numbers (instead of 10).
            -  Fixed bug that disallowed counting of REPEATs in "B" column.

    9/21/85 -  Changed output to use the same font for the entire line;
   (ver 1.3)   switching from normal to compressed in mid-line caused
               the printer to print in "one-direction" mode.
            -  Added (commented out) printer codes for the Epson FX-series
               printers.  I have a Star Gemini-10x, so they haven't
               been tested.
            -  Changed Comment indicator from a number to a 'C'. Program was
               not correctly picking up the end of comments if 2 or more
               begin-comments of the same type were used before an end-
               comment of that type (Turbo doesn't nest comments).
            -  Fixed problem where program was not recognising BEGINs, ENDs,
               etc. if they were immediately followed by a comment.
            -  Minor attempt at optimisation of Scan_Line procedure.
            -  An 'I' will now be printed in front of the line number while
               expanding Include files (like Turbo's compiler).
            -  Changed program so that all printed lines will be numbered.
               Also changed logic of file reads so that the line
               numbering will be exactly like Turbo's (CR/LF followed
               by CTRL-Z is considered another line).
               (Well, almost exactly!  Turbo stops looking at the text
               when it sees an "END.", while this program will continue
               to list the file past the end of program.)

    9/23/85 -  Added the Reserved Word list to the program, as a string array,
   (ver 1.4)   removing the requirement for the TURBOPRT.RES file.
            -  Changed the Reserved Word lookup to a binary search on the
               string array.  Speeds up the program a bit.
            -  Modified the program to use the Turbo ParamCount, ParamStr
               functions and Read(Kbd,..).
            -  Changed the program to print variables up to 25 characters in
               length without truncation.

    9/28/85 -  Changed structure of Xref word records from a linked
   (ver 1.5)   list to a B-tree.  Records storing line numbers now contain
               multiple occurances of line numbers.
            -  More optimisation in Scan_Line procedure.


                   ======   Future Desires   ======
                      (for this program, that is)

          I want to add some features to this program in the future.  Some
          of them are:

           -  More gracefully handle the printing of long (>80 chars) lines,
              which are currently just truncated.
           -  Change the "Console or Printer" output option to allow direction
              to any device.  The Console output is really only useful with
              very small programs or for debugging purposes.
           -  Fix up the command line parsing so the program can be run
              from batch files.  Maybe also allow the use of wildcards for
              specifying what programs to list.

          Then, some biggies:

           -  Lex-level analysis of procedures and variables, so that variables
              of the same name declared in different procedures will be
              handled properly.  I'd like this to include some kind of
              "level" indication on the program listing, also, so you could
              easily spot procedures within procedures, etc.
           -  Inclusion of procedures in the cross-reference.  This would
              include detection of redefinition of Standard Turbo Pascal
              identifiers.  It would also have to include forward references.
           -  Listing of identifier type (integer, real, etc.) in the cross-
              reference and of what procedure.
           -  Retain, in the cross-reference, the use of upper/lower case
              letters as used when the indentifier was declared.

              Put these all together and we get something like:

            ListIt               833    Procedure, of TurboPrt; forward at 688
                                 728   950
            monthmask            398    Constant, of WhenCreated
                                 415
            MoreRefs             431    RefsPtr, of BuildXref
                                 453   454   455   456   458   460   461
            MoreRefs             884    RefsPtr, of ListXref
                                 891   895   896   898   908   911
            RefsPtr              228    Type, of TurboPrt
                                 234   240   431   884

*)
{.page}
(* 

   Supported pseudo operations:

     1. Listing control: {.L-} turns it off, {.L+} turns it back on.
        Must be in column 1 and only entry on the line.

     2. Page ejection: {.PAGE}, must be in column 1 and only entry
        on the line.

   When program is first run will check for a file name passed by DOS, and
   will try to open that file.  If no name is passed, will ask operator for
   a file name to open.  Proc will tell operator if file doesn't exist and
   will allow multiple retrys.

   Optionally the file name can be passed via the command line.  Typing an
   "/I" after the filename will expand includes. Examples:

     TurboPrt  -  Will invoke program and ask for file name to be listed.

     TurboPrt MyProg.pas  - Will list file "MYPROG.PAS" and not expand
                             includes.

     TurboPrt MyProg /i  -  Will list file "MYPROG.PAS" and will expand
                            includes.

   On 2nd and later executions, program will not check for DOS passed file
   name.  In all cases, the program will assume a file type of .PAS if file
   type is not specified.  Program will exit when a null string is
   encountered in response to a file name request.

*)
{.page}
const
  maxline       = 64;          {last line on page to print}
  top_margin    = 1;           {lines to skip after top-of-form}

  header_length = 5;           {number of lines taken up by page header}


{ to customize code for your printer - adjust the next items }

{ The following codes are for a Gemini 10x - "Compressed" is Elite print }

  cp = #27#66#2;         {Elite font}
  rp = #27#66#1;         {regular (Pica) font }

{ The following codes should work on an Epson FX-series printer }
(*
  cp = #27#77;           {Elite font}
  rp = #27#80;           {regular (Pica) font }
*)

{  These printer codes were in the original program and are for
   (I assume) the Epson MX/IBM graphics printers. }
(*
  cp = #15;         {compressed print}
  rp = #18;         {regular width }
*)

  cr = #13;
  lf = #10;
  ff = #12;

Type
   two_letters = string[2];
   dtstr       = string[8];
   fnmtype     = string[40];
   instring    = string[135];
   regpack     = record
      ax,bx,cx,dx,bp,si,di,ds,es,flags : integer;
   end;

Var
  First     : boolean;           {true when prog is run}
  answer    : char;
  Buff1     : instring;          {input line buffer}

  Wordchk   : string[25];
  heaptop   : ^Integer;
  listfil   : text;              {FIB for LST: or CON: output}
  infile    : text;              {FIB for input file}
  fnam      : fnmtype;            {input file name}
  file_path : fnmtype;           {path to input file}
  bcount    : integer;           {begin/end counter}
  linect    : integer;           {output file line counter}
  RefLine   : integer;           {Line Reference number counter}
  pageno    : integer;           {page counter}
  offset    : integer;
  print     : boolean;           (* {.L-} don't print *)
                                 (* {.L+} print       *)
  print_head    : boolean;
  Print_Xref    : boolean;
  path_found    : boolean;
  Word_switch   : boolean;
  skip_this_line: boolean;
  comment_brace : boolean;
  comment_paren : boolean;

  c, Print_opt : char;
  comment_char : char;
  include_char : char;

  month, day, year,
  hour, minute, second : two_letters;

  sysdate, systime,
  filedate, filetime : dtstr;

  expand_includes    : boolean;
  holdarg            : instring;
  allregs            : regpack;

{.page}
{                 Xref stuff begins here                        }

const
  RefsPerRec = 10;
  NumReservedWords    = 244;
  BiggestReservedWord = 15;

type
   ReservedWord = String[BiggestReservedWord];
   XrefPtr = ^XrefRec;
   RefsPtr = ^RefsRec;

   XrefRec = Record
                 RefWord  : string[25];
                 LeftPtr  : XrefPtr;
                 RightPtr : XrefPtr;
                 NextRefs : RefsPtr;
             end;

   RefsRec = record
                 NumRefs  : 0..RefsPerRec;
                 Refs     : Array [1..RefsPerRec] of Integer;
                 NextRefs : RefsPtr;
             end;

var
  WordTree          : XRefPtr;
  ReservedWordCheck : ReservedWord;

const
  ReservedWordList : array [1..NumReservedWords] of ReservedWord =
 (
  'ABS'                  ,'ABSOLUTE'             ,'ADDR'
 ,'AND'                  ,'APPEND'               ,'ARC'
 ,'ARCTAN'               ,'ARRAY'                ,'ASSIGN'
 ,'AUX'                  ,'AUXINPTR'             ,'AUXOUTPTR'
 ,'BACK'                 ,'BEGIN'                ,'BLACK'
 ,'BLINK'                ,'BLOCKREAD'            ,'BLOCKWRITE'
 ,'BLUE'                 ,'BOOLEAN'              ,'BROWN'
 ,'BUFLEN'               ,'BYTE'                 ,'CASE'
 ,'CHAIN'                ,'CHAR'                 ,'CHDIR'
 ,'CHR'                  ,'CIRCLE'               ,'CLEARSCREEN'
 ,'CLOSE'                ,'CLREOL'               ,'CLRSCR'
 ,'COLORTABLE'           ,'CON'                  ,'CONCAT'
 ,'CONINPTR'             ,'CONOUTPTR'            ,'CONST'
 ,'CONSTPTR'             ,'COPY'                 ,'COS'
 ,'CRTEXIT'              ,'CRTINIT'              ,'CSEG'
 ,'CYAN'                 ,'DARKGRAY'             ,'DELAY'
 ,'DELETE'               ,'DELLINE'              ,'DISPOSE'
 ,'DIV'                  ,'DO'                   ,'DOWNTO'
 ,'DRAW'                 ,'DSEG'                 ,'EAST'
 ,'ELSE'                 ,'END'                  ,'EOF'
 ,'EOLN'                 ,'ERASE'                ,'EXECUTE'
 ,'EXIT'                 ,'EXP'                  ,'EXTERNAL'
 ,'FALSE'                ,'FILE'                 ,'FILEPOS'
 ,'FILESIZE'             ,'FILLCHAR'             ,'FILLPATTERN'
 ,'FILLSCREEN'           ,'FILLSHAPE'            ,'FLUSH'
 ,'FOR'                  ,'FORM'                 ,'FORWARD'
 ,'FRAC'                 ,'FREEMEM'              ,'FUNCTION'
 ,'GETDIR'               ,'GETDOTCOLOR'          ,'GETMEM'
 ,'GETPIC'               ,'GOTO'                 ,'GOTOXY'
 ,'GRAPHBACKGROUND'      ,'GRAPHCOLORMODE'       ,'GRAPHMODE'
 ,'GRAPHWINDOW'          ,'GREEN'                ,'HALT'
 ,'HEADING'              ,'HEAPPTR'              ,'HI'
 ,'HIDETURTLE'           ,'HIRES'                ,'HIRESCOLOR'
 ,'HOME'                 ,'IF'                   ,'IN'
 ,'INLINE'               ,'INPUT'                ,'INSERT'
 ,'INSLINE'              ,'INT'                  ,'INTEGER'
 ,'INTR'                 ,'IORESULT'             ,'KBD'
 ,'KEYPRESSED'           ,'LABEL'                ,'LENGTH'
 ,'LIGHTBLUE'            ,'LIGHTCYAN'            ,'LIGHTGRAY'
 ,'LIGHTGREEN'           ,'LIGHTMAGENTA'         ,'LIGHTRED'
 ,'LN'                   ,'LO'                   ,'LONGFILEPOS'
 ,'LONGFILESIZE'         ,'LONGSEEK'             ,'LOWVIDEO'
 ,'LST'                  ,'LSTOUTPTR'            ,'MAGENTA'
 ,'MARK'                 ,'MAXAVAIL'             ,'MAXINT'
 ,'MEM'                  ,'MEMAVAIL'             ,'MEMW'
 ,'MEMW'                 ,'MKDIR'                ,'MOD'
 ,'MOVE'                 ,'MSDOS'                ,'NEW'
 ,'NIL'                  ,'NORMVIDEO'            ,'NORTH'
 ,'NOSOUND'              ,'NOT'                  ,'NOWRAP'
 ,'ODD'                  ,'OF'                   ,'OFS'
 ,'OR'                   ,'ORD'                  ,'OUTPUT'
 ,'OVERLAY'              ,'OVRPATH'              ,'PACKED'
 ,'PALETTE'              ,'PARAMCOUNT'           ,'PARAMSTR'
 ,'PATTERN'              ,'PENDOWN'              ,'PENUP'
 ,'PI'                   ,'PLOT'                 ,'PORT'
 ,'PORTW'                ,'POS'                  ,'PRED'
 ,'PROCEDURE'            ,'PROGRAM'              ,'PTR'
 ,'PUTPIC'               ,'RANDOM'               ,'RANDOMIZE'
 ,'READ'                 ,'READLN'               ,'REAL'
 ,'RECORD'               ,'RED'                  ,'RELEASE'
 ,'RENAME'               ,'REPEAT'               ,'RESET'
 ,'REWRITE'              ,'RMDIR'                ,'ROUND'
 ,'SEEK'                 ,'SEEKEOF'              ,'SEEKEOLN'
 ,'SEG'                  ,'SET'                  ,'SETHEADING'
 ,'SETPENCOLOR'          ,'SETPOSITION'          ,'SHL'
 ,'SHOWTURTLE'           ,'SHR'                  ,'SIN'
 ,'SIZEOF'               ,'SOUND'                ,'SOUTH'
 ,'SQR'                  ,'SQRT'                 ,'SSEG'
 ,'STR'                  ,'STRING'               ,'SUCC'
 ,'SWAP'                 ,'TEXT'                 ,'TEXTBACKGROUND'
 ,'TEXTCOLOR'            ,'TEXTMODE'             ,'THEN'
 ,'TO'                   ,'TRM'                  ,'TRUE'
 ,'TRUNC'                ,'TRUNCATE'             ,'TURNLEFT'
 ,'TURNRIGHT'            ,'TURTLETHERE'          ,'TURTLEWINDOW'
 ,'TYPE'                 ,'UNTIL'                ,'UPCASE'
 ,'USR'                  ,'USRINPTR'             ,'USROUTPTR'
 ,'VAL'                  ,'VAR'                  ,'WEST'
 ,'WHEREX'               ,'WHEREY'               ,'WHILE'
 ,'WHITE'                ,'WINDOW'               ,'WITH'
 ,'WRAP'                 ,'WRITE'                ,'WRITELN'
 ,'XCOR'                 ,'XOR'                  ,'YCOR'
 ,'YELLOW'
);

function Reserved(var w : ReservedWord ) : boolean;
var
  low, high, mid : integer;
  done : boolean;
begin
  Reserved := False;
  done := False;
  low := 1;
  high := NumReservedWords;
  while (low <= high) and not done do
    begin
      mid := (low + high) div 2;
      if w < ReservedWordList[mid] then
        high := mid - 1
      else
      if w > ReservedWordList[mid] then
        low := mid + 1
      else
        begin
          Reserved := true;
          done := True;
        end;
    end;
end;
{.page}
procedure fill_blanks (var line: dtstr);
  var
    i : integer;
begin
  for i:= 1 to 8 do
      if line[i] = ' ' then
          line[i]:= '0';
end;  {fill_blanks}

procedure getdate(var date : dtstr);

begin
   allregs.ax := $2A * 256;
   MsDos(allregs);
   str((allregs.dx div 256):2,month);
   str((allregs.dx mod 256):2,day);
   str((allregs.cx - 1900):2,year);
   date := month + '/' + day + '/' + year;
   fill_blanks (date);
end;  {getdate}

procedure gettime(var time : dtstr);

begin
   allregs.ax := $2C * 256;
   MsDos(allregs);
   str((allregs.cx div 256):2,hour);
   str((allregs.cx mod 256):2,minute);
   str((allregs.dx div 256):2,second);
   time := hour + ':' + minute + ':' + second;
   fill_blanks (time);
end;  {gettime}

procedure WhenCreated (var date, time: dtstr; var infile: text);

const
  monthmask  = $000F;
  daymask    = $001F;
  minutemask = $003F;
  secondmask = $001F;

var
  fulltime,fulldate: integer;

begin

    allregs.ax := $57 * 256;
    allregs.bx := memw [seg(infile):ofs(infile)];
    MsDos(allregs);
    fulldate := allregs.dx;
    fulltime := allregs.cx;

    str(((fulldate shr 9) + 80):2,year);
    str(((fulldate shr 5) and monthmask):2,month);
    str((fulldate and daymask):2,day);
    date:= month + '/' + day + '/' + year;
    fill_blanks(date);

    str((fulltime shr 11):2,hour);
    str(((fulltime shr 5) and minutemask):2,minute);
    str(((fulltime and secondmask) * 2):2,second);
    time:= hour + ':' + minute + ':' + second;
    fill_blanks (time);
end;  {WhenCreated}


Procedure BuildXref (var TreePtr : XrefPtr);

var
  MoreRefs : RefsPtr;

Begin
  if TreePtr = nil then
    begin
      New(TreePtr);
      TreePtr^.RefWord  := Wordchk;
      TreePtr^.LeftPtr  := Nil;
      TreePtr^.RightPtr := Nil;
      New(TreePtr^.NextRefs);
      TreePtr^.NextRefs^.NumRefs := 1;
      TreePtr^.NextRefs^.Refs[1] := Refline;
      TReePtr^.NextRefs^.NextRefs := Nil;
    end
  else
    if Wordchk < TreePtr^.RefWord then
        BuildXref(TreePtr^.LeftPtr)
    else
    if Wordchk > TreePtr^.RefWord then
        BuildXref(TreePtr^.RightPtr)
    else
      begin
        MoreRefs := TreePtr^.NextRefs;
        While MoreRefs^.NextRefs <> Nil Do
          MoreRefs := MoreRefs^.NextRefs;
        if MoreRefs^.Refs[MoreRefs^.NumRefs] <> Refline then
          begin
            if MoreRefs^.NumRefs = RefsPerRec then
              begin
                New(MoreRefs^.NextRefs);
                MoreRefs := MoreRefs^.NextRefs;
                MoreRefs^.NumRefs := 0;
                MoreRefs^.NextRefs := Nil;
              end;
            MoreRefs^.NumRefs := MoreRefs^.NumRefs + 1;
            MoreRefs^.Refs[MoreRefs^.NumRefs] := Refline;
          end;
      end;
end;
procedure print_heading(filename : fnmtype);

var offset_inc: integer;
    temp      : integer;

begin
   if print then
     begin
       pageno := pageno + 1;
       if not (pageno = 1) then
           write(listfil, ff);  {top of form}
       linect := 0;
       for temp := 1 to top_margin do
          writeln(listfil);
       if print_opt = 'C' then
         write(listfil,rp);
       write(listfil,'TURBO Pascal Program Lister');
       writeln(listfil,' ':13,'Printed: ',sysdate,'  ',
               systime,'   Page ',pageno:4);
       if filename <> fnam then begin
          offset_inc:= 26 - length (filename);
          write(listfil,'Include File: ',filename,' ':offset_inc,
             'Created: ',filedate,'  ',filetime);
       end
       else write(listfil,'Main File: ',fnam,' ':offset,
             'Created: ',filedate,'  ',filetime);
       writeln(listfil); writeln(listfil);
       if print_opt = 'C' then
         write(listfil,cp);
       If Print_Xref then
         begin
           Writeln(Listfil,' ':40,'Cross-Reference');
           writeln(listfil,'------------------------------','  '
             ,'-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+');
         end
       else
         begin
           writeln(listfil,'  line# C B');
           writeln(listfil,'  ----- - -   ---------+---------+---------+'
              +'---------+---------+---------+---------+---------+');
        end;
       linect := top_margin + header_length;
     end; {check for print}
end;  {print_heading}

procedure printline(iptline : instring; filename : fnmtype);
begin
   if print then
     begin
       writeln(listfil,'     ',iptline);
       linect := linect + 1;
       if linect >= maxline then
         print_heading(filename);
     end; {check for print}
end;  {printline}

{.page}
function chkinc(var iptline : instring; var incflname : fnmtype) : boolean;
var
   done : boolean;
   i, j : integer;
begin
   i := 4; j := 1; incflname := '';
   if ((copy(iptline, 1, 3) = '{$I') or
       (copy(iptline, 1, 4) = '(*$I')) then begin
         if copy(iptline, 1, 4) = '(*$I' then i := 5;
         incflname := '';
         while (iptline[i] = ' ') and (i <= length(iptline)) do i := i + 1;
         done := false;
         while not done do begin
               if i <= length(iptline) then begin
                  if not (iptline[i] in [' ','}','+','-','*']) then begin
                     incflname[j] := iptline[i];
                     i := i + 1; j := j + 1;
                  end else done := true;
              end else done := true;
              if j > 14 then done := true;
         end;
         incflname[0] := chr(j - 1);
   end;
   if incflname <> '' then
     begin
          chkinc := true;
          for i := 1 to length(Incflname) do
              incflname[i] := upcase(incflname[i]);
     end
     else
         chkinc := false;
end;  {chkinc}


 PROCEDURE GET_IN_FILE;     {GETS INPUT FILE NAME }
   var
    existing : boolean;
    i        : integer;
  begin
    repeat             {until file exists}
      clrscr;
      gotoxy(25,1);
      write('TurboPrt - Release 1.5');
      if first and (ParamCount > 0) then
        fnam := ParamStr(1)
      else
        begin
          gotoxy(1,3);
          write(' Enter Filename to List or <CR> to Exit  ');
          readln(fnam);
          if fnam <> '' then
             begin
                  answer := ' ';
                  writeln;write(' Expand Includes? (Y/N) ');
                  read(Kbd,answer);
                  Answer := UpCase(Answer);
                  if answer = 'Y' then
                     expand_includes := true
                  else
                    begin
                      expand_includes := false;
                      answer := 'N';
                    end;
                  Writeln(answer);
             end
        end;

     if fnam = '' then          {***** EXIT *****}
     begin
          clrscr;
          halt;
     end;

     for i := 1 to length(fnam) do
         fnam[i] := upcase(fnam[i]);

     if pos('.',fnam) = 0 then       {file type given?}
       fnam := concat(fnam,'.PAS');  {file default to .PAS type}

     {get optional command line argument # 2}
     if first and (ParamCount > 1) then
       begin
         holdarg := ParamStr(2);
         for i := 1 to Length(holdarg) do
           holdarg[i] := UpCase(holdarg[i]);
         expand_includes := holdarg = '/I';
       end;

     assign( infile, fnam);
       {$I-}
     reset( infile );                {check for existence of file}
       {$I+}
     existing := (ioresult = 0);     {true if file found}
     if not existing then
       begin
        writeln;
        writeln(' File Doesn''t Exist!!'); {tell operator the sad news}
        sound(500);
        delay(250);
        nosound;
        delay(2000);
       end;
     if existing then
       begin                             {obtain path for include files}
          I := length(fnam);
          path_found := false;
          while ((I > 0) and Not Path_found) do
             if (fnam[i] in ['\',':']) then Path_found := true
             else I := I - 1;

          if Path_found then
          begin
             file_path := copy(fnam,1,I);
             fnam := copy(fnam,(i+1),(length(fnam)));
          end;
       end;
     first := false;        {get passed file name only once}
    until existing;                     {until file exists}


 end; {GET_IN_FILE}

{ GET_OUT_FILE procedure asks operator to select output to console
  device or list device, and then assigns and resets a file control
  block to the appropriate device.  'C' or 'P' is only correct
  response, and multiple retrys are allowed. }

Procedure Get_Out_File;
  begin
    repeat    {until good selection}
      gotoxy(1,7);
      clreol;
      write(' Output Listing to (C)onsole or (P)rinter ?  ');
      Read(Kbd,c);
      c := upcase(c); write(c);
   until c in ['C', 'P'];

   writeln;
   if c = 'C' then
    begin
      assign (listfil, 'CON:');
      print_opt := 'R';
    end
   else
      assign (listfil, 'LST:');

   reset(listfil);
   if c = 'P' then begin
     repeat
      gotoxy(1,9);
      clreol;
      Write(' (C)ompressed Print or (R)egular Print ? ');
      Read(Kbd,print_opt);
      print_opt := upcase(print_opt);
      write(print_opt);
    until print_opt in ['C','R'];
    writeln;
    if print_opt = 'R' then write(listfil,rp);
   end;
 end;  {GET_OUT_FILE}
{.page}
Procedure ListIt(filename : fnmtype); forward;



// this is where each line is scanned

Procedure Scan_Line;

{ Scan_Line procedure scans one line of Turbo Pascal source code
  looking for Begin/End pairs, Case/End pairs, Literal fields
  and Comment fields.  Bcount is begin/end and case/end counter.
  Begin/case/ends are only valid outside of comment fields and
  literal constant fields.
  Some of the code in the Scan_Line procedure appears at first glance
  to be repitive and/or redundant, but was added to speed up the
  process of scanning each line of source code.}

  var
    literal : boolean;          { true if in literal field}
    i, j    : integer;          {loop variable index}
    buff2   : instring;         {working line buffer}
    incflname : fnmtype;        {in file name}
    filedate_save : dtstr;
    filetime_save : dtstr;
  begin
    literal := false;
                                        {copy input buffer to working buffer}
    buff2 := concat(buff1, '       ');

    for i := 1 to length(buff2) do
      buff2[i] := UpCase(buff2[i]);      // Don't have to do this; uppercase works

    RefLine := RefLine + 1;              // Probably line no.

    // Include file checker
    if chkinc(buff2, incflname) and expand_includes then

     // in the case of an include file, recursively process

       begin
       for i := 1 to length(incflname) do         // unnecessary - filenames can be used as is
           incflname[i] := upcase(incflname[i]);
          if pos('.',incflname) = 0 then incflname := incflname + '.PAS';   // actually we have choice of extensions now

          printline('                      ******* Including "'
              +incflname+'" *******', incflname);
          filedate_save := filedate;  {save filedate & filetime for}
          filetime_save := filetime;  {main file                   }

          include_char := 'I';


          listit(incflname);           // recursive call
          include_char := ' ';
          filedate := filedate_save;  {restore}        // restore prior
          filetime := filetime_save;
          printline('                      *******   End of  "'
              +incflname+'" *******', incflname);
          skip_this_line := true;                       // probably ifgnores rst of line; may want to change this
       end  {include file check}

   else begin

    if ((buff2[1] = '{') and (buff2[2] = '.')) then      // {. commands
      if buff2[3] in ['L','P'] then                      // (.L or {.P
        if copy(buff2,1,7) = '{.PAGE}' then              // move to next page
          begin
            if print and (linect > (header_length + top_margin)) then
             begin
              skip_this_line := true;
              print_head := true;
             end;
          end
        else
        if copy(buff2,1,5) = '{.L+}' then                  // {.L+ /{.:- list on/off
          begin
            skip_this_line := true;
            print := true;
          end
        else
        if copy(buff2,1,5) = '{.L-}' then                    // {.L+ /{.:- list on/off
          begin
            skip_this_line := true;
            print := false;
          end;

    buff2 := concat('  ', buff2);  {add on some working space}

    i := 1;
    while buff2[i] = ' ' do                // skip blanks
        i := i + 1;

    while i <= (length(buff2) - 6) do       // if not at eol
      begin
        if not literal then   {possible to find comment delim}   // literal = "open code"
          begin               {determine if comment area delim}
           if buff2[i] in ['{', '}', '(', '*'] then     .. check for comments
             begin
               if (buff2[i] = '{') then comment_brace := true
               else
               if (buff2[i] = '}') then comment_brace := false
               else
               if (copy(buff2,i,2)='(*') then comment_paren := true
               else
               if (copy(buff2,i,2)='*)') then comment_paren := false;
             end;
         end
       else
         while buff2[i] <> chr(39) do       // inside quoted string
           i := i + 1;

         if not (comment_brace or comment_paren) then  {in comment area}
           begin
            if buff2[i] = chr(39) then
                literal := not literal;   {toggle literal flag}
            if not literal then
            begin                        .// "word_switch =
              if not Word_switch then
                  if ((buff2[i] in ['A'..'Z']) and
                      (not (buff2[i-1] in ['0'..'9','A'..'Z']))) then
                    Begin
                      Word_switch := true;
                      Wordchk := '';
                    end;
              if word_switch then
                 if (buff2[i] in ['A'..'Z','0'..'9','_']) then
                    Wordchk := concat(Wordchk,Buff2[i])
                 else
                 begin
                      word_switch := false;
                      ReservedWordCheck := Wordchk;
                      if not Reserved(ReservedWordCheck) then
                         BuildXref (WordTree)
                      else
                        begin
                          if ((Wordchk = 'BEGIN') or
                              (Wordchk = 'CASE') or
                              (Wordchk = 'REPEAT')) then
                            bcount := succ(bcount)
                          else
                          if ((Wordchk = 'END') or
                              (Wordchk = 'UNTIL')) then
                            if bcount > 0 then
                              bcount := pred(bcount);
                        end;
                 end;
            end;
           end;  { if in comment }
        i := i + 1;
        end;  { for i := }
      if comment_brace or comment_paren then
          comment_char := 'C'
      else
          comment_char := ' ';
      end;
    end;  {SCAN_LINE}
{.page}
Procedure ListIt;
  var
    infile : text;
    full_filename : fnmtype;
    end_of_it : boolean;
  begin
    if path_found then
       full_filename := concat(file_path,filename)
    else
       Full_filename := filename;
     assign(infile, full_filename);
   {$I-} reset(infile) {$I+} ;
   if IOresult <> 0 then begin
      writeln ('File ',filename,' not found.');
      halt;
   end;
     WhenCreated (filedate,filetime,infile);
     if filename = fnam then
         print_heading(filename);
         end_of_it := false;
         while not end_of_it do
           begin
            buff1 := '';
            read(infile, buff1);
            scan_line;
            if print_head then
             begin
              print_heading(filename);
              print_head := false;
             end;
            if skip_this_line then
              skip_this_line := false
            else
              if print then
                begin
                if length(buff1) > 80 then
                  buff1 := copy(buff1,1,80);
                writeln(listfil,include_char,' ',RefLine:5
                     ,' ',comment_char,bcount:2,'   ',buff1);
                linect := linect + 1;
                if linect >= maxline then
                    print_head := true;
                end;
           end_of_it := eof(infile);
           if not end_of_it then
               readln(infile);
           end;     {while not eof}
         close(infile);
  end; {ListIt}

Procedure ListXref (TreePtr : XrefPtr);

var
  MoreRefs : RefsPtr;
  i, x : integer;

Begin
  if TreePtr <> nil then
    begin
      LIstXref (TreePtr^.LeftPtr);
      MoreRefs := TreePtr^.NextRefs;
      x := 32 - Length(TreePtr^.RefWord);
      Write(listfil,TreePtr^.RefWord,' ':x);
      x := 0;
      MoreRefs := TreePtr^.NextRefs;
      While not (MoreRefs = Nil) do
        begin
          for i := 1 to MoreRefs^.NumRefs do
            begin
              if x = 10 then
                begin
                  Writeln(listfil);
                  Linect := Linect + 1;
                  if Linect >= maxline then Print_heading(fnam);
                  Write(listfil,' ':32);
                  x := 0;
                end;
              Write(listfil,MoreRefs^.Refs[i]:6);
              x := x + 1;
            end;
          MoreRefs := MoreRefs^.NextRefs;
        end;
      Writeln(listfil);
      Linect := Linect + 1;
      if Linect >= maxline then Print_heading(fnam);
      ListXref (TreePtr^.RightPtr);
    end;
end;
{.page}
  begin {main procedure}
     lowvideo;
     getdate(sysdate);
     gettime(systime);
     expand_includes := false;       {default settings}
     First := True;
     Mark(heaptop);

   repeat {forever}

     Print_opt := ' ';
     WordTree := nil;
     ClrScr;
     GotoXY(2, 2);
     get_in_file;      {file to list}
     offset := 29 - length(fnam);
     get_out_file;     {where to list it}
     pageno  := 0;
     linect  := 0;      {output line counter}
     RefLine := 0;
     bcount  := 0;
     print := true;
     skip_this_line := false;
     print_head := false;
     Print_xref := False;
     word_switch:= False;
     comment_brace := false;
     comment_paren := false;
     comment_char := ' ';
     include_char := ' ';
     listit(fnam);
     Print_Xref := True;
     Print_heading(fnam);
     Listxref(WordTree);
     if Print_opt = 'C' then
       write(listfil,rp);
     writeln(listfil,ff);
     Release (heaptop); {purge previous cross reference}
     write(cr, lf, 'Hit Any Key to Continue ');
     Read(Kbd,c);
   until false;        {repeat forever - exit is in GET_IN_FILE PROCEDURE}
 end.  {main procedure}
