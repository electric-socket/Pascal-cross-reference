{
BEGIN/END Pair Checker for Turbo Pascal.
Type BEGINEND ? for an explanation.
written 1/26/86, Kim Kokkonen, TurboPower Software.
Telephone 408-378-3672. Compuserve 72457,2131.
released to the public domain.
requires Turbo Pascal version 3 to compile.
For MSDOS version only. Minor modifications for CP/M.
compile with default options. Heap/Stack requirements are minimal.
}

{$V-}
{$I-}
{$C-}
{$G128,P512}

PROGRAM BeginEnd(Output);
 {-writes a listing to check for mismatched BEGIN/END pairs}

CONST
 version:STRING[5]='1.00';
 optiondelim='-';{character used to introduce a command line option}
 maxnumwrd=128;{max number of words per line}
 maxwrdchr=128;{max number of chars in a word}
 maxnest=40;{max nesting level of BEGINs}
 nr=9;{number of key words}
 endchar:ARRAY[1..4] OF Char=
 ('''','}','*',#00);{end characters for comments and literals}

TYPE
 regpack=RECORD
          CASE Integer OF
           1:(ax,bx,cx,dx,bp,si,di,ds,es,flags:Integer);
           2:(al,ah,bl,bh,cl,ch,dl,dh:Byte);
         END;
 word=STRING[maxwrdchr];
 keywords=ARRAY[1..nr] OF word;
 lineword=ARRAY[0..maxnumwrd] OF word;
 lineflag=ARRAY[0..maxnumwrd] OF Boolean;
 lineval=ARRAY[1..maxnumwrd] OF Integer;
 linebuf=STRING[255];
 filestring=STRING[64];
 Textfile=Text[1024];

VAR
 reg:regpack;
 tstart,tstop:Real;
 incname,infile:filestring;
 err,inf,incf:Textfile;
 lw:lineword;
 litflag:lineflag;
 li:linebuf;

 nestlev,wct,litnum,lcount:Integer;

 savenest:ARRAY[0..maxnest] OF Integer;
 declflag:ARRAY[0..maxnest] OF Boolean;
 procname:ARRAY[0..maxnest] OF word;

 incfile,including,startofproc,verbose,getprocname,showlines,
 consoleout,endofproc,commflag:Boolean;

CONST
 keys:keywords=(
  'BEGIN','END','PROCEDURE','FUNCTION','FORWARD',
  'EXTERNAL','PROGRAM','END.','CASE'
  );

 FUNCTION stupcase(s:word):word;
  {-return uppercase value of string}
 VAR
  i:Integer;
 BEGIN
  FOR i:=1 TO Length(s) DO s[i]:=UpCase(s[i]);
  stupcase:=s;
 END;{stupcase}

 FUNCTION iostat(bit:Integer):Boolean;
  {-check status of the standard I/O}
  {bit=0 for input, 1 for output}
  {returns true if I/O is through console}
 VAR
  temp0,temp1:Boolean;
 BEGIN
  reg.ax:=$4400;
  reg.bx:=bit;{standard input or output}
  MsDos(reg);
  temp0:=(reg.dx AND 128)<>0;
  temp1:=(reg.dx AND (1 SHL bit))<>0;
  iostat:=temp0 AND temp1;
 END;{iostat}

 FUNCTION breakpressed:Boolean;
  {-true if Break key has been pressed}
  {-note that keypressed function executes int 23 if ^C has been pressed}
 VAR
  c:Char;
  breakdown:Boolean;
 BEGIN
  {check current state}
  breakdown:=False;
  WHILE KeyPressed AND NOT(breakdown) DO BEGIN
   Read(Kbd,c);
   IF c=^C THEN breakdown:=True;
  END;
  breakpressed:=breakdown;
 END;{breakpressed}

 PROCEDURE breakhalt;
  {-executed when break is detected}
  {-exit with return code 1}
 BEGIN
  Halt(1);
 END;{breakhalt}

 PROCEDURE setbreak;
  {-set the ctrl-break address to a process exit handler}
 BEGIN
  reg.ax:=$2523;
  reg.ds:=CSeg;
  reg.dx:=Ofs(breakhalt);
  MsDos(reg);
 END;{setbreak}

 PROCEDURE time(VAR sec:Real);
  {-return time of day in seconds since midnight}
 BEGIN
  reg.ah:=$2C;
  MsDos(reg);
  sec:=1.0*(reg.dh+60.0*(reg.cl+60.0*reg.ch)+reg.dl/100.0);
 END;{time}

 PROCEDURE defaultextension(extension:filestring;VAR infile:filestring);
  {-assign a default extension to a DOS 2.0+ pathname}
  {extension should be a maximum of 3 characters, and does not include dot}
 VAR
  i:Integer;
  temp:filestring;
 BEGIN
  i:=Pos('..',infile);
  IF i=0 THEN
   temp:=infile
  ELSE
   {a pathname starting with ..}
   temp:=Copy(infile,i+2,64);
  i:=Pos('.',temp);
  IF i=0 THEN infile:=infile+'.'+extension;
 END;{defaultextension}


 PROCEDURE scanfile(VAR inf:Textfile);
  {-main routine to read file and do the work}
 VAR
  w:word;
  i:Integer;
  localline:Integer;

  PROCEDURE parseline(VAR wct:Integer);
   {-strip leading blanks and parse line into "words"}
  VAR
   startpos,lpos,len:Integer;
   getincname,lit,oldword:Boolean;
   cnext,c:Char;

   PROCEDURE checkinclude;
    {-see if an include file is requested}
   VAR
    c1,c2,c3:Char;
   BEGIN
    c1:=li[Succ(lpos)];c2:=UpCase(li[lpos+2]);c3:=li[lpos+3];
    IF (c1='$') AND (c2='I') AND NOT((c3='-') OR (c3='+')) THEN BEGIN
     incfile:=True;
     getincname:=True;
    END;
   END;{checkinclude}

   PROCEDURE startinc;
    {-get the filename for the include file and finish up the current line}
   VAR
    ch:Char;
    gotstart:Boolean;

    PROCEDURE stripblanks(VAR s:filestring);
     {-remove leading and trailing blanks from s}
    VAR
     l,t,len:Integer;
    BEGIN
     l:=0;
     len:=Length(s);
     REPEAT
      l:=Succ(l);
     UNTIL (l>len) OR (s[l]<>' ');
     t:=Succ(len);
     REPEAT
      t:=Pred(t);
     UNTIL (t<1) OR (s[t]<>' ');
     s:=Copy(s,l,Succ(t-l));
    END;{stripblanks}

   BEGIN
    IF including THEN BEGIN
     WriteLn;
     WriteLn('cannot nest include files...');
     Halt;
    END;
    incname:='';
    lpos:=lpos+2;{skip $i}
    gotstart:=False;
    REPEAT
     lpos:=Succ(lpos);
     IF li[lpos]<>' ' THEN gotstart:=True;
     incname:=incname+li[lpos];
     ch:=li[Succ(lpos)];
    UNTIL (ch='*') OR (ch='}') OR (gotstart AND (ch=' '));
    stripblanks(incname);
    defaultextension('PAS',incname);
    getincname:=False;
   END;{startinc}

   PROCEDURE saveword;
    {-store the parsed word into the word array}
   BEGIN
    lw[wct]:=Copy(li,startpos,lpos-startpos);
    wct:=Succ(wct);
    litflag[wct]:=commflag;
    oldword:=False;
    startpos:=lpos;
   END;{saveword}

   PROCEDURE singdelim;
    {-make a one char delimiter word}
   BEGIN
    IF oldword THEN saveword;
    lw[wct]:=c;
    wct:=Succ(wct);
    litflag[wct]:=commflag;
    oldword:=False;
    startpos:=Succ(lpos);
   END;{singdelim}

   PROCEDURE doubdelim;
    {-make a two character delimiter word}
   BEGIN
    IF oldword THEN saveword;
    lw[wct]:=c+cnext;
    wct:=Succ(wct);
    litflag[wct]:=commflag;
    oldword:=False;
    lpos:=Succ(lpos);{move over an extra character}
    startpos:=Succ(lpos);
   END;{doubdelim}

   PROCEDURE dopart;
    {-interpret partial delimiters}
   BEGIN
    IF lpos<len THEN BEGIN
     cnext:=li[Succ(lpos)];
     IF (cnext='=') OR (cnext='>') THEN doubdelim ELSE singdelim;
    END ELSE singdelim;
   END;{dopart}

   PROCEDURE startlit;
    {-start a new comment or constant string}

    PROCEDURE setlit;
     {-initialize the flags}
    BEGIN
     lit:=True;
     oldword:=True;
     litflag[wct]:=True;
    END;{setlit}

   BEGIN
    IF oldword THEN saveword;
    IF c='''' THEN BEGIN
     litnum:=1;
     setlit;
    END ELSE IF c='{' THEN BEGIN
     litnum:=2;
     setlit;
     commflag:=True;
     IF (lpos+2)<len THEN checkinclude;
    END ELSE IF c='!' THEN BEGIN
     litnum:=4;
     setlit;
    END ELSE IF lpos<len THEN BEGIN
     IF ((c='(') AND (li[Succ(lpos)]='*')) THEN BEGIN
      litnum:=3;
      setlit;
      commflag:=True;
      lpos:=Succ(lpos);
      IF (lpos+2)<len THEN checkinclude;
     END ELSE singdelim;
    END ELSE singdelim;
   END;{startlit}

   PROCEDURE endlit;
    {-see if the comment or literal string has been terminated}
   BEGIN
    IF c='}' THEN BEGIN
     commflag:=False;
     lit:=False;
    END ELSE IF c='*' THEN BEGIN
     IF ((lpos<len) AND (li[Succ(lpos)]=')')) THEN BEGIN
      commflag:=False;
      lit:=False;
      lpos:=Succ(lpos);{move past the ')'}
     END;{else keep the literal going}
    END ELSE IF (lpos=len) OR (li[Succ(lpos)]<>'''') THEN
     lit:=False
    ELSE
     {keep the literal going, skip the next ' character}
     lpos:=Succ(lpos);
   END;{endlit}

  BEGIN{parseline}
   wct:=0;len:=Length(li);
   IF len>0 THEN BEGIN

    lpos:=0;
    REPEAT{skip leading blanks}
     lpos:=Succ(lpos);
     IF lpos>len THEN Exit;
    UNTIL (li[lpos]<>' ');

    lit:=commflag;{initialize line variables}
    wct:=1;
    litflag[1]:=commflag;
    oldword:=commflag;
    startpos:=lpos;
    getincname:=False;

    REPEAT{scan through the line}
     c:=li[lpos];
     IF lit THEN BEGIN
      {we only care to find the end marker of the literal}
      IF c=endchar[litnum] THEN endlit;
      lpos:=Succ(lpos);
      IF NOT(lit) THEN saveword;
     END ELSE BEGIN{literal flag not on}
      IF (c<'A') OR (c>'z') THEN BEGIN
       {delimiters and numbers}
       IF (c='!') OR (c='''') OR (c='{') OR (c='(') THEN BEGIN
        {comment delimiters}
        startlit;
        IF getincname THEN BEGIN
         startinc;
        END;
       END ELSE BEGIN
        IF (c<'0') OR (c>'9') THEN BEGIN
         {non-comment delimiters}
         IF (c=' ') OR (c=^I) THEN BEGIN
          IF oldword THEN saveword;
          startpos:=Succ(lpos);
         END ELSE IF (c=':') OR (c='<') OR (c='>') THEN
          dopart
         ELSE IF (c=')') OR (c=';') OR (c=',') OR (c='=') OR
         (c='+') OR (c='-') OR (c='*') OR (c='/') THEN singdelim
         ELSE oldword:=True;
        END ELSE oldword:=True;
       END;
      END ELSE BEGIN
       {normal characters except for [,],^}
       IF (c='[') OR (c=']') THEN singdelim
       ELSE oldword:=True;
      END;
      lpos:=Succ(lpos);
     END;

    UNTIL lpos>len;
    IF oldword THEN saveword;
    wct:=Pred(wct);
   END;
  END;{parseline}

  FUNCTION keyword(VAR w:word):Boolean;
   {-see if a word is a keyword and capitalize it}
  VAR
   j:Integer;
  BEGIN
   w:=stupcase(w);
   keyword:=False;
   FOR j:=1 TO nr DO
    IF w=keys[j] THEN BEGIN
     keyword:=True;
     Exit;
    END;
  END;{keyword}

  PROCEDURE anakey(VAR w:word);
   {-analyze a keyword for effect on indentation, etc}

   PROCEDURE check(nestlev:Integer);
    {-make sure nestlev falls into legal bounds}
   BEGIN
    IF nestlev>maxnest THEN BEGIN
     WriteLn(err);
     WriteLn(err,'Exceeded program nesting capacity...');
     Halt(2);
    END;
    IF nestlev<0 THEN BEGIN
     WriteLn(err);
     WriteLn(err,'Too many END statements...');
     Halt(2);
    END;
   END;{check}

  BEGIN
   IF (w='BEGIN') THEN BEGIN
    IF declflag[nestlev] THEN BEGIN
     startofproc:=True;
     savenest[nestlev]:=nestlev;
    END ELSE BEGIN
     nestlev:=Succ(nestlev);
     savenest[nestlev]:=-1;
    END;
    declflag[nestlev]:=False;
   END ELSE IF w='END' THEN BEGIN
    IF NOT(declflag[nestlev]) THEN BEGIN
     IF nestlev=savenest[nestlev] THEN
      endofproc:=True;
     nestlev:=Pred(nestlev);
    END;
   END ELSE IF w='CASE' THEN BEGIN
    IF NOT(declflag[nestlev]) THEN BEGIN
     nestlev:=Succ(nestlev);
     declflag[nestlev]:=False;
     savenest[nestlev]:=-1;
    END;
   END ELSE IF (w='PROCEDURE') OR (w='FUNCTION') THEN BEGIN
    nestlev:=Succ(nestlev);
    getprocname:=True;
    declflag[nestlev]:=True;
   END ELSE IF (w='FORWARD') OR (w='EXTERNAL') THEN BEGIN
    {ignore forward and external declaration lines}
    nestlev:=Pred(nestlev);
   END ELSE IF w='PROGRAM' THEN BEGIN
    getprocname:=True;
   END ELSE IF w='END.' THEN BEGIN
    endofproc:=True;
    nestlev:=Pred(nestlev);
   END ELSE
    Exit;
   check(nestlev);
  END;{anakey}

  PROCEDURE increment(VAR lcount,localline:Integer);
   {-increment line counter and check for wraparound}
  BEGIN
   lcount:=Succ(lcount);
   IF lcount<0 THEN lcount:=0;
   localline:=Succ(localline);
  END;{increment}

  PROCEDURE writeoutline(lcount,localline:Integer;li:linebuf);
   {-format the output line}
  VAR
   l,n:linebuf;
  BEGIN
   IF showlines THEN BEGIN
    Str(lcount:5,n);
    l:='{'+n;
    IF verbose THEN BEGIN
     l:=l+' (';
     IF including THEN
      l:=l+stupcase(incname)
     ELSE
      l:=l+stupcase(infile);
     Str(localline:5,n);
     l:=l+' '+n+')';
    END;
    l:=l+'} '+li;
   END ELSE
    l:=li;
   IF consoleout THEN
    WriteLn(Copy(l,1,79))
   ELSE
    WriteLn(l);
  END;{writeoutline}

  PROCEDURE writemarker(p:word;m:Char);
   {-write a marker at begin or end of a procedure}
  VAR
   l:linebuf;
  BEGIN
   FillChar(l[1],78,m);
   l[0]:=Chr(79);
   l[1]:='{';
   Move(p[1],l[2],Length(p));
   l[79]:='}';
   WriteLn(l);
  END;{writemarker}

  PROCEDURE doincludeit;
   {-includes include files}
  BEGIN
   Assign(incf,incname);
   Reset(incf);
   IF IOResult<>0 THEN BEGIN
    WriteLn(err);WriteLn(err);
    WriteLn(err,'include file ',incname,' not found');
    Close(inf);
    Halt(2);
   END;
   incfile:=False;
   including:=True;
   scanfile(incf);
   including:=False;
  END;{doincludeit}

 BEGIN
  localline:=0;
  REPEAT

   {read the input file, line by line}
   ReadLn(inf,li);

   {status checking and display}
   IF breakpressed THEN breakhalt;
   increment(lcount,localline);
   IF NOT(consoleout) THEN
    IF (lcount AND 15=0) THEN
     Write(err,^M,lcount);

   {break the line into words to be analyzed}
   parseline(wct);

   i:=1;
   WHILE i<=wct DO BEGIN
    {pass through line and interpret words}
    IF NOT(litflag[i]) THEN BEGIN
     w:=lw[i];
     IF getprocname THEN BEGIN
      procname[nestlev]:=w;
      getprocname:=False;
     END;
     IF keyword(w) THEN anakey(w);
    END;
    i:=Succ(i);
   END;{of word scan}

   {write out the line and any pre or post lines}
   IF startofproc THEN BEGIN
    writemarker(stupcase(procname[nestlev]),'+');
    startofproc:=False;
   END;

   {here is the user line}
   writeoutline(lcount,localline,li);

   IF endofproc THEN BEGIN
    writemarker(stupcase(procname[Succ(nestlev)]),'-');
    endofproc:=False;
   END;

   IF IOResult<>0 THEN BEGIN
    WriteLn(err);
    WriteLn(err,'error during write....');
    Halt(2);
   END;

   {include the include file if found}
   IF incfile THEN doincludeit;

  UNTIL EoF(inf);
  Close(inf);
 END;{scanfile}

 PROCEDURE setval;
  {-interpret command line and prepare for run}
 VAR
  j:Integer;
  haltsoon:Boolean;
  arg:linebuf;

  PROCEDURE writehelp;
   {-write a help screen}
  BEGIN
   WriteLn(err,'Usage:  BEGINEND [MainSourceFile] [>OutPutFile]');
   WriteLn(err);
   WriteLn(err,'  BEGINEND writes a formatted listing of the program to the standard');
   WriteLn(err,'  output device. The listing includes the global line number in front');
   WriteLn(err,'  of each source line, and optionally the include file name and the');
   WriteLn(err,'  line number within the include file.');
   WriteLn(err);
   WriteLn(err,'  More importantly, the BEGIN and END statements that mark entry and');
   WriteLn(err,'  exit from procedures and functions are marked with a dividing line.');
   WriteLn(err,'  The dividing line is composed of +++ characters on entry and ---');
   WriteLn(err,'  characters on exit. Each dividing line is also labeled with the name of');
   WriteLn(err,'  the associated procedure. Mismatched BEGIN/END pairs will become');
   WriteLn(err,'  obvious when you see that the marker lines are out of whack.');
   WriteLn(err);
   WriteLn(err,'Options:');
   WriteLn(err,'  -V  Verbose mode. Writes include file name and local line number');
   WriteLn(err,'  -N  No line numbers are written before any line.');
   WriteLn(err,'  -?  Writes this help message.');
   Halt(0);
  END;{writehelp}

 BEGIN
  infile:='';
  haltsoon:=False;
  {scan the argument list}
  j:=1;
  WHILE j<=ParamCount DO BEGIN
   arg:=ParamStr(j);
   IF (arg='?') OR (arg=optiondelim+'?') THEN
    writehelp
   ELSE IF (arg[1]=optiondelim) AND (Length(arg)=2) THEN
    CASE UpCase(arg[2]) OF
     'V':verbose:=True;
     'N':showlines:=False;
    ELSE
     WriteLn(err,'unrecognized command option');
     haltsoon:=True;
    END
   ELSE BEGIN
    {input file name}
    infile:=arg;
    {add default extension}
    defaultextension('PAS',infile);
    {make sure it exists}
    Assign(inf,infile);
    Reset(inf);
    IF IOResult<>0 THEN BEGIN
     WriteLn(err,stupcase(infile),' not found or marked READ-ONLY....');
     haltsoon:=True;
    END;
   END;
   j:=Succ(j);
  END;
  IF infile='' THEN BEGIN
   WriteLn(err,'no input file specified....');
   Halt;
  END ELSE IF haltsoon THEN BEGIN
   WriteLn(err,'type BEGEND ? for help');
   Halt;
  END;
 END;{setval}

 PROCEDURE getfiles;
  {-get filenames,open up and assure good files}
 VAR
  good:Boolean;

  FUNCTION getyesno(m:linebuf;default:Char):Boolean;
   {-return true for yes, false for no}
  VAR
   ch:Char;
  BEGIN
   WriteLn(err);
   Write(err,m);
   REPEAT
    Read(Kbd,ch);
    IF ch=^C THEN BEGIN
     WriteLn('^C');
     Halt;
    END;
    IF ch=#13 THEN ch:=default ELSE ch:=UpCase(ch);
   UNTIL (ch IN ['Y','N']);
   WriteLn(err,ch);
   getyesno:=(ch='Y');
  END;{getyesno}

 BEGIN
  REPEAT
   Write(err,'Enter input file name (<cr> to quit): ');
   ReadLn(infile);
   IF Length(infile)=0 THEN Halt;
   {add default extension}
   defaultextension('PAS',infile);
   Assign(inf,infile);
   Reset(inf);
   IF IOResult<>0 THEN BEGIN
    WriteLn(err,stupcase(infile),' not found or marked READ-ONLY. try again....');
    good:=False;
   END ELSE
    good:=True;
  UNTIL good;
  showlines:=
  getyesno('Show global line numbers preceding each line? (<cr> for Yes) ','Y');
  IF showlines THEN
   verbose:=
   getyesno('Show Include file names and local line numbers? (<cr> for No) ','N')
  ELSE
   verbose:=False;
  WriteLn(err);
 END;{getfiles}

 PROCEDURE initglobals;
 BEGIN
  setbreak;
  lcount:=0;nestlev:=1;
  commflag:=False;incfile:=False;getprocname:=False;
  startofproc:=False;endofproc:=False;showlines:=True;
  including:=False;incfile:=False;verbose:=False;
  declflag[1]:=True;
  procname[1]:='PROGRAM';
  consoleout:=iostat(1);
  Assign(err,'ERR:');
  Rewrite(err);
  WriteLn(err);
  WriteLn(err,'Pascal BEGIN/END Checker - by TurboPower Software - Version ',version);
  WriteLn(err);
 END;{initglobals}

 PROCEDURE showrate;
 VAR
  deltat:Real;
 BEGIN
  Write(err,^M'total lines processed: ',lcount);
  deltat:=tstop-tstart;
  IF deltat>0 THEN BEGIN
   WriteLn(err,'  rate: ',(lcount/deltat):6:1,' LPS');
  END ELSE WriteLn(err);
 END;{showrate}

BEGIN{petmod}
 initglobals;
 IF ParamCount=0 THEN getfiles ELSE setval;
 time(tstart);
 scanfile(inf);
 time(tstop);
 showrate;
END.
                                                                    