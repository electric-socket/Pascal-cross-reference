Uses Crt;
{}
CONST MaxCorpus=10000;
      MaxDepth=200;
      MaxFiles=200; (* max insert files *)
   CONST DefaultColumns=70;
   CONST DefaultLines=60;
TYPE
     aFunctionType=(WhoKnows,aProcedure,aFunction);
     pDeclaration=^aDeclaration;
     pCaller=^aCaller;
     pSTRING=^STRING;
     aCaller=RECORD _Itself: pDeclaration;
		  _Next: pCaller;
		  _FileWhereFound: pSTRING;
                  CallLine: WORD;
	   END;
     aDeclaration=RECORD
           _FileWhereFound: pSTRING;
           Forwarded: BOOLEAN;
           _FileWhereForwarded: pSTRING;
           LineWhereForwarded: WORD;
	   StartLine,EndLine,Depth: WORD;
	   _Owner: pDeclaration;
	   _Caller: pCaller;
	   FunctionType: aFunctionType;
	   Body: STRING;
	END;
     aCharFile= TEXT;
     aState=(inProgram,inQuotes,inComment,
	   StartCommentMaybe,EndCommentMaybe,
           PerhapsInclude);


VAR CommentDelimiter: (Curly,Parenthesis);
    Dictionary: ARRAY[1..MaxCorpus] OF pDeclaration;
    IncludedFileName: ARRAY[0..MaxFiles] OF pSTRING;
    FileNbr: BYTE; (* index to IncludedFileName *)
    WordFound: BOOLEAN; (* Result of FindStringAt *)
    There: WORD; (* where found in index, or where to be inserted *)
    DictionarySize,CurrentDepth,CurrentLine: WORD;
    Continue,FunctionBeginsNext: BOOLEAN;
    State: aState;
    _Main: pDeclaration;

    WhatNext: aFunctionType;
    CurrentFunctions: INTEGER;
    CallerAt: ARRAY[1..MaxDepth] OF pDeclaration;
    ProgramName: STRING;

CONST DeclarationSize=SizeOf(aDeclaration)-SizeOf(STRING);


PROCEDURE MakeMain;
CONST Main='Main ProgramMain Program';
BEGIN
  GetMem(CallerAt[1],Length(Main)+1+DeclarationSize);
		       (*         ^ the length byte *)
  FillChar(CallerAt[1]^,Length(Main)+1+DeclarationSize,0);

  CallerAt[1]^.Body:=Main;
  CallerAt[1]^.Body[0]:=#12;
END;

PROCEDURE AddFunction(f: pDeclaration);
BEGIN
   Inc(CurrentFunctions);
   CallerAt[CurrentFunctions]:=f;
END;

PROCEDURE CloseFunction;
BEGIN WITH CallerAt[CurrentFunctions]^ DO
      BEGIN EndLine:=CurrentLine
      END;
      Dec(CurrentFunctions);
      FunctionBeginsNext:=CurrentFunctions>0;
END;

FUNCTION Capitalize(st: STRING): STRING;
VAR i: BYTE;
BEGIN FOR i:=1 TO Length(st) DO st[i]:=Upcase(st[i]);
      Capitalize:=st;
END;

PROCEDURE MakeID(fname: STRING; VAR id: STRING);
(* Returns a string "id" consisting of:
   function name, capitalized, starting with class identifier,
   function name, as found.
   the length byte is the length of the function name,
   i.e. half the actual length of the string returned
*)
VAR n,i: BYTE;
    Leader,Method: STRING;
    Underscore1,Underscore2: INTEGER;
BEGIN n:=Length(fname);
      IF n>125 THEN BEGIN n:=125; fname[0]:=#125 END;
      Leader:=fname;
      FOR i:=1 TO n DO Leader[i]:=Upcase(Leader[i]);
      Underscore1:=Pos('_',Leader);
      IF Underscore1>1 THEN (* save method *)
      BEGIN Method:=Copy(Leader,1,Underscore1-1);
	    Delete(Leader,1,Underscore1);
	    Underscore2:=Pos('_',Leader);
	    IF Underscore2>0 THEN
	    (* insert method after underscore *)
	    Insert(Method,Leader,Underscore2+1)
	    ELSE (* it wasn't a method after all, put it back *)
	    Leader:=Method+Leader;
	    Leader:='_'+Leader;
      END;

      id:=Leader+fname;
      id[0]:=CHR(n)
END;

FUNCTION StateName(s: aState): STRING;
(* for debugging purposes *)
BEGIN
  CASE s OF
     inProgram: StateName:='in program';
     inQuotes:  StateName:='in quotes';
     inComment: StateName:='comment';
     StartCommentMaybe: StateName:='comment next?';
     EndCommentMaybe: StateName:='end comment?';
  END;
END;

FUNCTION WordOfRank(n: INTEGER): STRING;
BEGIN
  WordOfRank:=Dictionary[n]^.Body
END;

PROCEDURE ReadDictionaryFrom(VAR Mainfile: aCharFile);
VAR ch,Delimiter: CHAR;
    CurrentWord,UCurrentWord,NewWord: STRING;
    RestoreDelimiter: BOOLEAN;
    Buffer: STRING;
    BufPos,BufLength: BYTE;
    IncludedFile: aCharFile;
    CurrentFile: ^aCharFile;
    IncludeFile: BOOLEAN; (* true when must include file on
                             exiting {$I....} *)
    InfileName: STRING;   (* the name of the file to include *)
    InMainFile: BOOLEAN;  (* true on entry, false when in included file*)
    _LastDeclaration: pDeclaration;
    MainFileState: RECORD
        Delimiter: CHAR;
        CurrentWord,UCurrentWord,NewWord: STRING;
        RestoreDelimiter: BOOLEAN;
        Buffer: STRING;
	BufPos,BufLength: BYTE;
	CurrentLine: WORD;
       END;

    PROCEDURE GetInfileName;
    BEGIN Inc(BufPos); ch:=Buffer[BufPos];
          IF ch>' ' THEN (* not an include file command *)
          BEGIN InfileName:=''; IncludeFile:=FALSE;
          END
          ELSE
          BEGIN
            WHILE ch<=' ' DO BEGIN Inc(BufPos); ch:=Buffer[BufPos]; END;
            InfileName:=ch;
            REPEAT Inc(BufPos); ch:=Buffer[BufPos];
                   InfileName:=InfileName+ch
            UNTIL ch IN [' ',^I,'*','}'];
            IncludeFile:=TRUE;
            (* now rub out that delimiter *)
            Dec(InfileName[0]);
            Dec(BufPos);
          END
    END;

    PROCEDURE EnterIncludedFile(VAR fn: STRING);
    BEGIN (* first, save Main file info *)
       MainFileState.Delimiter:=Delimiter;
       MainFileState.CurrentWord:=CurrentWord;
       MainFileState.UCurrentWord:=UCurrentWord;
       MainFileState.NewWord:=NewWord;
       MainFileState.RestoreDelimiter:=RestoreDelimiter;
       MainFileState.Buffer:=Buffer;
       MainFileState.BufPos:=BufPos;
       MainFileState.BufLength:=BufLength;
       MainFileState.CurrentLine:=CurrentLine;
       (* record included file name *)
       Inc(FileNbr);
       New(IncludedFileName[FileNbr]);
       IncludedFileName[FileNbr]^:=InfileName;

       (* now enter included file *)
       Assign(IncludedFile,fn); Reset(IncludedFile);
       CurrentFile:=@IncludedFile;
       InMainFile:=FALSE;
       WhatNext:=WhoKnows;
       CurrentLine:=0;
       FunctionBeginsNext:=FALSE;
       State:=inProgram;
       IncludeFile:=FALSE;
    END; {EnterIncludedFile}

    PROCEDURE ReenterMainFile;
    BEGIN (* restore Main file info *)
       Delimiter:=MainFileState.Delimiter;
       CurrentWord:=MainFileState.CurrentWord;
       UCurrentWord:=MainFileState.UCurrentWord;
       NewWord:=MainFileState.NewWord;
       RestoreDelimiter:=MainFileState.RestoreDelimiter;
       Buffer:=MainFileState.Buffer;
       BufPos:=MainFileState.BufPos;
       BufLength:=MainFileState.BufLength;
       CurrentLine:=MainFileState.CurrentLine;
       (* exit included file, and enter main file *)
       Close(IncludedFile);
       CurrentFile:=@MainFile;
       WhatNext:=WhoKnows;
       FunctionBeginsNext:=FALSE;
       State:=inProgram;
       InMainFile:=TRUE;
    END; {ReenterMainFile}


    FUNCTION FunctionEnd: BOOLEAN;
    BEGIN FunctionEnd:=(CurrentFunctions>0)
	  AND (CallerAt[CurrentFunctions]^.Depth=CurrentDepth)

    END;

    PROCEDURE Identify(VAR _Function,_Caller: pDeclaration;
             VAR NewWord: STRING; CurrentDepth: WORD);
    VAR lo,hi,mid: LONGINT; First,Last,Here: WORD;
    _f: pDeclaration;
    Found: BOOLEAN;
    BEGIN Found:=FALSE; _Function:=NIL; _Caller:=NIL;
      lo:=1; hi:=DictionarySize;
      WHILE (lo<=hi) AND NOT Found DO
      BEGIN
	mid:=(lo+hi) DIV 2;
	_f:=Dictionary[mid];
	IF  (NewWord<_f^.Body) THEN hi:=mid-1 ELSE
	IF  (NewWord>_f^.Body) THEN lo:=mid+1
        ELSE Found:=TRUE;
      END;
      IF NOT Found THEN Exit;
      IF Found THEN
      BEGIN IF CurrentFunctions<1 THEN _Caller:=CallerAt[1]
            ELSE
	    _Caller:=CallerAt[CurrentFunctions];
            _Function:=_f
      END;
    END;

    FUNCTION InsertionPointOf(VAR s: STRING): WORD;
    VAR lo,hi,mid: LONGINT;
    _f: pDeclaration;
    (* Found: BOOLEAN; *)
    BEGIN
      lo:=1; hi:=DictionarySize;
      WHILE (lo<=hi) (* AND NOT Found *) DO
      BEGIN
	mid:=(lo+hi) DIV 2;
	_f:=Dictionary[mid];
	IF  (s<_f^.Body) THEN hi:=mid-1 ELSE
	IF  (s>_f^.Body) THEN lo:=mid+1 ELSE
        IF  (CurrentLine<_f^.StartLine) THEN hi:=mid-1
        ELSE lo:=mid+1;
      END;
      InsertionPointOf:=hi+1;
    END;

    PROCEDURE AddCaller(_f,_Function: pDeclaration; Line: WORD);
    VAR  _u: pCaller;
    BEGIN WITH _Function^ DO
    BEGIN IF _Caller=NIL THEN
      BEGIN New(_Caller);
            FillChar(_Caller^,SizeOf(_Caller^),0);
            _Caller^._Next:=NIL;
	    _Caller^._Itself:=_f;
            _Caller^.CallLine:=Line;
            IF NOT InMainFile THEN
            _Caller^._FileWhereFound:=IncludedFileName[FileNbr];
      END
      ELSE
      BEGIN _u:=_Caller;
            WHILE _u^._Next<>NIL DO _u:=_u^._Next;
            New(_u^._Next);
            _u:=_u^._Next;
            FillChar(_u^,SizeOf(_u^),0);
	    _u^._Itself:=_f;
            _u^._Next:=NIL;
            _u^.CallLine:=Line;
            IF NOT InMainFile THEN
            _u^._FileWhereFound:=IncludedFileName[FileNbr];

      END
    END
    END; {AddCaller}


    PROCEDURE Load(VAR NewWord: STRING; CurrentLine: WORD);
    VAR n: WORD; There: WORD;
    BEGIN  There:=InsertionPointOf(NewWord);
	   IF There<=DictionarySize THEN (* make room there *)
	   Move(Dictionary[There],
		Dictionary[There+1],
		4*(DictionarySize-There+1));
           n:=(Length(NewWord))*2+1;
	   GetMem(Dictionary[There],n+DeclarationSize);
           _LastDeclaration:=Dictionary[There];
           FillChar(Dictionary[There]^,n+DeclarationSize,0);
	   WITH Dictionary[There]^ DO
	   BEGIN
                 Move(NewWord,Body[0],n);
                 IF NOT InMainFile THEN
                 _FileWhereFound:=IncludedFileName[FileNbr];
                 StartLine:=CurrentLine;
                 Depth:=CurrentDepth;
                 FunctionType:=WhatNext;
	   END;
           AddFunction(Dictionary[There]);
	   Inc(DictionarySize);
    END;

    PROCEDURE ProcessWord(c: CHAR);
    VAR functionID: WORD;
        _Caller,_Function: pDeclaration;
      FUNCTION IsReturnValue: BOOLEAN;
      BEGIN IsReturnValue:=FALSE;
            WHILE (c<=' ') AND (BufPos<BufLength)
            DO BEGIN Inc(BufPos); c:=Buffer[BufPos];
               RestoreDelimiter:=TRUE;
            END;
            IF (c=':') AND (BufPos<BufLength) THEN
            BEGIN
              Inc(BufPos);
              Delimiter:=Buffer[BufPos];
              IsReturnValue:= Delimiter='=';
            END;
      END; {IsReturnValue}
    BEGIN {ProcessWord}
      IF CurrentWord='' THEN Exit;
      RestoreDelimiter:=FALSE;
      UCurrentWord:=Capitalize(CurrentWord);
      IF (UCurrentWord='BEGIN') THEN
         BEGIN IF FunctionBeginsNext THEN
            (* it is the first BEGIN after a function
               declaration, for which depth has already
               been incremented, so do nothing other
               than ... *)
            FunctionBeginsNext:=FALSE
            ELSE Inc(CurrentDepth)
         END
         ELSE
      IF (UCurrentWord='CASE')
      OR (UCurrentWord='RECORD') THEN Inc(CurrentDepth)
	 ELSE
      IF (UCurrentWord='END') THEN
         BEGIN  IF FunctionEnd THEN CloseFunction;
                Dec(CurrentDepth);
         END
         ELSE
      IF (UCurrentWord='PROCEDURE') THEN
	 BEGIN WhatNext:=aProcedure;
               Inc(CurrentDepth);
               FunctionBeginsNext:=TRUE;
	 END
	 ELSE
      IF (UCurrentWord='FUNCTION') THEN
	 BEGIN WhatNext:=aFunction;
               Inc(CurrentDepth);
               FunctionBeginsNext:=TRUE;
	 END
         ELSE
      IF (UCurrentWord='FORWARD') AND FunctionBeginsNext THEN
         BEGIN (* last function was only a forward declaration,
                  retrieve it and fix it *)
               IF _LastDeclaration<>NIL THEN
               WITH _LastDeclaration^ DO
               BEGIN
                    Forwarded:=TRUE;
                    New(_FileWhereForwarded);
                    _FileWhereForwarded^:=_FileWhereFound^;
                    LineWhereForwarded:=StartLine;
                    FunctionBeginsNext:=FALSE;
                    Dec(CurrentDepth);
                    Dec(CurrentFunctions);
               END;
         END
         ELSE
      IF (WhatNext=aProcedure) OR (WhatNext=aFunction) THEN
	 BEGIN
	       MakeID(CurrentWord,NewWord);
               (* check if already in dictionary *)
               Identify(_Function,_Caller,NewWord,CurrentDepth);
               IF _Function=NIL THEN
	       Load(NewWord,CurrentLine)
               ELSE WITH _Function^ DO
               BEGIN StartLine:=CurrentLine;
                     AddFunction(_Function);
                     Depth:=CurrentDepth;
                     IF InMainFile THEN
                     _FileWhereFound:=NIL
                     ELSE _FileWhereFound:=IncludedFileName[FileNbr];
                     FunctionBeginsNext:=TRUE;
               END;
	       WhatNext:=WhoKnows;
	 END
      ELSE BEGIN
		 MakeID(CurrentWord,NewWord);
                 Identify(_Function,_Caller,NewWord,CurrentDepth);
                 IF _Caller<>NIL THEN
                 BEGIN IF (_Caller^.FunctionType=aFunction)
                        AND IsReturnValue
                        THEN RestoreDelimiter:=TRUE
                        ELSE
                        AddCaller(_Caller,_Function,CurrentLine);
                 END;
         END;
         CurrentWord:='';
    END; {ProcessWord}

    PROCEDURE ProcessBuffer;
    VAR ch: CHAR;
    BEGIN WHILE BufPos<BufLength DO
       BEGIN
         IF RestoreDelimiter THEN
         BEGIN ch:=Delimiter;
               RestoreDelimiter:=FALSE;
         END
	 ELSE
	 BEGIN Inc(BufPos); ch:=Buffer[BufPos];
	 END;
	 IF (State=inProgram) OR (State=StartCommentMaybe)
         THEN
	 CASE (ch) OF
	      'A'..'Z','a'..'z','0'..'9','_':
		   CurrentWord:=CurrentWord+ch;
	      '*': ;
	      ELSE
                  ProcessWord(ch);
                  State:=inProgram;
	 END;
         CASE ch OF
           '''': CASE State OF
		    inProgram,StartCommentMaybe: State:=inQuotes;
		    inQuotes:  State:=inProgram
                 END;
	   '{':  IF State=inProgram
           	 THEN BEGIN State:=inComment;
                      CommentDelimiter:=Curly;
                 END;
	   '}':  IF (State=inComment)
                 AND (CommentDelimiter=Curly) THEN
                 BEGIN State:=InProgram;
                       IF IncludeFile THEN
                       BEGIN
                           EnterIncludedFile(InfileName)
                       END;
                 END;
	   '(':  IF State=inProgram THEN State:=StartCommentMaybe;
           '*':  CASE State OF
		    StartCommentMaybe:
                       BEGIN State:=inComment;
                             CommentDelimiter:=Parenthesis
                       END;
		    inComment: IF CommentDelimiter=Parenthesis
                               THEN State:=EndCommentMaybe;
		    inProgram: ProcessWord(ch);
                 END;
	   ')':  IF  (State=EndCommentMaybe)
                 AND (CommentDelimiter=Parenthesis)
                 THEN BEGIN
                      State:=InProgram;
                      IF IncludeFile THEN EnterIncludedFile(InfileName)
                 END;
           '$':  IF State=inComment THEN State:=PerhapsInclude;
           'I','i':
                 IF State=PerhapsInclude
                 THEN BEGIN GetInfileName;
                      (* into InfileName,
                         and make IncludeFile true *)
                      State:=inComment
                 END;
	   #10:  BEGIN Inc(CurrentLine);
                       GotoXy(1,1); write(CurrentLine:6);
                 END;

	END;
      END;
      IF (State=inProgram) THEN ProcessWord(ch);
    END; {ProcessBuffer}

BEGIN  {ReadDictionary}
    CurrentFile:=@MainFile; InMainFile:=TRUE;
    Reset(CurrentFile^);
    CurrentWord:=''; RestoreDelimiter:=FALSE;
    IncludeFile:=FALSE;
    ClrScr;
    WHILE NOT Eof(Mainfile) AND Continue DO
    BEGIN Readln(CurrentFile^,Buffer);
          BufPos:=0; BufLength:=Length(Buffer);
          Inc(CurrentLine);
          Gotoxy(1,1); write(CurrentLine:6);
          ProcessBuffer;
          IF Eof(CurrentFile^) AND NOT Eof(Mainfile)
          THEN (* end of included file, so *)
          ReenterMainfile;
    END;
END; {ReadDictionary}

PROCEDURE WriteIndex(VAR f: TEXT; fn: STRING;
          LinesPerPage,Columns: INTEGER);

VAR Declaration,
    PreviousCaller,
    PreviousCallerFile,
    PreviousClass,ClassName,Line,
    FileName: STRING;
    i: WORD;
    n,Column,LineNo: INTEGER;
   _u: pCaller; _f: pDeclaration;
    PageFed: BOOLEAN;

    PROCEDURE NewLine;
    BEGIN writeln(f);
          IF LineNo=LinesPerPage THEN
          BEGIN writeln(f,^L); PageFed:=TRUE; LineNo:=0;
          END ELSE PageFed:=FALSE;
          Inc(LineNo);
    END;

    FUNCTION ClassFor(i: WORD): STRING;
    VAR str: STRING; p: INTEGER;
    BEGIN ClassFor:='';
	  str:=Dictionary[i]^.Body;
	  IF str[1]='_' THEN
	  BEGIN Delete(str,1,1);
		p:=Pos('_',str);
		IF p>0 THEN ClassFor:=Copy(str,1,p-1)
	  END;
    END;

    FUNCTION HeaderFor(i: WORD): STRING;
    VAR n: INTEGER; Declaration: STRING; Start,Fin: STRING[5];
    BEGIN WITH Dictionary[i]^ DO
    BEGIN  n:=Length(Body); Move(Body[n+1],Declaration[1],n);
      Declaration[0]:=CHR(n);
      IF Length(Declaration)>Columns-8 THEN
      BEGIN Declaration[0]:=CHR(Columns-8);
            Declaration[Length(Declaration)]:='*'
      END;
      Str(StartLine,Start); Str(EndLine,Fin);
      IF FunctionType=aProcedure
      THEN Declaration:=Declaration+' PROCEDURE'
      ELSE
      IF FunctionType=aFunction
      THEN Declaration:=Declaration+' FUNCTION';
      IF _FileWhereFound<>NIL
      THEN Declaration:=Declaration+' in '+_FileWhereFound^;
      Declaration:=Declaration+' ('+Start+'..'+Fin+')';
      IF Forwarded THEN
      BEGIN Str(LineWhereForwarded,Start);
          Declaration:=Declaration+' forward in '+_FileWhereForwarded^
                       +' ('+Start+')';
      END;
    END;
    HeaderFor:=Declaration;
  END;

  FUNCTION FileFor(i: INTEGER): STRING;
  BEGIN WITH Dictionary[i]^ DO
  BEGIN IF _FileWhereFound=NIL THEN FileFor:=''
        ELSE FileFor:=_FileWhereFound^
  END;
  END;

  PROCEDURE WriteCaller(i: INTEGER);
  VAR _u: pCaller; n: INTEGER;
      CallerName,FileName: STRING;
      FeedLine: BOOLEAN;
      PROCEDURE WriteClassNameContinued;
      BEGIN
        IF ClassName<>'' THEN
	BEGIN write(f,'Class '+ClassName);
              IF ClassName=PreviousClass THEN write(f,' continued');
              NewLine;
	END;
      END;
      PROCEDURE WriteDeclaration;
      BEGIN write(f,Declaration); NewLine
      END;
      PROCEDURE WriteCaller;
      BEGIN
           write(f,'':5,CallerName); Column:=5+Length(CallerName);
      END;
  BEGIN
    WITH Dictionary[i]^ DO
    BEGIN IF _Caller=NIL THEN
      BEGIN write(f,' **UNUSED**');
      END ELSE
      BEGIN PreviousCaller:='';
            FeedLine:=FALSE;
        _u:=_Caller;
        WHILE _u<>NIL DO
        BEGIN WITH _u^ DO
	   BEGIN
	     n:=Length(_Itself^.Body);
             Move(_Itself^.Body[n+1],CallerName[1],n);
	     CallerName[0]:=CHR(n);
             IF Length(CallerName)>Columns-10 THEN
             BEGIN CallerName[0]:=CHR(Columns-10);
                   CallerName[Length(CallerName)]:='*';
             END;

             IF CallerName<>PreviousCaller THEN
             BEGIN PreviousCaller:=CallerName;
                   PreviousCallerFile:='?';
                   FeedLine:=TRUE;
             END;
             IF _FileWhereFound=NIL THEN FileName:=''
             ELSE FileName:=_FileWhereFound^;
             Str(CallLine,Line);
             IF _FileWhereFound=NIL THEN FileName:=''
             ELSE FileName:=_FileWhereFound^;
             IF FileName<>PreviousCallerFile THEN
             BEGIN IF FileName<>'' THEN
                   Line:='in '+FileName+': '+Line;
                   PreviousCallerFile:=FileName;
             END;
             IF FeedLine THEN
             BEGIN NewLine; FeedLine:=FALSE;
		   IF PageFed THEN
		   BEGIN WriteClassNameContinued;
                         WriteDeclaration;
                   END;
                   WriteCaller;
             END;

             IF Column+Length(Line)+1>Columns THEN
             BEGIN NewLine;
                   IF PageFed THEN
		   BEGIN WriteClassNameContinued;
		         WriteDeclaration;
                         WriteCaller;
                   END;
                   write(f,'':8); Column:=8
             END;

             Write(f,' '+Line); Inc(Column,Length(Line)+1);
           END;
           _u:=_u^._Next;
        END;
      END;
  END;
  END;

BEGIN {WriteIndex}
      writeln(f,fn); writeln(f);
      LineNo:=3;
      PreviousClass:='';
      FOR i:=1 TO DictionarySize DO
      BEGIN Declaration:=HeaderFor(i);
	    ClassName:=ClassFor(i);
            FileName:=FileFor(i);
	    IF (ClassName<>'') AND (ClassName<>PreviousClass)
	    THEN
	    BEGIN NewLine;
                  writeln(f,'Class '+ClassName);
		  PreviousClass:=ClassName;
	    END;
	    write(f,Declaration);
            writeCaller(i);
            NewLine;
      END;
END;


PROCEDURE GetParameters(VAR p1,p2: STRING; VAR Lin,Col: INTEGER);
VAR st: ARRAY[1..4] OF STRING; i,n: INTEGER;

   FUNCTION FileExists(VAR fn: STRING): BOOLEAN;
   VAR f: FILE; Exists: BOOLEAN;
   BEGIN Assign(f,fn); {$I-} Reset(f); {$I+};
         Exists:=IOResult=0;
         (* writeln;
         write(fn,' reset and ');
         IF Exists THEN writeln('found.') ELSE writeln(' not found.');
         *)
         IF Exists THEN BEGIN FileExists:=TRUE; Close(f) END
         ELSE FileExists:=FALSE;
   END;
   FUNCTION GetInt(str: STRING): INTEGER;
   VAR n: LONGINT; ErrPos: INTEGER;
   BEGIN WHILE (str<>'') AND NOT (str[1] IN ['0'..'9']) DO
         Delete(str,1,1);
         Val(str,n,ErrPos);
         IF ErrPos=0 THEN Getint:=-Maxint ELSE GetInt:=n;
   END;

   PROCEDURE AppendPasTo(VAR str: STRING);
   VAR DotPos: INTEGER;
   BEGIN
         DotPos:=Length(str)+1;
         REPEAT Dec(DotPos);
         UNTIL (str[Dotpos]='.')
         OR (DotPos=Length(str)-3)
         OR (DotPos=1);
         IF str[DotPos]<>'.' THEN str:=str+'.PAS';
   END;
   PROCEDURE GetProgramFile;
   VAR fn: STRING; Ask, Abort: BOOLEAN;
   BEGIN Abort:=FALSE;
      IF (st[1]='') THEN Ask:=TRUE ELSE
      BEGIN p1:=st[1];  AppendPasTo(p1);
         Ask:=NOT FileExists(p1);
         IF Ask THEN writeln('Program file '+p1+' not found.')
      END;
      WHILE Ask DO
      BEGIN
         write('Program is in file: ');
         readln(p1);
         IF (p1='') THEN BEGIN Abort:=TRUE; Ask:=FALSE END ELSE
         BEGIN AppendPasTo(p1);
           IF FileExists(p1) THEN Ask:=FALSE
           ELSE
           writeln('Program file '+p1+' not found.')
         END
      END;
      IF Abort THEN Halt;
      p1:=Capitalize(p1);
   END;
   FUNCTION OKtoOverwrite(VAR fn: STRING): BOOLEAN;
   VAR Answer: STRING; Done: BOOLEAN; i: INTEGER;
   BEGIN IF NOT FileExists(fn) THEN
         OKtoOverwrite:=TRUE
         ELSE BEGIN  OKtoOverwrite:=FALSE;
         write('Overwrite '+fn+' (y/n)? '); Done:=FALSE;
         REPEAT Readln(Answer); Answer:=Capitalize(Answer);
                IF (Answer='Y') OR (Answer='YES')
                THEN BEGIN OKtoOverwrite:=TRUE; Done:=TRUE
                     END ELSE
                IF (Answer='N') OR (Answer='NO')
                THEN BEGIN OKtoOverwrite:=FALSE; Done:=TRUE
                END
                ELSE
                BEGIN GotoXY(Length(fn)+19,WhereY-1);
                      ClrEol;
                END;
         UNTIL Done;
         END;
   END;
   PROCEDURE GetIndexFile;
   VAR Ask,Abort: BOOLEAN;
   BEGIN
      IF (st[2]='') THEN Ask:=TRUE ELSE
      BEGIN p2:=st[2];
            Ask:=NOT OKtoOverwrite(p2);
      END;
      Abort:=FALSE;
      WHILE Ask DO
      BEGIN
            write('Index goes to file: ');
            readln(p2);
            IF p2='' THEN BEGIN Abort:=TRUE; Ask:=FALSE END
            ELSE
            Ask:=NOT OKtoOverwrite(p2);
      END;
      IF Abort THEN Halt;
   END;
   PROCEDURE GetLinesPerPage;
   BEGIN Lin:=DefaultLines;
         CASE n OF
           3:    IF Upcase(st[3][1])='L' THEN Lin:=GetInt(st[3]);
           4:    IF Upcase(st[3][1])='L' THEN Lin:=GetInt(st[3])
                 ELSE
                 IF Upcase(st[4][1])='L' THEN Lin:=GetInt(st[4]);
         END;
         IF ((Lin<10) OR (Lin>132)) AND (Lin<>0) THEN Lin:=DefaultLines
   END;
   PROCEDURE GetColumns;
   BEGIN Col:=DefaultColumns;
         CASE n OF
           3:    IF Upcase(st[3][1])='C' THEN Col:=GetInt(st[3]);
           4:    IF Upcase(st[3][1])='C' THEN Col:=GetInt(st[3])
                 ELSE
                 IF Upcase(st[4][1])='C' THEN Col:=GetInt(st[4]);
         END;
         IF (Col<50) OR (Col>132) THEN Col:=DefaultColumns
   END;

BEGIN n:=ParamCount;
    FOR i:=1 TO 4 DO st[i]:='';
    FOR i:=1 TO n DO st[i]:=ParamStr(i);
    GetProgramFile;
    GetIndexFile;
    GetLinesPerPage;
    GetColumns;
END;

VAR ProgramFile: aCharFile;
    IndexFile: TEXT;
    IndexName: STRING;
    LinesPerPage,Columns: INTEGER;
BEGIN writeln; writeln;
      GetParameters(ProgramName,IndexName,LinesPerPage,Columns);
      DictionarySize:=0;
      Continue:=TRUE; (* unused so far, always true *)
      MakeMain;
      FileNbr:=0; (* index to included files: none yet *)


      Assign(ProgramFile,ProgramName); reset(ProgramFile);
      writeln;

      CurrentDepth:=1;
      WhatNext:=WhoKnows;
      CurrentLine:=0;
      FunctionBeginsNext:=FALSE;
      CurrentFunctions:=1;
      State:=inProgram;

      ReadDictionaryFrom(ProgramFile);
      Close(ProgramFile);
      Assign(IndexFile,IndexName);
      Rewrite(IndexFile);
      WriteIndex(IndexFile,ProgramName,LinesPerPage,Columns);
      Close(IndexFile);
END.