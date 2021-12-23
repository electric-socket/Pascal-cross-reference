unit Scan;

interface


Procedure ScanInit;


implementation
Const
    MAXUNITS                  = 254;
    MAXEXTENSIONS             = 10;
    // Note: If you add new tokens,
    // TTOKENKIND, NUMKEYWORDS, and KEYWORD must **ALL** be adjusted.
    KWSize                    = 30;
    COMMENTMAX                = 20;   // maximum number of comment types
    MAXFOLDERS                = 25;

    Months: array[1..12] of string[3]=
            ('Jan','Feb','Mar','Apr', 'May','Jun',
             'Jul','Aug','Sep','Oct','Nov', 'Dec');
    Days: Array[0..6] of string[9]=
            ('Sun','Mon','Tue','Wed','Thu','Fri','Sat');



type

    UsageP      = ^TUsage;     // uses of cross-referenced item
    TUsage = Record    // Cross reference
          Next: UsageP;
          Page,                     // Location in listing
          Line,
          LocalLine: Integer;       // line number in unit
    end;

    ProcP = ^TProc;
    Callp = ^TCall;                 // procedure callby, callto
    TCall = Record
          Prev,Next:Callp;
          Proc: ProcP;
    end;


    Tproc = Record                    // procedure list
          ProcNumber: Integer;
          ProcName: String;
          UnitNumber: Byte;
          isFunction: Boolean;
          Calledby,
          Calls: CallP;
          Next,
          Prev: ProcP;
    end;

    CrossP      = ^TCross;     // cross reference item
    TCross = Record
       Prev,                  // Pointers to items in list
       Next: CrossP;
       UnitNumber:Byte;
       Name,                  // Name of this identifier
       ProcName: String;     // if local to a procedure
       Page,                  // Location in listing
       Line,
       LocalLine: Integer;    // line number in unit/file
       Usage: UsageP;         // List of usage
    end;

    PParam = ^TParam;

    TParam = record
      Name: TString;
      DataType: Integer;
      PassMethod: TPassMethod;
      Prev,
      Next:  PParam;
      Default: TConst;
    end;


 TSignature = record
    NumParams: Integer;
    NumDefaultParams: Integer;
    Param: PParam;
    Line,
    ResultType: Integer;

 end;


  TIdentKind = (EMPTYIDENT, UNITIDENT, GOTOLABEL, CONSTANT, USERTYPE, VARIABLE, PROC, FUNC);
  IdentP      = ^TIdentifier;// A specific identifier
  TIdentifier = record
    Prev,                    // Links for when this is a linked list
    Next: IdentP;            // instead of an array
    Cross: CrossP;           // pointer to its cros-reference list
    Kind: TIdentKind;        // CONST, TYPE, PROC, FUNC, etc.
    Name: String;
    Signature: TSignature;
    DeclaredLine,            // Line Bumber in file it was declared
    DefinedLine,             // For forward procedures and functions where it is actually defined
    UnitIndex,               // Unit it is defined in
    Block: Integer;          // Index of a block in which the identifier is defined
  end;

  CommentBlock = Record
        CStart:String[2];    // what the comment oprns with
        CEnd:String[2];      // what it closes with - ignoed for line comments
        isBlock,             // is a "bloick" comment, extending until cloure.
                             // a "line comment" only runs to end of lind
        isTwo: boolean;      // does the comment use both chars
  end;


  TTokenKind = (
        EMPTYTOK,

        COMMENTTOK,      // comments
        IDENTTOK,        // identifiers
        INTNUMBERTOK,    // integer
        REALNUMBERTOK,   // reals

        // conditional compilation tokens
        CCSIFTOK,        // $IF
        CCSIFDEFTOK,     // $IFDEF
        CCSIFNDEFTOK,    // $IFNDEF
        CCSELSEIFTOK,    // $ELSEIF
        CCSELSETOK,      // $ELSE
        CCSENDIFTOK,     // $ENDIF
        CCSDEFINETOK,    // $DEFINE
        CCSUNDEFTOK,     // $UNDEF

        JUNKTOK,         // anything not recognized
        MODIFIERTOK,     // modifier, dependent on special
        PROCEDURALTOK,   // procedure or function
        KEYWORDTOK,      // ordinary keyword
        BLOCKTOK,        // this keyword starts a block
        ENDTOK           // this keyword ends a block
      );

  
  BlockKind = (NoBlock, ProcBlock,   BeginBlock,  CaseBlock, RepeatBlock, IgnoreBlock,
               RecordBlock, ObjectBlock);

  Closures = (NoClose, ENDClose, UntilClose);

  Specials = (SpecNothing, SpecProcFunc, SpecUnitProg, SpecUses,
              SpecObject,  SpecConst,    SpecType,     SpecRecord,
              SpecWith,    SpecDo,       SpecLabel,    SpecVar);
  PKeyword = ^TKeyword;
  KWString = string[KwSize]
  TKeyword = record
       Next,             // Links
       Prev: PKeyword;
       Keyword: KWstring;
       Count: Integer;    // Times used
       Block:BlockKind;   // starts a block
       Close:Closures;    // ends a block
       isModifier,
       Sentinel: Boolean; // is this the middle of the list
       Special: Specials;
   End;

   BlockP = ^TBlock;
   TBlock = record
          Prev,
          Next: BlockP;
          ProcName: String;
          BlockType: BlockKind;
          Closure: Closures;

   end;
   TBuffer = record
        Ptr: PChar;
        Size,                            // size of file
        Pos: Integer;                    // position in file
      end;

    TToken = record
       Name: String;     // For easy analysis, this is always in upper case
       DeclaredLine:Integer;    // Where it was declared
       case Kind: TTokenKind of
           IDENTTOK:         (NonUppercaseName: String);  // This is needed
                                                                // in the case of EXTERNAL procedures where the name
                                                                // might have to be in mixed case
           INTNUMBERTOK:     (OrdValue: LongInt);    // For all other ordinal types
           REALNUMBERTOK:    (RealValue: Double);
    end;

    TUnit = record
        Name: ^String;
        UnitNumber: Byte;
        UsedBy,                                  // Units that use this one
        UsedUnits: set of Byte;
    end;

    PScannerState = ^TScannerState;
    TScannerState = record
       Token: TToken;      // Current token
    FileName,
    Folder,
    BaseName,
    Suffix: String;  // name of file
       Position,           // current position in
       Line: Integer;      // line being examined
       Buffer: TBuffer;
       ch, ch2: Char;
       inComment,           // we're inside a comment
       EndOfFile: Boolean;  // end of file
       Prev,
       Next: PScannerState;
     end;



Var
    F,             // input
    List,          // text
    PDF,           // pdf
    Html: File;    // HTML

    Extensions: array [1..MAXEXTENSIONS] of String;
    ExtensionCount: Integer;

    Units: Array[1..MaxUnits] of TUnit;
    UnitIndex: Byte;

    ScannerBase,ScannerState: PScannerState;
    KeywordBase,
    Keywords: PKeyword;

    CommentTable: Array[1..CommentMax] of CommentBlock;
    CommentCount: Integer;

    CrossBase,            // base and top of cross-refernce table
    CrossTop: CrossP;     // cross reference item

    StartTime: TSystemTime;


    Folders: array [1..MAXFOLDERS] of String;
    NumFolders: Integer;


Function Instr(A,B:String):Integer;
var
    I,K,Bp,
    LA,LB:Integer;
begin
    Result := 0;
    La := Length(A);
    Lb := Length(B);
    if Lb>La THEN EXIT;
    fOR I:= 1 TO la DO
    BEGIN
        iF lb+i>la THEN break;
        BP :=1;
        fOR k := I TO I+Lb DO
        BEGIN
            if A[K] <> B[BP] then break;
            inc(BP);
        END;
        if BP<>LB then Continue;
        Result :=I;
        exit
    end;
END;


procedure SplitPath(const Path: String; var Folder, Name, Ext: String);
var
    DotPos, SlashPos, i: Integer;
begin
    Folder := '';
    Name := Path;
    Ext := '';

    DotPos := 0;
    SlashPos := 0;

    for i := Length(Path) downto 1 do
        if (Path[i] = '.') and (DotPos = 0) then
            DotPos := i
        else if (Path[i] = '\') and (SlashPos = 0) then
            SlashPos := i;

    if DotPos > 0 then
    begin
        Name := Copy(Path, 1, DotPos - 1);
        Ext  := Copy(Path, DotPos, Length(Path) - DotPos + 1);
    end;

    if SlashPos > 0 then
    begin
        Folder := Copy(Path, 1, SlashPos);
        Name   := Copy(Path, SlashPos + 1, Length(Name) - SlashPos);
    end;

end;


Procedure OpenFile(Name:String; NoLoop:Boolean);
VAR
    ActualSize,
    FolderIndex,
    Ext:Integer;
    Found: Boolean;


begin
    with Scannerstate^ do
    begin
// First search the source folder, then the units folder, then the folders
// specified in $UNITPATH
        FolderIndex := 1;
        SplitPath(Name,Folder,BaseName,Suffix);

        FileMode :=0; // force read-only in case file is read only
        Found := FALSE;
        repeat
            For Ext := 0 to NumFolders do
            begin
                if found then  break;
                if Ext =0 then // First time, try the original name
                   FileName := Name
                else if noloop then
                   break       // don't try reshaping name of main program
                else If Extensions[Ext]='' then
                    break // try next folder
                else
                    FileName := BaseName + Extensions[Ext];
                Assign(F, (Folders[FolderIndex] + FileName ));
                Reset(F, 1);
                if IOResult = 0 then Found :=True;
            end;
                Inc(FolderIndex);
        until Found or (FolderIndex > NumFolders);
        if FolderIndex > NumFolders then
            Catastrophic('Fatal: Unable to open source file "' + Name+'".');

        Line := 1;
        Position := 0;
//        FileName := Name;
        with Buffer do
        begin
           Size := FileSize(F);
           Pos := 0;
           GetMem(Ptr, Size );
           ActualSize := 0;
           BlockRead(F, Ptr^, Size , ActualSize);
           Close(F);
           if ActualSize <> Size then
              Catastrophic('Fatal: Unable to read source file ' + Name);
        end;
        ch  := ' ';
        EndOfFile := FALSE;
    end;
end;


procedure NextCh;
begin
    with Scannerstate^ , Buffer do
      begin
        CH := #0;
        if Pos <= Size then
        begin
            ch := PChar(Integer(Ptr) + Pos)^;
            if ch=#10 then
               Inc(line);
            Inc(Pos);
        end
        else
            EndOfFile := TRUE;
      end;
end;

procedure AddKeyword(TheItem:String;
                     TheBlock: BlockKind;
                     TheClose: Closures;
                     TheSpecial: Specials );

   Procedure InitKeyword;
   begin
        Keywords^.Next:= NIL;
        Keywords^.Count:=0;
        Keywords^.Keyword:=TheItem;
        Keywords^.Block:=TheBlock;
        KeyWords^.Close:=TheClose;
        Keywords^.Sentinel:=False;
        Keywords^.isModifier := False;
        Keywords^.Special:=TheSpecial;
   end;

Begin
    If KeywordBase = NIL then
    begin
        New(KeywordBase);
        Keywords := KeywordBase;
        Keywords^.Prev := NIL;
        InitKeyword;
        exit;
    End;
    Keywords := KeywordBase;
    while keywords^.Next <> Nil do
       Keywords := Keywords^.Next;
    New(Keywords^.Next);
    Keywords^.Next^.Prev := Keywords;
    Keywords^ := Keywords^.Next
    InitKeyword;
end;

// A modifier is a special kind of Keyword
// it is only a reserved word in a certain
// context. Like 'Forward' which is only
// a keyword when used after a procedure
// or function definition
Procedure AddModifier(TheItem:String; Modifies:Specials);
begin
    AddKeyWord(TheItem, NoBlock,NoClose, Modifies);
    // "Keywords" now points to the item just added
    KeyWords^.isModifier := True;

end;


Procedure CreateComment(StartC,EndC:String2; Block:Boolean);
begin
    Inc(CommentCount);
    If CommentCount>CommentMax then
         Catastrophic('Fatal: Too Many Comment types.');;
    CommentTable[CommentCount].CStart   := StartC;
    CommentTable[CommentCount].CEnd     := EndC;
    CommentTable[CommentCount].isBlock  := Block;
    CommentTable[CommentCount].isTwo    := Length(StartC)=2;
end;


Procedure ScanInit;
begin

    CommentCount := 0;

    PasPath := ParamStr(1);
    SplitPath(PasPath,     PasFolder,     PasName,     temp);
    Folders[1] := PasPath;
    Folders[2] := ProgramPath;
    Folders[3] := Paspath+'units\';
    Folders[4] := ProgramPath+'units\';
    NumFolders := 4;

    GetLocalTime(StartTime);
    TimeString := Days[StartTime.dayOfWeek]+' '+Months[StartTime.month]+
                  ' '+IntToStr(StartTime.day)+', '+IntToStr(StartTime.year);
    TimeString := TimeString+' '+IntToStr(StartTime.Hour)+
                  ':'+I2(StartTime.Minute)+':'+I2(StartTime.Second);


     // If a keyword is marrked as beng a block, it continues until
     // the close symbol is found. If a sybol has no block but
     // has a close symbol,, then it is that terminator
     // keywords are converted  to upper case, so they must be
     // upper case when defined.
    AddKeyword('AND',      NoBlock,   NoClose,  SpecNothing);
    AddKeyword('ARRAY',    NoBlock,   NoClose,  SpecNothing);
    AddKeyword('BEGIN',    BeginBlock,ENDClose, SpecNothing);
    AddKeyword('CASE',     CaseBlock, ENDClose, SpecNothing);
    AddKeyword('CONST',    NoBlock,   NoClose,  SpecConst);
    AddKeyword('DIV',      NoBlock,   NoClose,  SpecNothing);
    AddKeyword('DO',       NoBlock,   NoClose,  SpecDo);
    AddKeyword('DOWNTO',   NoBlock,   NoClose,  SpecNothing);
    AddKeyword('ELSE',     NoBlock,   NoClose,  SpecNothing);
    AddKeyword('END',      NoBlock,   ENDClose, SpecNothing);
    AddKeyword('FILE',     NoBlock,   NoClose,  SpecNothing);
    AddKeyword('FOR',      NoBlock,   NoClose,  SpecNothing);
    AddKeyword('FUNCTION', NoBlock,   NoClose,  SpecProcFunc);
    AddKeyword('GOTO',     NoBlock,   NoClose,  SpecNothing);
    AddKeyword('IF',       NoBlock,   NoClose,  SpecNothing);
    AddKeyword('IN',       NoBlock,   NoClose,  SpecNothing);
    AddKeyword('LABEL',    NoBlock,   NoClose,  SpecLabel);
    AddKeyword('MOD',      NoBlock,   NoClose,  SpecNothing);
    AddKeyword('NIL',      NoBlock,   NoClose,  SpecNothing);
    AddKeyword('NOT',      NoBlock,   NoClose,  SpecNothing);
    AddKeyword('OF',       NoBlock,   NoClose,  SpecNothing);
    AddKeyword('OR',       NoBlock,   NoClose,  SpecNothing);
    AddKeyword('OTHERWISE',NoBlock,   NoClose,  SpecNothing);
    AddKeyword('PACKED',   NoBlock,   NoClose,  SpecNothing);
    AddKeyword('PROCEDURE',NoBlock,   NoClose,  SpecProcFunc);
    AddKeyword('PROGRAM',  NoBlock,   NoClose,  SpecNothing);
    AddKeyword('RECORD',   RecordBlock, ENDClose,   SpecNothing);
    AddKeyword('REPEAT',   RepeatBlock, UntilClose, SpecNothing);
    AddKeyword('SET',      NoBlock,   NoClose,  SpecNothing);
    AddKeyword('THEN',     NoBlock,   NoClose,  SpecNothing);
    AddKeyword('TO',       NoBlock,   NoClose,  SpecNothing);
    AddKeyword('TYPE'      NoBlock,   NoClose,  SpecType);
    AddKeyword('UNTIL',    NoBlock,   UntilClose, SpecNothing);
    AddKeyword('VAR',      NoBlock,   NoClose,  SpecVar)
    AddKeyword('WHILE',    NoBlock,   NoClose,  SpecNothing);
    AddKeyword('WITH',     NoBlock,   NoClose,  SpecWith);
    CreateComment('(*','*)',True);
    CreateComment('{','}',True);
    CreateComment('//','',False);
    CreateComment('''','''',True); // for our purposes, we treat quote as a comment

    AddModifier('FORWARD', SpecProcFunc);
end;

end.


