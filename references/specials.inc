 // ``THIS IS THE END OF THE UNIT

WE ARE BELOW THE END; THIS IS NOT COMPILED
//Other items

// Scanner
    // Predefined type indices
    // Note that STringTypeIndex MUST BE LAST in order to reserve space for predefined types

    ANYTYPEINDEX          = 1;      // Untyped parameter, or base type for untyped pointers and files
    INTEGERTYPEINDEX      = 2;
    SMALLINTTYPEINDEX     = 3;
    SHORTINTTYPEINDEX     = 4;
    INT64TYPEINDEX        = 5;
    INT128TYPEINDEX       = 6;
    WORDTYPEINDEX         = 7;
    BYTETYPEINDEX         = 8;
    CHARTYPEINDEX         = 9;
    BOOLEANTYPEINDEX      = 10;
    REALTYPEINDEX         = 11;     // Basic real type: 64-bit double (all temporary real results are of this type)
    CURRENCYTYPEINDEX     = 12;     // Currency: 31 digits, 27 before the decimal point, 4 after
    SINGLETYPEINDEX       = 13;
    POINTERTYPEINDEX      = 14;     // Untyped pointer, compatible with any other pointers
    FILETYPEINDEX         = 15;     // Untyped file, compatible with text files
    STRINGTYPEINDEX       = 16;     // String of maximum allowed length

// XDPW Ketwords

Keyword: array [1..NUMKEYWORDS] of String[KeywordSize] =
     (
     'AND',
     'ARRAY',
     'ASM',
     'BEGIN',
     'CASE',
     'CONST',
     'DIV',
     'DO',
     'DOWNTO',
     'ELSE',      //  10
     'END',
     'FILE',
     'FOR',
     'FUNCTION',
     'GOTO',
     'IF',
     'IMPLEMENTATION',
     'IN',
     'INTERFACE',
     'LABEL',         // 20
     'MOD',
     'NIL',
     'NOT',
     'OF',
     'OR',
     'OTHERWISE',
     'PACKED',
     'PROCEDURE',
     'PROGRAM',
     'RECORD',           // 30
     'REPEAT',
     'SET',
     'SHL',
     'SHR',
     'STRING',
     'THEN',
     'TO',
     'TYPE',
     'UNIT',
     'UNTIL',             // 40
     'USES',
     'VAR',
     'WHILE',
     'WITH',
     'XOR'        // this ties to value of XORTOK
     );

// Turbo keywords

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
 ,'{TURNRIGHT'            ,'TURTLETHERE'          ,'TURTLEWINDOW'
 ,'TYPE'                 ,'UNTIL'                ,'UPCASE'
 ,'USR'                  ,'USRINPTR'             ,'USROUTPTR'
 ,'VAL'                  ,'VAR'                  ,'WEST'
 ,'WHEREX'               ,'WHEREY'               ,'WHILE'
 ,'WHITE'                ,'WINDOW'               ,'WITH'
 ,'WRAP'                 ,'WRITE'                ,'WRITELN'
 ,'XCOR'                 ,'XOR'                  ,'YCOR'
 ,'YELLOW'
);

const
  wordlist: array[1..reserved_count] of string[14] =
    ('ABSOLUTE','ADDR','AND','ARC','ARCTAN','ARRAY','ASSIGN','AUX',
     'AUXINPTR','AUXOUTPTR','BACK',
     'BEGIN','BLOCKREAD','BLOCKWRITE','BOOLEAN','BYTE',
     'CASE','CHAIN','CHAR','CHDIR','CHR','CIRCLE','CLEARSCREEN',
     'CLOSE','CLREOL','CLRSCR','COLORTABLE','CON','CONCAT','CONINPTR',
     'CONOUTPTR','CONST',
     'CONSTPTR','COPY','COS','CRTEXIT','CRTINIT','CSEG','DELAY',
     'DELETE','DELLINE','DISPOSE',
     'DIV','DO','DOWNTO','DRAW','ELSE','END','END.','EOF','EOLN','ERASE',
     'EXECUTE','EXP','EXTERNAL','FALSE','FILE','FILEPOS','FILESIZE',
     'FILLCHAR','FILLPATTERN','FILLSCREEN','FILLSHAPE',
     'FLUSH','FOR','FORWARD','FRAC','FREEMEM',
     'FUNCTION','GETDIR','GETDOT',
     'GETMEM','GETPIC','GOTO','GOTOXY',
     'GRAPHBACKGROUND','GRAPHCOLORMODE',
     'GRAPHMODE','GRAPHWINDOW','HALT','HEAPPTR',
     'HEADING','HI','HIDETURTLE',
     'HIRES','HIRESCOLOR','HOME',
     'IF','IN','INLINE','INPUT','INSERT','INSLINE','INT','INTEGER','INTR',
     'IORESULT','KBD','KEYPRESSED','LABEL','LENGTH','LN','LO','LOWVIDEO',
     'LST','LSTOUTPTR','MARK','MAXAVAIL',
     'MAXINT','MEMAVAIL','MEMW','MKDIR','MOD',
     'MOVE','MSDOS','NEW','NIL','NORMVIDEO','NOSOUND',
     'NOT','ODD','OF','OFS','OR','ORD','OUTPUT','OVERLAY',
     'PACKED','PALETTE','PARAMCOUNT','PARAMSTR','PATTERN',
     'PENDOWN','PENUP',
     'PI','PLOT','PORT','POS','PRED','PROCEDURE',
     'PROGRAM','PTR','PUTPIC','RANDOM','RANDOMIZE','READ','READLN','REAL',
     'RECORD','RELEASE','RENAME','REPEAT','RESET',
     'REWRITE','RMDIR','ROUND','SEEK','SEEKEOF','SEEKEOLN',
     'SEG','SET','SETHEADING','SETPENCOLOR','SETPOSITION',
     'SHL','SHOWTURTLE','SHR','SIN','SIZEOF','SOUND',
     'SQR','SQRT','STR','STRING',
     'SUCC','SWAP','TEXT','TEXTBACKGROUND','TEXTCOLOR','TEXTMODE',
     'THEN','TO','TRM','TRUE','TRUNC',
     'TURNLEFT','TURNRIGHT','TURTLETHERE','TURTLEWINDOW','TYPE',
     'UNTIL','UPCASE','USR','USRINPTR','USROUTPTR','VAL','VAR',
     'WHEREX','WHEREY','WHILE','WINDOW',
     'WITH','WRAP','WRITE','WRITELN','XCOR','XOR','YCOR');

}
// Scan for include files


function chkinc(var iptline : ansistring; var incflname : ansistring) : boolean;
var
   done : boolean;
   i, j : integer;
begin
   i := 4; j := 1; incflname := '';

{
  copy should be done once or the test used to
  set the index variable
 }
  if ((copy(iptline, 1, 4) = '{$I ') then I:= 5
  else if (copy(iptline, 1, 5) = '(*$I ')) then I := 6
  else if (copy(iptline, 1, 10) = '{$INCLUDE ')) then I := 11
  else if (copy(iptline, 1, 11) = '(*$INCLUDE ')) then I := 12;
  IF i>0 THEN
{
  SKIP ALL BLANKS STARTING FROM I UNTIL NONBLANK OR Eol
  COLLECT ALL CHARS UNYIL * OR vlose brace
  IF * MAKE SURE NEXT IS close btace
  IF WE GOT EOL, THAT'S END OF NAME
  THEN LOOK FOR CLOSE
  NOW WITH FILE NAME WE CAN READ IY ONCE COMMENT CLOSES

 }
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






     const
      // dependent constants

        Digits:    set of TCharacter = ['0'..'9'];
        HexDigits: set of TCharacter = ['0'..'9', 'A'..'F'];
        Spaces:    set of TCharacter = [#1..#31, ' '];
        RadixSet:  set of TCharacter = ['0'..'9', 'A'..'I', 'J'..'R', 'S'..'Z'];
        // to allow for EBCDIC or ASCII this uses smaller blocks of chars
      Identifiers: Set of TCharacter = ['A'..'I', 'J'..'R', 'S'..'Z',
                                          'a'..'i', 'j'..'r', 's'..'z','_'];
        AlphaNums: set of TCharacter = ['0'..'9', 'A'..'I', 'J'..'R', 'S'..'Z',
                                                  'a'..'i', 'j'..'r', 's'..'z','_'];


// To count statement blocks: Begin, Repeat, Case;
// Increase on BEGIN, DECREASE on END
// increase on REPEAT, DECREASE on UNTIL
// Increase on CASE, DECREASE on END

 BlockCount: Integer =0; //< +1 on BEGIn / REPEAT / CASE; -1 on END / UNTIL
 BeginCount: Integer = 0;     //< Starts at 0 each proc/func,
                              // +1 on begin, -1 on END

 FirstStatement: Boolean;     //< Is this the first statement on a line?

 LastIdentifier: String;      //< previous identifier, used in modifier assignment
 Skipping: Boolean;           //< Global Var to indicate in Skipping mode (See Conditional unit)



** Symbol Table from XDPW:**

IdentP      = ^TIdentifier;//< Pointer to aA specific identifier
PField = ^TField;                   //< pointer to a record field
PParam = ^TParam;
PParams = array [1..MAXPARAMS] of PParam;
TByteSet  = set of Byte; // used for units
TCallConv = (DEFAULTCONV, STDCALLCONV, CDECLCONV); // default, cdecl, stdcall

TConst = packed record
            case Kind: TTypeKind of
            INTEGERTYPE: (OrdValue: LongInt);         // For all ordinal types
            REALTYPE:    (RealValue: Double);
            INT64TYPE:   (Int64Value:TInt64);
            ENUMTYPE:    (EnumValue: integer);  // enumerations
            SINGLETYPE:  (SingleValue: Single);
            ARRAYTYPE:   (StrValue: TShortString);
            POINTERTYPE: (PointerValue: Pointer);
            SETTYPE:     (SetValue: TByteSet);        // For all set types
end;

// a record field
TField = record
  Name: TString;                    //< field name
  DataType: Integer;                //< datatype
  Offset: Integer;                  //< offset in record
end;

TIdentKind = (EMPTYIDENT, UNITIDENT, GOTOLABEL, CONSTANT,
              USERTYPE, VARIABLE, PROC, FUNC, UNDEFIDENT);

TParam = record
         Name: TString;
         DataType: Integer;
         PassMethod: TPassMethod;
         Default: TConst;
         end;

TPassMethod = (EMPTYPASSING, VALPASSING, CONSTPASSING, VARPASSING);

TPredefProc =
  (
  EMPTYPROC,

  // Procedures
  INCPROC,      // inc
  DECPROC,      // dec
  READPROC,
  WRITEPROC,
  READLNPROC,
  WRITELNPROC,
  INLINEPROC,     // INLINE(n,N);
  NEWPROC,        // new
  DISPOSEPROC,    // dispose
  BREAKPROC,      // These four: Break, Continue,
  CONTINUEPROC,   // Exit, and Halt, are styled as
  EXITPROC,       // if they were procedures but are
  HALTPROC,       // handled internally by the compiler

  // Functions
  SIZEOFFUNC,     // SIZEOF(
  ORDFUNC,
  CHRFUNC,
  LOWFUNC,
  HIGHFUNC,
  PREDFUNC,
  SUCCFUNC,
  ROUNDFUNC,
  TRUNCFUNC,
  ABSFUNC,    // Pascal STD Func ABS()
  SQRFUNC,	//        	SQR()
  SINFUNC,	//		SIN()
  COSFUNC,	//		COS()
  ARCTANFUNC,	//	`	ARCTAN()
  EXPFUNC,	//		EXP()
  LNFUNC,	//		LN()
  SQRTFUNC	//		SQRT()
  );		// Missing:  EOF() EOLN() and ODD()

TRelocType = (EMPTYRELOC, CODERELOC, INITDATARELOC, UNINITDATARELOC, IMPORTRELOC);

TScannerState = record
  Token: TToken;      // Current token
  UnitName,           // current unit
  FileName: TString;  // name of file
  ProcCount,          // number of procedures
  FuncCount,          // and functions in unit
  ExtFunc,            // number of External Procedures
  ExtProc,            // and functions in unit
  Position,           // current position in
  Line: Integer;      // line being examined
  Buffer: TBuffer;
  ch, ch2: TCharacter;
  inComment,           // we're inside a comment
  IOCheck,             // IOChecking handled by {$I+ or {$I-} on a per-file basis
  EndOfUnit: Boolean;  // end of file
end;

TScope = (EMPTYSCOPE, GLOBAL, LOCAL);

TSignature = record
           NumParams: Integer;
           NumDefaultParams: Integer;
           Param: PParams;
           Line,
           ResultType: Integer;
           CallConv: TCallConv;
end;

  TTokenP = ^TToken;
  TToken = record
     Prev,Next: TTokenP;
     Name: TString;     // For easy analysis, this is always in upper case
     DeclaredPos,            // Where it was declared
     DeclaredLine:Integer;
     case Kind: TTokenKind of
         IDENTTOK:         (NonUppercaseName: TShortString);  // This is needed
                                                              // in the case of EXTERNAL procedures where the name
                                                              // might have to be in mixed case
         BOOLEANTOK:       (BoolValue: Boolean);
         INT64NUMBERTOK:   (Int64Value:TInt64);
         INT128NUMBERTOK:  (Int128Value:TInt128);
         CURRENCYTOK:      (CurrencyValue:TCurrency);
         INTNUMBERTOK:     (OrdValue: LongInt);    // For all other ordinal types
         REALNUMBERTOK:    (RealValue: Double);
         STRINGLITERALTOK: (StrAddress: Integer;
                            StrLength: Integer);
 end;

  TTypeKind = (	EMPTYTYPE,      ANYTYPE,        INTEGERTYPE,    INT64TYPE,
                INT128TYPE,     CURRENCYTYPE,   SMALLINTTYPE,   SHORTINTTYPE,
                WORDTYPE,       BYTETYPE,       CHARTYPE,       BOOLEANTYPE,
                REALTYPE,       SINGLETYPE,     POINTERTYPE,    FILETYPE,
                ARRAYTYPE,      RECORDTYPE,     INTERFACETYPE,  SETTYPE,
                PROCEDURALTYPE, METHODTYPE,     ENUMERATEDTYPE, SUBRANGETYPE,
                FORWARDTYPE);



// Type record
TType = record
  // links for when this is a linked list
        Prev,
        Next: TypePtr;
        Block: Integer;         //< level relative to declaration, i.e. proc nesting
        BaseType: Integer;      //< indexes to another type
        AliasType: Integer;
// kind of value
        case Kind: TTypeKind of
             SUBRANGETYPE:              (Low, High: Integer);  //< integer subranges

             ARRAYTYPE:                 (IndexType: Integer;    //< type of what it is an array of
                              IsOpenArray: Boolean); //< whether arry does not have fixed dimensions
  // number of fields in a record
             RECORDTYPE, INTERFACETYPE: (NumFields: Integer;
                              Field: array [1..MAXFIELDS] of PField); //< list of fields
  // Procedures and functions
             PROCEDURALTYPE:            (Signature: TSignature; //< Proc/Func parameters
                              SelfPointerOffset: LongInt);  //< For interface method variables as temporary results
  // methods
             METHODTYPE:                (MethodIdentIndex: Integer);   //< For static methods as temporary results
  // Forward declaration
             FORWARDTYPE:               (TypeIdentName: TShortString);
end;

TUnit = record    // more later
  Name: TString;
end;



