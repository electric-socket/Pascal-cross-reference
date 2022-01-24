{$I CrossrefPrefixCode.inc}  // STD definitions
// Crossref - Pascal Cross reference
// Version 0.01
// January 9, 2021
// Copyright 2021 Paul Robinson
// Licensed under GPL v2

// Setup.pas - Intended to initialize the application
// 2021-12-17

Unit setup;


interface


Uses sysutils, Tables, _SymbolTable;



   // Temporary Procedure
   Procedure AD(D:UnicodeString);
   // General initialization
    Procedure Init;

   //symbol table related
implementation



Procedure Init;
Var
    I: Integer;
    T: TokenType;

begin

    writeln;    writeln;Writeln('** Start **'); writeln;

    New(StateTable);
    With StateTable^ Do
    Begin
        Cond:= NoState;
        Closer:= '';
        WithList := NIL;
        WithCount:= 0;
        Prev:= NIL;
    End;

    WithTable := NIL;

    For I := 1 to IdentMax do
       SymbolTable[I] := Nil;

    TopExtension := 2;
    Extensions[1] :='pas';
    Extensions[2] :='pp';

    TopFolder := 1;
    FolderTable[1] := UnicodeString(GetCurrentDir);


                 // name,         nickname,     type      Visible
    // one byte (non-numeric) types
    AddBaseClass('boolean',          'bool',     Booltype);   // boolean
    AddBaseClass('char',             'char',     charType);   // 8-bit character
    AddBaseClass('ansichar',         'ansc',     charType);   // alias for char

    // Unicode related
    AddBaseClass('widechar',         'wchr',    wcharType);   // 16-bit char  
    AddBaseClass('unicodestring',     'uni',   uniStrType);   // unicodeString

    // Integer types
    AddBaseClass('shortint',          'sby', shortintType);   // 8-bit signed integer
    AddBaseClass('byte',             'byte',     Bytetype);   // 8-bit unsigned integer
    AddBaseClass('smallint',         'sint', smallintType);   // 16-bit signed integer
    AddBaseClass('word',             'word',     wordType);   // 16-bit integer
    AddBaseClass('cardinal',         'card', cardinalType);   // 32-bit signed integer
    AddBaseClass('integer',           'int',      intType);   // 32-bit unsigned integer
    AddBaseClass('longint',           'int',      intType);   // alias for int
    AddBaseClass('long',              'int',      intType);   // alias for int
    AddBaseClass('int64',             'I64',    int64Type);   // 64-bit int
    AddBaseClass('pointer',           'ptr',  pointerType);   // address

    // floating point
    AddBaseClass('real',              'real',    realtype);   // 32-bit real
    AddBaseClass('single',            'sng',   singleType);   // single
    AddBaseClass('double',            'dbl',   DoubleType);   // double
                                                              // precision real
    AddBaseClass('extended',          'ext',  ExtendedType);  // 80-bit real

// groupings
    AddBaseClass('set',               'set',      setType);   // set
    // Name of enumerated group
    AddBaseClass('enumerator group', 'enum',    EnumGroup,False);
    // Name of member
    AddBaseClass('enumerated value', 'valu',    EnumType, False);

// array types
    AddBaseClass('array',             'arr',   ArrayType);    // general array
    // array of char with length
    AddBaseClass('string',            'str',   stringType);
    AddBaseClass('ansistring',        'anss',   AnsStrType);

// structures
    AddBaseClass('record',            'rec',   RecordType);   // record
    AddBaseClass('object',            'obj',   objectType);   // object
    AddBaseClass('class',             'cls',    ClassType);   // class
    AddBaseClass('template',         'tmpl', TemplateType);   // class
    AddBaseClass('file',             'file',      FileType);  // file
    AddBaseClass('text',             'file',      FileType);  // file of char


// Std functions and procedures

    AddStdConst('false',       Booltype);
    AddStdConst('true',        Booltype);
    AddStdConst('text',        FileType);
    AddStdConst('nil',         pointerType);
    AddStdConst('maxint',      intType);

    AddStdFunc('abs',    realtype);   
    AddStdFunc('arctan', realtype);  
    AddStdFunc('chr',    charType);   
    AddStdFunc('cos',    realtype);
    AddStdFunc('eof',    Booltype);   
    AddStdFunc('eoln',   Booltype);   
    AddStdFunc('exp',    realtype);
    AddStdFunc('ln',     realtype);
    AddStdFunc('odd',    Booltype);
    AddStdFunc('ord',    intType);
    AddStdFunc('pred',   intType);
    AddStdFunc('round',  realtype);    
    AddStdFunc('sin',    realtype);
    AddStdFunc('sqr',    realtype);
    AddStdFunc('sqrt',   realtype);
    AddStdFunc('succ',   intType);
    AddStdFunc('trunc',  realtype);


// Initialize keywords

{
    KeywordType[DOTOK]        := nxstmtDec;
    KeywordType[VARTOK]       := ElemDec;     KeywordState[VARTOK]  := invar ;
    KeywordType[THENTOK]      := nxstmtDec;
    KeywordType[TRYTOK]       := blockDec   KeywordState[TRYTOK] := inTry ;


    KeywordType[CASETOK]      := BlockDec;   KeywordState[CASETOK] := inCase ;
           KeywordClose[CASETOK] := ENDTOK;
    KeywordType[WITHTOK]      := BlockDec;    KeywordState[ELSETOK] := inwith ;
    KeywordType[ELSETOK]      := nxstmtDec;
    KeywordType[TYPETOK]      := ElemDec;     KeywordState[TYPETOK] := intype ;
    KeywordClose[BEGINTOK]    := BlockDec;    KeywordState[BEGINTOK] := inBegin;
           KeywordClose[BEGINTOK] := ENDTOK;
    KeywordType[UNTILTOK]     := closureDec;
    KeywordType[WHILETOK]     := nxstmtDec;
    KeywordType[CONSTTOK]     := ElemDec;    KeywordState[CONSTTOK] := inConst ;
    KeywordType[REPEATTOK]    := BlockDec;  KeywordState[REPEATTOK] := inRepeat;
           KeywordClose[REPEATTOK] := UNTILTOK;
    // END (and any closure) does not have to
    // say anything; the gentleman calling err
    // I mean keyword calling will
    // announce when it needs her
    KeywordType[ENDTOK]      := closureDec;
    KeywordType[RECORDTOK]   := structDec; KeywordState[RECORDTOK] := inRecord ;
           KeywordClose[RECORDTOK]:= ENDTOK;
    KeywordType[CLASSTOK]    := structDec; KeywordState[CLASSTOK] := inClass ;
           KeywordClose[CLASSTOK]:=ENDTOK;
    KeywordType[OBJECTTOK]   := structDec; KeywordState[OBJECTTOK] := inObject;
           KeywordClose[OBJECTTOK]:= ENDTOK;
    KeywordType[LIBRARYTOK]  := unitLibProgDec;
           KeywordState[LIBRARYTOK]:=inLibrary;     
    KeywordType[UNITTOK]     := unitLibProgDec;  KeywordState[UNITTOK]:=inUnit;
    KeywordType[PROGRAMTOK]  := unit:ibProgDec;
           KeywordState[PROGRAMTOK]:=inProgram;
    KeywordType[FUNCTION]    := pfdec; KeywordState[FUNCTION] := inFunction ;
    KeywordType[PROCEDURETOK]:= pfdec; KeywordState[PROCEDURETOK]:= inProcedure;
 }
    AddStdProc('get');
    AddStdProc('put');
    AddStdProc('reset');
    AddStdProc('rewrite');
    AddStdProc('read');
    AddStdProc('write');
    AddStdProc('pack');
    AddStdProc('unpack');
    AddStdProc('new');
    AddStdProc('dispose');
    AddStdProc('readln');
    AddStdProc('writeln');
    AddStdProc('page');
    AddStdProc('mark');
    AddStdProc('release');
    AddStdProc('halt');
    AddStdProc('break');
    AddStdProc('continue');

    AddStdFile('input');
    AddStdFile('output');
    AddStdFile('stdin');
    AddStdFile('stdout');
    AddStdFile('stderr');
 {
            AddKeyWord('as');
            AddKeyWord('dispinterface');
            AddKeyWord('except');
            AddKeyWord('exports');
            AddKeyWord('finalization');
            AddKeyWord('finally');
            AddKeyWord('initialization');
            AddKeyWord('inline');
            AddKeyWord('is');

            AddKeyWord('on');
            AddKeyWord('out');

            AddKeyWord('property','',GenMod, inProperty);
            AddKeyWord('raise');

    }

    // extended
    if Lang_extended then
    begin
{
        AddKeyWord('and_then','', nxstmtDec );
        AddKeyword('export');
        AddKeyword('import');
        AddKeyword('module','end',unitprogDec, inunit );
        AddKeyword('only');
        AddKeyword('or_else','', nxstmtDec );
        // equiv to e+lse on case stmt
        AddKeyword('otherwise', '' , BlockDec, incase );
        AddKeyword('protected','',genmod);
        AddKeyword('qualified','',genmod);
        AddKeyword('restricted','',genmod);
        AddKeyword('value');
   }
        AddStdFunc('pow',Realtype);
    end;

    if lang_turbo then // turbo pascal
    begin
        {

        AddKeyword('asm','',BlockDec,inIgnore);
        AddKeyword('constructor','',pfDec,inConstructor);
        AddKeyword('destructor','',pfDec,inDestructor);
        AddKeyword('implementation','',impDec,inImplementation);
        AddKeyword('in');
        AddKeyword('interface','',interDec,inInterface);
        AddKeyword('object','end',structdec,inObject);
        AddKeyword('operator','',genMod);
        AddKeyword('reintroduce');
        AddKeyword('self');
        AddKeyWord('unit','end',unitprogDec, inunit );
        AddKeyword('uses','',usesDec);
 }
        AddStdUnit('system');
    end;

    AddStdUnit('system');
    if Lang_XD then  // XDPascal
    begin
{
        AddKeyword('implementation','',impDec,inImplementation);
        AddKeyword('interface','',interDec,inInterface);
        AddKeyWord('unit','end',unitprogDec, inunit );
        AddKeyword('uses','',usesDec);
}
        AddStdProc('exit');
        AddStdProc('halt');
        AddStdProc('break');
        AddStdProc('continue');

        AddStdUnit('system');
    end;

    if Lang_Borland then // Borland Pascal
    begin



        AddStdUnit('system');
    end;

    if Lang_object then // Object Pascal
    begin

        // these are not reserved words, but it's
        // orobably not a good idea to rdefine them
        AddStdProc('exit');
        AddStdProc('halt');
        AddStdProc('break');
        AddStdProc('continue');
        AddStdUnit('system');
    end;

    if Lang_Delphi then  // Delphi
    begin


        AddStdUnit('system');
    end;

    if Lang_FreePascal then
    begin

       {$I FPC_defs.inc}

        AddStdUnit('system');
    end;

    if Lang_extended then   // Extebded Pascal
    BEGIN


    END;

    if lang_turbo then      // Turbo Pascal
    BEGIN

    END;

    if Lang_XD then         // XDPascal
    BEGIN

    END;

    if Lsng_GNU then        // GNU Pascal
    BEGIN

    END;

    IF lang_Stanford then   // Stanford Pascal
    BEGIN
  //     I := BComment1;
  //     whilr

    END;

    if Lang_Borland then    // Borland Pascal
    BEGIN

    END;

    if Lang_object then     // Object Pascal
    BEGIN

    END;

    if Lang_Delphi then     // Delphi
    BEGIN

    END;

    if Lang_FreePascal then // Ftee Pascal
    BEGIN

    END;

    IF Allow_Control_Chars then // Allow ^A for Control-A
    BEGIN

    END;



    dumpSymbolTable;
 end;




{$NOTE AD Procedure is Temporsry}
Procedure AD(D:UnicodeString);
Begin

end;

end.


                                                     
BEGIN END;
