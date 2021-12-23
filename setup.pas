// Pascal Cross-Reference Program
// 2021-12-17

Unit setup;


interface
{$DEFINE Interface}

Uses Scan,sysutils;

Const
       VERSION_MAJOR             = 0;
       VERSION_RELEASE           = 0;
       VERSION_PATCH             = 0;
       VERSION_FULL              = VERSION_MAJOR*1000+
                                   VERSION_RELEASE *10+
                                   VERSION_PATCH;

// note, the folowing MUST be a string of digits in quotes
// as PROGRAM UPD does an auto-upddate on every compile

       VERSION_REV               = '1';


Months: array[1..12] of string[9]=
        ('January','February','March',   'April',   'May','     June',
         'July',    'August', 'September','October','November', 'December');
Days: Array[0..6] of string[9]=
        ('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday');

SlashChar = '\'; // Your system's directory separator,
                 // \ on windows; / on unix, linux, etc

// ** Keyword definitions **
// ** keyword values **

// keyword definitions for Pascal Cross-Reference

 // 0 = keyword has no significance, e.g. 'for' has 0 signifiance, but
 // 'do' has 1 because a statement beginning with do (for, while, with)
 // carries for one statement unless encased in a block (begin or repeat)
 // but with, on the other hand, changes the relevance of each identifier
 // following in the next statement..

 // these are the base standard set

 
  kwdo        = 1 ;   // do      - starts a single statement
  kwthen      = 1 ;   // then    - good for one statement
  kwelse      = 1 ;   // else    - also good for one statement
  kwbegin     = 10 ;  // begin   - start of counted block
  kwrecord    = 11 ;  // record  - starts a declaration block
  kwcase      = 12 ;  // case    - case is also special
  kwend       = 13 ;  // end     - block closure
  kwrepeat    = 14 ;  // repeat  - another block
  kwuntil     = 15 ;  // until   - special closure
  kwwith      = 16 ;  // with    - with has special processing
  kwconst     = 20 ;  // const     - start of definitions
  kwtype      = 21 ;  // type      - start of definitions
  kwvar       = 22 ;  // var       - start of definitions
  kwfunction  = 23 ;  // function  - subroutine returning a value
  kwprocedure = 24 ;  // procedure - subroutine not returning a value
  kwprogram   = 30 ;  // program   - main program
  kwunit      = 31 ;  // unit      - a module
  kwforward   = 101 ; // forward   : modifier of procedurals
  kwif        = 0 ;
  kwof        = 0 ;
  kwto        = 0 ;
  kwin        = 0 ;
  kwor        = 0 ;
  kwand       = 0 ;
  kwnot       = 0 ;
  kwxor       = 0 ;
  kwshl       = 0 ;
  kwshr       = 0 ;
  kwfor       = 0 ;
  kwdiv       = 0 ;
  kwmod       = 0 ;
  kwset       = 0 ;
  kwgoto      = 0 ;
  kwfile      = 0 ;
  kwwhile     = 0 ;
  kwarray     = 0 ;
  kwlabel     = 0 ;
  kwdownto    = 0 ;
  kwpacked    = 0 ;

// bext ones are for Extended Pascal





//  ** Identifier group **

// These are the characters allowed as the firsr (and subsequrnt)
// characters allowed for an identifier. If your compilrt allows
// additiona;characters (likr $) to start an identifier, either
// add it like underscore, or if, like digits, it's allowed as
// the second and subsequent characyers, attach it to digits. The
// result being the character set in 'identifier' represents the
// legal leading character, and alphanums the ones legal for the
// remainder of an identifier.

Identmax  = 31;
upcase = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L',
              'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X',
              'Y', 'Z'];
locase = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l',
              'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x',
              'y', 'z'];
 alphabet = upcase + locase;
 Underscore =  ['_'];
 Identifier =   alphabet + Underscore ;
 digits = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'];
 alphanums = Identifier + digits + Underscore + Underscore +
            (* 4 spares *)  Underscore + Underscore ;
 type
     IdentSize = 1..Identmax;     // 26 letters + _ + 4 spares
 const
  ValidIdent: array[IdentSize] of Char = (
            'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L',
            'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X',
            'Y', 'Z', '_', (* and 4 spares *) '_','_','_','_');


type
// base types

// this only concerns identifier vslues;
// procedutal types ate encapsulated elsewhere
Sizetype= (notype,     // not a base type
           enumGroup,  // name of enumerator list
           EnumType,   // enumerated value,
           boolType,   // boolean
           bytetype,   // 8-bit integer
           charType,   // 8-bit character
           wordType,   // 16-bit integer
          wcharType,   // 16-bit char
            intType,   // 32-bit integer
           realtype,   // 32-bit real
         singleType,   // single
         DoubleType,   // double precision real
          int64Type,   // 64-bit int
        ExtendedType,  // 80-bit real
           pointerType,// address
           setType,    // set
           ArrayType,  // general array
           stringType, // array of char with length
           AnsStrType, // Ansistring
           uniStrType, // unicodeString
           RecordType, // record
           objectType, // object
           ClassType,  // class
           FileType);  // file


        UnitP = ^UnitRecord;
        useP= ^UserUsed;     // list of user and used
        ItemP = ^Item; // symbol table item
        UserUsed = record
                       this: unitp;  // this unit's pointer
                       next: UseP;   // next usage pointer
                   end;

        UnitRecord = record
                     Name,                 // irs name
                     FileName: ansistring; // file used
                     isDeclared,           // someone refeenced it
                     isfinished: boolean;  // we've read it through
                     usedby,               // units used by
                     UserOf: UserUsed;       // uses other units
                     GlobalTable,          // interface items
                     LocalTable:ItemP;     // implementation items
                     end;

                   // info bits 
        MajorKind= ( inVisible,            //< not visible in cross-reference 
                     BaseType,             //< base type: char, integer etc.
                     refonly,              //< list only if referenced
                                           //< used (for predefined items) 
                     stdPF,                //< standard procedure/funcrion
                     predefined,           //< stystem or user predefined
                     compilerdefined,      //< predefined by standards
                                           //< or by this orogram
                     AboveTheLine,         //< in interface
                     isLocal,              //< identifier declared within p/f 
                     selector,             //< selector in record CASE stmt   
                     SignatureKind,        //< args of proc/func 
                     temporary,            //< temporary item for search
                                           //< purposes (usually for WITH stmt
                     declared,             //< forward declaraation (forward proc or pointer)
                     definred,             //< Actual defiition (begin, etc.)
                     // actual identifiers
                     prockind,             //< procedure
                     Funckind,             //< dunction
                     constKind,            //< in CONST in code or signature
                     varKind,              //< VAR declaration
                     typeKind,             //< type declaration
                     valueKind,            //< unspecified arg in signature
                     fieldKind,            //< field of record, object or class
                     // record, object and class are in SizeType
                     ConstrKind,            //< constructor
                     DestrKind,             //< Destructor
                     isUnit);               //< is a unit

         IdentType = (proceduralType,       //< proc, func, method, prop, cons, destr
                      SignatureType,        //< argument of procedural type
                      ElementType,          //< const, type, var
                      UnitType,             //< unit or program
                      PreUnit,              //< predefined unit
                      KeywordType,          //< action
                      ModifierType,         //< modifies keyword
                      PredefinedType);      //< predefined by system or other units

         LineP = ^LineTable;                //< list of line number usage
         signatureP = ^Signature;           //< proc/func arge
         Item = record                      //< identifier record  "Symbol Table"
                      Abbrev,               //< optional Short name
                      NameUC,               //< Upper case copy of Name
                      Name: ansistring;     //< its name as given
                      DefPage,              //< page where it's defined
                      DefLine: Integer;     //< Line # or line # in Defpage
                      Usage: LineP;         //< Places where it's used
                      UnitIn: UnitP;        //< Unit it's in
                      Owner,                //< If in record, class of
                                            //< object, which one
                      PrevinUnitATL,        //< prior item in this units interface
                      NextinUnitATL,        //< next item in interface
                      PrevinUnitLocal,      //< Prior item in implementation this unit
                      NextinUnitLocal,      //< Next item in implementation this unit
                      // enclosure: immediate container: record, object, class or procedural
                      PrevInPlace,          //< Prior item this enclosure
                      NextInPlace,          //< Next item this enclosure
                      PrevTotal,            //> Prior item in global symbol list
                      NextTotal: ItemP;     //< Next item in global symbol list
                      size: sizetype;       //< what it uses
                      Kind: set of Majorkind;      //< statistics
                      case What: identType of      //< what it is
                        proceduralType:
                          ( SigCount: integer;     //< number of signatures
                            SignatureList: SignatureP; //< signature items
                            LocalTable,             //< List of local vars
                            ResultType: ItemP);     //< type if function or nil

                        SignatureType:
                           ( NextSignature: SignatureP;); //< next arg of p/f
                          elementType:                    //< elements: const, type. var
                           ( NextField: ItemP);         //< if a field in record
                                                        //< class, or object next one

                        KeywordType, ModifierType:      //< Keywords and modifiers
                           ( Closedby,                  //<If this is a block
                           // keyword does it start a block
                             Closs: Itemp;              //< or does it
                           // close a keyword block
                             KeyWordClass:Word;         //< What type of keyword is it
                             Specials: word);           //< Sprcial instrucyions

                        PreUnit, UnitType:
                           ( Status: UnitP  );     //< Unit fetails
                      end;

           LineTable = record
                          UsePage,              // page where it's used
                          UseLine: Integer;     // Line # or line # in Defpage
                          Next: LineP;
                       end;

           Signature= record
                        BaseType: ItemP;        // type of this arg
                        Next: SignatureP;    // next arg
                      end;

    WithP = ^WithRec;
    // used for WITH statements
    WithRec = record
          ThisWith: Itemp;
          Prev: WithP;
    end;


VAR
{ In order to make searches faster, I divide up the symbol table
  into 27 segments, consisting of the first letter of the identifier
  A-Z plus _ (underline). It is a double-liked list, SymbolTable is
  organized A-Z while SymbolBottom is organized Z-A. Basically I
  know how to do automatic sorting on a double-linked list, so
  that's what I used.
}
   SymbolBottom,                           //< in reverse order
   Symboltable: Array[IdentSize] of ItemP; //< alphabetical order

   WithTable: WithP;
   // this ptogtam's name
   PasPath,PasFolder,PasName,PasExt: UnicodeString;


   // General initialization
    Procedure Init;
    // adds base classes, i.e. int, char, etc.
    Procedure AddBaseClass(N,   // name
              Abbr:AnsiString;  // short name
              NewSZ:SizeType;   // what it is
              Visible: Boolean = TRUE); // hide item if false
    // installs keywords
    Procedure AddKeyword(N:Ansistring; Flag:Integer);
    Function Plural(N:Int64; Plu:AnsiString; Sng: AnsiString): Ansistring;
    Function Version:String ;
    Function CopyRight:String ;
    Procedure DumpSymbolTable;


{$UNDEF Interface}
implementation
{$DEFINE implementation}


  Function Plural(N:Int64; Plu:AnsiString; Sng: AnsiString): Ansistring;
  Begin
      Result := IntToStr(N);
      Result := ' '+Result+' ';
      If n<>1 Then
          Result:= Result+ Plu
       Else
          Result := Result + Sng;
  End;

Function CopyRight:String;
begin
    Result := 'Copytight 2021 Paul Robinson';
end;

Function Version:String;
begin
     Result := 'Version '+IntToStr( VERSION_MAJOR )+'.' +
               IntToStr( VERSION_RELEASE )+'.' +
               IntToStr( VERSION_PATCH)+' Rev. ' +
                         VERSION_REV ;

end;


// This routine accepts the pointer passed to it
// obtains memory, then initializes it, retutning
// the address of  the memotu available
Procedure InitEntry(Var NewItem:ItemP);
Begin
  New(NewItem);
  With NewItem^ do
  begin
      NextTotal := NIL;
      PrevTotal := NIL;
      NextField := NIL;

      // to prevent errors, null *all* pointers
      Usage              := NIL;
      UnitIn             := NIL;
      Owner              := NIL;
      PrevinUnitATL      := NIL;
      NextinUnitATL      := NIL;
      PrevinUnitLocal    := NIL;
      NextinUnitLocal    := NIL;
      PrevInPlace        := NIL;
      NextInPlace        := NIL;
      PrevTotal          := NIL;
      NextTotal          := NIL;
  end;
End;


   // This routine maintains the list so that SymbolTable[Which]
   // (where 'which' being the index in the alphabet with A as 1,
   // Z as 26, and _ (underscore) as 27m so that it always points
   // to the lowest value entry that starts with it's assigned
   // letter, i.e. for starting at K, it covers idemtifiers starting
   // with 'ka' up through 'kz', while SymbolBottom[Which] is the
   // opposite, (always the highest), i.e. starting at 'KZ' and
   // working down to  'ka'.

    Function InsertInSymbolTable(Which:Integer;  Ident: AnsiString ): ItemP;
    Var
       AddItem,
       NextItem,
       PriorItem: ItemP;

    begin
        If SymbolTable[Which] = NIL then
        begin
            InitEntry(SymbolTable[Which]);

{       Not needed here because initentry will zero these anyway
            SymbolTable[Which]^.PrevTotal := NIl;
            SymbolTable[Which]^.NextTotal := NIL;
}
            SymbolBottom[Which] := SymbolTable[Which];
            Result :=  SymbolTable[Which];
            exit
         end;
         If Ident < SymbolTable[Which]^.NameUC then // before beginning
         begin
             InitEntry(AddItem);
{       Not needed here because initentry will zero this anyway
             Additem^.PrevTotal := NIL;
}
             AddItem^.NextTotal := SymbolTable[Which];
             SymbolTable[Which]^.PrevTotal := AddItem;
             SymbolTable[Which] := AddItem;
             Result := AddItem;
             exit
         end;
         PriorItem := SymbolTable[Which];
         NextItem := SymbolTable[Which]^.NextTotal;

         repeat // at this point the prior
                // item was less than ItemUC, so let's see
                // if this one is, is equal, or is more
                If NextItem = NIL then
                begin
                    InitEntry( AddItem);
{       Not needed here because initentry will zero this anyway
                 Additem^.NextTotal  := NIL;
}
                     AddItem^.PrevTotal  := PriorItem;
                     PriorItem^.NextTotal := AddItem;
                     SymbolBottom[Which] := AddItem;
                     Result := AddItem;
                     exit
                 end;
                 if Ident <= NextItem^.NameUC then
                 begin  // it goes before this one, after previous
                     InitEntry(AddItem);

                     PriorItem^.NextTotal := AddItem;
                     AddItem^.NextTotal := NextItem;
                     NextItem^.PrevTotal := AddItem;
                     AddItem^.PrevTotal := PriorItem;
                     Result := AddItem;
                     exit
                 end;
              PriorItem := NextItem;
              NextItem  := NextItem^.NextTotal;
         until  false;
    end;


// adds a base class, i.e. int, char, etc. to stmbol table
Procedure AddBaseClass(N,   // name
          Abbr:AnsiString;  // short name
          NewSZ:SizeType;   // what it is
          Visible: Boolean = TRUE); // hide item if false
var
  Which: integer;
  Ident,
  CW: ansistring;
  NewItem: ItemP;



begin
      Ident := UpperCase(N);
      CW := Copy(Ident,1,1);
      For Which := 1 to IdentMax do
         if CW = ValidIdent[Which] then
           break;

      NewItem := InsertInSymbolTable(Which,Ident);

      With NewItem^ do
      begin
          Name  := N;
          NameUC:= Ident;
          Abbrev:= Abbr;
          Size  := NewSZ;
          Kind  := [BaseType,RefOnly,predefined];
          If Not Visible then
              Kind := Kind+[invisible];
          What  := ElementType;
      end;
end;

Procedure AddKeyword(N:Ansistring; Flag:Integer);
Begin



end;


Procedure AddStdVar(N:Ansistring; NewSZ:SizeType);
Begin


END;

Procedure AddStdFunc(N:Ansistring; NewSZ:SizeType);
BEGIN


END;

Procedure AddStdProc(N:Ansistring; NewSZ:SizeType);
BEGIN


END;


Procedure AddStdConst(N:Ansistring; NewSZ:SizeType);
var
  Which: integer;
  Ident,
  CW: ansistring;
  NewItem: ItemP;


begin
      Ident := UpperCase(N);
      CW := Copy(Ident,1,1);
      For Which := 1 to IdentMax do
         if CW = ValidIdent[Which] then
           break;

      NewItem := InsertInSymbolTable(Which,Ident);

      With NewItem^ do
      begin
          Name  := N;
          NameUC:= Ident;
          Abbrev:= '';
          Size  := NewSZ;
          Kind  := [RefOnly,predefined,ConstKind];
          If inVisible in Kind then
               Kind := Kind+[invisible];
          What  := ElementType;
      END;
END;

Procedure AddStdUnit(N:Ansistring; Visible:Boolean = true);
var
  Which: integer;
  Ident,
  CW: ansistring;
  NewItem: ItemP;

BEGIN
    Ident := UpperCase(N);
    CW := Copy(Ident,1,1);
    For Which := 1 to IdentMax do
         if CW = ValidIdent[Which] then
           break;

    NewItem := InsertInSymbolTable(Which,Ident);

    With NewItem^ do
    begin
        Name  := N;
        NameUC:= Ident;
        Abbrev:= '';
        Size  := notype;
        Kind  := [RefOnly,predefined,ConstKind];
        If not Visible  then
             Kind := Kind+[invisible];
        What  := ElementType;
      END;


END;

Procedure AddStdProc(N:Ansistring);
BEGIN


END;


Procedure Init;
Var
    I: Integer;

begin
{$DEFINE INIT}
    For I := 1 to IdentMax do
       SymbolTable[I] := Nil;

                 // name,         nickname,     type      Vidible
    AddBaseClass('enumerator',       'enum',    EnumGroup,False);  // PLACEHOLDER
    AddBaseClass('enumerated value', 'econ',    EnumType, False);  // PLACEHOLDER
    AddBaseClass('byte',             'byte',     Bytetype);   // 8-bit integer
    AddBaseClass('boolean',          'bool',     Booltype);   // boolean
    AddBaseClass('char',             'char',     charType);   // 8-bit character
    AddBaseClass('word',             'word',     wordType);   // 16-bit integer
    AddBaseClass('widechar',         'wchr',    wcharType);   // 16-bit char
    AddBaseClass('integer',           'int',      intType);   // 32-bit integer
    AddBaseClass('longint',           'int',      intType);   // alias for integer
    AddBaseClass('int64',             'I64',    int64Type);   // 64-bit int
    AddBaseClass('pointer',           'ptr',  pointerType);   // address
    AddBaseClass('set',               'set',      setType);   // set
    AddBaseClass('array',             'arr',   ArrayType);    // general array
    AddBaseClass('string',           'strg',   stringType);   // array of char with length
    AddBaseClass('ansistring',        'ans',   AnsStrType);   // Ansistring
    AddBaseClass('unicodestring',     'uni',   uniStrType);   // unicodeString
    AddBaseClass('record',            'rec',   RecordType);   // record
    AddBaseClass('object',            'obj',   objectType);   // object
    AddBaseClass('class',             'cls',    ClassType);   // class
    AddBaseClass('real',              'real',    realtype);   // 32-bit real
    AddBaseClass('single',            'sng',   singleType);   // single
    AddBaseClass('double',            'Dbl',   DoubleType);   // double precision real
    AddBaseClass('extended',          'Ext',  ExtendedType);  // 80-bit real
    AddBaseClass('file',             'file',      FileType);  // file


// std keywords

    AddKeyWord('if',          kwif);
    AddKeyWord('do',          kwdo);
    AddKeyWord('of',          kwof);
    AddKeyWord('to',          kwto);
    AddKeyWord('in',          kwin);
    AddKeyWord('or',          kwor);
    AddKeyWord('and',         kwand);
    AddKeyWord('not',         kwnot);
    AddKeyWord('xor',         kwxor);
    AddKeyWord('shl',         kwshl);
    AddKeyWord('shr',         kwshr);
    AddKeyWord('end',         kwend);
    AddKeyWord('for',         kwfor);
    AddKeyWord('var',         kwvar);
    AddKeyWord('div',         kwdiv);
    AddKeyWord('mod',         kwmod);
    AddKeyWord('set',         kwset);
    AddKeyWord('then',        kwthen);
    AddKeyWord('else',        kwelse);
    AddKeyWord('with',        kwwith);
    AddKeyWord('goto',        kwgoto);
    AddKeyWord('case',        kwcase);
    AddKeyWord('type',        kwtype);
    AddKeyWord('file',        kwfile);
    AddKeyWord('begin',       kwbegin);
    AddKeyWord('until',       kwuntil);
    AddKeyWord('while',       kwwhile);
    AddKeyWord('array',       kwarray);
    AddKeyWord('const',       kwconst);
    AddKeyWord('label',       kwlabel);
    AddKeyWord('repeat',      kwrepeat);
    AddKeyWord('record',      kwrecord);
    AddKeyWord('downto',      kwdownto);
    AddKeyWord('packed',      kwpacked);
    AddKeyWord('forward',     kwforward);
    AddKeyWord('program',     kwprogram);
    AddKeyWord('function',    kwfunction);
    AddKeyWord('procedure',  kwprocedure);

    AddStdConst('false',       Booltype);
    AddStdConst('true',        Booltype);
    AddStdConst('input',       FileType);
    AddStdConst('output',      FileType);
    AddStdConst('text',        FileType);
    AddStdConst('nil',         pointerType);
    AddStdConst('maxint',      intType);
    AddStdConst('sethigh',     intType);
    AddStdConst('ordmaxchar',  intType);

    AddStdFunc('abs',    realtype);
    AddStdFunc('sqr',    realtype);
    AddStdFunc('round',  realtype);
    AddStdFunc('trunc',  realtype);
    AddStdFunc('odd',    Booltype);
    AddStdFunc('ord',    intType);
    AddStdFunc('chr',    intType);
    AddStdFunc('pred',   intType);
    AddStdFunc('succ',   intType);
    AddStdFunc('eof',    Booltype);
    AddStdFunc('eoln',   Booltype);
    AddStdFunc('sin',    realtype);
    AddStdFunc('cos',    realtype);
    AddStdFunc('exp',    realtype);
    AddStdFunc('sqrt',   realtype);
    AddStdFunc('ln',     realtype);
    AddStdFunc('arctan', realtype);

    AddStdUnit('system');

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


// extended
    AddKeyWord('unit',kwunit);



    dumpSymbolTable;
 end;


Procedure DumpSymbolTable;
Var
    Walker: ItemP;
    Q: Majorkind;
    K,
    I: Integer;

begin
    For I := 1 to 27 do
    begin
        K := 0;
        Write('Symbol ',ValidIdent[I]);
        If SymbolTable[I] = NIL then
        begin
           Writeln(' -- Empty');
           continue;
        end;
        Walker := SymbolTable[I];
        Writeln(' Begins');
        While Walker <> NIL do
        begin
            Inc(K);
            With Walker^ do
            begin
               Write(K:4,' ',Name,' (',abbrev,') ');
               for Q := invisible to isunit do
               if Q in Kind then
                  write(Q,' ');
               writeln;
            end;
            Walker:= Walker^.NextTotal;
        end;
    end;
end;

end.



