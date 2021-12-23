// Pascal Cross0Reference Program
// 2021-12-17

Unit setup;


interface

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


Identmax  = 27;
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

 alphanums = Identifier + digits;

 type
     IdentSize = 1..Identmax;     // 26 letters + _

 const

  ValidIdent: array[IdentSize] of Char = (
            'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L',
            'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X',
            'Y', 'Z', '_');


type
// base types
// this only concerns identifier vslues;
// procedutal types ate encapsulated elsewhere
Sizetype= (enumGroup,  // name of enumerator list
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
         MajorKind= (prockind,             //< procedure
                     Funckind,             //< dunction
                     constKind,            //< in code or signature
                     selector,             //< selector in record CASE stmt
                     inVisible,            //< not visible in cross-reference
                     BaseType,             //< base type: char, integer etc.
                     refonly,              //< list only if referenced
                                           //< used (for predefined items)
                     stdProc,              //< standard procedure/function
                     varKind,
                     typeKind,
                     valueKind,             //< unspecified arg in signature
                     SignatureKind,         //< arg of proc/func
                     fieldKind,             //< field of record, object or class
                     recordKind,
                     ObjectKind,
                     ClassKind,
                     ConstrKind,            //< constructor
                     DestrKind,             //< Destructor
                     AboveTheLine,          //< in interface
                     inLocal,               //< identifier declared in p/f
                     inProgtam,             //< in main program                     ;
                     inUnit);               //< is a unit

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
                      Abbrev,               //< Short name
                      NameUC,               //< Upper case copy of Name
                      Name: ansistring;     // its name
                      DefPage,              // page where it's defined
                      DefLine: Integer;     // Line # or line # in Defpage
                      Usage: LineP;         // Places where it's used
                      UnitIn: UnitP;        // Unit it's in
                      Owner,                // If in record, class of
                                            // obhect, which one
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
                            LocalTable,             //<
                            ResultType: ItemP);     //< type if function or nil

                        SignatureType:
                           ( NextSignature: SignatureP;); //< next arg of p/f
                          elementType:                    //< elements: const, type. var
                           ( NextField: ItemP);         //< if a field in record
                                                        //< class, or object next one

                        KeywordType, ModifierType:      //< Keywords and modifiers
                           ( KeyWordClass:Word;         //< What type of ketword is it
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
    // used for WITH statement
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
   SymbolBottom,                      //< in reverse order
   Symboltable: Array[IdentSize] of ItemP; //< alphabetical order

    WithTable: WithP;
   PasPath,PasFolder,PasName,PasExt: UnicodeString;


    Procedure Init;
    // adds a base class, i.e. int, char, etc.
    Procedure AddBaseClass(N,   // name
              Abbr:AnsiString;  // short name
              NewSZ:SizeType;   // what it is
              Visible: Boolean = TRUE); // hide item if false
    Procedure AddKeyword(N:Ansistring; KWType:integer);
    Function Plural(N:Int64; Plu:AnsiString; Sng: AnsiString): Ansistring;
    Function Version:String ;
    Function CopyRight:String ;
    Procedure DumpSymbolTable;


implementation



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



// adds a base class, i.e. int, char, etc.
Procedure AddBaseClass(N,   // name
          Abbr:AnsiString;  // short name
          NewSZ:SizeType;   // what it is
          Visible: Boolean = TRUE); // hide item if false
var
  Which: integer;
  Ident,
  CW: ansistring;


    Procedure InitEntry(Var NewItem:ItemP);
    Begin
      New(NewItem);
      With NewItem^ do
      begin
          Name  := N;
          NameUC:= Ident;
          Abbrev:= Abbr;
          Size  := NewSZ;
          Kind  := [BaseType,RefOnly];
          If Not Visible then
             Kind:= Kind+[invisible];

          What      := ElementType;
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

   // This routine maintains the list so
   // that SymbolTable[Which] is always the lowest value
   // entry, e.g. 'a', and SymbolBottom[Which] is
   // always the highest, e.g. Z

    Procedure InsertInSymbolTable;
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
             exit
         end;
         PriorItem := SymbolTable[Which];
         NextItem := SymbolTable[Which]^.NextTotal;

         repeat // at this point the prior
                // item was less than ItemUC, so let's see
                // if this one is, is equal, or is more
             if NextItem = NIL then   // after last
             begin
                 InitEntry(AddItem);
{       Not needed here because initentry will zero this anyway
                 Additem^.NextTotal  := NIL;
}
                 AddItem^.PrevTotal  := PriorItem;
                 PriorItem^.NextTotal := AddItem;
                 SymbolBottom[Which] := AddItem;
                 exit
              end;
              if Ident <= NextItem^.NameUC then
              begin  // it goes before this one, after previous
                  InitEntry(AddItem);
                  PriorItem^.NextTotal := AddItem;
                  AddItem^.NextTotal := NextItem;
                  NextItem^.PrevTotal := AddItem;
                  AddItem^.PrevTotal := PriorItem;
                  exit
              end;
              PriorItem := NextItem;
              NextItem  := NextItem^.NextTotal;
         until  false;
    end;


begin
      Ident := UpperCase(N);
      CW := Copy(Ident,1,1);
      For Which := 1 to IdentMax do
         if CW = ValidIdent[Which] then
           break;

      InsertInSymbolTable;


end;

Procedure AddKeyword(N:Ansistring; KWType:integer);
Begin



end;



Procedure Init;
Var
    I: Integer;
begin
    For I := 1 to IdentMax do
       SymbolTable[I] := Nil;

                 // name,     nickname,  size,     type
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

    AddKeyWord('begin',kwbegin);
    AddKeyWord('case',kwcase;
    AddKeyWord('do',kwdo);
    AddKeyWord('end',kwend);
    AddKeyWord('for',kwfor);
    AddKeyWord('function',kwfunction);
    AddKeyWord('if',kwif);
    AddKeyWord('procedure',kwprocedure);
    AddKeyWord('program',100);
    AddKeyWord('unir',120);
    AddKeyWord('with',5);





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
               for Q := prockind to inunit do
               if Q in Kind then
                  write(Q,' ');
               writeln;
            end;
            Walker:= Walker^.NextTotal;
        end;
    end;
end;

end.



