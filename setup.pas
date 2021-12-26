// Pascal Cross-Reference Program
// Copyright 2021 Paul Robinson
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
// procedutal types are encapsulated elsewhere
Datatype= (notype,     // not a base type
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
            ClassType, // class
         TemplateType, // template
            FileType); // file


        UnitP = ^UnitRecord;
        useP= ^UserUsed;     // list of user and used
        ItemP = ^Item; // symbol table item
        UserUsed = record
                       this: unitp;  // this unit's pointer
                       next: UseP;   // next usage pointer
                   end;
        UnitRecord = record
                     Name,                 // its name
                     FileName: ansistring; // file used
                     isDeclared,           // someone refeenced it
                     isfinished: boolean;  // we've read it through
                     usedby,               // units used by
                     UserOf: UserUsed;     // uses other units
                     GlobalTable,          // interface items
                     LocalTable:ItemP;     // implementation items
                     end;


    StateCond = (NoState,         // Not in a state
                 inProgram,       // program
                 inUnit,          // unit found
                 inLibrary,       // library
                 inInterface,     // Which part (carries to succeeding statements)
                 inImplementation,
                 inConst,         // CONST, TYPE, VAR declaration
                 inType,
                 inVar,
                 inforward,       // FORWARD
                 inExternal,      // EXTERNAL
                 inProcedure,     // Proc, Func, etc. (carries forward
                 inFunction,      //                   until closed)
                 inConstructor,
                 inDestructor,
                 inProperty,
                 inRecord,        // block types
                 inObject,
                 inClass,
                 inGeneric,       // defining a generic (template)
                 inOneStmt,       // this affects the next statement
                 inWith,          // a with statement is in effect (will
                                  // carry over to block or stmt following
                 inBegin,         // begin-end block
                 inRepeat,        // repeat-until block
                 inTry,           // try-finally block
                 inIgnore,        // block where we should ignore code (ASM-END)
                 inCase,          // case-end but not record case (that is
                                  // closed with the end statement on record)
                 condIf,          // we are currently in a
                                  // successful {@IF ditective
                 condElse         // we are currently in the {$ELSE part
                                  // of a no-match {$IF directive
                 );

        KeywordType = (NoActDec,     //< words having no action for
                                     //< our purposes: array, to, of, in, etc
                                     //< most keywords will have this setting
                       ElemDec,      //< const, type, or var section, is
                                     //< released by:
                                     //<  implementation when interface ends
                                     //<  procedure, function in open code
                                     //<  begin in proc/func
                       ClearElem,    //< This keyword clears ElemDec flag

                      // These all start a block when found ; closed by
                       unitprogDec,  //< unit, module, libtary, or
                                     //< program header ; end
                       InterDec,     //< interface declaration  ; implemetation
                       impDec,       //< implementation declaration ; end
                       pfdec,        //< procedure or funcrion
                       BlockDec,     //< any block
                       CaseDec,      //< Case stmt
                       structdec,    //< Record, Object or class
                       nxstmtDec,    //< has next statement effect: probably
                                     //< "do" as used in for, while, with
                       closureDec,   //< This keyword closes some block or structure
                       usesDec,      //< uses clause
                       genMod,       //< general modifier
                       prepfmod,     //< modifier used before procedural header
                                     //< proc, func, property or method
                       postpfmod     //< modifier used after procedural
                  );

                   // info bits 
        MajorKind= ( inVisible,            //< not visible in cross-reference
                                           //< used for placeholder or
                                           //< "dummy" items
                     BaseType,             //< base type: char, integer etc.
                     refonly,              //< list only if requested to
                                           //< show usage of standard items
                                           //< used (for predefined items)

                     stdPF,                //< standard procedure/funcrion
                                           //< as opposed to a const/type/var
                     predefined,           //< stystem or user predefined
                                           //< has no definition location
                     compilerdefined,      //< predefined by compiler
                                           //< or by this program
                     AboveTheLine,         //< in interface
                     isLocal,              //< identifier declared within p/f 
                     selector,             //< selector in record CASE stmt   
                     SignatureKind,        //< args of proc/func 
                     temporary,            //< temporary item for search
                                           //< purposes (usually for WITH stmt
                     declared,             //< forward declaraation (forward or
                                           //< interface p/f  or pointer)
                     defined,              //< Actual definition (resolution of
                                           //< pointer reference or p/f after
                                           //< interface or forward)

                     // these indicate actual identifiers
                     prockind,             //< procedure
                     Funckind,             //< function
                     constKind,            //< CONST dec in code or signature
                     typeKind,             //< TYPE declaration
                     varKind,              //< VAR declaration in code or sig
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
                      StructureType,        //< structure: template, class,
                                            //< object, record etc.
                      isKeywordType,        //< action
                      ModifierType,         //< modifies keyword
                      PredefinedType);      //< predefined by system or other units

         LineP = ^LineTable;                //< list of line number usage

         signatureP = ^Signature;           //< proc/func arge

         StateP = ^State;
         State  = Record            //< Our running state
                Cond: StateCond;    //< condition
                closer: string;     //< if in block, symbol we watch for
                                    //< to close it
                BlockCount:integer; //< If in a block, how deep.
                WithCount: byte;    // if the with stmt had multiple records,
                                    // (or multiple With statements) number
                prev: StateP;       // if we are in a block, pointer to
                                    // state to return to
         end;



         Item = record                      //< identifier record  "Symbol Table"
                      Abbrev,               //< optional Short name
                      NameUC,               //< Upper case copy of Name
                      Name: ansistring;     //< its name as given
                      Index,                //< Identifier Serial Number
                      Count,                //< Usage count
                      DefPage,              //< page where it's defined
                      DefLine: Integer;     //< Line # or line # in Defpage
                      Usage: LineP;         //< Places where it's used
                      UnitIn: UnitP;        //< Unit it's in
                      Owner,                //< If in record, class of
                                            //< object, which one
                      LPS,                  //< chain of "lastProcStanding"
                                            //< links for nested procedures
                                            //< (see comment on that variable)

                      {  Note: here I have 4 pointers:
                         Prev/Next + inunit + ATL/Local, to indicate the list
                         of procedures declared/defined in a unit, both
                         "above the line" and those in implementation. Now, I
                         did a reconsideration. If it's in one of these places
                         do I need both? Well, for public procedures we need
                         to show where declared, and where defined. However,
                         we already have a setting for that in KIND. So,
                         because I might need two lists, I'll dike these out
                         for now so I can restore then later if I find I
                         do need differentiated list.
                      PrevinUnitATL,        //< prior item in this units interface
                      NextinUnitATL,        //< next item in interface
                      PrevinUnitLocal,      //< Prior item in implementation this unit
                      NextinUnitLocal,      //< Next item in implementation this unit
                      }
                      PrevInUnit,           //< prior itrm in unit
                      NextinUnit,           //< next item in unit

                      // enclosure: immediate container: record, object,
                      // class or procedural
                      PrevInPlace,          //< Prior item this enclosure
                      NextInPlace,          //< Next item this enclosure
                      PrevTotal,            //> Prior item in global symbol list
                      NextTotal: ItemP;     //< Next item in global symbol list
                      DT: Datatype;       //< what it uses
                      Kind: set of Majorkind;      //< statistics
                      case What: identType of      //< what it is
                        proceduralType:
                          ( SigCount: integer;     //< number of signatures
                            SignatureList: SignatureP; //< signature items
                            LocalTable,             //< List of local vars
                            ResultType: ItemP);     //< type if function or nil

                        SignatureType:
                           ( NextSignature: SignatureP;); //< next arg of p/f

                        ElementType:                    //< elements: const, type. var
                           ( NextField: ItemP);         //< if a field in record
                                                        //< class, or object next one
                                                        //< Our "owner" points to record

                        StructureType:                  //< structure: templ, obj, rec
                          ( Child: ItemP);              //< Structure it encapsulates
                                                        //< this is pointed back thru
                                                        //< "owner" field

                        isKeywordType, ModifierType:    //< Keywords and modifiers
                        (    Closedby:ItemP;            //< If this starts a block
                                                        //< what closes it
                             KW: KeywordType;           //< what it does
                             StateChange: StateCond;    //< if it chnages
                                                        //< our state
                             KeyWordClass,              //< What class of
                                                        //< keyword is it
                             Specials: word);           //< Special instrucrions

                        PreUnit, UnitType:              //< Unit details
                           ( Status: UnitP;             //< what has happened
                             Dotted: boolean;           //< is this a "dotted"
                                    //< unit (effectively a subunit or "record"
                                    //< of the parenr unit
                             ParentUnit,                //< if we are a
                                                        //< dotted unit
                            { Don't need this; use "owner" field
                             SiblingUnit,              //< If regular unit, nil

                                  // if a child unit, next one on chain
                             }
                             ChildUnit: ItemP);
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

    SearchType = (NoSearch,    // not started
                  TableEmpty,  // This particulae letter
                               // of table has no entries
                  SearchLow,   // This record precedes the
                               // lowest record
                  LowerRecord,   // Record not found, returning
                               // highest record found that
                               // is less than the search term
                  SearchMatch);// Matching record; if more
                               // than one, this is the first


VAR
    SearchResult: SearchType;

{ In order to make searches faster, I divide up the symbol table
  into 27 segments, consisting of the first letter of the identifier
  A-Z plus _ (underline). It is a double-liked list, SymbolTable is
  organized A-Z while SymbolBottom is organized Z-A. Basically I
  know how to do automatic sorting on a double-linked list, so
  that's what I used.
}
   SymbolBottom,                           //< in reverse order
   Symboltable: Array[IdentSize] of ItemP; //< alphabetical order

   // When a procedural declaration occurs, we save its entry, for various
   // purposes: (1) in case it will be modified (Forward or external);
   // (2) we see a const/type/var/declaration not in interface, we know
   // to whom this element belongs to
   // (3) we see a BEGIN statement, to know where references are
   // This is a "push down" list, if a new proc/func is defined
   // in IMPLEMENTATION, or in the case of a nested p/f, an intervening
   // p/f, we attach the prior one
   LastProcStanding: ItemP;

   // if in WITH, which one
   WithTable: WithP;

   // what we are doing - this is also a oush-down list
   StateTable: StateP;

   // this program's file name
   PasPath,PasFolder,PasName,PasExt: UnicodeString;

   // Bookkeeping
   IdentifierCount: Integer = 0;

   // pascal language variants
   Lang_extended,            // extended Pascal
   Lang_turbo,               // turbo Pascal
   Lang_XD,                  // XDPascal
   Lang_Borland,             // Borland Pascal
   Lang_object,              // object Pascal
   Lang_Delphi,              // Delphi
   Lang_FreePascal: boolean; // Free Pascal



   // General initialization
    Procedure Init;

   //symbol table related
    Function SearchSymbolTable(Which:Integer;             //< letter index #
                           Ident: AnsiString): ItemP; //< uppercase identifier
    Procedure DumpSymbolTable;

    // adds base classes, i.e. int, char, etc.
    Procedure AddBaseClass(N,   // name
              Abbr:AnsiString;  // short name
              NewDT:DataType;   // what it is
              Visible: Boolean = TRUE); // hide item if false
    // installs keywords
    Procedure AddKeyword(const N:AnsiString;  // name
                               BlockClose: AnsiString = '';  // presumed to require no block closure
                               KW:KeywordType=noactdec;// what kind of keyword
                               SC:StateCond =NoState);  // any changes

    // general Utility funcyions
    Function Plural(N:Int64; Plu:AnsiString; Sng: AnsiString): Ansistring;
    Function Version:String ;
    Function CopyRight:String ;



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


// This routine accepts the pointer passed to it
// obtains memory, then initializes it, retutning
// the address of  the memotu available
Procedure InitEntry(Var NewItem:ItemP);
Begin
  New(NewItem);
  inc(IdentifierCount);
  With NewItem^ do
  begin
      // don't init Ident because we want to
      // leave the key alone
      Name      := '';
      Abbrev    := '';
      NameUC    := '';
      NextTotal := NIL;
      PrevTotal := NIL;
      NextField := NIL;
      Count     := 0;
      Index     := IdentifierCount; // Serial Number

      // to prevent errors, null *all* pointers
      Usage              := NIL;
      UnitIn             := NIL;
      Owner              := NIL;
      LPS                := NIL;
      {
      PrevinUnitATL      := NIL;
      NextinUnitATL      := NIL;
      PrevinUnitLocal    := NIL;
      NextinUnitLocal    := NIL;
      }
      DefPage            := 0;
      DefLine            := 0;
      // Zero the largest variant and
      // you zero all others
      SigCount           := 0;
      SignatureList      := Nil;
      LocalTable         := Nil;
      ResultType         := Nil;

      PrevinUnit         := NIL;
      NextinUnit         := NIL;
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

    Function InsertInSymbolTable(Which:Integer;     //< letter index #
                                                    //< into symbol table
                                 Ident: AnsiString; //< uppercase identifier

                                 // the next one is if we want to
                                 // stop and return if we find an item
                                 // of the same name
                                 StopOnEqual: Boolean=FALSE ): ItemP;
    Var
       AddItem,
       NextItem,
       PriorItem: ItemP;

    begin
        If SymbolTable[Which] = NIL then
        begin     // first entry this "letter"
            InitEntry(SymbolTable[Which]);  // replaces new()
                                            // to initialise structure
            SymbolTable[Which]^.NameUC := Ident;

{       Not needed here because initentry will zero these anyway
            SymbolTable[Which]^.PrevTotal := NIl;
            SymbolTable[Which]^.NextTotal := NIL;
}
            // top and bottom now point to same entry
            SymbolBottom[Which] := SymbolTable[Which];

            // give this structure to caller, ready to use
            Result :=  SymbolTable[Which];
            exit
         end;
         If Ident <= SymbolTable[Which]^.NameUC then // is before beginning
         begin
             if StopOnEqual and (Ident = SymbolTable[Which]^.NameUC) then
                 Result := SymbolTable[Which]  // return item found
             else
             begin
                 InitEntry(AddItem);  // replaces new() to initialise structure
                 AddItem^.NameUC :=Ident;
{                Not needed here because initentry will zero this anyway
                 Additem^.PrevTotal := NIL;
}
                 // attach this entry to first item
                 AddItem^.NextTotal := SymbolTable[Which];

                 // make this top of chain
                 SymbolTable[Which]^.PrevTotal := AddItem;

                 // now make top of chain point here
                 SymbolTable[Which] := AddItem;
                 // give this structure to caller, ready to use
                 Result := AddItem;
             end;
             exit
         end;
         PriorItem := SymbolTable[Which];
         NextItem := SymbolTable[Which]^.NextTotal;

         repeat // at this point the prior
                // item was less than ItemUC, so let's see
                // if this one is, is equal, or is more
                If NextItem = NIL then
                begin  // we're at bottom of top-to-bottom chain
                    InitEntry( AddItem);      // replaces new()
                                              // to initialise structure
                    AddItem^.NameUC :=Ident;

{       Not needed here because initentry will zero this anyway
                 Additem^.NextTotal  := NIL;
}
             //  attach this entry to end of top-to-bottom chain
                     AddItem^.PrevTotal  := PriorItem;
             //  place at bottom of chain
                     PriorItem^.NextTotal := AddItem;
             // tell bottom pointer we're now last ebtry
                     SymbolBottom[Which] := AddItem;
             // give this structure to caller, ready to use
                     Result := AddItem;
                     exit
                 end;
                 if Ident <= NextItem^.NameUC then
                 begin  // "you know it takes some time,
                        // in the middle, in the middle"
                     if StopOnEqual and (Ident =NextItem^.NameUC) then
                        Result := NextItem    // return item found
                     else
                     begin
                         // it goes before this one, after previous
                         InitEntry(AddItem);  // replaces new() to
                                             // initialise structure
                         AddItem^.NameUC :=Ident;

                         // insert in the middlr of structure, "middle"
                         // being "after the first and before the last

                         // first, insert after prior item
                         PriorItem^.NextTotal := AddItem;

                         // now, the chain is broken, insert this "link" in the
                         // top-to-bottom chain
                         AddItem^.NextTotal := NextItem;

                         // now that top-down chain is fixed, insert
                         // this entry in the other (bottom-to-top) chain
                         NextItem^.PrevTotal := AddItem;

                         // agin we brokr the chain, insert this entry and close
                         AddItem^.PrevTotal := PriorItem;

                         // give this structure to caller, ready to use
                         Result := AddItem;
                     end;
                     exit
                 end;
                 // "But I still haven't found, what I'm looking for."
                 // Follow the chain and keep looking

                 // "rotate" the two items, prior item points here
                 PriorItem := NextItem;
                 // move to next item
                 NextItem  := NextItem^.NextTotal;
         until  false;   // keep going forever (or more likely, until we
                         // reach the  correct insertion point)
    end;

// This scans the symbol table and returns one of the following:
//   1. The particular table is empty so return NIL, set the SearchStatus
//      variable to TableEempty
//   2. The item we are searching for is alphabetically lower than the lowesr
//      item in the rable, return nil, set SearchStatus to SearchLow
//   3. The item is a match. return the record,
//      set SearchStatus to SearchMatch
//   4. The item is not found. Return the lowest record not exceeding this
//      item's alphabetic value, set SearchStatus to  LowerRecord.
Function SearchSymbolTable(Which:Integer;             //< letter index #
                           Ident: AnsiString): ItemP; //< uppercase identifier

    Var
    NextItem,
    PriorItem: ItemP;

    begin

        If SymbolTable[Which] = NIL then
        begin     // first entry this "letter"
            SearchResult := TableEmpty;
            Result :=  NIL;
            exit
        end;
        If Ident <= SymbolTable[Which]^.NameUC then // is before beginning
        begin
             if (Ident = SymbolTable[Which]^.NameUC) then
             Begin
                 SearChResult := SearchMatch;
                 Result := SymbolTable[Which]  // return item found
             End
             else
             begin
                 SearchResult := SearchLow;
                 Result := NIL;
             end;
             exit
        end;
        PriorItem := SymbolTable[Which];
        NextItem := SymbolTable[Which]^.NextTotal;

        repeat
                If NextItem = NIL then
                begin  // we're at bottom of top-to-bottom chain
                    SearchResult := LowerRecord;    // we're returning prior rec
                    Result := PriorItem;
                    exit
                 end;
                 if Ident = NextItem^.NameUC then
                 begin  // "you know it takes some time,
                        // in the middle, in the middle"
                     SearchResult :=  SearchMatch;  // return item found
                     Result := NextItem ;
                     exit;
                 end;
                 If Ident < NextItem^.NameUC then
                 begin
                     // it goes before this one, after previous
                     SearchResult :=  LowerRecord; // we're returning prior rec
                     Result := PriorItem;
                     exit;
                 end;

                 // "But I still haven't found, what I'm looking for."
                 // Follow the chain and keep looking

                 // "rotate" the two items, prior item points here
                 PriorItem := NextItem;
                 // move to next item
                 NextItem  := NextItem^.NextTotal;
        until  false;   // keep going forever (or more likely, until we
                         // reach the  correct insertion point)
END;



// adds a base class, i.e. int, char, etc. to stmbol table
Procedure AddBaseClass(N,   // name
          Abbr:AnsiString;  // short name
          NewDT:DataType;   // what it is
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
          DT    := NewDT;
          Kind  := [BaseType,RefOnly,predefined];
          If Not Visible then
              Kind := Kind+[invisible];
          What  := ElementType;
      end;
end;

// adds keywords and modifiers
Procedure AddKeyword(const N:AnsiString;                 // name
                           BlockClose: AnsiString = '';  // presumed to require no block closure
                           KW:KeywordType=noactdec;      // what kind of keyword
                           SC:StateCond =NoState);       // any changes
var
      Which: integer;
      Ident,
      CW: ansistring;
      CloseItem,      // keyword belonging to blockclose
      NewItem: ItemP;
                        

BEGIN
      // standard initialization
        Ident := UpperCase(N);
        CW := Copy(Ident,1,1);
        For Which := 1 to IdentMax do
             if CW = ValidIdent[Which] then
               break;

        // TRUE in the following function call means that if this name
        // is previously defined, return that one, don't make a duplicate

        NewItem := InsertInSymbolTable(Which,Ident,TRUE);

        // tight now we have one of three things:
        // 1. A new blank record, which we created now, initialize it
        // 2. A previously called "closer" kryword which
        //    is now appearing to pick up its initialization
        // 3. A duplicate keyword because different dialects of Pascal
        //    have additional keywords and modifiers. Since modifiers
        //    can be the same as identifiers, make sure if one has the
        //    same name as a modifier, make the modifier later in the
        //    in the stack so the identifier is found first

        if  NewItem^.name <> '' then  // don't reinitialize
        With NewItem^ do
        begin
            Name  := N;
            DT    := NoType;
            Kind  := [ refonly ,CompilerDefined ];

            StateChange := SC;
                  Count := 0;
        END;

        // If this keyword requires a "closer" find it. If it has not yet
        // been defined, create it. When it shows up, we'll fully initialize it
        if BlockClose <> '' then
        begin // Oksy, go look for it
            Ident := UpperCase(BlockClose);
            CW := Copy(Ident,1,1);
            For Which := 1 to IdentMax do
                if CW = ValidIdent[Which] then
                    break;

            // In our case, we request a symbol table slot,
            // if this entry was nor previously created
            // (by another keyword that uses it to
            // close a block,) we don't want a new entry,
            // we want the previous one.

            // if it's already there, take it
            CloseItem := InsertInSymbolTable(Which,Ident,TRUE);
            NewItem^.Closedby := CloseItem;    // this will never be nil

            if CloseItem^.Name ='' then  // it doesn't exist,
                                         // this is a new entry
            with CloseItem^ do
            begin
                 Kind  := [ temporary ]; // use as debugging tool
                 What  :=  isKeywordType;
                 Inc(Count);
            end;
        end;
END;


Procedure AddStdVar(N:Ansistring; NewDT:DataType);
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
            DT    := NewDT; 

            Kind  := [ varkind, refonly, Predefined];
{ add later if hidden procs needed
            If not Visible  then
                 Kind := Kind+[invisible];
}
            What  := ElementType;
          END;

END;


Procedure AddStdType(N:Ansistring; NewDT:DataType);
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
            DT    := NewDT;
            Kind  := [ Typekind, refonly, Predefined ];
{ add later if hidden procs needed
            If not Visible  then
                 Kind := Kind+[invisible];
}
            What  := ElementType;
          END;

END;


Procedure AddStdFunc(N:Ansistring; NewDT:DataType);
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
            DT    := NewDT;
            //       set by       its a     don't    Standard
            //       compiler     function  list     procedural
            Kind  := [predefined, funckind, refonly, stdPF];
{ add later if hidden procs needed
            If not Visible  then
                 Kind := Kind+[invisible];
}
            What  := proceduralType;
          END;

END;


Procedure AddStdConst(N:Ansistring; NewDT:DataType);
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
          DT    := NewDT;
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
        DT  := notype;
        Kind  := [RefOnly,predefined,isUnit];
        If not Visible  then
             Kind := Kind+[invisible];
        What  := UnitType;
      END;
END;

Procedure AddStdProc(N:Ansistring);
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
            DT    := notype;
            Kind  := [predefined, prockind, refonly, stdPF];
{ add later if hidden procs needed
            If not Visible  then
                 Kind := Kind+[invisible];
}
            What  := proceduralType;
          END;

END;

Procedure AddStdFile(N:Ansistring);
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
            DT  := FileType;
            Kind  := [predefined,  refonly];
{ add later if hidden files needed
            If not Visible  then
                 Kind := Kind+[invisible];
}
            What  := proceduralType;
          END;

END;



Procedure Init;
Var
    I: Integer;

begin
    New(StateTable);
    With StateTable^ Do
    Begin
        Cond:= NoState;
        Closer:= '';
        WithCount:= 0;
        Prev:= NIL;
    End;

    WithTable := NIL;

    For I := 1 to IdentMax do
       SymbolTable[I] := Nil;

{For thr initialization, we load only the
 identifiers of standard Pascal. Extended,
 Turbo, Borand, UCSD, XDPascal, Object, Delphi, or
 Free Pascal extensions will be installed
 later. This is another resson to have
 keywords as symbol table entries: I can
 add or remove them as needed, but standard
 Pascal items can't be removed. (Can't
 examine a program if Begin is dropped!)
}

                 // name,         nickname,     type      Visible
    AddBaseClass('enumerator',       'enum',    EnumGroup,False);  // PLCEHLDR
    AddBaseClass('enumerated value', 'econ',    EnumType, False);  // PLCEHLDR
    AddBaseClass('byte',             'byte',     Bytetype);   // 8-bit integer
    AddBaseClass('boolean',          'bool',     Booltype);   // boolean
    AddBaseClass('char',             'char',     charType);   // 8-bit character
    AddBaseClass('ansichar',         'ansc',     charType);   // alias for char
    AddBaseClass('word',             'word',     wordType);   // 16-bit integer
    AddBaseClass('widechar',         'wchr',    wcharType);   // 16-bit char
    AddBaseClass('integer',           'int',      intType);   // 32-bit integer
    AddBaseClass('longint',           'int',      intType);   // alias for int
    AddBaseClass('int64',             'I64',    int64Type);   // 64-bit int
    AddBaseClass('pointer',           'ptr',  pointerType);   // address
    AddBaseClass('set',               'set',      setType);   // set
    AddBaseClass('array',             'arr',   ArrayType);    // general array
    AddBaseClass('string',            'str',   stringType);   // array of char
                                                              // with length
    AddBaseClass('ansistring',        'anss',   AnsStrType);   // Ansistring
    AddBaseClass('unicodestring',     'uni',   uniStrType);   // unicodeString
    AddBaseClass('record',            'rec',   RecordType);   // record
    AddBaseClass('object',            'obj',   objectType);   // object
    AddBaseClass('class',             'cls',    ClassType);   // class
    AddBaseClass('template',         'tmpl', TemplateType);   // class
    AddBaseClass('real',              'real',    realtype);   // 32-bit real
    AddBaseClass('single',            'sng',   singleType);   // single
    AddBaseClass('double',            'Dbl',   DoubleType);   // double
                                                              // precision real
    AddBaseClass('extended',          'Ext',  ExtendedType);  // 80-bit real
    AddBaseClass('file',             'file',      FileType);  // file

// Std functions and procedures

    AddStdConst('false',       Booltype);
    AddStdConst('true',        Booltype);
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


// std keywords


    AddKeyWord('and');
    AddKeyWord('do','', nxstmtDec  );
    // END (and any closure) does not have to
    // say anything; the gentleman calling err
    // I mean keyword calling will
    // announce when it needs her
    AddKeyWord('end','', closureDec );
    AddKeyWord('if','', NoActDec, NoState );  // default
    AddKeyWord('in' );
    AddKeyWord('not');
    AddKeyWord('of' );
    AddKeyWord('or' );
    AddKeyWord('to' );
    AddKeyWord('shl');
    AddKeyWord('shr');
    AddKeyWord('xor');
    AddKeyWord('for'  );
    AddKeyWord('var','', ElemDec , invar );
    AddKeyWord('div'  );
    AddKeyWord('mod'  );
    AddKeyWord('set'   );
    AddKeyWord('then','', nxstmtDec  );
    AddKeyWord('else','', nxstmtDec , inCase ); // also used on case
    AddKeyWord('with','', BlockDec, inwith );
    AddKeyWord('goto' );
    AddKeyWord('case', 'end' , BlockDec, incase );
    AddKeyWord('type', '', ElemDec,intype  );
    AddKeyWord('begin', 'end' ,BlockDec, inBegin  );
    AddKeyWord('until','',closureDec);
    AddKeyWord('while','',nxstmtDec);
    AddKeyWord('array'  );
    AddKeyWord('const','',ElemDec, inConst );
    AddKeyWord('label' );
    AddKeyWord('repeat','until' ,BlockDec,  inRepeat );
    AddKeyWord('record', 'end' , structdec, inRecord );
    AddKeyWord('downto' );
    AddKeyWord('packed' );
    AddKeyWord('forward','', postpfmod, inForward );
    AddKeyWord('program','',unitprogDec, inProgram );
    AddKeyWord('external', '',postpfmod, inExternal );
    AddKeyWord('function','',pfdec,inFunction );
    AddKeyWord('procedure', '' ,pfdec, inProcedure );

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


    // extended
    if Lang_extended then
    begin
        AddKeyWord('and_then','', nxstmtDec );
        AddKeyword('export');
        AddKeyword('import');
        AddKeyword('module','end',unitprogDec, inunit );
        AddKeyword('only');
        AddKeyword('or_else','', nxstmtDec );
        // equiv to rlse on case stmt
        AddKeyword('otherwise', '' , BlockDec, incase );
        AddKeyword('protected','',genmod);
        AddKeyword('qualified','',genmod);
        AddKeyword('restricted','',genmod);
        AddKeyword('value');

        AddStdFunc('pow',Realtype);
    end;



 {   AddKeyword(AnsiString;  // name
               AnsiString   // presumed to require no block closure
               KeywordType  // what kind of keyword
               NoState      // any changes


               StateCond = (NoState,         // Not in a state
                            inProgram,       // program
                            inUnit,          // unit found
                            inInterface,     // Which part (carries to succeeding statements)
                            inImplementation,
                            inConst,         // CONST, TYPE, VAR declaration
                            inType,
                            inVar,
                            inforward,       // FORWARD
                            inExternal,      // EXTERNAL
                            inProcedure,     // Proc, Func, etc. (carries forward
                            inFunction,      //                   until closed)
                            inConstructor,
                            inDestructor,
                            inProperty,
                            inRecord,        // block types
                            inObject,
                            inClass,
                            inGeneric,       // defining a generic (template)
                            inOneStmt,       // this affects the next statement
                            inWith,          // a with statement
                            inBegin,         // begin-end block
                            inRepeat,        // repeat-until block
                            inTry,           // try-finally block
                            inIgnore,        // block where we should ignore code (ASM-END)
                            inCase,          // case-end but
                            condIf,          // in an $IF directive
                            condElse         // in the $ELSE part
                            );

                   KeywordType = (NoActDec,     //< words having no action
                                  ElemDec,      //< const, type, or var
                                  ClearElem,    //< clears ElemDec flag
                                  unitprogDec,  //< unit or program
                                  InterDec,     //< interface
                                  impDec,       //< implementation
                                  pfdec,        //< procedure or funcrion
                                  BlockDec,     //< any block
                                  CaseDec,      //< Case stmt
                                  structdec,    //< Record, Object or class
                                  nxstmtDec,    //< has next statement effect
                                  closureDec,   //<  closes some block
                                  usesDec,      //< uses clause
                                  genMod,       //< general modifier
                                  prepfmod,     //< used before procedural
                                  postpfmod,    //< used after procedural
                           );
}

    if lang_turbo then // turbo pascal
    begin
        AddKeyword('absolute','',genMod);
        AddKeyword('asm','',BlockDec,inIgnore);
        AddKeyword('constructor','',pfDec,inConstructor);
        AddKeyword('destructor','',pfDec,inDestructor);
        AddKeyword('implementation','',impDec,inImplementation);
        AddKeyword('in');
        AddKeyword('inherited','',genMod);
        AddKeyword('inline','',pfDec);
        AddKeyword('interface','',interDec,inInterface);
        AddKeyword('object','end',structdec,inObject);
        AddKeyword('operator','',genMod);
        AddKeyword('reintroduce');
        AddKeyword('self');
        AddKeyWord('unit','end',unitprogDec, inunit );
        AddKeyword('uses','',usesDec);

        AddStdUnit('system');
    end;

    if Lang_XD then  // XDPascal
    begin
        AddKeyword('implementation','',impDec,inImplementation);
        AddKeyword('interface','',interDec,inInterface);
        AddKeyWord('unit','end',unitprogDec, inunit );
        AddKeyword('uses','',usesDec);


        AddStdUnit('system');
    end;

    if Lang_Borland then // Borland Pascal
    begin



        AddStdUnit('system');
    end;

    if Lang_object then // Object Pascal
    begin
        AddKeyWord('as');
        AddKeyWord('class','end',structDec, inClass);
        AddKeyWord('dispinterface');
        AddKeyWord('except');
        AddKeyWord('exports');
        AddKeyWord('finalization');
        AddKeyWord('finally');
        AddKeyWord('initialization');
        AddKeyWord('inline');
        AddKeyWord('is');
        AddKeyWord('library','end',unitprogDec, inLibrary);
        AddKeyWord('on');
        AddKeyWord('out');
        AddKeyWord('packed');
        AddKeyWord('property','',GenMod, inProperty);
        AddKeyWord('raise');
        AddKeyWord('resourcestring');
        AddKeyWord('threadvar');
        AddKeyWord('try','',blockDec, inTry);

        // these are not reserved words, but it's
        // orobably not a good idea to rdefine them
        AddStdProc('exit');
        AddStdProc('halt');
// these are considered modifiers
//        AddSysProc('break');
//        AddSysProc('continue');

        // modifiers
        AddKeyword('absolute','',genmod);
        AddKeyword('abstract','',genmod);
        AddKeyword('alias','',genmod);
        AddKeyword('assembler','',genmod);
        AddKeyword('bitpacked','',genmod);
        AddKeyword('break','',genmod);
        AddKeyword('cdecl','',genmod);
        AddKeyword('continue','',genmod);
        AddKeyword('cppdecl','',genmod);
        AddKeyword('cvar','',genmod);
        AddKeyword('default','',genmod);
        AddKeyword('deprecated','',genmod);
        AddKeyword('dynamic','',genmod);
        AddKeyword('enumerator','',genmod);
        AddKeyword('experimental','',genmod);
        AddKeyword('export','',genmod);
        AddKeyword('external','',genmod);
        AddKeyword('far','',genmod);
        AddKeyword('far16','',genmod);
//        forward  -this is std
        AddKeyword('helper','',genmod);
        AddKeyword('implements','',genmod);
        AddKeyword('index','',genmod);
        AddKeyword('interrupt','',genmod);
        AddKeyword('iocheck','',genmod);
        AddKeyword('local','',genmod);
        AddKeyword('message','',genmod);
        AddKeyword('name','',genmod);
        AddKeyword('near','',genmod);
        AddKeyword('nodefault','',genmod);
        AddKeyword('noreturn','',genmod);
        AddKeyword('nostackframe','',genmod);
        AddKeyword('oldfpccall','',genmod);
        AddKeyword('otherwise','',genmod);
        AddKeyword('overload','',genmod);
        AddKeyword('override','',genmod);
        AddKeyword('pascal','',genmod);
        AddKeyword('platform','',genmod);
        AddKeyword('private','',genmod);
        AddKeyword('protected','',genmod);
        AddKeyword('public','',genmod);
        AddKeyword('published','',genmod);
        AddKeyword('read','',genmod);     // both a mod and a proc
        AddKeyword('register','',genmod);
        AddKeyword('reintroduce','',genmod);
        AddKeyword('result','',genmod);   // teyurn val of proc
        AddKeyword('safecall','',genmod);
        AddKeyword('saveregisters','',genmod);
        AddKeyword('softfloat','',genmod);
        AddKeyword('specialize','',genmod);
        AddKeyword('static','',genmod);
        AddKeyword('stdcall','',genmod);
        AddKeyword('stored','',genmod);
        AddKeyword('strict','',genmod);
        AddKeyword('unaligned','',genmod);
        AddKeyword('unimplemented','',genmod);
        AddKeyword('varargs','',genmod);
        AddKeyword('virtual','',genmod);
        AddKeyword('winapi','',genmod);
        AddKeyword('write','',genmod);   // both std proc and mod

        AddStdUnit('system');
    end;

    if Lang_Delphi then  // Delphi
    begin

        AddStdUnit('system');
    end;

    if Lang_FreePascal then
    begin

        AddStdUnit('system');
    end;









    dumpSymbolTable;
 end;


Procedure DumpSymbolTable;
Var
    Walker: ItemP;
    Q: Majorkind;
    T: DataType;
    X: IdentType;
    Z,
    K,
    I: Integer;
    SC: StateCond;

    Procedure Retrieve(N:AnsiString);
    var
      Which: integer;
      Ident,
      CW: ansistring;
      NewItem: ItemP;

    BEGIN
        Writeln(' Utem is ',N);
        Ident := UpperCase(N);
        CW := Copy(Ident,1,1);
        For Which := 1 to IdentMax do
             if CW = ValidIdent[Which] then
               break;

        NewItem := SearchSymbolTable(Which,Ident);
        writeln('Searchresult=',Searchresult);
        If NewItem = NIL then
           Writeln('Returned null')
        Else
           Writeln('Result is ',NewItem^.NameUC);

    END;

begin
    Z :=0;
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
            Inc(Z);
            With Walker^ do
            begin
               Write(Z:4,',', K:4,' Name=');
               Write(Name,', ');
               if Name = '' then
                   Write('"',NameUC,'" ');

               Write('(',abbrev,') [');
               For T := notype to filetype  do
                  if T in [DT] then
                    write(T,' ');
               write('] ');
               for Q := invisible to isunit do
               if Q in Kind then
                  write(Q,' ');
               if Count>0 then
                  write(' Count=',Count,', ');;
               writeln;
               Write('    ');
               for X := proceduralType to PredefinedType do
                 if X in [What] then
                    Write(X,' ');
               if Closedby<>Nil then
               begin
                 Write('Closedby=',ClosedBy^.NameUC,'= ');
               end;
               for SC :=  NoState to inCase do
                  if  SC  in [StateChange] then
                    Write(SC,' ');
               writeln;
            end;
            Walker:= Walker^.NextTotal;
        end;
    end;
    Writeln;
    Writeln(' Total: ',Z);

    Writeln('Test search');
    Retrieve('if');
    Retrieve('apple');
    Retrieve('z');
    Retrieve('baker');
    Retrieve('begone');
    Retrieve('Written');
    Retrieve('I');



end;

end.



