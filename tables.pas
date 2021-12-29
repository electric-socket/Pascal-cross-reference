{$I CrossrefPrefixCode.inc}  // STD definitions
// Crossref - Pascal Cross reference
// Version 0.01
// January 9, 2021
// Copyright 2021 Paul Robinson
// Licensed under GPL v2

// Tables,pas -  used for shared data structures
unit Tables;

{$mode ObjFPC}{$H+}
interface
Const
    Months: array[1..12] of string[9]=
        ('January','February','March',   'April',   'May','     June',
         'July',    'August', 'September','October','November', 'December');
    Days: Array[0..6] of string[9]=
        ('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday');



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

     NameMax = 10;  // max number of folders or extensions
     SlashChar = '\'; // Your system's directory separator,
                      // \ on windows; / on unix, linux, etc

     // Size of largest variant in Item in 32/64-bit words
     {$IFDEF Bits32}
            DummyMax =24;
     {$ELSE}
            DummyMax =12;
     {$ENDIF}


type
     TokenType =(
       // State indicators
            EMPTYTOK,        // no symbol yet

            EOFTOK,          // end of file during buffered read
            ENDOFLINETOK,    // end of line or // found

        // Delimiters
            OPARENTOK,       // (
            CPARENTOK,       // )
            MULTOK,          // *
            PLUSTOK,         // +
            COMMATOK,        // ,
            MINUSTOK,        // -
            PERIODTOK,       // .
            DIVTOK,          // /
            HASHTOK,         // #
            DOLLARTOK,       // $
            COLONTOK,        // :
            SEMICOLONTOK,    // ;
            ADDRESSTOK,      // @
            OBRACKETTOK,     // [
            CBRACKETTOK,     // ]
            DEREFERENCETOK,  // ^
            AMPERSANDTOK,    // &
            EQTOK,           // =
            LTTOK,           // <
            GTTOK,           // >
            RANGETOK,        // ..
            BECOMESTOK,      // :=
            LETOK,           // <=
            NETOK,           // <>
            GETOK,           // >=
            KEYWORDTOK,      // Item is keyword
            MODIFIERTOK,     // Item is modifier
            NONKEYWORD,      // Poyential keyword protected by &
            IDENTTOK         // Identifier not recognized
       );


     TokenEntry = Record
           CH: WideChar;
           SY: TokenType;
     end;


     InPtr = ^InputBuffer;
     InputBuffer = Record
            Next: InPtr;                 //< prior buffer
            F: Text;                     //< File being read
            InIndex,                     //< Index in string
            Len:Integer;                 //< Length of string

            Error,                       //< Error opening file if nonzero
            Position,                    //< position on text line
            LineNumber: Integer;         //< count of lines read this file

            TextLine,                    //< input linr as written
            TextUC,                      //< input line in UPPER CASE

            // file descriptor
            UnitName,                    //< declared name of unit
            FullName,                    //< Fullnsme
            Folder,                      //< folder it's in, null if none
            Name,                        //< name excluding extension
            Ext: UnicodeString;          //< extension if any
     end;

     OutputBuffer = Record

// Line Information
            // where we are on each output line
            TextPosition,                //< position on output line
            HTMLPosition,                //< position on HTML line
            PDFPosition,                 //< position on PDF line

            // User can select a maximum width per line, lines longer than
            // this will fold ro the next line which will not be marked with
            // line prefix information.  0 means no limit, minimum is 48
            TextWidthMax,                //< Maximum length of output lines
            HTMLWidthMax,                //< Maximum length of HTML lines
            PDFWidthMax,                 //< Maximum length of PDF lines
                                         //< (If doing separate documents)

            TextLineCount,               //< Total Number of output lines
            HTMLLineCountr,              //< Total Number of HTML lines
            PDFLineCount,                //< Total Number of PDF lines
                                         //< (If doing separate documents)

// Page Information
            TextPageNumber,              //< Page Number on output file
            HTMLPageNumber,              //< Page Number on HTML file
            PDFPageNumber,               //< Page Number on PDF file
                                         //< (If doing separate documents)

            // This is the displayed line number
            TextPageLineNumber,              //< Line Number this page
            HTMLPageLineNumber,              //< Line Number this page
            PDFPageLineNumber,               //< Line Number this page

            // This is the actual numbert of lines printed this page
            // (accounting for headers, footers, folded lines)
            TextPageLines,               //< Number of Lines this page
            HTMLPageLines,               //< Number of Lines this page
            PDFPageLines,                //< Number of Lines this page

            // User can select a maximum psge length, once that many lines
            // have been geneerated, a new page is generated: 0 means
            // no limit, minimum is 28. Note these can be different for
            // each output format, and do not have to match. Recommended
            // is 60 lines/page text, 0 lines HTML, 100 lines PDF
            TextLengthMax,               //< Maximum height of output page
            HTMLLengthMax,               //< Maximum height of HTML page
            PDFLengthMax                 //< Maximum height of PDF page
                                         //< (If doing separate documents)

            : Integer;

            // file descriptorS
            TextFileName,                //< Full nsme
            TextFolder,                  //< folser it's in, null if none
            TextName,                    //< name excluding extension
            FileExt,                     //< extension if any

            HTMLFileName,                //< Full nsme
            HTMLFolder,                  //< folser it's in, null if none
            HTMLName,                    //< name excluding extension
            HTMLExt,                     //< extension if any

            // If doing separate documents
            PDFFileName,                 //< Full nsme
            PDFFolder,                   //< folser it's in, null if none
            PDFName,                     //< name excluding extension
            PDFExt                       //< extension if any
            : UnicodeString;

            TexrF,                       //< Files being written
            HTMLF,
            PDFF: Text;                  //< PDF output only if each unit
                                         //< is generated to a separate file
     end;

     NameRange = 1..NameMax; // max. folders or exyensions


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

        KeywordType = (UndefKeyword, //< Three Guesses
                       NoActDec,     //< words having no action for
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
        MajorKind= ( NothingAtAll,         //< default, unassignef
                     inVisible,            //< not visible in cross-reference
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

         IdentType = (UnusedType,           //< For initializing the variant
                      proceduralType,       //< proc, func, method, prop, cons, destr
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

         // Size of largest variant in Item in 32/64-bit words
         DummySize = 1 .. DummyMax;

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

                      // enclosure: immediate container: record, object,
                      // class or procedural
                      PrevInPlace,          //< Prior item this enclosure
                      NextInPlace,          //< Next item this enclosure
                      PrevTotal,            //> Prior item in global symbol list
                      NextTotal: ItemP;     //< Next item in global symbol list
                      DT: Datatype;         //< what it uses
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
                      PrevinUnitATL,        //< prior item in this
                                            //< unit's interface
                      NextinUnitATL,        //< next item in interface
                      PrevinUnitLocal,      //< Prior item in
                                            //< implementation this unit
                      NextinUnitLocal,      //< Next item in implementation
                                            //< this unit
                    }
                             PrevInUnit,                //< prior itrm in unit
                             NextinUnit,                //< next item in unit
                             ParentUnit,                //< if we are a
                                                        //< dotted unit
                            { Don't need this; use "owner" field
                             SiblingUnit,              //< If regular unit, nil

                                  // if a child unit, next one on chain
                             }
                             ChildUnit: ItemP);

                             UnusedType:       // Take at least as much room
                                               // as the largest other
                                               // variant to INIT can zero it
{$IFDEF Bits32}
                            (  Dummy: Array[DummySize] of integer; );
{$ELSE}
                            (  Dummy: Array[DummySize] of int64; );
{$ENDIF}
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
                       LowerRecord, // Record not found, returning
                                    // highest record found that
                                    // is less than the search term
                       SearchMatch);// Matching record; if more
                                    // than one, this is the first

  // ** Symbol Table items **

VAR
     SearchResult: SearchType;

     { In order to make searches faster, I divide up the symbol table
       into 27 segments, consisting of the first letter of the identifier
       A-Z plus _ (underline). It is a double-liked list, SymbolTable is
       organized A-Z while SymbolBottom is organized Z-A. Basically I
       know how to do automatic sorting on a double-linked list, so
       that's what I used.
     }
     SymbolBottom,       //< in reverse order
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
     Lang_extended: boolean = true;   // extended Pascal
     Lang_turbo: boolean = true;      // turbo Pascal
     Lang_XD: boolean = true;         // XDPascal
     Lang_Borland: boolean = true;    // Borland Pascal
     Lang_object: boolean = true;     // object Pascal
     Lang_Delphi: boolean = true;     // Delphi
     Lang_FreePascal: boolean = true; // Free Pascal

     // folders and extensions
     FolderTable,
     Extensions: Array [NameRange] of unicodeString;
     TopFolder,
     TopExtension: NameRange;

      TokenList: Array [1..26] of TokenEntry = (
      (CH: 'A'; SY: IDENTTOK),
      (CH: 'B'; SY: IDENTTOK),
      (CH: 'C'; SY: IDENTTOK),
      (CH: 'D'; SY: IDENTTOK),
      (CH: 'E'; SY: IDENTTOK),
      (CH: 'F'; SY: IDENTTOK),
      (CH: 'G'; SY: IDENTTOK),
      (CH: 'H'; SY: IDENTTOK),
      (CH: 'I'; SY: IDENTTOK),
      (CH: 'J'; SY: IDENTTOK),
      (CH: 'K'; SY: IDENTTOK),
      (CH: 'L'; SY: IDENTTOK),
      (CH: 'M'; SY: IDENTTOK),
      (CH: 'N'; SY: IDENTTOK),
      (CH: 'O'; SY: IDENTTOK),
      (CH: 'P'; SY: IDENTTOK),
      (CH: 'Q'; SY: IDENTTOK),
      (CH: 'R'; SY: IDENTTOK),
      (CH: 'S'; SY: IDENTTOK),
      (CH: 'T'; SY: IDENTTOK),
      (CH: 'U'; SY: IDENTTOK),
      (CH: 'V'; SY: IDENTTOK),
      (CH: 'W'; SY: IDENTTOK),
      (CH: 'X'; SY: IDENTTOK),
      (CH: 'Y'; SY: IDENTTOK),
      (CH: 'Z'; SY: IDENTTOK)
      );


       Buffer: InPtr;


implementation

end.

