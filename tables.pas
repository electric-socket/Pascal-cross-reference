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
Uses Windows;  // Needed to define date vars

const


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
    alphaUC = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L',
                  'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X',
                  'Y', 'Z'];
    alphaLC = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l',
                  'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x',
                  'y', 'z'];
     alphabet = alphaUC + alphaLC;
     Underscore =  ['_'];
     IdentScan =  alphabet + Underscore ;   // used for scan; includes lowercase
     IdentSearch =  alphaUC + Underscore ;  // user for seaech; uppercase only

     digits = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'];
     alphanum = IdentScan + digits + Underscore + Underscore +  // for scan
                (* 4 spares *)  Underscore + Underscore ;
     alphanumU= IdentSearch + digits + Underscore + Underscore +  // for search
                (* 4 spares *)  Underscore + Underscore ;

type
     TokenType =(
        // State indicators
            EMPTYTOK,        // no symbol yet; this is used as
                             // start of symbol search range
        // Delimiters
        // Note on delimiters: they will be added to the keyword table, with
        // two consecutive characters checked; then the first one is; so
        // double characters, .. := (* etc. must appear before the single one
            ADDRESSTOK,      // @
            AMPERSANDTOK,    // &
            BCOMMENTTOK,     // { OPEN BRACE COMMENT
            BCOMMENTENDTOK,  // } CLOSE BRACE COMMENT
            BCOMMENT1TOK,    // { Optional comment 1
            BCOMMENT1ENDTOK, // } Closure            
            BCOMMENT2TOK,    // { Optional comment 2
            BCOMMENT2ENDTOK, // } Closure
            BCOMMENT3TOK,    // { Optional comment 3
            BCOMMENT3ENDTOK, // } Closure
            BCOMMENT4TOK,    // { Optional comment 4
            BCOMMENT4ENDTOK, // } Closure
            BCOMMENT5TOK,    // { Optional comment 5
            BCOMMENT5ENDTOK, // } Closure
            COMMENTTOK,      // (*
            COMMENTENDTOK,   // *)
            CBRACKETTOK,     // ]
            DCBRACKETTOK,    // .) OLD STYLE [
            DOBRACKETTOK,    // (. OLD STYLE [
            DOUBLEDOTTOK,    // .. (Can't be RANGETOK  as must come before PERIOD )
            BECOMESTOK,      // :=
            BOOLLETOK,       // <= These three are Named
            BOOLNETOK,       // <> so these symbols come
            BOOLGETOK,       // >= before the single char
            COLONTOK,        // :
            COMMATOK,        // ,
            CPARENTOK,       // )
            DBLSLASHTOK,     // // This item must come before DIVTOK
            DEREFERENCETOK,  // ^
            DIVTOK,          // /
            DOLLARTOK,       // $
            EQTOK,           // =
            GTTOK,           // >
            HASHTOK,         // #
            LTTOK,           // <
            MINUSTOK,        // -
            MULTOK,          // *
            OBRACETOK,       // [
            OPARENTOK,       // (
            PERIODTOK,       // .
            PLUSTOK,         // +
            QUOTETOK,        // '
            SEMICOLONTOK,    // ;
            DIGIT0TOK,       // 0
            DIGIT1TOK,       // 1
            DIGIT2TOK,       // 2
            DIGIT3TOK,       // 3
            DIGIT4TOK,       // 4
            DIGIT5TOK,       // 5
            DIGIT6TOK,       // 6
            DIGIT7TOK,       // 7
            DIGIT8TOK,       // 8
            DIGIT9TOK,       // 9
            CCNONETOK,      // used as start of conditionals
        // conditional compilation tokens
            CCDEFINETOK,    // $DEFINE
            CCELSETOK,      // $ELSE  
            CCELSEIFTOK,    // $ELSEIF
            CCENDIFTOK,     // $ENDIFE
            CCI,            // $I[NCLUDE]
            CCIFTOK,        // $IF
            CCIFDEFTOK,     // $IFDEF
            CCIFNDEFTOK,    // $IFNDEF
            CCINCLUDE,      // $INCLUDE
            CCINCLUDEPATH,  // $INCLUDEPATH
            CCUNDEFTOK,     // $UNDEF
            cclast,         // sentinel for end
        //  Keywords
          // marker for keywords starting with a
            ABSOLUTETOK,      // ABSOLUTE
            ANDTOK,           // AND
            AND_THENTOK,      // AND_THEN
            ARRAYTOK,         // ARRAY
            ASTOK,            // AS
            ASMTOK,           // ASM
          // marker for keywords starting with b   
            BEGINTOK,         // BEGIN
            BITPACKEDTOK,     // BITPACKED
          // marker for keywords starting with c                 CASETOK,          // CASE
            CLASSTOK,         // CLASS
            CONSTRUCTORTOK,   // CONSTRUCTOR
            CONSTTOK,         // CONST
            CPPCLASSTOK,      // CPPCLASS
          // marker for keywords starting with d
            DESTRUCTORTOK,    // DESTRUCTOR
            DISPINTERFACETOK, // DISPINTERFACE
            IDIVTOK,          // DIV
            DOTOK,            // DO
            DOWNTOTOK,        // DOWNTO
          // marker for keywords starting with e
            ELSETOK,          // ELSE
            ENDTOK,           // END
            EXCEPTTOK,        // EXCEPT
            EXPORTSTOK,       // EXPORTS
            EXTERNALTOK,      // EXTERNAL
          // marker for keywords starting with f
            FILETOK,          // FILE
            FINALIZATIONTOK,  // FINALIZATION
            FINALLYTOK,       // FINALLY
            FORTOK,           // FOR
            FUNCTIONTOK,      // FUNCTION
          // marker for keywords starting with g     
            GOTOTOK,          // GOTO
          // marker for keywords starting with h
          // marker for keywords starting with i
            IFTOK,            // IF
            IMPLEMENTATIONTOK,// IMPLEMENTATION
            INHERITEDTOK,     // INHERITED
            INITIALIZATIONTOK,// INITIALIZATION
            INTOK,            // IN
            INTERFACETOK,     // INTERFACE
            ISTOK,            // IS
          // marker for keywords starting with j
          // marker for keywords starting with k
          // marker for keywords starting with l       
            LABELTOK,         // LABEL
            LIBRARYTOK,	      // library
          // marker for keywords starting with m
            MODTOK,           // MOD
            MODULETOK,        // MODULE  (EP)
          // marker for keywords starting with n    
            NILTOK,           // NIL
            NOTTOK,           // NOT
          // marker for keywords starting with o   
            OBJECTTOK,        // OBJECT
            OFTOK,            // OF
            OPERATORTOK,      // OPERATOR
            ORTOK,            // OR
            OR_ELSETOK,       // OR_ELSE
            OTHERWISETOK,     // OTHERWISE  (EP,FP)
          // marker for keywords starting with p      
            PACKEDTOK,        // PACKED
            PROCEDURETOK,     // PROCEDURE
            PROGRAMTOK,       // PROGRAM
            PROPERTYTOK,      // PROPERTY
          // marker for keywords starting with q
          // marker for keywords starting with r  
            RAISETOK,         // RAISE
            RECORDTOK,        // RECORD
            REMTOK,           // REM (VSI)
            REPEATTOK,        // REPEAT
            RESOURCESTRINGTOK,  // RESOURCESTRING
            RETURNTOK,        // RETURN   (VSI)
         // marker for keywords starting with s  
            SELFTOK,          // SELF
            SETTOK,           // SET
            SHLTOK,           // SHL
            SHRTOK,           // SHR
            STDCALLTOK,       // STDCALL
         // marker for keywords starting with t  
            THENTOK,          // THEN
            THREADVARTOK,     // THREADVAR
            TOTOK,            // TO
            TRYTOK,           // TRY
            TYPETOK,          // TYPE
         // marker for keywords starting with u     
            UNITTOK,          // UNIT
            UNTILTOK,         // UNTIL
            USESTOK,          // USES
         // marker for keywords starting with v
            VARTOK,           // VAR
            VIRTUALTOK,       // VIRTUAL
         // marker for keywords starting with w
            WHILETOK,         // WHILE
            WITHTOK,          // WITH
         // marker for keywords starting with x
            XORTOK,           // XOR
         // marker for keywords starting with Y
         // marker for keywords starting with Z
            lastkW,           // used as sentinel

            IDENTTOK,         // Used to indicate ordinary identifier
            NONKEYWORDTOK,    // Protected identifier (Potential keyword protected by &)  
            MODIFIERTOK,      // Item is modifier
            ENUMTOK,	      // Enumerator
            ENUMVALUETOK,     // Enumerator value

          // these are not reserved words but modifiers; used as keywords
          // in certain contexts
            emptymod,           // no mod
            ABSTRACTMOD,	// ABSTRACT
            ALIASMOD,		// ALIAS
            ASSEMBLERMOD,	// ASSEMBLER
            CDECLMOD,		// CDECL
            CPPDECLMOD,		// CPPDECL
            CVARMOD,		// CVAR
            DEFAULTMOD,		// DEFAULT
            DEPRECATEDMOD,	// DEPRECATED
            DYNAMICMOD,		// DYNAMIC
            ENUMERATORMOD,	// ENUMERATOR
            EXPERIMENTALMOD,	// EXPERIMENTAL
            EXPORTMOD,		// EXPORT
            EXTERNALMOD,	// EXTERNAL
            FARMOD,		// FAR
            FAR16MOD,		// FAR16
            FORWARDMOD,		// FORWARD
            GENERICMOD,		// GENERIC
            HELPERMOD,		// HELPER
            IMPLEMENTSMOD,	// IMPLEMENTS
            INDEXMOD,		// INDEX
            INTERRUPTMOD,	// INTERRUPT
            IOCHECKMOD,		// IOCHECK
            LOCALMOD,		// LOCAL
            MESSAGEMOD,		// MESSAGE
            NAMEMOD,		// NAME
            NEARMOD,		// NEAR
            NODEFAULTMOD,	// NODEFAULT
            NORETURNMOD,	// NORETURN
            NOSTACKFRAMEMOD,	// NOSTACKFRAME
            OLDFPCCALLMOD,	// OLDFPCCALL
            OVERLOADMOD,	// OVERLOAD
            OVERRIDEMOD,	// OVERRIDE
            PASCALMOD,		// PASCAL
            PLATFORMMOD,	// PLATFORM
            PRIVATEMOD,		// PRIVATE
            PROTECTEDMOD,	// PROTECTED
            PUBLICMOD,		// PUBLIC
            PUBLISHEDMOD,	// PUBLISHED
            READMOD,		// READ
            REGISTERMOD,	// REGISTER
            REINTRODUCEMOD,	// REINTRODUCE
            RESULTMOD,		// RESULT
            SAFECALLMOD,	// SAFECALL
            SAVEREGISTERSMOD,	// SAVEREGISTERS
            SOFTFLOATMOD,	// SOFTFLOAT
            SPECIALIZEMOD,	// SPECIALIZE
            STATICMOD,		// STATIC
            STDCALLMOD,		// STDCALL
            STOREDMOD,		// STORED
            STRICTMOD,		// STRICT
            UNALIGNEDMOD,	// UNALIGNED
            UNIMPLEMENTEDMOD,	// UNIMPLEMENTED
            VARARGSMOD,		// VARARGS
            VIRTUALMOD,		// VIRTUAL
            WINAPIMOD,		// WINAPI
            lastmod,            // also no mod
            lasttok             // Table setinel
          );

{ These are not keywords

            CONSTPTOK,        // Procedural CONST signature item
            VARPTOK,          // var in procedural signature
            VALUETOK,         // value procedural signature
            STRINGTOK,        // STRING  }

          KeywordTypeList =(  // keyword and modifier chatacteristics
            NoActDec,           //< words having no action for
                                //< our purposes: array, to, of, in, etc
                                //< most keywords will have this setting
            ElemDec,            //< const, type, or var section, is
                                //< released by:
                                //<  implementation when interface ends
                                //<  procedure, function in open code
                                //<  begin in proc/func
            ClearElemDec,       //< This keyword clears ElemDec flag

           // These all start a block when found ; closed by
            UnitLibProgDec,     //< unit, library, or program header
            InterDec,           //< interface declaration
            impDec,             //< implementation declaration
            pfdec,              //< procedure or funcrion
            BlockDec,           //< any block
            CaseDec,            //< Case stmt
            structdec,          //< Record, Object or class
            nxstmtDec,          //< has next statement effect: probably
                                //< "do" as used in for, while, with, etc.
            closureDec,         //< This keyword closes some block or structure
            usesDec,            //< uses clause
            // Modifiers
            genMod,             //< general modifier
            prepfmod,           //< modifier used before procedural header
                                //< proc, func, property or method
            postpfmod,          //< modifier used after procedural
            varmod,             //< only in VAR declaration
            classmod,           //< only in a CLASS
            objectmod,          //< only in an OBJECY
            isforwardmod,       //< "FORWARD"
            isexternalmod       //< "EXTERNAL"
       );


       StateCond =(
              NoState,         // Not in a state
              inComment,       // start of comment
              inNumber,        // start of number
              inHexNum,        // hexadecimal number
              inProgram,       // program
              inUnit,          // unit found
              inLibrary,       // library
              inUses,          // uses stmt
              inInterface,     // Which part (carries to succeeding statements)
              inImplementation,
              inConstDec,         // CONST, TYPE, VAR declaration
              inTypeDec,
              inVarDec,
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
              isSpecific,      // declaring a specific of a generic
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



          TokenRecord = Record
                  Key: String[18];               // string token consists of
                 Kind: Set of KeywordTypeList;   // what it carries
                State: StateCond;                // what ir does
              Closure: TokenType;                // what closes it

          end;





     IdentSize = 1..Identmax;     // 26 letters + _ + 4 spares

const
          ValidIdent: array[IdentSize] of Char = (
                'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L',
                'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X',
                'Y', 'Z', '_', (* and 4 spares *) '_','_','_','_');

     NameMax = 10;  // max number of folders or extensions
     {$IFDEF windows}
     SlashChar = '\'; // Your system's directory separator,
                      // \ on windows; / on unix, linux, etc
     QuoteChar = '"'; // what commands or file names containing
                      // spaces are quotrd with
     {$ELSE}
     SlashChar = '/'; // Your system's directory separator,
                      // \ on windows; / on unix, linux, etc
     QuoteChar = ''''; // what commands or file names containing
                      // spaces are quoted with
     {$ENDIF}

     // Size of largest variant in Item in 32/64-bit words
     {$IFDEF Bits32}
            DummyMax =24;
     {$ELSE}
            DummyMax =12;
     {$ENDIF}

     DigitZero = ORD('0');



TYPE
     InPtr = ^InputBuffer;
     InputBuffer = Record
            Next: InPtr;                 //< prior buffer

            InIndex,                     //< Index in string
            Len,                         //< Length of string

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
            F: Text;                     //< File being read
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

     NameRange = 0..NameMax; // max. folders or exyensions

      // ** Symbol Table items **

     LineP       = ^LineType;
     ItemP       = ^Item;                      // symbol table item

     PROCCALLTY  = ^PROCCALL;
     PROCSTRUCTY = ^PROCSTRUC;

     LineType = PACKED Record                    // DESCRIPTION OF LINE NUMBERS
        LINENum : byte;                            // Line number
        PAGENum : integer;                         // Page number
        CONTLINK : LINEP                           // Nrxt linr number record
     End;

    { PROCCALL }

    PROCCALL = PACKED Record           //< DESCRIPTION OF PROCEDURE/FUNCTION
                                       //< CALL BY AND CALL TO
        PROCNAME : ItemP;                     //< POINTER TO THE CORRESPONDING
                                              //< IDENTIFIER RECORD
        PrevCallBy,                           //< Ptr to prior calling proc
        NextCallBy,                           //< Ptr to next calling proc
        PREVCALL,                             //< Ptr to prior called proc
        NEXTCALL : PROCCALLTY;                //< Ptr to next called p-roc
        FIRST,                                //< LINE NUMBER RECORD FOR
                                              //< THE FIRST CALL
        LAST : LineP                          //> LINE NUMBEER RCORD FOR
                                              //< THE LAST CALL
    End;

    ObjectCall = PACKED Record
        ObjectName: ItemP;

    end;

    PROCSTRUC = PACKED Record           //< DESCRIPTION OF PROCEDURE NESTING
      PROCNAME : ItemP;                    //< POINTER TO THE
                                           //< ASSOCIATED IDENTIFIER
      PREVPROC,                            //< Ptrs to prior nested proc
      NEXTPROC : PROCSTRUCTY;              //< POINTER TO THE NESTED PROCEDURE
                                           //< DECLARED NEXT
      PROCLEVEL,                           //< NESTING DEPTH OF PROCEDURE
      DeclLine,                            //< PROCEDURE DECLARATION LINE NUMBER
                                           //< (Interface or forward)
      DefLine,                             //< PROCEDURE DEFINITION LINE NUMBER
                                           //< (Place where actual header is)
      BeginLine,                           //< Line No. of first BEGIN
      EndLine: byte;                       //< Line No. of last END
      DeclPage,                            //< PROCEDURE DECLARATION PAGE NUMBER
      DefPage,                             //< PROCEDURE DEFINITION PAGE NUMBER 
      BeginPage,                           //< Page No. of first BEGIN
      EndPage: Integer;                    //< Page No. of last END
    End;


// base types

// this only concerns identifier vslues;
// procedutal types are encapsulated elsewhere
Datatype= (notype,     //< not a base type     This must be first
           enumGroup,  //< name of enumerator list
           EnumType,   //< enumerated value,
           boolType,   //< boolean
       ShortIntType,   //< signed 8-bit int
           bytetype,   //< 8-bit unsignrd integer
           charType,   //< 8-bit character
       SmallintType,   //< 16-bit signed integer
           wordType,   //< 16-bit unsigned integer
          wcharType,   //< 16-bit char
       CardinalType,   //< 32-bit unsigned
           LongType,   //< 32-bit unsigned integer
            IntType,   //< Integer (compiler dependent)
           realType,   //< real
         singleType,   //< single
         DoubleType,   //< double precision real
          int64Type,   //< int64
        ExtendedType,  //< 80-bit real
           pointerType,//< address
           setType,    //< set
           ArrayType,  //< general array
           stringType, //< array of char with length
           AnsStrType, //< Ansistring
           uniStrType, //< unicodeString
           RecordType, //< record
           objectType, //< object
            ClassType, //< class
         TemplateType, //< template
            FileType); //< file        This must be last

        UnitP = ^UnitRecord;
        useP= ^UserUsed;     // list of user and used

        UserUsed = record   // list of units
                       this: unitp;  // this unit's pointer
                       inInterface,
                       inImplementation: Boolean;
                       next: UseP;   // next usage pointer
                   end;
        UnitRecord = record
                     Name,                 // its name
                     FileName: ansistring; // file used
                     isDeclared,           // someone refeenced it
                     isfinished: boolean;  // we've read it through

                     // I probably don't neef these two,
                     // nuy I'll hold onto them for now
                     FIRST ,               //< Pointer to first line number record
                     LAST  : LineP;        //< Pointer to last line number record
                     Prev,                 //< Pointer to bottom of alphabetical list of units
                     Next:  Unitp;         //< Pointer to top of alphabetical list of units
                     usedby,               //< pointer to list of units
                                           //< this unit is used by
                     UserOf: UserUsed;     //< pointer to list of units
                                           //< this unit uses
                     GlobalTable,          //< interface items
                     LocalTable:ItemP;     // implementation items
                     end;



        ConstVal = (strg, int, reel, bool);
        ConstP =^ConstantTable;
        ConstantTable = Record
                     Next: ConstP;
                     case val: Constval of
                       Strg: (S: ^UnicodeString);
                       int:  (I: Int64);
                       Reel: (D: Double);
                       Bool: (B: boolean)
        end;


                   // info bits
        MajorKind= ( NothingAtAll,         //< default, unassigned
                     inVisible,            //< not visible in cross-reference
                                           //< used for placeholder or
                                           //< "dummy" items
                     BaseType,             //< base type: char, integer etc.
                     refonly,              //< list only if requested to
                                           //< show usage of standard items
                                           //< used (for predefined items)
                     stdkind,              //< standard item
                                           //< as opposed to a const/type/var
                     predefined,           //< stystem or user predefined
                                           //< has no definition location
                     compdef,      //< predefined by compiler
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
                     isProperty,            //< Property
                     inVar,                 //< marked IN
                     outVar,                //< marked OUT
                     readVar,               //< marjed READ
                     WriteVar,              //< marjed WRITE
                     isPrivate,             //< explicit private
                     isPublic,              //< explicit public   
                     isLibrary,              //< if a unit is marked as library
                     isProgram,             //< if the main program
                     isUnit);              //< if a unit

         IdentType = (UnusedType,           //< For initializing the variant
                      proceduralType,       //< proc, func, method, prop, cons, destr
                      SignatureType,        //< argument of procedural type
                      ElementType,          //< type, var
                      ConstType,            //< constant
                      UnitType,             //< unit, library, or program
                      PredefUnit,           //< predefined unit
                      StructureType,        //< structure: template, class,
                                            //< object, record etc.
                      PredefinedType);      //< predefined by system
                                            //< or other units

         signatureP = ^Signature;           //< proc/func arge

         // We don't necessarily care about the names of the
         // arguments in a proc/func call; what we want is. since
         // ptocedurals can be polymotphic (different proc/func having
         // the same name but with different arguments), what we want is
         // where there are multile ptocedutal items where the routine
         // has the same name, and the type of the arhuments are
         // identical, not necessarily the names of the arguments/
         // Which brings up a question; if there ate two procs, same
         // mame but all default items, which is used?
         Signature= record
                 BaseType: ItemP;           //< type of this arg
                     Next: SignatureP;      //< next arg
         end;

         // used for WITH statements
         WithP = ^WithRec;
         WithRec = record
               ThisWith: ItemP;
               Prev: WithP;
         end;

         StateP = ^State;
         State  = Record            //< Our running state
                Cond: StateCond;    //< condition
                closer: string;     //< if in block, symbol we watch for
                                    //< to close it
                BlockCount:integer; //< If in a block, how deep.
                Withlist: WithP;    //< when in WITH, which one
                WithCount:integer;  //< (or multiple With statements) number
                prev: StateP;       // if we are in a block, pointer to
                                    // state to return to
         end;

         // Size of largest variant in Item record in 32/64-bit words
         DummySize = 1 .. DummyMax;

         Item = record                  //< identifier record  "Symbol Table"
                      Abbrev,               //< optional Short name
                      NameUC,               //< Upper case copy of Name
                      Name: ansistring;     //< its name as given
                      Index,                //< Identifier Serial Number
                      Count:Integer;        //< Usage count

                      UnitIn: UnitP;        //< Unit it's in or its own unit record
                      Owner,                //< Immediate enclosute of this
                               //< identifier, either (in ascending order):
                               //< record, class, or object;
                               //< proc or func nested inside anotherl
                               //< unit, library or main progtam (these
                               //< last ones have no owner, or unit
                               //< owner is main)

                      // these cover idents in owner: idents in unit, in proc;
                      // firlds in record; proc, prop, method, func in
                      // class or object

                      PrevInPlace,          //< Prior item this enclosure
                      // (Unit, Procedural, Object, ot Class )
                      NextInPlace,          //< Next item this enclosure
                      PrevTotal,            //< Prior item in global symbol list
                      NextTotal: ItemP;     //< Next item in global symbol list
                      FIRST ,               //< Pointer to first
                                            //< line number record
                      LAST  : LineP;        //< Pointer to last
                                            //< line number record
                      DT: Datatype;         //< what it uses
                      Kind: set of Majorkind;      //< statistics

                  // Variant part
                   case What: identType of  //< what it is

               proceduralType:              //< proc, func, method, property
                        ( SigCount: integer;       //< number of signatures
                     SignatureList: SignatureP;    //< signature items
                            CALLED,                //< POINTER TO THE FIRST
                                                   //< PROCEDURE CALLED BY IT
                          CALLEDBY: PROCCALLTY;    //< POINTER TO FIRST
                                                   //< CALLING PROCEDURE
                         ProcFirst,                //< Pointer to first line
                                                   //< number record
                          ProcLast: LineP;         //< Pointer to last line
                                                   //< number record
                        LocalTable,                //< List of local vars
                        ResultType: ItemP);        //< type if function or nil
                                                   //< if proc

                SignatureType:              // procedure/function arguments
                   ( NextSignature: SignatureP;);  //< next arg of p/f

                  ElementType:        //< elements: const, type. var
                       ( NextField: ItemP);        //< if a field in record
                                                   //< class, or object,
                                                   //< next one; our "owner"
                                                   //< points to record

                    ConstType:        //< To use constants in conditional
                            ( Valu: ConstP);       //< compilation, have to
                                                   //< save value while in scope

                StructureType:        //< structure: template, obj, class, rec
                          ( Child: ItemP);         //< Structure it encapsulates
                                                   //< this is pointed back thru
                                                   //< "owner" field

                   PredefUnit,        //< predefined or tegular unit details
                     UnitType:
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
                        PrevInUnit,         //< prior itrm in unit
                        NextinUnit,         //< next item in unit
                        ParentUnit,         //< if we are a dotted unit
                            { Don't need these; use "owner" field
                             SiblingUnit,   //< If regular unit, nil; if a
                                            //< child unit, next one on chain }
                         ChildUnit: ItemP);
                        UnusedType:         // Take at least as much room
                                            // as the largest other
                                            // variant to INIT can zero it
{$IFDEF Bits32}
                            (  Dummy: Array[DummySize] of integer; );
{$ELSE}
                            (  Dummy: Array[DummySize] of int64; );
{$ENDIF}
         end;  // Item record

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


 CONST
      DataTypeNames: Array [notype..FileType] of String = (
                   'unspecified type ',
                   'enumerator ',
                   'enumerated value ',
                   'boolean ',
                   'shortint (signed 8-bit int) ',
                   'byte (unsigned 8-bit int) ',
                   'char ',
                   'smallint (16-bit signed int) ',
                   'word (16-bit unsigned int) ',
                   'widechar (16-bit char) ',
                   'cardinal (32-bit unsigned int) ',
                   'long (32-bit signed int) ',
                   'Integer ',
                   'real ',
                   'single ',
                   'double ',
                   'int64 ',
                   'extended (80-bit real) ',
                   'pointer ',
                   'set ',
                   'array ',
                   'string',
                   'ansistring ',
                   'unicodestring ',
                   'record ',
                   'object ',
                   'class ',
                   'template ',
                   'file ');

      MajorKindNames: array[NothingAtAll .. isUnit] of string =(
              'undefined ',
              'invisible ',
              'base type ',             //< base type: char, integer etc.
              'reference only ',            //< used (for predefined items)
              'standard ',                //< standard procedure/funcrion
              'predefined ',                                           //< has no definition location
              'compiler defined ',
              'in interface ',
              'in proc/func ',              //< identifier declared within p/f
              'case selector ',
              'proc/func argument ',        //< args of proc/func
              'temporary ',            //< temporary item for search
              'declared before def ',
              'defined ',
              'procedure ',
              'fuction ',
              'const ',
              'type ',
              'var ',
              'value ',
              'field ',
              'constructor ',
              'destructor ',
              'property ',
              'in var ',                 //< marked IN
              'out var ',
              'read var ',
              'write var ',
              'private ',
              'public ',
              'library ',
              'program ',
              'unit'
              );


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

     // what we are doing - this is also a push-down list
     StateTable: StateP;


     Tokens: array [EmptyTok .. LastTok] of TokenRecord = (
        ( Key: '';		Kind: [NoActDec];  State: NoState),		// Sentinel for start of list
        ( Key: '@';		Kind: [NoActDec];  State: NoState),		// Address
        ( Key: '&';		Kind: [NoActDec];  State: NoState),		// Ampersand
        ( Key: '{';		Kind: [NoActDec];  State: inComment),		// Brace Comment
        ( Key: '}';		Kind: [NoActDec];  State: NoState),		// closure
        ( Key: '';		Kind: [NoActDec];  State: inComment),		// Optional Comment 1
        ( Key: '';		Kind: [NoActDec];  State: NoState),		// closure
        ( Key: '';		Kind: [NoActDec];  State: inComment),		// Optional Comment 2
        ( Key: '';		Kind: [NoActDec];  State: NoState),		// closure             
        ( Key: '';		Kind: [NoActDec];  State: inComment),		// Optional Comment 3
        ( Key: '';		Kind: [NoActDec];  State: NoState),		// closure
        ( Key: '';		Kind: [NoActDec];  State: inComment),		// Optional Comment 4
        ( Key: '';		Kind: [NoActDec];  State: NoState),		// closure
        ( Key: '';		Kind: [NoActDec];  State: inComment),		// Optional Comment 5
        ( Key: '';		Kind: [NoActDec];  State: NoState),		// closure
        ( Key: '(*';		Kind: [NoActDec];  State: inComment),		// Paren Star Comment
        ( Key: '*)';		Kind: [NoActDec];  State: NoState),		// Comment End
        ( Key: ']';		Kind: [NoActDec];  State: NoState),		// Close Bracket
        ( Key: '.)';		Kind: [NoActDec];  State: NoState),		// Dot Bracket (Old School [ )
        ( Key: '(.';		Kind: [NoActDec];  State: NoState),		// Dor Bracket End (Old School ] )
        ( Key: '..';		Kind: [NoActDec];  State: NoState),		// Range
        ( Key: ':=';		Kind: [NoActDec];  State: NoState),		// Becomes
        ( Key: '<=';		Kind: [NoActDec];  State: NoState),		// LE
        ( Key: '<>';		Kind: [NoActDec];  State: NoState),		// NE
        ( Key: '>=';		Kind: [NoActDec];  State: NoState),		// GE
        ( Key: ':';		Kind: [NoActDec];  State: NoState),		// Colon
        ( Key: ',';		Kind: [NoActDec];  State: NoState),		// Comma
        ( Key: ')';		Kind: [NoActDec];  State: NoState),		// Close Paren
        ( Key: '//';		Kind: [NoActDec];  State: inComment),		// Double Slash
        ( Key: '^';		Kind: [NoActDec];  State: NoState),		// Dereference
        ( Key: '/';		Kind: [NoActDec];  State: NoState),		// Div
        ( Key: '$';		Kind: [NoActDec];  State: inHexNum),		// Dollar
        ( Key: '=';		Kind: [NoActDec];  State: NoState),		// EQ
        ( Key: '>';		Kind: [NoActDec];  State: NoState),		// GT
        ( Key: '#';		Kind: [NoActDec];  State: inNumber),		// Hash
        ( Key: '<';		Kind: [NoActDec];  State: NoState),		// LT
        ( Key: '-';		Kind: [NoActDec];  State: NoState),		// Minus
        ( Key: '*';		Kind: [NoActDec];  State: NoState),		// Mul
        ( Key: '[';		Kind: [NoActDec];  State: NoState),		// Open Brace
        ( Key: '(';		Kind: [NoActDec];  State: NoState),		// Open Paren
        ( Key: '.';		Kind: [NoActDec];  State: NoState),		// Period
        ( Key: '+';		Kind: [NoActDec];  State: NoState),		// Plus
        ( Key: '''';		Kind: [NoActDec];  State: NoState),		// Quote
        ( Key: ';';		Kind: [NoActDec];  State: NoState),  		// Semicolon
        ( Key: '0';		Kind: [NoActDec];  State: inNumber),  		// 0
        ( Key: '1';		Kind: [NoActDec];  State: inNumber),  		// 1
        ( Key: '2';		Kind: [NoActDec];  State: inNumber),  		// 2
        ( Key: '3';		Kind: [NoActDec];  State: inNumber),  		// 3
        ( Key: '4';		Kind: [NoActDec];  State: inNumber),  		// 4
        ( Key: '5';		Kind: [NoActDec];  State: inNumber),  		// 5
        ( Key: '6';		Kind: [NoActDec];  State: inNumber),  		// 6
        ( Key: '7';		Kind: [NoActDec];  State: inNumber),  		// 7
        ( Key: '8';		Kind: [NoActDec];  State: inNumber),  		// 8
        ( Key: '9';		Kind: [NoActDec];  State: inNumber),             // 9
        // Semicolon
        ( Key: '';		Kind: [NoActDec];  State: NoState),		// end of stmbols / Start of conditionals we care about

                                                                                 NoState,         // Not in a state
             ,       // program
             ,          // unit found
              ,       // library
              ,          // uses stmt
              ,     // Which part (carries to succeeding statements)
              ,
              ,         // CONST, TYPE, VAR declaration
                            ,
              ,       // FORWARD
              ,      // EXTERNAL
              ,     // Proc, Func, etc. (carries forward
              ,      //                   until closed)
              ,
              ,
              ,
              ,        // block types
              ,
              ,
              ,       // defining a generic (template)
              ,      // declaring a specific of a generic
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



        ( Key: '$DEFINE';	Kind: [NoActDec];  State: NoState),		// $DEFINE
        ( Key: '$ELSE';		Kind: [NoActDec];  State: NoState),		// $ELSE
        ( Key: '$ELSEIF';	Kind: [NoActDec];  State: NoState),		// $ELSEIFf Conditionals
        ( Key: '$ENDIF';	Kind: [NoActDec];  State: NoState),		// $ENDIF
        ( Key: '$I';		Kind: [NoActDec];  State: NoState),              // $I[NCLUDE]
        ( Key: '$IF';		Kind: [NoActDec];  State: NoState),		// $IF
        ( Key: '$IFDEF';	Kind: [NoActDec];  State: NoState),		// $IFDEF
        ( Key: '$IFNDEF';	Kind: [NoActDec];  State: NoState),		// $IFNDEF
        ( Key: '$INCLUDE';	Kind: [NoActDec];  State: NoState),             // $INCLUDE
        ( Key: '$INCLUDEPATH';	Kind: [NoActDec];  State: NoState),             // $INCLUDEPATH
        ( Key: '$UNDEF';	Kind: [NoActDec];  State: NoState),		// $UNDEF
        ( Key: '';		Kind: [NoActDec];  State: NoState),		// End of Conditionals / start of krywotds
	        // A
        ( Key: 'ABSOLUTE';	Kind: [NoActDec];  State: NoState),	// Absolute
        ( Key: 'AND';		Kind: [NoActDec];  State: NoState),		// And
        ( Key: 'AND_THEN';	Kind: [NoActDec];  State: NoState),             // and_then  (Extended Pascal)
        ( Key: 'ARRAY';		Kind: [NoActDec];  State: NoState),		// Array
        ( Key: 'AS';		Kind: [NoActDec];  State: NoState),              // As
        ( Key: 'ASM';		Kind: [NoActDec];  State: NoState),		// Asm
	        // B
        ( Key: 'BEGIN';		Kind: [NoActDec];  State: NoState),		// Begin
        ( Key: 'BITPACKED';	Kind: [NoActDec];  State: NoState),
	        // C
        ( Key: 'CASE';		Kind: [NoActDec];  State: NoState),		// Case
        ( Key: 'CLASS';		Kind: [NoActDec];  State: inClass),		// Class
        ( Key: 'CONSTRUCTOR';	Kind: [NoActDec];  State: inConstructor),	// Constructor
        ( Key: 'CONST';		Kind: [NoActDec];  State: inConstDec),		// Const
	        // D
        ( Key: 'DESTRUCTOR';	Kind: [NoActDec];  State: inDestructor),
        ( Key: 'DISPINTERFACE';	Kind: [NoActDec];  State: NoState),
        ( Key: 'DIV';		Kind: [NoActDec];  State: NoState),		// Div
        ( Key: 'DO';		Kind: [NoActDec];  State: NoState),		// Do
        ( Key: 'DOWNTO';	Kind: [NoActDec];  State: NoState),	        // Downto
	        // E
        ( Key: 'ELSE';		Kind: [NoActDec];  State: NoState),		// Else
        ( Key: 'END';		Kind: [NoActDec];  State: NoState),		// End
        ( Key: 'EXCEPT';	Kind: [NoActDec];  State: NoState),
        ( Key: 'EXPORTS';	Kind: [NoActDec];  State: NoState),		// Exports
        ( Key: 'EXTERNAL';	Kind: [NoActDec];  State: inExternal),	// External
	        // F
        ( Key: 'FILE';		Kind: [NoActDec];  State: NoState),		// File
        ( Key: 'FINALIZATION';	Kind: [NoActDec];  State: NoState),	// Finalization
        ( Key: 'FINALLY';	Kind: [NoActDec];  State: NoState),		// Finally
        ( Key: 'FOR';		Kind: [NoActDec];  State: NoState),		// For
        ( Key: 'FUNCTION';	Kind: [NoActDec];  State: inFunction),	// Function
	        // G
        ( Key: 'GOTO';		Kind: [NoActDec];  State: NoState),     // Goto
	        // H
	        // I
        ( Key: 'IF';		Kind: [NoActDec];  State: NoState),
        ( Key: 'IMPLEMENTATION';Kind: [NoActDec];  State: inImplementation),
        ( Key: 'INHERITED';	Kind: [NoActDec];  State: NoState),
        ( Key: 'INITIALIZATION';Kind: [NoActDec];  State: NoState),
        ( Key: 'IN';		Kind: [NoActDec];  State: NoState),
        ( Key: 'INTERFACE';	Kind: [NoActDec];  State: inInterface),
        ( Key: 'IS';		Kind: [NoActDec];  State: NoState),
	        // J
	        // K
	        // L
        ( Key: 'LABEL';		Kind: [NoActDec];  State: NoState),
        ( Key: 'LIBRARY';	Kind: [NoActDec];  State: inLibrary),
	        // M
        ( Key: 'MOD';		Kind: [NoActDec];  State: NoState),		// Mod
        ( Key: 'MODULE';	Kind: [NoActDec];  State: NoState),
	        // N
        ( Key: 'NIL';		Kind: [NoActDec];  State: NoState),
        ( Key: 'NOT';		Kind: [NoActDec];  State: NoState),
	        // O
        ( Key: 'OBJECT';	Kind: [NoActDec];  State: inObject),
        ( Key: 'OF';		Kind: [NoActDec];  State: NoState),
        ( Key: 'OPERATOR';	Kind: [NoActDec];  State: NoState),
        ( Key: 'OR';		Kind: [NoActDec];  State: NoState),
        ( Key: 'OR_ELSE';	Kind: [NoActDec];  State: NoState),
        ( Key: 'OTHERWISE';	Kind: [NoActDec];  State: NoState),
	        // P
        ( Key: 'PACKED';	Kind: [NoActDec];  State: NoState),
        ( Key: 'PROCEDURE';	Kind: [NoActDec];  State: inProcedure),
        ( Key: 'PROGRAM';	Kind: [NoActDec];  State: inProgram),
        ( Key: 'PROPERTY';	Kind: [NoActDec];  State: inProperty),
	        // Q
	        // R
        ( Key: 'RAISE';		Kind: [NoActDec];  State: NoState),
        ( Key: 'RECORD';	Kind: [NoActDec];  State: inRecord),
        ( Key: 'REM';		Kind: [NoActDec];  State: NoState),
        ( Key: 'REPEAT';	Kind: [NoActDec];  State: NoState),
        ( Key: 'RESOURCESTRING';Kind: [NoActDec];  State: NoState),
        ( Key: 'RETURN';	Kind: [NoActDec];  State: NoState),
	        // S
        ( Key: 'SELF';		Kind: [NoActDec];  State: NoState),
        ( Key: 'SET';		Kind: [NoActDec];  State: NoState),
        ( Key: 'SHL';		Kind: [NoActDec];  State: NoState),
        ( Key: 'SHR';		Kind: [NoActDec];  State: NoState),
        ( Key: 'STDCALL';	Kind: [NoActDec];  State: NoState),
	        // T
        ( Key: 'THEN';		Kind: [NoActDec];  State: NoState),
        ( Key: 'THREADVAR';	Kind: [NoActDec];  State: NoState),
        ( Key: 'TO';		Kind: [NoActDec];  State: NoState),
        ( Key: 'TRY';		Kind: [NoActDec];  State: NoState),
        ( Key: 'TYPE';		Kind: [NoActDec];  State: inTypeDec),
	        // U
        ( Key: 'UNIT';		Kind: [NoActDec];  State: inUnit),		// Unit
        ( Key: 'UNTIL';		Kind: [NoActDec];  State: NoState),		// Until
        ( Key: 'USES';		Kind: [NoActDec];  State: inUses),		// Uses
	        // V
        ( Key: 'VAR';		Kind: [NoActDec];  State: inVarDec),		// Var
        ( Key: 'VIRTUAL';	Kind: [NoActDec];  State: NoState),
	        // W
        ( Key: 'WHILE';		Kind: [NoActDec];  State: NoState),		// While
        ( Key: 'WITH';		Kind: [NoActDec];  State: NoState),		// With
	        // X
        ( Key: 'XOR';		Kind: [NoActDec];  State: NoState),		// Xor
	        // Y
	        // Z
        ( Key: '';		Kind: [NoActDec];  State: NoState),    //< last keyword token

        ( Key: '*IDENT';	Kind: [NoActDec];  State: NoState),          // Identifier
        // Identifiers that might match keywords
        ( Key: '*NO KEYWORD';	Kind: [NoActDec];  State: NoState),
        ( Key: '*MODIFER';	Kind: [NoActDec];  State: NoState),
        ( Key: '*ENUMERATOR';	Kind: [NoActDec];  State: NoState),
        ( Key: '*ENUM VALUE';	Kind: [NoActDec];  State: NoState),

        // These are all modifiers; they only become keywords if
        // used within the same context applicable to that keyword,
        // e.g. the indentifier "forward" is a perfectly reasonable
        // variable name, and you can use it for any identifier. It
        // only bcomes a keyword if used just after a procedural
        // declaration. Thus I don't have to hide them until activated,
        // as they only become acticated if you use them as a keyword.
        ( Key: '';		Kind: [NoActDec];  State: NoState),
        ( Key:  'ABSTRACT';	Kind: [NoActDec];  State: NoState),
        ( Key:  'ALIAS';	Kind: [NoActDec];  State: NoState),
        ( Key:  'ASSEMBLER';	Kind: [NoActDec];  State: NoState),
        ( Key:  'CDECL';	Kind: [NoActDec];  State: NoState),
        ( Key:  'CPPDECL';	Kind: [NoActDec];  State: NoState),
        ( Key:  'CVAR';		Kind: [NoActDec];  State: NoState),
        ( Key:  'DEFAULT';	Kind: [NoActDec];  State: NoState),
        ( Key:  'DEPRECATED';	Kind: [NoActDec];  State: NoState),
        ( Key:  'DYNAMIC';	Kind: [NoActDec];  State: NoState),
        ( Key:  'ENUMERATOR';	Kind: [NoActDec];  State: NoState),
        ( Key:  'EXPERIMENTAL';	Kind: [NoActDec];  State: NoState),
        ( Key:  'EXPORT';	Kind: [NoActDec];  State: NoState),
        ( Key:  'EXTERNAL';	Kind: [NoActDec];  State: NoState),
        ( Key:  'FAR';		Kind: [NoActDec];  State: NoState),
        ( Key:  'FAR16';	Kind: [NoActDec];  State: NoState),
        ( Key:  'FORWARD';	Kind: [NoActDec];  State: inforward),
        ( Key:  'GENERIC';	Kind: [NoActDec];  State: inGeneric),
        ( Key:  'HELPER';	Kind: [NoActDec];  State: NoState),
        ( Key:  'IMPLEMENTS';	Kind: [NoActDec];  State: NoState),
        ( Key:  'INDEX';	Kind: [NoActDec];  State: NoState),
        ( Key:  'INTERRUPT';	Kind: [NoActDec];  State: NoState),
        ( Key:  'IOCHECK';	Kind: [NoActDec];  State: NoState),
        ( Key:  'LOCAL';	Kind: [NoActDec];  State: NoState),
        ( Key:  'MESSAGE';	Kind: [NoActDec];  State: NoState),
        ( Key:  'NAME';		Kind: [NoActDec];  State: NoState),
        ( Key:  'NEAR';		Kind: [NoActDec];  State: NoState),
        ( Key:  'NODEFAULT';	Kind: [NoActDec];  State: NoState),
        ( Key:  'NORETURN';	Kind: [NoActDec];  State: NoState),
        ( Key:  'NOSTACKFRAME';	Kind: [NoActDec];  State: NoState),
        ( Key:  'OLDFPCCALL';	Kind: [NoActDec];  State: NoState),
        ( Key:  'OVERLOAD';	Kind: [NoActDec];  State: NoState),
        ( Key:  'OVERRIDE';	Kind: [NoActDec];  State: NoState),
        ( Key:  'PASCAL';	Kind: [NoActDec];  State: NoState),
        ( Key:  'PLATFORM';	Kind: [NoActDec];  State: NoState),
        ( Key:  'PRIVATE';	Kind: [NoActDec];  State: NoState),
        ( Key:  'PROTECTED';	Kind: [NoActDec];  State: NoState),
        ( Key:  'PUBLIC';	Kind: [NoActDec];  State: NoState),
        ( Key:  'PUBLISHED';	Kind: [NoActDec];  State: NoState),
        ( Key:  'READ';		Kind: [NoActDec];  State: NoState),
        ( Key:  'REGISTER';	Kind: [NoActDec];  State: NoState),
        ( Key:  'REINTRODUCE';	Kind: [NoActDec];  State: NoState),
        ( Key:  'RESULT';	Kind: [NoActDec];  State: NoState),
        ( Key:  'SAFECALL';	Kind: [NoActDec];  State: NoState),
        ( Key:  'SAVEREGISTERS';Kind: [NoActDec];  State: NoState),
        ( Key:  'SOFTFLOAT';	Kind: [NoActDec];  State: NoState),
        ( Key:  'SPECIALIZE';	Kind: [NoActDec];  State: NoState),
        ( Key:  'STATIC';	Kind: [NoActDec];  State: NoState),
        ( Key:  'STDCALL';	Kind: [NoActDec];  State: NoState),
        ( Key:  'STORED';	Kind: [NoActDec];  State: NoState),
        ( Key:  'STRICT';	Kind: [NoActDec];  State: NoState),
        ( Key:  'UNALIGNED';	Kind: [NoActDec];  State: NoState),
        ( Key:  'UNIMPLEMENTED';Kind: [NoActDec];  State: NoState),
        ( Key:  'VARARGS';	Kind: [NoActDec];  State: NoState),
        ( Key:  'VIRTUAL';	Kind: [NoActDec];  State: NoState),
        ( Key:  'WINAPI';	Kind: [NoActDec];  State: NoState),
        ( Key: '';		Kind: [NoActDec];  State: NoState),         // last modifier
        ( Key: '';		Kind: [NoActDec];  State: NoState)          // Last Token
	          );


     // Bookkeeping
     IdentifierCount: Integer = 0;  // total number of identifiers
     UniqueIdents:    Integer = 0;  // total number of unique identifiers

     // folders and extensions
     FolderTable,
     Extensions: Array [NameRange] of unicodeString;
     TopFolder,
     TopExtension: NameRange;

     Buffer: InPtr = NIL;

     TaskStart,               //> Start Time of a particular task
     RaskEnd,                 //< Used to determine how long it took
     StartTime,               //< time program started
     EndTime: SystemTime;     //< for elapsed time

     // file descriptor for this program
     PasPath,                 //< Path of this program
     PasFolder,               //< Folder program is in
     PasName,                 //< Name of program
     PasExt: UnicodeString;   //< Extension (.EXE on Windows

     // Keyword and behavior switches
     Lang_Extended,        //< Extebded Pascal
     lang_Turbo,           //< Turbo Pascal
     Lang_XD,              //< XDPascal
     Lsng_GNU,             //< GNU Pascal
     lang_Stanford,        //< Stanford Pascal
     Lang_Borland,         //< Borland Pascal
     Lang_object,          //< Object Pascal
     Lang_Delphi,          //< Delphi
     Lang_FreePascal,      //< Ftee Pascal
     Allow_Control_Chars,  //< Allow ^A for Control-A ^B, etc.
     // _ is a valid char in identifrers
     Allow_Underscore: Boolean;


implementation

end.

