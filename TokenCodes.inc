  // Token codes
  TokenCodes = (
  UNTYPETOK		= 0;
// In order by use
  CONSTTOK,   // Const
  TYPETOK		= 2;     // Type
  VARTOK		= 3;     // Var
  PROCEDURETOK		= 4;     // Procedure
  FUNCTIONTOK		= 5;     // Function
  LABELTOK		= 6;	 // Label
  UNITTOK		= 7;	 // Unit
  ENUMTOK		= 8;	 // Enumerator

//  GETINTVECTOK	= 10;
//  SETINTVECTOK	= 11;
  CASETOK		= 12;    // Case
  BEGINTOK		= 13;    // Begin
  ENDTOK		= 14;    // End
  IFTOK			= 15;    // If
  THENTOK		= 16;    // Then
  ELSETOK		= 17;    // Else
  WHILETOK		= 18;    // While
  DOTOK			= 19;    // Do
  REPEATTOK		= 20;    // Repeat
  UNTILTOK		= 21;    // Until
  FORTOK		= 22;    // For
  TOTOK			= 23;    // To
  DOWNTOTOK		= 24;    // Downto
// These are mot reserved words
//  ASSIGNTOK		= 25;    // Assign proc
//  WRITETOK		= 26;    // Write (as proc)
//  READLNTOK		= 27;    // Teadln
//  HALTTOK		= 28;    // Halt
  USESTOK		= 29;    // uses
  ARRAYTOK		= 30;    // array
  OFTOK			= 31;    // of
//  STRINGTOK		= 32;
//  INCTOK		= 33;
//  DECTOK		= 34;
//  ORDTOK		= 35;
//  CHRTOK		= 36;
  ASMTOK		= 37;    // ASM
  ABSOLUTETOK		= 38;    // absolute
//  BREAKTOK		= 39;
//  CONTINUETOK		= 40;
//  EXITTOK		= 41;
  RANGETOK		= 42;    // ..

  EQTOK			= 43;    // =
  NETOK			= 44;    // <>
  LTTOK			= 45;    // <
  LETOK			= 46;    // <=
  GTTOK			= 47;    // >
  GETOK			= 48;    // >=
//  LOTOK			= 49; // LO(
//  HITOK			= 50; // HI(

  DOTTOK		= 51;    // .
  COMMATOK		= 52;    // ,
  SEMICOLONTOK		= 53;    // ;
  OPARTOK		= 54;    // (
  CPARTOK		= 55;    // )
  DEREFERENCETOK	= 56;    // ^
  ADDRESSTOK		= 57;    // @
  OBRACKETTOK		= 58;    // [
  CBRACKETTOK		= 59;    // ]
  COLONTOK		= 60;    // :

  PLUSTOK		= 61;    // +
  MINUSTOK		= 62;    // -
  MULTOK		= 63;    // *
  DIVTOK		= 64;    // /
  IDIVTOK		= 65;    // DIV
  MODTOK		= 66;    // MOD
  SHLTOK		= 67;    // SHL (Shify left)
  SHRTOK		= 68;    // SHR (Shift right)
  ORTOK			= 69;    // OR
  XORTOK		= 70;    // XOR
  ANDTOK		= 71;    // AND
  NOTTOK		= 72;    // NOT

// Defined procedutes/funcs, not keywords
//  ASSIGNFILETOK		= 73; Assign(
//  RESETTOK		= 74; // Reset(
//  REWRITETOK		= 75;
//  APPENDTOK		= 76;
//  BLOCKREADTOK		= 77;
//  BLOCKWRITETOK		= 78;
//  CLOSEFILETOK		= 79;
//  GETRESOURCEHANDLETOK	= 80;

//  WRITELNTOK		= 81;
//  SIZEOFTOK		= 82;
//  LENGTHTOK		= 83;
//  HIGHTOK		= 84;
//  LOWTOK		= 85;
//  INTTOK		= 86;
//  FRACTOK		= 87;
//  TRUNCTOK		= 88;
//  ROUNDTOK		= 89;
//  ODDTOK		= 90;

  PROGRAMTOK		= 91;     // program
  LIBRARYTOK		= 92;     // library

//   LIBRARY		= 92;     // library

{   EXPORTS		= 93;
  EXTERNAL		= 94;
 INTERFACE		= 95;
 IMPLEMENTATION     = 96;
 INITIALIZATION     = 97;
 CONSTRUCTOR	= 98;
 DESTRUCTOR		= 99; }
// modifiers
 OVERLOAD		= 100;
   ASSEMBLER		= 101;
  FORWARD		= 102;
  REGISTER		= 103;
  INTERRUPT		= 104;
  PASCAL		= 105;
  STDCALL		= 106;
  INLINE		= 107;

//   PACKED		= 110;


 VOLATILE		= 113;




  EXPORTSTOK		= 93;  // exports
  EXTERNALTOK		= 94;  // external
  INTERFACETOK		= 95;  // interface
  IMPLEMENTATIONTOK     = 96;  // implementation
  INITIALIZATIONTOK     = 97;  // initialization
  CONSTRUCTORTOK	= 98;  // constructor
  DESTRUCTORTOK		= 99;  // destructot
{
  OVERLOADTOK		= 100; // overload
  ASSEMBLERTOK		= 101;
  FORWARDTOK		= 102;
  REGISTERTOK		= 103;
  INTERRUPTTOK		= 104;
  PASCALTOK		= 105;
  STDCALLTOK		= 106;
  INLINETOK		= 107;

//  SUCCTOK		= 108;
//  PREDTOK		= 109;  }
  PACKEDTOK		= 110;
  GOTOTOK		= 111;
  INTOK			= 112;
//  VOLATILETOK		= 113;


  SETTOK		= 127;	// Size = 32 SET OF

  // this block is referenced by datasize
  BYTETOK		= 128;	// Size = 1 BYTE  Unsignrd Int8
  WORDTOK		= 129;	// Size = 2 WORD  Unsignrd Int15
  CARDINALTOK		= 130;	// Size = 4 CARDINAL Signrd Int32
  SHORTINTTOK		= 131;	// Size = 1 SHORTINT Signrd Int8
  SMALLINTTOK		= 132;	// Size = 2 SMALLINT Signrd Int16
  INTEGERTOK		= 133;	// Size = 4 INTEGER  Unsignrd Int32
  CHARTOK		= 134;	// Size = 1 CHAR
  BOOLEANTOK		= 135;	// Size = 1 BOOLEAN
  POINTERTOK		= 136;	// Size = 4 POINTER
//  STRINGPOINTERTOK	= 137;	// Size = 4 POINTER to STRING
  FILETOK		= 138;	// Size = 4/? FILE
  RECORDTOK		= 139;	// Size = 4/???
  OBJECTTOK		= 140;	// Size = 4/???
  SHORTREALTOK		= 141;	// Size = 2 SHORTREAL			Fixed-Point Q8.8
  REALTOK		= 142;	// Size = 4 REAL			Fixed-Point Q24.8
  SINGLETOK		= 143;	// Size = 4 SINGLE / FLOAT		IEEE-754 32bit
  HALFSINGLETOK		= 144;	// Size = 2 HALFSINGLE / FLOAT16	IEEE-754 16bit
  PCHARTOK		= 145;	// Size = 4 POINTER TO ARRAY OF CHAR
  ENUMTOK		= 146;	// Size = 1 BYTE
  PROCVARTOK		= 147;	// Size = 4
  TEXTFILETOK		= 148;	// Size = 4/?? FILE
  FORWARDTYPE		= 149;	// Size = 4
  // end of datasize block

  SHORTSTRINGTOK	= 150;	// zamieniamy na STRINGTOK
  FLOATTOK		= 151;	// zamieniamy na SINGLETOK
  FLOAT16TOK		= 152;	// zamieniamy na HALFSINGLETOK
  TEXTTOK		= 153;	// zamieniamy na TEXTFILETOK

  DATAORIGINOFFSET	= 160;
  CODEORIGINOFFSET	= 161;

  IDENTTOK		= 180;
  INTNUMBERTOK		= 181;
  FRACNUMBERTOK		= 182;
  CHARLITERALTOK	= 183;
  STRINGLITERALTOK	= 184;

  MACRORELEASE		= 189;
  PROCALIGNTOK		= 190;
  LOOPALIGNTOK		= 191;
  INFOTOK		= 192;
  WARNINGTOK		= 193;
  ERRORTOK		= 194;
  UNITBEGINTOK		= 195;
  UNITENDTOK		= 196;
  IOCHECKON		= 197;
  IOCHECKOFF		= 198;
  EOFTOK		= 199;     // MAXTOKENNAMES = 200


