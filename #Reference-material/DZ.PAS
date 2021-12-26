{ $C-,V+,R+,I+,U-,K+}  {* essential for programmed pause-abort facility;
                                  see procedure dealwithuser *}
program xrefFull ( INPUT , OUTPUT ) ;

Uses Crt, SysUtils;

// XREF.CST
CONST
   Min_Memory     =   3 ; { Lowest acceptable amount of free memory during run }
   ch_per_word    =  34 ;              { characters per word }
   Ref_Per_Line   =   7 ;              { line numbers per printed reference line }
   linenum_size   =   5 ;              { size of displayed line numbers }
   reserved_count = 208 ;              { number of reserved words }
   Lines_Per_Page : INTEGER = 84 ;
   Chars_Per_Line : INTEGER = 136 ;
   diag           : boolean = true ;

   MaxAvail = -1;

// DATETIME.CST
CONST
      MinYear           =  00  ;
      MaxYear           = 3200 ;
      MinAbsDay         = 1.0  ;
      MaxAbsDay         = 803916.0 ; (* 366 + MAXYEAR * 365.25 *)
      MaxWeekDayNumber  = 7    ;
      MaxMonthNumber    = 12   ;






// PRINTER.CST
CONST
   TL_Corner         = #240    ;
   TR_Corner         = #242    ;
   BL_Corner         = #246    ;
   BR_Corner         = #247    ;
   Horz              = #241    ;
   Vert              = #245    ;
   Left_Junc         = #244    ;
   Right_Junc        = #249    ;
   Top_Junc          = #243    ;
   Bot_Junc          = #248    ;
   Cross             = #250    ;
   Form_Feed         = ^L      ;
   Start_Macro       = ^['+'   ; (* START DEFINING MACRO INSTRUCTION 16 CHARS MAX *)
   Exec_Macro        = ^['!'   ; (* EXECUTE MACRO *)
   End_Macro_Ins     = #30     ; (* END DEFINING MACRO *)
   Accept_8th_Bit    = ^['#'   ; (* ACCEPT 8TH BIT AS IS FROM HOST CPU *)
   Copy_Rom_Chars    = ^['*0'  ; (* COPY PRINTER ROM CHARS TO RAM *)
   Pre_Define_Char   = ^['*1'  ; (* DEFINE CHAR IN RAM n1, n2, m1, m2 *)
   Cancel_Down_Load  = ^['$0'  ; (* CANCEL DOWN LOAD SET 96 CHARS MAX *)
   Select_Down_Load  = ^['$1'  ; (* SELECT DOWN LOAD SET 96 CHARS MAX *)
   Start_Under_Line  = ^['-'^A ; (* START UNDER LINING TEXT *)
   Stop_Under_Line   = ^['-'#0 ; (* STOP UNDER LINING TEXT *)
   LPI06             = ^['2'   ; (* SET LINE FEED TO 12/72 OF AN INCH *)
   LPI08             = ^['0'   ; (* SET LINE FEED TO  9/72 OF AN INCH *)
   LPI09             = ^['A'^H ; (* SET LINE FEED TO  8/72 OF AN INCH *)
   Italics_On        = ^['4'   ; (* SELECT ITALIC CHARS IN ROM *)
   Italics_Off       = ^['5'   ; (* SELECT DEFAULT ROMM CHARS *)
   Reset_Printer     = ^['@'   ; (* SOFTWARE RESET PRINTER *)
   CPL80             = ^['B'^A ;
   CPL96             = ^['B'^B ;
   CPL132            = ^['B'^C ;
   Pre_Graf_60       = ^['K'   ; (* START 60 DOTS PER INCH FOR n1, n2  Col = n1 + n2 * 256 *)
   Pre_Graf_120      = ^['L'   ; (* START 120 DOTS PER INCH FOR n1, n2  CHARS *)
   Pre_Graf_120_Fast = ^['y'   ; (* START 120 DOTS PER INCH IN HIGH SPEED *)
   Pre_Graf_240      = ^['z'   ; (* START 240 DOTS PER INCH n1, n2 *)
   Pre_Set_Left      = ^['M'   ; (* SET LEFT HAND n1 CHARS IN *)
   Pre_Set_Skip      = ^['N'   ; (* SET PERF SKIP *)
   Super_On          = ^['S'#0 ; (* SUPER SCRIPT CHARS *)
   Sub_On            = ^['S'^A ; (* SUB SCRIPT CHARS *)
   Super_Sub_Off     = ^['T'   ; (* OFF SUPER OR SUB SCRIPT NORMAL CHARS *)
   Double_Wide_On    = ^['W'^A ; (* SET DOUBLE WIDE CHARS *)
   Double_Wide_Off   = ^['W'#0 ;
   Empha_On          = ^['E'   ;
   Empha_Off         = ^['F'   ;
   Double_Strike_On  = ^['G'   ;
   Double_Strike_Off = ^['H'   ; (* CANCELS DOUBLE STRIKE MODE *)
   No_Perf_Skip      = ^['O'   ;

// STRINGS.TYP
TYPE
   Work_String         = STRING [ 255 ] ;
   String_80           = STRING [  80 ] ;
   File_Str            = STRING [  64 ] ;
   String_40           = STRING [  40 ] ;
   Name_Str            = STRING [  32 ] ;
   String_25           = STRING [  25 ] ;
   Paragraph           = STRING [  16 ] ;
   String_10           = STRING [  10 ] ;
   String_08           = STRING [   8 ] ;
   Six_Byte            = STRING [   6 ] ;
   Four_Byte           = STRING [   4 ] ;
   String_03           = STRING [   3 ] ;
   Two_Byte            = STRING [   2 ] ;

//I REG.TYP
TYPE


    Reg = RECORD CASE INTEGER OF
             1: (AX,BX,CX,DX,BP,SI,DI,DS,ES,FLAGS : INTEGER ) ;
             2: (AL,AH,BL,BH,CL,CH,DL,DH : BYTE ) ;
          END ; (* RECORD *)

// DATETIME.TYP
TYPE

TimePtr = ^TSystemTime;

TSystemTime = record
   Year,
   Month,
   DayOfWeek,
   Day : word;
   Hour,
   Minute,
   Second,
   MilliSecond: word;
end ;


      Time0             = STRING [  11 ] ;
      Year_Number       = MinYear..MaxYear ;
      Month_Number      = 1..MaxMonthNumber ;
      Month_Name        = ARRAY [ Month_Number ] OF Paragraph ;
      Max_Day_Number    = ARRAY [ Month_Number ] OF INTEGER ;
      Hundredths        = 0..99 ;
      Second_Number     = 0..59 ;
      Minute_Number     = 0..59 ;
      Hour_Number       = 0..23 ;
      Week_Day_Number   = 1..7 ;
      Day_Number        = 1..31 ;
      Julian_Number     = 1..366 ;


      Week_Day_Name    = ARRAY [ Week_Day_Number ] OF Paragraph ;
      Date0            = STRING [ 11 ] ;
      Date1            = STRING [  7 ] ;
      Time1            = STRING [  8 ] ;
      Date_Time_Rec    = RECORD
                            Abs_Date : REAL ;
                            Abs_Time : REAL ;
                         END ;


// XREF.TYP
TYPE
   TitleType     = STRING [  10 ] ;
   option_type   = char ;
   switchsettype = set of char;
   state         = (none,symbol,quote,com1,pcom2,com2,pcom2x);
   Output_Type   = ( UserCRT , PRINTER , Odd_Pages , Even_Pages , DISK , NULL ) ;

   Item_Ptr      = ^item;
   item          = record lno: integer;
                       next: Item_Ptr;
                   END ;

   Word_Ptr      = ^word;
   word          = record key: string[ch_per_word];
                       first, last: Item_Ptr;
                       left, right: Word_Ptr;
                   END ;

   Inc_Ptr       = ^Inc_Rec ;
   Inc_Rec       = RECORD
                      Prev          : Inc_Ptr ;
                      Inc_File_Name : File_Str ;
                      Inc_Title     : TitleType ;
                   END ;

   Line_Ptr      = ^Line_Rec ;
   Line_Rec      = RECORD
                      Next : Line_Ptr ;
                      CPL  : INTEGER ;
                      Line : Work_String ;
                   END ; (* Line Record *)

   Page_Ptr      = ^Page_Rec ;
   Page_Rec      = RECORD
                      Prev       : Page_Ptr ;
                      Next       : Page_Ptr ;
                      Out_Device : Output_Type ;
                      Max_Cpl    : INTEGER ;
                      Cur_LPP    : INTEGER ;
                      First_Line : Line_Ptr ;
                      Last_Line  : Line_Ptr ;
                   END ;

// DATETIME.VAR
CONST
      WeekDayNames     : Week_Day_Name = ('SUNDAY','MONDAY','TUESDAY','WEDENSDAY','THURSDAY','FRIDAY','SATURDAY');
      MonthNames       : Month_Name = ('JANUARY','FEBUARY','MARCH','APRILL','MAY','JUNE',
                                      'JULY','AUGUST','SEPTEMBER','OCTOBER','NOVEMBER','DECEMBER');
      MaxDaysPerMonth  : Max_Day_Number = ( 31 , 28 , 31 , 30 , 31 , 30 , 31 , 31 , 30 , 31 , 30 , 31 ) ;

VAR
       Cur_Date_Time   : Date_Time_Rec ;
       CurHundredth    : Hundredths ;
       CurSecond       : Second_Number ;
       CurMinute       : Minute_Number ;
       CurHour         : Hour_Number ;
       CurDayNum       : day_Number ;
       CurWeekDayNum   : Week_Day_Number ;
       CurWeekDayNam   : Paragraph ;
       CurMonthNum     : month_number ;
       CurMonthNam     : Paragraph ;
       CurYearNum      : Year_Number ;
       CurJulianNum    : Julian_Number ;
       CurAbsDayNum    : REAL ;
       CurDate0        : Date0 ;

       TimeDatee: TSystemTime;


// XREF.VAR
VAR
   taken_careof,
   parsing_for_dollars,
   itsa_directive,
   itsan_include         : BOOLEAN ;
   Cur_Drive,
   f, Lastf, Switch      : CHAR ;
   Page_Numb, Line_Numb  : INTEGER ;
   Output_Device         : Output_Type ;
   switches              : switchsettype ;
   scan, tscan           : state ;
   title                 : titletype ;
   File_Nam              : String_08 ;
   File_Ext              : String_03 ;
   Main_Path,
   Cur_Path,
   Main_File_Name,
   File_Name, incname,
   outname ,
   Out_Even_Name ,
   File_Path             : File_Str ;
   Out_Odd_Name          : File_Str ;
   id                    : string[127] ;
   Inc_Root , Inc_Last   : Inc_Ptr ;
   root                  : Word_Ptr ;
   Line                  : Line_Ptr ;
   First_Page            : Page_Ptr ;
   Cur_Page              : Page_Ptr ;
   Last_Page             : Page_Ptr ;

   fv,iv,outf,OddF,EveF,
   Tem_File              : TEXT ;

   CON,

   LST:Text;  //(*)

   procedure GetLocalTime(var SystemTime: TSYSTEMTIME); external 'kernel32.dll' name 'GetLocalTime';
   function  GetCommandLineA: Pointer stdcall; external 'KERNEL32.DLL';
   Function GetFileTime( hFile:Integer; var CreationTime,LastAccessTime,
                         LastWriteTime: TimePtr): boolean;  external 'KERNEL32.DLL';

(********************************* level 000 *****************************)
//   BCDTOINT.FUN
(********************************************************************)
(* THIS FUNCTION WILL A FOUR DIGIT BCD INTEGER AND CONVERT IT TO A  *)
(* NORMAL INTEGER                                                   *)
(* INPUT        : BCD INTEGER                                       *)
(* INPUT/OUTPUT : NONE                                              *)
(* OUTPUT       : NORMAL INTEGER                                    *)
(* USES         : NO SIDE EFFECTS                                   *)
(* CALLS        : NONE                                              *)
(* ERRORS       : NONE                                              *)
(* GLOBAL VARS  : NONE                                              *)
(* SUBJECT      : BCD, CONVERT TO INTEGER                           *)
(* DOMAIN       : ARNO A. KARNER                      LEVEL : 000   *)
(********************************************************************)

FUNCTION  BcdToInteger              ( Bcd : INTEGER ) : INTEGER ;

BEGIN (* FUNCTION *)
   BcdToInteger := ( ( Bcd SHR 12 ) * 1000 +
                   ( ( Bcd AND $F00 ) SHR 8 ) * 100 +
                   ( ( BCD AND $F0 ) SHR 4 ) * 10 +
                   ( Bcd AND $F ) ) ;
END ; (* FUNCTION *)

//   EXIST.FUN
(********************************************************************)
(* THIS FUNCTION WILL DETERMIN IF THE GIVEN FILE EXISTS             *)
(* INPUT        : FILE-SPEC + FILE-NAME + FILE-EXTENSION            *)
(* INPUT/OUTPUT : NONE                                              *)
(* OUTPUT       : BOOLEAN FILE EXISTS IF TRUE                       *)
(* USES         : NO SIDE EFFECTS                                   *)
(* CALLS        : ASSIGN , IOresult                                 *)
(* ERRORS       : NONE                                              *)
(* GLOBAL VARS  : NONE                                              *)
(* SUBJECT      : FILES, DOES IT EXIST YES OR NO                    *)
(* DOMAIN       : ARNO A. KARNER\PUBLIC               LEVEL : 000   *)
(********************************************************************)

FUNCTION Exist                                ( FileSpec : String_80 )
                                              : BOOLEAN ;
   VAR
      Fil : TEXT ;
      i:iNTEGER;

   BEGIN
      IF IOresult <> 0 THEN WRITELN (' IO RESULT HOSED ALREADY ' ) ;
      ASSIGN ( Fil , FileSpec ) ;
{$I-}
      RESET ( Fil )
{$I+} ;
      i := iorESULT;
      IF ( I  = 0 ) THEN
         BEGIN
            Exist := TRUE ;
            CLOSE (Fil) ;
          END
       ELSE
           BEGIN
               Exist := FALSE ;
               WRITELN('fILE ERROR ',i)
            END;
   END ;

//   MEMORY.FUN
(********************************************************************)
(* THIS FUNCTION WILL RETURN THE VALUE OF FREE MEMORY IN K BYTES    *)
(* INPUT        : NONE                                              *)
(* INPUT/OUTPUT : NONE                                              *)
(* OUTPUT       : INTEGER : FREE MEMORY IN K BYTES                  *)
(* USES         : NO SIDE EFFECTS                                   *)
(* CALLS        : MAXAVIL                                           *)
(* ERRORS       : NONE                                              *)
(* GLOBAL VARS  : NONE                                              *)
(* SUBJECT      : MEMORY, FREE K BYTES                              *)
(* DOMAIN       : ARNO A. KARNER\PUBLIC               LEVEL : 000   *)
(********************************************************************)

FUNCTION Memory : INTEGER ;

VAR
   Memspace : real;

BEGIN (* FUNC *)
   memspace := maxavail;
   IF memspace < 0 then memspace := 65536.0 + memspace;
   memory := TRUNC ( ( memspace * 16.0 ) / 1024.0 ) ;
END ; (* FUNC *)


// VALDATEN.FUN
(********************************************************************)
(* THIS FUNCTION WILL CHECK THE INPUT DATE NUMBERS TO SEE IF VAILD  *)
(* INPUT        : MONTH : MonthNumber , DAY : DayNumber ,           *)
(*              : YEAR : Year_Number                                *)
(* INPUT/OUTPUT : NONE                                              *)
(* OUTPUT       : BOOLEAN = TRUE THEN OK                            *)
(* USES         : NO SIDE EFFECTS                                   *)
(* CALLS        : NONE                                              *)
(* ERRORS       : NONE                                              *)
(* GLOBAL VARS  : NONE                                              *)
(* SUBJECT      : DATE NUMBERS, CHECK IF VALID                      *)
(* DOMAIN       : ARNO A. KARNER                      LEVEL : 000   *)
(********************************************************************)

FUNCTION ValidDateNumber                   ( MonthNum : Month_Number ;
                                             DayNum   : Day_Number   ;
                                             YearNum  : Year_Number )
                                             : BOOLEAN ;

VAR
   Stat : BOOLEAN ;

BEGIN (* FUNCTION *)
   Stat := TRUE ;
   IF MonthNum > 12
     THEN Stat := FALSE
     ELSE BEGIN (* ELSE 00 *)
             IF DayNum > MaxDaysPerMonth [ MonthNum ]
             THEN BEGIN (* THEN 01 *)
                     IF ( MonthNum <> 2 ) OR
                        ( YearNum MOD 4 > 0 ) OR
                        ( DayNum > 29 )
                        THEN Stat := FALSE ;
                  END ; (* THEN 01 *)
          END ; (* ELSE 00 *)
   ValidDateNumber := Stat ;
END ; (* FUNCTION *)

// REV_POS.FUN
(********************************************************************)
(* THIS FUNCTION WILL RETURN THE POSITION OF THE SEARCH STRING IN   *)
(*   THE GIVEN STRING SEARCHING FROM BACK TO FRONT INSTEAD OF FRONT *)
(*   TO BACK                                                        *)
(* INPUT        : Search_Str,Pattern_Str                            *)
(* INPUT/OUTPUT : NONE                                              *)
(* OUTPUT       : INTEGER                                           *)
(* USES         : NO SIDE EFFECTS                                   *)
(* CALLS        : NONE                                              *)
(* ERRORS       : NONE                                              *)
(* GLOBAL VARS  : NONE                                              *)
(* SUBJECT      : STRINGS, CONTAININ                                *)
(* DOMAIN       : ARNO A. KARNER                      LEVEL : 000   *)
(********************************************************************)

FUNCTION Rev_Pos ( Pattern_Str, Search_Str : Work_String ) : INTEGER;

VAR
   No_Match        : BOOLEAN ;
   Slen , Plen , I : INTEGER ;

BEGIN (* FUNC *)
   Slen := LENGTH ( Search_Str ) ;
   Plen := LENGTH ( Pattern_Str ) ;
   IF ( Plen > Slen ) OR ( Plen = 0 )
   THEN BEGIN (* NO WAY HOSEA *)
           Rev_Pos := 0 ;
        END   (* NO WAY HOSEA *)
   ELSE BEGIN (* SAME LENGTH OR LESS *)
           IF Plen = Slen
           THEN BEGIN (* STRINGS SAME LENGTH *)
                  IF Search_Str = Pattern_Str
                  THEN Rev_Pos := 1
                  ELSE Rev_Pos := 0 ;
                END   (* STRINGS SAME LENGTH *)
           ELSE BEGIN (* SEARCH FOR PATTERN *)
                   I := Slen - Plen + 1 ;
                   No_Match := TRUE ;
                   WHILE ( I > 0 ) AND No_Match DO
                   BEGIN (* WHILE 00 *)
                      No_Match:=(Pattern_Str<>COPY(Search_Str,I,Plen));
                      IF No_Match THEN I := I -1 ;
                   END ;
                   IF NOT No_Match
                   THEN Rev_Pos := I (* PATTERN MATCHED AT THE Ith Pos *)
                   ELSE Rev_Pos := 0 ;
                END ; (* SEARCH FOR PATTERN *)
        END ; (* SAME LENGTH OR LESS *)
END ; (* FUNC *)

// FILESEPR.PAS
(********************************************************************)
(* THIS PROCEDURE WILL TAKE AN INPUT FILE PATH AND RETURN ITS       *)
(* COMPONETES Path, Name, Extension                                 *)
(* INPUT        : F_S ; PATH\FILE NAME . EXT                        *)
(* INPUT/OUTPUT : NONE                                              *)
(* OUTPUT       : File_Path,File_Name,File_Ext                      *)
(* USES         : NO SIDE EFFECTS                                   *)
(* CALLS        : NONE                                              *)
(* ERRORS       : NONE                                              *)
(* GLOBAL VARS  : NONE                                              *)
(* SUBJECT      : FILE NAMES, PARSS FILE SPEC                       *)
(* DOMAIN       : ARNO A. KARNER                      LEVEL : 000   *)
(********************************************************************)

PROCEDURE File_Separator               ( VAR F_S       : File_Str ;
                                         VAR File_Path : File_Str ;
                                         VAR File_Name : String_08 ;
                                         VAR File_Ext  : String_03 ) ;

VAR
   Tem       : File_Str ;
   I,J, Len  : INTEGER ;

BEGIN (* PROC *)
   File_Ext  := '' ;
   File_Name := '' ;
   File_Path := '' ;
   Len := Length ( F_S ) ;
   I   := Len ;
   WHILE (( I > 0 ) AND ( NOT ( F_S [ I ] = '\' ) )) DO I := I - 1 ;
   IF I <> 0
   THEN BEGIN (* FOUND PATH *)
           File_Path := COPY ( F_S , 1 , I ) ;
           Tem := COPY ( F_S , I + 1 , Len - I ) ;
        END  (* FOUND PATH *)
   ELSE Tem := F_S ;
   J := POS ( '.' , Tem ) ;
   Len := LENGTH ( Tem ) ;
   IF J <> 0
   THEN BEGIN (* FOUND EXT *)
           IF J <> Len
           THEN BEGIN (* NOT ZERO LENGTH EXT *)
                   File_Ext := COPY ( Tem , J + 1 , Len - J ) ;
                   File_Name := COPY ( Tem , 1 , J - 1 ) ;
                END
           ELSE File_Name := COPY ( Tem , 1 , Len - 1 ) ;
        END   (* FOUND EXT *)
   ELSE BEGIN (* GET FILE NAME *)
           File_Name := Tem ;
        END ; (* GET FILE NAME *)
END ; (* PROC *)



// A_LIN_CH.PAS
(********************************************************************)
(* THIS PROCEDURE WILL ADD A CHAR TO THE LINE RECORD ON THE END OF  *)
(* INPUT LINE POINTER. WILL ALSO UPDATE CPL BY 1                    *)
(* INPUT        : Line_Ptr,ch                                       *)
(* INPUT/OUTPUT : NONE                                              *)
(* OUTPUT       : UPDATED LINE RECORD                               *)
(* USES         : NO SIDE EFFECTS                                   *)
(* CALLS        : NONE                                              *)
(* ERRORS       : NONE CHECKED                                      *)
(* GLOBAL VARS  : NONE                                              *)
(* SUBJECT      : PRINTER, LINE OF TEXT                             *)
(* DOMAIN       : ARNO A. KARNER                      LEVEL : 000   *)
(********************************************************************)

PROCEDURE Add_Line_Ch ( Lin : Line_Ptr ;
                        Ch  : CHAR ) ;

BEGIN (* PROC *)
   WITH Lin^ DO
   BEGIN
      Line := Line + Ch ;
      CPL  := CPL + 1 ;
   END ;
END ; (* PROC *)

// A_LIN_ST.PAS
  (********************************************************************)
(* THIS PROCEDURE WILL ADD A STRING OF CHARS WITH PRINTABLE LEN     *)
(* INPUT        : Line_Ptr,St,Len                                   *)
(* INPUT/OUTPUT : NONE                                              *)
(* OUTPUT       : UPDATED LINE RECORD                               *)
(* USES         : NO SIDE EFFECTS                                   *)
(* CALLS        : NONE                                              *)
(* ERRORS       : ERROR JUST REPORTED                               *)
(* GLOBAL VARS  : NONE                                              *)
(* SUBJECT      : PRINTER, LINE OF TEXT                             *)
(* DOMAIN       : ARNO A. KARNER                      LEVEL : 000   *)
(********************************************************************)

PROCEDURE Add_Line_Str                           ( Lin : Line_Ptr ;
                                                   St  : Work_String ;
                                                   Len : INTEGER ) ;

BEGIN (* PROC *)
   WITH Lin^ DO
   BEGIN (* WITH *)
      IF LENGTH ( ST ) + LENGTH ( Line ) < 255
      THEN BEGIN (* SIZE OK *)
              Line := Line + St ;
              CPL := CPL + Len ;
           END
      ELSE BEGIN
              WRITELN ('Error in add line string > 255 ') ;
              WRITELN ( Line ) ;
              WRITELN ( St ) ;
              WRITELN ( Len ) ;
           END ;
   END ; (* WITH *)
END ; (* PROC *)

//   DELPAGE.PAS
(********************************************************************)
(* THIS PROCEDURE WILL DELET THE INPUT PAGE RECORD AND UPDATE PTRS  *)
(* INPUT        : NONE                                              *)
(* INPUT/OUTPUT : First,P,Last                                      *)
(* OUTPUT       : NONE                                              *)
(* USES         : NO SIDE EFFECTS                                   *)
(* CALLS        : NONE                                              *)
(* ERRORS       : NONE                                              *)
(* GLOBAL VARS  : NONE                                              *)
(* SUBJECT      : PRINTER DEL PAGE                                  *)
(* DOMAIN       : ARNO A. KARNER                      LEVEL : 000   *)
(********************************************************************)

PROCEDURE Delete_Page ( Var First , P , Last : Page_Ptr ) ;

BEGIN (* PROC *)
   IF P^.Prev = NIL THEN First := P^.Next ELSE P^.Prev^.Next := P^.Next ;
   IF P^.Next = NIL THEN Last  := P^.Prev ELSE P^.Next^.Prev := P^.Prev ;
END ; (* PROC *)

//   PRNTOUT.PAS
(********************************************************************)
(* THIS PROCEDURE WILL OUTPUT THE STRING OF TEXT TO THE DESIRED DEV *)
(* INPUT        : Output_Device,S                                   *)
(* INPUT/OUTPUT : NONE                                              *)
(* OUTPUT       : NONE                                              *)
(* USES         : NO SIDE EFFECTS                                   *)
(* CALLS        : NONE                                              *)
(* ERRORS       : I/O POSS BUT NOT CHECK                            *)
(* GLOBAL VARS  : NONE                                              *)
(* SUBJECT      : STRINGS,OUTPUT                                    *)
(* DOMAIN       : ARNO A. KARNER                      LEVEL : 000   *)
(********************************************************************)

PROCEDURE Print_Out                  ( Output_Device : Output_Type ;
                                       S             : Work_String ) ;

BEGIN (* OUTPUT LINE OF LISTING *)
   CASE Output_Device OF
      UserCRT        : WRITELN ( CON , S ) ;
      Printer    : IF ( POS ( Form_Feed , S ) = 0 )
                   THEN WRITELN ( LST , S )
                   ELSE WRITE   ( LST , S ) ;
      Disk       : WRITELN ( Outf , S ) ;
      Odd_Pages  : WRITELN ( OddF , S ) ;
      Even_Pages : WRITELN ( EveF , S ) ;
      Null       : ;
   END ; (* CASE *)
END ; (* OUTPUT LINE OF LISTING *)

(********************************* level 001 *****************************)
//   TIMNTIM0.FUN
(********************************************************************)
(* THIS FUNCTION WILL RETURN A STRING IN TIME 0 FORMAT HH:MM:SS.hh  *)
(* INPUT        : Hour, Minute, Second, Hundredths of seconds       *)
(* INPUT/OUTPUT : NONE                                              *)
(* OUTPUT       : HOURS,MINUTES,SECONDS,HUNDREDTHS in a 11 char str *)
(* USES         : NO SIDE EFFECTS                                   *)
(* CALLS        : INT $1A                                           *)
(* ERRORS       : NONE CHECKED                                      *)
(* GLOBAL VARS  : NONE                                              *)
(* SUBJECT      : CONVERT, TIME NUMBERS TO TIME 0 STRING            *)
(* DOMAIN       : ARNO A. KARNER                      LEVEL : 001   *)
(********************************************************************)

FUNCTION  Tim_Num_Time0                  ( Hour      : Hour_Number   ;
                                           Minute    : Minute_Number ;
                                           Second    : Second_Number ;
                                           Hundredth : Hundredths )
                                           : Time0 ;

VAR
   Strng    : Time0 ;
   Strng1   : Time0 ;

BEGIN (* FUNC *)
   STR ( Hour : 2 , Strng ) ;
   Strng := Strng + ':' ;
   STR ( Minute : 2 , Strng1 ) ;
   Strng := Strng + Strng1 + ':' ;
   STR ( Second : 2 , Strng1 ) ;
   Strng := Strng + Strng1 + '.' ;
   STR ( Hundredth : 2 , Strng1 ) ;
   Strng := Strng + Strng1 ;
   IF Strng [  4 ] = ' ' THEN Strng [  4 ] := '0' ;
   IF Strng [  7 ] = ' ' THEN Strng [  7 ] := '0' ;
   IF Strng [ 10 ] = ' ' THEN Strng [ 10 ] := '0' ;
   Tim_Num_Time0 := Strng ;
END ; (* FUNC *)

//   FIXPATH.FUN
(********************************************************************)
(* THIS FUNCTION WILL FIX PATH NAMES THAT LACK TRAILING SLASHES     *)
(*  IT WILL ALSO REMOVE ..\ AND TRAILING SPACES                     *)
(* INPUT        : Work_String                                       *)
(* INPUT/OUTPUT : NONE                                              *)
(* OUTPUT       : File_Str                                          *)
(* USES         : NO SIDE EFFECTS                                   *)
(* CALLS        : Rev_Pos                                           *)
(* ERRORS       : NONE                                              *)
(* GLOBAL VARS  : NONE                                              *)
(* SUBJECT      : PATH NAMES                                        *)
(* DOMAIN       : ARNO A. KARNER                      LEVEL : 001   *)
(********************************************************************)

FUNCTION Fix_Path_Str          ( Path_Str : Work_String ) : File_Str ;

VAR
   I,J : INTEGER ;
   Tem : Work_String ;

BEGIN (* FUNC *)
   (* DELETE ALL TRAILING SPACES *)
   WHILE Path_Str [ LENGTH ( Path_Str ) ] = ' ' DO
      Path_Str := COPY ( Path_Str , 1 , LENGTH ( Path_Str ) - 1 ) ;
   (* ADD SUB DIRECTORY SYMBOL IF NOT THERE *)
   IF (Rev_Pos ( '\', Path_Str ) > Rev_Pos ( '.', Path_Str ))
      AND ( Path_Str [ LENGTH ( Path_Str ) ] <> '\' )
   THEN Tem := Path_Str + '\'
   ELSE Tem := Path_Str ;
   I := POS ( '\..\', Tem ) ;
   IF I <> 0
   THEN J := Rev_Pos ( '\', Copy ( Tem, 1, I -1 ))
   ELSE J := 0 ;
   WHILE (I > 2) AND (J > 0) DO  (* loop while deletable directories*)
   BEGIN (* WHILE *)
      Tem := COPY ( Tem, 1 , J ) + COPY ( Tem, I + 4, 255 ) ;
      I := POS ( '\..\' , Tem ) ;
      IF I <> 0
      THEN J := Rev_Pos ( '\', Copy ( Tem, 1, I -1 ))
      ELSE J := 0 ;
   END ; (* WHILE *)
   Fix_Path_Str := COPY ( Tem , 1 , 64 ) ;
END ; (* FUNC *)

//   DATNDAT0.PAS
(********************************************************************)
(*  THIS PROCEDURE RETURNS A STRING 11 CHARS LONG IN THEN FOLLOWING *)
(*     FORMAT MMM DD YYYY                                           *)
(* INPUT        : Month_Number, Day_Number, YearNumber              *)
(* INPUT/OUTPUT : NONE                                              *)
(* OUTPUT       : STAT = NO ERRS AND VALID OUTPUTS IF TRUE          *)
(*              : OUTDATE0 = STRING 11 CHARS LONG 'MMM DD YYYY'     *)
(* USES         : NO SIDE EFFECTS                                   *)
(* CALLS        : Valid_Date_Number                                 *)
(* ERRORS       : INVALID DATE NUMBERS                              *)
(* GLOBAL VARS  : NONE                                              *)
(* SUBJECT      : CONVERT, DATE NUMBERS TO DATE 0 STRING            *)
(* DOMAIN       : ARNO A. KARNER                      LEVEL : 001   *)
(********************************************************************)

PROCEDURE Date_Nums_To_Date0           (     MonthNum : Month_Number ;
                                             DayNum   : Day_Number   ;
                                             YearNum  : Year_Number  ;
                                         VAR OutDate0 : Date0       ;
                                         VAR Stat     : BOOLEAN   ) ;

(* GLOBAL VARS *)
(* MonthNames [1..12] OF PARAGRAPH *)

VAR
   TempDate0 : Date0 ;
   TempDate1 : Date0 ;

BEGIN (* PROC *)
   Stat := ValidDateNumber ( MonthNum , DayNum , YearNum ) ;
   IF NOT Stat THEN
           BEGIN (* THEN *)
              WRITELN ('ERR INVALID DATE NUMBERS ') ;
           END   (* THEN *)
      ELSE BEGIN (* ELSE *)
              TempDate0 := '' ;
              TempDate0 := COPY ( MonthNames [ MonthNum ] , 1 , 3 )
                           + ' ' ;
              STR ( DayNum , TempDate1 ) ;
              IF LENGTH (TempDate1)=1 THEN
                   TempDate1 := '0' + TempDate1 ;
              TempDate0 := TempDate0 + TempDate1 + ' ' ;
              STR ( YearNum :4 , TempDate1 ) ;
              TempDate0 := TempDate0 + TempDate1 ;
              OutDate0 := TempDate0 ;
      END ; (* ELSE *)
   IF NOT Stat THEN WRITELN ( 'ERROR IN PROC DATNDAT0.PRC' ) ;
END ; (* PROC *)

//   FDATNUMS.PAS
(********************************************************************)
(* THIS PROCEDURE WILL RETURN THE DATE AND TIME OF LAST WRITE TO    *)
(* THE INPUT FILE OR RETURN OK = FALSE                              *)
(* INPUT        : THE FILE                                          *)
(* INPUT/OUTPUT : NONE                                              *)
(* OUTPUT       : MONTH,DAY,YEAR,HOUR,MINUTE,SEC,OK                 *)
(* USES         : NO SIDE EFFECTS                                   *)
(* CALLS        : MSDOS $57                                         *)
(* ERRORS       : INVALID COMMAND, BAD HANDLE                       *)
(* GLOBAL VARS  : NONE                                              *)
(* SUBJECT      : FILE, DATE TIME, READ                             *)
(* DOMAIN       : ARNO A. KARNER\PUBLIC               LEVEL : 001   *)
(********************************************************************)

PROCEDURE  _File_Date_Time_Num         ( VAR The_File : text ;
                                        VAR Month    : Month_Number ;
                                        VAR Day      : Day_Number ;
                                        VAR Year     : Year_Number ;
                                        VAR Hour     : Hour_Number ;
                                        VAR Minute   : Minute_Number ;
                                        VAR Second   : Second_Number ;
                                        VAR OK       : BOOLEAN ) ;


VAR
   Packed_Digits,
   Handle      : INTEGER ;
   Regs : reg ;

BEGIN (* PROC *)
   Halt(0); // (*)
{
   OK := TRUE ;
   Handle := MEMW [ SEG ( The_File ) : OFS ( The_File ) ] ;
   Regs.al := 0;
   Regs.AH := $57;
   Regs.BX := Handle;
   MSDOS ( Regs ) ;
   IF ( Regs.Flags AND 1 <> 0 )
   THEN OK := FALSE
   ELSE BEGIN (* OK *)
           Packed_Digits := Regs.dx;
           Year          := ( Packed_Digits SHR 9 ) + 1980 ;
           Month         := ( ( Packed_Digits SHR 5 ) and $000F ) ;
           Day           := ( Packed_Digits and $001F ) ;
           Packed_Digits := Regs.CX ;
           Second        := ( Packed_Digits AND $1F ) * 2 ;
           Minute        := ( Packed_Digits SHR 5 ) AND $3F ;
           Hour          := Packed_Digits SHR 11 ;
        END ; (* OK *)
        }
END ; (* PROC *)


//   READDATE.PAS
(********************************************************************)
(* THIS PROCEDURE WILL PROCEDURE WILL READ THE DATE FROM THE REAL   *)
(*     TIME CLOCK                                                   *)
(* INPUT        : NONE                                              *)
(* INPUT/OUTPUT : NONE                                              *)
(* OUTPUT       : CURRENT MONTH NUMBER                              *)
(*              : CURRENT DAY NUMBER                                *)
(*              : CURRENT YEAR NUMBER                               *)
(* USES         : NO SIDE EFFECTS                                   *)
(* CALLS        : INTERUPT $1A , BCD_TO_INTEGER                     *)
(* ERRORS       : NONE                                              *)
(* GLOBAL VARS  : REG.TYPE                                          *)
(* SUBJECT      : DATE, READ REAL TIME CLOCK                        *)
(* DOMAIN       : ARNO A. KARNER\PUBLIC               LEVEL : 001   *)
(********************************************************************)

PROCEDURE Read_Date                      ( VAR Month : Month_Number  ;
                                           VAR Day   : Day_Number    ;
                                           VAR Year  : Year_Number ) ;



VAR
   D:TSystemTime;

BEGIN
    GetLocalTime(D);

   Month    := D.Month;
   Day      := D.Day;
   Year     := D.Year;



END ; (* END PROC *)


//   READTIME.PAS
(********************************************************************)
(* THIS PROCEDURE WILL READ THE TIME IN THE REAL TIME CLOCK         *)
(* INPUT        : NONE                                              *)
(* INPUT/OUTPUT : NONE                                              *)
(* OUTPUT       : HOURS,MINUTES,SECONDS,HUNDREDTHS                  *)
(* USES         : NO SIDE EFFECTS                                   *)
(* CALLS        : INT $1A,BCD TO INTEGER                            *)
(* ERRORS       : NONE                                              *)
(* GLOBAL VARS  : NONE                                              *)
(* SUBJECT      : TIME, READ REAL TIME CLOCK                        *)
(* DOMAIN       : ARNO A. KARNER                      LEVEL : 001   *)
(********************************************************************)

PROCEDURE Read_Time                  ( VAR Hour      : Hour_Number   ;
                                       VAR Minute    : Minute_Number ;
                                       VAR Second    : Second_Number ;
                                       VAR Hundredth : Hundredths ) ;

VAR
   SecondBcd    : INTEGER ;
   HourBcd      : INTEGER ;
   MinuteBcd    : INTEGER ;
   HundredthBcd : INTEGER ;
   Regs         : Reg ;

BEGIN
   Halt(0);
   {
   Regs.AH := 2 ;
   INTR ($1A,Regs) ;
   WITH Regs DO
   BEGIN (* WITH 01 *)
      HourBcd      := CH ;
      MinuteBcd    := CL ;
      SecondBcd    := DH ;
      HundredthBcd := DL ;
   END ;  (* WITH 01 REGS *)
   Hour      := BcdToInteger ( HourBcd ) ;
   Minute    := BcdToInteger ( MinuteBcd ) ;
   Second    := BcdTointeger ( SecondBcd ) ;
   Hundredth := BcdTointeger ( HundredthBcd ) ;
   }
END ; (* END PROC *)


//   N_Line.PAS
(********************************************************************)
(* THIS PROCEDURE WILL CREATE AND INITIALIZE LINE RECORD            *)
(* INPUT        : NONE                                              *)
(* INPUT/OUTPUT : Line                                              *)
(* OUTPUT       : NONE                                              *)
(* USES         : NO SIDE EFFECTS                                   *)
(* CALLS        : Memory                                            *)
(* ERRORS       : OUT OF MEMORY, PROGRAM HALTED                     *)
(* GLOBAL VARS  : NONE                                              *)
(* SUBJECT      : PRINTER , NEW LINE RECORD                         *)
(* DOMAIN       : ARNO A. KARNER                      LEVEL : 001   *)
(********************************************************************)

PROCEDURE New_Line_Ptr                        ( VAR Lin : Line_Ptr ) ;

BEGIN (* PROC *)
   IF Memory > Min_Memory
   THEN BEGIN (* ENOUGH MEMORY *)
           NEW ( Lin ) ;
           WITH Lin^ DO
           BEGIN (* INIT PAGE *)
              Next  := NIL ;
              CPL := 0 ;
              Line  := '' ;
           END ; (* FIRST PAGE *)
        END   (* ENOUGH MEMORY *)
   ELSE BEGIN (* OUT OF MEMORY *)
           WRITELN ( 'Error out of memory. Program aborted.') ;
           HALT ;
        END ; (* OUT OF MEMORY *)
END ; (* PROC *)

//   N_Page.PAS
(********************************************************************)
(* THIS PROCEDURE WILL CREATE AND INITALIZE A PAGE RECORD           *)
(* INPUT        : NONE                                              *)
(* INPUT/OUTPUT : First,Cur,Last                                    *)
(* OUTPUT       : NONE                                              *)
(* USES         : NO SIDE EFFECTS                                   *)
(* CALLS        : Memory                                            *)
(* ERRORS       : OUT OF MEMORY PROGRAM HALTS                       *)
(* GLOBAL VARS  : NONE                                              *)
(* SUBJECT      : PRINTER, NEW PAGE RECORD                          *)
(* DOMAIN       : ARNO A. KARNER                      LEVEL : 001   *)
(********************************************************************)

PROCEDURE New_Page_Ptr         ( VAR First , Cur , Last : Page_Ptr ) ;

BEGIN (* PROC *)
   IF Memory > Min_Memory
   THEN BEGIN (* ENOUGH MEMORY *)
           NEW ( Cur ) ;
           WITH Cur^ DO
           BEGIN (* INIT PAGE *)
              IF First = NIL
              THEN BEGIN (* FIRST PAGE *)
                      First := Cur ;
                      Last  := Cur ;
                      Prev  := NIL ;
                      Next  := NIL ;
                   END  (* FIRST PAGE *)
              ELSE BEGIN (* NEW LAST PAGE *)
                      Prev       := Last ;
                      Next       := NIL ;
                      Last^.Next := Cur ;
                      Last       := Cur ;
                   END ; (* NEW LAST PAGE *)
              First_Line := NIL ;
              Last_Line := NIL ;
              Cur_LPP := 0 ;
              Max_CPL := 0 ;
           END ; (* INIT PAGE *)
        END   (* ENOUGH MEMORY *)
   ELSE BEGIN (* OUT OF MEMORY *)
           WRITELN ( 'Error out of memory. Program aborted.') ;
           HALT ;
        END ; (* OUT OF MEMORY *)
END ; (* PROC *)

//   MISCPROC.PAS
procedure scrn_update(indent : boolean);
const
 mainx = 18;
 incx = 20;

BEGIN
 IF indent
  THEN
   gotoxy(incx,wherey)
  ELSE
   gotoxy(mainx,wherey);
 WRITE( Line_Numb : 10 )
END;

function get_answer(opt1,opt2 : option_type) : option_type;
var ch : char;
 BEGIN
  REPEAT
   // read(kbd,ch)
     Read(cH);
  UNTIL ch in [opt1,opt2,upcase(opt1),upcase(opt2)];
  WRITELN(ch);
  get_answer := upcase(ch)
 END;

function get_choices(opt1,opt2,opt3 : option_type) : option_type;
var ch : char;
 BEGIN
  REPEAT
//   read(kbd,ch)
      READ(cH);
  UNTIL ch in [opt1,opt2,opt3,upcase(opt1),upcase(opt2),upcase(opt3)];
  WRITELN(ch);
  get_choices := upcase(ch)
 END;

procedure empty_keyboard;
var
 c : char;
BEGIN
 while keypressed do
    //  read(kbd,c)
     READ(C)
END;
 PROCEDURE dealwithuser;

 VAR
    oldx,oldy : integer;
    answer : option_type;
    c : char;

 BEGIN
    empty_keyboard;
    oldx:=wherex; oldy:=wherey;
    WRITELN;
    WRITE ('Press space to continue, Esc to abort ...');
    answer := get_answer(#32,#27);
    IF answer=#27
    THEN BEGIN (* ABORT *)
            IF ( 'P' IN Switches )
            THEN BEGIN
                    WRITELN ( LST ) ;
                    WRITE ( LST , Form_Feed ) ;
                 END ;
            HALT ;
         END
    ELSE BEGIN
            gotoxy(wherex,wherey-1);
            delline;
            IF (oldy=25) or (oldy=23) THEN oldy := 23;
            gotoxy(oldx,oldy)
         END ;
 END ; (* NESTED DEAL WITH USER *)


//   PRN_PAGE.PAS
(********************************************************************)
(* THIS PROCEDURE WILL OUTPUT PAGE TO THE OUT_DEVICE CONTAINED      *)
(* THIS PAGE RECORD                                                 *)
(* INPUT        : NONE                                              *)
(* INPUT/OUTPUT : First,Last,No                                     *)
(* OUTPUT       : OUTPUTED PAGE OF INFORMATION                      *)
(* USES         : NO SIDE EFFECTS                                   *)
(* CALLS        : Print_Out                                         *)
(* ERRORS       : NONE                                              *)
(* GLOBAL VARS  : NONE                                              *)
(* SUBJECT      : PRINTER, PAGE OUTPUT                              *)
(* DOMAIN       : ARNO A. KARNER                      LEVEL : 001   *)
(********************************************************************)

PROCEDURE Print__Page                 ( VAR First , Last : Page_Ptr ;
                                        VAR No           : INTEGER ) ;

VAR
   P        : Page_Ptr ;
   L        : Line_Ptr ;
   Tem      : String_10 ;
   T, Setup : Work_String ;

BEGIN (* PROC *)
   Setup := '';
   No := No + 1 ;
   STR ( No : Linenum_Size , Tem ) ;
   P := First ;
   First := First^.Next ;
   IF ('6' IN Switches )
   THEN BEGIN (* 66 LINES PER INCH *)
           Setup          := Setup + LPI06 ;
        END
   ELSE BEGIN (* 88 LINES PER PAGE *)
           IF ('9' IN Switches )
           THEN BEGIN (* 9 LINES PER INCH  *)
                   Setup          := Setup + LPI09 ;
                END   (* 9 LINES PER INCH  *)
           ELSE BEGIN (* DEFAULT 8 LINES PER INCH *)
                   Setup          := Setup + LPI08 ;
                END ; (* DEFAULT 8 LINES PER INCH *)
        END ;
   WITH P^ DO
   BEGIN (* WITH THIS PAGE *)
      IF Max_CPL <= 80
      THEN BEGIN
              Setup := Setup + CPL80 ;
              Chars_Per_Line := 80 ;
           END
      ELSE IF Max_CPL <= 96
           THEN BEGIN
                   Setup := Setup + CPL96 ;
                   Chars_Per_Line := 96 ;
                END
           ELSE BEGIN
                   Setup := Setup + CPL132 ;
                   Chars_Per_Line := 136 ;
                END ;
      IF ('D' IN Switches ) THEN Setup := Setup + Double_Strike_On
           ELSE Setup := Setup + Double_Strike_Off ;
      IF First_Line <> NIL
      THEN BEGIN (* PUT PAGE NUMBER IN *)
              IF ODD ( No )
              THEN BEGIN (* ODD PAGE *)
                      STR ( Max_CPL : 6 , T ) ;
                      Setup := Setup + T ;
                      STR ( Cur_LPP : 6 , T ) ;
                      WHILE LENGTH ( T ) < Chars_Per_Line - 27 DO
                            T := T + ' ' ;
                      T := T + 'Page ' + Tem ;
                      First_Line^.Line := Setup + T ;
                      IF ('O' IN Switches)
                      THEN Out_Device := Odd_Pages
                      ELSE IF ('P' IN Switches )
                           THEN Out_Device := Printer
                           ELSE IF ('S' IN Switches )
                                THEN Out_Device := UserCRT ;
                   END   (* ODD PAGE *)
              ELSE BEGIN (* EVEN PAGE *)
                      STR ( Max_CPL : 6 , T ) ;
                      Setup := Setup + T ;
                      STR ( Cur_LPP : 6 , T ) ;
                      Setup := Setup + T + '  ' ;
                      First_Line^.Line := Setup + ' Page ' + Tem ;
                      IF ('E' IN Switches)
                      THEN Out_Device := Even_Pages
                      ELSE IF ('P' IN Switches )
                           THEN Out_Device := Printer
                           ELSE IF ('S' IN Switches )
                                THEN Out_Device := UserCRT ;
                   END ; (* EVEN PAGE *)
           END ; (* PUT PAGE NUMBER IN *)
      WHILE First_Line <> NIL DO
      BEGIN (* PRINT LINES *)
         L          := First_Line ;
         First_Line := First_Line^.Next ;
         Print_Out ( Out_Device , L^.Line ) ;
         DISPOSE ( L ) ;
      END ; (* PRINT LINES *)
      IF ( 'P' IN Switches )
      THEN Print_Out ( Out_Device , Form_Feed ) ;
   END ; (* WITH THIS PAGE *)
   DISPOSE (P) ;
END ; (* PROC *)


(********************************* level 002 *****************************)
//   CURDATE0.PAS
(********************************************************************)
(* THIS FUNCTION WILL RETURN THE CURRENT DATE STRING IN DATE 0      *)
(* FORMAT MMM DD YYYY WHERE MMM IS AN 3 LETTER ABRVEATION OF THE    *)
(* MONTH DD IS THE DAY NUMBER YYYY YEAR                             *)
(* INPUT        : NONE USES REAL TIME CLOCK                         *)
(* INPUT/OUTPUT : NONE                                              *)
(* OUTPUT       : CURRENT DATE                                      *)
(* USES         : NO SIDE EFFECTS                                   *)
(* CALLS        : Read_Date, Date_Nums_To_Date0                     *)
(* ERRORS       : IMPROPER DATE NUMBERS                             *)
(* GLOBAL VARS  : NONE                                              *)
(* SUBJECT      : DATE , CURRENT , DATE 0 FORMAT                    *)
(* DOMAIN       : ARNO A. KARNER                      LEVEL : 002   *)
(********************************************************************)

FUNCTION Cur_date0   : Date0 ;

VAR
   OK : BOOLEAN ;
   M  : Month_Number ;
   D  : Day_Number ;
   Y  : Year_Number ;
   Da : Date0 ;

BEGIN (* CUR DATE0 *)
   Read_Date ( M , D , Y ) ;
   Date_Nums_To_Date0 ( M , D , Y , Da , OK ) ;
   IF OK THEN Cur_Date0 := Da ELSE Cur_Date0 := 'DATE ERR' ;
END ; (* CUR DATE0 *)

//   CURTIME0.PAS
(********************************************************************)
(* THIS FUNCTION WILL RETURN THE CURRENT TIME IN TIME 0 FORMAT      *)
(* HH:MM:SS.HH                                                      *)
(* INPUT        : NONE USES REAL TIME CLOCK                         *)
(* INPUT/OUTPUT : NONE                                              *)
(* OUTPUT       : Cur_Time0                                         *)
(* USES         : NO SIDE EFFECTS                                   *)
(* CALLS        : Read_Time, Tim_Num_Time0                          *)
(* ERRORS       : NONE                                              *)
(* GLOBAL VARS  : NONE                                              *)
(* SUBJECT      : TIME, CURRENT TIME, TIME 0 FORMAT                 *)
(* DOMAIN       : ARNO A. KARNER\PUBLIC               LEVEL : 002   *)
(********************************************************************)

FUNCTION Cur_Time0 : Time0 ;

VAR
   H  : Hour_Number ;
   M  : Minute_Number ;
   S  : Second_Number ;
   Hu : Hundredths ;

BEGIN (* FUNC *)
   Read_Time ( H , M , S , Hu ) ;
   Cur_Time0 := Tim_Num_Time0 ( H , M , S , Hu ) ;
END ; (* FUNC *)

//   FTIMSTMP.PAS
(********************************************************************)
(* THIS FUNCTION WILL RETURN THE FILES DATE0 TIME0 TIME STAMP       *)
(* INPUT        : THE FILE VARIABLE                                 *)
(* INPUT/OUTPUT : NONE                                              *)
(* OUTPUT       : DATE0 TIME0 OF THE INPUT FILE IF OK               *)
(* USES         : NO SIDE EFFECTS                                   *)
(* CALLS        : File_Date_Time_Num ,Date_Nums_To_Date0,           *)
(*              : Tim_Num_Time0                                     *)
(* ERRORS       : FILE PROBLEMS                                     *)
(* GLOBAL VARS  : NONE                                              *)
(* SUBJECT      : FILES , DATE TIME , TIME STAMP                    *)
(* DOMAIN       : ARNO A. KARNER                      LEVEL : 002   *)
(********************************************************************)

FUNCTION File_Time_Stamp                       ( The_File : file_str)
                                               : String_40 ;

VAR
   Age: Integer;
   DateTime: TDateTime;

   M  : Month_Number ;
   D  : Day_Number ;
   D0 : Date0 ;
   Y  : Year_Number ;
   H  : Hour_Number ;
   Mi : Minute_Number ;
   S  : Second_Number ;
   Hu : Hundredths ;
   T  : String_40 ;
   OK : BOOLEAN ;

BEGIN (* FUNC *)
   Result := ' ';
   exit;

   Age := FileAge(The_File);
   DateTime :=  FileDateToDateTime(Age)



//   _File_Date_Time_Num ( The_File , M , D , Y , H , Mi , S , OK ) ;
//   Date_Nums_To_Date0 ( M , D , Y , D0 , OK ) ;
//   T := D0 + '  ' + Tim_Num_Time0 ( H , Mi , S , Hu ) ;
//   File_Time_Stamp := T ;
END ; (* FUNC *)


//   PRNTPAGE.PAS
(********************************************************************)
(* THIS PROCEDURE WILL PROCESS THE PAGES AND SEND ON TO OUTPUT IF   *)
(* THAT PAGE IS DONE BEING PROCESSED                                *)
(* INPUT        : NONE                                              *)
(* INPUT/OUTPUT : First,Last,No                                     *)
(* OUTPUT       : PAGE IF DONE PROCESSING                           *)
(* USES         : NO SIDE EFFECTS                                   *)
(* CALLS        : Print__Page,New_Line_Ptr,Delete_Page              *)
(* ERRORS       : NONE                                              *)
(* GLOBAL VARS  : NONE                                              *)
(* SUBJECT      : PRINTER, PROCESS PAGE                             *)
(* DOMAIN       : ARNO A. KARNER                      LEVEL : 002   *)
(********************************************************************)

PROCEDURE Print_Page                  ( VAR First , Last : Page_Ptr ;
                                        VAR No           : INTEGER ) ;

VAR
   Squeezed : BOOLEAN ;
   L        : Line_Ptr ;
   P        : Page_Ptr ;

BEGIN (* AT LEAST ONE OR MORE PAGES PRESENT *)
   { COMPRESS PAGES SWITCH }
   P := First^.Next ;
   IF ( 'C' IN Switches )
   THEN BEGIN (* TRY TO COMPRESS THE TWO PAGES *)
           IF ( First^.Next <> Last )
           THEN BEGIN (* THREE OR MORE PAGES PRESENT *)
                   WITH First^ DO
                   BEGIN (* INSIDE FIRST PAGE REC *)
                      IF ( P^.Cur_LPP + Cur_LPP + 1 < Lines_Per_Page )
                      THEN BEGIN (* COMBINE FIRST TWO PAGES INTO ONE *)
                              New_Line_Ptr    ( L ) ;
                              Last_Line^.Next := L ;
                              Last_Line       := L ;{INSERT BLANK LINE}
                              Last_Line^.Next := P^.First_Line ;
                              Last_Line       := P^.Last_Line ;
                              Squeezed        := TRUE;
                              Cur_LPP         := Cur_LPP + P^.Cur_LPP+1;
                              IF Max_CPL < P^.Max_CPL
                                 THEN Max_CPL := P^.Max_CPL ;
                           END   (* COMBINE FIRST TWO PAGES INTO ONE *)
                      ELSE BEGIN
                              Print__Page ( First , Last , Page_Numb ) ;
                              Squeezed := FALSE ;
                           END ;
                   END ; (* INSIDE FIRST PAGE REC *)
                   IF squeezed THEN Delete_Page ( First , P , Last ) ;
                END ; (* THREE OR MORE PAGES PRESENT *)
        END   (* TRY TO COMPRESS THE TWO PAGES *)
   ELSE BEGIN (* PRINT PAGE *)
           Print__Page ( First , Last , Page_Numb ) ;
        END ; (* PRINT PAGE *)
END ; (* TWO OR MORE PAGES PRESENT *)


(********************************* level 003 *****************************)
//   NEWPAGE.PAS
(********************************************************************)
(* THIS PROCEDURE WILL CREATE A NEW PAGE FOR THE OUTPUT STREAM      *)
(* INPUT        : fv, File_Name , Title                             *)
(* INPUT/OUTPUT : NONE                                              *)
(* OUTPUT       : NONE                                              *)
(* USES         : MODIFIES First_Page, Cur_Page, Last_Page,         *)
(*              : Page_Numb                                         *)
(* CALLS        : Cur_Date0,Cur_Time0,New_Page_Ptr,New_Line_Ptr     *)
(*              : Print_Page                                        *)
(* ERRORS       : NONE                                              *)
(* GLOBAL VARS  : NONE                                              *)
(* SUBJECT      : PRINTER , NEW PAGE DONE WITH PREVIOUS             *)
(* DOMAIN       : ARNO A. KARNER                      LEVEL : 003   *)
(********************************************************************)

PROCEDURE New_Page                     ( VAR fv        : TEXT ;
                                             File_Name : File_Str ;
                                             Title     : TitleType ) ;

VAR
   Squeezed    : BOOLEAN ;
   L           : Line_Ptr ;
   P           : Page_Ptr ;
   date        : Date0 ;
   Date_Stuff1, Date_Stuff, Tem : Work_String ;

BEGIN (* PROC *)
    (* CHECK IF PRINTER AND NOT FIRST PAGE *)
   Date_Stuff  := Title + ': ' + File_Name ;
   Date_Stuff1 := 'Listed ' + Cur_date0 + '  ' + Cur_Time0 +
                   '    Created ' + File_Time_Stamp ( File_Name ) ;
   New_Page_Ptr ( First_Page , Cur_Page , Last_Page ) ;
   WITH Cur_Page^ DO
   BEGIN (* WITH *)
      Out_Device := Output_Device ;
      New_Line_Ptr ( L ) ;
      First_Line := L ;
      Last_Line  := L ;
      New_Line_Ptr ( L ) ;
      Last_Line^.Next := L ;
      Last_Line  := L ;
      L^.Line := ( Date_Stuff ) ;
      New_Line_Ptr ( L ) ;
      Last_Line^.Next := L ;
      Last_Line  := L ;
      L^.Line := ( Date_Stuff1 ) ;
      New_Line_Ptr ( L ) ;
      Last_Line^.Next := L ;
      Last_Line  := L ;
      Max_CPL := 78 ;
      Cur_LPP := 4 ;
   END ; (* WITH *)
   IF ( First_Page <> Last_Page )
   THEN Print_Page ( First_Page , Last_Page , Page_Numb ) ;
END ; (* PROC *)



(********************************* level 004 *****************************)
//   NEW_LINE.PAS
(********************************************************************)
(* THIS PROCEDURE WILL INSERT THE INPUT LINE INTO THE INPUT PAGE    *)
(* INPUT        : fv,File_Name,Title,Line_Len                       *)
(* INPUT/OUTPUT : Line,Line_Num                                     *)
(* OUTPUT       : NEXT LINE RECORD READY FOR DATA PREV ONE STORED   *)
(* USES         : NO SIDE EFFECTS                                   *)
(* CALLS        : New_Page                                          *)
(* ERRORS       : NONE                                              *)
(* GLOBAL VARS  : NONE                                              *)
(* SUBJECT      : PRINTER, NEW LINE                                 *)
(* DOMAIN       : ARNO A. KARNER                      LEVEL : 004   *)
(********************************************************************)

PROCEDURE New_Line                        ( VAR fv        : TEXT ;
                                                File_Name : File_Str ;
                                                Title     : TitleType ;
                                            VAR Line      : Line_Ptr ;
                                            VAR Line_Num  : INTEGER ) ;

VAR
   Tem : work_String ;

BEGIN (* PROC START NEW LINE OF TEXT *)
   IF Cur_Page^.Cur_LPP >= Lines_Per_Page
     THEN New_Page ( fv , File_Name , Title ) ;
   WITH Cur_Page^ DO
   BEGIN (* INSIDE PAGE REC *)
      Cur_LPP         := Cur_LPP + 1 ;
      Last_Line^.NEXT := Line ;
      Last_Line       := Line ;
      IF Line^.CPL > Max_CPL THEN Max_CPL := Line^.CPL ;
   END ; (* INSIDE PAGE REC *)
   New_Line_Ptr ( Line ) ;
   Line_Num := Line_Num + 1 ;
   STR ( Line_Num : Linenum_Size , Tem ) ;
   Tem := Tem + ' ' ;
   Line^.Line := Tem ;
   Line^.CPL  := Linenum_Size + 1 ;
END ; (* PROC START NEW LINE OF TEXT *)


(********************************* level 005 *****************************)
//I WRITEID.PAS
procedure writeid;
VAR
   I ,
   xx : integer;
  function rsvdword: boolean;
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
    var
      i, j, k: integer;
      upid:    string[127];
    begin
      upid := '';
      for i := 1 to length(id) do
        upid := upid + upcase(copy(id,i,1));
      i := 1;
      j := reserved_count - 1;
      repeat
        k := (i+j) div 2;
        if upid > wordlist[k] then i := k+1
                            else j := k
    until i = j;
    rsvdword := (upid = wordlist[i])
    end {rsvdword};

   PROCEDURE Search ( var w1 : Word_Ptr ) ;

   VAR
      w : Word_Ptr;
      x : Item_Ptr;

   BEGIN (* NESTED *)
      w := w1 ;
      if w = nil
      THEN BEGIN
              new(w);
              new(x);
              with w^ do
              begin
                 key   := id;
                 left  := nil;
                 right := nil;
                 first := x ;
                 last  := x ;
              end ;
              x^.lno := Line_Numb ;
              x^.next := nil;
              w1 := w
           end
      ELSE BEGIN
              if id < w^.key
              THEN search ( w^.left )
              ELSE BEGIN
                      IF id > w^.key
                      THEN search ( w^.right )
                      ELSE BEGIN
                              new(x);
                              x^.lno := Line_Numb ;
                              x^.next := nil;
                              w^.last^.next := x;
                              w^.last := x
                           END ;
                 END ;
          END ;
    END ; (* NESTED SEARCH *)


    Procedure Regular_video;
    begin
        TextBackground(black);
        TextColor(white);
    end;

    Procedure Reverse_video;
    begin
        TextBackground(white);
        TextColor(black);
    end;

  FUNCTION locase(ch:char) : char;
  BEGIN
   If ch in ['A'..'Z']
    then locase := chr(ord(ch) or $20)
    else locase := ch
  END;

begin (* PROC *)
   if rsvdword then
        BEGIN (* RESERVED WORD *)

           UpCase ( Id ) ;

           I := LENGTH ( Id ) ;
           IF ( 'P' IN Switches )
           THEN BEGIN
                   Add_Line_Str ( Line , Empha_On + Id + Empha_Off , I ) ;
                END (* PRINTER *)
           ELSE Add_Line_Str ( Line , Id , I ) ;
        END
   ELSE BEGIN (* NOT RESERVED WORD *)
           I := LENGTH ( Id ) ;
           Add_Line_Str ( Line , Id , I ) ;
           If ( 'N' in switches ) then
           begin
              for xx := 1 to I do id [ xx ] := locase ( id [ xx ] ) ;
              search ( root ) ;
           end ;
        end ;
end {writeid};

//   STRTPARS.PAS
PROCEDURE Start_Parsing ;
BEGIN (* NESTED START PARSING *)
   IF f in ['a'..'z','A'..'Z','_']
   THEN BEGIN (* ALPHA *)
           id := f;
           scan := symbol
        END
   ELSE BEGIN (* NOT ALPHA *)
           Add_Line_Ch ( Line , f ) ;
           IF f = '''' THEN scan := quote
           ELSE BEGIN (* NOT ALPHA OR QUOTE *)
                   IF f = '{'
                   THEN BEGIN
                           scan := com1;
                           IF 'I' in switches THEN parsing_for_dollars := true ;
                        END
                   ELSE IF f = '(' THEN scan := pcom2 ;
                END ; (* NOT ALPHA OR QUOTE *)
        END ; (* NOT ALPHA *)
END ; (* NESTED START PARSING *)

//   PRINTREE.PAS
(********************************************************************)
(* THIS PROCEDURE WILL PRINT OUT CROSS REFRENCE LISTING TO THE      *)
(* OUTPUT STREEM CONTROLED BY THE SWITCHES                          *)
(* INPUT        : NONE                                              *)
(* INPUT/OUTPUT :                                                   *)
(* OUTPUT       :                                                   *)
(* USES         : NO SIDE EFFECTS                                   *)
(* CALLS        :                                                   *)
(* ERRORS       :                                                   *)
(* GLOBAL VARS  : NONE                                              *)
(* SUBJECT      :                                                   *)
(* DOMAIN       : ARNO A. KARNER                      LEVEL : 00?   *)
(********************************************************************)

PROCEDURE printtree ( w : Word_Ptr ) ;

   PROCEDURE Print_Word ( w : Word ) ;

   VAR
      l   : INTEGER ;
      x   : Item_Ptr;
      Tem : Work_String ;

   BEGIN (* NESTED PRINT WORD *)
      Tem := W.Key ;
      WHILE LENGTH ( Tem ) < Ch_Per_Word DO Tem := Tem + ' ' ;
      Tem := Tem + ' : ' ;
      Add_Line_Str ( Line , Tem , LENGTH ( Tem ) ) ;
      x := w.first;
      l := 0 ;
      repeat
         IF l = Ref_Per_Line THEN
         BEGIN (* NEW LINE *)
            New_Line ( fv , File_Name , Title , Line , Line_Numb ) ;
            Tem := '' ;
            WHILE LENGTH ( Tem ) < Ch_Per_Word DO Tem := Tem + ' ' ;
            Tem := Tem + ' : ' ;
            Add_Line_Str ( Line , Tem , LENGTH ( Tem ) ) ;
            IF NOT ( 'S' IN Switches ) THEN scrn_update(false);
            l := 0
         END ; (* NEW LINE *)
         l := l + 1 ;
         STR ( X^.Lno : LineNum_Size , Tem ) ;
         Add_Line_Str ( Line, Tem , LENGTH ( Tem ) ) ;
         x := x^.next
      UNTIL X = NIL ;
      New_Line ( fv , File_Name , Title , Line , Line_Numb ) ;
      IF NOT ( 'S' IN Switches ) THEN scrn_update ( false )
   END ; (* NESTED PRINT WORD *)

BEGIN (* PRINT TREE *)
   IF w <> NIL THEN
   BEGIN
      printtree ( w^.left ) ;
      Print_Word ( w^ ) ;
      printtree ( w^. right ) ;
   END ;
END ; (* PRINT TREE *)


//   DOLIST.PAS
PROCEDURE do_listing           ( VAR fv    : TEXT ;
                                     title : titletype ;
                                     fn    : file_Str ;
                                     mode  : state ) ;

VAR
   Path : File_Str ;
   Name : String_08 ;
   Ext  : String_03 ;

   PROCEDURE bugout;

   BEGIN
    parsing_for_dollars := false;
    itsan_include := false;
    itsa_directive := false
   END;


   PROCEDURE isitan_include;

   BEGIN
      while f = ' ' do
      BEGIN
         Add_Line_Ch ( Line , f ) ;
         read ( fv , f )
      END ;
      incname:='';
      REPEAT
         Add_Line_Ch ( Line , f ) ;
         incname :=incname + f;
         read(fv,f);
      UNTIL NOT (f in ['.','\',':','A'..'Z','a'..'z','_','0'..'9']);
      File_Separator ( IncName, Path, Name, Ext ) ;
      IF Ext = '' THEN incname := incname + '.PAS';
      IncName := Fix_Path_Str ( Main_Path + IncName ) ;
      IF NOT ( 'T' IN Switches )
      THEN BEGIN (* REGULAR INCLUDED THIS SUCKS *)
              IF Exist ( Incname )
              THEN BEGIN (* INCLUDED EXISTS *)
                      assign ( iv , incname);
                      Reset ( iv ) ;
                      IF NOT ('S' in switches) THEN
                      BEGIN
                         WRITELN ;
                         WRITELN;
                         WRITE ('  Listing include file ',incname);
                         IF 'F' in switches THEN WRITELN(' to file ',outname)
                         ELSE WRITELN;
                         WRITE('  Processing line #')
                      END;
                      taken_careof := False ;
                      do_listing ( iv , 'Include' , incname , none ) ;
                      New_Page ( fv , fn , title ) ;
                      taken_careof := true;
                      IF NOT ('S' in switches) THEN
                      BEGIN
                         WRITELN;
                         WRITELN;
                         WRITE ('Listing main file ',File_Name);
                         IF 'F' in switches THEN WRITELN(' to file ',outname)
                         ELSE WRITELN;
                         WRITE ('Processing line #')
                      END;
                  END  (* INCLUDED EXISTS *)
             ELSE WRITELN ('Error included File : ',Incname ,' does not exist ' ) ;
          END
     ELSE BEGIN (* SAVE FILE NAMES *)
              IF Memory > Min_Memory THEN
              BEGIN (* ENOUGH MEMORY *)
                 NEW ( Inc_Last ) ;
                 WITH Inc_Last^ DO
                 BEGIN (* WITH *)
                    Prev := Inc_Root ;
                    Inc_File_Name := IncName ;
                    Inc_Title := 'Included' ;
                 END ; (* WITH *)
                 Inc_Root := Inc_Last ;
              END ; (* ENOUGH MEMORY *)
           END ; (* SAVE FILE NAMES *)
      parsing_for_dollars := false;
      itsa_directive      := false;
      itsan_include       := false;
   END ; (* NESTED ITS AN INCLUDED *)


   PROCEDURE Symbol_Parsing ;
   BEGIN (* NESTED SYMBOL PARSING *)
      IF f in ['.','a'..'z','A'..'Z','0'..'9','_']
      THEN BEGIN
              id := id + f;
           END
      ELSE BEGIN (* END NAME *)
              WRITEid;
              Add_Line_Ch ( Line , f ) ;
              IF f = '''' THEN scan := quote
              ELSE BEGIN (* NOT QUOTE *)
                      IF f = '{'
                      THEN BEGIN
                              scan := com1;
                              IF 'I' in switches THEN parsing_for_dollars := true
                           END
                      ELSE IF f = '(' THEN scan := pcom2
                           ELSE  scan := none ;
                   END ; (* NOT QUOTE *)
           END ; (* END NAME *)
   END ; (* NESTED SYMBOL PARSING *)

   PROCEDURE Com1_Parsing ;
   BEGIN (* NESTED COM1 PARSING *)
      Add_Line_Ch ( Line , f ) ;
      IF ( f = '+' ) or ( f = '-' ) THEN bugout;
      IF itsan_include THEN
      BEGIN
         isitan_include ;
      END;
      IF itsa_directive THEN
      BEGIN (* IT IS A DERECTIVE *)
         IF ( f = 'I' ) or ( f = 'i' )
         THEN BEGIN
                 itsan_include  := true;
                 itsa_directive := false
              END
         ELSE itsa_directive := false;
      END ; (* IT IS A DIRECTIVE *)
      IF parsing_for_dollars THEN
      BEGIN (* PARSING FOR DOLLARS *)
         IF f = '$'
         THEN BEGIN
                 parsing_for_dollars :=false;
                 itsa_directive := true
              END
         ELSE parsing_for_dollars := false;
      END ; (* PARSING FOR DOLLARS *)
      IF f = '}' THEN
      BEGIN (* END OF COMMENT *)
         parsing_for_dollars := false;
         itsa_directive := false;
         itsan_include := false;
         scan := none
      END ; (* END OF COMMENT *)
   END ; (* NESTED COM1 PARSING *)

   PROCEDURE Pcom2_Parsing ;
   BEGIN (* PCOM 2 PARSING *)
      IF f in['a'..'z','A'..'Z','_']
      THEN BEGIN
              id := f;
              scan := symbol
           END
      ELSE BEGIN
              Add_Line_Ch ( Line , f ) ;
              IF f = '''' THEN scan := quote
              ELSE BEGIN
                      IF f = '{'
                      THEN BEGIN
                              scan := com1;
                              IF 'I' in switches THEN parsing_for_dollars := true ;
                           END
                      ELSE BEGIN
                              IF f = '(' THEN scan := pcom2
                              ELSE BEGIN
                                      IF f = '*'
                                      THEN BEGIN
                                              scan := com2;
                                              IF 'I' in switches THEN parsing_for_dollars := true ;
                                           END
                                      ELSE  scan := none
                                   END ;
                           END ;
                   END ;
           END ;
   END ; (* PCOM 2 PARSING *)

   PROCEDURE Com2_Parsing ;
   BEGIN
      Add_Line_Ch ( Line , f ) ;
      IF (f='+') or (f='-')  THEN bugout;
      IF itsan_include THEN
      BEGIN
         isitan_include;
      END;
      IF itsa_directive  AND  ( ( f = 'I' ) OR ( f = 'i' ) )
      THEN BEGIN (* IT IS A DIRECTIVE *)
              itsan_include := true;
              itsa_directive := false
           END
      ELSE itsa_directive := false;
      IF parsing_for_dollars AND ( F = '$' )
      THEN BEGIN
              itsa_directive := true;
              parsing_for_dollars := false
           END
      ELSE    parsing_for_dollars := false;
      IF f = '*' THEN scan := pcom2x
      ELSE BEGIN
              IF (f = ')') and (lastf='*') THEN
              BEGIN
                 parsing_for_dollars := false;
                 itsa_directive := false;
                 itsan_include := false;
                 scan := none
              END ;
           END ;
   END ; (* COM2 PARSING *)


BEGIN  (* DO LISTING *)
   Fn := Fix_Path_Str ( Fn ) ;
   scan                := mode ;
   parsing_for_dollars := false;
   itsa_directive      := false;
   itsan_include       := false;
   WHILE NOT EOF ( fv ) DO
   BEGIN (* NOT END OF FILE *)
   IF NOT Taken_Careof
   THEN BEGIN (* First page title of current file *)
           New_Page ( fv , fn , title ) ; (* File must be open *)
           Taken_Careof := TRUE ;
        END;
   IF NOT ('S' in switches) THEN  scrn_update(title='Include');
      while ( NOT EOLN (fv) ) AND ( NOT EOF (fv) ) DO
      BEGIN
         IF keypressed THEN dealwithuser;
         read ( fv , f);
         case scan of
            none:   Start_Parsing ;
            symbol: Symbol_Parsing ;
            quote:  BEGIN
                       Add_Line_Ch ( Line , f ) ;
                       IF f = '''' THEN scan := none ;
                    END;
            com1:   Com1_Parsing ;
            pcom2:  Pcom2_Parsing ;
            com2:   Com2_Parsing ;
            pcom2x: BEGIN
                       Add_Line_Ch ( Line , f ) ;
                       IF (f = ')') THEN scan := none
                       ELSE BEGIN
                               scan := com2;
                               lastf:=f
                            END
                    END;
         END ; (* CASE *)
      END ; (* NOT END OF LINE *)
      IF scan = symbol THEN
      BEGIN
         WRITEid;
         scan := none
      END;
      New_Line ( fv , File_Name , Title , Line , Line_Numb ) ;
      readln (  fv ) ;
   END ; (* NOT EOF *)
   CLOSE ( Fv ) ;
END;


//   GETINFO.PAS
procedure get_info;

var
   n,m,i : integer;
   Dir,
   Path : File_Str ;
   PCh : ^Char;
   Name : String_08 ;
   Ext  : String_03 ;

   workparams : string[127];
   The_File : TEXT ;

   procedure query_File_Name;

   begin
      File_Name := '' ;
      write('Enter name of file to be listed [.PAS] : ');
      readln ( File_Name ) ;
      IF LENGTH ( File_Name ) = 0 THEN HALT ;
      File_Separator ( File_Name , Path , Name , ext ) ;
      WRITELN ( Path , '//' , Name , '//' , Ext ) ;
      IF ( Ext = '' )
      THEN File_Name := Path + Name + '.PAS' ;
   end;

   procedure get_File_Name;

   begin
      M := 0;
      repeat
         M := M+1
      until (M > length(workparams)) or (workparams[M] <> ' ');
      N:=M;
      REPEAT
         N:=N+1
      UNTIL (N>length(workparams)) OR (workparams[N]='/');
      File_Name := copy ( workparams , m , ( n - m ) ) ;
      File_Separator ( File_Name , Path , Name , ext ) ;
      IF ( Ext = '' ) AND ( File_Name [ LENGTH ( File_Name ) ] <> '.' )
      THEN File_Name := Path + Name + '.PAS' ;
      IF ( LENGTH ( File_Name ) = 0 ) OR ( not Exist ( File_Name ) )
      THEN BEGIN
              repeat
                 writeln ( 'File ' , File_Name , ' not found.' ) ;
                 query_File_Name;
              until Exist (File_Name);
           END ;
   END ; (* NESTED PROC *)

   procedure waytogo_user;  {* File_Name and switches on command line*}
   begin
      n := pos('/',workparams) + 1;
      While n<=length(workparams) do
      begin
         if upcase( workparams [ n ] ) IN
            ['6','8','9','B','C','D','E','F','I','N','M','O','P','S','T']
         then switches := switches + [upcase(workparams[n])];
         n:=n+1
      end ;
   end;

   PROCEDURE Switch_Menu ;
   VAR
      Answer, Answer1 : char;
   BEGIN (* NESTED *)
      write('Output to file, screen, or printer (F,S,P) ? ');
      answer := get_choices('f','s','p');
      If answer = 'P'
      THEN BEGIN (* PRINTER OPTIONS *)
              Switches := Switches + ['P'] ;
              write('Printer output in Double strike mode (Y,N) ? ');
              if get_answer('y','n') = 'Y'
              then switches := switches + ['D'] ;
              WRITE ('Spool even pages to disk file (Y,N) ? ');
              if get_answer('y','n') = 'Y'
              then switches := switches + ['E'];
              WRITE ('Spool odd pages to disk file (Y,N) ? ');
              if get_answer('y','n') = 'Y'
              then switches := switches + ['O'];
              WRITE ('Compress Procedures on page (Y,N) ? ');
              if get_answer('y','n') = 'Y'
              then switches := switches + ['C'];
              WRITE ('Lines per inch (6,8,9) ? ');
              Answer1 := Get_Choices ('6','8','9') ;
              IF Answer1 = '6' THEN BEGIN
                                       Switches := Switches + ['6'] ;
                                    END
              ELSE IF Answer1 = '8'
                   THEN BEGIN
                           Switches := Switches + ['8'] ;
                        END
                   ELSE BEGIN
                           switches := switches + ['9'] ;
                        END ;
           END
      else
         if answer='S'
         then switches := switches + ['S']
         else begin
                 switches := switches + ['F'];
                 write('Enter name of output file [',copy(File_Name,1,
                        pos('.',File_Name)-1),'.','LST]');
                 readln(outname);
                 if outname=''
                 then outname := copy(File_Name,1,pos('.',File_Name)-1)
                              + '.' + 'LST'
              end;
      write('List Include files within the Main listing (Y,N) ? ');
      if get_answer('y','n') = 'Y'
      then BEGIN
              switches := switches + ['I'];
              WRITE ('Produce top down program listing (Y,N) ? ');
              if get_answer('y','n') = 'Y'
              then switches := switches + ['T'];
        END ;
      write('Produce cross reference of user-defined Vars (Y,N) ? ');
      if get_answer('y','n') = 'Y'
      then switches := switches + ['N'];
      WRITE ('Delete back up files as listed (Y,N) ? ');
      if get_answer('y','n') = 'Y'
      then switches := switches + ['B'];
      WRITE ('Output only modified files (Y,N) ? ') ;
      IF Get_Answer ( 'y' , 'n' ) = 'Y'
      THEN Switches := Switches + ['M'] ;
   end;

begin  (* PROC *)
   PCh := GetCommandLineA;
   workParams := '';
   while Pch^ <> #0 do
   begin
         workparams := workparams+PCh^ ;
         Inc(PCh);
    end;
   If ( POS ( '/' , workparams ) > 0 ) AND
      ( POS ( '/' , WorkParams ) < LENGTH ( WorkParams ) )
   THEN BEGIN (* SWITCHES ON COMMAND LINE *)
           get_File_Name;
           WayToGo_User ;
        END
   ELSE BEGIN (* 03 INPUT DATA SUPLIED *)
           Get_File_Name ;
           Switch_Menu ;
        END ;
   while File_Name [ LENGTH ( File_Name ) ] = #0 DO
        delete ( File_Name , length ( File_Name ) , 1 ) ;
   WRITELN ('FILE NAME ' , File_Name ) ;
   I := LENGTH ( File_Name ) ;
   WHILE ( I > 0 ) AND ( File_Name [ I ] <> '.' ) DO I := I - 1 ;
   IF ( I <> 0 ) AND ( '.BAK' = Copy ( File_Name , I , 4 ) ) THEN
   BEGIN (* DEL BAK RENAME FILE *)
      WRITE ( 'Deleting backup file' ) ;
      ASSIGN ( The_File , File_Name ) ;
      ERASE ( The_File ) ;
      File_Name := Copy ( File_Name , 1 , I ) + 'PAS' ;
   END ; (* DEL BAK RENAME FILE *)
   GetDir ( 0, Dir ) ;
   Dir := Fix_Path_Str ( Dir ) ;
   File_Name := Dir + File_Name ;
   File_Separator ( File_Name , Main_Path, Name, Ext ) ;
   File_Name := Fix_Path_Str ( File_Name ) ;

//   File_Name = UpCase(File_Name) ;

   Main_File_Name := File_Name ;
END ; (* PROC *)


//   MAIN.PAS
BEGIN {*** main ***}
   (***************** INIT ***********************)
   lastf :=' ';  { to prevent an error; see CASE scan of com2,pcom2x }
   Switches     := [];
   get_info;

   empty_keyboard;
   IF ( 'S' IN Switches )
   THEN Output_Device := UserCRT
   ELSE IF ( 'F' IN Switches )
        THEN Output_Device := Disk
        ELSE Output_Device := Printer ;
   (* INIT VARS *)
   First_Page   := NIL ;
   Last_Page    := NIL ;
   Cur_Page     := NIL ;
   Page_Numb    := 0 ;
   New_Line_Ptr ( Line ) ;
   Line_Numb    := 1 ;
   STR          ( Line_Numb : Linenum_Size , ID ) ;
   Id           := Id + ' ' ;
   Add_Line_Str ( Line , Id , LENGTH ( Id ) ) ;
   Taken_Careof := False ;
   Inc_Root     := NIL ;
   Inc_Last     := NIL ;
   Root         := nil;
   scan         := none;
   title        := 'Main';

   (************************** INIT OUTPUT DEVICE *********************)
   IF NOT ('S' in switches) THEN
   BEGIN
      WRITELN ;
      WRITELN ('Listing main file ',File_Name);
      IF 'F' in switches THEN
      BEGIN
         WRITELN ( 'All data to file ', outname ) ;
         ASSIGN  ( OutF , OutName ) ;
         REWRITE ( OutF ) ;
      END ;
      IF 'O' IN Switches THEN
      BEGIN
         File_Separator ( File_Name , File_Path , File_Nam , File_Ext);
         Out_Odd_Name := File_Path + File_Nam + '.ODD' ;
         WRITELN ( File_Path , '//',File_Nam ,'//', File_Ext ) ;
         WRITELN ( 'Odd Pages To File ' , Out_Odd_Name ) ;
         ASSIGN  ( OddF , Out_Odd_Name ) ;
         REWRITE ( OddF ) ;
         Switches := Switches + ['P'] ;
      END ;
      IF 'E' IN Switches THEN
      BEGIN
         File_Separator ( File_Name , File_Path , File_Nam , File_Ext);
         Out_Even_Name := File_Path + File_Nam + '.EVE' ;
         WRITELN ( 'Even Pages to file ' , Out_Even_Name ) ;
         ASSIGN  ( EveF , Out_Even_Name ) ;
         REWRITE ( EveF ) ;
         Switches := Switches + ['P'] ;
      END ;
   END;
   IF ('P' IN Switches )
   THEN BEGIN
           WRITE ( LST , Pre_Set_Skip + ^B ); { PERF SKIP = 2 }
           IF '6' IN Switches THEN Lines_Per_Page := 64
           ELSE IF '8' IN Switches THEN Lines_Per_Page := 84
                ELSE Lines_Per_Page := 94;
        END;
   NEW ( Inc_Last ) ;
   WITH Inc_Last^ DO
   BEGIN (* WITH *)
      Prev := Inc_Root ;
      Inc_File_Name := File_Name ;
      Inc_Title := 'Main' ;
   END ; (* WITH *)
   Inc_Root := Inc_Last ;
   WHILE Inc_Root <> NIL DO
   BEGIN (* PRINT FILES *)
      Inc_Last := Inc_Root ;
      WITH Inc_Last^ DO
      BEGIN (* WITH *)
         Inc_Root := Prev ;
         title    := Inc_Title ;
         File_Name := Inc_File_Name ;
      END ; (* WITH *)
      DISPOSE ( Inc_Last ) ;
      Inc_Last := NIL ;
      File_Separator ( File_Name , File_Path , File_Nam , File_Ext ) ;
      IF ( EXIST ( File_Path + File_Nam + '.BAK' ) )
      THEN BEGIN (* BACKUP FILE EXISTS *)
              IF ( 'B' IN Switches )
              THEN BEGIN (* DELETE BACKUP *)
                      IF NOT ( 'S' IN Switches ) THEN
                      BEGIN (* MESSAGE *)
                         WRITELN ;
                         WRITELN ( 'Deleting ' , File_Path , File_Nam ,
                                   '.BAK' ) ;
                         WRITE   ( 'Processing line # ' ) ;
                      END ; (* MESSAGE *)
                      ASSIGN ( Tem_File , File_Path + File_Nam +
                              '.BAK' ) ;
                      ERASE  ( Tem_File ) ;
                      IF ( 'M' IN Switches )
                      THEN BEGIN (* YES OUTPUT *)
                              IF ( 'S' IN Switches )
                              THEN Output_Device := UserCRT ;
                              IF ( 'P' IN Switches )
                              THEN BEGIN
                                      Output_Device := Printer ;
                                   END ;
                           END ; (* YES OUTPUT *)
                   END ; (* DELETE BACKUP *)
           END   (* BACKUP FILE EXISTS *)
      ELSE BEGIN (* NO BACKUP FILE EXISTS *)
              IF NOT ( 'M' IN Switches )
              THEN BEGIN (* YES OUTPUT *)
                      IF ( 'S' IN Switches )
                      THEN Output_Device := UserCRT ;
                      IF ( 'P' IN Switches )
                      THEN Output_Device := Printer ;
                   END   (* YES OUTPUT *)
              ELSE Output_Device := Null ;
           END ; (* NO BACKUP FILE EXISTS *)
      ASSIGN ( fv , File_Name ) ;
      Reset  ( Fv ) ;
      Taken_Careof := False ;
      do_listing ( fv , title , File_Name , none ) ;
   END ; (* PRINT FILES *)
(*************************** CROSS REFRENCE ***********************)
   IF ('N' in switches) THEN
   BEGIN (* PRODUCE CROSS REF *)
      IF NOT ('S' in switches) THEN
      BEGIN
         WRITELN;
         WRITE('Listing cross reference of ',Main_File_Name);
         IF 'F' in switches THEN WRITELN(' to file ',outname)
         ELSE WRITELN;
         WRITE('Processing line #')
      END;
      title := 'Xref';
      ASSIGN ( fv , Main_File_Name ) ;
      Reset  ( fv ) ;
      New_Page ( fv , Main_File_Name , Title ) ;
      printtree(root);
      CLOSE ( fv ) ;
   END ;
   (************************* PRINT TRAILING PAGES ******************)
   WHILE First_Page <> NIL DO
     Print__Page ( First_Page , Last_Page , Page_Numb ) ;
   (************************* CLOSE FILES AND EXIT *****************)
   IF 'O' IN Switches THEN CLOSE ( Oddf ) ;
   IF 'E' IN Switches THEN CLOSE ( EveF ) ;
   IF 'F' IN Switches THEN CLOSE ( OutF ) ;
END.



