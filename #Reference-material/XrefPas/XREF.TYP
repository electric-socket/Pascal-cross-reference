TYPE
   TitleType     = STRING [  10 ] ;
   option_type   = STRING [   1 ] ;
   switchsettype = set of char;
   state         = (none,symbol,quote,com1,pcom2,com2,pcom2x);
   Output_Type   = ( CRT , PRINTER , Odd_Pages , Even_Pages , DISK , NULL ) ;

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
