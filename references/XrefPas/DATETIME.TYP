TYPE
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


