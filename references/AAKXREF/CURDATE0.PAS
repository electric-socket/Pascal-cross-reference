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
