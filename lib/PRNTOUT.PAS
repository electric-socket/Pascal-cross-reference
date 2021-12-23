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
      CRT        : WRITELN ( CON , S ) ;
      Printer    : IF ( POS ( Form_Feed , S ) = 0 )
                   THEN WRITELN ( LST , S )
                   ELSE WRITE   ( LST , S ) ;
      Disk       : WRITELN ( Outf , S ) ;
      Odd_Pages  : WRITELN ( OddF , S ) ;
      Even_Pages : WRITELN ( EveF , S ) ;
      Null       : ;
   END ; (* CASE *)
END ; (* OUTPUT LINE OF LISTING *)