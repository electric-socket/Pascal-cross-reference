(********************************************************************)
(* THIS PROCEDURE WILL CONVERT ALL LOWER CASE CHARS TO UPPER CASE   *)
(* CHARS FOR ALL CHARS WITHIN THE STRING                            *)
(* INPUT        : NONE                                              *)
(* INPUT/OUTPUT : DATA-STRING : Work_String                         *)
(* OUTPUT       : NONE                                              *)
(* USES         : NO SIDE EFFECTS                                   *)
(* CALLS        : NONE                                              *)
(* ERRORS       : NONE                                              *)
(* GLOBAL VARS  : NONE                                              *)
(* SUBJECT      : STRINGS, CONVERT TO UPPER CASE                    *)
(* DOMAIN       : PUBLIC                              LEVEL : 000   *)
(********************************************************************)

PROCEDURE Upper_Case                ( VAR TempString : Work_String ) ;

BEGIN
   INLINE
       ($C4/$BE/TempString/    (*         LES  DI,TempString [BP]   *)
        $26/$8A/$0D/           (*         MOV  CL,ES:[DI]           *)
        $FE/$C1/               (*         INC  CL                   *)
        $FE/$C9/               (* L1:     DEC  CL                   *)
        $74/$13/               (*         JZ   L2                   *)
        $47/                   (*         INC  DI                   *)
        $26/$80/$3D/$61/       (*         CMP  ES:BYTE PTR [DI],'a' *)
        $72/$F5/               (*         JB   L1                   *)
        $26/$80/$3D/$7A/       (*         CMP  ES:BYTE PTR [DI],'z' *)
        $77/$EF/               (*         JA   L1                   *)
        $26/$80/$2D/$20/       (*         SUB  ES:BYTE PTR [DI],$20 *)
        $EB/$E9) ;             (*         JMP SHORT L1              *)
                               (* L2:                               *)
   END ;