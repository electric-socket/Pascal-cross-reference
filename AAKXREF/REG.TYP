TYPE
    Reg = RECORD CASE INTEGER OF
             1: (AX,BX,CX,DX,BP,SI,DI,DS,ES,FLAGS : INTEGER ) ;
             2: (AL,AH,BL,BH,CL,CH,DL,DH : BYTE ) ;
          END ; (* RECORD *)

