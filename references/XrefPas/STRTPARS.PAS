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
