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
              IF ( 'C' IN Switches ) OR ( 'D' IN Switches )
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
