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
     read(kbd,ch)
    UNTIL ch in [opt1,opt2,upcase(opt1),upcase(opt2)];
    WRITELN(ch);
    get_answer := upcase(ch)
   END;

 function get_choices(opt1,opt2,opt3 : option_type) : option_type;
  var ch : char;
   BEGIN
    REPEAT
     read(kbd,ch)
    UNTIL ch in [opt1,opt2,opt3,upcase(opt1),upcase(opt2),upcase(opt3)];
    WRITELN(ch);
    get_choices := upcase(ch)
   END;

 procedure empty_keyboard;
  var
   c : char;
  BEGIN
   while keypressed do
    read(kbd,c)
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
