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
{$V- }
           Upper_Case ( Id ) ;
{$V+ }
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
