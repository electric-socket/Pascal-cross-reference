program linklist2;
type listP = ^TheList;
     TheList = record
          PrevTotal,
          Next: ListP;
          NameUC: String;
      end;

Var
   I:  Integer;
   ItemUC : String;
   AddItem,
   NextItem,
   GoUp,
   GoDown,
   ListTop,
   ListBottom: ListP;


   Procedure DisplayLinks;
   begin
       GoUp := ListTop;
       GoDown := GoUp;
       While GoUp<>NIL do
       begin
           Write(' ');
           write('Item: ',GoUp^.NameUC,' PrevTotal: ');
           if GoUp^.PrevTotal = nil then
               write('Null ')
           else
               Write(' ',GoUp^.PrevTotal^.NameUC,' ');
           write(' Next: ');
           if GoUp^.Next =NIL then
              writeln('Null ')
           else
              Writeln(' ',GoUp^.Next^.NameUC);

           GoDown := GoUp;
           GoUp := GoUp^.Next;
       end;
       writeln('Reverse Listing');
       GoDown := ListBottom;
       While GoDown <> Nil Do
       begin
           Write(' [',GoDown^.NameUC,'] P: ');
           If Godown^.PrevTotal = Nil  then
              write(' NIL ')
           else
               Write(GoDown^.PrevTotal^.NameUC,' ');
           Write(' N: ');
           If Godown^.Next = Nil  then
               write(' NIL ')
           else
               Write(GoDown^.Next^.NameUC,' ');
           GoDown := GoDown^.PrevTotal;
           writeln;
       end;
    end;

    procedure Insert;
    Var
        PriorItem: ListP;
    begin
        If ListTop = NIL then
        begin
            New(ListTop);
            ListTop^.PrevTotal := NIl;
            ListTop^.Next := NIL;
            ListBottom := ListTop;
            ListTop^.NameUC := ItemUC;
            exit
        end;
        If ItemUC < ListTop^.NameUC then // before beginning
        begin
            New(AddItem);
            Additem^.PrevTotal := NIL;
            AddItem^.Next := ListTop;
            ListTop^.PrevTotal := AddItem;
            ListTop := AddItem;
            ListTop^.NameUC := ItemUC;
            exit
        end;
         PriorItem := ListTop;
         NextItem := ListTop^.Next;
         repeat // at this point the prior
                // item was less than ItemUC, so let's see
                // if this one is, is equal, or is more
              if NextItem = NIL then   // after last
              begin
                   New(AddItem);
                   Additem^.Next  := NIL;
                   AddItem^.PrevTotal  := PriorItem;
                   PriorItem^.Next := AddItem;
                   ListBottom := AddItem;
                   Additem^.NameUC  := ItemUC;
                   exit
               end;
               if ItemUC <= NextItem^.NameUC then
               begin  // it goes before this one, after previous
                   New(AddItem);
                   PriorItem^.Next := AddItem;
                   AddItem^.Next := NextItem;
                   NextItem^.PrevTotal := AddItem;
                   AddItem^.PrevTotal := PriorItem;
                   AddItem^.NameUC := ItemUC;
                   exit
               end;
               PriorItem := NextItem;
               NextItem  := NextItem^.Next;
          until  false;
    end;


begin
    ListBottom := NIL;
    ListTop := NIL;
    writeln('----');
    For I := 1 to 10 do
    begin
       ItemUC := Chr(I+64);
       insert;
    end;
    Writeln('-----',I);
    repeat
      DisplayLinks;

      Write(' Enter  value: ');
      ReadLN(ItemUC);
      insert;
    until ItemUc='';
    Writeln('Final Result');
    DisplayLinks;
    writeln;
    Write ('Press Enter to Exit');
    Readln;
end.

