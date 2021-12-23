unit sortattempt;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

implementation

// This routine maintains the list so
// that SymbolTable[Which] is always the lowest value
// entry, e.g. 'a' or -1, and SymbolBottom[Which] is
// always the highest, e.g. Z or 9999
procedure Insert;
Var
    PriorItem,
    NextItem: ListP;

begin
    If SymbolTable[Which] = NIL then
    begin
        SymbolTable[Which] := NewItem;
        SymbolTable[Which]^.Prev := NIl;
        SymbolTable[Which]^.Next := NIL;
        SymbolBottom[Which] := SymbolTable[Which];
        exit
    end;
    If NewItem^.NameUC <= SymbolTable[Which]^.Item then // before beginning
    begin
        NewItem^.Prev := NIL;
        NewItem^.Next := SymbolTable[Which];
        SymbolTable[Which]^.Prev := NewItem;
        SymbolTable[Which] := NewItem;
        exit
    end;
     PriorItem := SymbolTable[Which];
     NextItem := SymbolTable[Which]^.Next;
     repeat // at this point the prior
            // item was less than NewItem^.NameUC, so let's see
            // if this one is, is equal, or is more
          if NextItem = NIL then   // after last
          begin
               NewItem^.Next  := NIL;
               NewItem^.Prev  := PriorItem;
               PriorItem^.Next := NewItem;
               SymbolBottom[Which] := NewItem;
               exit
           end;
           if NewItem^.NameUC <= NextItem^.Item then
           begin  // it goes before this one, after previous
               PriorItem^.Next := NewItem;
               NewItem^.Next := NextItem;
               NextItem^.Prev := NewItem;
               NewItem^.Prev := PriorItem;
               exit
           end;
           PriorItem := NextItem;
           NextItem  := NextItem^.Next;
      until  false;
end;


end.

