unit _SymbolTable;

{$mode ObjFPC}{$H+}

interface

uses    SysUtils, Tables;


    Function SearchSymbolTable(Which:Integer;             //< letter index #
                           Ident: AnsiString): ItemP; //< uppercase identifier
    Procedure DumpSymbolTable;

    // adds base classes, i.e. int, char, etc.
    Procedure AddBaseClass(N,   // name
              Abbr:AnsiString;  // short name
              NewDT:DataType;   // what it is
              Visible: Boolean = TRUE); // hide item if false

    Procedure InitEntry(Var NewItem:ItemP);

    Function InsertInSymbolTable(Which:Integer;  Ident: AnsiString;
                                 StopOnEqual: Boolean=FALSE ): ItemP;

    Procedure AddStdVar(N:Ansistring; NewDT:DataType);

    Procedure AddStdType(N:Ansistring; NewDT:DataType);

    Procedure AddStdFunc(N:Ansistring; NewDT:DataType);

    Procedure AddStdConst(N:Ansistring; NewDT:DataType);

    Procedure AddStdUnit(N:Ansistring; Visible:Boolean = true);

    Procedure AddStdProc(N:Ansistring);

    Procedure AddStdFile(N:Ansistring);

    procedure DisplaySymbolTableEntries(N:Ansistring);


implementation


// This routine accepts the pointer passed to it
// obtains memory, then initializes it, retutning
// the address of  the memotu available
Procedure InitEntry(Var NewItem:ItemP);
var
    K: Integer;

Begin
    New(NewItem);
    inc(IdentifierCount);

    // initialize everything to prevent subtle bugs
    With NewItem^ do
    begin
    Abbrev       := '';
    NameUC       := '';
    Name         := '';
    Index        := IdentifierCount;   // Serial number
    Count        := 0;
    First        := NIL;
    Last         := NIL;
    UnitIn       := NIL;
    Owner        := NIL;
    PrevInPlace  := NIL;
    NextInPlace  := NIL;
    PrevTotal    := NIL;
    NextTotal    := NIL;
    DT           := NoType;
    Kind         := [NothingAtAll];
    What         := UnusedType;
    For K := 1 to Dummymax do
       Dummy[K] := 0;


        Name      := '';
        Abbrev    := '';
        NameUC    := '';
    end;
End;


   // This routine maintains the list so that SymbolTable[Which]
   // (where 'which' being the index in the alphabet with A as 1,
   // Z as 26, and _ (underscore) as 27m so that it always points
   // to the lowest value entry that starts with it's assigned
   // letter, i.e. for starting at K, it covers idemtifiers starting
   // with 'ka' up through 'kz', while SymbolBottom[Which] is the
   // opposite, (always the highest), i.e. starting at 'KZ' and
   // working down to  'ka'.

    Function InsertInSymbolTable(Which:Integer;     //< letter index #
                                                    //< into symbol table
                                 Ident: AnsiString; //< uppercase identifier

                                 // the next one is if we want to
                                 // stop and return if we find an item
                                 // of the same name
                                 StopOnEqual: Boolean=FALSE ): ItemP;
    Var
       AddItem,
       NextItem,
       PriorItem: ItemP;

    begin
        If SymbolTable [ Which ] = NIL then
        begin     // first entry this "letter"
            InitEntry(SymbolTable[Which]);  // replaces new()
                                            // to initialise structure
            SymbolTable[Which]^.NameUC := Ident;

            {  Not needed here because initentry will zero these anyway
            SymbolTable[Which]^.PrevTotal := NIl;
            SymbolTable[Which]^.NextTotal := NIL;
            }
            // top and bottom now point to same entry
            SymbolBottom[Which] := SymbolTable[Which];

            // give this structure to caller, ready to use
            Result :=  SymbolTable[Which];
            exit
         end;
        // We are at the neginning of the tablr; if this iss less than
        // or rqual to thr first entry, make this the first entry
         If Ident <= SymbolTable[Which]^.NameUC then // is before beginning
         begin
             if StopOnEqual and (Ident = SymbolTable[Which]^.NameUC) then
                 Result := SymbolTable[Which]  // return item found
             else
             begin
                 InitEntry(AddItem);  // replaces new() to initialise structure
                 AddItem^.NameUC :=Ident;

                 // attach this entry to first item
                 AddItem^.NextTotal := SymbolTable[Which];

                 // make this top of chain
                 SymbolTable[Which]^.PrevTotal := AddItem;

                 // now make top of chain point here
                 SymbolTable[Which] := AddItem;
                 // give this structure to caller, ready to use
                 Result := AddItem;
             end;
             exit
         end;
         PriorItem := SymbolTable[Which];
         NextItem := SymbolTable[Which]^.NextTotal;

         repeat // at this point the prior
                // item was less than ItemUC, so let's see
                // if this one is, is equal, or is more
                If NextItem = NIL then
                begin  // we're at bottom of top-to-bottom chain
                    InitEntry( AddItem);      // replaces new()
                                              // to initialise structure
                    AddItem^.NameUC :=Ident;

             //  attach this entry to end of top-to-bottom chain
                     AddItem^.PrevTotal  := PriorItem;
             //  place at bottom of chain
                     PriorItem^.NextTotal := AddItem;
             // tell bottom pointer we're now last ebtry
                     SymbolBottom[Which] := AddItem;
             // give this structure to caller, ready to use
                     Result := AddItem;
                     exit
                 end;
                 if Ident <= NextItem^.NameUC then
                 begin  // "you know it takes some time,
                        // in the middle, in the middle"
                     if StopOnEqual and (Ident =NextItem^.NameUC) then
                        Result := NextItem    // return item found
                     else
                     begin
                         // it goes before this one, after previous
                         InitEntry(AddItem);  // replaces new() to
                                             // initialise structure
                         AddItem^.NameUC :=Ident;

                         // insert in the middlr of structure, "middle"
                         // being "after the first and before the last"

                         // first, insert after prior item
                         PriorItem^.NextTotal := AddItem;

                         // now, the chain is broken, insert this "link" in the
                         // top-to-bottom chain
                         AddItem^.NextTotal := NextItem;

                         // now that top-down chain is fixed, insert
                         // this entry in the other (bottom-to-top) chain
                         NextItem^.PrevTotal := AddItem;

                         // again we brokr the chain,
                         // insert this entry and close
                         AddItem^.PrevTotal := PriorItem;

                         // give this structure to caller, ready to use
                         Result := AddItem;
                     end;
                     exit
                 end;
                 // "But I still haven't found, what I'm looking for."
                 // Follow the chain and keep looking

                 // "rotate" the two items, prior item points here
                 PriorItem := NextItem;
                 // move to next item
                 NextItem  := NextItem^.NextTotal;
         until  false;   // keep going forever (or more likely, until we
                         // reach the  correct insertion point)
    end;

// This scans the symbol table and returns one of the following:
//   1. The particular table is empty so return NIL, set the SearchStatus
//      variable to TableEempty
//   2. The item we are searching for is alphabetically lower than the lowesr
//      item in the rable, return nil, set SearchStatus to SearchLow
//   3. The item is a match. return the record,
//      set SearchStatus to SearchMatch
//   4. The item is not found. Return the lowest record not exceeding this
//      item's alphabetic value, set SearchStatus to  LowerRecord.
Function SearchSymbolTable(Which:Integer;             //< letter index #
                           Ident: AnsiString): ItemP; //< uppercase identifier

    Var
    NextItem,
    PriorItem: ItemP;

    begin

        If SymbolTable[Which] = NIL then
        begin     // first entry this "letter"
            SearchResult := TableEmpty;
            Result :=  NIL;
            exit
        end;
        If Ident <= SymbolTable[Which]^.NameUC then // is @ or before beginning
        begin
             if (Ident = SymbolTable[Which]^.NameUC) then
             Begin
                 SearchResult := SearchMatch;
                 Result := SymbolTable[Which]  // return item found
             End
             else
             begin
                 SearchResult := SearchLow;   // before beginning
                 Result := NIL;
             end;
             exit
        end;
        PriorItem := SymbolTable[Which];
        NextItem := SymbolTable[Which]^.NextTotal;

        repeat
                If NextItem = NIL then
                begin  // we're at bottom of top-to-bottom chain
                    SearchResult := LowerRecord;    // we're returning prior rec
                    Result := PriorItem;
                    exit
                 end;
                 if Ident = NextItem^.NameUC then
                 begin  // "you know it takes some time,
                        // in the middle, in the middle"
                     SearchResult :=  SearchMatch;  // return item found
                     Result := NextItem ;
                     exit;
                 end;
                 If Ident < NextItem^.NameUC then
                 begin
                     // it goes before this one, after previous
                     SearchResult :=  LowerRecord; // we're returning prior rec
                     Result := PriorItem;
                     exit;
                 end;

                 // "But I still haven't found, what I'm looking for."
                 // Follow the chain and keep looking

                 // "rotate" the two items, prior item points here
                 PriorItem := NextItem;
                 // move to next item
                 NextItem  := NextItem^.NextTotal;
        until  false;   // keep going forever (or more likely, until we
                         // reach the  correct insertion point)
END;

//*FIXME This is not finished
Procedure SearchSymbolTableByWhat(Which:Integer;  //< letter index #
                           Ident: AnsiString;     //< uppercase identifier
                           Wha: IdentType;        //< What kind of idetifier
                           NewItem: ItemP);       //< Returned space
begin
    NewItem := SearchSymbolTable(Which,Ident);
    if NewItem = NIL then // find out why
       if SearchResult = TableEmpty then
       begin
           InitEntry(SymbolTable[Which]);
           SymbolBottom[Which] := SymbolTable[Which];  // both pointers same
           NewItem := SymbolTable[Which];
           exit;
       end;


end;


// adds a base class, i.e. int, char, etc. to stmbol table
Procedure AddBaseClass(N,   // name
          Abbr:AnsiString;  // short name
          NewDT:DataType;   // what it is
          Visible: Boolean = TRUE); // hide item if false
var
  Which: integer;
  Ident,
  CW: ansistring;
  NewItem: ItemP;



begin
      Ident := UpperCase(N);
      CW := Copy(Ident,1,1);
      For Which := 1 to IdentMax do
         if CW = ValidIdent[Which] then
           break;

      NewItem := InsertInSymbolTable(Which,Ident);

      With NewItem^ do
      begin
          Name  := N;
          NameUC:= Ident;
          Abbrev:= Abbr;
          DT    := NewDT;
          Kind  := [BaseType,RefOnly,predefined];
          If Not Visible then
              Kind := Kind+[invisible];
          What  := ElementType;
      end;
end;


Procedure AddStdVar(N:Ansistring; NewDT:DataType);
var
      Which: integer;
      Ident,
      CW: ansistring;
      NewItem: ItemP;

BEGIN
        Ident := UpperCase(N);
        CW := Copy(Ident,1,1);
        For Which := 1 to IdentMax do
             if CW = ValidIdent[Which] then
               break;

        // add this item, but if already in table,
        // return current item
        NewItem := InsertInSymbolTable(Which, Ident, True);


        With NewItem^ do
        begin
            // if already there, we're done
            if Name<>'' then
               exit;
            Name  := N;
            NameUC:= Ident;
            Abbrev:= '';
            DT    := NewDT;

            Kind  := [ varkind, refonly, Predefined];
{ add later if hidden procs needed
            If not Visible  then
                 Kind := Kind+[invisible];
}
            What  := ElementType;
          END;

END;


Procedure AddStdType(N:Ansistring; NewDT:DataType);
var
      Which: integer;
      Ident,
      CW: ansistring;
      NewItem: ItemP;

BEGIN
        Ident := UpperCase(N);
        CW := Copy(Ident,1,1);
        For Which := 1 to IdentMax do
             if CW = ValidIdent[Which] then
               break;

        NewItem := InsertInSymbolTable(Which,Ident);

        With NewItem^ do
        begin
            Name  := N;
            NameUC:= Ident;
            Abbrev:= '';
            DT    := NewDT;
            Kind  := [ Typekind, refonly, Predefined ];
{ add later if hidden types needed
            If not Visible  then
                 Kind := Kind+[invisible];
}
            What  := ElementType;
          END;

END;


Procedure AddStdFunc(N:Ansistring; NewDT:DataType);
var
      Which: integer;
      Ident,
      CW: ansistring;
      NewItem: ItemP;

BEGIN
        Ident := UpperCase(N);
        CW := Copy(Ident,1,1);
        For Which := 1 to IdentMax do
             if CW = ValidIdent[Which] then
               break;

        // add the item, but not if already in table
        NewItem := InsertInSymbolTable(Which,Ident,true);

        With NewItem^ do
        begin
        // if peviously defined, we're done
            if Name <> '' then
                exit;
            Name  := N;
            NameUC:= Ident;
            Abbrev:= '';
            DT    := NewDT;
            //       set by       its a     don't    Standard
            //       compiler     function  list     procedural
            Kind  := [predefined, funckind, refonly, stdkind];
{ add later if hidden procs needed
            If not Visible  then
                 Kind := Kind+[invisible];
}
            What  := proceduralType;
          END;

END;


Procedure AddStdConst(N:Ansistring; NewDT:DataType);
var
  Which: integer;
  Ident,
  CW: ansistring;
  NewItem: ItemP;

begin
      Ident := UpperCase(N);
      CW := Copy(Ident,1,1);
      For Which := 1 to IdentMax do
         if CW = ValidIdent[Which] then
           break;

      NewItem := InsertInSymbolTable(Which,Ident,true);

      With NewItem^ do
      begin
          if name <> '' then
              exit;
          Name  := N;
          NameUC:= Ident;
          Abbrev:= '';
          DT    := NewDT;
          Kind  := [RefOnly,predefined,ConstKind];
          If inVisible in Kind then
               Kind := Kind+[invisible];
          What  := ElementType;
      END;
END;

// Sure, this is only used for the System unit,
// but, all of these have ro be initialized anyway
Procedure AddStdUnit(N:Ansistring; Visible:Boolean = true);
var
  Which: integer;
  Ident,
  CW: ansistring;
  NewItem: ItemP;

BEGIN
    Ident := UpperCase(N);
    CW := Copy(Ident,1,1);
    For Which := 1 to IdentMax do
         if CW = ValidIdent[Which] then
           break;

    NewItem := InsertInSymbolTable(Which,Ident, true);

    With NewItem^ do
    begin
    // bail if already defined
        if name <> '' then
            exit;

        Name  := N;
        NameUC:= Ident;
        Abbrev:= '';
        DT  := notype;
        Kind  := [RefOnly,predefined,isUnit];
        If not Visible  then
             Kind := Kind+[invisible];
        What  := UnitType;
      END;
END;

Procedure AddStdProc(N:Ansistring);
var
      Which: integer;
      Ident,
      CW: ansistring;
      NewItem: ItemP;

BEGIN
        Ident := UpperCase(N);
        CW := Copy(Ident,1,1);
        For Which := 1 to IdentMax do
             if CW = ValidIdent[Which] then
               break;

        NewItem := InsertInSymbolTable(Which,Ident, true);

        With NewItem^ do
        begin
            // if already present, we're done
            if name <> '' then
                exit;
            Name  := N;
            NameUC:= Ident;
            Abbrev:= '';
            DT    := notype;
            Kind  := [predefined, prockind, refonly, stdkind];
{ add later if hidden procs needed
            If not Visible  then
                 Kind := Kind+[invisible];
}
            What  := proceduralType;
          END;

END;

Procedure AddStdFile(N:Ansistring);
var
      Which: integer;
      Ident,
      CW: ansistring;
      NewItem: ItemP;

BEGIN
        Ident := UpperCase(N);
        CW := Copy(Ident,1,1);
        For Which := 1 to IdentMax do
             if CW = ValidIdent[Which] then
               break;

        NewItem := InsertInSymbolTable(Which,Ident);

        With NewItem^ do
        begin
            Name  := N;
            NameUC:= Ident;
            Abbrev:= '';
            DT  := FileType;
            Kind  := [predefined,  refonly];
{ add later if hidden files needed
            If not Visible  then
                 Kind := Kind+[invisible];
}
            What  := proceduralType;
          END;

END;

 
Procedure DumpSymbolTable;
Var
    Walker: ItemP;
    Q: Majorkind;
    T: DataType;
    X: IdentType;
    Z,
    K,
    I: Integer;
    SC: StateCond;

    Procedure Retrieve(N:AnsiString);
    var
      Which: integer;
      Ident,
      CW: ansistring;
      NewItem: ItemP;

    BEGIN
        Writeln(' Item searching fot is ',N);
        Ident := UpperCase(N);
        CW := Copy(Ident,1,1);
        For Which := 1 to IdentMax do
             if CW = ValidIdent[Which] then
               break;

        NewItem := SearchSymbolTable(Which,Ident);
        writeln('Searchresult=',Searchresult);
        If NewItem = NIL then
           Writeln('Returned null')
        Else
           Writeln('Result is ',NewItem^.NameUC);

    END;


    begin // DumpSymbolTable
        Z :=0;
        For I := 1 to 27 do
        begin
            K := 0;
            Write('Symbol ',ValidIdent[I]);
            If SymbolTable[I] = NIL then
            begin
               Writeln(' -- Empty');
               continue;
            end;

            walker := NIL;
            // Walker := SymbolTable[I];
            Writeln(' Begins');
            While Walker <> NIL do
            begin
                Inc(K);
                Inc(Z);
                With Walker^ do
                begin
                   Write(Z:4,',', K:4,' Name=');
                   Write(Name,', ');
                   if Name = '' then
                       Write('"',NameUC,'" ');

                   Write('(',abbrev,') [');
                   For T := notype to filetype  do
                      if T in [DT] then
                        write(T,' ');
                   write('] ');
                   for Q := invisible to isunit do
                   if Q in Kind then
                      write(Q,' ');
                   if Count>0 then
                      write(' Count=',Count,', ');;
                   writeln;
                   Write('    ');
                   for X := proceduralType to PredefinedType do
                     if X in [What] then
                        Write(X,' ');


                   writeln;
                end;
                Walker:= Walker^.NextTotal;
            end;
        end;

        DisplaySymbolTableEntries('integer');
        DisplaySymbolTableEntries('apple');
        DisplaySymbolTableEntries('system');
        DisplaySymbolTableEntries('arctan');

        Writeln;
        Writeln(' Total: ',Z);

        Writeln('Test search');
        Retrieve('system');
        Retrieve('apple');

     end;  // DumpSymbolTable


    procedure DisplaySymbolTableEntries(N:Ansistring);
    var
      MK: MajorKind;
      Which: integer;
      Ident,
      CW: ansistring;
      CO: String[2];
      CurrentItem: ItemP;

    BEGIN
        Ident := UpperCase(N);
        CW := Copy(Ident,1,1);
        For Which := 1 to IdentMax do
             if CW = ValidIdent[Which] then
               break;

        CurrentItem := SearchSymbolTable(Which, Ident);
        If SearchResult <>  SearchMatch then
        begin
            writeln('No entry found for ',ident);
            exit;
        end;
        while (CurrentItem <> NIL) and (CurrentItem^.NameUC=Ident) do
        with CurrentItem^ Do
           begin
               Write('Name: ',Name,' Index: ', Index, ' Usage Count: ',count);
               if UnitIn = NIL then
                   Write(' Not in any Unit ')
               else
                   Write(' Unit: ',UnitIn^.Name);
               if dt<> notype then
                   Write(' Datatype: ', DataTypeNames[dt]) ;
               writeln;
               Write('Kind:') ;
               CO :=' ';
               For MK := NothingAtAll to isUnit do
                  if MK in Kind then
                  begin
                    Write(CO,MajorKindNames[MK]);
                    CO := ', '
                  end;


               writeln;
               CurrentItem := CurrentItem^.NextTotal;
           end;

    end;

end.

