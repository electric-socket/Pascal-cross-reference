{$I CrossrefPrefixCode.inc}  // STD definitions
// Crossref - Pascal Cross reference
// Version 0.01
// January 9, 2021
// Copyright 2021 Paul Robinson
// Licensed under GPL v2

// Setup.pas - Intended to initialize the application
// 2021-12-17

Unit setup;


interface


Uses Scan, sysutils, Tables;

Const
       VERSION_MAJOR             = 0;
       VERSION_RELEASE           = 0;
       VERSION_PATCH             = 0;
       VERSION_FULL              = VERSION_MAJOR*1000+
                                   VERSION_RELEASE *10+
                                   VERSION_PATCH;

// note, the folowing MUST be a string of digits in quotes
// as PROGRAM UPD does an auto-upddate on every compile

       VERSION_REV               = '1';



   // General initialization
    Procedure Init;

   //symbol table related
    Function SearchSymbolTable(Which:Integer;             //< letter index #
                           Ident: AnsiString): ItemP; //< uppercase identifier
    Procedure DumpSymbolTable;

    // adds base classes, i.e. int, char, etc.
    Procedure AddBaseClass(N,   // name
              Abbr:AnsiString;  // short name
              NewDT:DataType;   // what it is
              Visible: Boolean = TRUE); // hide item if false
    // installs keywords
    Procedure AddKeyword(const N:AnsiString;                 // name
                               BlockClose: AnsiString = '';  // presumed to require no block closure
                               Key:KeywordType=noactdec;      // what kind of keyword
                               SC:StateCond =NoState);       // any changes

    // general Utility funcyions
    Function Plural(N:Int64; Plu:AnsiString; Sng: AnsiString): Ansistring;
    Function Version:String ;
    Function CopyRight:String ;



implementation



  Function Plural(N:Int64; Plu:AnsiString; Sng: AnsiString): Ansistring;
  Begin
      Result := IntToStr(N);
      Result := ' '+Result+' ';
      If n<>1 Then
          Result:= Result+ Plu
       Else
          Result := Result + Sng;
  End;

Function CopyRight:String;
begin
    Result := 'Copytight 2021 Paul Robinson';
end;

Function Version:String;
begin
     Result := 'Version '+IntToStr( VERSION_MAJOR )+'.' +
               IntToStr( VERSION_RELEASE )+'.' +
               IntToStr( VERSION_PATCH)+' Rev. ' +
                         VERSION_REV ;

end;


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
    DefPage      := 0;
    DefLine      := 0;
    Usage        := NIL;
    UnitIn       := NIL;
    Owner        := NIL;
    LPS          := NIL;
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
        If SymbolTable[Which] = NIL then
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
         If Ident <= SymbolTable[Which]^.NameUC then // is before beginning
         begin
             if StopOnEqual and (Ident = SymbolTable[Which]^.NameUC) then
                 Result := SymbolTable[Which]  // return item found
             else
             begin
                 InitEntry(AddItem);  // replaces new() to initialise structure
                 AddItem^.NameUC :=Ident;
                 { Not needed here because initentry will zero this anyway
                 Additem^.PrevTotal := NIL;
                 }
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

{       Not needed here because initentry will zero this anyway
                 Additem^.NextTotal  := NIL;
}
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
                         // being "after the first and before the last

                         // first, insert after prior item
                         PriorItem^.NextTotal := AddItem;

                         // now, the chain is broken, insert this "link" in the
                         // top-to-bottom chain
                         AddItem^.NextTotal := NextItem;

                         // now that top-down chain is fixed, insert
                         // this entry in the other (bottom-to-top) chain
                         NextItem^.PrevTotal := AddItem;

                         // agin we brokr the chain, insert this entry and close
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
        If Ident <= SymbolTable[Which]^.NameUC then // is before beginning
        begin
             if (Ident = SymbolTable[Which]^.NameUC) then
             Begin
                 SearChResult := SearchMatch;
                 Result := SymbolTable[Which]  // return item found
             End
             else
             begin
                 SearchResult := SearchLow;
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

// adds keywords and modifiers
Procedure AddKeyorMod(const N:AnsiString;                 // name
                           BlockClose: AnsiString = '';   // presumed to need
                                                          // no corresponding
                                                          // keyword for a
                                                          // block closure
                           Key:KeywordType=noactdec;      // what kind
                                                          // of keyword
                           SC:StateCond =NoState;         // any changes
                                                          // in state
                           ID:IdentType= isKeywordType);      // what type
 var
      Which: integer; //< Index subscript entry in symbol tble
      Ident,          //< Identifier in ALL CAPS
      CW: ansistring; //< First char of identifier

      TempItem,
      CloseItem,      //< keyword belonging to blockclose
      PriorItem,     //< for searching the symbol table
      NextItem,
      NewItem: ItemP; //< Item retrieved

      Procedure ItemInit;
      begin
          With NewItem^ do
          begin
              Name        := N;
              DT          := NoType;
              Kind        := [ refonly ,CompilerDefined ]; // remove temporary bit
              What        := ID;
              Closedby    := NIL;
              KW          := Key;
              StateChange := SC;
          end;
      end;

      // Install this keyword last, when there are other
      // identifiers with the same name
      Procedure AddLast(Setup: boolean=true);
      Begin
          PriorItem := NewItem;
          NextItem := NewItem^.NextTotal;

          repeat
              if NextItem = nil then
              begin
                  // attach to prioritem and update SymbolBottom
                  // We don't need newitem as that was something else
                  // create ours

                  InitEntry(NewItem);
                  if setup then    // initialize out keyword
                      ItemInit
                  else
                  with NewItem^ do
                      begin  // initialize for closing keyword
                          NameUC   := Ident;
                          DT       := NoType;
                          Kind     := [ refonly ,CompilerDefined, temporary ]; // use as debugging tool
                          What     := ID;
                          Closedby := NIL;
                          Inc(Count);
                      end;
                  PriorItem^.NextTotal := NewItem;
                  NewItem^.PrevTotal := PriorItem;
                  SymbolBottom[Which] := NewItem;
                  exit;
              end
              else
                  If Nextitem^.NameUC = Ident then
                  begin
                      if NextItem^.What = ID then
                          Exit;        // this keyword/modifier already
                                       // registered; we're done, bail
                      PriorItem := nextitem;
                      NextItem := nextItem^.nextTotal;
                  end
                  else
                  begin // attach before nextitem
                      InitEntry(NewItem);
                      PriorItem^.NextTotal := NewItem;
                      NewItem^.PrevTotal := PriorItem;
                      NewItem^.NextTotal := NextItem;
                      NextItem^.PrevTotal := NewItem;
                      ItemInit;
                      exit;
                  end;
          until false;
      end;

BEGIN //*Procedure AddKeyorMod

      // standard initialization - find what symbol table to use
        Ident := UpperCase(N);
        CW := Copy(Ident,1,1);
        For Which := 1 to IdentMax do
             if CW = ValidIdent[Which] then
               break;

        // TRUE in the following function call means that if this name
        // is previously defined, return that one, don't make a duplicate

        NewItem := InsertInSymbolTable(Which,Ident,TRUE);

        // tight now we have one of three things:
        // 1. A new blank record, which we created now, initialize it
        // 2. A previously declared "closer" kryword which
        //    is now appearing to pick up its initialization
        // 3. A duplicate keyword because different dialects of Pascal
        //    have additional keywords and modifiers. Since modifiers
        //    can be the same as refular identifiers, make sure if one
        //    has the same name as a modifier, make the modifier later
        //    in the  stack so the identifier is found first.

       if  NewItem^.name <> '' then  // don't initialize just yet

        // The returned item is already initialized; we might not
        // nrrd to do anything

        // Here we have a hit on the same identifier which may be a
        // duplicate because of multiple add requesrs (different
        // Pascal language dialects each adding their keywords, or
        // it's a keyword or modifier with the same name as another
        // identifier (WRITE comes to mind as both a standard
        // procedure and a modifier)). So, if this is he same type,
        // (keyword) it's a duplicate and we can ignore it/ If not,
        // move this new one to after any others. stopping and discarding
        // if we/find a keyword along the way.

           if NewItem^.What = ID then  // Keyword already installed
              Exit
           else
            // it's not a keyword, place this one after any others
           Addlast
       else
           ItemInit;      // initialize

        // If this keyword requires a "closer" find it. If it has
        // not yet been defined, create it. When it shows up, we'll
        // fully initialize it
        if BlockClose <> '' then
        begin // Oksy, go look for it
            Ident := UpperCase(BlockClose);
            CW := Copy(Ident,1,1);
            For Which := 1 to IdentMax do
                if CW = ValidIdent[Which] then
                    break;

            // In our case, we request a symbol table slot,
            // if this entry was nor previously created
            // (by another keyword that uses it to
            // close a block,) we don't want a new entry,
            // we want the previous one.

            // if it's already there, take it

            CloseItem := InsertInSymbolTable(Which,Ident,TRUE);

            If CloseItem^.What <> unusedType then
            begin // that's not blank, find or create it
                if closeitem^.What <> ID then
                begin
                    tempItem := newitem;
                    NewItem  := CloseItem;
                    AddLast(false);
                    TempItem^.Closedby := Newitem;
                    exit;
                end
                else
                   NewItem^.Closedby := CloseItem;    // this will never be nil
            end
            else    // this is a new closing entry
                with CloseItem^ do
                begin
                     DT       := NoType;
                     Kind     := [ refonly ,CompilerDefined, temporary ]; // use as debugging tool
                     What     :=  ID;
                     Closedby :=  NIL;
                     Inc(Count);
                end;
        end;

end; //*Procedure AddKeyorMod

Procedure AddKeyword(const N:AnsiString;                 // name
                           BlockClose: AnsiString = '';  // presumed to require
                                                         // no corresponding
                                                         // keyword for a
                                                         // block closure
                           Key:KeywordType=noactdec;     // what kind
                                                         // of keyword
                           SC:StateCond =NoState);

begin
    AddKeyorMod(N, BlockClose,Key,SC)
end;


Procedure AddModifier(const N:AnsiString;                // name
                            BlockClose: AnsiString = ''; // presumed to require
                                                         // no corresponding
                                                         // keyword for a
                                                         // block closure
                           Key:KeywordType=noactdec;     // what kind
                                                         // of keyword
                           SC:StateCond =NoState);

begin
    AddKeyorMod(N, BlockClose,Key,SC, ModifierType);
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

        NewItem := InsertInSymbolTable(Which,Ident);

        With NewItem^ do
        begin
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
{ add later if hidden procs needed
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

        NewItem := InsertInSymbolTable(Which,Ident);

        With NewItem^ do
        begin
            Name  := N;
            NameUC:= Ident;
            Abbrev:= '';
            DT    := NewDT;
            //       set by       its a     don't    Standard
            //       compiler     function  list     procedural
            Kind  := [predefined, funckind, refonly, stdPF];
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

      NewItem := InsertInSymbolTable(Which,Ident);

      With NewItem^ do
      begin
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

    NewItem := InsertInSymbolTable(Which,Ident);

    With NewItem^ do
    begin
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

        NewItem := InsertInSymbolTable(Which,Ident);

        With NewItem^ do
        begin
            Name  := N;
            NameUC:= Ident;
            Abbrev:= '';
            DT    := notype;
            Kind  := [predefined, prockind, refonly, stdPF];
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



Procedure Init;
Var
    I: Integer;

begin

    writeln;    writeln;Writeln('** Start **'); writeln;

    New(StateTable);
    With StateTable^ Do
    Begin
        Cond:= NoState;
        Closer:= '';
        WithCount:= 0;
        Prev:= NIL;
    End;

    WithTable := NIL;

    For I := 1 to IdentMax do
       SymbolTable[I] := Nil;

    TopExtension := 2;
    Extensions[1] :='pas';
    Extensions[2] :='pp';

    TopFolder := 1;
    FolderTable[1] := UnicodeString(GetCurrentDir);


{For thr initialization, we load only the identifiers of standard Pascal.
 Extended, Turbo, Borand, UCSD, XDPascal, Object, Delphi, or Free Pascal
 extensions will be installed later as needed. This is another resson to
 have keywords as symbol table entries: I can add or remove them as
 needed, but standard Pascal items can't be removed. (Can't  examine a
 program if 'Begin' is dropped!)
}

                 // name,         nickname,     type      Visible
    AddBaseClass('enumerator',       'enum',    EnumGroup,False);  // PLCEHLDR
    AddBaseClass('enumerated value', 'econ',    EnumType, False);  // PLCEHLDR
    AddBaseClass('byte',             'byte',     Bytetype);   // 8-bit integer
    AddBaseClass('boolean',          'bool',     Booltype);   // boolean
    AddBaseClass('char',             'char',     charType);   // 8-bit character
    AddBaseClass('ansichar',         'ansc',     charType);   // alias for char
    AddBaseClass('word',             'word',     wordType);   // 16-bit integer
    AddBaseClass('widechar',         'wchr',    wcharType);   // 16-bit char
    AddBaseClass('integer',           'int',      intType);   // 32-bit integer
    AddBaseClass('longint',           'int',      intType);   // alias for int
    AddBaseClass('int64',             'I64',    int64Type);   // 64-bit int
    AddBaseClass('pointer',           'ptr',  pointerType);   // address
    AddBaseClass('set',               'set',      setType);   // set
    AddBaseClass('array',             'arr',   ArrayType);    // general array
    AddBaseClass('string',            'str',   stringType);   // array of char
                                                              // with length
    AddBaseClass('ansistring',        'anss',   AnsStrType);   // Ansistring
    AddBaseClass('unicodestring',     'uni',   uniStrType);   // unicodeString
    AddBaseClass('record',            'rec',   RecordType);   // record
    AddBaseClass('object',            'obj',   objectType);   // object
    AddBaseClass('class',             'cls',    ClassType);   // class
    AddBaseClass('template',         'tmpl', TemplateType);   // class
    AddBaseClass('real',              'real',    realtype);   // 32-bit real
    AddBaseClass('single',            'sng',   singleType);   // single
    AddBaseClass('double',            'dbl',   DoubleType);   // double
                                                              // precision real
    AddBaseClass('extended',          'ext',  ExtendedType);  // 80-bit real
    AddBaseClass('file',             'file',      FileType);  // file

// Std functions and procedures

    AddStdConst('false',       Booltype);
    AddStdConst('true',        Booltype);
    AddStdConst('text',        FileType);
    AddStdConst('nil',         pointerType);
    AddStdConst('maxint',      intType);
    AddStdConst('sethigh',     intType);
    AddStdConst('ordmaxchar',  intType);

    AddStdFunc('abs',    realtype);
    AddStdFunc('sqr',    realtype);
    AddStdFunc('round',  realtype);
    AddStdFunc('trunc',  realtype);
    AddStdFunc('odd',    Booltype);
    AddStdFunc('ord',    intType);
    AddStdFunc('chr',    intType);
    AddStdFunc('pred',   intType);
    AddStdFunc('succ',   intType);
    AddStdFunc('eof',    Booltype);
    AddStdFunc('eoln',   Booltype);
    AddStdFunc('sin',    realtype);
    AddStdFunc('cos',    realtype);
    AddStdFunc('exp',    realtype);
    AddStdFunc('sqrt',   realtype);
    AddStdFunc('ln',     realtype);
    AddStdFunc('arctan', realtype);


// std keywords


    AddKeyWord('and');
    AddKeyWord('do','', nxstmtDec  );
    AddKeyWord('if','', NoActDec, NoState );  // default
    AddKeyWord('in' );
    AddKeyWord('not');
    AddKeyWord('of' );
    AddKeyWord('or' );
    AddKeyWord('to' );
    AddKeyWord('shl');
    AddKeyWord('shr');
    AddKeyWord('xor');
    AddKeyWord('for'  );
    AddKeyWord('var','', ElemDec , invar );
    AddKeyWord('div'  );
    AddKeyWord('mod'  );
    AddKeyWord('set'   );
    AddKeyWord('then','', nxstmtDec  );
    AddKeyWord('else','', nxstmtDec , inCase ); // also used on case
    AddKeyWord('with','', BlockDec, inwith );
    AddKeyWord('goto' );
    AddKeyWord('case', 'end' , BlockDec, incase );
    AddKeyWord('type', '', ElemDec,intype  );
    AddKeyWord('begin', 'end' ,BlockDec, inBegin  );
    AddKeyWord('until','',closureDec);
    AddKeyWord('while','',nxstmtDec);
    AddKeyWord('array'  );
    AddKeyWord('const','',ElemDec, inConst );
    AddKeyWord('label' );
    AddKeyWord('repeat','until' ,BlockDec,  inRepeat );
    // END (and any closure) does not have to
    // say anything; the gentleman calling err
    // I mean keyword calling will
    // announce when it needs her
    AddKeyWord('end','', closureDec );
    AddKeyWord('record', 'end' , structdec, inRecord );
    AddKeyWord('downto' );
    AddKeyWord('packed' );
    AddKeyWord('forward','', postpfmod, inForward );
    AddKeyWord('program','',unitprogDec, inProgram );
    AddKeyWord('external', '',postpfmod, inExternal );
    AddKeyWord('function','',pfdec,inFunction );
    AddKeyWord('procedure', '' ,pfdec, inProcedure );

    AddStdProc('get');
    AddStdProc('put');
    AddStdProc('reset');
    AddStdProc('rewrite');
    AddStdProc('read');
    AddStdProc('write');
    AddStdProc('pack');
    AddStdProc('unpack');
    AddStdProc('new');
    AddStdProc('dispose');
    AddStdProc('readln');
    AddStdProc('writeln');
    AddStdProc('page');
    AddStdProc('mark');
    AddStdProc('release');
    AddStdProc('halt');
    AddStdProc('break');
    AddStdProc('continue');

    AddStdFile('input');
    AddStdFile('output');
    AddStdFile('stdin');
    AddStdFile('stdout');
    AddStdFile('stderr');


    // extended
    if Lang_extended then
    begin
        AddKeyWord('and_then','', nxstmtDec );
        AddKeyword('export');
        AddKeyword('import');
        AddKeyword('module','end',unitprogDec, inunit );
        AddKeyword('only');
        AddKeyword('or_else','', nxstmtDec );
        // equiv to rlse on case stmt
        AddKeyword('otherwise', '' , BlockDec, incase );
        AddKeyword('protected','',genmod);
        AddKeyword('qualified','',genmod);
        AddKeyword('restricted','',genmod);
        AddKeyword('value');

        AddStdFunc('pow',Realtype);
    end;

    if lang_turbo then // turbo pascal
    begin
        AddKeyword('absolute','',genMod);
        AddKeyword('asm','',BlockDec,inIgnore);
        AddKeyword('constructor','',pfDec,inConstructor);
        AddKeyword('destructor','',pfDec,inDestructor);
        AddKeyword('implementation','',impDec,inImplementation);
        AddKeyword('in');
        AddKeyword('inherited','',genMod);
        AddKeyword('inline','',pfDec);
        AddKeyword('interface','',interDec,inInterface);
        AddKeyword('object','end',structdec,inObject);
        AddKeyword('operator','',genMod);
        AddKeyword('reintroduce');
        AddKeyword('self');
        AddKeyWord('unit','end',unitprogDec, inunit );
        AddKeyword('uses','',usesDec);

        AddStdUnit('system');
    end;

    if Lang_XD then  // XDPascal
    begin
        AddKeyword('implementation','',impDec,inImplementation);
        AddKeyword('interface','',interDec,inInterface);
        AddKeyWord('unit','end',unitprogDec, inunit );
        AddKeyword('uses','',usesDec);


        AddStdUnit('system');
    end;

    if Lang_Borland then // Borland Pascal
    begin



        AddStdUnit('system');
    end;

    if Lang_object then // Object Pascal
    begin
        AddKeyWord('as');
        AddKeyWord('class','end',structDec, inClass);
        AddKeyWord('dispinterface');
        AddKeyWord('except');
        AddKeyWord('exports');
        AddKeyWord('finalization');
        AddKeyWord('finally');
        AddKeyWord('initialization');
        AddKeyWord('inline');
        AddKeyWord('is');
        AddKeyWord('library','end',unitprogDec, inLibrary);
        AddKeyWord('on');
        AddKeyWord('out');
        AddKeyWord('packed');
        AddKeyWord('property','',GenMod, inProperty);
        AddKeyWord('raise');
        AddKeyWord('resourcestring');
        AddKeyWord('threadvar');
        AddKeyWord('try','',blockDec, inTry);

        // these are not reserved words, but it's
        // orobably not a good idea to rdefine them
        AddStdProc('exit');
        AddStdProc('halt');
// these are considered modifiers
//        AddSysProc('break');
//        AddSysProc('continue');

        // modifiers
        AddKeyword('absolute','',genmod);
        AddKeyword('abstract','',genmod);
        AddKeyword('alias','',genmod);
        AddKeyword('assembler','',genmod);
        AddKeyword('bitpacked','',genmod);
        AddKeyword('break','',genmod);
        AddKeyword('cdecl','',genmod);
        AddKeyword('continue','',genmod);
        AddKeyword('cppdecl','',genmod);
        AddKeyword('cvar','',genmod);
        AddKeyword('default','',genmod);
        AddKeyword('deprecated','',genmod);
        AddKeyword('dynamic','',genmod);
        AddKeyword('enumerator','',genmod);
        AddKeyword('experimental','',genmod);
        AddKeyword('export','',genmod);
        AddKeyword('external','',genmod);
        AddKeyword('far','',genmod);
        AddKeyword('far16','',genmod);
//        forward  -this is std
        AddKeyword('helper','',genmod);
        AddKeyword('implements','',genmod);
        AddKeyword('index','',genmod);
        AddKeyword('interrupt','',genmod);
        AddKeyword('iocheck','',genmod);
        AddKeyword('local','',genmod);
        AddKeyword('message','',genmod);
        AddKeyword('name','',genmod);
        AddKeyword('near','',genmod);
        AddKeyword('nodefault','',genmod);
        AddKeyword('noreturn','',genmod);
        AddKeyword('nostackframe','',genmod);
        AddKeyword('oldfpccall','',genmod);
        AddKeyword('otherwise','',genmod);
        AddKeyword('overload','',genmod);
        AddKeyword('override','',genmod);
        AddKeyword('pascal','',genmod);
        AddKeyword('platform','',genmod);
        AddKeyword('private','',genmod);
        AddKeyword('protected','',genmod);
        AddKeyword('public','',genmod);
        AddKeyword('published','',genmod);
        AddKeyword('read','',genmod);     // both a mod and a proc
        AddKeyword('register','',genmod);
        AddKeyword('reintroduce','',genmod);
        AddKeyword('result','',genmod);   // teyurn val of proc
        AddKeyword('safecall','',genmod);
        AddKeyword('saveregisters','',genmod);
        AddKeyword('softfloat','',genmod);
        AddKeyword('specialize','',genmod);
        AddKeyword('static','',genmod);
        AddKeyword('stdcall','',genmod);
        AddKeyword('stored','',genmod);
        AddKeyword('strict','',genmod);
        AddKeyword('unaligned','',genmod);
        AddKeyword('unimplemented','',genmod);
        AddKeyword('varargs','',genmod);
        AddKeyword('virtual','',genmod);
        AddKeyword('winapi','',genmod);
        AddKeyword('write','',genmod);   // both std proc and mod

        AddStdUnit('system');
    end;

    if Lang_Delphi then  // Delphi
    begin

        AddStdUnit('system');
    end;

    if Lang_FreePascal then
    begin

        AddStdUnit('system');
    end;









    dumpSymbolTable;
 end;


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
        Writeln(' Utem is ',N);
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

begin
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
        Walker := SymbolTable[I];
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
               if Closedby<>Nil then
               begin
                 Write('Closedby=',ClosedBy^.NameUC,'= ');
               end;
               for SC :=  NoState to inCase do
                  if  SC  in [StateChange] then
                    Write(SC,' ');
               writeln;
            end;
            Walker:= Walker^.NextTotal;
        end;
    end;
    Writeln;
    Writeln(' Total: ',Z);

    Writeln('Test search');
    Retrieve('if');
    Retrieve('apple');
    Retrieve('z');
    Retrieve('baker');
    Retrieve('begone');
    Retrieve('Written');
    Retrieve('I');



end;

end.



