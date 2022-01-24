unit Notice;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Windows, Tables;

Const
       VERSION_MAJOR             = 0;
       VERSION_RELEASE           = 0;
       VERSION_PATCH             = 6;
       VERSION_FULL              = VERSION_MAJOR*1000+
                                   VERSION_RELEASE *10+
                                   VERSION_PATCH;

// note, the folowing MUST be a string of digits in quotes
// as PROGRAM UPD does an auto-upddate on every compile

       VERSION_REV               = '1';

Var
       ProgramPath,                    // FQFN of name up to folder
       ProgramFolder,                  // folser it's in, null if root
       ProgramName,                    // name excluding extension
       ProgramExt:UnicodeString;       // This program's extension if any



Function CopyRight:String;
Function Version:String;
Function ElapsedTime(Const StartTime, EndTime:SystemTime):String;
Procedure Banner;
Function Plural(N:Int64; Plu:AnsiString; Sng: AnsiString): Ansistring;
Function CTS(Const CTime:SystemTime): AnsiString; //< Create Time String


implementation

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

Function Plural(N:Int64; Plu:AnsiString; Sng: AnsiString): Ansistring;
Begin
    Result := IntToStr(N);
    Result := ' '+Result+' ';
    If n<>1 Then
        Result:= Result+ Plu
     Else
        Result := Result + Sng;
End;

// Create Time String
Function CTS(Const CTime:SystemTime): AnsiString;
begin
   Result := Days[CTime.dayOfWeek]+
             ' '+Months[CTime.month]+
             ' '+IntToStr(CTime.day)+
             ', '+IntToStr(CTime.year)+
             ' '+IntToStr(CTime.Hour)+
             ':';
   if CTime.Minute < 10 then Result := Result+'0';
   Result := Result+ IntToStr(CTime.Minute)+':';
   if CTime.Second < 10 then Result := Result+'0';
   Result := Result+ IntToStr(CTime.Second);
end;



Procedure Banner;
Begin
    Writeln(ProgramName,' - Create cross-reference listing of ',
                          'a Pascal program', Version);
    Writeln(Copyright,' - Released under GPL ver. 2');
    GetLocalTime(StartTime);
    Write('Good ');
    If StartTime.Hour <12 then
        Write('morning')
    else if StartTime.Hour <18 then
        Write('afternoon')
    else
        Write('evening');

    writeln(', it is now ', CTS(StartTime));
    Writeln('Current directory ',GetCurrentDir);
    Writeln;

end;


Function ElapsedTime(Const StartTime, EndTime:SystemTime):String;
VAR
    H,M,S,MS: Integer;

Begin
   // Now tell them how long it took

    H :=  EndTime.Hour;
    if StartTime.Hour < EndTime.Hour  then
        h:=H + 24;
    h := h - StartTime.Hour;
    M := EndTime.Minute ;
    if M < StartTime.minute then
    begin
        H := H-1;
        M := M+60;
    end;
    M := M - StartTime.minute;
    S := EndTime.second  ;
    if S < StartTime.second then
    BEGIN
        M := M-1;
        S := S+60;
    END;
    S := S-StartTime.second;
    MS := EndTime.MilliSecond;
    IF MS < StartTime.MilliSecond then
    begin
        MS := MS+1000;
        S := S-1;
    end;
    MS := MS-StartTime.MilliSecond;

// we won't bother with days,
// nobody is going to process something taking that long

    If H >0 then
        Result  := Plural(H,'hours','hour')+' ';
    If M >0 then

        Result  := Result  + Plural(M,'minutes','minute')+' ';
    if Result  <> '' then
        Result  := Result  +' and';
    Result  := Result  + IntToStr(S)+'.' + IntToStr(MS)+' seconds.';

end;





end.

