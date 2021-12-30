Unit unit_Evaluator;

Interface

Type
{................................................. .............................}
TExpressionEvaluator = Class
private
FLook: AnsiChar;
FBuffer: AnsiString;
FErrorMsg: AnsiString;

Function IsAlpha(Const c: AnsiChar): Boolean;
Function IsAddOp(Const c: AnsiChar): Boolean;
Function IsMulOp(Const c: AnsiChar): Boolean;
Function IsDigit(Const c: AnsiChar): Boolean;
Function IsWhiteSpace(Const c: AnsiChar): Boolean;
Procedure Error(Const aErrorMsg: AnsiString);
Function Factor: Single;
Function Term: Single;
Procedure Expected(s: AnsiString);
Procedure Match(s: AnsiString);
Procedure GetChar;
Procedure SkipWhiteSpaces;
Function GetNumber: Single;
Function Expression: Single;
public
Function Evaluate(Const aExpression: AnsiString; Var aExpressionResult: Single): Boolean;
Property ErrorMsg: AnsiString Read FErrorMsg;
End;
{................................................. .............................}

Implementation

Uses
SysUtils;

Const
{................................................. .............................}
cCR = #13;
cLF = #10;
cSpace = ' ';
cTab = ^I;

{................................................. .............................}

{................................................. .............................}
Function TExpressionEvaluator.IsAlpha(Const c: AnsiChar): Boolean;
Begin
Result := c In['a'..'z','A'..'Z'];
End;
{................................................. .............................}

{................................................. .............................}
Function TExpressionEvaluator.IsDigit(Const c: AnsiChar): Boolean;
Begin
Result := c In['0'..'9'];
End;
{................................................. .............................}

{................................................. .............................}
Function TExpressionEvaluator.IsAddOp(Const c: AnsiChar): Boolean;
Begin
Result := c In['-','+'];
End;
{................................................. .............................}

{................................................. .............................}
Function TExpressionEvaluator.IsMulOp(Const c: AnsiChar): Boolean;
Begin
Result := c In['/','*'];
End;
{................................................. .............................}

{................................................. .............................}
Function TExpressionEvaluator.IsWhiteSpace(Const c: AnsiChar): Boolean;
Begin
Result := c In[cCR,cLF,cSpace,cTAB];
End;
{................................................. .............................}

{................................................. .............................}
Procedure TExpressionEvaluator.Error(Const aErrorMsg: AnsiString);
Begin
FErrorMsg := aErrorMsg;
Raise Exception.Create(aErrorMsg);
End;
{................................................. .............................}

{................................................. .............................}
Procedure TExpressionEvaluator.Expected(s: AnsiString);
Begin
Error(s + ' Expected');
End;
{................................................. .............................}

{................................................. .............................}
Procedure TExpressionEvaluator.Match(s: AnsiString);
Var
i: Integer;
Begin
For i := 1 To Length(s) Do Begin
If FLook = s[i] Then
GetChar
Else
Expected('''' + s + '''');
End;
SkipWhiteSpaces;
End;
{................................................. .............................}

{................................................. .............................}
Procedure TExpressionEvaluator.GetChar;
Begin
If FBuffer = '' Then
FLook := #0
Else Begin
FLook := FBuffer[1];
Delete(FBuffer,1,1);
End;
End;
{................................................. .............................}

{................................................. .............................}
Procedure TExpressionEvaluator.SkipWhiteSpaces;
Begin
While IsWhiteSpace(FLook) Do
GetChar;
End;
{................................................. .............................}

{................................................. .............................}
Function TExpressionEvaluator.GetNumber: Single;
Var
Number: AnsiString;
Code: Integer;
Begin
If Not IsDigit(FLook) Then Error('Number Expected');
Number := '';
// read integer part
While IsDigit(FLook) Do Begin
Number := Number + FLook;
GetChar;
End;
If FLook = '.' Then Begin
// read fractional part
Number := Number + FLook;
Match(FLook);
If Not IsDigit(FLook) Then Error('Digit Expected after "."');
While IsDigit(FLook) Do Begin
Number := Number + FLook;
GetChar;
End;
End;
Val(Number,Result,Code);
If Code <> 0 Then Error('Illegal Number "'+Number+'"');

SkipWhiteSpaces;
End;
{................................................. .............................}

{................................................. .............................}
Function TExpressionEvaluator.Factor: Single;
Begin
If FLook = '(' Then Begin
Match('(');
Result := Expression;
Match(')');
End
Else
Result := GetNumber;
End;
{................................................. .............................}

{................................................. .............................}
Function TExpressionEvaluator.Term: Single;
Begin
Result := Factor;
While IsMulOp(FLook) Do Begin
Case FLook of
'*': Begin
Match('*');
Result := Result * Term;
End;
'/': Begin
Match('/');
Result := Result / Term;
End;
End;
End;
End;
{................................................. .............................}

{................................................. .............................}
Function TExpressionEvaluator.Expression: Single;
begin
If IsAddOp(FLook) Then
Result := 0
Else
Result := Term;

While IsAddOp(FLook) Do Begin
Case FLook of
'+': Begin
Match('+');
Result := Result + Term;
End;
'-': Begin
Match('-');
Result := Result - Term;
End;
End;
End;
End;
{................................................. .............................}

{................................................. .............................}
Function TExpressionEvaluator.Evaluate(Const aExpression: AnsiString; Var aExpressionResult: Single): Boolean;
Begin
Result := True;

FBuffer := aExpression;
FErrorMsg := '';
GetChar;
Try
aExpressionResult := Expression;
Except
Result := False;
End;
End;
{................................................. .............................}

{................................................. .............................}
End.
