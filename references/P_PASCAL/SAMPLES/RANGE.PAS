(*$c+*)
PROGRAM Something(input,output);

{Original version of this program submitted
 by Sten Ljungkvist of SAAB-SCANIA, Sweden.
 This is a demonstration of European ASCII
 letters in Pascal identifiers.  To type these
 letters, use a "copy con yourfile", and hold
 down the "alt" key while typing the three-digit
 ASCII value of the letter on the numeric keypad.
 When you release the "alt" key, the letter will
 appear on your screen. }

CONST
	l�gsta = 0;	{� = alt 132}
	f�rsta = 0;	{� = alt 148}
	h�gsta = 9999;
TYPE
	�ngstr�m = 2000 .. 10000; {� = alt 143; � = alt 148}
	intword = l�gsta .. h�gsta;
VAR
	���� : �ngstr�m; {These are some sample letters:
			  � = alt 129; � = alt 135;
			  � = alt 136; � = alt 164}
	index : intword;
BEGIN
	{Note that specifying output in a WRITE is optional:}
	writeln(output,'Something or another!'); writeln;
	FOR index := h�gsta-10 TO h�gsta DO
	BEGIN
	 ���� := index;
	 writeln(' This is the value of ����[',
		(h�gsta-index) : 2, ']: ', ���� : 5)
	END
END.
