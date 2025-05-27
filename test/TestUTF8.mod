MODULE TestUTF8;

FROM UTF8 IMPORT IsValidUTF8, SkipBOM;
FROM StrIO IMPORT WriteString, WriteLn;

PROCEDURE TestIsValidUTF8;
VAR
  valid1 : ARRAY [0..3] OF CHAR;
  valid2 : ARRAY [0..3] OF CHAR;
  valid3 : ARRAY [0..4] OF CHAR;
  valid4 : ARRAY [0..5] OF CHAR;
  invalid1 : ARRAY [0..2] OF CHAR;
  invalid2 : ARRAY [0..3] OF CHAR;
  invalid3 : ARRAY [0..3] OF CHAR;
  invalid4 : ARRAY [0..3] OF CHAR;
BEGIN
  valid1 := "abc";
  valid2[0] := CHR(0C3H); valid2[1] := CHR(0A9H); valid2[2] := 'c'; valid2[3] := 0C;
  valid3[0] := CHR(0E2H); valid3[1] := CHR(082H); valid3[2] := CHR(0ACH); valid3[3] := 'x'; valid3[4] := 0C;
  valid4[0] := CHR(0F0H); valid4[1] := CHR(09FH); valid4[2] := CHR(092H); valid4[3] := CHR(096H); valid4[4] := 'z'; valid4[5] := 0C;
  invalid1[0] := CHR(0C3H); invalid1[1] := 'x'; invalid1[2] := 0C;
  invalid2[0] := CHR(0E2H); invalid2[1] := CHR(082H); invalid2[2] := 'x'; invalid2[3] := 0C;
  invalid3[0] := CHR(080H); invalid3[1] := 'y'; invalid3[2] := 'z'; invalid3[3] := 0C;
  invalid4[0] := CHR(0C0H); invalid4[1] := CHR(0AFH); invalid4[2] := 'q'; invalid4[3] := 0C;

  WriteString("TestIsValidUTF8: ");
  IF IsValidUTF8(valid1, 3) AND
     IsValidUTF8(valid2, 2) AND
     IsValidUTF8(valid3, 3) AND
     IsValidUTF8(valid4, 4) AND
     NOT IsValidUTF8(invalid1, 2) AND
     NOT IsValidUTF8(invalid2, 3) AND
     NOT IsValidUTF8(invalid3, 1) AND
     NOT IsValidUTF8(invalid4, 2)
  THEN
    WriteString("PASS");
  ELSE
    WriteString("FAIL");
  END;
  WriteLn;
END TestIsValidUTF8;

PROCEDURE TestSkipBOM;
VAR
  buf : ARRAY [0..7] OF CHAR;
  len : CARDINAL;
BEGIN
  (* Test with BOM *)
  buf[0] := CHR(0EFH); buf[1] := CHR(0BBH); buf[2] := CHR(0BFH);
  buf[3] := 'a'; buf[4] := 'b'; buf[5] := 'c'; buf[6] := 0C;
  len := 6;
  SkipBOM(buf, len);
  WriteString("TestSkipBOM (with BOM): ");
  IF (len = 3) AND (buf[0] = 'a') AND (buf[1] = 'b') AND (buf[2] = 'c') THEN
    WriteString("PASS");
  ELSE
    WriteString("FAIL");
  END;
  WriteLn;

  (* Test without BOM *)
  buf[0] := 'x'; buf[1] := 'y'; buf[2] := 'z'; buf[3] := 0C;
  len := 3;
  SkipBOM(buf, len);
  WriteString("TestSkipBOM (no BOM): ");
  IF (len = 3) AND (buf[0] = 'x') AND (buf[1] = 'y') AND (buf[2] = 'z') THEN
    WriteString("PASS");
  ELSE
    WriteString("FAIL");
  END;
  WriteLn;
END TestSkipBOM;

BEGIN
  TestIsValidUTF8;
  TestSkipBOM;
END TestUTF8.