MODULE TestUTF8;

IMPORT UTF8;
IMPORT StrIO;
IMPORT WholeStr;
IMPORT SYSTEM;
IMPORT EXCEPTIONS;

PROCEDURE WriteResult(testName: ARRAY OF CHAR; pass: BOOLEAN);
BEGIN
  StrIO.WriteString(testName);
  IF pass THEN
    StrIO.WriteString(": PASS");
  ELSE
    StrIO.WriteString(": FAIL");
  END;
  StrIO.WriteLn;
END WriteResult;

PROCEDURE TestCharLen;
(* Test with valid characters *)
VAR
  pass: BOOLEAN;
BEGIN
  pass := TRUE;

  (* 1-byte ASCII *)
  IF UTF8.CharLen(CHR(0)) # 1 THEN pass := FALSE END;
  IF UTF8.CharLen(CHR(65)) # 1 THEN pass := FALSE END;  (* 'A' *)

  (* 2-byte sequence: 0xC2 (valid start of 2-byte UTF-8) *)
  IF UTF8.CharLen(CHR(0C2H)) # 2 THEN pass := FALSE END;

  (* 3-byte sequence: 0xE0 (valid start of 3-byte UTF-8) *)
  IF UTF8.CharLen(CHR(0E0H)) # 3 THEN pass := FALSE END;

  (* 4-byte sequence: 0xF0 (valid start of 4-byte UTF-8) *)
  IF UTF8.CharLen(CHR(0F0H)) # 4 THEN pass := FALSE END;

  (* Invalid first byte: 0xFF (should raise exception, but here we just check it doesn't return a valid length) *)
  (* This may raise an exception depending on implementation, so we skip this in a simple test *)
  WriteResult("TestCharLen", pass);
END TestCharLen;

PROCEDURE TestInvalidCharLen;
(* Test with invalid characters - exception expected. *)
VAR
  pass: BOOLEAN;
BEGIN
  pass := FALSE;
  (* Invalid first byte: (should raise exception) *)
  IF UTF8.CharLen(CHR(0FFH)) # 2 THEN END;
  WriteResult("TestInvalidCharLen", pass);
EXCEPT
  IF UTF8.IsUtf8Exception() THEN    
    pass := TRUE;  (* Exception raised as expected *)
    WriteResult("TestInvalidCharLen", pass);
    RETURN;
  END;
END TestInvalidCharLen;

PROCEDURE CheckIsValid(testName: ARRAY OF CHAR; buf: ARRAY OF CHAR; len: CARDINAL; expected: BOOLEAN);
VAR
  pass: BOOLEAN;
BEGIN
  pass := UTF8.IsValid(buf, len) = expected;
  WriteResult(testName, pass);
END CheckIsValid;

PROCEDURE TestIsValid;
VAR
  buf: ARRAY [0..7] OF CHAR;
BEGIN
  (* Valid 1-byte ASCII *)
  buf[0] := CHR(65);  (* 'A' *)
  CheckIsValid("IsValid: ASCII", buf, 1, TRUE);

  (* Valid 2-byte UTF-8: U+00A2 (¢) = C2 A2 *)
  buf[0] := CHR(0C2H); buf[1] := CHR(0A2H);
  CheckIsValid("IsValid: 2-byte", buf, 2, TRUE);

  (* Valid 3-byte UTF-8: U+20AC (€) = E2 82 AC *)
  buf[0] := CHR(0E2H); buf[1] := CHR(082H); buf[2] := CHR(0ACH);
  CheckIsValid("IsValid: 3-byte", buf, 3, TRUE);

  (* Valid 4-byte UTF-8: U+1F600 (😀) = F0 9F 98 80 *)
  buf[0] := CHR(0F0H); buf[1] := CHR(09FH); buf[2] := CHR(098H); buf[3] := CHR(080H);
  CheckIsValid("IsValid: 4-byte", buf, 4, TRUE);

  (* Invalid: continuation byte as first byte *)
  buf[0] := CHR(080H);
  CheckIsValid("IsValid: Invalid start", buf, 1, FALSE);

  (* Invalid: incomplete 2-byte sequence *)
  buf[0] := CHR(0C2H);
  CheckIsValid("IsValid: Incomplete 2-byte", buf, 1, FALSE);

  (* Invalid: overlong encoding for ASCII 'A' (should be 1 byte, not 2) *)
  buf[0] := CHR(0C1H); buf[1] := CHR(0A1H);
  CheckIsValid("IsValid: Overlong ASCII", buf, 2, FALSE);

  (* Invalid: incomplete 3-byte sequence *)
  buf[0] := CHR(0E2H); buf[1] := CHR(082H);
  CheckIsValid("IsValid: Incomplete 3-byte", buf, 2, FALSE);

  (* Invalid: incomplete 4-byte sequence *)
  buf[0] := CHR(0F0H); buf[1] := CHR(09FH); buf[2] := CHR(098H);
  CheckIsValid("IsValid: Incomplete 4-byte", buf, 3, FALSE);

  (* Valid: mixed valid sequence *)
  buf[0] := CHR(65); (* 'A' *)
  buf[1] := CHR(0C2H); buf[2] := CHR(0A2H); (* ¢ *)
  buf[3] := CHR(0E2H); buf[4] := CHR(082H); buf[5] := CHR(0ACH); (* € *)
  CheckIsValid("IsValid: Mixed valid", buf, 6, TRUE);
END TestIsValid;

PROCEDURE TestCodePointToUTF8;
BEGIN
  (* TODO: Implement test cases for UTF8.CodePointToUTF8 *)
END TestCodePointToUTF8;

PROCEDURE TestSkipBOM;
BEGIN
  (* TODO: Implement test cases for UTF8.SkipBOM *)
END TestSkipBOM;

PROCEDURE TestNextCodePoint;
BEGIN
  (* TODO: Implement test cases for UTF8.NextCodePoint *)
END TestNextCodePoint;

PROCEDURE TestPrevCodePoint;
BEGIN
  (* TODO: Implement test cases for UTF8.PrevCodePoint *)
END TestPrevCodePoint;

BEGIN
  TestCharLen;
  TestInvalidCharLen;
  TestIsValid;
  TestCodePointToUTF8;
  TestSkipBOM;
  TestNextCodePoint;
  TestPrevCodePoint;
END TestUTF8.