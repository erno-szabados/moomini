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

PROCEDURE TestIsValid;
BEGIN
  (* TODO: Implement test cases for UTF8.IsValid *)
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