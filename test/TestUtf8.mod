MODULE TestUtf8;

IMPORT Utf8;
IMPORT StrIO;
IMPORT WholeStr;
IMPORT SYSTEM;

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
  IF Utf8.CharLen(CHR(0)) # 1 THEN pass := FALSE END;
  IF Utf8.CharLen(CHR(65)) # 1 THEN pass := FALSE END;  (* 'A' *)

  (* 2-byte sequence: 0xC2 (valid start of 2-byte UTF-8) *)
  IF Utf8.CharLen(CHR(0C2H)) # 2 THEN pass := FALSE END;

  (* 3-byte sequence: 0xE0 (valid start of 3-byte UTF-8) *)
  IF Utf8.CharLen(CHR(0E0H)) # 3 THEN pass := FALSE END;

  (* 4-byte sequence: 0xF0 (valid start of 4-byte UTF-8) *)
  IF Utf8.CharLen(CHR(0F0H)) # 4 THEN pass := FALSE END;

  (* Invalid first byte: 0xFF (should raise exception, but here we just check it doesn't return a valid length) *)
  (* This may raise an exception depending on implementation, so we skip this in a simple test *)
  WriteResult("TestCharLen", pass);
END TestCharLen;

PROCEDURE TestInvalidCharLen;
(* Test with invalid characters - exception expected. *)
VAR
  pass: BOOLEAN;
BEGIN
  pass := TRUE;
  (* Invalid first byte: (should raise exception) *)
  IF Utf8.CharLen(CHR(0FFH)) # 0 THEN pass := FALSE END;
  WriteResult("TestInvalidCharLen", pass);
END TestInvalidCharLen;

PROCEDURE CheckIsValid(testName: ARRAY OF CHAR; buf: ARRAY OF CHAR; len: CARDINAL; expected: BOOLEAN);
VAR
  pass: BOOLEAN;
BEGIN
  pass := Utf8.IsValid(buf, len) = expected;
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

PROCEDURE CheckEncode(testName: ARRAY OF CHAR; codePoint: CARDINAL; expected: ARRAY OF CHAR; expectedLen: CARDINAL; expectedResult: BOOLEAN);
VAR
  buf: ARRAY [0..7] OF CHAR;
  bytesWritten: CARDINAL;
  pass, result: BOOLEAN;
  i: CARDINAL;
BEGIN
  result := Utf8.Encode(codePoint, buf, 0, bytesWritten);
  pass := (result = expectedResult) AND (bytesWritten = expectedLen);
  IF pass AND expectedResult THEN
    FOR i := 0 TO expectedLen-1 DO
      IF buf[i] # expected[i] THEN pass := FALSE; END;
    END;
  END;
  WriteResult(testName, pass);
END CheckEncode;

PROCEDURE TestEncode;
VAR
  buf: ARRAY [0..2] OF CHAR;
  bytesWritten: CARDINAL;
  result: BOOLEAN;
BEGIN
  (* 1-byte ASCII: U+0041 'A' *)
  CheckEncode("Encode: ASCII A", 65, "A", 1, TRUE);

  (* 2-byte: U+00A2 (¢) = C2 A2 *)
  CheckEncode("Encode: U+00A2", 162, CHR(0C2H) + CHR(0A2H), 2, TRUE);

  (* 3-byte: U+20AC (€) = E2 82 AC *)
  CheckEncode("Encode: U+20AC", 8364, CHR(0E2H) + CHR(082H) + CHR(0ACH), 3, TRUE);

  (* 4-byte: U+1F600 (😀) = F0 9F 98 80 *)
  CheckEncode("Encode: U+1F600", 128512, CHR(0F0H) + CHR(09FH) + CHR(098H) + CHR(080H), 4, TRUE);

  (* Invalid: code point > U+10FFFF *)
  CheckEncode("Encode: Invalid > U+10FFFF", 1114112, "", 0, FALSE);

  (* Invalid: buffer too small for 4-byte sequence *)
  result := Utf8.Encode(128512, buf, 0, bytesWritten);
  WriteResult("Encode: Buffer too small", (result = FALSE) AND (bytesWritten = 0));
 
END TestEncode;

PROCEDURE TestHasBOM;
VAR
  buf: ARRAY [0..7] OF CHAR;
BEGIN
  (* Valid BOM: EF BB BF *)
  buf[0] := CHR(0EFH); buf[1] := CHR(0BBH); buf[2] := CHR(0BFH);
  WriteResult("HasBOM: Valid BOM", Utf8.HasBOM(buf, 3));

  (* Valid BOM with extra data *)
  buf[3] := CHR(65); (* 'A' *)
  WriteResult("HasBOM: BOM with extra", Utf8.HasBOM(buf, 4));

  (* Not enough bytes for BOM *)
  buf[0] := CHR(0EFH); buf[1] := CHR(0BBH);
  WriteResult("HasBOM: Too short", NOT Utf8.HasBOM(buf, 2));

  (* Wrong BOM sequence *)
  buf[0] := CHR(0EFH); buf[1] := CHR(0BBH); buf[2] := CHR(0BCH);
  WriteResult("HasBOM: Wrong last byte", NOT Utf8.HasBOM(buf, 3));

  (* No BOM, ASCII data *)
  buf[0] := CHR(65); buf[1] := CHR(66); buf[2] := CHR(67);
  WriteResult("HasBOM: ASCII", NOT Utf8.HasBOM(buf, 3));
END TestHasBOM;

PROCEDURE CheckNextChar(testName: ARRAY OF CHAR; buf: ARRAY OF CHAR; len: CARDINAL; expected: ARRAY OF CARDINAL; expectedCount: CARDINAL; expectedResult: BOOLEAN);
VAR
  index, i: CARDINAL;
  codePoint: CARDINAL;
  pass, result: BOOLEAN;
BEGIN
  index := 0;
  pass := TRUE;
  FOR i := 0 TO expectedCount-1 DO
    result := Utf8.NextChar(buf, index, codePoint);
    IF NOT result OR (codePoint # expected[i]) THEN
      pass := FALSE;
      RETURN;
    END;
  END;
  (* After all expected codepoints, NextChar should return FALSE *)
  IF Utf8.NextChar(buf, index, codePoint) THEN
    pass := FALSE;
  END;
  IF pass # expectedResult THEN
    pass := FALSE;
  END;
  WriteResult(testName, pass);
END CheckNextChar;

PROCEDURE TestNextChar;
VAR
  buf1: ARRAY [0..0] OF CHAR;
  buf2: ARRAY [0..1] OF CHAR;
  buf3: ARRAY [0..2] OF CHAR;
  buf4: ARRAY [0..3] OF CHAR;
  bufMix: ARRAY [0..5] OF CHAR;
  expected: ARRAY [0..3] OF CARDINAL;
BEGIN
  (* 1-byte ASCII: 'A' *)
  buf1[0] := CHR(65);
  expected[0] := 65;
  CheckNextChar("NextChar: ASCII A", buf1, 1, expected, 1, TRUE);

  (* 2-byte: U+00A2 (¢) = C2 A2 *)
  buf2[0] := CHR(0C2H); buf2[1] := CHR(0A2H);
  expected[0] := 162;
  CheckNextChar("NextChar: U+00A2", buf2, 2, expected, 1, TRUE);

  (* 3-byte: U+20AC (€) = E2 82 AC *)
  buf3[0] := CHR(0E2H); buf3[1] := CHR(082H); buf3[2] := CHR(0ACH);
  expected[0] := 8364;
  CheckNextChar("NextChar: U+20AC", buf3, 3, expected, 1, TRUE);

  (* 4-byte: U+1F600 (😀) = F0 9F 98 80 *)
  buf4[0] := CHR(0F0H); buf4[1] := CHR(09FH); buf4[2] := CHR(098H); buf4[3] := CHR(080H);
  expected[0] := 128512;
  CheckNextChar("NextChar: U+1F600", buf4, 4, expected, 1, TRUE);

  (* Mixed: 'A', ¢, € *)
  bufMix[0] := CHR(65); (* 'A' *)
  bufMix[1] := CHR(0C2H); bufMix[2] := CHR(0A2H); (* ¢ *)
  bufMix[3] := CHR(0E2H); bufMix[4] := CHR(082H); bufMix[5] := CHR(0ACH); (* € *)
  expected[0] := 65; expected[1] := 162; expected[2] := 8364;
  CheckNextChar("NextChar: Mixed", bufMix, 6, expected, 3, TRUE);

  (* Invalid: incomplete 2-byte sequence *)
  buf1[0] := CHR(0C2H);
  CheckNextChar("NextChar: Incomplete 2-byte", buf1, 1, expected, 0, FALSE);

  (* Invalid: bad continuation byte *)
  buf2[0] := CHR(0C2H); buf2[1] := CHR(0FFH);
  CheckNextChar("NextChar: Bad continuation", buf2, 2, expected, 0, FALSE);

  (* Invalid: empty buffer *)
  CheckNextChar("NextChar: Empty", buf1, 0, expected, 0, FALSE);
END TestNextChar;

PROCEDURE CheckPrevChar(testName: ARRAY OF CHAR; buf: ARRAY OF CHAR; len: CARDINAL; expected: ARRAY OF CARDINAL; expectedCount: CARDINAL; expectedResult: BOOLEAN);
VAR
  index, i: CARDINAL;
  codePoint: CARDINAL;
  pass, result: BOOLEAN;
BEGIN
  index := len;
  pass := TRUE;
  FOR i := expectedCount TO 1 BY -1 DO
    result := Utf8.PrevChar(buf, index, codePoint);
    IF NOT result OR (codePoint # expected[i-1]) THEN
      pass := FALSE;
      RETURN;
    END;
  END;
  (* After all expected codepoints, PrevChar should return FALSE *)
  IF Utf8.PrevChar(buf, index, codePoint) THEN
    pass := FALSE;
  END;
  IF pass # expectedResult THEN
    pass := FALSE;
  END;
  WriteResult(testName, pass);
END CheckPrevChar;

PROCEDURE TestPrevChar;
VAR
  buf1: ARRAY [0..0] OF CHAR;
  buf2: ARRAY [0..1] OF CHAR;
  buf3: ARRAY [0..2] OF CHAR;
  buf4: ARRAY [0..3] OF CHAR;
  bufMix: ARRAY [0..5] OF CHAR;
  bufInvalid: ARRAY [0..2] OF CHAR;
  expected: ARRAY [0..3] OF CARDINAL;
  index: CARDINAL;
  codePoint: CARDINAL;
BEGIN
  (* 1-byte ASCII: 'A' *)
  buf1[0] := CHR(65);
  expected[0] := 65;
  CheckPrevChar("PrevChar: ASCII A", buf1, 1, expected, 1, TRUE);

  (* 2-byte: U+00A2 (¢) = C2 A2 *)
  buf2[0] := CHR(0C2H); buf2[1] := CHR(0A2H);
  expected[0] := 162;
  CheckPrevChar("PrevChar: U+00A2", buf2, 2, expected, 1, TRUE);

  (* 3-byte: U+20AC (€) = E2 82 AC *)
  buf3[0] := CHR(0E2H); buf3[1] := CHR(082H); buf3[2] := CHR(0ACH);
  expected[0] := 8364;
  CheckPrevChar("PrevChar: U+20AC", buf3, 3, expected, 1, TRUE);

  (* 4-byte: U+1F600 (😀) = F0 9F 98 80 *)
  buf4[0] := CHR(0F0H); buf4[1] := CHR(09FH); buf4[2] := CHR(098H); buf4[3] := CHR(080H);
  expected[0] := 128512;
  CheckPrevChar("PrevChar: U+1F600", buf4, 4, expected, 1, TRUE);

  (* Mixed: 'A', ¢, € *)
  bufMix[0] := CHR(65); (* 'A' *)
  bufMix[1] := CHR(0C2H); bufMix[2] := CHR(0A2H); (* ¢ *)
  bufMix[3] := CHR(0E2H); bufMix[4] := CHR(082H); bufMix[5] := CHR(0ACH); (* € *)
  expected[0] := 65; expected[1] := 162; expected[2] := 8364;
  CheckPrevChar("PrevChar: Mixed", bufMix, 6, expected, 3, TRUE);

  (* Invalid: incomplete 2-byte sequence - Direct test *)
  buf1[0] := CHR(0C2H);
  index := 1;
  WriteResult("PrevChar: Incomplete 2-byte", NOT Utf8.PrevChar(buf1, index, codePoint));

  (* Invalid: bad continuation byte - Direct test *)
  bufInvalid[0] := CHR(0E2H); (* Start of 3-byte sequence *)
  bufInvalid[1] := CHR(082H); (* Good continuation *)
  bufInvalid[2] := CHR(0FFH); (* Bad continuation - not 10xxxxxx *)
  index := 3;
  WriteResult("PrevChar: Bad continuation", NOT Utf8.PrevChar(bufInvalid, index, codePoint));

  (* Invalid: empty buffer - Direct test *)
  index := 0;
  WriteResult("PrevChar: Empty", NOT Utf8.PrevChar(buf1, index, codePoint));
END TestPrevChar;

BEGIN
  TestCharLen;
  TestInvalidCharLen;
  TestIsValid;
  TestEncode;
  TestHasBOM;
  TestNextChar;
  TestPrevChar;
END TestUtf8.