MODULE TestUTF8;

FROM UTF8 IMPORT IsValidUTF8, SkipBOM, UTF8CharLen, CodePointToUTF8, NextCodePoint, PrevCodePoint;
FROM StrIO IMPORT WriteString, WriteLn;
FROM WholeStr IMPORT IntToStr;

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

PROCEDURE TestUTF8CharLen;
VAR
  b: CHAR;
  len: CARDINAL;
  allPass: BOOLEAN;
  str: ARRAY [0..15] OF CHAR;
BEGIN
  WriteString("TestUTF8CharLen:" ); WriteLn;
  allPass := TRUE;

  (* 1-byte ASCII *)
  b := 'A';
  len := UTF8CharLen(b);
  WriteString("  ASCII ('A'): expected 1, got ");
  IntToStr(len, str); WriteString(str);
  IF len = 1 THEN WriteString(" PASS") ELSE WriteString(" FAIL"); allPass := FALSE; END;
  WriteLn;

  (* 2-byte leading byte: 110xxxxx *)
  b := CHR(0C3H);  (* 11000011 *)
  len := UTF8CharLen(b);
  WriteString("  2-byte (0xC3): expected 2, got ");
  IntToStr(len, str); WriteString(str);
  IF len = 2 THEN WriteString(" PASS") ELSE WriteString(" FAIL"); allPass := FALSE; END;
  WriteLn;

  (* 3-byte leading byte: 1110xxxx *)
  b := CHR(0E2H);  (* 11100010 *)
  len := UTF8CharLen(b);
  WriteString("  3-byte (0xE2): expected 3, got ");
  IntToStr(len, str); WriteString(str);
  IF len = 3 THEN WriteString(" PASS") ELSE WriteString(" FAIL"); allPass := FALSE; END;
  WriteLn;

  (* 4-byte leading byte: 11110xxx *)
  b := CHR(0F0H);  (* 11110000 *)
  len := UTF8CharLen(b);
  WriteString("  4-byte (0xF0): expected 4, got ");
  IntToStr(len, str); WriteString(str);
  IF len = 4 THEN WriteString(" PASS") ELSE WriteString(" FAIL"); allPass := FALSE; END;
  WriteLn;

  (* Continuation byte: 10xxxxxx *)
  b := CHR(080H);  (* 10000000 *)
  len := UTF8CharLen(b);
  WriteString("  Continuation (0x80): expected 0, got ");
  IntToStr(len, str); WriteString(str);
  IF len = 0 THEN WriteString(" PASS") ELSE WriteString(" FAIL"); allPass := FALSE; END;
  WriteLn;

  (* Invalid leading byte: 11111111 *)
  b := CHR(0FFH);
  len := UTF8CharLen(b);
  WriteString("  Invalid (0xFF): expected 0, got ");
  IntToStr(len, str); WriteString(str);
  IF len = 0 THEN WriteString(" PASS") ELSE WriteString(" FAIL"); allPass := FALSE; END;
  WriteLn;

  IF allPass THEN
    WriteString("TestUTF8CharLen: ALL PASS");
  ELSE
    WriteString("TestUTF8CharLen: SOME FAIL");
  END;
  WriteLn;
END TestUTF8CharLen;

PROCEDURE PrintHexBytes(buf: ARRAY OF CHAR; count: CARDINAL);
VAR
  i: CARDINAL;
  str: ARRAY [0..7] OF CHAR;
BEGIN
  IF count = 0 THEN RETURN END;
  IF count > LENGTH(buf) THEN count := LENGTH(buf); END;
  FOR i := 0 TO count-1 DO
    IF i > 0 THEN WriteString(" "); END;
    WriteString("0x");
    IF ORD(buf[i]) < 16 THEN WriteString("0"); END;
    IntToStr(ORD(buf[i]), str); WriteString(str);
  END;
END PrintHexBytes;

PROCEDURE TestCodePointToUTF8;
VAR
  buf: ARRAY [0..7] OF CHAR;
  bytes: CARDINAL;
  str: ARRAY [0..15] OF CHAR;
  ok: BOOLEAN;
  allPass: BOOLEAN;
BEGIN
  WriteString("TestCodePointToUTF8:"); WriteLn;
  allPass := TRUE;

  (* 1-byte ASCII *)
  bytes := 0;
  ok := CodePointToUTF8(ORD('A'), buf, bytes);
  WriteString("  'A' (U+0041): ");
  IF ok & (bytes = 1) & (buf[0] = 'A') THEN
    WriteString("PASS");
  ELSE
    WriteString("FAIL");
    allPass := FALSE;
  END;
  (* WriteString(" ["); PrintHexBytes(buf, bytes); WriteString("]"); WriteLn; *)
  WriteLn;

  (* 2-byte: U+00E9 (é) *)
  bytes := 0;
  ok := CodePointToUTF8(0E9H, buf, bytes);
  WriteString("  U+00E9 (é): ");
  IF ok & (bytes = 2) & (ORD(buf[0]) = 0C3H) & (ORD(buf[1]) = 0A9H) THEN
    WriteString("PASS");
  ELSE
    WriteString("FAIL");
    allPass := FALSE;
  END;
  WriteLn;
  (* WriteString(" ["); PrintHexBytes(buf, bytes); WriteString("]"); WriteLn; *)

  (* 3-byte: U+20AC (€) *)
  bytes := 0;
  ok := CodePointToUTF8(020ACH, buf, bytes);
  WriteString("  U+20AC (€): ");
  IF ok & (bytes = 3) & (ORD(buf[0]) = 0E2H) & (ORD(buf[1]) = 082H) & (ORD(buf[2]) = 0ACH) THEN
    WriteString("PASS");
  ELSE
    WriteString("FAIL");
    allPass := FALSE;
  END;
  (* WriteString(" ["); PrintHexBytes(buf, bytes); WriteString("]"); WriteLn; *)
  WriteLn;
  
  (* 4-byte: U+1F496 (💖) *)
  bytes := 0;
  ok := CodePointToUTF8(01F496H, buf, bytes);
  WriteString("  U+1F496 (💖): ");
  IF ok & (bytes = 4) & (ORD(buf[0]) = 0F0H) & (ORD(buf[1]) = 09FH) & (ORD(buf[2]) = 092H) & (ORD(buf[3]) = 096H) THEN
    WriteString("PASS");
  ELSE
    WriteString("FAIL");
    allPass := FALSE;
  END;
  (* WriteString(" ["); PrintHexBytes(buf, bytes); WriteString("]"); WriteLn; *)
  WriteLn;

  (* Invalid code point: U+110000 *)
  bytes := 0;
  ok := CodePointToUTF8(0110000H, buf, bytes);
  WriteString("  U+110000 (invalid): ");
  IF NOT ok & (bytes = 0) THEN
    WriteString("PASS");
  ELSE
    WriteString("FAIL");
    allPass := FALSE;
  END;
  (* WriteString(" ["); PrintHexBytes(buf, bytes); WriteString("]"); WriteLn; *)
  WriteLn;

  IF allPass THEN
    WriteString("TestCodePointToUTF8: ALL PASS");
  ELSE
    WriteString("TestCodePointToUTF8: SOME FAIL");
  END;
  WriteLn;
END TestCodePointToUTF8;

PROCEDURE TestNextCodePoint;
VAR
  buf: ARRAY [0..7] OF CHAR;
  idx: CARDINAL;
  cp: CARDINAL;
  ok: BOOLEAN;
BEGIN
  WriteString("TestNextCodePoint:"); WriteLn;

  (* 1-byte ASCII *)
  buf[0] := 'A'; buf[1] := 'B'; buf[2] := 0C;
  idx := 0;
  ok := NextCodePoint(buf, idx, cp);
  WriteString("  'A': ");
  IF ok & (cp = ORD('A')) & (idx = 1) THEN WriteString("PASS") ELSE WriteString("FAIL") END;
  WriteLn;

  (* 2-byte UTF-8: é (U+00E9) *)
  buf[0] := CHR(0C3H); buf[1] := CHR(0A9H); buf[2] := 0C;
  idx := 0;
  ok := NextCodePoint(buf, idx, cp);
  WriteString("  é (U+00E9): ");
  IF ok & (cp = 0E9H) & (idx = 2) THEN WriteString("PASS") ELSE WriteString("FAIL") END;
  WriteLn;

  (* 3-byte UTF-8: € (U+20AC) *)
  buf[0] := CHR(0E2H); buf[1] := CHR(082H); buf[2] := CHR(0ACH); buf[3] := 0C;
  idx := 0;
  ok := NextCodePoint(buf, idx, cp);
  WriteString("  € (U+20AC): ");
  IF ok & (cp = 020ACH) & (idx = 3) THEN WriteString("PASS") ELSE WriteString("FAIL") END;
  WriteLn;

  (* 4-byte UTF-8: 💖 (U+1F496) *)
  buf[0] := CHR(0F0H); buf[1] := CHR(09FH); buf[2] := CHR(092H); buf[3] := CHR(096H); buf[4] := 0C;
  idx := 0;
  ok := NextCodePoint(buf, idx, cp);
  WriteString("  💖 (U+1F496): ");
  IF ok & (cp = 01F496H) & (idx = 4) THEN WriteString("PASS") ELSE WriteString("FAIL") END;
  WriteLn;

  (* Invalid sequence: continuation byte as first *)
  buf[0] := CHR(080H); buf[1] := 0C;
  idx := 0;
  ok := NextCodePoint(buf, idx, cp);
  WriteString("  Invalid (continuation as first): ");
  IF NOT ok THEN WriteString("PASS") ELSE WriteString("FAIL") END;
  WriteLn;

  (* End of buffer *)
  buf[0] := 'A'; buf[1] := 0C;
  idx := 2;
  ok := NextCodePoint(buf, idx, cp);
  WriteString("  End of buffer: ");
  IF NOT ok THEN WriteString("PASS") ELSE WriteString("FAIL") END;
  WriteLn;
END TestNextCodePoint;

PROCEDURE TestPrevCodePoint;
VAR
  buf: ARRAY [0..7] OF CHAR;
  idx: CARDINAL;
  cp: CARDINAL;
  ok: BOOLEAN;
BEGIN
  WriteString("TestPrevCodePoint:"); WriteLn;

  (* 1-byte ASCII, move back from index 1 *)
  buf[0] := 'A'; buf[1] := 'B'; buf[2] := 0C;
  idx := 1;
  ok := PrevCodePoint(buf, idx, cp);
  WriteString("  'A' from idx=1: ");
  IF ok & (cp = ORD('A')) & (idx = 0) THEN WriteString("PASS") ELSE WriteString("FAIL") END;
  WriteLn;

  (* 2-byte UTF-8: é (U+00E9), move back from index 2 *)
  buf[0] := CHR(0C3H); buf[1] := CHR(0A9H); buf[2] := 0C;
  idx := 2;
  ok := PrevCodePoint(buf, idx, cp);
  WriteString("  é (U+00E9) from idx=2: ");
  IF ok & (cp = 0E9H) & (idx = 0) THEN WriteString("PASS") ELSE WriteString("FAIL") END;
  WriteLn;

  (* 3-byte UTF-8: € (U+20AC), move back from index 3 *)
  buf[0] := CHR(0E2H); buf[1] := CHR(082H); buf[2] := CHR(0ACH); buf[3] := 0C;
  idx := 3;
  ok := PrevCodePoint(buf, idx, cp);
  WriteString("  € (U+20AC) from idx=3: ");
  IF ok & (cp = 020ACH) & (idx = 0) THEN WriteString("PASS") ELSE WriteString("FAIL") END;
  WriteLn;

  (* 4-byte UTF-8: 💖 (U+1F496), move back from index 4 *)
  buf[0] := CHR(0F0H); buf[1] := CHR(09FH); buf[2] := CHR(092H); buf[3] := CHR(096H); buf[4] := 0C;
  idx := 4;
  ok := PrevCodePoint(buf, idx, cp);
  WriteString("  💖 (U+1F496) from idx=4: ");
  IF ok & (cp = 01F496H) & (idx = 0) THEN WriteString("PASS") ELSE WriteString("FAIL") END;
  WriteLn;

  (* Invalid: index at 0 *)
  buf[0] := 'A'; buf[1] := 0C;
  idx := 0;
  ok := PrevCodePoint(buf, idx, cp);
  WriteString("  Invalid (idx=0): ");
  IF NOT ok THEN WriteString("PASS") ELSE WriteString("FAIL") END;
  WriteLn;

  (* Invalid: index in the middle of a multi-byte char *)
  buf[0] := CHR(0E2H); buf[1] := CHR(082H); buf[2] := CHR(0ACH); buf[3] := 0C;
  idx := 2; (* in the middle of a 3-byte char *)
  ok := PrevCodePoint(buf, idx, cp);
  WriteString("  Invalid (middle of multi-byte): ");
  IF NOT ok THEN WriteString("PASS") ELSE WriteString("FAIL") END;
  WriteLn;
END TestPrevCodePoint;


BEGIN
  TestIsValidUTF8;
  TestSkipBOM;
  TestUTF8CharLen;
  TestCodePointToUTF8;
  TestNextCodePoint;
  TestPrevCodePoint;
END TestUTF8.