IMPLEMENTATION MODULE Utf8;

(* This module implements UTF-8 encoding and utility procedures. *)

FROM StrIO IMPORT WriteString, WriteLn;
FROM WholeStr IMPORT CardToStr, IntToStr;
FROM SYSTEM IMPORT SHIFT, CAST;

CONST
  Bom0 = CHR(0EFH);
  Bom1 = CHR(0BBH);
  Bom2 = CHR(0BFH);

  Mask1B = BITSET{7};         (*0b00000000*)
  Mask2B = BITSET{7,6,5};     (*0b11000000*)
  Mask3B = BITSET{7,6,5,4};   (*0b11110000*)
  Mask4B = BITSET{7,6,5,4,3}; (*0b11111000*)


PROCEDURE CharLen(firstByte: CHAR): CARDINAL;
(* Determine the length of a UTF-8 character based on the first byte *)
VAR
  b: BITSET;
  ord: CARDINAL;
BEGIN
  b := VAL(BITSET, firstByte);
  IF (b * Mask1B) = {} THEN
    RETURN 1;
  ELSIF (b * Mask2B) = BITSET{7,6} THEN
    RETURN 2;
  ELSIF (b * Mask3B) = BITSET{7,6,5} THEN
    RETURN 3;
  ELSIF (b * Mask4B) = BITSET{7,6,5,4} THEN
    RETURN 4;
  ELSE
    RETURN 0;
  END;
END CharLen;

(* Helper procedure to validate a 2-byte UTF-8 sequence *)
PROCEDURE IsValid2ByteSequence(buf: ARRAY OF CHAR; i: CARDINAL): BOOLEAN;
VAR firstByte: CHAR;
BEGIN
  firstByte := buf[i];
  (* Check continuation byte: must be 10xxxxxx *)
  IF (VAL(BITSET, buf[i+1]) * {7,6}) # {7} THEN RETURN FALSE END;
  (* Overlong encoding check: C0, C1 are invalid starts for 2-byte sequences *)
  IF (ORD(firstByte) = 0C0H) OR (ORD(firstByte) = 0C1H) THEN RETURN FALSE END;
  RETURN TRUE;
END IsValid2ByteSequence;

(* Helper procedure to validate a 3-byte UTF-8 sequence *)
PROCEDURE IsValid3ByteSequence(buf: ARRAY OF CHAR; i: CARDINAL): BOOLEAN;
VAR firstByte: CHAR;
BEGIN
  firstByte := buf[i];
  (* Check continuation bytes: must be 10xxxxxx *)
  IF (VAL(BITSET, buf[i+1]) * {7,6}) # {7} THEN RETURN FALSE END;
  IF (VAL(BITSET, buf[i+2]) * {7,6}) # {7} THEN RETURN FALSE END;
  (* Overlong and surrogate checks *)
  IF (ORD(firstByte) = 0E0H) & (ORD(buf[i+1]) < 0A0H) THEN RETURN FALSE END; (* E0 80..9F is overlong *)
  IF (ORD(firstByte) = 0EDH) & (ORD(buf[i+1]) >= 0A0H) THEN RETURN FALSE END; (* ED A0..BF are surrogates *)
  RETURN TRUE;
END IsValid3ByteSequence;

(* Helper procedure to validate a 4-byte UTF-8 sequence *)
PROCEDURE IsValid4ByteSequence(buf: ARRAY OF CHAR; i: CARDINAL): BOOLEAN;
VAR firstByte: CHAR;
BEGIN
  firstByte := buf[i];
  (* Check continuation bytes: must be 10xxxxxx *)
  IF (VAL(BITSET, buf[i+1]) * {7,6}) # {7} THEN RETURN FALSE END;
  IF (VAL(BITSET, buf[i+2]) * {7,6}) # {7} THEN RETURN FALSE END;
  IF (VAL(BITSET, buf[i+3]) * {7,6}) # {7} THEN RETURN FALSE END;
  (* Overlong and range checks *)
  IF (ORD(firstByte) = 0F0H) & (ORD(buf[i+1]) < 090H) THEN RETURN FALSE END; (* F0 80..8F is overlong *)
  IF (ORD(firstByte) > 0F4H) OR ((ORD(firstByte) = 0F4H) & (ORD(buf[i+1]) > 08FH)) THEN RETURN FALSE END; (* Code points > U+10FFFF *)
  RETURN TRUE;
END IsValid4ByteSequence;

PROCEDURE IsValid(buf: ARRAY OF CHAR; len: CARDINAL): BOOLEAN;
(* Determine if passed buffer contains a valid UTF-8 sequence. *)
VAR
  i: CARDINAL;
  c: CHAR;
  expectedCharLen: CARDINAL;
BEGIN
  i := 0;
  WHILE i < len DO
    c := buf[i];
    expectedCharLen := CharLen(c);
    IF expectedCharLen = 0 THEN
      (* Invalid first byte, return FALSE *)
      RETURN FALSE;
    END;

    (* Check if buffer has enough bytes for the potential sequence *)
    IF i + expectedCharLen > len THEN RETURN FALSE END;

    (* Validate the sequence based on its expected length *)
    CASE expectedCharLen OF
      1: (* 1-byte sequence: 0xxxxxxx *)
        (* No further checks needed for the byte itself. *)
        INC(i, 1);
    | 2: (* 2-byte sequence: 110xxxxx 10xxxxxx *)
        IF NOT IsValid2ByteSequence(buf, i) THEN RETURN FALSE; END;
        INC(i, 2);
    | 3: (* 3-byte sequence: 1110xxxx 10xxxxxx 10xxxxxx *)
        IF NOT IsValid3ByteSequence(buf, i) THEN RETURN FALSE; END;
        INC(i, 3);
    | 4: (* 4-byte sequence: 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx *)
        IF NOT IsValid4ByteSequence(buf, i) THEN RETURN FALSE; END;
        INC(i, 4);
    (* ELSE case is not needed as CharLen will return 0 on error. *)
    END;
  END;
  RETURN TRUE;
END IsValid;

PROCEDURE Encode(codePoint: CARDINAL; VAR buffer: ARRAY OF CHAR; index: CARDINAL; VAR bytesWritten: CARDINAL): BOOLEAN;
(* Convert the passed UTF-8 codepoint to a byte sequence at buffer[index]. *)
VAR
  c: CARDINAL;
  bits: BITSET;
  mask: BITSET;
BEGIN
  mask := BITSET{0..5}; (* 3FH = 0011 1111B *)
  IF codePoint <= 7FH THEN
    (* 1-byte ASCII *)
    IF HIGH(buffer) < index THEN bytesWritten := 0; RETURN FALSE; END;
    buffer[index] := CHR(codePoint);
    bytesWritten := 1;
    RETURN TRUE;
  ELSIF codePoint <= 7FFH THEN
    (* 2-byte sequence *)
    IF HIGH(buffer) < index+1 THEN bytesWritten := 0; RETURN FALSE; END;
    bits := CAST(BITSET, codePoint);
    buffer[index] := CHR(0C0H + VAL(CARDINAL, SHIFT(bits, -6)));
    buffer[index+1] := CHR(080H + VAL(CARDINAL, bits * mask)); 
    bytesWritten := 2;
    RETURN TRUE;
  ELSIF codePoint <= 0FFFFH THEN
    (* 3-byte sequence *)
    IF HIGH(buffer) < index+2 THEN 
      bytesWritten := 0; 
      RETURN FALSE; 
    END;
    bits := CAST(BITSET, codePoint);
    buffer[index] := CHR(0E0H + VAL(CARDINAL, SHIFT(bits, -12)));
    buffer[index+1] := CHR(080H + VAL(CARDINAL, (SHIFT(bits, -6)) * mask));  
    buffer[index+2] := CHR(080H + VAL(CARDINAL, bits * mask));               
    bytesWritten := 3;
    RETURN TRUE;
  ELSIF codePoint <= 10FFFFH THEN
    (* 4-byte sequence *)
    IF HIGH(buffer) < index+3 THEN 
      bytesWritten := 0; 
      RETURN FALSE; 
    END;
    bits := CAST(BITSET, codePoint);
    buffer[index] := CHR(0F0H + VAL(CARDINAL, SHIFT(bits, -18)));
    buffer[index+1] := CHR(080H + VAL(CARDINAL, (SHIFT(bits, -12)) * mask)); 
    buffer[index+2] := CHR(080H + VAL(CARDINAL, (SHIFT(bits, -6)) * mask));  
    buffer[index+3] := CHR(080H + VAL(CARDINAL, bits * mask));               
    bytesWritten := 4;
    RETURN TRUE;
  ELSE
    (* Invalid code point *)
    bytesWritten := 0;
    RETURN FALSE;
  END;
END Encode;


PROCEDURE HasBOM(buffer: ARRAY OF CHAR; len: CARDINAL): BOOLEAN;
(* Returns TRUE if passed buffer starts with the 3-byte UTF-8 BOM, FALSE otherwise. *)
VAR
  i: CARDINAL;
BEGIN
  IF len >= 3 THEN
    IF (buffer[0] = Bom0) AND (buffer[1] = Bom1) AND (buffer[2] = Bom2) THEN
      RETURN TRUE;
    END;
  END;
  RETURN FALSE;
END HasBOM;


PROCEDURE NextChar(VAR byteArray: ARRAY OF CHAR; VAR index: CARDINAL; VAR codePoint: CARDINAL): BOOLEAN;
(* Reads the next UTF-8 character (code point) from a byte array, advances the index, and returns the code point.  *)
(* Returns FALSE if the end of the array is reached or an invalid sequence is encountered. *)
VAR
  first: CHAR;
  len: CARDINAL;
  cp: CARDINAL;
BEGIN
  (* Check for end of buffer *)
  IF index > HIGH(byteArray) THEN RETURN FALSE END;
  
  (* Get first byte and determine sequence length *)
  first := byteArray[index];
  len := CharLen(first);
  
  (* Early validation *)
  IF len = 0 THEN RETURN FALSE END;
  IF index + len > HIGH(byteArray) + 1 THEN RETURN FALSE END;

  (* Validate sequence based on length *)
  CASE len OF
    1: (* 1-byte ASCII, no additional validation needed *)
      cp := ORD(first);
  | 2: (* 2-byte sequence *)
      IF NOT IsValid2ByteSequence(byteArray, index) THEN RETURN FALSE END;
      cp := ((ORD(first) - 0C0H) * 64) + (ORD(byteArray[index+1]) - 080H);
  | 3: (* 3-byte sequence *)
      IF NOT IsValid3ByteSequence(byteArray, index) THEN RETURN FALSE END;
      cp := ((ORD(first) - 0E0H) * 4096) + ((ORD(byteArray[index+1]) - 080H) * 64) + 
            (ORD(byteArray[index+2]) - 080H);
  | 4: (* 4-byte sequence *)
      IF NOT IsValid4ByteSequence(byteArray, index) THEN RETURN FALSE END;
      cp := ((ORD(first) - 0F0H) * 262144) + ((ORD(byteArray[index+1]) - 080H) * 4096) +
            ((ORD(byteArray[index+2]) - 080H) * 64) + (ORD(byteArray[index+3]) - 080H);
  ELSE
    RETURN FALSE;
  END;

  codePoint := cp;
  INC(index, len);
  RETURN TRUE;
END NextChar;

PROCEDURE PrevChar(VAR byteArray: ARRAY OF CHAR; VAR index: CARDINAL; VAR codePoint: CARDINAL): BOOLEAN;
(* Reads the previous UTF-8 character (code point) from a byte array, retracts the index, and returns the code point.  *)
(* Returns FALSE if the start of the array is reached or an invalid sequence is encountered. *)
VAR
  start, i, len: CARDINAL;
  b: BITSET;
  first: CHAR;
  cp: CARDINAL;
BEGIN
  (* Check for invalid index or empty array *)
  IF (index = 0) OR (HIGH(byteArray) < 0) THEN RETURN FALSE END;
  start := index;

  (* Move back to the first byte of the previous code point *)
  i := start;
  IF i = 0 THEN RETURN FALSE END;
  DEC(i);

  (* At most 3 continuation bytes before a leading byte *)
  WHILE (i > 0) & ((VAL(BITSET, byteArray[i]) * BITSET{7,6}) = BITSET{7}) DO
    DEC(i);
  END;

  (* Check if we found a valid start byte *)
  first := byteArray[i];
  len := CharLen(first);
  IF len = 0 THEN RETURN FALSE END;

  (* Check for incomplete or invalid sequence *)
  IF (i + len > HIGH(byteArray) + 1) OR (i + len <> start) THEN
    RETURN FALSE;
  END;

  (* Validate sequence based on length *)
  CASE len OF
    1: (* 1-byte ASCII, no additional validation needed *)
      cp := ORD(first);
  | 2: (* 2-byte sequence *)
      IF NOT IsValid2ByteSequence(byteArray, i) THEN RETURN FALSE END;
      cp := ((ORD(first) - 0C0H) * 64) + (ORD(byteArray[i+1]) - 080H);
  | 3: (* 3-byte sequence *)
      IF NOT IsValid3ByteSequence(byteArray, i) THEN RETURN FALSE END;
      cp := ((ORD(first) - 0E0H) * 4096) + ((ORD(byteArray[i+1]) - 080H) * 64) + (ORD(byteArray[i+2]) - 080H);
  | 4: (* 4-byte sequence *)
      IF NOT IsValid4ByteSequence(byteArray, i) THEN RETURN FALSE END;
      cp := ((ORD(first) - 0F0H) * 262144) + ((ORD(byteArray[i+1]) - 080H) * 4096) +
            ((ORD(byteArray[i+2]) - 080H) * 64) + (ORD(byteArray[i+3]) - 080H);
  ELSE
    RETURN FALSE;
  END;

  codePoint := cp;
  index := i;
  RETURN TRUE;
END PrevChar;


BEGIN
END Utf8.