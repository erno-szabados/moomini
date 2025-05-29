IMPLEMENTATION MODULE UTF8;

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

PROCEDURE UTF8CharLen(firstByte: CHAR): CARDINAL;
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
    (* Invalid first byte for UTF-8 character *)
    RETURN 0;
  END;
END UTF8CharLen;


PROCEDURE IsValidUTF8(buf: ARRAY OF CHAR; len: CARDINAL): BOOLEAN;
(* Determine if passed buffer contains a valid UTF-8 sequence. *)
VAR
  i: CARDINAL;
  c: CHAR;
  b: BITSET;
BEGIN
  i := 0;
  WHILE i < len DO
    c := buf[i];
    b := VAL(BITSET, c);

    IF (b * Mask1B) = {} THEN
      (* 1-byte ASCII: 0xxxxxxx *)
      INC(i);
    ELSIF (b * {7,6}) = {7,6} THEN
      IF (b * {5}) = {} THEN
        (* 2-byte sequence: 110xxxxx 10xxxxxx *)
        IF i+1 >= len THEN RETURN FALSE END;
        IF (VAL(BITSET, buf[i+1]) * {7,6}) # {7} THEN RETURN FALSE END;
        (* Overlong encoding check *)
        IF (ORD(c) = 0C0H) OR (ORD(c) = 0C1H) THEN RETURN FALSE END;
        INC(i, 2);
      ELSE
        (* 3-byte or 4-byte sequence *)
        IF (b * {5,4}) = {5} THEN
          (* 3-byte sequence: 1110xxxx 10xxxxxx 10xxxxxx *)
          IF i+2 >= len THEN RETURN FALSE END;
          IF (VAL(BITSET, buf[i+1]) * {7,6}) # {7} THEN RETURN FALSE END;
          IF (VAL(BITSET, buf[i+2]) * {7,6}) # {7} THEN RETURN FALSE END;
          (* Overlong and surrogate checks *)
          IF (ORD(c) = 0E0H) & ((ORD(buf[i+1]) * 0E0H) = 080H) THEN RETURN FALSE END;
          IF (ORD(c) = 0EDH) & ((ORD(buf[i+1]) * 0E0H) = 0A0H) THEN RETURN FALSE END;
          INC(i, 3);
        ELSIF (b * {5,4}) = {5,4} THEN
          (* 4-byte sequence: 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx *)
          IF i+3 >= len THEN RETURN FALSE END;
          IF (VAL(BITSET, buf[i+1]) * {7,6}) # {7} THEN RETURN FALSE END;
          IF (VAL(BITSET, buf[i+2]) * {7,6}) # {7} THEN RETURN FALSE END;
          IF (VAL(BITSET, buf[i+3]) * {7,6}) # {7} THEN RETURN FALSE END;
          (* Overlong and range checks *)
          IF (ORD(c) = 0F0H) & ((ORD(buf[i+1]) * 0F0H) = 080H) THEN RETURN FALSE END;
          IF (ORD(c) > 0F4H) OR ((ORD(c) = 0F4H) & (ORD(buf[i+1]) > 08FH)) THEN RETURN FALSE END;
          INC(i, 4);
        ELSE
          RETURN FALSE;
        END;
      END;
    ELSE
      RETURN FALSE;
    END;
  END; 
  RETURN TRUE;
END IsValidUTF8;

PROCEDURE CodePointToUTF8(codePoint: CARDINAL; VAR buffer: ARRAY OF CHAR; VAR bytesWritten: CARDINAL): BOOLEAN;
(* Convert the passed UTF-8 codepoint to a byte sequence. *)
VAR
  c: CARDINAL;
  bits: BITSET;
  mask: BITSET;
BEGIN
  mask := BITSET{0..5}; (* 3FH = 0011 1111B *)
  IF codePoint <= 7FH THEN
    (* 1-byte ASCII *)
    IF HIGH(buffer) < 1 THEN bytesWritten := 0; RETURN FALSE; END;
    buffer[0] := CHR(codePoint);
    bytesWritten := 1;
    RETURN TRUE;
  ELSIF codePoint <= 7FFH THEN
    (* 2-byte sequence *)
    IF HIGH(buffer) < 2 THEN bytesWritten := 0; RETURN FALSE; END;
    bits := CAST(BITSET, codePoint);
    buffer[0] := CHR(0C0H + VAL(CARDINAL, SHIFT(bits, -6)));
    buffer[1] := CHR(080H + VAL(CARDINAL, bits * mask)); 
    bytesWritten := 2;
    RETURN TRUE;
  ELSIF codePoint <= 0FFFFH THEN
    (* 3-byte sequence *)
    IF HIGH(buffer) < 3 THEN 
      bytesWritten := 0; 
      RETURN FALSE; 
    END;
    bits := CAST(BITSET, codePoint);
    buffer[0] := CHR(0E0H + VAL(CARDINAL, SHIFT(bits, -12)));
    buffer[1] := CHR(080H + VAL(CARDINAL, (SHIFT(bits, -6)) * mask));  
    buffer[2] := CHR(080H + VAL(CARDINAL, bits * mask));               
    bytesWritten := 3;
    RETURN TRUE;
  ELSIF codePoint <= 10FFFFH THEN
    (* 4-byte sequence *)
    IF HIGH(buffer) < 4 THEN 
      bytesWritten := 0; 
    RETURN FALSE; 
    END;
    bits := CAST(BITSET, codePoint);
    buffer[0] := CHR(0F0H + VAL(CARDINAL, SHIFT(bits, -18)));
    buffer[1] := CHR(080H + VAL(CARDINAL, (SHIFT(bits, -12)) * mask)); 
    buffer[2] := CHR(080H + VAL(CARDINAL, (SHIFT(bits, -6)) * mask));  
    buffer[3] := CHR(080H + VAL(CARDINAL, bits * mask));               
    bytesWritten := 4;
    RETURN TRUE;
  ELSE
    (* Invalid code point *)
    bytesWritten := 0;
    RETURN FALSE;
  END;
END CodePointToUTF8;


PROCEDURE SkipBOM(VAR buf: ARRAY OF CHAR; VAR len: CARDINAL);
(* Modify the passed buffer by copying it to skip the UTF-8 BOM. *)
VAR
  i: CARDINAL;
BEGIN
  IF len >= 3 THEN
    IF (buf[0] = Bom0) AND (buf[1] = Bom1) AND (buf[2] = Bom2) THEN
      (* Shift buffer left by 3 bytes *)
      FOR i := 0 TO len-4 DO
        buf[i] := buf[i+3];
      END;
      len := len - 3;
    END;
  END;
END SkipBOM;


PROCEDURE NextCodePoint(VAR byteArray: ARRAY OF CHAR; VAR index: CARDINAL; VAR codePoint: CARDINAL): BOOLEAN;
(* Reads the next UTF-8 character (code point) from a byte array, advances the index, and returns the code point.  *)
(* Returns FALSE if the end of the array is reached or an invalid sequence is encountered. *)
VAR
  first: CHAR;
  len: CARDINAL;
  i: CARDINAL;
  b: BITSET;
  cp: CARDINAL;
BEGIN
  IF index > HIGH(byteArray) THEN RETURN FALSE END;
  IF index = HIGH(byteArray) THEN RETURN FALSE END;
  first := byteArray[index];
  len := UTF8CharLen(first);
  IF len = 0 THEN RETURN FALSE END;
  IF index + len > HIGH(byteArray) + 1 THEN RETURN FALSE END;

  (* Check continuation bytes *)
  FOR i := 1 TO len-1 DO
    b := VAL(BITSET, byteArray[index + i]);
    IF (b * BITSET{7,6}) # BITSET{7} THEN RETURN FALSE END;
  END;

  (* Decode code point *)
  IF len = 1 THEN
    cp := ORD(first);
  ELSIF len = 2 THEN
    cp := ((ORD(first) - 0C0H) * 64) + (ORD(byteArray[index+1]) - 080H);
  ELSIF len = 3 THEN
    cp := ((ORD(first) - 0E0H) * 4096) + ((ORD(byteArray[index+1]) - 080H) * 64) + (ORD(byteArray[index+2]) - 080H);
  ELSIF len = 4 THEN
    cp := ((ORD(first) - 0F0H) * 262144) + ((ORD(byteArray[index+1]) - 080H) * 4096) +
          ((ORD(byteArray[index+2]) - 080H) * 64) + (ORD(byteArray[index+3]) - 080H);
  ELSE
    RETURN FALSE;
  END;

  codePoint := cp;
  INC(index, len);
  RETURN TRUE;
END NextCodePoint;

PROCEDURE PrevCodePoint(VAR byteArray: ARRAY OF CHAR; VAR index: CARDINAL; VAR codePoint: CARDINAL): BOOLEAN;
(* Reads the previous UTF-8 character (code point) from a byte array, moves index back, and returns the code point. *)
VAR
  start, i, len: CARDINAL;
  b: BITSET;
  first: CHAR;
  cp: CARDINAL;
BEGIN
  IF index = 0 THEN RETURN FALSE END;
  start := index;

  (* Move back to the first byte of the previous code point *)
  i := start - 1;
  (* At most 3 continuation bytes before a leading byte *)
  WHILE (i > 0) & ((VAL(BITSET, byteArray[i]) * BITSET{7,6}) = BITSET{7}) DO
    DEC(i);
  END;

  (* Now i should point to the first byte of the code point *)
  first := byteArray[i];
  len := UTF8CharLen(first);

  IF (len = 0) OR (i + len > start) OR (i + len > HIGH(byteArray) + 1) OR (i + len <> start) THEN
    RETURN FALSE
  END;

  (* Check continuation bytes *)
  FOR start := 1 TO len-1 DO
    b := VAL(BITSET, byteArray[i + start]);
    IF (b * BITSET{7,6}) # BITSET{7} THEN RETURN FALSE END;
  END;

  (* Decode code point *)
  IF len = 1 THEN
    cp := ORD(first);
  ELSIF len = 2 THEN
    cp := ((ORD(first) - 0C0H) * 64) + (ORD(byteArray[i+1]) - 080H);
  ELSIF len = 3 THEN
    cp := ((ORD(first) - 0E0H) * 4096) + ((ORD(byteArray[i+1]) - 080H) * 64) + (ORD(byteArray[i+2]) - 080H);
  ELSIF len = 4 THEN
    cp := ((ORD(first) - 0F0H) * 262144) + ((ORD(byteArray[i+1]) - 080H) * 4096) +
          ((ORD(byteArray[i+2]) - 080H) * 64) + (ORD(byteArray[i+3]) - 080H);
  ELSE
    RETURN FALSE;
  END;

  codePoint := cp;
  index := i;
  RETURN TRUE;
END PrevCodePoint;

END UTF8.