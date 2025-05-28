IMPLEMENTATION MODULE UTF8;

FROM StrIO IMPORT WriteString, WriteLn;
FROM WholeStr IMPORT CardToStr, IntToStr;
FROM SYSTEM IMPORT SHIFT, CAST;

(* This module provides utilities for handling UTF-8 encoded text,
   including checking validity and skipping the Byte Order Mark (BOM). *)

(* UTF-8 encoding constants for Byte Order Mark (BOM) *)
(* BOM is used to indicate that the text is encoded in UTF-8 *)
(* BOM: EF BB BF in hexadecimal, which corresponds to 0xEF, 0xBB, 0xBF in decimal *)

CONST
  Bom0 = CHR(0EFH);
  Bom1 = CHR(0BBH);
  Bom2 = CHR(0BFH);

  Mask1B = BITSET{7};         (*0b00000000*)
  Mask2B = BITSET{7,6,5};     (*0b11000000*)
  Mask3B = BITSET{7,6,5,4};   (*0b11110000*)
  Mask4B = BITSET{7,6,5,4,3}; (*0b11111000*)

PROCEDURE UTF8CharLen(firstByte: CHAR): CARDINAL;
VAR
  b: BITSET;
  ord: CARDINAL;
BEGIN

  (* Masks for UTF-8 character lengths *)
  
  (* Check the first byte to determine the length of the UTF-8 character *)
  (* 1-byte: 0xxxxxxx *)
  (* 2-byte: 110xxxxx 10xxxxxx *)
  (* 3-byte: 1110xxxx 10xxxxxx 10xxxxxx *)
  (* 4-byte: 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx *)
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

(* Determine the length of a UTF-8 character based on the first byte *)

PROCEDURE IsValidUTF8(buf: ARRAY OF CHAR; len: CARDINAL): BOOLEAN;
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
VAR
  c: CARDINAL;
  bits: BITSET;
  mask: BITSET;
  s: ARRAY [0..3] OF CHAR; (* Temporary buffer for conversion *)
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

END UTF8.