IMPLEMENTATION MODULE UTF8;

FROM StrIO IMPORT WriteString, WriteLn;
FROM WholeStr IMPORT CardToStr;

(* This module provides utilities for handling UTF-8 encoded text,
   including checking validity and skipping the Byte Order Mark (BOM). *)

(* UTF-8 encoding constants for Byte Order Mark (BOM) *)
(* BOM is used to indicate that the text is encoded in UTF-8 *)
(* BOM: EF BB BF in hexadecimal, which corresponds to 0xEF, 0xBB, 0xBF in decimal *)

CONST
  BOM0 = CHR(0EFH);
  BOM1 = CHR(0BBH);
  BOM2 = CHR(0BFH);

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

    IF (b * {7}) = {} THEN
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

PROCEDURE SkipBOM(VAR buf: ARRAY OF CHAR; VAR len: CARDINAL);
VAR
  i: CARDINAL;
BEGIN
  IF len >= 3 THEN
    IF (buf[0] = BOM0) AND (buf[1] = BOM1) AND (buf[2] = BOM2) THEN
      (* Shift buffer left by 3 bytes *)
      FOR i := 0 TO len-4 DO
        buf[i] := buf[i+3];
      END;
      len := len - 3;
    END;
  END;
END SkipBOM;

END UTF8.