IMPLEMENTATION MODULE UTF8;

FROM SYSTEM IMPORT BITSET32;

CONST
  BOM0 = CHR(0EFH);
  BOM1 = CHR(0BBH);
  BOM2 = CHR(0BFH);

PROCEDURE IsValidUTF8(buf: ARRAY OF CHAR; len: CARDINAL): BOOLEAN;
VAR
  i: CARDINAL;
  c: CHAR;
  mask: BITSET32;
BEGIN
  (* i := 0;
  WHILE i < len DO
    c := buf[i];
    mask := BITSET32{ORD(buf[i+1])};
    
    IF ORD(c) < 128 THEN
      INC(i);
    ELSIF (ORD(c) >= 192) AND (ORD(c) <= 223) THEN
      (* 2-byte sequence *)
      IF (i+1 >= len) OR (mask * BITSET32{192} # BITSET32{128}) THEN
        RETURN FALSE;
      END;
      i := i + 2;
    ELSE
      RETURN FALSE;
    END;
  END; *)
  RETURN TRUE;
END IsValidUTF8;


PROCEDURE SkipBOM(VAR buf: ARRAY OF CHAR; VAR len: CARDINAL);
VAR
  i: CARDINAL;
BEGIN
  (* IF len >= 3 THEN
    IF (buf[0] = BOM0) AND (buf[1] = BOM1) AND (buf[2] = BOM2) THEN
      (* Shift buffer left by 3 bytes *)
      FOR i := 0 TO len-4 DO
        buf[i] := buf[i+3];
      END;
      len := len - 3;
    END;
  END; *)
END SkipBOM;

END UTF8.