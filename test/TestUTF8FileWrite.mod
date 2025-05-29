MODULE TestUTF8FileWrite;

IMPORT StrIO;
IMPORT TextIO;
IMPORT IOChan;
IMPORT StdChans;
IMPORT ChanConsts;
IMPORT StreamFile;
IMPORT WholeStr;
IMPORT UTF8;
FROM SYSTEM IMPORT ADR;

(* Test writing a UTF-8 encoded string to a text file and validating the contents *)

PROCEDURE TestWriteUTF8ToFile;
VAR
  buf : ARRAY [0..63] OF CHAR;
  tmp : ARRAY [0..7] OF CHAR;
  bytes : CARDINAL;
  p : POINTER TO CHAR;
  ok : BOOLEAN;
  i : CARDINAL;
  cid : IOChan.ChanId;
  res : ChanConsts.OpenResults;
BEGIN
  (* Build UTF-8 string: "A é € 💖" *)
  p := ADR(buf[0]);
  ok := UTF8.CodePointToUTF8(ORD('A'), tmp, bytes);
  FOR i := 0 TO bytes-1 DO p^ := tmp[i]; INC(p); END;
  p^ := ' '; INC(p);

  ok := ok & UTF8.CodePointToUTF8(0E9H, tmp, bytes);
  FOR i := 0 TO bytes-1 DO p^ := tmp[i]; INC(p); END;
  p^ := ' '; INC(p);

  ok := ok & UTF8.CodePointToUTF8(020ACH, tmp, bytes);
  FOR i := 0 TO bytes-1 DO p^ := tmp[i]; INC(p); END;
  p^ := ' '; INC(p);

  ok := ok & UTF8.CodePointToUTF8(01F496H, tmp, bytes);
  FOR i := 0 TO bytes-1 DO p^ := tmp[i]; INC(p); END;

  (* Null-terminate *)
  p^ := 0C;

  IF NOT ok THEN
    StrIO.WriteString("UTF-8 encoding failed."); StrIO.WriteLn;
    RETURN;
  END;

  (* Write buffer to file *)
  cid := StdChans.OutChan();
  StreamFile.Open(cid, "test_utf8.txt", ChanConsts.write + ChanConsts.text, res);
  IF res = ChanConsts.opened THEN
    TextIO.WriteString(cid, buf);
    TextIO.WriteLn(cid);
    StreamFile.Close(cid);
    StrIO.WriteString("Wrote UTF-8 text to test_utf8.txt");
    StrIO.WriteLn;
  ELSE
    StrIO.WriteString("Failed to open file for writing."); StrIO.WriteLn;
  END;
END TestWriteUTF8ToFile;

PROCEDURE TestReadAndValidateUTF8File;
VAR
  cid : IOChan.ChanId;
  res : ChanConsts.OpenResults;
  filebuf : ARRAY [0..127] OF CHAR;
  len, i : CARDINAL;
  valid : BOOLEAN;
BEGIN
  (* Open file for reading *)
  cid := StdChans.InChan();
  StreamFile.Open(cid, "test_utf8.txt", ChanConsts.read + ChanConsts.text, res);
  IF res = ChanConsts.opened THEN
    (* Read one line *)
    TextIO.ReadString(cid, filebuf);
    StreamFile.Close(cid);

    (* Calculate string length *)
    i := 0;
    WHILE (i <= HIGH(filebuf)) AND (filebuf[i] # 0C) DO
      INC(i);
    END;
    len := i;

    (* Validate UTF-8 *)
    valid := UTF8.IsValidUTF8(filebuf, len);
    IF valid THEN
      StrIO.WriteString("UTF-8 validation PASS on read file.");
      StrIO.WriteLn;
    ELSE
      StrIO.WriteString("UTF-8 validation FAIL on read file.");
      StrIO.WriteLn;
    END;
  ELSE
    StrIO.WriteString("Failed to open file for reading."); StrIO.WriteLn;
  END;
END TestReadAndValidateUTF8File;

BEGIN
  TestWriteUTF8ToFile;
  TestReadAndValidateUTF8File;
END TestUTF8FileWrite.