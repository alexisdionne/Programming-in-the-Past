IDENTIFICATION DIVISION.
PROGRAM-ID. CaesarCipher.
AUTHOR. Alexis Dionne.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 phrase             PIC A(50) value "The quick brown fox jumped over the lazy dog".
01 shift              PIC 99 value 12.
01 shifted            PIC 99 value 0.
01 max-shift          PIC 99.
01 substring          PIC 99 value 27.
01 lowercase-letters  PIC A(26) value "abcdefghijklmnopqrstuvwxyz".
01 uppercase-letters  PIC A(26) value "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
01 cipher-key         PIC A(26) value "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
01 cnt                PIC 99 value 1.
  
PROCEDURE DIVISION.
BEGIN.
  INSPECT phrase CONVERTING lowercase-letters TO uppercase-letters.
  PERFORM make-key UNTIL cnt=27.
  DISPLAY "KEY: "cipher-key.
  PERFORM encrypt.
  PERFORM decrypt.
  COMPUTE cnt = shift.
  DISPLAY "Enter the max-shift you'd like solved: ".
  ACCEPT max-shift.
  COMPUTE cnt = max-shift.
  PERFORM solve UNTIL cnt=0.
  STOP RUN
  .

make-key.
  COMPUTE shifted = cnt + shift.
  IF shifted IS GREATER THAN 26 THEN
    COMPUTE shifted = (cnt + shift) - 26
    INSPECT cipher-key(cnt:substring) REPLACING ALL cipher-key(cnt:1) BY uppercase-letters(shifted:1)
  ELSE
    *>DISPLAY "to be changed to: "uppercase-letters(shifted:1)
    *>DISPLAY "ELSE"
    INSPECT cipher-key(cnt:substring) REPLACING ALL cipher-key(cnt:1) BY uppercase-letters(shifted:1)
  END-IF.
  ADD 1 to cnt.
  SUBTRACT 1 FROM substring.
  .
  
encrypt.
  INSPECT phrase CONVERTING uppercase-letters TO cipher-key.
  DISPLAY "Encrypted: "phrase.
  .
  
decrypt.
  INSPECT phrase CONVERTING cipher-key TO uppercase-letters.
  DISPLAY "Decrypted: "phrase.
  .
  
solve.
  
  SUBTRACT 1 FROM cnt.
  .
END PROGRAM CaesarCipher.
