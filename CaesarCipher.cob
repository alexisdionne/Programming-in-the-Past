IDENTIFICATION DIVISION.
PROGRAM-ID. CaesarCipher.
AUTHOR. Alexis Dionne.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 phrase             PIC A(50) value "The quick brown fox jumps over the lazy dog".
01 shift              PIC 99.
01 shifted            PIC 99 value 0.
01 max-shift          PIC 99.
01 substring          PIC 99 value 27.
01 lowercase-letters  PIC A(26) value "abcdefghijklmnopqrstuvwxyz".
01 uppercase-letters  PIC A(26) value "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
01 cipher-key         PIC A(26) value "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
01 cnt                PIC 99 value 1.
  
PROCEDURE DIVISION.
  INSPECT phrase CONVERTING lowercase-letters TO uppercase-letters.
  PERFORM First-Run.
  PERFORM Solve-Run.
  STOP RUN.
  
First-Run.
  DISPLAY "Please enter your first shift amount: ".
  ACCEPT shift.
  DISPLAY " ".
  PERFORM make-key UNTIL cnt=27.
  PERFORM encrypt.
  DISPLAY "Encrypted: "phrase.
  PERFORM decrypt.
  DISPLAY "Decrypted: "phrase.
  DISPLAY " ".
  .
  
Solve-Run.
  DISPLAY "Enter the number of shifts you'd like solved: ".
  ACCEPT max-shift.
  DISPLAY " ".
  PERFORM solve UNTIL max-shift=0.
  .

make-key.
  COMPUTE shifted = cnt + shift.
  IF shifted IS GREATER THAN 26 THEN
    COMPUTE shifted = (cnt + shift) - 26
    INSPECT cipher-key(cnt:substring) REPLACING ALL cipher-key(cnt:1) BY uppercase-letters(shifted:1)
  ELSE
    INSPECT cipher-key(cnt:substring) REPLACING ALL cipher-key(cnt:1) BY uppercase-letters(shifted:1)
  END-IF.
  ADD 1 to cnt.
  SUBTRACT 1 FROM substring.
  .
  
encrypt.
  INSPECT phrase CONVERTING uppercase-letters TO cipher-key.
  .

decrypt.
  INSPECT phrase CONVERTING cipher-key TO uppercase-letters.
  MOVE uppercase-letters TO cipher-key.
  .

solve.
  MOVE 1 to cnt.
  MOVE 27 to substring.
  MOVE max-shift to shift.
  PERFORM make-key UNTIL cnt=27
  PERFORM encrypt.
  DISPLAY "CaesarCipher "shift": "phrase.
  PERFORM decrypt.
  SUBTRACT 1 FROM max-shift.
  .

END PROGRAM CaesarCipher.
