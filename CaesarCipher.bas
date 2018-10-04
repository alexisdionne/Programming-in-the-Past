FUNCTION ENCRYPT(text AS STRING, KEY AS INTEGER) AS STRING
  DIM char as integer
  FOR I as integer = 0 TO LEN(text) 
    IF text[I] >= 65 AND text[I] <= 90 THEN   
      char = text[I] + KEY
      IF char > 90 THEN char -= 26
      text[I] = char
    END IF
    IF text[I] >= 97 AND text[I] <= 122 THEN
      char = text[I] + KEY
      IF char > 122 THEN char -= 26
      text[I] = char
    END IF 
  NEXT
  RETURN text
END FUNCTION
 
FUNCTION DECRYPT(text AS STRING, KEY AS INTEGER) AS STRING
  DIM char AS INTEGER
  FOR I AS INTEGER = 0 TO LEN(text) 
    SELECT CASE AS CONST text[I] 
      CASE 65 TO 90    
        char = text[I] - KEY
        IF char < 65 THEN char += 26
        text[I] = char
      CASE 97 TO 122
        char = text[I] - KEY
        IF char < 97 THEN char += 26
        text[I] = char
    END SELECT
  NEXT
  RETURN text
END FUNCTION

SUB SOLVE(text AS STRING, maxShift AS INTEGER)
    FOR I AS INTEGER = maxShift TO 0 STEP -1
        PRINT "CaesarCipher ";I;": ";ENCRYPT (text, I)
        text = DECRYPT (text, I)
    NEXT
END SUB
 
DIM AS STRING phrase = "The quick brown fox jumps over the lazy dog"
DIM AS INTEGER shift
INPUT "Enter a shift amount: "; shift
phrase = ENCRYPT (phrase, shift)
PRINT "Encrypted  : "; phrase   
phrase = DECRYPT (phrase, shift)
PRINT "Decrypted  : "; phrase
PRINT " "
INPUT "Enter a number of shifts to solve "; shift
SOLVE phrase, shift
SLEEP