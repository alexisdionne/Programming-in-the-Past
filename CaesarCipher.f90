! Alexis Dionne
! Programming in the Past - Fortran
! CMPT 331
! 9/14/2018


PROGRAM Caesar_Cypher

  CHARACTER(len = 43) :: str = "The quick brown fox jumps over the lazy dog"
  INTEGER :: key = 3
  INTEGER :: maxShift = 26
  
  str = ENCRYPT(str, key)
  PRINT *, "Encrypted: ", str
  str = DECRYPT(str, key)
  PRINT *, "Decrypted: ", str
  CALL SOLVE(str, maxShift)
  
CONTAINS  

SUBROUTINE SOLVE(strToConvert, maxShift)
  !Iterates over the maximum shift amount by calling ENCRYPT
  INTEGER :: counter
  INTEGER :: maxShift
  CHARACTER(len = *) :: strToConvert
  CHARACTER(len = len(strToConvert)) :: convertedStr
  DO counter = maxShift, 0, -1
    convertedStr = encrypt(strToConvert, counter)
    PRINT *, "Caesar ", counter, ": ", convertedStr
  END DO
END SUBROUTINE

FUNCTION ENCRYPT(strToEncrypt, shiftAmount)
  !Converts characters in a string to numbers, shifts them, and converts them 
  !into the encrypted message
  CHARACTER(1) :: currentChar
  CHARACTER(len = *) :: strToEncrypt
  INTEGER :: shiftAmount
  CHARACTER(len = len(strToEncrypt)) :: encrypted
  CHARACTER(len = :), allocatable :: ENCRYPT
  INTEGER :: charAsNumber, counter
  
  encrypted = strToEncrypt
  
  DO counter = 1, LEN(strToEncrypt)
    currentChar = strToEncrypt(counter:counter)
    charAsNumber = IACHAR(currentChar)
    SELECT CASE (currentChar) !I was originally going to have a 
      CASE ('A':'Z')
        charAsNumber = MODULO(INT(charAsNumber - 65 + shiftAmount), 26) + 65
      CASE ('a':'z')
        charAsNumber = MODULO(INT(charAsNumber - 97 + shiftAmount), 26) + 97
    END SELECT
    currentChar = ACHAR(charAsNumber)
    encrypted(counter:counter) = currentChar
  END DO
  ENCRYPT = encrypted
  RETURN
END FUNCTION

FUNCTION DECRYPT(strToDecrypt, shiftAmount)
  CHARACTER(1) :: currentChar
  CHARACTER(len = *) :: strToDecrypt
  INTEGER :: shiftAmount
  CHARACTER(len = len(strToDecrypt)) :: decrypted
  CHARACTER(len = :), allocatable :: DECRYPT
  INTEGER :: charAsNumber, counter
  
  decrypted = strToDecrypt
  
  DO counter = 1, LEN(strToDecrypt)
    currentChar = strToDecrypt(counter:counter)
    charAsNumber = IACHAR(currentChar)
    SELECT CASE (currentChar)
      CASE ('A':'Z')
        charAsNumber = MODULO(INT(charAsNumber - 65 - shiftAmount), 26) + 65
      CASE ('a':'z')
        charAsNumber = MODULO(INT(charAsNumber - 97 - shiftAmount), 26) + 97
    END SELECT
    currentChar = ACHAR(charAsNumber)
    decrypted(counter:counter) = currentChar
  END DO
  DECRYPT = decrypted
  RETURN
END FUNCTION 
  
END PROGRAM Caesar_Cypher
