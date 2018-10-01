! Alexis Dionne
! Programming in the Past - Fortran
! CMPT 331
! 9/14/2018
 
MODULE Cypher_Functions
IMPLICIT NONE

CONTAINS 

FUNCTION solve(strToConvert, maxShift)
  INTEGER :: counter
  CHARACTER(LEN = LEN(strToConvert)) :: convertedStr
  
  DO counter = maxShift, 0
    convertedStr = encrypt(strToConvert, counter)
    PRINT *, "Caesar ", counter, ": ", convertedStr
    counter = counter - 1
  END
END FUNCTION

FUNCTION encrypt(strToEncrypt, shiftAmount) RESULT(strEncrypted)
  CHARACTER :: currentChar, strToEncrypt
  CHARACTER(LEN(strToEncrypt)) :: strEncrypted
  INTEGER :: charAsNumber, counter
  
  strToEncrypt = ToUpper(strToEncrypt)
  
  DO counter = 1, LEN(strToEncrypt)
    currentChar = strToEncrypt(counter:counter)
    charAsNumber = IACHAR(currentChar)
    charAsNumber = (charAsNumber + shiftAmount) % 90
    IF (charAsNumber <= shiftAmount) charAsNumber = charAsNumber + 64
    currentChar = ACHAR(charAsNumber)
    strEncrypted = strEncrypted//currentChar
  END DO
END FUNCTION

FUNCTION decrypt(strToDecrypt, shiftAmount) RESULT(strDecrypted)
  CHARACTER :: currentChar
  CHARACTER(LEN(strToDecrypt)) :: strDecrypted
  INTEGER :: charAsNumber, counter
  
  strToDecrypt = ToUpper(strToDecrypt)
  
  DO counter = 1, LEN(strToDecrypt)
    currentChar = strToDecrypt(counter:counter)
    charAsNumber = IACHAR(currentChar)
    charAsNumber = (charAsNumber - shiftAmount) % 90
    IF (charAsNumber <= shiftAmount) charAsNumber = charAsNumber + 64
    currentChar = ACHAR(charAsNumber)
    strDecrypted = strDecrypted // currentChar
  END DO
END FUNCTION  
FUNCTION ToUpper(strToConvert) RESULT(strConverted)
	CHARACTER(*) :: strToConvert
	CHARACTER(LEN(strToConvert)) :: strConverted
	CHARACTER :: currentCharacter
	INTEGER,PARAMETER :: DUC = IACHAR('A') - IACHAR('a')
	INTEGER :: counter
	
	DO counter = 1, LEN(strToConvert)
		currentCharacter = strToConvert(counter:counter);
		IF (currentCharacter >= 'a' .AND. currentCharacter <= 'z') 
			currentCharacter = ACHAR(IACHAR(currentCharacter) + DUC)
		strConverted(i:i) = currentCharacter
  END DO
END FUNCTION
END MODULE

PROGRAM Caesar_Cypher
  USE Cypher_Functions
  IMPLICIT NONE
  
  CHARACTER(LEN = 20) :: str
  CHARACTER :: encrypt, decrypt
  
  str = encrypt("Programming is fun!", 17)
  PRINT *, str
  
END PROGRAM Caesar_Cypher
