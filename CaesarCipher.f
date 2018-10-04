! Alexis Dionne
! Programming in the Past - Fortran
! CMPT 331
! 9/14/2018
 
SUBROUTINE solve(strToConvert, maxShift)
  INTEGER :: counter
  INTEGER strLength(INT(strToConvert))
  CHARACTER(len=strLength) :: convertedStr
  
  DO counter = maxShift, 0
    convertedStr = encrypt(strToConvert, counter)
    PRINT *, "Caesar ", counter, ": ", convertedStr
  END DO
  RETURN
END

CHARACTER(LEN = 50) FUNCTION ENCRYPT(strToEncrypt, shiftAmount)
  CHARACTER :: currentChar, strToEncrypt
  INTEGER strLength(INT(strToEncrypt))
  CHARACTER(len=strLength) :: strEncrypted
  INTEGER :: charAsNumber, counter, shiftAmount
  
  strToEncrypt = ToUpper(strToEncrypt)
  
  DO counter = 1, LEN(strToEncrypt)
    currentChar = strToEncrypt(counter:counter)
    charAsNumber = IACHAR(currentChar)
    charAsNumber = MOD((charAsNumber + shiftAmount), 90)
    IF (charAsNumber <= shiftAmount) charAsNumber = charAsNumber + 64
    currentChar = ACHAR(charAsNumber)
    strEncrypted = strEncrypted//currentChar
  END DO
  ENCRYPT = strEncrypted
  RETURN
END FUNCTION

CHARACTER(LEN = 50) FUNCTION DECRYPT(strToDecrypt, shiftAmount)
  CHARACTER :: currentChar, strToDecrypt
  CHARACTER(LEN(strToDecrypt)) :: strDecrypted
  INTEGER :: charAsNumber, counter
  
  strToDecrypt = ToUpper(strToDecrypt)
  
  DO counter = 1, LEN(strToDecrypt)
    currentChar = strToDecrypt(counter:counter)
    charAsNumber = IACHAR(currentChar)
    charAsNumber = MOD((charAsNumber - shiftAmount), 90)
    IF (charAsNumber <= shiftAmount) charAsNumber = charAsNumber + 64
    currentChar = ACHAR(charAsNumber)
    strDecrypted = strDecrypted // currentChar
  END DO
  DECRYPT = strDecrypted
  RETURN
END FUNCTION  

CHARACTER(LEN = 50) FUNCTION TOUPPER(strToConvert)
	CHARACTER :: strToConvert
	CHARACTER(LEN(strToConvert)) :: strConverted
	CHARACTER :: currentCharacter
	INTEGER,PARAMETER :: DUC = IACHAR('A') - IACHAR('a')
	INTEGER :: counter
	
	DO counter = 1, LEN(strToConvert)
		currentCharacter = strToConvert(counter:counter);
		IF (currentCharacter >= 'a' .AND. currentCharacter <= 'z') THEN
			currentCharacter = ACHAR(IACHAR(currentCharacter) + DUC)
    END IF
		strConverted(i:i) = currentCharacter
  END DO
  TOUPPER = strConverted
  RETURN
END FUNCTION

PROGRAM Caesar_Cypher
  
  CHARACTER :: str
  
  str = encrypt(str, 17)
  PRINT *, str
  
END PROGRAM Caesar_Cypher
