# Caesar Cipher in Fortran

Here's an example implementation of the Caesar cipher algorithm in Fortran:

```fortran
program caesar_cipher
    implicit none
    character(len=100) :: plaintext, ciphertext
    integer :: shift_value, i
    character(len=1) :: char_input
    
    ! Get input from user
    write(*,*) 'Enter the text to encrypt:'
    read(*,'(A)') plaintext
    
    write(*,*) 'Enter the shift value (1-25):'
    read(*,*) shift_value
    
    ! Encrypt the text
    call encrypt_caesar(plaintext, shift_value, ciphertext)
    
    ! Display results
    write(*,*) 'Original text:', trim(plaintext)
    write(*,*) 'Encrypted text:', trim(ciphertext)
    
    ! Decrypt the text
    call decrypt_caesar(ciphertext, shift_value, plaintext)
    write(*,*) 'Decrypted text:', trim(plaintext)
    
end program caesar_cipher

subroutine encrypt_caesar(text, shift, result)
    implicit none
    character(len=*), intent(in) :: text
    integer, intent(in) :: shift
    character(len=len(text)), intent(out) :: result
    integer :: i, char_code
    
    result = ' '
    
    do i = 1, len_trim(text)
        char_code = ichar(text(i:i))
        
        ! Handle uppercase letters
        if (char_code >= 65 .and. char_code <= 90) then
            char_code = mod(char_code - 65 + shift, 26) + 65
        ! Handle lowercase letters  
        else if (char_code >= 97 .and. char_code <= 122) then
            char_code = mod(char_code - 97 + shift, 26) + 97
        end if
        
        result(i:i) = char(char_code)
    end do
    
end subroutine encrypt_caesar

subroutine decrypt_caesar(text, shift, result)
    implicit none
    character(len=*), intent(in) :: text
    integer, intent(in) :: shift
    character(len=len(text)), intent(out) :: result
    integer :: i, char_code
    
    result = ' '
    
    do i = 1, len_trim(text)
        char_code = ichar(text(i:i))
        
        ! Handle uppercase letters
        if (char_code >= 65 .and. char_code <= 90) then
            char_code = mod(char_code - 65 - shift + 26, 26) + 65
        ! Handle lowercase letters  
        else if (char_code >= 97 .and. char_code <= 122) then
            char_code = mod(char_code - 97 - shift + 26, 26) + 97
        end if
        
        result(i:i) = char(char_code)
    end do
    
end subroutine decrypt_caesar
```

## How it works:

1. **Main Program**: 
   - Takes user input for plaintext and shift value
   - Calls encryption subroutine
   - Displays original, encrypted, and decrypted text

2. **Encryption Subroutine**:
   - Processes each character in the input text
   - Shifts uppercase letters (A-Z) by the specified amount
   - Shifts lowercase letters (a-z) by the specified amount
   - Uses modular arithmetic to wrap around the alphabet

3. **Decryption Subroutine**:
   - Reverses the encryption process
   - Subtracts the shift value instead of adding it
   - Handles wrapping using modular arithmetic

## Sample Output:
```
Enter the text to encrypt:
Hello World
Enter the shift value (1-25):
3
Original text: Hello World
Encrypted text: Khoor Zruog
Decrypted text: Hello World
```

This implementation handles both uppercase and lowercase letters while preserving spaces and non-alphabetic characters.