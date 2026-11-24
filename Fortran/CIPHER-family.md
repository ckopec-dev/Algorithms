# CIPHER__family Algorithm Example in Fortran

Here's an example implementation of a simple cipher algorithm family in Fortran, demonstrating the basic structure and concepts:

```fortran
program cipher_family_example
    implicit none
    integer, parameter :: n = 100
    character(len=1), dimension(n) :: plaintext, ciphertext
    character(len=1), dimension(n) :: key
    integer :: i, j, key_length
    
    ! Initialize key (simple Caesar cipher with shift of 3)
    key_length = 1
    key(1) = char(3)  ! Shift by 3 positions
    
    ! Sample plaintext
    plaintext = ['H','e','l','l','o',' ','W','o','r','l','d']
    
    ! Encrypt using Caesar cipher (simple cipher family member)
    call caesar_encrypt(plaintext, ciphertext, key_length)
    
    ! Display results
    write(*,*) 'Original text: ', trim(plaintext)
    write(*,*) 'Encrypted text: ', trim(ciphertext)
    
    ! Decrypt
    call caesar_decrypt(ciphertext, plaintext, key_length)
    write(*,*) 'Decrypted text: ', trim(plaintext)
    
contains
    
    subroutine caesar_encrypt(plain, cipher, k_len)
        character(len=1), intent(in) :: plain(:)
        character(len=1), intent(out) :: cipher(:)
        integer, intent(in) :: k_len
        integer :: i, shift_val
        
        shift_val = ichar(key(1))
        
        do i = 1, size(plain)
            if (plain(i) >= 'A' .and. plain(i) <= 'Z') then
                cipher(i) = char(mod(ichar(plain(i)) - ichar('A') + shift_val, 26) + ichar('A'))
            else if (plain(i) >= 'a' .and. plain(i) <= 'z') then
                cipher(i) = char(mod(ichar(plain(i)) - ichar('a') + shift_val, 26) + ichar('a'))
            else
                cipher(i) = plain(i)
            end if
        end do
    end subroutine caesar_encrypt
    
    subroutine caesar_decrypt(cipher, plain, k_len)
        character(len=1), intent(in) :: cipher(:)
        character(len=1), intent(out) :: plain(:)
        integer, intent(in) :: k_len
        integer :: i, shift_val
        
        shift_val = ichar(key(1))
        
        do i = 1, size(cipher)
            if (cipher(i) >= 'A' .and. cipher(i) <= 'Z') then
                plain(i) = char(mod(ichar(cipher(i)) - ichar('A') - shift_val + 26, 26) + ichar('A'))
            else if (cipher(i) >= 'a' .and. cipher(i) <= 'z') then
                plain(i) = char(mod(ichar(cipher(i)) - ichar('a') - shift_val + 26, 26) + ichar('a'))
            else
                plain(i) = cipher(i)
            end if
        end do
    end subroutine caesar_decrypt
    
end program cipher_family_example
```

## Cipher Family Concepts in Fortran

This example demonstrates:

1. **Basic Cipher Structure**: Encryption and decryption subroutines
2. **Key Management**: Simple key handling using character arrays
3. **Algorithm Flexibility**: Easy to modify for different cipher types
4. **Character Manipulation**: Using `ichar()` and `char()` functions
5. **Modular Design**: Separate encryption/decryption routines

## Key Features of This Cipher Family:

- **Caesar Cipher**: Simple shift-based encryption
- **Extensible**: Easy to add new cipher algorithms
- **Character-Based**: Works with ASCII character sets
- **Fortran-Compatible**: Uses standard Fortran features

This approach shows how Fortran can implement cryptographic algorithms while maintaining readability and maintainability.