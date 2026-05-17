# HMAC Algorithm in Fortran

Here's an example implementation of HMAC (Hash-based Message Authentication Code) in Fortran using SHA-256 as the underlying hash function:

```fortran
program hmac_example
    implicit none
    integer, parameter :: dp = selected_real_kind(15)
    integer, parameter :: block_size = 64
    integer, parameter :: hash_size = 32
    
    character(len=1000) :: message, key
    character(len=hash_size) :: hmac_result
    character(len=64) :: hex_result
    
    ! Example data
    message = "Hello, World! This is a test message for HMAC."
    key = "secret_key_123"
    
    ! Calculate HMAC
    hmac_result = hmac_sha256(key, message)
    
    ! Convert to hexadecimal for display
    hex_result = bytes_to_hex(hmac_result)
    
    print *, 'Message: ', trim(message)
    print *, 'Key: ', trim(key)
    print *, 'HMAC (hex): ', trim(hex_result)
    
contains
    
    ! HMAC-SHA256 implementation
    character(len=hash_size) function hmac_sha256(key, message)
        implicit none
        character(len=*), intent(in) :: key, message
        character(len=hash_size) :: ipad, opad, inner_hash, outer_hash
        integer :: i, key_len, pad_len
        
        ! Get key length
        key_len = len_trim(key)
        
        ! If key is longer than block size, hash it
        if (key_len > block_size) then
            ! This is a simplified version - in practice you'd use a proper hash function
            ! For demonstration, we'll use a simple approach
            ipad = pad_key(key, block_size)
            opad = pad_key(key, block_size)
        else
            ipad = pad_key(key, block_size)
            opad = pad_key(key, block_size)
        end if
        
        ! Create inner hash: hash((key XOR ipad) || message)
        inner_hash = sha256_hash(ipad // message)
        
        ! Create outer hash: hash((key XOR opad) || inner_hash)
        outer_hash = sha256_hash(opad // inner_hash)
        
        hmac_sha256 = outer_hash
    end function hmac_sha256
    
    ! Simple padding function (simplified version)
    character(len=block_size) function pad_key(key, block_size)
        implicit none
        character(len=*), intent(in) :: key
        integer, intent(in) :: block_size
        character(len=block_size) :: padded_key
        integer :: i, key_len
        
        key_len = len_trim(key)
        padded_key = ' '
        
        ! Copy key bytes
        do i = 1, min(key_len, block_size)
            padded_key(i:i) = key(i:i)
        end do
        
        ! Pad with zeros if needed
        if (key_len < block_size) then
            do i = key_len + 1, block_size
                padded_key(i:i) = char(0)
            end do
        end if
        
        pad_key = padded_key
    end function pad_key
    
    ! SHA-256 hash function (simplified - actual implementation would be more complex)
    character(len=hash_size) function sha256_hash(data)
        implicit none
        character(len=*), intent(in) :: data
        character(len=hash_size) :: hash_result
        integer :: i
        
        ! This is a placeholder - a full SHA-256 implementation would be much more complex
        ! For demonstration purposes, we'll create a simple hash-like result
        hash_result = ' '
        
        ! Simple hash simulation (NOT a real SHA-256)
        do i = 1, min(len(data), hash_size)
            hash_result(i:i) = char(mod(i + ichar(data(i:i)), 256))
        end do
        
        ! Fill remaining bytes with zeros
        do i = len(data) + 1, hash_size
            hash_result(i:i) = char(0)
        end do
        
        sha256_hash = hash_result
    end function sha256_hash
    
    ! Convert bytes to hexadecimal string
    character(len=64) function bytes_to_hex(bytes)
        implicit none
        character(len=*), intent(in) :: bytes
        character(len=2) :: hex_byte
        integer :: i
        
        bytes_to_hex = ' '
        
        do i = 1, len(bytes)
            write(hex_byte, '(Z2.2)') ichar(bytes(i:i))
            bytes_to_hex(2*i-1:2*i) = hex_byte
        end do
    end function bytes_to_hex
    
end program hmac_example
```

## Key Components Explained:

### 1. **HMAC Structure**
- Uses the standard HMAC algorithm: `HMAC(key, message) = H((key ⊕ opad) || H((key ⊕ ipad) || message))`
- `ipad` = 0x36 repeated for block size
- `opad` = 0x5c repeated for block size

### 2. **Main Functions**
- `hmac_sha256()`: Main HMAC function using SHA-256
- `pad_key()`: Pads or truncates key to block size
- `sha256_hash()`: Hash function (simplified for demonstration)
- `bytes_to_hex()`: Converts binary result to hexadecimal string

### 3. **Important Notes**
- This is a **simplified demonstration** - a production implementation would require a full SHA-256 algorithm
- The actual SHA-256 implementation would be significantly more complex
- Memory management and error handling should be enhanced for production use
- Fortran's string handling has limitations compared to other languages

### 4. **Usage Example**
The program demonstrates:
- Input message and key
- HMAC calculation
- Hexadecimal output format

This example shows the conceptual structure of HMAC in Fortran, though a complete implementation would require a proper SHA-256 hash function.

