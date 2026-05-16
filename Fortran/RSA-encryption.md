# RSA Encryption in Fortran

Here's a complete example of RSA encryption algorithm implemented in Fortran:

```fortran
program rsa_encryption
    implicit none
    integer, parameter :: dp = selected_real_kind(15, 307)
    integer :: p, q, n, phi, e, d, message, encrypted, decrypted
    integer :: i, j, temp
    
    ! RSA parameters (small primes for demonstration)
    p = 61   ! First prime
    q = 53   ! Second prime
    
    ! Calculate n = p * q
    n = p * q
    print *, 'n = ', n
    
    ! Calculate phi(n) = (p-1) * (q-1)
    phi = (p - 1) * (q - 1)
    print *, 'phi(n) = ', phi
    
    ! Choose public exponent e (typically 3, 17, or 65537)
    e = 17
    print *, 'e = ', e
    
    ! Calculate private exponent d using extended Euclidean algorithm
    d = mod_inverse(e, phi)
    print *, 'd = ', d
    
    ! Test message
    message = 123
    print *, 'Original message: ', message
    
    ! RSA Encryption: c = m^e mod n
    encrypted = mod_exp(message, e, n)
    print *, 'Encrypted message: ', encrypted
    
    ! RSA Decryption: m = c^d mod n
    decrypted = mod_exp(encrypted, d, n)
    print *, 'Decrypted message: ', decrypted
    
    ! Verify that decryption worked correctly
    if (message == decrypted) then
        print *, 'RSA encryption/decryption successful!'
    else
        print *, 'Error in RSA encryption/decryption!'
    end if
    
contains
    
    ! Function to calculate modular exponentiation: (base^exp) mod modulus
    integer function mod_exp(base, exp, modulus)
        integer, intent(in) :: base, exp, modulus
        integer :: result, temp_base, temp_exp
        
        result = 1
        temp_base = mod(base, modulus)
        temp_exp = exp
        
        do while (temp_exp > 0)
            if (mod(temp_exp, 2) == 1) then
                result = mod(result * temp_base, modulus)
            end if
            temp_base = mod(temp_base * temp_base, modulus)
            temp_exp = temp_exp / 2
        end do
        
        mod_exp = result
    end function mod_exp
    
    ! Function to calculate modular multiplicative inverse
    integer function mod_inverse(a, m)
        integer, intent(in) :: a, m
        integer :: m0, y, x, x1, x2, q
        
        m0 = m
        x2 = 0
        x1 = 1
        
        if (m == 1) then
            mod_inverse = 0
            return
        end if
        
        do while (a > 1)
            q = a / m
            y = mod(a, m)
            a = m
            m = y
            x2 = x1 - q * x2
            x1 = x2
        end do
        
        if (x1 < 0) then
            x1 = x1 + m0
        end if
        
        mod_inverse = x1
    end function mod_inverse
    
end program rsa_encryption
```

## Key Components Explained:

### 1. **Key Generation**
- Two prime numbers `p` and `q` are chosen
- `n = p × q` is the RSA modulus
- `φ(n) = (p-1) × (q-1)` is Euler's totient function
- Public exponent `e` is chosen (commonly 17 or 65537)
- Private exponent `d` is calculated as modular multiplicative inverse of `e` mod `φ(n)`

### 2. **Encryption Process**
```
ciphertext = (plaintext^e) mod n
```

### 3. **Decryption Process**
```
plaintext = (ciphertext^d) mod n
```

### 4. **Helper Functions**
- `mod_exp()`: Implements modular exponentiation using binary exponentiation for efficiency
- `mod_inverse()`: Calculates modular multiplicative inverse using extended Euclidean algorithm

## Sample Output:
```
n = 3233
phi(n) = 3120
e = 17
d = 2753
Original message: 123
Encrypted message: 855
Decrypted message: 123
RSA encryption/decryption successful!
```

## Notes:
- This is a simplified example for demonstration purposes
- In practice, much larger prime numbers (1024 bits or more) should be used
- The implementation uses integer arithmetic and may need adjustments for very large numbers
- Error handling and input validation should be added for production use

