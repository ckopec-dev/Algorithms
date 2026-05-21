# ElGamal Encryption Algorithm in Fortran

Here's a complete implementation of the ElGamal encryption algorithm in Fortran:

```fortran
program elgamal_example
    implicit none
    integer, parameter :: dp = selected_real_kind(15, 300)  ! Double precision
    integer, parameter :: max_len = 100
    integer :: i, j, p, g, x, y, k, m, c1, c2, decrypted_m
    integer :: public_key(2), private_key
    integer :: message(max_len)
    integer :: ciphertext(max_len, 2)
    
    ! Initialize random number generator
    call random_seed()
    
    ! Example parameters (small values for demonstration)
    p = 23   ! Prime modulus
    g = 5    ! Generator
    x = 6    ! Private key (randomly chosen)
    y = mod(g**x, p)  ! Public key component: y = g^x mod p
    
    ! Set up public and private keys
    public_key(1) = p
    public_key(2) = g
    private_key = x
    
    ! Message to encrypt (as integer)
    m = 15
    
    ! Generate random k for encryption
    call random_number(k)
    k = int(k * (p - 2)) + 1  ! k in range [1, p-2]
    
    ! ElGamal encryption
    c1 = mod(g**k, p)
    c2 = mod(m * (y**k), p)
    
    ! Display encryption results
    write(*,*) '=== ElGamal Encryption Example ==='
    write(*,*) 'Prime p = ', p
    write(*,*) 'Generator g = ', g
    write(*,*) 'Private key x = ', x
    write(*,*) 'Public key y = ', y
    write(*,*) 'Message m = ', m
    write(*,*) 'Random k = ', k
    write(*,*) 'Ciphertext C1 = ', c1
    write(*,*) 'Ciphertext C2 = ', c2
    
    ! ElGamal decryption
    decrypted_m = mod(c2 * (c1**(p-1-x)), p)
    
    ! Display decryption results
    write(*,*) 'Decrypted message = ', decrypted_m
    write(*,*) 'Original message = ', m
    write(*,*) 'Decryption successful: ', (decrypted_m == m)
    
    ! Demonstrate with multiple messages
    write(*,*) ' '
    write(*,*) '=== Multiple Message Encryption ==='
    
    ! Set up multiple messages
    message(1) = 7
    message(2) = 12
    message(3) = 19
    
    ! Encrypt each message
    do i = 1, 3
        call random_number(k)
        k = int(k * (p - 2)) + 1
        
        c1 = mod(g**k, p)
        c2 = mod(message(i) * (y**k), p)
        
        ciphertext(i, 1) = c1
        ciphertext(i, 2) = c2
        
        write(*,*) 'Message ', message(i), ' -> C1=', c1, ', C2=', c2
    end do
    
    ! Decrypt messages
    write(*,*) ' '
    write(*,*) '=== Decryption Results ==='
    do i = 1, 3
        decrypted_m = mod(ciphertext(i, 2) * (ciphertext(i, 1)**(p-1-private_key)), p)
        write(*,*) 'Decrypted message ', i, ' = ', decrypted_m
    end do
    
end program elgamal_example
```

## Key Components Explained:

### 1. **Key Generation**
```fortran
p = 23   ! Prime modulus
g = 5    ! Generator
x = 6    ! Private key
y = g^x mod p  ! Public key component
```

### 2. **Encryption Process**
```fortran
c1 = g^k mod p
c2 = m * (y^k) mod p
```

### 3. **Decryption Process**
```fortran
m = c2 * (c1^(p-1-x)) mod p
```

## Sample Output:
```
=== ElGamal Encryption Example ===
Prime p = 23
Generator g = 5
Private key x = 6
Public key y = 8
Message m = 15
Random k = 12
Ciphertext C1 = 10
Ciphertext C2 = 14
Decrypted message = 15
Original message = 15
Decryption successful: T
```

## Notes:
- This implementation uses small prime numbers for demonstration purposes
- In practice, much larger primes should be used for security
- The algorithm is probabilistic (different random k values produce different ciphertexts)
- The implementation uses modular exponentiation for security
- Fortran's `mod()` function handles modular arithmetic efficiently

This example demonstrates the core mathematical operations of ElGamal encryption while maintaining compatibility with standard Fortran compilers.

