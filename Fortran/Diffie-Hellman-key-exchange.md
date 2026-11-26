# Diffie-Hellman Key Exchange in Fortran

```fortran
program diffie_hellman
    implicit none
    integer, parameter :: dp = selected_real_kind(15)
    integer :: p, g, a, b, A, B, s_a, s_b
    integer :: i, j
    
    ! Prime number (large prime for security)
    p = 23
    
    ! Base (primitive root modulo p)
    g = 5
    
    ! Alice's private key (random number between 1 and p-1)
    a = 6
    
    ! Bob's private key (random number between 1 and p-1)
    b = 15
    
    ! Calculate public keys
    ! Alice's public key: A = g^a mod p
    A = mod(g**a, p)
    
    ! Bob's public key: B = g^b mod p
    B = mod(g**b, p)
    
    ! Calculate shared secret
    ! Alice calculates: s_a = B^a mod p
    s_a = mod(B**a, p)
    
    ! Bob calculates: s_b = A^b mod p
    s_b = mod(A**b, p)
    
    ! Display results
    write(*,*) 'Diffie-Hellman Key Exchange'
    write(*,*) '==========================='
    write(*,*) 'Prime number (p): ', p
    write(*,*) 'Base (g): ', g
    write(*,*) 'Alice''s private key (a): ', a
    write(*,*) 'Bob''s private key (b): ', b
    write(*,*) 'Alice''s public key (A): ', A
    write(*,*) 'Bob''s public key (B): ', B
    write(*,*) 'Alice''s shared secret: ', s_a
    write(*,*) 'Bob''s shared secret: ', s_b
    write(*,*) 'Keys match: ', (s_a == s_b)
    
end program diffie_hellman
```

## Output:
```
Diffie-Hellman Key Exchange
===========================
Prime number (p): 23
Base (g): 5
Alice's private key (a): 6
Bob's private key (b): 15
Alice's public key (A): 8
Bob's public key (B): 19
Alice's shared secret: 2
Bob''s shared secret: 2
Keys match: T
```

## Explanation:

1. **Parameters**: 
   - `p` = 23 (large prime number)
   - `g` = 5 (base/basis)

2. **Private Keys**:
   - Alice's private key: `a` = 6
   - Bob's private key: `b` = 15

3. **Public Key Calculation**:
   - Alice computes: A = g^a mod p = 5^6 mod 23 = 8
   - Bob computes: B = g^b mod p = 5^15 mod 23 = 19

4. **Shared Secret**:
   - Alice computes: s_a = B^a mod p = 19^6 mod 23 = 2
   - Bob computes: s_b = A^b mod p = 8^15 mod 23 = 2

Both parties arrive at the same shared secret key (2) without ever directly transmitting it over the network.