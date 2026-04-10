# Diffie-Hellman Key Exchange in Forth

Here's an implementation of the Diffie-Hellman key exchange algorithm in Forth:

```forth
\ Diffie-Hellman Key Exchange Implementation in Forth

\ Constants
1000000007 CONSTANT MODULUS    \ Large prime number
2 CONSTANT BASE              \ Base (generator)

\ Modular exponentiation (a^b mod m)
: modexp ( a b m -- result )
    >R 1 SWAP
    BEGIN
        DUP 0>
    WHILE
        DUP 1 AND IF
            OVER R@ MOD * SWAP R> MOD SWAP
        THEN
        DUP 2 / SWAP
        OVER R@ MOD SWAP
        R> 1+ R>
    REPEAT
    DROP DROP R> DROP
;

\ Generate private key (random number between 1 and MODULUS-1)
: generate-private-key ( -- private-key )
    MODULUS 1- 1+ random
;

\ Generate public key = BASE^private-key mod MODULUS
: generate-public-key ( private-key -- public-key )
    BASE SWAP MODULUS modexp
;

\ Generate shared secret = other-public^private-key mod MODULUS
: generate-shared-secret ( private-key other-public -- shared-secret )
    MODULUS modexp
;

\ Example usage
: dh-example ( -- )
    CR ." Diffie-Hellman Key Exchange Example" CR
    
    \ Alice generates her private and public keys
    generate-private-key -> alice-private
    alice-private generate-public-key -> alice-public
    
    \ Bob generates his private and public keys
    generate-private-key -> bob-private
    bob-private generate-public-key -> bob-public
    
    CR ." Alice's private key: " alice-private .
    CR ." Alice's public key: " alice-public .
    CR ." Bob's private key: " bob-private .
    CR ." Bob's public key: " bob-public .
    
    \ Generate shared secrets
    alice-private bob-public generate-shared-secret -> alice-shared
    bob-private alice-public generate-shared-secret -> bob-shared
    
    CR ." Alice's shared secret: " alice-shared .
    CR ." Bob's shared secret: " bob-shared .
    
    \ Verify they are the same
    alice-shared bob-shared = IF
        CR ." ✓ Shared secrets match!" CR
    ELSE
        CR ." ✗ Shared secrets don't match!" CR
    THEN
;

\ Run the example
dh-example
```

## How it works:

1. **Setup**: Uses a large prime modulus (1000000007) and base (2)
2. **Key Generation**: Each party generates a private key (random number) and computes a public key using modular exponentiation
3. **Key Exchange**: Parties exchange their public keys
4. **Shared Secret**: Each party computes the shared secret using their private key and the other party's public key
5. **Verification**: Both parties should arrive at the same shared secret

## Key Forth Concepts Used:

- **Modular Exponentiation**: Efficient computation of large powers modulo a number
- **Variable Storage**: Using `->` to store values in named variables
- **Function Composition**: Building complex operations from simpler ones
- **Stack Manipulation**: Proper handling of Forth's stack-based operations

This implementation demonstrates the core mathematical operations needed for Diffie-Hellman key exchange in a Forth environment.

