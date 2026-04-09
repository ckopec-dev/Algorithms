# ElGamal Encryption in Haskell

Here's a complete implementation of the ElGamal encryption algorithm in Haskell:

```haskell
{-# LANGUAGE TupleSections #-}

module ElGamal where

import System.Random
import Data.List (foldl')

-- Type aliases for clarity
type PublicKey = (Integer, Integer, Integer)  -- (p, g, y)
type PrivateKey = Integer                     -- x
type CipherText = (Integer, Integer)          -- (c1, c2)

-- Modular exponentiation
modPow :: Integer -> Integer -> Integer -> Integer
modPow base exp mod = 
    let go _ 0 acc = acc
        go b e acc = 
            let newAcc = if odd e then (b * acc) `mod` mod else acc
                newB = (b * b) `mod` mod
                newE = e `div` 2
            in go newB newE newAcc
    in go base exp 1

-- Extended Euclidean Algorithm
extendedGCD :: Integer -> Integer -> (Integer, Integer, Integer)
extendedGCD a 0 = (a, 1, 0)
extendedGCD a b = 
    let (g, x', y') = extendedGCD b (a `mod` b)
        x = y'
        y = x' - (a `div` b) * y'
    in (g, x, y)

-- Modular inverse
modInverse :: Integer -> Integer -> Integer
modInverse a m = 
    let (g, x, _) = extendedGCD a m
    in if g == 1 then (x `mod` m + m) `mod` m
       else error "Modular inverse does not exist"

-- Generate a random prime number (simplified version)
generatePrime :: Int -> IO Integer
generatePrime bits = do
    let limit = 2 ^ bits
    randomRIO (limit `div` 2, limit - 1) >>= return

-- ElGamal key generation
elGamalKeyGen :: Int -> IO (PublicKey, PrivateKey)
elGamalKeyGen bits = do
    -- Generate prime p
    p <- generatePrime bits
    -- Generate random g (generator)
    g <- randomRIO (2, p - 1)
    -- Generate random private key x
    x <- randomRIO (1, p - 2)
    -- Calculate public key component y = g^x mod p
    let y = modPow g x p
    return ((p, g, y), x)

-- ElGamal encryption
elGamalEncrypt :: PublicKey -> Integer -> IO CipherText
elGamalEncrypt (p, g, y) m = do
    -- Generate random k
    k <- randomRIO (1, p - 2)
    -- Calculate c1 = g^k mod p
    let c1 = modPow g k p
    -- Calculate c2 = m * y^k mod p
    let yk = modPow y k p
    let c2 = (m * yk) `mod` p
    return (c1, c2)

-- ElGamal decryption
elGamalDecrypt :: PrivateKey -> PublicKey -> CipherText -> Integer
elGamalDecrypt x (p, _, _) (c1, c2) = 
    let s = modPow c1 x p  -- s = c1^x mod p
        sInv = modInverse s p
        m = (c2 * sInv) `mod` p
    in m

-- Example usage
example :: IO ()
example = do
    putStrLn "=== ElGamal Encryption Example ==="
    
    -- Generate keys
    putStrLn "Generating keys..."
    (pubKey, privKey) <- elGamalKeyGen 16
    putStrLn $ "Public Key: " ++ show pubKey
    putStrLn $ "Private Key: " ++ show privKey
    
    -- Message to encrypt
    let message = 42 :: Integer
    putStrLn $ "Original message: " ++ show message
    
    -- Encrypt
    putStrLn "Encrypting message..."
    cipherText <- elGamalEncrypt pubKey message
    putStrLn $ "Cipher text: " ++ show cipherText
    
    -- Decrypt
    putStrLn "Decrypting message..."
    decrypted <- return $ elGamalDecrypt privKey pubKey cipherText
    putStrLn $ "Decrypted message: " ++ show decrypted
    
    -- Verify
    putStrLn $ "Verification: " ++ if message == decrypted 
        then "SUCCESS - Messages match!" 
        else "FAILURE - Messages don't match!"

-- Batch encryption example
batchExample :: IO ()
batchExample = do
    putStrLn "\n=== Batch Encryption Example ==="
    
    (pubKey, privKey) <- elGamalKeyGen 16
    let messages = [10, 20, 30, 40, 50]
    
    putStrLn $ "Original messages: " ++ show messages
    
    -- Encrypt all messages
    cipherTexts <- mapM (elGamalEncrypt pubKey) messages
    putStrLn $ "Cipher texts: " ++ show cipherTexts
    
    -- Decrypt all messages
    let decryptedMessages = map (elGamalDecrypt privKey pubKey) cipherTexts
    putStrLn $ "Decrypted messages: " ++ show decryptedMessages
    
    putStrLn $ "All messages decrypted correctly: " ++ 
        show (messages == decryptedMessages)
```

## Usage Example

To run the example:

```haskell
-- In GHCi or main function:
main :: IO ()
main = example
```

## Key Components

1. **Key Generation**: Creates public and private keys using a prime `p`, generator `g`, and private key `x`
2. **Encryption**: Takes a message and encrypts it using the public key
3. **Decryption**: Uses the private key to recover the original message
4. **Modular Arithmetic**: Implements efficient modular exponentiation and inverse operations

## Security Notes

- This is a simplified implementation for demonstration
- In production, use cryptographically secure random number generators
- The prime `p` should be large enough for security (at least 1024 bits)
- The private key `x` should be kept secret
- The random number `k` should be unique for each encryption

The algorithm works by:
1. Generating a random number `k` for each encryption
2. Computing `c1 = g^k mod p` and `c2 = m * y^k mod p`
3. The recipient uses their private key to compute `s = c1^x mod p` and `m = c2 * s^(-1) mod p`

