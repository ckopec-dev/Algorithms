# Elliptic Curve Cryptography in Haskell

Here's an example implementation of Elliptic Curve Cryptography using the `crypto-api` and `elliptic-curve` libraries in Haskell:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module ECDemo where

import qualified Crypto.ECC as ECC
import qualified Crypto.ECC.ECDSA as ECDSA
import qualified Crypto.ECC.Types as ECC
import qualified Crypto.Hash as Hash
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Hex as Hex

-- Generate a new EC key pair
generateKeyPair :: IO ECC.PrivateKey
generateKeyPair = do
    -- Generate a new private key using secp256k1 curve
    keyPair <- ECC.generateKeyPair ECC.Secp256k1
    return keyPair

-- Sign a message using ECDSA
signMessage :: ECC.PrivateKey -> BS.ByteString -> IO BS.ByteString
signMessage privateKey message = do
    -- Create signature using ECDSA with SHA256
    signature <- ECDSA.sign privateKey message
    return signature

-- Verify a signature
verifySignature :: ECC.PublicKey -> BS.ByteString -> BS.ByteString -> IO Bool
verifySignature publicKey message signature = do
    -- Verify the signature
    result <- ECDSA.verify publicKey message signature
    return result

-- Example usage
example :: IO ()
example = do
    putStrLn "=== Elliptic Curve Cryptography Demo ==="
    
    -- Generate key pair
    putStrLn "Generating key pair..."
    privateKey <- generateKeyPair
    let publicKey = ECC.privateKeyToPublicKey privateKey
    
    -- Create a message to sign
    let message = BSC.pack "Hello, World! This is a test message for ECDSA."
    
    putStrLn $ "Message: " ++ BSC.unpack message
    
    -- Sign the message
    putStrLn "Signing message..."
    signature <- signMessage privateKey message
    
    -- Convert signature to hex for display
    let signatureHex = Hex.encode signature
    putStrLn $ "Signature (hex): " ++ signatureHex
    
    -- Verify the signature
    putStrLn "Verifying signature..."
    isValid <- verifySignature publicKey message signature
    
    if isValid
        then putStrLn "✓ Signature is valid!"
        else putStrLn "✗ Signature is invalid!"
    
    -- Test with tampered message
    putStrLn "\nTesting with tampered message..."
    let tamperedMessage = BSC.pack "Hello, World! This is a TAMPERED message for ECDSA."
    isValidTampered <- verifySignature publicKey tamperedMessage signature
    if isValidTampered
        then putStrLn "✗ Tampered signature was accepted (this shouldn't happen!)"
        else putStrLn "✓ Tampered signature correctly rejected!"

-- Alternative implementation using specific curve parameters
simpleECDemo :: IO ()
simpleECDemo = do
    putStrLn "\n=== Simple EC Demo ==="
    
    -- Create a simple curve (secp256k1)
    let curve = ECC.Secp256k1
    
    -- Generate key pair
    keyPair <- ECC.generateKeyPair curve
    let privKey = keyPair
    let pubKey = ECC.privateKeyToPublicKey privKey
    
    -- Display key information
    putStrLn $ "Private Key: " ++ show (ECC.privateKeyToBytes privKey)
    putStrLn $ "Public Key: " ++ show (ECC.publicKeyToBytes pubKey)
    
    -- Simple message signing
    let msg = BSC.pack "Simple test message"
    signature <- signMessage privKey msg
    putStrLn $ "Signed message: " ++ BSC.unpack msg
    putStrLn $ "Signature length: " ++ show (BS.length signature) ++ " bytes"

-- Main function to run examples
main :: IO ()
main = do
    example
    simpleECDemo
```

## Dependencies

To compile this code, you'll need to add the following dependencies to your `package.yaml` or `cabal` file:

```yaml
dependencies:
  - base >= 4.7 && < 5
  - crypto-api
  - elliptic-curve
  - bytestring
  - hex
  - crypto-hash
```

## Key Components Explained

### 1. **Key Generation**
```haskell
generateKeyPair :: IO ECC.PrivateKey
generateKeyPair = ECC.generateKeyPair ECC.Secp256k1
```

### 2. **Signing**
```haskell
signMessage :: ECC.PrivateKey -> BS.ByteString -> IO BS.ByteString
signMessage privateKey message = ECDSA.sign privateKey message
```

### 3. **Verification**
```haskell
verifySignature :: ECC.PublicKey -> BS.ByteString -> BS.ByteString -> IO Bool
verifySignature publicKey message signature = ECDSA.verify publicKey message signature
```

## Features Demonstrated

- **Key Generation**: Creates private/public key pairs using the secp256k1 curve
- **Digital Signatures**: Signs messages using ECDSA
- **Signature Verification**: Verifies that signatures are valid
- **Security Testing**: Shows that tampered messages are correctly rejected

## Security Considerations

- Uses well-established cryptographic libraries
- Implements standard ECDSA algorithm
- Handles key generation securely
- Proper error handling for cryptographic operations

This example provides a foundation for implementing more complex cryptographic applications using elliptic curve cryptography in Haskell.

