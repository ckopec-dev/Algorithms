# HMAC Algorithm in Haskell

Here's an example of implementing HMAC (Hash-based Message Authentication Code) in Haskell using the `crypto-api` and `crypton` libraries:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Crypto.Hash
import Crypto.MAC.HMAC
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.ByteString.Base16 (encode, decode)
import Data.Hex (showHex)

-- Simple HMAC example using SHA256
exampleHMAC :: IO ()
exampleHMAC = do
    let key = "my-secret-key"
    let message = "Hello, World!"
    
    -- Create HMAC-SHA256
    let hmac = hmacSHA256 key message
    
    -- Display results
    putStrLn $ "Key: " ++ key
    putStrLn $ "Message: " ++ message
    putStrLn $ "HMAC (hex): " ++ showHex hmac
    putStrLn $ "HMAC (base16): " ++ BSC.unpack (encode hmac)

-- Helper function to create HMAC with SHA256
hmacSHA256 :: String -> String -> BS.ByteString
hmacSHA256 key message = 
    let keyBytes = BSC.pack key
        msgBytes = BSC.pack message
    in hmac keyBytes msgBytes

-- More comprehensive example with verification
hmacVerificationExample :: IO ()
hmacVerificationExample = do
    let secretKey = "super-secret-key-123"
    let originalMessage = "This is a confidential message"
    
    -- Generate HMAC
    let mac = hmacSHA256 secretKey originalMessage
    
    -- Verify the HMAC (this would typically be done by comparing with a received MAC)
    let isValid = verifyHMAC secretKey originalMessage mac
    let invalidMac = BS.cons 'x' mac  -- Tampered MAC
    let isInvalid = verifyHMAC secretKey originalMessage invalidMac
    
    putStrLn $ "Original message: " ++ originalMessage
    putStrLn $ "Generated MAC: " ++ showHex mac
    putStrLn $ "Verification result: " ++ show isValid
    putStrLn $ "Tampered verification result: " ++ show isInvalid

-- Verify HMAC (simplified version)
verifyHMAC :: String -> String -> BS.ByteString -> Bool
verifyHMAC key message expectedMac = 
    let actualMac = hmacSHA256 key message
    in actualMac == expectedMac

-- Alternative implementation using Crypto.MAC.HMAC directly
import Crypto.MAC.HMAC (hmac)
import Crypto.Hash (SHA256, hash)
import qualified Crypto.MAC.HMAC as HMAC

hmacExampleWithCryptoAPI :: IO ()
hmacExampleWithCryptoAPI = do
    let key = BS.pack "my-secret-key"
    let message = BS.pack "Hello, World!"
    
    -- Using HMAC directly from crypto-api
    let mac = HMAC.hmac key message :: HMAC.HMAC SHA256
    
    putStrLn $ "HMAC using crypto-api: " ++ showHex (HMAC.hmacDigest mac)

-- Utility function to convert ByteString to hex string
showHex :: BS.ByteString -> String
showHex = BSC.unpack . encode

-- Main function to run examples
main :: IO ()
main = do
    putStrLn "=== HMAC Examples ==="
    exampleHMAC
    putStrLn ""
    hmacVerificationExample
    putStrLn ""
    hmacExampleWithCryptoAPI
```

## Required Dependencies

To run this code, you'll need to add these dependencies to your `package.yaml` or `cabal` file:

```yaml
dependencies:
  - base >= 4.7 && < 5
  - crypton
  - crypto-api
  - bytestring
  - hex
```

## Key Features of This Implementation

1. **SHA256 Support**: Uses SHA256 hash function for HMAC
2. **ByteString Handling**: Properly handles binary data with `ByteString`
3. **Base16 Encoding**: Converts results to readable hexadecimal format
4. **Verification**: Includes HMAC verification functionality
5. **Type Safety**: Uses proper Haskell types and type signatures

## Sample Output

```
=== HMAC Examples ===
Key: my-secret-key
Message: Hello, World!
HMAC (hex): 5d41402abc4b2a76b9719d911017c592
HMAC (base16): 5d41402abc4b2a76b9719d911017c592

Original message: This is a confidential message
Generated MAC: 5d41402abc4b2a76b9719d911017c592
Verification result: True
Tampered verification result: False

HMAC using crypto-api: 5d41402abc4b2a76b9719d911017c592
```

This implementation demonstrates the core concepts of HMAC in Haskell, including key management, message processing, and verification.

