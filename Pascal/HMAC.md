# HMAC Algorithm in Pascal

Here's an example implementation of HMAC (Hash-based Message Authentication Code) in Pascal using SHA-256:

```pascal
program HMACExample;

{$APPTYPE CONSOLE}

uses
  SysUtils, Classes, StrUtils;

type
  THashFunction = function(const Data: string): string of object;

// SHA-256 helper functions (simplified implementation)
function SHA256(const Data: string): string;
begin
  // This is a simplified placeholder - in practice, you'd use a full SHA-256 implementation
  Result := 'SHA256_HASH_OF_' + Data;
end;

// HMAC implementation
function HMAC(const Key, Message: string; HashFunc: THashFunction): string;
var
  I: Integer;
  K: string;
  ipad, opad, hash1, hash2: string;
begin
  // Ensure key is exactly 64 bytes (512 bits) for SHA-256
  if Length(Key) > 64 then
    K := HashFunc(Key)
  else
    K := Key;
    
  // Pad key to 64 bytes if necessary
  while Length(K) < 64 do
    K := K + #0;
    
  // Create ipad and opad
  ipad := '';
  opad := '';
  
  for I := 1 to 64 do
  begin
    ipad := ipad + Char(Ord(K[I]) xor $36);
    opad := opad + Char(Ord(K[I]) xor $5C);
  end;
  
  // Calculate HMAC
  hash1 := HashFunc(ipad + Message);
  hash2 := HashFunc(opad + hash1);
  
  Result := hash2;
end;

// Alternative more complete HMAC implementation
function HMAC_SHA256(const Key, Message: string): string;
var
  I: Integer;
  K: string;
  ipad, opad: string;
  Hash1, Hash2: string;
begin
  // Key preparation
  if Length(Key) > 64 then
    K := SHA256(Key)  // Hash the key if it's too long
  else
    K := Key;
    
  // Pad key to 64 bytes
  while Length(K) < 64 do
    K := K + #0;
    
  // Create inner and outer padding
  ipad := '';
  opad := '';
  
  for I := 1 to 64 do
  begin
    ipad := ipad + Char(Ord(K[I]) xor $36);
    opad := opad + Char(Ord(K[I]) xor $5C);
  end;
  
  // Calculate HMAC: Hash(opad + Hash(ipad + message))
  Hash1 := SHA256(ipad + Message);
  Hash2 := SHA256(opad + Hash1);
  
  Result := Hash2;
end;

// Main program
var
  Key, Message, Result: string;
begin
  try
    // Example usage
    Key := 'secret_key';
    Message := 'Hello, World!';
    
    Writeln('HMAC Example');
    Writeln('============');
    Writeln('Key: ', Key);
    Writeln('Message: ', Message);
    
    Result := HMAC_SHA256(Key, Message);
    Writeln('HMAC-SHA256: ', Result);
    
    // Test with different message
    Message := 'The quick brown fox jumps over the lazy dog';
    Result := HMAC_SHA256(Key, Message);
    Writeln('Message: ', Message);
    Writeln('HMAC-SHA256: ', Result);
    
    Readln;
  except
    on E: Exception do
      Writeln('Error: ', E.Message);
  end;
end.
```

## Key Components Explained

### 1. **HMAC Algorithm Steps:**
1. **Key Preparation**: If key > block size (64 bytes for SHA-256), hash it first
2. **Padding**: Create inner (`ipad`) and outer (`opad`) padding
3. **Calculation**: `HMAC = Hash(opad + Hash(ipad + message))`

### 2. **Key Features:**
- **Flexible Hash Function**: Uses function pointer for different hash algorithms
- **Key Handling**: Properly handles keys longer than block size
- **Padding Implementation**: Correctly implements XOR operations with 0x36 and 0x5C

### 3. **Usage Example:**
```pascal
// Simple usage
Key := 'my_secret_key';
Message := 'Important data';
HMAC_Result := HMAC_SHA256(Key, Message);
```

**Note**: This is a simplified implementation. For production use, you should use a complete SHA-256 library or cryptographic toolkit, as the SHA-256 function shown is just a placeholder.

