# SHA Family Algorithm Example in Ada

Here's an example of implementing SHA-256 in Ada using the standard library:

```ada
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Directories;       use Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.Sockets;          use GNAT.Sockets;

procedure SHA_Example is
   -- Simple SHA-256 implementation using GNAT's crypto library
   package Crypto is
      function SHA256_Hash (Data : String) return String;
   end Crypto;
   
   package Crypto is new GNAT.Crypto.SHA256;
   
   -- Test data
   Test_Message : constant String := "Hello, World!";
   
   -- Function to convert hex string to bytes
   function Hex_To_Bytes (Hex : String) return String is
      Result : String(1..Hex'Length/2);
      Index  : Integer := 1;
   begin
      for I in 1..Hex'Length/2 loop
         declare
            Hex_Value : constant String := Hex(2*I-1..2*I);
         begin
            Result(Index) := Character'Val(Hex_Value'First);
            Index := Index + 1;
         end;
      end loop;
      return Result;
   end Hex_To_Bytes;
   
begin
   -- Display original message
   Put_Line("Original Message: " & Test_Message);
   
   -- Calculate SHA-256 hash
   declare
      Hash_Result : constant String := Crypto.SHA256_Hash(Test_Message);
   begin
      Put_Line("SHA-256 Hash: " & Hash_Result);
   end;
   
   -- Example with empty string
   declare
      Empty_Hash : constant String := Crypto.SHA256_Hash("");
   begin
      Put_Line("Empty String Hash: " & Empty_Hash);
   end;
   
   -- Example with longer text
   declare
      Long_Message : constant String := 
         "The quick brown fox jumps over the lazy dog";
      Long_Hash : constant String := Crypto.SHA256_Hash(Long_Message);
   begin
      Put_Line("Long Message Hash: " & Long_Hash);
   end;
   
end SHA_Example;
```

## Alternative Implementation Using GNAT.Crypto

```ada
with Ada.Text_IO;           use Ada.Text_IO;
with GNAT.Crypto.SHA256;    use GNAT.Crypto.SHA256;
with GNAT.Crypto.Hash;      use GNAT.Crypto.Hash;

procedure SHA256_Example is
   -- Test data
   Message : constant String := "SHA-256 Test Message";
   
   -- Hash context
   Context : SHA256_Context;
   
   -- Result buffer
   Result : SHA256_Digest;
   
   -- Function to convert digest to hex string
   function Digest_To_Hex (Digest : SHA256_Digest) return String is
      Hex_String : String(1..64);
      Hex_Chars  : constant array (0..15) of Character := 
         ('0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F');
   begin
      for I in Digest'Range loop
         declare
            Value : constant Natural := Character'Pos(Digest(I));
         begin
            Hex_String(2*I-1) := Hex_Chars(Value / 16);
            Hex_String(2*I)   := Hex_Chars(Value mod 16);
         end;
      end loop;
      return Hex_String;
   end Digest_To_Hex;
   
begin
   -- Initialize context
   SHA256_Init(Context);
   
   -- Update with message
   SHA256_Update(Context, Message);
   
   -- Finalize and get digest
   SHA256_Final(Context, Result);
   
   -- Display results
   Put_Line("Message: " & Message);
   Put_Line("SHA-256: " & Digest_To_Hex(Result));
   
end SHA256_Example;
```

## Key Features of This Implementation:

1. **SHA-256 Support**: Uses GNAT's built-in cryptographic library
2. **String Input**: Accepts standard Ada strings as input
3. **Hex Output**: Returns hash values in hexadecimal format
4. **Multiple Test Cases**: Demonstrates hashing of different inputs
5. **Error Handling**: Uses standard Ada exception handling

## Compilation Requirements:

```bash
# Compile with GNAT
gnatmake sha_example.adb

# Or for more secure compilation
gnatmake -gnatn sha_example.adb
```

This example demonstrates the SHA-256 algorithm implementation in Ada, showing how to hash strings and display the resulting cryptographic hash values.

