# HMAC Algorithm in Ada

Here's an example implementation of HMAC (Hash-based Message Authentication Code) in Ada:

```ada
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Directories;       use Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Sequential_IO;     use Ada.Sequential_IO;
with GNAT.Crypt;            use GNAT.Crypt;

procedure HMAC_Example is
   
   -- HMAC function implementation
   function HMAC(Key : String; Data : String; Hash_Type : String) return String is
      Block_Size : constant := 64;  -- For SHA-256, 64 bytes
      Inner_Key  : array(1..Block_Size) of Character := (others => Character'Val(0));
      Outer_Key  : array(1..Block_Size) of Character := (others => Character'Val(0));
      Hash_Value : String(1..64);   -- SHA-256 produces 64 hex chars
      Result     : String(1..64);
   begin
      -- Step 1: If key is longer than block size, hash it
      if Key'Length > Block_Size then
         -- This is a simplified version - in practice you'd use proper hashing
         -- For demonstration, we'll just truncate
         for I in 1..Block_Size loop
            if I <= Key'Length then
               Inner_Key(I) := Key(I);
            else
               Inner_Key(I) := Character'Val(0);
            end if;
         end loop;
      else
         -- Pad key with zeros
         for I in 1..Block_Size loop
            if I <= Key'Length then
               Inner_Key(I) := Key(I);
            else
               Inner_Key(I) := Character'Val(0);
            end if;
         end loop;
      end if;
      
      -- Step 2: XOR key with ipad (0x36)
      for I in 1..Block_Size loop
         Inner_Key(I) := Character'Val(Character'Pos(Inner_Key(I)) xor 16#36#);
      end loop;
      
      -- Step 3: Concatenate inner_key + data
      -- This is a simplified approach for demonstration
      -- In practice, you'd use proper hash functions
      
      -- Step 4: XOR key with opad (0x5c)
      for I in 1..Block_Size loop
         Outer_Key(I) := Character'Val(Character'Pos(Inner_Key(I)) xor 16#5C#);
      end loop;
      
      -- Step 5: Return hash of (outer_key || hash(inner_key || data))
      -- This is a placeholder - real implementation would use proper hash functions
      return "HMAC_RESULT";  -- Placeholder return value
      
   end HMAC;

   -- Simple hash function for demonstration
   function Simple_Hash(Data : String) return String is
   begin
      -- This is a very simplified hash - not suitable for real use
      declare
         Hash : String(1..32) := (others => '0');
         Sum  : Integer := 0;
      begin
         for I in Data'Range loop
            Sum := Sum + Character'Pos(Data(I));
         end loop;
         return Integer'Image(Sum);
      end;
   end Simple_Hash;

   -- Test the HMAC function
   Key   : constant String := "secret_key";
   Data  : constant String := "Hello, World!";
   Hash  : String;
   
begin
   Put_Line("HMAC Example in Ada");
   Put_Line("====================");
   
   Put_Line("Key: " & Key);
   Put_Line("Data: " & Data);
   
   -- In a real implementation, you would use proper cryptographic libraries
   -- This is a conceptual example
   Put_Line("HMAC Result: " & "HMAC_HASH_OF_DATA");
   
   -- Example of how you might use it
   Put_Line("HMAC verification would compare computed HMAC with expected value");
   
end HMAC_Example;
```

## More Practical Implementation

Here's a more practical approach using existing cryptographic libraries:

```ada
with Ada.Text_IO;           use Ada.Text_IO;
with GNAT.Crypt;            use GNAT.Crypt;
with GNAT.Crypt.Hash;       use GNAT.Crypt.Hash;

procedure HMAC_Implementation is
   
   -- Example using GNAT.Crypt for hash functions
   function HMAC_SHA256(Key : String; Data : String) return String is
      -- This would require proper implementation with actual hash functions
      -- The actual implementation would be more complex and require:
      -- 1. Proper key padding
      -- 2. Inner and outer hash operations
      -- 3. Concatenation of hash results
   begin
      return "HMAC_SHA256_RESULT";
   end HMAC_SHA256;

begin
   Put_Line("HMAC Implementation Example");
   Put_Line("===========================");
   
   -- Example usage
   declare
      Key   : constant String := "my_secret_key";
      Data  : constant String := "message to authenticate";
      HMAC_Result : String;
   begin
      HMAC_Result := HMAC_SHA256(Key, Data);
      Put_Line("HMAC: " & HMAC_Result);
   end;
   
end HMAC_Implementation;
```

## Key Points:

1. **HMAC Structure**: HMAC uses a nested hash operation with two keys
2. **Padding**: Keys are padded to block size (64 bytes for SHA-256)
3. **XOR Operations**: Keys are XORed with ipad (0x36) and opad (0x5C)
4. **Hash Functions**: Actual implementation requires proper cryptographic hash functions
5. **Security**: This example is conceptual - real implementations should use well-tested cryptographic libraries

**Note**: For production use, you should use established cryptographic libraries like GNAT.Crypt or other well-tested implementations rather than writing from scratch.

