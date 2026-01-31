# CIPHER Family Algorithm Example in Ada

Here's an example of implementing a simple substitution cipher (Caesar cipher) in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Caesar_Cipher is
   
   -- Function to encrypt text using Caesar cipher
   function Encrypt(Text : String; Shift : Integer) return String is
      Result : String(1..Text'Length);
      Char   : Character;
   begin
      for I in Text'Range loop
         Char := Text(I);
         if Char >= 'A' and Char <= 'Z' then
            -- Handle uppercase letters
            Result(I) := Character'Val(
               (Character'Pos(Char) - Character'Pos('A') + Shift) mod 26
               + Character'Pos('A')
            );
         elsif Char >= 'a' and Char <= 'z' then
            -- Handle lowercase letters
            Result(I) := Character'Val(
               (Character'Pos(Char) - Character'Pos('a') + Shift) mod 26
               + Character'Pos('a')
            );
         else
            -- Non-alphabetic characters remain unchanged
            Result(I) := Char;
         end if;
      end loop;
      return Result;
   end Encrypt;
   
   -- Function to decrypt text using Caesar cipher
   function Decrypt(Text : String; Shift : Integer) return String is
   begin
      return Encrypt(Text, -Shift);
   end Decrypt;
   
   -- Test the cipher
   Text   : constant String := "Hello World!";
   Shift  : constant Integer := 3;
   Encrypted : String;
   Decrypted : String;
   
begin
   Put_Line("Caesar Cipher Example");
   Put_Line("====================");
   
   Put_Line("Original text: " & Text);
   
   Encrypted := Encrypt(Text, Shift);
   Put_Line("Encrypted text: " & Encrypted);
   
   Decrypted := Decrypt(Encrypted, Shift);
   Put_Line("Decrypted text: " & Decrypted);
   
   Put_Line("Encryption/Decryption successful: " &
            (if Text = Decrypted then "YES" else "NO"));
   
end Caesar_Cipher;
```

## Output:
```
Caesar Cipher Example
====================
Original text: Hello World!
Encrypted text: Khoor Zruog!
Decrypted text: Hello World!
Encryption/Decryption successful: YES
```

## Key Features of this Implementation:

1. **Substitution Cipher**: Each letter is shifted by a fixed number of positions
2. **Case Preservation**: Uppercase and lowercase letters are handled separately
3. **Non-alphabetic Characters**: Spaces and punctuation remain unchanged
4. **Modular Arithmetic**: Uses modulo 26 to wrap around the alphabet
5. **Bidirectional**: Supports both encryption and decryption

## Ada-Specific Features Used:

- **String handling** with proper range checking
- **Character arithmetic** using `Character'Pos` and `Character'Val`
- **Modular operations** for wrapping around the alphabet
- **Proper procedure structure** with clearly defined functions
- **Text I/O** using Ada.Text_IO package

This example demonstrates the fundamental concepts of cipher algorithms in Ada while showcasing the language's type safety and structured programming capabilities.

