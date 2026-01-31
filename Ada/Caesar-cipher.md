# Caesar Cipher in Ada

Here's an example implementation of the Caesar cipher algorithm in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;

procedure Caesar_Cipher is
   
   -- Function to encrypt a character using Caesar cipher
   function Encrypt_Char(Char_To_Encrypt : Character; Shift : Integer) return Character is
      Encrypted_Char : Character;
   begin
      if Char_To_Encrypt in 'A'..'Z' then
         -- Handle uppercase letters
         Encrypted_Char := Character'Val(
            (Character'Pos(Char_To_Encrypt) - Character'Pos('A') + Shift) mod 26 + Character'Pos('A')
         );
      elsif Char_To_Encrypt in 'a'..'z' then
         -- Handle lowercase letters
         Encrypted_Char := Character'Val(
            (Character'Pos(Char_To_Encrypt) - Character'Pos('a') + Shift) mod 26 + Character'Pos('a')
         );
      else
         -- Non-alphabetic characters remain unchanged
         Encrypted_Char := Char_To_Encrypt;
      end if;
      
      return Encrypted_Char;
   end Encrypt_Char;
   
   -- Function to decrypt a character using Caesar cipher
   function Decrypt_Char(Char_To_Decrypt : Character; Shift : Integer) return Character is
      Decrypted_Char : Character;
   begin
      if Char_To_Decrypt in 'A'..'Z' then
         -- Handle uppercase letters
         Decrypted_Char := Character'Val(
            (Character'Pos(Char_To_Decrypt) - Character'Pos('A') - Shift + 26) mod 26 + Character'Pos('A')
         );
      elsif Char_To_Decrypt in 'a'..'z' then
         -- Handle lowercase letters
         Decrypted_Char := Character'Val(
            (Character'Pos(Char_To_Decrypt) - Character'Pos('a') - Shift + 26) mod 26 + Character'Pos('a')
         );
      else
         -- Non-alphabetic characters remain unchanged
         Decrypted_Char := Char_To_Decrypt;
      end if;
      
      return Decrypted_Char;
   end Decrypt_Char;
   
   -- Main encryption function for strings
   function Caesar_Encrypt(Text : String; Shift : Integer) return String is
      Result : String(1..Text'Length);
   begin
      for I in Text'Range loop
         Result(I) := Encrypt_Char(Text(I), Shift);
      end loop;
      return Result;
   end Caesar_Encrypt;
   
   -- Main decryption function for strings
   function Caesar_Decrypt(Text : String; Shift : Integer) return String is
      Result : String(1..Text'Length);
   begin
      for I in Text'Range loop
         Result(I) := Decrypt_Char(Text(I), Shift);
      end loop;
      return Result;
   end Caesar_Decrypt;
   
   -- Test the Caesar cipher
   Text : constant String := "Hello World!";
   Shift_Value : constant Integer := 3;
   
   Encrypted_Text : String;
   Decrypted_Text : String;
   
begin
   Put_Line("Caesar Cipher Example");
   Put_Line("====================");
   
   Put_Line("Original text: " & Text);
   Put_Line("Shift value: " & Integer'Image(Shift_Value));
   
   Encrypted_Text := Caesar_Encrypt(Text, Shift_Value);
   Put_Line("Encrypted text: " & Encrypted_Text);
   
   Decrypted_Text := Caesar_Decrypt(Encrypted_Text, Shift_Value);
   Put_Line("Decrypted text: " & Decrypted_Text);
   
   Put_Line("Decryption successful: " & Boolean'Image(Decrypted_Text = Text));
   
end Caesar_Cipher;
```

## Sample Output:
```
Caesar Cipher Example
====================
Original text: Hello World!
Shift value:  3
Encrypted text: Khoor Zruog!
Decrypted text: Hello World!
Decryption successful: TRUE
```

## Key Features:

1. **Character Handling**: Properly handles both uppercase and lowercase letters
2. **Non-alphabetic Characters**: Spaces, punctuation, and numbers remain unchanged
3. **Modular Design**: Separate functions for encryption and decryption
4. **Wrap-around**: Uses modulo arithmetic to handle letter wrapping (Z → C)
5. **Bidirectional**: Supports both encryption and decryption with the same shift value

## How it works:

- Each letter is shifted by the specified number of positions in the alphabet
- Uppercase letters are shifted within the uppercase range
- Lowercase letters are shifted within the lowercase range
- The modulo operation ensures wrapping (e.g., shifting 'Z' by 1 gives 'A')
- Non-alphabetic characters are preserved unchanged

