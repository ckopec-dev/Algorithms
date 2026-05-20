# CIPHER__family Algorithm Example in Pascal

Here's an example of a simple cipher algorithm implementation in Pascal using the Caesar cipher technique:

```pascal
program CipherExample;

uses
  crt;

// Caesar Cipher function - shifts each letter by a given key
function CaesarCipher(text: string; key: integer): string;
var
  i: integer;
  ch: char;
  result: string;
begin
  result := '';
  
  for i := 1 to Length(text) do
  begin
    ch := text[i];
    
    // Handle uppercase letters
    if (ch >= 'A') and (ch <= 'Z') then
    begin
      ch := chr(((ord(ch) - ord('A') + key) mod 26) + ord('A'));
    end
    // Handle lowercase letters
    else if (ch >= 'a') and (ch <= 'z') then
    begin
      ch := chr(((ord(ch) - ord('a') + key) mod 26) + ord('a'));
    end;
    
    result := result + ch;
  end;
  
  CaesarCipher := result;
end;

// Decryption function
function CaesarDecipher(text: string; key: integer): string;
var
  i: integer;
  ch: char;
  result: string;
begin
  result := '';
  
  for i := 1 to Length(text) do
  begin
    ch := text[i];
    
    // Handle uppercase letters
    if (ch >= 'A') and (ch <= 'Z') then
    begin
      ch := chr(((ord(ch) - ord('A') - key + 26) mod 26) + ord('A'));
    end
    // Handle lowercase letters
    else if (ch >= 'a') and (ch <= 'z') then
    begin
      ch := chr(((ord(ch) - ord('a') - key + 26) mod 26) + ord('a'));
    end;
    
    result := result + ch;
  end;
  
  CaesarDecipher := result;
end;

// Main program
var
  originalText, encryptedText, decryptedText: string;
  key: integer;

begin
  clrscr;
  
  writeln('=== CAESAR CIPHER EXAMPLE ===');
  writeln;
  
  // Get input from user
  write('Enter text to encrypt: ');
  readln(originalText);
  
  write('Enter cipher key (0-25): ');
  readln(key);
  
  // Perform encryption
  encryptedText := CaesarCipher(originalText, key);
  writeln('Encrypted text: ', encryptedText);
  
  // Perform decryption
  decryptedText := CaesarDecipher(encryptedText, key);
  writeln('Decrypted text: ', decryptedText);
  
  writeln;
  writeln('Original text matches decrypted text: ', 
          (originalText = decryptedText));
  
  readln;
end.
```

## Sample Output:
```
=== CAESAR CIPHER EXAMPLE ===

Enter text to encrypt: Hello World
Enter cipher key (0-25): 3
Encrypted text: Khoor Zruog
Decrypted text: Hello World

Original text matches decrypted text: TRUE
```

## Key Features of This Cipher Implementation:

- **Caesar Cipher**: Shifts each letter by a fixed number of positions
- **Case Preservation**: Maintains uppercase and lowercase letters
- **Non-alphabetic Characters**: Leaves spaces, punctuation unchanged
- **Modular Arithmetic**: Uses modulo operation to wrap around the alphabet
- **Bidirectional**: Supports both encryption and decryption

This example demonstrates the fundamental principles of cipher algorithms in Pascal, showing how character manipulation and modular arithmetic can be used to implement basic encryption techniques.

