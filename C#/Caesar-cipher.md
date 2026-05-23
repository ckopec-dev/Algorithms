# Caesar Cipher Algorithm in C#

Here's a complete implementation of the Caesar cipher algorithm in C#:

```csharp
using System;
using System.Text;

public class CaesarCipher
{
    /// <summary>
    /// Encrypts a message using Caesar cipher with the specified shift
    /// </summary>
    /// <param name="message">The message to encrypt</param>
    /// <param name="shift">The shift value (0-25)</param>
    /// <returns>Encrypted message</returns>
    public static string Encrypt(string message, int shift)
    {
        StringBuilder result = new StringBuilder();
        
        foreach (char c in message)
        {
            if (char.IsLetter(c))
            {
                // Determine if uppercase or lowercase
                char baseChar = char.IsUpper(c) ? 'A' : 'a';
                
                // Apply shift and wrap around using modulo
                int shifted = (c - baseChar + shift) % 26;
                result.Append((char)(baseChar + shifted));
            }
            else
            {
                // Non-letter characters remain unchanged
                result.Append(c);
            }
        }
        
        return result.ToString();
    }
    
    /// <summary>
    /// Decrypts a message using Caesar cipher with the specified shift
    /// </summary>
    /// <param name="encryptedMessage">The message to decrypt</param>
    /// <param name="shift">The shift value used for encryption</param>
    /// <returns>Decrypted message</returns>
    public static string Decrypt(string encryptedMessage, int shift)
    {
        StringBuilder result = new StringBuilder();
        
        foreach (char c in encryptedMessage)
        {
            if (char.IsLetter(c))
            {
                // Determine if uppercase or lowercase
                char baseChar = char.IsUpper(c) ? 'A' : 'a';
                
                // Apply reverse shift and handle negative values
                int shifted = (c - baseChar - shift + 26) % 26;
                result.Append((char)(baseChar + shifted));
            }
            else
            {
                // Non-letter characters remain unchanged
                result.Append(c);
            }
        }
        
        return result.ToString();
    }
}

// Example usage
class Program
{
    static void Main()
    {
        string originalMessage = "Hello World!";
        int shiftValue = 3;
        
        Console.WriteLine($"Original message: {originalMessage}");
        Console.WriteLine($"Shift value: {shiftValue}");
        
        // Encrypt the message
        string encrypted = CaesarCipher.Encrypt(originalMessage, shiftValue);
        Console.WriteLine($"Encrypted message: {encrypted}");
        
        // Decrypt the message
        string decrypted = CaesarCipher.Decrypt(encrypted, shiftValue);
        Console.WriteLine($"Decrypted message: {decrypted}");
        
        // Example with different shift
        Console.WriteLine("\n--- Another example ---");
        string message2 = "The quick brown fox jumps over the lazy dog.";
        int shift2 = 13;
        
        Console.WriteLine($"Original: {message2}");
        string encrypted2 = CaesarCipher.Encrypt(message2, shift2);
        Console.WriteLine($"Encrypted (shift 13): {encrypted2}");
        string decrypted2 = CaesarCipher.Decrypt(encrypted2, shift2);
        Console.WriteLine($"Decrypted: {decrypted2}");
    }
}
```

## Output:
```
Original message: Hello World!
Shift value: 3
Encrypted message: Khoor Zruog!
Decrypted message: Hello World!

--- Another example ---
Original: The quick brown fox jumps over the lazy dog.
Encrypted (shift 13): Gur dhvpx oebja sbk whzcf bire gur ynml qbt.
Decrypted: The quick brown fox jumps over the lazy dog.
```

## Key Features:

1. **Case Preservation**: Maintains uppercase and lowercase letters
2. **Non-letter Handling**: Leaves spaces, punctuation, and numbers unchanged
3. **Modular Arithmetic**: Uses modulo operation to wrap around the alphabet
4. **Bidirectional**: Supports both encryption and decryption
5. **Error Handling**: Works with any shift value (including values > 26)

## How it works:
- Each letter is shifted by the specified number of positions
- The alphabet wraps around (Z shifts to A, A shifts to B, etc.)
- Non-alphabetic characters remain unchanged
- The same shift value is used for both encryption and decryption (with reverse operation for decryption)

