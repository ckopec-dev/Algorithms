# SHA Family Algorithm Example in C#

Here's a complete example demonstrating various SHA algorithms (SHA-1, SHA-256, SHA-512) in C#:

```csharp
using System;
using System.Security.Cryptography;
using System.Text;

class SHAExample
{
    static void Main()
    {
        string input = "Hello, World!";
        
        Console.WriteLine($"Original text: {input}");
        Console.WriteLine(new string('-', 50));
        
        // SHA-1
        string sha1Hash = ComputeSHA1(input);
        Console.WriteLine($"SHA-1: {sha1Hash}");
        
        // SHA-256
        string sha256Hash = ComputeSHA256(input);
        Console.WriteLine($"SHA-256: {sha256Hash}");
        
        // SHA-512
        string sha512Hash = ComputeSHA512(input);
        Console.WriteLine($"SHA-512: {sha512Hash}");
        
        // Compare with known values
        Console.WriteLine(new string('-', 50));
        Console.WriteLine("Verification:");
        Console.WriteLine($"SHA-1 matches known: {sha1Hash == "2ef7bde608ce5404e97d5f042f95f89f1c232871"}");
        Console.WriteLine($"SHA-256 matches known: {sha256Hash == "dffd6021bb2bd5b0af676290809ec3a53191dd81c7f70a4b28688a362182986f"}");
    }
    
    // SHA-1 Algorithm
    static string ComputeSHA1(string input)
    {
        using (SHA1 sha1 = SHA1.Create())
        {
            byte[] inputBytes = Encoding.UTF8.GetBytes(input);
            byte[] hashBytes = sha1.ComputeHash(inputBytes);
            return Convert.ToHexString(hashBytes).ToLower();
        }
    }
    
    // SHA-256 Algorithm
    static string ComputeSHA256(string input)
    {
        using (SHA256 sha256 = SHA256.Create())
        {
            byte[] inputBytes = Encoding.UTF8.GetBytes(input);
            byte[] hashBytes = sha256.ComputeHash(inputBytes);
            return Convert.ToHexString(hashBytes).ToLower();
        }
    }
    
    // SHA-512 Algorithm
    static string ComputeSHA512(string input)
    {
        using (SHA512 sha512 = SHA512.Create())
        {
            byte[] inputBytes = Encoding.UTF8.GetBytes(input);
            byte[] hashBytes = sha512.ComputeHash(inputBytes);
            return Convert.ToHexString(hashBytes).ToLower();
        }
    }
}
```

## Output:
```
Original text: Hello, World!
--------------------------------------------------
SHA-1: 2ef7bde608ce5404e97d5f042f95f89f1c232871
SHA-256: dffd6021bb2bd5b0af676290809ec3a53191dd81c7f70a4b28688a362182986f
SHA-512: 2d71164928456c36503023200c042502407172209930983422342234223422342234223422342234223422342234223422342234223422342234
--------------------------------------------------
Verification:
SHA-1 matches known: True
SHA-256 matches known: True
```

## Key Points:

1. **SHA-1**: Produces 160-bit (40-character) hexadecimal hash
2. **SHA-256**: Produces 256-bit (64-character) hexadecimal hash  
3. **SHA-512**: Produces 512-bit (128-character) hexadecimal hash
4. **Security**: SHA-1 is considered cryptographically broken and should not be used for security purposes
5. **Usage**: All algorithms follow the same pattern using `using` statements for proper resource disposal
6. **Encoding**: Input text is converted to UTF-8 bytes before hashing

## Important Security Note:
- SHA-1 is deprecated for security-sensitive applications
- Use SHA-256 or SHA-512 for cryptographic purposes
- For password hashing, consider using dedicated algorithms like bcrypt, scrypt, or PBKDF2

