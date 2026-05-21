# MD5 Hashing in C#

Here's an example of implementing MD5 hashing in C#:

```csharp
using System;
using System.Security.Cryptography;
using System.Text;

public class MD5Example
{
    public static string CalculateMD5(string input)
    {
        using (MD5 md5Hash = MD5.Create())
        {
            // Convert the input string to a byte array and compute the hash
            byte[] data = md5Hash.ComputeHash(Encoding.UTF8.GetBytes(input));
            
            // Create a new StringBuilder to collect the bytes
            StringBuilder builder = new StringBuilder();
            
            // Convert each byte of the hash to a hexadecimal string
            for (int i = 0; i < data.Length; i++)
            {
                builder.Append(data[i].ToString("x2"));
            }
            
            return builder.ToString();
        }
    }
    
    public static void Main()
    {
        string input = "Hello, World!";
        string hash = CalculateMD5(input);
        
        Console.WriteLine($"Input: {input}");
        Console.WriteLine($"MD5 Hash: {hash}");
        
        // Example with different inputs
        string[] testInputs = {
            "password123",
            "admin",
            "secret",
            ""
        };
        
        Console.WriteLine("\n--- MD5 Hash Examples ---");
        foreach (string testInput in testInputs)
        {
            Console.WriteLine($"'{testInput}' -> {CalculateMD5(testInput)}");
        }
    }
}
```

## Output
```
Input: Hello, World!
MD5 Hash: 65a8e27d8879283831b664bd8b7f0ad4

--- MD5 Hash Examples ---
'password123' -> 482c811da5d5b4bc6d497ffa98491e38
'admin' -> 21232f297a57a5a743894a0e4a801fc3
'secret' -> 5ebe2294ecd0e0f08eab6cd89d65d009
'' -> d41d8cd98f00b204e9800998ecf8427e
```

## Key Points

- **MD5 Algorithm**: Produces a 128-bit (16-byte) hash value
- **Output Format**: Hexadecimal string representation (32 characters)
- **Security Note**: MD5 is cryptographically broken and should not be used for security purposes
- **Usage**: Primarily for checksums, data integrity verification, or non-security applications
- **Encoding**: Uses UTF-8 encoding for string-to-byte conversion

## Alternative Implementation (Simplified)

```csharp
using System.Security.Cryptography;
using System.Text;

public static string GetMD5Hash(string input)
{
    using (MD5 md5 = MD5.Create())
    {
        byte[] inputBytes = Encoding.UTF8.GetBytes(input);
        byte[] hashBytes = md5.ComputeHash(inputBytes);
        
        return Convert.ToHexString(hashBytes).ToLower();
    }
}
```

**Note**: For security-sensitive applications, consider using SHA-256 or other cryptographically secure hash algorithms instead of MD5.

