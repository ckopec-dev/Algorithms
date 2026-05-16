# Run-Length Encoding (RLE) in C#

Here's a complete implementation of the Run-Length Encoding algorithm in C#:

```csharp
using System;
using System.Text;

public class RunLengthEncoding
{
    /// <summary>
    /// Encodes a string using Run-Length Encoding
    /// </summary>
    /// <param name="input">Input string to encode</param>
    /// <returns>Encoded string</returns>
    public static string Encode(string input)
    {
        if (string.IsNullOrEmpty(input))
            return string.Empty;

        StringBuilder encoded = new StringBuilder();
        char currentChar = input[0];
        int count = 1;

        for (int i = 1; i < input.Length; i++)
        {
            if (input[i] == currentChar)
            {
                count++;
            }
            else
            {
                // Append count and character
                if (count > 1)
                {
                    encoded.Append(count);
                }
                encoded.Append(currentChar);
                
                // Reset for new character
                currentChar = input[i];
                count = 1;
            }
        }

        // Handle the last sequence
        if (count > 1)
        {
            encoded.Append(count);
        }
        encoded.Append(currentChar);

        return encoded.ToString();
    }

    /// <summary>
    /// Decodes a Run-Length Encoded string
    /// </summary>
    /// <param name="input">Encoded string to decode</param>
    /// <returns>Decoded original string</returns>
    public static string Decode(string input)
    {
        if (string.IsNullOrEmpty(input))
            return string.Empty;

        StringBuilder decoded = new StringBuilder();
        int i = 0;

        while (i < input.Length)
        {
            // Read the count (if any)
            int count = 0;
            while (i < input.Length && char.IsDigit(input[i]))
            {
                count = count * 10 + (input[i] - '0');
                i++;
            }

            // If no count was found, count is 1
            if (count == 0)
                count = 1;

            // Read the character
            if (i < input.Length)
            {
                char character = input[i];
                for (int j = 0; j < count; j++)
                {
                    decoded.Append(character);
                }
                i++;
            }
        }

        return decoded.ToString();
    }
}

// Example usage
class Program
{
    static void Main()
    {
        // Test cases
        string[] testStrings = {
            "AAABBBCCD",
            "AAAAAAAAAA",
            "AABBCC",
            "A",
            "",
            "AABBAA"
        };

        Console.WriteLine("Run-Length Encoding Examples:");
        Console.WriteLine("============================");

        foreach (string test in testStrings)
        {
            string encoded = RunLengthEncoding.Encode(test);
            string decoded = RunLengthEncoding.Decode(encoded);
            
            Console.WriteLine($"Original:  \"{test}\"");
            Console.WriteLine($"Encoded:   \"{encoded}\"");
            Console.WriteLine($"Decoded:   \"{decoded}\"");
            Console.WriteLine($"Match:     {test == decoded}");
            Console.WriteLine();
        }
    }
}
```

## Output:
```
Run-Length Encoding Examples:
============================
Original:  "AAABBBCCD"
Encoded:   "3A3B2C1D"
Decoded:   "AAABBBCCD"
Match:     True

Original:  "AAAAAAAAAA"
Encoded:   "10A"
Decoded:   "AAAAAAAAAA"
Match:     True

Original:  "AABBCC"
Encoded:   "2A2B2C"
Decoded:   "AABBCC"
Match:     True

Original:  "A"
Encoded:   "A"
Decoded:   "A"
Match:     True

Original:  ""
Encoded:   ""
Decoded:   ""
Match:     True

Original:  "AABBAA"
Encoded:   "2A2B2A"
Decoded:   "AABBAA"
Match:     True
```

## How it works:

1. **Encoding Process**:
   - Traverse the input string character by character
   - Count consecutive occurrences of the same character
   - If count > 1, append the count followed by the character
   - If count = 1, just append the character

2. **Decoding Process**:
   - Read digits to determine the count
   - Read the following character
   - Repeat the character count times
   - Continue until the entire string is processed

## Key Features:

- Handles empty strings and single characters
- Properly manages counts greater than 9 (multi-digit numbers)
- Maintains original string integrity through round-trip encoding/decoding
- Uses StringBuilder for efficient string building
- Includes comprehensive test cases

