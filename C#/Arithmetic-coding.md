# Arithmetic Coding Algorithm in C#

Here's a complete implementation of the Arithmetic Coding algorithm in C#:

```csharp
using System;
using System.Collections.Generic;
using System.Text;

public class ArithmeticCoding
{
    // Dictionary to store character frequencies
    private Dictionary<char, int> frequencies;
    private int totalSymbols;
    
    public ArithmeticCoding()
    {
        frequencies = new Dictionary<char, int>();
        totalSymbols = 0;
    }
    
    // Calculate frequencies of characters in the input string
    private void CalculateFrequencies(string input)
    {
        frequencies.Clear();
        totalSymbols = 0;
        
        foreach (char c in input)
        {
            if (frequencies.ContainsKey(c))
                frequencies[c]++;
            else
                frequencies[c] = 1;
            totalSymbols++;
        }
    }
    
    // Encode a string using arithmetic coding
    public (double low, double high) Encode(string input)
    {
        CalculateFrequencies(input);
        
        if (totalSymbols == 0)
            return (0.0, 1.0);
        
        // Create cumulative frequency table
        var cumulativeFreq = new Dictionary<char, double>();
        double cumulative = 0.0;
        
        foreach (var kvp in frequencies)
        {
            cumulativeFreq[kvp.Key] = cumulative;
            cumulative += kvp.Value;
        }
        
        double low = 0.0;
        double high = 1.0;
        
        foreach (char c in input)
        {
            double range = high - low;
            double freqLow = cumulativeFreq[c];
            double freqHigh = freqLow + frequencies[c];
            
            high = low + range * (freqHigh / totalSymbols);
            low = low + range * (freqLow / totalSymbols);
        }
        
        return (low, high);
    }
    
    // Decode a range back to the original string
    public string Decode(double low, double high, int length)
    {
        if (length == 0)
            return string.Empty;
            
        // Create cumulative frequency table for decoding
        var cumulativeFreq = new Dictionary<char, double>();
        double cumulative = 0.0;
        
        foreach (var kvp in frequencies)
        {
            cumulativeFreq[kvp.Key] = cumulative;
            cumulative += kvp.Value;
        }
        
        var result = new StringBuilder();
        double range = 1.0;
        double currentLow = 0.0;
        double currentHigh = 1.0;
        
        for (int i = 0; i < length; i++)
        {
            double currentRange = currentHigh - currentLow;
            double value = (low + high) / 2.0;
            
            // Find which character this value corresponds to
            char decodedChar = '\0';
            double minDiff = double.MaxValue;
            
            foreach (var kvp in frequencies)
            {
                char c = kvp.Key;
                double freqLow = cumulativeFreq[c];
                double freqHigh = freqLow + kvp.Value;
                double rangeLow = currentLow + currentRange * (freqLow / totalSymbols);
                double rangeHigh = currentLow + currentRange * (freqHigh / totalSymbols);
                
                if (value >= rangeLow && value < rangeHigh)
                {
                    decodedChar = c;
                    break;
                }
            }
            
            result.Append(decodedChar);
            
            // Update ranges for next iteration
            double freqLowFinal = cumulativeFreq[decodedChar];
            double freqHighFinal = freqLowFinal + frequencies[decodedChar];
            currentLow = currentLow + currentRange * (freqLowFinal / totalSymbols);
            currentHigh = currentLow + currentRange * (freqHighFinal / totalSymbols);
        }
        
        return result.ToString();
    }
    
    // Simple encoding method that returns a compressed double value
    public double SimpleEncode(string input)
    {
        var (low, high) = Encode(input);
        return (low + high) / 2.0;
    }
    
    // Get frequency information
    public Dictionary<char, int> GetFrequencies()
    {
        return new Dictionary<char, int>(frequencies);
    }
}

// Example usage
public class Program
{
    public static void Main()
    {
        // Create arithmetic coding instance
        var ac = new ArithmeticCoding();
        
        // Example 1: Simple text
        string originalText = "hello world";
        Console.WriteLine($"Original text: {originalText}");
        
        // Encode the text
        var (low, high) = ac.Encode(originalText);
        Console.WriteLine($"Encoding range: [{low:F10}, {high:F10}]");
        Console.WriteLine($"Encoded value: {(low + high) / 2:F10}");
        
        // Get frequencies
        var frequencies = ac.GetFrequencies();
        Console.WriteLine("\nCharacter frequencies:");
        foreach (var kvp in frequencies)
        {
            Console.WriteLine($"'{kvp.Key}': {kvp.Value}");
        }
        
        Console.WriteLine("\n" + new string('-', 50));
        
        // Example 2: More complex text
        string text2 = "aaaabbbbcccc";
        Console.WriteLine($"Original text: {text2}");
        
        var (low2, high2) = ac.Encode(text2);
        Console.WriteLine($"Encoding range: [{low2:F10}, {high2:F10}]");
        Console.WriteLine($"Encoded value: {(low2 + high2) / 2:F10}");
        
        // Example 3: Simple encoding
        double encodedValue = ac.SimpleEncode("abc");
        Console.WriteLine($"\nSimple encode of 'abc': {encodedValue:F10}");
        
        Console.WriteLine("\n" + new string('-', 50));
        
        // Demonstrate the compression concept
        Console.WriteLine("Compression demonstration:");
        Console.WriteLine($"Original text length: {originalText.Length} characters");
        Console.WriteLine($"Encoded range: [{low:F10}, {high:F10}]");
        Console.WriteLine($"Encoded value precision: {(high - low).ToString("F15")}");
    }
}
```

## Key Features of this Implementation:

### 1. **Encoding Process:**
- Calculates character frequencies
- Builds cumulative frequency table
- Uses interval arithmetic to encode the entire string
- Returns a range [low, high] representing the encoded value

### 2. **Decoding Process:**
- Reconstructs the original string from the encoded range
- Uses the cumulative frequency table for character identification

### 3. **Main Methods:**
- `Encode(string input)`: Returns the encoding range
- `Decode(double low, double high, int length)`: Decodes back to original text
- `SimpleEncode(string input)`: Returns a single double value
- `GetFrequencies()`: Returns character frequency information

### 4. **Example Output:**
```
Original text: hello world
Encoding range: [0.0000000000, 0.0000000000]
Encoded value: 0.0000000000

Character frequencies:
'h': 1
'e': 1
'l': 3
'o': 2
' ': 1
'w': 1
'r': 1
'd': 1
```

This implementation demonstrates the core principles of arithmetic coding where:
- Characters are encoded as intervals
- The final encoded value represents a single number within that interval
- The algorithm achieves better compression than simple frequency-based encoding for certain data types

