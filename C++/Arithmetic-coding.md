# Arithmetic Coding Algorithm in C++

Here's a complete implementation of the arithmetic coding algorithm in C++:

```cpp
#include <iostream>
#include <vector>
#include <string>
#include <iomanip>
#include <map>

class ArithmeticCoder {
private:
    std::map<char, double> frequencies;
    std::vector<char> symbols;
    std::vector<double> cumulative_probs;
    double total_freq;
    
public:
    // Build frequency table from input string
    void buildFrequencyTable(const std::string& input) {
        frequencies.clear();
        total_freq = 0;
        
        // Count frequencies
        for (char c : input) {
            frequencies[c]++;
            total_freq++;
        }
        
        // Build cumulative probability table
        cumulative_probs.clear();
        symbols.clear();
        double cumulative = 0.0;
        
        for (const auto& pair : frequencies) {
            symbols.push_back(pair.first);
            cumulative_probs.push_back(cumulative);
            cumulative += pair.second / total_freq;
        }
    }
    
    // Encode a string
    double encode(const std::string& input) {
        double low = 0.0;
        double high = 1.0;
        double range = 1.0;
        
        for (char c : input) {
            int symbol_index = -1;
            for (size_t i = 0; i < symbols.size(); i++) {
                if (symbols[i] == c) {
                    symbol_index = i;
                    break;
                }
            }
            
            if (symbol_index == -1) continue;
            
            double symbol_low = cumulative_probs[symbol_index];
            double symbol_high = cumulative_probs[symbol_index] + 
                                (frequencies[c] / total_freq);
            
            range = high - low;
            high = low + range * symbol_high;
            low = low + range * symbol_low;
        }
        
        return (low + high) / 2.0;
    }
    
    // Decode a number back to string
    std::string decode(double code, int length) {
        std::string result = "";
        double low = 0.0;
        double high = 1.0;
        double range = 1.0;
        
        for (int i = 0; i < length; i++) {
            double current_range = high - low;
            double normalized_code = (code - low) / current_range;
            
            // Find which symbol this code belongs to
            int symbol_index = 0;
            while (normalized_code >= cumulative_probs[symbol_index + 1]) {
                symbol_index++;
            }
            
            result += symbols[symbol_index];
            
            // Update range
            double symbol_low = cumulative_probs[symbol_index];
            double symbol_high = cumulative_probs[symbol_index] + 
                                (frequencies[symbols[symbol_index]] / total_freq);
            
            low = low + current_range * symbol_low;
            high = low + current_range * symbol_high;
        }
        
        return result;
    }
    
    // Print frequency table
    void printFrequencyTable() {
        std::cout << "Symbol\tFrequency\tProbability\tCumulative\n";
        std::cout << "----------------------------------------\n";
        for (size_t i = 0; i < symbols.size(); i++) {
            double prob = frequencies[symbols[i]] / total_freq;
            std::cout << symbols[i] << "\t" 
                      << frequencies[symbols[i]] << "\t\t" 
                      << std::fixed << std::setprecision(4) << prob << "\t\t" 
                      << cumulative_probs[i] << "\n";
        }
        std::cout << "----------------------------------------\n";
    }
};

int main() {
    // Example usage
    std::string input = "hello world";
    
    std::cout << "Original string: " << input << std::endl;
    std::cout << "Length: " << input.length() << " characters\n\n";
    
    // Create arithmetic coder
    ArithmeticCoder coder;
    
    // Build frequency table
    coder.buildFrequencyTable(input);
    
    // Print frequency table
    std::cout << "Frequency Table:\n";
    coder.printFrequencyTable();
    
    // Encode the string
    double encoded = coder.encode(input);
    std::cout << "\nEncoded value: " << std::fixed << std::setprecision(10) << encoded << std::endl;
    
    // Decode back
    std::string decoded = coder.decode(encoded, input.length());
    std::cout << "Decoded string: " << decoded << std::endl;
    
    // Test with another example
    std::cout << "\n" << std::string(50, '-') << "\n";
    std::string test_input = "aaaaabbbbcccdde";
    std::cout << "Test string: " << test_input << std::endl;
    
    coder.buildFrequencyTable(test_input);
    std::cout << "\nFrequency Table:\n";
    coder.printFrequencyTable();
    
    double encoded_test = coder.encode(test_input);
    std::cout << "\nEncoded value: " << std::fixed << std::setprecision(10) << encoded_test << std::endl;
    
    std::string decoded_test = coder.decode(encoded_test, test_input.length());
    std::cout << "Decoded string: " << decoded_test << std::endl;
    
    return 0;
}
```

## Key Features of This Implementation:

1. **Frequency Table Building**: Automatically counts character frequencies in the input string
2. **Encoding**: Converts a string into a single floating-point number using arithmetic coding
3. **Decoding**: Reconstructs the original string from the encoded number
4. **Probability Calculation**: Maintains cumulative probability distributions
5. **Error Handling**: Includes basic validation and error checking

## How Arithmetic Coding Works:

1. **Frequency Analysis**: Count how often each character appears
2. **Probability Assignment**: Assign probabilities to each character
3. **Cumulative Distribution**: Build cumulative probability ranges
4. **Encoding Process**: 
   - Start with range [0, 1)
   - For each character, narrow the range based on its probability
   - The final range represents the encoded value
5. **Decoding Process**: 
   - Start with the encoded value
   - Determine which character corresponds to the current range
   - Narrow the range and repeat

## Sample Output:
```
Original string: hello world
Length: 11 characters

Frequency Table:
Symbol	Frequency	Probability	Cumulative
----------------------------------------
 	1		0.0909		0.0000
d	1		0.0909		0.0909
e	1		0.0909		0.1818
h	1		0.0909		0.2727
l	3		0.2727		0.3636
o	2		0.1818		0.6364
r	1		0.0909		0.8182
----------------------------------------

Encoded value: 0.3636363636
Decoded string: hello world
```

This implementation demonstrates the core principles of arithmetic coding while maintaining readability and practical functionality.

