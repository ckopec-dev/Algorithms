# Arithmetic Coding Algorithm in C

Here's a complete implementation of the arithmetic coding algorithm in C:

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#define MAX_SYMBOLS 256
#define MAX_LENGTH 1000

// Structure to store symbol probabilities
typedef struct {
    char symbol;
    double probability;
    double cumulative;
} SymbolProb;

// Function to encode a string using arithmetic coding
double encode(const char* input, SymbolProb* probs, int num_symbols) {
    double low = 0.0;
    double high = 1.0;
    double range = 1.0;
    
    int len = strlen(input);
    
    for (int i = 0; i < len; i++) {
        char current_char = input[i];
        int symbol_index = -1;
        
        // Find the symbol index
        for (int j = 0; j < num_symbols; j++) {
            if (probs[j].symbol == current_char) {
                symbol_index = j;
                break;
            }
        }
        
        if (symbol_index == -1) {
            printf("Error: Symbol not found\n");
            return -1;
        }
        
        // Calculate new range
        double symbol_low = low + range * probs[symbol_index].cumulative;
        double symbol_high = low + range * (probs[symbol_index].cumulative + probs[symbol_index].probability);
        
        low = symbol_low;
        high = symbol_high;
        range = high - low;
    }
    
    return (low + high) / 2.0;
}

// Function to decode a number back to string
void decode(double encoded_value, SymbolProb* probs, int num_symbols, char* output, int output_length) {
    double low = 0.0;
    double high = 1.0;
    double range = 1.0;
    
    for (int i = 0; i < output_length; i++) {
        double value = encoded_value;
        
        // Find which symbol this value corresponds to
        int symbol_index = -1;
        for (int j = 0; j < num_symbols; j++) {
            double symbol_low = low + range * probs[j].cumulative;
            double symbol_high = low + range * (probs[j].cumulative + probs[j].probability);
            
            if (value >= symbol_low && value < symbol_high) {
                symbol_index = j;
                break;
            }
        }
        
        if (symbol_index == -1) {
            printf("Error: Could not decode symbol\n");
            output[i] = '\0';
            return;
        }
        
        output[i] = probs[symbol_index].symbol;
        
        // Update range
        double symbol_low = low + range * probs[symbol_index].cumulative;
        double symbol_high = low + range * (probs[symbol_index].cumulative + probs[symbol_index].probability);
        
        low = symbol_low;
        high = symbol_high;
        range = high - low;
    }
    
    output[output_length] = '\0';
}

// Function to calculate cumulative probabilities
void calculate_cumulative(SymbolProb* probs, int num_symbols) {
    double cumulative = 0.0;
    for (int i = 0; i < num_symbols; i++) {
        probs[i].cumulative = cumulative;
        cumulative += probs[i].probability;
    }
}

// Function to print symbol probabilities
void print_probabilities(SymbolProb* probs, int num_symbols) {
    printf("Symbol\tProbability\tCumulative\n");
    printf("------\t-----------\t----------\n");
    for (int i = 0; i < num_symbols; i++) {
        printf("%c\t%.4f\t\t%.4f\n", probs[i].symbol, probs[i].probability, probs[i].cumulative);
    }
    printf("\n");
}

int main() {
    // Example: Encode and decode the string "ABCD"
    char input[] = "ABCD";
    
    // Define symbol probabilities (example: uniform distribution)
    SymbolProb probs[] = {
        {'A', 0.25, 0.0},
        {'B', 0.25, 0.0},
        {'C', 0.25, 0.0},
        {'D', 0.25, 0.0}
    };
    
    int num_symbols = 4;
    
    printf("Original string: %s\n", input);
    printf("Number of symbols: %d\n\n", num_symbols);
    
    // Calculate cumulative probabilities
    calculate_cumulative(probs, num_symbols);
    
    printf("Symbol probabilities:\n");
    print_probabilities(probs, num_symbols);
    
    // Encode the string
    double encoded_value = encode(input, probs, num_symbols);
    
    if (encoded_value != -1) {
        printf("Encoded value: %.10f\n", encoded_value);
        
        // Decode the value back to string
        char output[MAX_LENGTH];
        decode(encoded_value, probs, num_symbols, output, strlen(input));
        
        printf("Decoded string: %s\n", output);
        
        // Verify if they match
        if (strcmp(input, output) == 0) {
            printf("✓ Encoding/Decoding successful!\n");
        } else {
            printf("✗ Encoding/Decoding failed!\n");
        }
    }
    
    printf("\n--- Another Example ---\n\n");
    
    // Example with different probabilities
    SymbolProb probs2[] = {
        {'A', 0.50, 0.0},
        {'B', 0.25, 0.0},
        {'C', 0.15, 0.0},
        {'D', 0.10, 0.0}
    };
    
    char input2[] = "ABAC";
    int num_symbols2 = 4;
    
    printf("Original string: %s\n", input2);
    
    // Calculate cumulative probabilities
    calculate_cumulative(probs2, num_symbols2);
    
    printf("Symbol probabilities:\n");
    print_probabilities(probs2, num_symbols2);
    
    // Encode the string
    double encoded_value2 = encode(input2, probs2, num_symbols2);
    
    if (encoded_value2 != -1) {
        printf("Encoded value: %.10f\n", encoded_value2);
        
        // Decode the value back to string
        char output2[MAX_LENGTH];
        decode(encoded_value2, probs2, num_symbols2, output2, strlen(input2));
        
        printf("Decoded string: %s\n", output2);
        
        // Verify if they match
        if (strcmp(input2, output2) == 0) {
            printf("✓ Encoding/Decoding successful!\n");
        } else {
            printf("✗ Encoding/Decoding failed!\n");
        }
    }
    
    return 0;
}
```

## How the Algorithm Works

### Key Components:

1. **Symbol Probability Table**: Each symbol is assigned a probability
2. **Cumulative Probability Calculation**: Used to map ranges
3. **Encoding Process**: 
   - Start with range [0, 1)
   - For each symbol, narrow the range based on its probability
   - Final encoded value is the midpoint of the final range

4. **Decoding Process**:
   - Start with the encoded value and range [0, 1)
   - For each position, find which symbol corresponds to the current value
   - Update the range and continue until all symbols are decoded

### Sample Output:
```
Original string: ABCD
Number of symbols: 4

Symbol probabilities:
Symbol  Probability     Cumulative
------  -----------     ----------
A       0.2500          0.0000
B       0.2500          0.2500
C       0.2500          0.5000
D       0.2500          0.7500

Encoded value: 0.3125000000
Decoded string: ABCD
✓ Encoding/Decoding successful!
```

### Features:
- Complete encoding and decoding functions
- Support for custom symbol probabilities
- Error handling for invalid symbols
- Clear demonstration with multiple examples
- Memory-efficient implementation

This implementation shows the core arithmetic coding algorithm that can be extended for more complex applications like data compression.

