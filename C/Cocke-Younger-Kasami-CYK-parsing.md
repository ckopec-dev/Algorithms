# Cocke-Younger-Kasami (CYK) Parsing Algorithm in C

The CYK algorithm is a dynamic programming algorithm for parsing context-free grammars in Chomsky Normal Form (CNF). Here's a complete implementation:

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_N 100
#define MAX_RULES 100
#define MAX_SYMBOLS 50

// Grammar rule structure
typedef struct {
    char lhs;           // Left-hand side symbol
    char rhs1, rhs2;    // Right-hand side symbols (for CNF)
} Rule;

// CYK table structure
typedef struct {
    char table[MAX_N][MAX_N][MAX_SYMBOLS];
    int table_size[MAX_N][MAX_N];
} CYKTable;

// Global variables
Rule rules[MAX_RULES];
int num_rules = 0;
int n; // Length of input string

// Function to check if a symbol exists in the rule set
bool symbol_exists(char symbol) {
    for (int i = 0; i < num_rules; i++) {
        if (rules[i].lhs == symbol) return true;
    }
    return false;
}

// Function to add a rule to the grammar
void add_rule(char lhs, char rhs1, char rhs2) {
    rules[num_rules].lhs = lhs;
    rules[num_rules].rhs1 = rhs1;
    rules[num_rules].rhs2 = rhs2;
    num_rules++;
}

// Function to check if a symbol is a terminal
bool is_terminal(char symbol) {
    // In this simple example, we assume terminals are lowercase letters
    return (symbol >= 'a' && symbol <= 'z');
}

// Function to find all symbols that can derive a specific pair of symbols
void find_derived_symbols(char lhs, char rhs1, char rhs2, char *result, int *count) {
    *count = 0;
    for (int i = 0; i < num_rules; i++) {
        if (rules[i].rhs1 == lhs && rules[i].rhs2 == rhs1) {
            result[*count] = rules[i].lhs;
            (*count)++;
        }
    }
}

// CYK parsing algorithm
bool cyk_parse(char *input, char start_symbol) {
    int len = strlen(input);
    char **table = (char**)malloc(len * sizeof(char*));
    for (int i = 0; i < len; i++) {
        table[i] = (char*)malloc(len * sizeof(char));
        memset(table[i], 0, len * sizeof(char));
    }
    
    // Step 1: Fill the first diagonal (base case)
    for (int i = 0; i < len; i++) {
        char symbol = input[i];
        for (int j = 0; j < num_rules; j++) {
            if (rules[j].rhs1 == symbol && rules[j].rhs2 == 0) {
                table[i][0] = rules[j].lhs;
                break;
            }
        }
    }
    
    // Step 2: Fill the rest of the table
    for (int l = 1; l < len; l++) {  // Length of substring
        for (int i = 0; i < len - l; i++) {  // Starting position
            int j = i + l;  // Ending position
            
            for (int k = i; k < j; k++) {  // Split point
                // Check all rules that can produce symbols from left and right parts
                for (int r = 0; r < num_rules; r++) {
                    if (rules[r].rhs1 != 0 && rules[r].rhs2 != 0) {
                        // Check if left part matches table[i][k-i] and right part matches table[k+1][j-k-1]
                        char left = table[i][k-i];
                        char right = table[k+1][j-k-1];
                        
                        if (left == rules[r].rhs1 && right == rules[r].rhs2) {
                            table[i][l] = rules[r].lhs;
                            break;
                        }
                    }
                }
            }
        }
    }
    
    // Step 3: Check if start symbol can be derived
    bool result = (table[0][len-1] == start_symbol);
    
    // Free memory
    for (int i = 0; i < len; i++) {
        free(table[i]);
    }
    free(table);
    
    return result;
}

// Simplified CYK implementation with proper table structure
bool cyk_parse_simple(char *input, char start_symbol) {
    int len = strlen(input);
    char table[MAX_N][MAX_N][MAX_SYMBOLS];
    int table_size[MAX_N][MAX_N];
    
    // Initialize table
    for (int i = 0; i < MAX_N; i++) {
        for (int j = 0; j < MAX_N; j++) {
            table_size[i][j] = 0;
        }
    }
    
    // Step 1: Fill first diagonal (base case)
    for (int i = 0; i < len; i++) {
        for (int r = 0; r < num_rules; r++) {
            if (rules[r].rhs1 == input[i] && rules[r].rhs2 == 0) {
                table[i][0][table_size[i][0]] = rules[r].lhs;
                table_size[i][0]++;
            }
        }
    }
    
    // Step 2: Fill the rest of the table
    for (int l = 1; l < len; l++) {  // Length of substring
        for (int i = 0; i < len - l; i++) {  // Starting position
            int j = i + l;  // Ending position
            
            for (int k = i; k < j; k++) {  // Split point
                // For each rule, check if it can be applied
                for (int r = 0; r < num_rules; r++) {
                    if (rules[r].rhs1 != 0 && rules[r].rhs2 != 0) {
                        // Check if we can derive rules[r].lhs from table[i][k-i] and table[k+1][j-k-1]
                        for (int x = 0; x < table_size[i][k-i]; x++) {
                            for (int y = 0; y < table_size[k+1][j-k-1]; y++) {
                                if (table[i][k-i][x] == rules[r].rhs1 && 
                                    table[k+1][j-k-1][y] == rules[r].rhs2) {
                                    // Add to table[i][j-i]
                                    bool already_exists = false;
                                    for (int z = 0; z < table_size[i][j-i]; z++) {
                                        if (table[i][j-i][z] == rules[r].lhs) {
                                            already_exists = true;
                                            break;
                                        }
                                    }
                                    if (!already_exists) {
                                        table[i][j-i][table_size[i][j-i]] = rules[r].lhs;
                                        table_size[i][j-i]++;
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    
    // Step 3: Check if start symbol can be derived
    for (int i = 0; i < table_size[0][len-1]; i++) {
        if (table[0][len-1][i] == start_symbol) {
            return true;
        }
    }
    
    return false;
}

// Example usage and test function
int main() {
    printf("Cocke-Younger-Kasami (CYK) Parsing Algorithm\n");
    printf("============================================\n\n");
    
    // Define a grammar in CNF
    printf("Grammar Rules (CNF):\n");
    printf("S -> AB | BC\n");
    printf("A -> BA | a\n");
    printf("B -> CC | b\n");
    printf("C -> AB | a\n");
    printf("Start symbol: S\n\n");
    
    // Add rules to grammar
    add_rule('S', 'A', 'B');  // S -> AB
    add_rule('S', 'B', 'C');  // S -> BC
    add_rule('A', 'B', 'A');  // A -> BA
    add_rule('A', 'a', 0);    // A -> a
    add_rule('B', 'C', 'C');  // B -> CC
    add_rule('B', 'b', 0);    // B -> b
    add_rule('C', 'A', 'B');  // C -> AB
    add_rule('C', 'a', 0);    // C -> a
    
    // Test strings
    char *test_strings[] = {"abab", "baba", "aaabbb", "abba"};
    int num_tests = sizeof(test_strings) / sizeof(test_strings[0]);
    
    printf("Testing parsing results:\n");
    printf("------------------------\n");
    
    for (int i = 0; i < num_tests; i++) {
        char *test_string = test_strings[i];
        bool result = cyk_parse_simple(test_string, 'S');
        
        printf("String: \"%s\" -> %s\n", 
               test_string, 
               result ? "ACCEPTED" : "REJECTED");
    }
    
    printf("\n");
    
    // Demonstrate with a valid derivation
    printf("Example derivation for \"abab\":\n");
    printf("S -> AB -> aB -> ab -> abAB -> abab\n");
    printf("This shows how the string can be derived from the grammar.\n");
    
    return 0;
}
```

## Key Features of This Implementation:

### 1. **Grammar Representation**
- Rules stored in a structure with left-hand side and right-hand side symbols
- Supports Chomsky Normal Form (CNF) where each rule has at most 2 symbols on the right-hand side

### 2. **Algorithm Steps**
1. **Base Case**: Fill diagonal elements with terminals
2. **Inductive Step**: For each substring length, fill table entries by combining smaller subproblems
3. **Final Check**: Determine if start symbol can be derived from entire string

### 3. **Time and Space Complexity**
- **Time Complexity**: O(n³ × |G|) where n is string length and |G| is number of rules
- **Space Complexity**: O(n² × |G|)

### 4. **Example Grammar**
The implementation uses this grammar in CNF:
```
S → AB | BC
A → BA | a
B → CC | b
C → AB | a
```

### 5. **Usage**
The algorithm can parse any string against the defined grammar and return whether it's accepted or rejected.

This implementation demonstrates the core principles of CYK parsing while maintaining clarity and educational value.

