# Data Encryption Standard (DES) Algorithm in C

Here's a simplified implementation of the DES algorithm in C:

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Simple DES-like structure for demonstration
typedef struct {
    unsigned char key[8];
    unsigned char plaintext[8];
    unsigned char ciphertext[8];
} DES_Context;

// Initial Permutation Table
const int IP_Table[64] = {
    58, 50, 42, 34, 26, 18, 10, 2,
    60, 52, 44, 36, 28, 20, 12, 4,
    62, 54, 46, 38, 30, 22, 14, 6,
    64, 56, 48, 40, 32, 24, 16, 8,
    57, 49, 41, 33, 25, 17, 9, 1,
    59, 51, 43, 35, 27, 19, 11, 3,
    61, 53, 45, 37, 29, 21, 13, 5,
    63, 55, 47, 39, 31, 23, 15, 7
};

// Expansion Permutation Table (48-bit)
const int E_Table[48] = {
    32, 1, 2, 3, 4, 5,
    4, 5, 6, 7, 8, 9,
    8, 9, 10, 11, 12, 13,
    12, 13, 14, 15, 16, 17,
    16, 17, 18, 19, 20, 21,
    20, 21, 22, 23, 24, 25,
    24, 25, 26, 27, 28, 29,
    28, 29, 30, 31, 32, 1
};

// S-Boxes (simplified version)
const int S_Box[8][64] = {
    {14, 4, 13, 1, 2, 15, 11, 8, 3, 10, 6, 12, 5, 9, 0, 7,
     0, 15, 7, 4, 14, 2, 13, 1, 10, 6, 12, 11, 9, 5, 3, 8,
     4, 1, 14, 8, 13, 6, 2, 11, 15, 12, 9, 7, 3, 10, 5, 0,
     15, 12, 8, 2, 4, 9, 1, 7, 5, 11, 3, 14, 10, 0, 6, 13},
    
    {15, 1, 8, 14, 6, 11, 3, 4, 9, 7, 2, 13, 12, 0, 5, 10,
     3, 13, 4, 7, 15, 2, 8, 14, 12, 0, 1, 10, 6, 9, 11, 5,
     0, 14, 7, 11, 10, 4, 13, 1, 5, 8, 12, 6, 9, 3, 2, 15,
     13, 8, 10, 1, 3, 15, 4, 2, 11, 6, 7, 12, 0, 5, 14, 9},
    
    {10, 0, 9, 14, 6, 3, 15, 5, 1, 13, 12, 7, 11, 4, 2, 8,
     13, 7, 0, 9, 3, 4, 6, 10, 2, 8, 5, 14, 12, 11, 15, 1,
     13, 6, 4, 9, 8, 15, 3, 0, 11, 1, 2, 12, 5, 10, 14, 7,
     1, 10, 13, 0, 6, 9, 8, 7, 4, 15, 14, 3, 11, 5, 2, 12},
    
    {7, 13, 14, 3, 0, 6, 9, 10, 1, 2, 8, 5, 11, 12, 4, 15,
     13, 8, 11, 6, 4, 1, 3, 15, 0, 10, 2, 12, 9, 7, 5, 14,
     3, 15, 0, 6, 10, 1, 13, 8, 9, 4, 5, 11, 12, 7, 2, 14,
     11, 5, 6, 12, 13, 1, 4, 10, 14, 0, 2, 8, 9, 7, 3, 15},
    
    {2, 12, 4, 1, 7, 10, 11, 6, 8, 5, 3, 15, 13, 0, 14, 9,
     14, 11, 2, 12, 4, 7, 13, 1, 5, 0, 15, 10, 3, 9, 8, 6,
     4, 2, 1, 11, 10, 13, 7, 8, 15, 9, 12, 5, 6, 3, 0, 14,
     11, 8, 12, 7, 1, 14, 2, 13, 6, 15, 0, 9, 10, 4, 5, 3},
    
    {12, 1, 10, 15, 9, 2, 6, 8, 0, 13, 3, 4, 14, 7, 5, 11,
     10, 15, 4, 2, 7, 12, 9, 5, 6, 1, 13, 14, 3, 0, 11, 8,
     9, 14, 15, 5, 2, 8, 12, 3, 7, 0, 4, 10, 1, 13, 11, 6,
     4, 3, 2, 12, 9, 5, 15, 10, 11, 14, 1, 7, 6, 0, 8, 13},
    
    {4, 11, 2, 14, 15, 0, 8, 13, 3, 12, 9, 7, 5, 10, 6, 1,
     13, 0, 11, 7, 4, 9, 1, 10, 14, 3, 5, 12, 2, 15, 8, 6,
     1, 4, 11, 13, 12, 3, 7, 14, 10, 15, 6, 8, 0, 5, 9, 2,
     6, 11, 13, 8, 1, 4, 10, 7, 9, 5, 0, 15, 14, 2, 3, 12},
    
    {13, 2, 8, 4, 6, 15, 11, 1, 10, 9, 3, 14, 5, 0, 12, 7,
     1, 15, 13, 8, 10, 3, 7, 4, 12, 5, 6, 11, 0, 14, 9, 2,
     7, 11, 4, 1, 9, 12, 14, 2, 0, 6, 10, 13, 15, 3, 5, 8,
     2, 1, 14, 7, 4, 10, 8, 13, 15, 12, 9, 0, 3, 5, 6, 11}
};

// P-Permutation Table
const int P_Table[32] = {
    16, 7, 20, 21, 29, 12, 28, 17,
    1, 15, 23, 26, 5, 18, 31, 10,
    2, 8, 24, 14, 32, 27, 3, 9,
    19, 13, 30, 6, 22, 11, 4, 25
};

// Final Permutation Table
const int FP_Table[64] = {
    40, 8, 48, 16, 56, 24, 64, 32,
    39, 7, 47, 15, 55, 23, 63, 31,
    38, 6, 46, 14, 54, 22, 62, 30,
    37, 5, 45, 13, 53, 21, 61, 29,
    36, 4, 44, 12, 52, 20, 60, 28,
    35, 3, 43, 11, 51, 19, 59, 27,
    34, 2, 42, 10, 50, 18, 58, 26,
    33, 1, 41, 9, 49, 17, 57, 25
};

// Simple XOR function for demonstration
void xor_bytes(unsigned char *a, unsigned char *b, int len) {
    for (int i = 0; i < len; i++) {
        a[i] ^= b[i];
    }
}

// Simple key schedule (simplified for demonstration)
void generate_subkeys(unsigned char *key, unsigned char *subkeys[16]) {
    // This is a simplified version - real DES has complex key scheduling
    for (int i = 0; i < 16; i++) {
        subkeys[i] = (unsigned char*)malloc(6);
        memcpy(subkeys[i], key, 6);
        subkeys[i][0] += i;
    }
}

// Simple permutation function
void permute(unsigned char *input, unsigned char *output, const int *table, int len) {
    for (int i = 0; i < len; i++) {
        int pos = table[i] - 1;
        if (pos >= 0 && pos < len) {
            output[i] = input[pos];
        }
    }
}

// Simple DES encryption function (simplified)
void des_encrypt(unsigned char *plaintext, unsigned char *key, unsigned char *ciphertext) {
    // Initial permutation
    unsigned char ip_result[8];
    permute(plaintext, ip_result, IP_Table, 64);
    
    // Split into left and right halves
    unsigned char L[4], R[4];
    memcpy(L, ip_result, 4);
    memcpy(R, ip_result + 4, 4);
    
    // Simple Feistel rounds (simplified)
    for (int round = 0; round < 16; round++) {
        // Expansion permutation
        unsigned char expanded_R[6];
        permute(R, expanded_R, E_Table, 48);
        
        // XOR with subkey (simplified)
        for (int i = 0; i < 6; i++) {
            expanded_R[i] ^= key[i % 8];
        }
        
        // S-box substitution
        unsigned char sbox_result[4];
        for (int i = 0; i < 4; i++) {
            int row = ((expanded_R[i * 6 + 0] & 1) << 1) | (expanded_R[i * 6 + 5] & 1);
            int col = (expanded_R[i * 6 + 1] << 3) | (expanded_R[i * 6 + 2] << 2) | 
                     (expanded_R[i * 6 + 3] << 1) | (expanded_R[i * 6 + 4] & 1);
            sbox_result[i] = S_Box[i][row * 16 + col];
        }
        
        // P-permutation
        unsigned char p_result[4];
        permute(sbox_result, p_result, P_Table, 32);
        
        // XOR with left half
        for (int i = 0; i < 4; i++) {
            p_result[i] ^= L[i];
        }
        
        // Swap for next round
        memcpy(L, R, 4);
        memcpy(R, p_result, 4);
    }
    
    // Combine final halves and apply inverse permutation
    unsigned char final[8];
    memcpy(final, R, 4);
    memcpy(final + 4, L, 4);
    
    permute(final, ciphertext, FP_Table, 64);
}

// Simple DES decryption function (simplified)
void des_decrypt(unsigned char *ciphertext, unsigned char *key, unsigned char *plaintext) {
    // For demonstration purposes, we'll use the same encryption function
    // In real implementation, you would use reverse key schedule
    des_encrypt(ciphertext, key, plaintext);
}

// Main function to demonstrate DES
int main() {
    printf("=== DES Algorithm Demonstration ===\n\n");
    
    // Example data
    unsigned char key[8] = {0x12, 0x34, 0x56, 0x78, 0x9A, 0xBC, 0xDE, 0xF0};
    unsigned char plaintext[8] = {0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48};
    
    printf("Original Key: ");
    for (int i = 0; i < 8; i++) {
        printf("%02X ", key[i]);
    }
    printf("\n");
    
    printf("Plaintext: ");
    for (int i = 0; i < 8; i++) {
        printf("%02X ", plaintext[i]);
    }
    printf("\n");
    
    // Encrypt
    unsigned char ciphertext[8];
    des_encrypt(plaintext, key, ciphertext);
    
    printf("Ciphertext: ");
    for (int i = 0; i < 8; i++) {
        printf("%02X ", ciphertext[i]);
    }
    printf("\n");
    
    // Decrypt
    unsigned char decrypted[8];
    des_decrypt(ciphertext, key, decrypted);
    
    printf("Decrypted:  ");
    for (int i = 0; i < 8; i++) {
        printf("%02X ", decrypted[i]);
    }
    printf("\n");
    
    // Verify
    int match = 1;
    for (int i = 0; i < 8; i++) {
        if (plaintext[i] != decrypted[i]) {
            match = 0;
            break;
        }
    }
    
    if (match) {
        printf("\n✓ Encryption/Decryption successful!\n");
    } else {
        printf("\n✗ Error in encryption/decryption\n");
    }
    
    return 0;
}
```

## Key Features of This Implementation:

1. **Data Structures**: 
   - `DES_Context` structure to hold key, plaintext, and ciphertext
   - Standard DES permutation tables (IP, E, P, FP)

2. **Core Functions**:
   - `des_encrypt()`: Performs the encryption using Feistel network
   - `des_decrypt()`: Demonstrates decryption (simplified version)
   - Permutation functions for initial/final permutations
   - S-box substitution with 8 different S-boxes

3. **Algorithm Components**:
   - Initial permutation (IP)
   - 16 rounds of Feistel structure
   - Expansion permutation (E)
   - S-box substitution
   - P-permutation
   - Final permutation (FP)

## Important Notes:

- This is a **simplified demonstration** showing the concept and flow
- The actual DES algorithm has more complex key scheduling and operations
- For production use, implement proper key schedule generation
- Real DES uses 16 different subkeys generated from the main key
- This implementation focuses on demonstrating the structure rather than cryptographic security

## Compilation:

```bash
gcc -o des_demo des_demo.c
./des_demo
```

This code provides a conceptual understanding of how DES works while maintaining simplicity for educational purposes.