# Data Encryption Standard (DES) Algorithm in C++

Here's a simplified implementation of the DES algorithm in C++:

```cpp
#include <iostream>
#include <bitset>
#include <vector>
#include <string>

class DES {
private:
    // Initial Permutation Table
    static const int IP[64];
    
    // Final Permutation Table (inverse of IP)
    static const int FP[64];
    
    // Expansion Permutation Table
    static const int E[48];
    
    // S-boxes (8 S-boxes, each 4x16)
    static const int S[8][64];
    
    // P-Permutation Table
    static const int P[32];
    
    // PC1 Permutation Table (for key scheduling)
    static const int PC1[56];
    
    // PC2 Permutation Table (for key scheduling)
    static const int PC2[48];
    
    // Left Shift Table
    static const int LS[16];
    
    // Key Schedule
    std::vector<std::bitset<48>> subkeys;
    
    // Helper functions
    static std::bitset<64> permute(const std::bitset<64>& input, const int* table, int size);
    static std::bitset<32> f_function(const std::bitset<32>& right, const std::bitset<48>& key);
    static std::bitset<64> initial_permutation(const std::bitset<64>& input);
    static std::bitset<64> final_permutation(const std::bitset<64>& input);
    static std::bitset<48> expand(const std::bitset<32>& input);
    static std::bitset<32> permute_p(const std::bitset<32>& input);
    static std::bitset<56> compress_key(const std::bitset<64>& key);
    static std::bitset<48> generate_subkey(const std::bitset<56>& key, int round);
    
public:
    DES(const std::bitset<64>& key);
    std::bitset<64> encrypt(const std::bitset<64>& plaintext);
    std::bitset<64> decrypt(const std::bitset<64>& ciphertext);
};

// Initial Permutation Table
const int DES::IP[64] = {
    58, 50, 42, 34, 26, 18, 10, 2,
    60, 52, 44, 36, 28, 20, 12, 4,
    62, 54, 46, 38, 30, 22, 14, 6,
    64, 56, 48, 40, 32, 24, 16, 8,
    57, 49, 41, 33, 25, 17, 9, 1,
    59, 51, 43, 35, 27, 19, 11, 3,
    61, 53, 45, 37, 29, 21, 13, 5,
    63, 55, 47, 39, 31, 23, 15, 7
};

// Final Permutation Table
const int DES::FP[64] = {
    40, 8, 48, 16, 56, 24, 64, 32,
    39, 7, 47, 15, 55, 23, 63, 31,
    38, 6, 46, 14, 54, 22, 62, 30,
    37, 5, 45, 13, 53, 21, 61, 29,
    36, 4, 44, 12, 52, 20, 60, 28,
    35, 3, 43, 11, 51, 19, 59, 27,
    34, 2, 42, 10, 50, 18, 58, 26,
    33, 1, 41, 9, 49, 17, 57, 25
};

// Expansion Permutation Table
const int DES::E[48] = {
    32, 1, 2, 3, 4, 5,
    4, 5, 6, 7, 8, 9,
    8, 9, 10, 11, 12, 13,
    12, 13, 14, 15, 16, 17,
    16, 17, 18, 19, 20, 21,
    20, 21, 22, 23, 24, 25,
    24, 25, 26, 27, 28, 29,
    28, 29, 30, 31, 32, 1
};

// P-Permutation Table
const int DES::P[32] = {
    16, 7, 20, 21, 29, 12, 28, 17,
    1, 15, 23, 26, 5, 18, 31, 10,
    2, 8, 24, 14, 32, 27, 3, 9,
    19, 13, 30, 6, 22, 11, 4, 25
};

// PC1 Permutation Table
const int DES::PC1[56] = {
    57, 49, 41, 33, 25, 17, 9,
    1, 58, 50, 42, 34, 26, 18,
    10, 2, 59, 51, 43, 35, 27,
    19, 11, 3, 60, 52, 44, 36,
    63, 55, 47, 39, 31, 23, 15,
    7, 62, 54, 46, 38, 30, 22,
    14, 6, 61, 53, 45, 37, 29,
    21, 13, 5, 28, 20, 12, 4
};

// PC2 Permutation Table
const int DES::PC2[48] = {
    14, 17, 11, 24, 1, 5,
    3, 28, 15, 6, 21, 10,
    23, 19, 12, 4, 26, 8,
    16, 7, 27, 20, 13, 2,
    41, 52, 31, 37, 47, 55,
    30, 40, 51, 45, 33, 48,
    44, 49, 39, 56, 34, 53,
    46, 42, 50, 36, 29, 32
};

// Left Shift Table
const int DES::LS[16] = {1, 1, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 1};

// S-boxes (simplified for demonstration)
const int DES::S[8][64] = {
    // S1
    {14, 4, 13, 1, 2, 15, 11, 8, 3, 10, 6, 12, 5, 9, 0, 7,
     0, 15, 7, 4, 14, 2, 13, 1, 10, 6, 12, 11, 9, 5, 3, 8,
     4, 1, 14, 8, 13, 6, 2, 11, 15, 12, 9, 7, 3, 10, 5, 0,
     15, 12, 8, 2, 4, 9, 1, 7, 5, 11, 3, 14, 10, 0, 6, 13},
    // S2
    {15, 1, 8, 14, 6, 11, 3, 4, 9, 7, 2, 13, 12, 0, 5, 10,
     3, 13, 4, 7, 15, 2, 8, 1, 14, 12, 6, 5, 10, 11, 9, 0,
     0, 14, 7, 11, 10, 4, 13, 1, 5, 8, 12, 6, 9, 3, 2, 15,
     13, 8, 10, 1, 3, 15, 4, 2, 11, 6, 7, 12, 0, 5, 14, 9},
    // S3
    {10, 0, 9, 14, 6, 3, 15, 5, 1, 13, 12, 7, 11, 4, 2, 8,
     13, 7, 0, 9, 3, 4, 6, 10, 2, 8, 5, 14, 12, 11, 15, 1,
     13, 6, 4, 9, 8, 15, 3, 0, 11, 1, 2, 12, 5, 10, 14, 7,
     1, 10, 13, 0, 6, 9, 8, 7, 4, 15, 14, 3, 11, 5, 2, 12},
    // S4
    {7, 13, 14, 3, 0, 6, 9, 10, 1, 2, 8, 5, 11, 12, 4, 15,
     13, 8, 11, 6, 4, 1, 3, 15, 0, 14, 9, 7, 2, 12, 5, 10,
     3, 15, 0, 6, 10, 1, 13, 8, 9, 4, 5, 11, 12, 7, 2, 14,
     2, 12, 4, 1, 7, 10, 11, 6, 8, 5, 3, 15, 13, 0, 14, 9},
    // S5
    {10, 15, 0, 6, 9, 3, 1, 13, 12, 7, 11, 4, 2, 8, 5, 14,
     11, 3, 15, 1, 4, 10, 12, 0, 13, 5, 2, 7, 9, 14, 6, 8,
     10, 11, 0, 13, 7, 12, 1, 14, 6, 5, 15, 4, 8, 3, 9, 2,
     1, 15, 13, 8, 10, 3, 12, 0, 4, 14, 6, 11, 2, 7, 9, 5},
    // S6
    {15, 1, 13, 14, 10, 6, 4, 2, 0, 8, 5, 12, 11, 3, 7, 9,
     13, 12, 0, 1, 14, 10, 3, 7, 11, 2, 15, 4, 6, 8, 5, 9,
     15, 13, 1, 10, 0, 6, 9, 2, 8, 5, 12, 4, 14, 3, 11, 7,
     0, 14, 10, 4, 15, 11, 12, 13, 2, 1, 5, 6, 3, 9, 8, 7},
    // S7
    {10, 6, 12, 0, 8, 15, 3, 11, 13, 7, 4, 14, 1, 9, 5, 2,
     15, 12, 10, 1, 0, 9, 4, 13, 7, 5, 14, 6, 8, 3, 2, 11,
     10, 15, 6, 11, 13, 0, 12, 1, 4, 8, 5, 14, 3, 7, 2, 9,
     11, 12, 13, 10, 3, 15, 0, 14, 6, 8, 2, 1, 5, 4, 7, 9},
    // S8
    {13, 15, 10, 2, 8, 12, 1, 14, 6, 11, 0, 5, 9, 4, 7, 3,
     14, 12, 10, 15, 1, 8, 0, 11, 7, 3, 5, 9, 6, 4, 2, 13,
     10, 15, 12, 8, 13, 11, 4, 0, 14, 6, 9, 1, 3, 7, 5, 2,
     15, 10, 13, 4, 2, 14, 11, 12, 8, 0, 5, 1, 7, 3, 6, 9}
};

// Constructor
DES::DES(const std::bitset<64>& key) {
    // Generate subkeys for 16 rounds
    std::bitset<56> compressed_key = compress_key(key);
    
    for (int i = 0; i < 16; i++) {
        subkeys.push_back(generate_subkey(compressed_key, i));
    }
}

// Helper function to compress key
std::bitset<56> DES::compress_key(const std::bitset<64>& key) {
    std::bitset<56> compressed;
    for (int i = 0; i < 56; i++) {
        compressed[55 - i] = key[PC1[i] - 1];
    }
    return compressed;
}

// Helper function to generate subkey
std::bitset<48> DES::generate_subkey(const std::bitset<56>& key, int round) {
    std::bitset<28> left, right;
    
    // Split key into left and right parts
    for (int i = 0; i < 28; i++) {
        left[i] = key[i];
        right[i] = key[i + 28];
    }
    
    // Left shift
    for (int i = 0; i < LS[round]; i++) {
        std::bitset<28> temp_left = left;
        std::bitset<28> temp_right = right;
        
        left >>= 1;
        right >>= 1;
        
        left[27] = temp_left[0];
        right[27] = temp_right[0];
    }
    
    // Combine left and right parts
    std::bitset<56> combined;
    for (int i = 0; i < 28; i++) {
        combined[i] = left[i];
        combined[i + 28] = right[i];
    }
    
    // Apply PC-2 permutation to get 48-bit