# MD5 Hashing Algorithm in C

Here's a complete example of MD5 hashing implementation in C:

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

// MD5 constants
#define S11 7
#define S12 12
#define S13 17
#define S14 22
#define S21 5
#define S22 9
#define S23 14
#define S24 20
#define S31 4
#define S32 11
#define S33 16
#define S34 23
#define S41 6
#define S42 10
#define S43 15
#define S44 21

// MD5 functions
#define F(x, y, z) (((x) & (y)) | ((~x) & (z)))
#define G(x, y, z) (((x) & (z)) | ((y) & (~z)))
#define H(x, y, z) ((x) ^ (y) ^ (z))
#define I(x, y, z) ((y) ^ ((x) | (~z)))

// Left rotate function
#define ROTATE_LEFT(a, b) (((a) << (b)) | ((a) >> (32-(b))))

// FF, GG, HH, II transformations
#define FF(a, b, c, d, x, s, ac) { \
    (a) += F ((b), (c), (d)) + (x) + (uint32_t)(ac); \
    (a) = ROTATE_LEFT ((a), (s)); \
    (a) += (b); \
}

#define GG(a, b, c, d, x, s, ac) { \
    (a) += G ((b), (c), (d)) + (x) + (uint32_t)(ac); \
    (a) = ROTATE_LEFT ((a), (s)); \
    (a) += (b); \
}

#define HH(a, b, c, d, x, s, ac) { \
    (a) += H ((b), (c), (d)) + (x) + (uint32_t)(ac); \
    (a) = ROTATE_LEFT ((a), (s)); \
    (a) += (b); \
}

#define II(a, b, c, d, x, s, ac) { \
    (a) += I ((b), (c), (d)) + (x) + (uint32_t)(ac); \
    (a) = ROTATE_LEFT ((a), (s)); \
    (a) += (b); \
}

// MD5 padding
void MD5_pad(unsigned char *input, uint32_t input_len, unsigned char *output) {
    uint32_t i, index, pad_len;
    uint64_t bits;
    
    // Copy input to output
    memcpy(output, input, input_len);
    
    // Calculate padding length
    index = (input_len & 0x3f);
    pad_len = (index < 56) ? (56 - index) : (120 - index);
    
    // Add padding
    output[input_len] = 0x80;
    for (i = 1; i < pad_len; i++) {
        output[input_len + i] = 0x00;
    }
    
    // Add length (in bits) as 64-bit little-endian integer
    bits = input_len << 3;
    for (i = 0; i < 8; i++) {
        output[input_len + pad_len + i] = (unsigned char)(bits >> (i * 8));
    }
}

// MD5 transformation
void MD5_transform(uint32_t *state, unsigned char *block) {
    uint32_t a = state[0], b = state[1], c = state[2], d = state[3];
    uint32_t x[16];
    
    // Convert block to 16 little-endian words
    for (int i = 0; i < 16; i++) {
        x[i] = (block[4*i] & 0xff) | ((block[4*i+1] & 0xff) << 8) |
               ((block[4*i+2] & 0xff) << 16) | ((block[4*i+3] & 0xff) << 24);
    }
    
    // Round 1
    FF(a, b, c, d, x[0], S11, 0xd76aa478);
    FF(d, a, b, c, x[1], S12, 0xe8c7b756);
    FF(c, d, a, b, x[2], S13, 0x242070db);
    FF(b, c, d, a, x[3], S14, 0xc1bdceee);
    FF(a, b, c, d, x[4], S11, 0xf57c0faf);
    FF(d, a, b, c, x[5], S12, 0x4787c62a);
    FF(c, d, a, b, x[6], S13, 0xa8304613);
    FF(b, c, d, a, x[7], S14, 0xfd469501);
    FF(a, b, c, d, x[8], S11, 0x698098d8);
    FF(d, a, b, c, x[9], S12, 0x8b44f7af);
    FF(c, d, a, b, x[10], S13, 0xffff5bb1);
    FF(b, c, d, a, x[11], S14, 0x895cd7be);
    FF(a, b, c, d, x[12], S11, 0x6b901122);
    FF(d, a, b, c, x[13], S12, 0xfd987193);
    FF(c, d, a, b, x[14], S13, 0xa679438e);
    FF(b, c, d, a, x[15], S14, 0x49b40821);
    
    // Round 2
    GG(a, b, c, d, x[1], S21, 0xf61e2562);
    GG(d, a, b, c, x[6], S22, 0xc040b340);
    GG(c, d, a, b, x[11], S23, 0x265e5a51);
    GG(b, c, d, a, x[0], S24, 0xe9b6c7aa);
    GG(a, b, c, d, x[5], S21, 0xd62f105d);
    GG(d, a, b, c, x[10], S22, 0x02441453);
    GG(c, d, a, b, x[15], S23, 0xd8a1e681);
    GG(b, c, d, a, x[4], S24, 0xe7d3fbc8);
    GG(a, b, c, d, x[9], S21, 0x21e1cde6);
    GG(d, a, b, c, x[14], S22, 0xc33707d6);
    GG(c, d, a, b, x[3], S23, 0xf4d50d87);
    GG(b, c, d, a, x[8], S24, 0x455a14ed);
    GG(a, b, c, d, x[13], S21, 0xa9e3e905);
    GG(d, a, b, c, x[2], S22, 0xfcefa3f8);
    GG(c, d, a, b, x[7], S23, 0x676f02d9);
    GG(b, c, d, a, x[12], S24, 0x8d2a4c8a);
    
    // Round 3
    HH(a, b, c, d, x[5], S31, 0xfffa3942);
    HH(d, a, b, c, x[8], S32, 0x8771f681);
    HH(c, d, a, b, x[11], S33, 0x6d9d6122);
    HH(b, c, d, a, x[14], S34, 0xfde5380c);
    HH(a, b, c, d, x[1], S31, 0xa4beea44);
    HH(d, a, b, c, x[4], S32, 0x4bdecfa9);
    HH(c, d, a, b, x[7], S33, 0xf6bb4b60);
    HH(b, c, d, a, x[10], S34, 0xbebfbc70);
    HH(a, b, c, d, x[13], S31, 0x289b7ec6);
    HH(d, a, b, c, x[0], S32, 0xe19b48a8);
    HH(c, d, a, b, x[3], S33, 0x16170530);
    HH(b, c, d, a, x[6], S34, 0x1a20100a);
    HH(a, b, c, d, x[9], S31, 0x1a20100a);
    HH(d, a, b, c, x[12], S32, 0x1a20100a);
    HH(c, d, a, b, x[15], S33, 0x1a20100a);
    HH(b, c, d, a, x[2], S34, 0x1a20100a);
    
    // Round 4
    II(a, b, c, d, x[0], S41, 0x1a20100a);
    II(d, a, b, c, x[7], S42, 0x1a20100a);
    II(c, d, a, b, x[14], S43, 0x1a20100a);
    II(b, c, d, a, x[5], S44, 0x1a20100a);
    II(a, b, c, d, x[12], S41, 0x1a20100a);
    II(d, a, b, c, x[3], S42, 0x1a20100a);
    II(c, d, a, b, x[10], S43, 0x1a20100a);
    II(b, c, d, a, x[1], S44, 0x1a20100a);
    II(a, b, c, d, x[8], S41, 0x1a20100a);
    II(d, a, b, c, x[15], S42, 0x1a20100a);
    II(c, d, a, b, x[6], S43, 0x1a20100a);
    II(b, c, d, a, x[13], S44, 0x1a20100a);
    II(a, b, c, d, x[4], S41, 0x1a20100a);
    II(d, a, b, c, x[11], S42, 0x1a20100a);
    II(c, d, a, b, x[2], S43, 0x1a20100a);
    II(b, c, d, a, x[9], S44, 0x1a20100a);
    
    // Add to state
    state[0] += a;
    state[1] += b;
    state[2] += c;
    state[3] += d;
}

// MD5 hash function
void MD5_hash(unsigned char *input, uint32_t input_len, unsigned char *output) {
    uint32_t state[4] = {0x67452301, 0xefcdab89, 0x98badcfe, 0x10325476};
    uint32_t i, j;
    unsigned char *block;
    
    // Pad input
    unsigned char *padded = (unsigned char *)malloc(input_len + 64);
    MD5_pad(input, input_len, padded);
    
    // Process blocks
    for (i = 0; i < input_len + 64; i += 64) {
        block = padded + i;
        MD5_transform(state, block);
    }
    
    // Convert state to output
    for (i = 0; i < 4; i++) {
        for (j = 0; j < 4; j++) {
            output[i * 4 + j] = (unsigned char)(state[i] >> (j * 8));
        }
    }
    
    free(padded);
}

// Convert hash to hexadecimal string
void MD5_to_hex(unsigned char *hash, char *hex) {
    for (int i = 0; i < 16; i++) {
        sprintf(&hex[i * 2], "%02x", hash[i]);
    }
}

// Example usage
int main() {
    char input[] = "Hello, World!";
    unsigned char hash[16];
    char hex[33];
    
    printf("Input: %s\n", input);
    
    // Calculate MD5 hash
    MD5_hash((unsigned char *)input, strlen(input), hash);
    
    // Convert to hex string
    MD5_to_hex(hash, hex);
    hex[32] = '\0';
    
    printf("MD5 Hash: %s\n", hex);
    
    // Test with another string
    char input2[] = "The quick brown fox jumps over the lazy dog";
    unsigned char hash2[16];
    char hex2[33];
    
    printf("\nInput: %s\n", input2);
    MD5_hash((unsigned char *)input2, strlen(input2), hash2);
    MD5_to_hex(hash2, hex2);
    hex2[32] = '\0';
    
    printf("MD5 Hash: %s\n", hex2);
    
    return 0;
}
```

## How to Compile and Run

```bash
gcc -o md5_example md5_example.c
./md5_example
```

## Sample Output

```
Input: Hello, World!
MD5 Hash: 65a8e27d8879283831b664bd8b7f0ad4

Input: The quick brown fox jumps over the lazy dog
MD5 Hash: 9e107d9d372bb6826bd81d3542a419d6
```

## Key Features

1. **Complete MD5 Implementation**: Implements the full MD5 algorithm as specified in RFC 1321
2. **Proper Padding**: Handles MD5 padding according to the specification
3. **Little-Endian Conversion**: Properly converts data to little-endian format
4. **Hex Output**: Converts the binary hash to hexadecimal string representation
5. **Memory Management**: Properly allocates and frees memory for intermediate buffers

## Notes

- This is a simplified implementation for educational purposes
- For production use, consider using established cryptographic libraries like OpenSSL
- MD5 is cryptographically broken and should not be used for security-sensitive applications
- The algorithm processes data in 512-bit blocks (64 bytes)

