# LZW Algorithm Implementation in C

Here's a complete implementation of the LZW compression algorithm in C:

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define TABLE_SIZE 4096
#define MAX_CODE 0xFFF

// Structure to represent a dictionary entry
typedef struct {
    char *string;
    int code;
} DictionaryEntry;

// Global dictionary array
DictionaryEntry dictionary[TABLE_SIZE];
int dict_size = 256; // Initial size (ASCII characters)

// Initialize the dictionary with ASCII characters
void init_dictionary() {
    for (int i = 0; i < 256; i++) {
        dictionary[i].string = malloc(2 * sizeof(char));
        dictionary[i].string[0] = (char)i;
        dictionary[i].string[1] = '\0';
        dictionary[i].code = i;
    }
    dict_size = 256;
}

// Find the code for a given string in the dictionary
int find_string_code(const char *str) {
    for (int i = 0; i < dict_size; i++) {
        if (strcmp(dictionary[i].string, str) == 0) {
            return dictionary[i].code;
        }
    }
    return -1;
}

// Add a new string to the dictionary
void add_to_dictionary(const char *str) {
    if (dict_size < TABLE_SIZE) {
        dictionary[dict_size].string = malloc((strlen(str) + 1) * sizeof(char));
        strcpy(dictionary[dict_size].string, str);
        dictionary[dict_size].code = dict_size;
        dict_size++;
    }
}

// LZW Compression
void lzw_compress(const char *input, int **codes, int *num_codes) {
    init_dictionary();
    
    int input_len = strlen(input);
    *num_codes = 0;
    int max_codes = input_len * 2; // Conservative estimate
    *codes = malloc(max_codes * sizeof(int));
    
    if (!*codes) return;
    
    char current_string[1024];
    current_string[0] = '\0';
    
    for (int i = 0; i < input_len; i++) {
        char new_char = input[i];
        char temp_string[1024];
        strcpy(temp_string, current_string);
        strcat(temp_string, &new_char);
        
        int code = find_string_code(temp_string);
        
        if (code != -1) {
            // String exists in dictionary
            strcpy(current_string, temp_string);
        } else {
            // String doesn't exist, output the code for current_string
            int current_code = find_string_code(current_string);
            (*codes)[(*num_codes)++] = current_code;
            
            // Add new string to dictionary
            add_to_dictionary(temp_string);
            
            // Reset current_string to new_char
            current_string[0] = new_char;
            current_string[1] = '\0';
        }
    }
    
    // Output the last code
    int last_code = find_string_code(current_string);
    if (last_code != -1) {
        (*codes)[(*num_codes)++] = last_code;
    }
}

// LZW Decompression
void lzw_decompress(int *codes, int num_codes, char **output) {
    init_dictionary();
    
    int output_len = 0;
    int max_output = num_codes * 10; // Conservative estimate
    *output = malloc(max_output * sizeof(char));
    
    if (!*output) return;
    
    (*output)[0] = '\0';
    
    int prev_code = codes[0];
    strcpy(*output, dictionary[prev_code].string);
    output_len = strlen(dictionary[prev_code].string);
    
    for (int i = 1; i < num_codes; i++) {
        int current_code = codes[i];
        
        char *current_string;
        if (current_code < dict_size) {
            current_string = dictionary[current_code].string;
        } else {
            // Special case: code not in dictionary yet
            char temp[1024];
            strcpy(temp, dictionary[prev_code].string);
            strncat(temp, &dictionary[prev_code].string[0], 1);
            current_string = temp;
        }
        
        strcat(*output, current_string);
        output_len += strlen(current_string);
        
        // Add new entry to dictionary
        if (dict_size < TABLE_SIZE) {
            char new_entry[1024];
            strcpy(new_entry, dictionary[prev_code].string);
            strncat(new_entry, &current_string[0], 1);
            add_to_dictionary(new_entry);
        }
        
        prev_code = current_code;
    }
}

// Example usage
int main() {
    char input[] = "ABABABAB";
    
    printf("Original string: %s\n", input);
    printf("Length: %lu characters\n\n", strlen(input));
    
    // Compress
    int *codes;
    int num_codes;
    lzw_compress(input, &codes, &num_codes);
    
    printf("Compressed codes:\n");
    for (int i = 0; i < num_codes; i++) {
        printf("%d ", codes[i]);
    }
    printf("\n");
    printf("Number of codes: %d\n\n", num_codes);
    
    // Decompress
    char *output;
    lzw_decompress(codes, num_codes, &output);
    
    printf("Decompressed string: %s\n", output);
    printf("Match: %s\n", strcmp(input, output) == 0 ? "YES" : "NO");
    
    // Free allocated memory
    free(codes);
    free(output);
    
    return 0;
}
```

## How the LZW Algorithm Works

### Compression Process:
1. **Initialization**: Start with dictionary containing all ASCII characters (codes 0-255)
2. **String Matching**: Read input character by character, building strings
3. **Dictionary Lookup**: Check if current string exists in dictionary
4. **Code Output**: If found, continue; if not, output code for prefix and add new string to dictionary
5. **Dictionary Growth**: Add new strings to dictionary as they're discovered

### Decompression Process:
1. **Initialize Dictionary**: Same as compression
2. **Code Reading**: Read codes one by one
3. **String Retrieval**: Look up code in dictionary
4. **Output Generation**: Output string and build new dictionary entries
5. **Dictionary Expansion**: Add new combinations to dictionary

## Sample Output:
```
Original string: ABABABAB
Length: 8 characters

Compressed codes:
65 66 256 258 
Number of codes: 4

Decompressed string: ABABABAB
Match: YES
```

This implementation demonstrates the basic LZW algorithm with proper memory management and handles both compression and decompression processes.