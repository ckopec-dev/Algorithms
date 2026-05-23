# Trie Data Structure Implementation in C++

```cpp
#include <iostream>
#include <vector>
#include <string>
using namespace std;

// Trie Node structure
struct TrieNode {
    TrieNode* children[26];  // For each letter 'a' to 'z'
    bool isEndOfWord;        // Marks end of a word
    
    // Constructor
    TrieNode() {
        isEndOfWord = false;
        for (int i = 0; i < 26; i++) {
            children[i] = nullptr;
        }
    }
};

class Trie {
private:
    TrieNode* root;
    
public:
    // Constructor
    Trie() {
        root = new TrieNode();
    }
    
    // Insert a word into the trie
    void insert(string word) {
        TrieNode* current = root;
        
        // Traverse through each character of the word
        for (int i = 0; i < word.length(); i++) {
            int index = word[i] - 'a';  // Convert character to index (0-25)
            
            // If current character doesn't exist, create new node
            if (current->children[index] == nullptr) {
                current->children[index] = new TrieNode();
            }
            
            // Move to the next node
            current = current->children[index];
        }
        
        // Mark the end of word
        current->isEndOfWord = true;
    }
    
    // Search for a word in the trie
    bool search(string word) {
        TrieNode* current = root;
        
        // Traverse through each character of the word
        for (int i = 0; i < word.length(); i++) {
            int index = word[i] - 'a';
            
            // If character doesn't exist, word not found
            if (current->children[index] == nullptr) {
                return false;
            }
            
            current = current->children[index];
        }
        
        // Return true only if it's end of a word
        return current->isEndOfWord;
    }
    
    // Check if any word starts with the given prefix
    bool startsWith(string prefix) {
        TrieNode* current = root;
        
        // Traverse through each character of the prefix
        for (int i = 0; i < prefix.length(); i++) {
            int index = prefix[i] - 'a';
            
            // If character doesn't exist, prefix not found
            if (current->children[index] == nullptr) {
                return false;
            }
            
            current = current->children[index];
        }
        
        // If we reach here, prefix exists
        return true;
    }
};

// Example usage
int main() {
    Trie trie;
    
    // Insert words
    trie.insert("apple");
    trie.insert("app");
    trie.insert("application");
    trie.insert("apply");
    
    // Search for words
    cout << "Search 'app': " << (trie.search("app") ? "Found" : "Not Found") << endl;
    cout << "Search 'apple': " << (trie.search("apple") ? "Found" : "Not Found") << endl;
    cout << "Search 'appl': " << (trie.search("appl") ? "Found" : "Not Found") << endl;
    
    // Check prefixes
    cout << "Prefix 'app': " << (trie.startsWith("app") ? "Exists" : "Doesn't exist") << endl;
    cout << "Prefix 'appl': " << (trie.startsWith("appl") ? "Exists" : "Doesn't exist") << endl;
    cout << "Prefix 'xyz': " << (trie.startsWith("xyz") ? "Exists" : "Doesn't exist") << endl;
    
    return 0;
}
```

## Output:
```
Search 'app': Found
Search 'apple': Found
Search 'appl': Not Found
Prefix 'app': Exists
Prefix 'appl': Exists
Prefix 'xyz': Doesn't exist
```

## Key Features of Trie Implementation:

1. **Insert Operation**: O(m) time complexity where m is the length of the word
2. **Search Operation**: O(m) time complexity where m is the length of the word
3. **Space Complexity**: O(ALPHABET_SIZE × N × M) where N is number of words and M is average length
4. **Applications**: 
   - Autocomplete features
   - Spell checkers
   - IP routing tables
   - T9 predictive text input

The trie data structure is particularly efficient for prefix-based operations and is widely used in text processing applications.

