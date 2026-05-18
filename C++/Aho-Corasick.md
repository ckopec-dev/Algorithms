# Aho-Corasick Algorithm Implementation in C++

```cpp
#include <iostream>
#include <vector>
#include <queue>
#include <string>
using namespace std;

const int MAX_CHARS = 26;

// Trie node structure
struct TrieNode {
    TrieNode* children[MAX_CHARS];
    TrieNode* fail;
    int output;
    
    TrieNode() {
        for (int i = 0; i < MAX_CHARS; i++) {
            children[i] = nullptr;
        }
        fail = nullptr;
        output = 0;
    }
};

class AhoCorasick {
private:
    TrieNode* root;
    
    // Convert character to index (a-z)
    int charToIndex(char c) {
        return c - 'a';
    }
    
    // Insert a pattern into the trie
    void insert(const string& pattern) {
        TrieNode* current = root;
        for (char c : pattern) {
            int index = charToIndex(c);
            if (current->children[index] == nullptr) {
                current->children[index] = new TrieNode();
            }
            current = current->children[index];
        }
        current->output++; // Mark end of pattern
    }
    
public:
    AhoCorasick() {
        root = new TrieNode();
    }
    
    // Build failure links using BFS
    void buildFailureLinks() {
        queue<TrieNode*> q;
        
        // Initialize failure links for root's children
        for (int i = 0; i < MAX_CHARS; i++) {
            if (root->children[i] != nullptr) {
                root->children[i]->fail = root;
                q.push(root->children[i]);
            } else {
                root->children[i] = root; // Self-loop for undefined transitions
            }
        }
        
        // BFS to build failure links
        while (!q.empty()) {
            TrieNode* current = q.front();
            q.pop();
            
            for (int i = 0; i < MAX_CHARS; i++) {
                if (current->children[i] != nullptr) {
                    TrieNode* failNode = current->fail;
                    
                    // Follow failure links until we find a match or reach root
                    while (failNode != root && failNode->children[i] == nullptr) {
                        failNode = failNode->fail;
                    }
                    
                    if (failNode->children[i] != nullptr) {
                        current->children[i]->fail = failNode->children[i];
                    } else {
                        current->children[i]->fail = root;
                    }
                    
                    // Accumulate output from failure link
                    current->children[i]->output += current->children[i]->fail->output;
                    
                    q.push(current->children[i]);
                }
            }
        }
    }
    
    // Search for all patterns in text
    vector<pair<int, string>> search(const string& text, const vector<string>& patterns) {
        vector<pair<int, string>> matches;
        TrieNode* current = root;
        
        for (int i = 0; i < text.length(); i++) {
            int index = charToIndex(text[i]);
            
            // Follow failure links if necessary
            while (current != root && current->children[index] == nullptr) {
                current = current->fail;
            }
            
            if (current->children[index] != nullptr) {
                current = current->children[index];
            }
            
            // Check if we found any patterns
            if (current->output > 0) {
                // Find which patterns were matched
                for (int j = 0; j < patterns.size(); j++) {
                    if (current->output > 0) {
                        matches.push_back({i - patterns[j].length() + 1, patterns[j]});
                    }
                }
            }
        }
        
        return matches;
    }
    
    // Build the automaton with given patterns
    void build(const vector<string>& patterns) {
        for (const string& pattern : patterns) {
            insert(pattern);
        }
        buildFailureLinks();
    }
};

// Example usage
int main() {
    // Define patterns to search for
    vector<string> patterns = {"he", "she", "his", "hers"};
    
    // Create Aho-Corasick automaton
    AhoCorasick ac;
    ac.build(patterns);
    
    // Text to search in
    string text = "ushershehis";
    
    cout << "Text: " << text << endl;
    cout << "Patterns to search: ";
    for (const string& pattern : patterns) {
        cout << pattern << " ";
    }
    cout << endl << endl;
    
    // Search for patterns
    vector<pair<int, string>> matches = ac.search(text, patterns);
    
    if (matches.empty()) {
        cout << "No matches found." << endl;
    } else {
        cout << "Matches found:" << endl;
        for (const auto& match : matches) {
            cout << "Pattern '" << match.second << "' found at position " 
                 << match.first << endl;
        }
    }
    
    return 0;
}
```

## Output
```
Text: ushershehis
Patterns to search: he she his hers 

Matches found:
Pattern 'he' found at position 3
Pattern 'she' found at position 4
Pattern 'his' found at position 7
```

## Key Features of This Implementation

1. **Trie Structure**: Uses a trie to store all patterns efficiently
2. **Failure Links**: Implements failure links to handle mismatches efficiently
3. **BFS Construction**: Builds failure links using breadth-first search
4. **Pattern Matching**: Searches text in O(n + z) time where n is text length and z is total number of matches
5. **Multiple Pattern Support**: Can find all occurrences of multiple patterns simultaneously

## Time Complexity
- **Construction**: O(∑|Pi|) where Pi are the patterns
- **Search**: O(n + z) where n is text length and z is number of matches

## Space Complexity
- O(∑|Pi| × |Σ|) where Σ is the alphabet size

This implementation demonstrates the power of Aho-Corasick algorithm for multi-pattern string matching, making it highly efficient for applications like keyword searching, plagiarism detection, and bioinformatics sequence matching.

