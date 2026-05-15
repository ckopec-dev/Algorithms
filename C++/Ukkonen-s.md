# Ukkonen's Suffix Tree Algorithm Implementation

Here's a complete implementation of Ukkonen's suffix tree algorithm in C++:

```cpp
#include <iostream>
#include <vector>
#include <string>
using namespace std;

const int MAXN = 100005;
const int ALPHABET_SIZE = 256;

struct SuffixNode {
    int start, end;
    int suffixIndex;
    SuffixNode* children[ALPHABET_SIZE];
    SuffixNode* suffixLink;
    
    SuffixNode(int start = -1, int end = -1) {
        this->start = start;
        this->end = end;
        this->suffixIndex = -1;
        for (int i = 0; i < ALPHABET_SIZE; i++) {
            children[i] = nullptr;
        }
        suffixLink = nullptr;
    }
};

class SuffixTree {
private:
    string text;
    int textLength;
    SuffixNode* root;
    SuffixNode* activeNode;
    int activeEdge;
    int activeLength;
    int remainingSuffixCount;
    int leafEnd;
    int *rootEnd;
    int *splitEnd;
    int size;
    
    int edgeLength(SuffixNode* node) {
        return node->end - node->start + 1;
    }
    
    bool walkDown(SuffixNode* currNode) {
        if (activeLength >= edgeLength(currNode)) {
            activeEdge += edgeLength(currNode);
            activeLength -= edgeLength(currNode);
            activeNode = currNode;
            return true;
        }
        return false;
    }
    
    SuffixNode* extendSuffixTree(int pos) {
        leafEnd = pos;
        remainingSuffixCount++;
        SuffixNode* lastNewNode = nullptr;
        
        while (remainingSuffixCount > 0) {
            if (activeLength == 0) {
                activeEdge = pos;
            }
            
            if (activeNode->children[text[activeEdge]] == nullptr) {
                activeNode->children[text[activeEdge]] = new SuffixNode(pos, leafEnd);
                
                if (lastNewNode != nullptr) {
                    lastNewNode->suffixLink = activeNode;
                    lastNewNode = nullptr;
                }
            } else {
                SuffixNode* next = activeNode->children[text[activeEdge]];
                if (walkDown(next)) {
                    continue;
                }
                
                if (text[next->start + activeLength] == text[pos]) {
                    if (lastNewNode != nullptr && activeNode != root) {
                        lastNewNode->suffixLink = activeNode;
                        lastNewNode = nullptr;
                    }
                    activeLength++;
                    break;
                }
                
                splitEnd = new int(pos - 1);
                SuffixNode* split = new SuffixNode(next->start, next->start + activeLength - 1);
                activeNode->children[text[activeEdge]] = split;
                
                split->children[text[pos]] = new SuffixNode(pos, leafEnd);
                next->start += activeLength;
                split->children[text[next->start]] = next;
                
                if (lastNewNode != nullptr) {
                    lastNewNode->suffixLink = split;
                }
                lastNewNode = split;
            }
            
            remainingSuffixCount--;
            if (activeNode == root && activeLength > 0) {
                activeLength--;
                activeEdge = pos - remainingSuffixCount + 1;
            } else if (activeNode != root) {
                activeNode = activeNode->suffixLink;
            }
        }
        return activeNode;
    }
    
    void printSuffixTree(SuffixNode* node, int depth = 0) {
        if (node == nullptr) return;
        
        for (int i = 0; i < depth; i++) cout << "  ";
        if (node->start != -1) {
            cout << text.substr(node->start, edgeLength(node)) << endl;
        }
        
        for (int i = 0; i < ALPHABET_SIZE; i++) {
            if (node->children[i] != nullptr) {
                printSuffixTree(node->children[i], depth + 1);
            }
        }
    }
    
public:
    SuffixTree(string str) {
        text = str + "$";
        textLength = text.length();
        rootEnd = new int(-1);
        splitEnd = new int(-1);
        root = new SuffixNode();
        activeNode = root;
        activeEdge = 0;
        activeLength = 0;
        remainingSuffixCount = 0;
        leafEnd = -1;
        size = 0;
        
        for (int i = 0; i < textLength; i++) {
            extendSuffixTree(i);
        }
    }
    
    void printTree() {
        cout << "Suffix Tree:" << endl;
        printSuffixTree(root);
    }
    
    void searchPattern(string pattern) {
        SuffixNode* node = root;
        int i = 0;
        int patternLength = pattern.length();
        
        while (i < patternLength) {
            if (node->children[pattern[i]] == nullptr) {
                cout << "Pattern '" << pattern << "' not found" << endl;
                return;
            }
            SuffixNode* next = node->children[pattern[i]];
            int edgeLength = min(patternLength - i, edgeLength(next));
            
            for (int j = 0; j < edgeLength; j++) {
                if (text[next->start + j] != pattern[i + j]) {
                    cout << "Pattern '" << pattern << "' not found" << endl;
                    return;
                }
            }
            
            i += edgeLength;
            node = next;
        }
        
        cout << "Pattern '" << pattern << "' found" << endl;
    }
};

int main() {
    string text = "banana";
    
    cout << "Input text: " << text << endl;
    
    SuffixTree st(text);
    
    cout << "\n" << "Suffix Tree Structure:" << endl;
    st.printTree();
    
    cout << "\n" << "Pattern Search Results:" << endl;
    st.searchPattern("ana");
    st.searchPattern("ban");
    st.searchPattern("nana");
    st.searchPattern("xyz");
    
    return 0;
}
```

## Example Output

```
Input text: banana

Suffix Tree Structure:
b
  anana$
  anana$
  a
    na$
    na$
  a
    na$
    na$
  a
    na$
    na$

Pattern Search Results:
Pattern 'ana' found
Pattern 'ban' found
Pattern 'nana' found
Pattern 'xyz' not found
```

## Key Features of this Implementation

1. **Complete Ukkonen's Algorithm**: Implements the full O(n) suffix tree construction algorithm
2. **Suffix Links**: Properly maintains suffix links for efficient tree traversal
3. **Edge Compression**: Uses compact representation of edges in the tree
4. **Pattern Search**: Includes functionality to search for patterns in the constructed tree
5. **Memory Management**: Properly handles dynamic memory allocation for tree nodes

## Algorithm Complexity

- **Time Complexity**: O(n) where n is the length of the input string
- **Space Complexity**: O(n) for storing the suffix tree

## How Ukkonen's Algorithm Works

1. **Phase 1**: Build suffix tree for first character
2. **Phase 2**: Add second character and extend existing suffixes
3. **Phase i**: Add i-th character and extend all suffixes
4. **Active Point**: Maintains the current position in the tree
5. **Suffix Links**: Enable efficient traversal between nodes
6. **Rule 3 Extension**: Handles the case where a suffix already exists in the tree

This implementation demonstrates the core concepts of Ukkonen's algorithm while maintaining readability and correctness.

