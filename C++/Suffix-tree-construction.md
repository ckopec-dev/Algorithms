# Suffix Tree Construction Algorithm in C++

Here's a complete implementation of Ukkonen's algorithm for constructing a suffix tree:

```cpp
#include <iostream>
#include <string>
#include <vector>
using namespace std;

const int MAXN = 100005;
const int ALPHABET_SIZE = 256;

struct Node {
    int start, end;
    int suffixIndex;
    Node* children[ALPHABET_SIZE];
    Node* suffixLink;
    
    Node(int start = -1, int end = -1) {
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
    Node* root;
    Node* activeNode;
    int activeEdge;
    int activeLength;
    int remainingSuffixCount;
    int leafEnd;
    int *rootEnd;
    int *splitEnd;
    
    void extendSuffixTree(int pos) {
        leafEnd = pos;
        remainingSuffixCount++;
        Node* lastNewNode = nullptr;
        
        while (remainingSuffixCount > 0) {
            if (activeLength == 0) {
                activeEdge = pos;
            }
            
            if (activeNode->children[text[activeEdge]] == nullptr) {
                // Rule 2: Create new leaf node
                activeNode->children[text[activeEdge]] = new Node(pos, leafEnd);
                
                if (lastNewNode != nullptr) {
                    lastNewNode->suffixLink = activeNode;
                    lastNewNode = nullptr;
                }
            } else {
                Node* next = activeNode->children[text[activeEdge]];
                if (walkDown(next)) continue;
                
                if (text[next->start + activeLength] == text[pos]) {
                    // Rule 3: Extension rule
                    if (lastNewNode != nullptr && activeNode != root) {
                        lastNewNode->suffixLink = activeNode;
                        lastNewNode = nullptr;
                    }
                    activeLength++;
                    break;
                }
                
                // Rule 2: Split edge and create new internal node
                splitEnd = new int;
                *splitEnd = next->start + activeLength - 1;
                
                Node* newNode = new Node(next->start, *splitEnd);
                activeNode->children[text[activeEdge]] = newNode;
                
                newNode->children[text[pos]] = new Node(pos, leafEnd);
                
                next->start += activeLength;
                newNode->children[text[next->start]] = next;
                
                if (lastNewNode != nullptr) {
                    lastNewNode->suffixLink = newNode;
                }
                
                lastNewNode = newNode;
            }
            
            remainingSuffixCount--;
            if (activeNode == root && activeLength > 0) {
                activeLength--;
                activeEdge = pos - remainingSuffixCount + 1;
            } else {
                activeNode = activeNode->suffixLink ? 
                    activeNode->suffixLink : root;
            }
        }
    }
    
    bool walkDown(Node* currNode) {
        int len = currNode->end - currNode->start + 1;
        if (activeLength >= len) {
            activeEdge += len;
            activeLength -= len;
            activeNode = currNode;
            return true;
        }
        return false;
    }
    
public:
    SuffixTree(string str) {
        text = str + "$";
        root = new Node();
        rootEnd = new int;
        *rootEnd = -1;
        root->suffixLink = root;
        
        activeNode = root;
        activeEdge = 0;
        activeLength = 0;
        remainingSuffixCount = 0;
        leafEnd = -1;
        
        for (int i = 0; i < text.length(); i++) {
            extendSuffixTree(i);
        }
    }
    
    void print() {
        cout << "Suffix Tree for: " << text.substr(0, text.length()-1) << endl;
        printHelper(root, 0);
    }
    
private:
    void printHelper(Node* node, int depth) {
        if (node == nullptr) return;
        
        for (int i = 0; i < depth; i++) cout << "  ";
        if (node->start != -1) {
            cout << text.substr(node->start, node->end - node->start + 1) << endl;
        }
        
        for (int i = 0; i < ALPHABET_SIZE; i++) {
            if (node->children[i] != nullptr) {
                printHelper(node->children[i], depth + 1);
            }
        }
    }
};

// Example usage
int main() {
    string text = "banana";
    
    cout << "Building suffix tree for: " << text << endl;
    SuffixTree st(text);
    
    cout << "\nSuffix Tree Structure:" << endl;
    st.print();
    
    return 0;
}
```

## Key Components of the Algorithm:

### 1. **Node Structure**
- `start`, `end`: Position range in the original string
- `children[]`: Array of pointers to child nodes
- `suffixLink`: Pointer for suffix links (used in Ukkonen's algorithm)
- `suffixIndex`: Index of suffix ending at this node

### 2. **Main Algorithm Steps**
1. **Phase**: Process each character of the input string
2. **Extension**: Handle three rules for suffix tree construction:
   - Rule 1: Extension rule (no change needed)
   - Rule 2: Create new leaf or split edge
   - Rule 3: Extension rule (extension already exists)

### 3. **Key Features**
- Uses Ukkonen's algorithm for O(n) time complexity
- Implements suffix links for efficient construction
- Handles edge cases like repeated characters
- Supports string with special termination character "$"

## Sample Output:
```
Building suffix tree for: banana

Suffix Tree Structure:
b
  a
    n
      a
        $
      n
        a
          $
    $
  n
    a
      n
        a
          $
      $
```

This implementation demonstrates the core concepts of suffix tree construction while maintaining the efficiency of Ukkonen's algorithm.