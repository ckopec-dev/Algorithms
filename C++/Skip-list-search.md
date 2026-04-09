# Skip List Search Algorithm in C++

```cpp
#include <iostream>
#include <random>
#include <vector>

class SkipList {
private:
    struct Node {
        int key;
        std::vector<Node*> forward;
        
        Node(int key, int level) : key(key), forward(level + 1, nullptr) {}
    };
    
    Node* header;
    int maxLevel;
    int currentLevel;
    std::random_device rd;
    std::mt19937 gen;
    
    int randomLevel() {
        int level = 0;
        while (gen() & 1) {
            level++;
        }
        return std::min(level, maxLevel);
    }
    
public:
    SkipList(int maxLevel = 16) : maxLevel(maxLevel), currentLevel(0) {
        gen.seed(rd());
        header = new Node(-1, maxLevel);
    }
    
    Node* search(int key) {
        Node* current = header;
        
        // Start from the highest level and move down
        for (int i = currentLevel; i >= 0; i--) {
            // Move forward while the next node's key is less than target key
            while (current->forward[i] != nullptr && 
                   current->forward[i]->key < key) {
                current = current->forward[i];
            }
        }
        
        // Move to the next node (which should be the target or null)
        current = current->forward[0];
        
        // Return the node if found, otherwise return nullptr
        if (current != nullptr && current->key == key) {
            return current;
        }
        
        return nullptr;
    }
    
    void insert(int key) {
        std::vector<Node*> update(maxLevel + 1, header);
        Node* current = header;
        
        // Find the position to insert
        for (int i = currentLevel; i >= 0; i--) {
            while (current->forward[i] != nullptr && 
                   current->forward[i]->key < key) {
                current = current->forward[i];
            }
            update[i] = current;
        }
        
        current = current->forward[0];
        
        // If key already exists, don't insert
        if (current != nullptr && current->key == key) {
            return;
        }
        
        // Create new node
        int newLevel = randomLevel();
        
        if (newLevel > currentLevel) {
            for (int i = currentLevel + 1; i <= newLevel; i++) {
                update[i] = header;
            }
            currentLevel = newLevel;
        }
        
        Node* newNode = new Node(key, newLevel);
        
        // Insert the node
        for (int i = 0; i <= newLevel; i++) {
            newNode->forward[i] = update[i]->forward[i];
            update[i]->forward[i] = newNode;
        }
    }
    
    void printList() {
        std::cout << "Skip List:" << std::endl;
        for (int i = currentLevel; i >= 0; i--) {
            Node* current = header->forward[i];
            std::cout << "Level " << i << ": ";
            while (current != nullptr) {
                std::cout << current->key << " ";
                current = current->forward[i];
            }
            std::cout << std::endl;
        }
        std::cout << std::endl;
    }
};

// Example usage
int main() {
    SkipList skipList(3);
    
    // Insert some elements
    std::vector<int> elements = {3, 6, 7, 9, 12, 19, 17, 26, 21, 25};
    
    std::cout << "Inserting elements: ";
    for (int elem : elements) {
        skipList.insert(elem);
        std::cout << elem << " ";
    }
    std::cout << std::endl << std::endl;
    
    // Print the skip list structure
    skipList.printList();
    
    // Search for elements
    std::cout << "Searching for elements:" << std::endl;
    
    std::vector<int> searchElements = {7, 19, 15, 25};
    
    for (int key : searchElements) {
        SkipList::Node* result = skipList.search(key);
        if (result != nullptr) {
            std::cout << "Found " << key << std::endl;
        } else {
            std::cout << "Element " << key << " not found" << std::endl;
        }
    }
    
    return 0;
}
```

## Algorithm Explanation

The **Skip List Search Algorithm** works as follows:

1. **Start from the highest level** of the skip list
2. **Move forward** while the next node's key is less than the target key
3. **Drop down** to the next lower level when the next node's key is greater than or equal to the target
4. **Continue** until reaching level 0
5. **Check** if the found node contains the target key

## Time Complexity
- **Average case**: O(log n)
- **Worst case**: O(n)
- **Space complexity**: O(n log n)

## Key Features
- **Probabilistic data structure** that provides logarithmic search time
- **Multiple levels** of linked lists for efficient traversal
- **Randomized** level assignment for balancing
- **No need for rebalancing** like AVL trees or Red-Black trees

The search algorithm efficiently navigates through the skip list by using the "skip" property, where higher levels provide shortcuts to reduce the number of comparisons needed.

