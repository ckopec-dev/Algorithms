# Skip List Search Algorithm in C

```c
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#define MAX_LEVEL 16
#define P 0.5

// Node structure for skip list
typedef struct Node {
    int data;
    struct Node* forward[MAX_LEVEL];
} Node;

// Skip list structure
typedef struct SkipList {
    Node* header;
    int level;
} SkipList;

// Create a new node
Node* createNode(int data, int level) {
    Node* newNode = (Node*)malloc(sizeof(Node));
    newNode->data = data;
    for (int i = 0; i < level; i++) {
        newNode->forward[i] = NULL;
    }
    return newNode;
}

// Create a new skip list
SkipList* createSkipList() {
    SkipList* list = (SkipList*)malloc(sizeof(SkipList));
    list->header = createNode(0, MAX_LEVEL);
    list->level = 0;
    return list;
}

// Random level generation
int randomLevel() {
    int level = 1;
    while (rand() % 2 == 1 && level < MAX_LEVEL) {
        level++;
    }
    return level;
}

// Search algorithm for skip list
Node* skipListSearch(SkipList* list, int key) {
    Node* current = list->header;
    
    // Start from the highest level and move down
    for (int i = list->level - 1; i >= 0; i--) {
        // Move forward while the next node's data is less than key
        while (current->forward[i] != NULL && 
               current->forward[i]->data < key) {
            current = current->forward[i];
        }
    }
    
    // Move to the next node at level 0
    current = current->forward[0];
    
    // Check if we found the key
    if (current != NULL && current->data == key) {
        return current;
    }
    
    return NULL; // Key not found
}

// Insert function (for demonstration)
void skipListInsert(SkipList* list, int data) {
    Node* update[MAX_LEVEL];
    Node* current = list->header;
    
    // Find the position to insert
    for (int i = list->level - 1; i >= 0; i--) {
        while (current->forward[i] != NULL && 
               current->forward[i]->data < data) {
            current = current->forward[i];
        }
        update[i] = current;
    }
    
    current = current->forward[0];
    
    // If key already exists, don't insert
    if (current != NULL && current->data == data) {
        return;
    }
    
    // Generate random level for new node
    int newLevel = randomLevel();
    
    // If new level is greater than current list level, update list level
    if (newLevel > list->level) {
        for (int i = list->level; i < newLevel; i++) {
            update[i] = list->header;
        }
        list->level = newLevel;
    }
    
    // Create new node
    Node* newNode = createNode(data, newLevel);
    
    // Insert node in all levels
    for (int i = 0; i < newLevel; i++) {
        newNode->forward[i] = update[i]->forward[i];
        update[i]->forward[i] = newNode;
    }
}

// Print the skip list
void printSkipList(SkipList* list) {
    printf("Skip List:\n");
    for (int i = list->level - 1; i >= 0; i--) {
        Node* current = list->header->forward[i];
        printf("Level %d: ", i);
        while (current != NULL) {
            printf("%d -> ", current->data);
            current = current->forward[i];
        }
        printf("NULL\n");
    }
    printf("\n");
}

// Main function to demonstrate the search algorithm
int main() {
    srand(time(NULL));
    
    // Create skip list
    SkipList* list = createSkipList();
    
    // Insert some elements
    int elements[] = {3, 6, 7, 9, 12, 17, 21, 25};
    int n = sizeof(elements) / sizeof(elements[0]);
    
    printf("Inserting elements: ");
    for (int i = 0; i < n; i++) {
        printf("%d ", elements[i]);
        skipListInsert(list, elements[i]);
    }
    printf("\n\n");
    
    // Print the skip list
    printSkipList(list);
    
    // Test search algorithm
    int searchKeys[] = {7, 12, 15, 25};
    int searchCount = sizeof(searchKeys) / sizeof(searchKeys[0]);
    
    printf("Search Results:\n");
    for (int i = 0; i < searchCount; i++) {
        Node* result = skipListSearch(list, searchKeys[i]);
        if (result != NULL) {
            printf("Found %d in the skip list\n", searchKeys[i]);
        } else {
            printf("%d not found in the skip list\n", searchKeys[i]);
        }
    }
    
    return 0;
}
```

## Algorithm Explanation

The `skipListSearch` function works as follows:

1. **Start at highest level**: Begin from the topmost level of the skip list
2. **Move forward**: While the next node's data is less than the search key, move forward at the current level
3. **Drop down**: When we can't move forward (next node >= key), drop down to the next lower level
4. **Final check**: After reaching level 0, check if the current node contains the key

## Time Complexity
- **Average case**: O(log n)
- **Worst case**: O(n)
- **Space complexity**: O(n log n)

## Key Features
- Uses multiple levels to skip elements
- Efficient search compared to linked lists
- Average logarithmic time complexity
- Easy to implement and understand

The algorithm demonstrates how skip lists provide fast search capabilities by maintaining multiple layers of linked lists with different spacing.