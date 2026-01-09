# Floyd's Cycle Detection Algorithm in C

Floyd's cycle detection algorithm (also known as the "tortoise and hare" algorithm) is used to detect cycles in linked lists. Here's a complete implementation:

```c
#include <stdio.h>
#include <stdlib.h>

// Definition of a linked list node
struct Node {
    int data;
    struct Node* next;
};

// Function to create a new node
struct Node* createNode(int data) {
    struct Node* newNode = (struct Node*)malloc(sizeof(struct Node));
    newNode->data = data;
    newNode->next = NULL;
    return newNode;
}

// Floyd's cycle detection algorithm
int hasCycle(struct Node* head) {
    // Handle empty list or single node
    if (head == NULL || head->next == NULL) {
        return 0; // No cycle
    }
    
    // Initialize two pointers
    struct Node* slow = head;    // Tortoise - moves one step
    struct Node* fast = head;    // Hare - moves two steps
    
    // Traverse the list
    while (fast != NULL && fast->next != NULL) {
        slow = slow->next;          // Move slow pointer one step
        fast = fast->next->next;    // Move fast pointer two steps
        
        // If pointers meet, there's a cycle
        if (slow == fast) {
            return 1; // Cycle detected
        }
    }
    
    return 0; // No cycle found
}

// Function to detect cycle and return the starting node (if cycle exists)
struct Node* detectCycleStart(struct Node* head) {
    if (head == NULL || head->next == NULL) {
        return NULL;
    }
    
    struct Node* slow = head;
    struct Node* fast = head;
    
    // Phase 1: Detect if cycle exists
    while (fast != NULL && fast->next != NULL) {
        slow = slow->next;
        fast = fast->next->next;
        
        if (slow == fast) {
            break; // Cycle detected
        }
    }
    
    // If no cycle found
    if (fast == NULL || fast->next == NULL) {
        return NULL;
    }
    
    // Phase 2: Find the start of the cycle
    slow = head;
    while (slow != fast) {
        slow = slow->next;
        fast = fast->next;
    }
    
    return slow; // Return the starting node of the cycle
}

// Function to print the linked list (for testing)
void printList(struct Node* head) {
    struct Node* current = head;
    while (current != NULL) {
        printf("%d -> ", current->data);
        current = current->next;
    }
    printf("NULL\n");
}

// Example usage
int main() {
    // Create a linked list without cycle: 1 -> 2 -> 3 -> 4 -> 5 -> NULL
    struct Node* head = createNode(1);
    head->next = createNode(2);
    head->next->next = createNode(3);
    head->next->next->next = createNode(4);
    head->next->next->next->next = createNode(5);
    
    printf("List without cycle: ");
    printList(head);
    
    // Check for cycle
    if (hasCycle(head)) {
        printf("Cycle detected!\n");
    } else {
        printf("No cycle detected.\n");
    }
    
    // Create a linked list with cycle: 1 -> 2 -> 3 -> 4 -> 2 (cycle)
    struct Node* cycleHead = createNode(1);
    cycleHead->next = createNode(2);
    cycleHead->next->next = createNode(3);
    cycleHead->next->next->next = createNode(4);
    
    // Create cycle: 4 -> 2 (pointing back to node with value 2)
    cycleHead->next->next->next->next = cycleHead->next;
    
    printf("\nList with cycle: ");
    printList(cycleHead);
    
    // Check for cycle
    if (hasCycle(cycleHead)) {
        printf("Cycle detected!\n");
        
        // Find the start of cycle
        struct Node* cycleStart = detectCycleStart(cycleHead);
        if (cycleStart != NULL) {
            printf("Cycle starts at node with value: %d\n", cycleStart->data);
        }
    } else {
        printf("No cycle detected.\n");
    }
    
    return 0;
}
```

## How it works:

1. **Two Pointers Approach**: Uses two pointers moving at different speeds
2. **Slow Pointer**: Moves one step at a time (tortoise)
3. **Fast Pointer**: Moves two steps at a time (hare)
4. **Cycle Detection**: If there's a cycle, the fast pointer will eventually "lap" the slow pointer
5. **Cycle Start Detection**: Once a cycle is detected, reset one pointer to head and move both one step at a time until they meet

## Time and Space Complexity:
- **Time Complexity**: O(n)
- **Space Complexity**: O(1)

## Output:
```
List without cycle: 1 -> 2 -> 3 -> 4 -> 5 -> NULL
No cycle detected.

List with cycle: 1 -> 2 -> 3 -> 4 -> 2 -> 3 -> 4 -> 2 -> ... (infinite loop)
Cycle detected!
Cycle starts at node with value: 2
```

