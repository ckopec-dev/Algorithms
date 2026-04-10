# Floyd's Cycle Detection Algorithm in C

Floyd's Cycle Detection Algorithm (also known as the "Tortoise and Hare" algorithm) is used to detect cycles in linked lists. Here's a complete implementation:

```c
#include <stdio.h>
#include <stdlib.h>

// Define the structure for a linked list node
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

// Floyd's Cycle Detection Algorithm
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
        
        // If they meet, there's a cycle
        if (slow == fast) {
            return 1; // Cycle detected
        }
    }
    
    return 0; // No cycle found
}

// Function to detect cycle and return the starting node (optional enhancement)
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
    
    // If no cycle
    if (fast == NULL || fast->next == NULL) {
        return NULL;
    }
    
    // Phase 2: Find the start of cycle
    slow = head;
    while (slow != fast) {
        slow = slow->next;
        fast = fast->next;
    }
    
    return slow; // Start of the cycle
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
    
    printf("Original list (no cycle):\n");
    printList(head);
    
    // Check for cycle
    if (hasCycle(head)) {
        printf("Cycle detected in the list\n");
    } else {
        printf("No cycle in the list\n");
    }
    
    // Create a cycle: 1 -> 2 -> 3 -> 4 -> 5 -> 2 (cycle back to node 2)
    struct Node* cycleNode = head->next->next->next->next; // Node 5
    cycleNode->next = head->next; // Point back to node 2
    
    printf("\nList with cycle created:\n");
    printList(head);
    
    // Check for cycle
    if (hasCycle(head)) {
        printf("Cycle detected in the list\n");
        
        // Find the start of cycle
        struct Node* cycleStart = detectCycleStart(head);
        if (cycleStart != NULL) {
            printf("Cycle starts at node with data: %d\n", cycleStart->data);
        }
    } else {
        printf("No cycle in the list\n");
    }
    
    return 0;
}
```

## How the Algorithm Works:

1. **Two Pointers**: Uses two pointers moving at different speeds
   - **Slow pointer** (tortoise): moves one step at a time
   - **Fast pointer** (hare): moves two steps at a time

2. **Cycle Detection**: If there's a cycle, the fast pointer will eventually "lap" the slow pointer and they'll meet

3. **Time Complexity**: O(n) - linear time
4. **Space Complexity**: O(1) - constant space

## Sample Output:
```
Original list (no cycle):
1 -> 2 -> 3 -> 4 -> 5 -> NULL
No cycle in the list

List with cycle created:
1 -> 2 -> 3 -> 4 -> 5 -> 2 -> 3 -> 4 -> 5 -> 2 -> NULL
Cycle detected in the list
Cycle starts at node with data: 2
```

This implementation demonstrates both basic cycle detection and how to find the starting point of the cycle in a linked list.

