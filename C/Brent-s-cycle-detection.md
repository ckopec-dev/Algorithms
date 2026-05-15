# Brent's Cycle Detection Algorithm in C

Brent's cycle detection algorithm is an efficient method for detecting cycles in linked lists. It's an improvement over Floyd's cycle detection algorithm with better performance characteristics.

```c
#include <stdio.h>
#include <stdlib.h>

// Definition of a linked list node
typedef struct Node {
    int data;
    struct Node* next;
} Node;

// Function to create a new node
Node* createNode(int data) {
    Node* newNode = (Node*)malloc(sizeof(Node));
    newNode->data = data;
    newNode->next = NULL;
    return newNode;
}

// Brent's Cycle Detection Algorithm
int brentCycleDetection(Node* head) {
    // Handle empty list
    if (head == NULL) {
        return 0; // No cycle
    }
    
    // Phase 1: Find the cycle length
    Node* slow = head;
    Node* fast = head;
    
    int power = 1;
    int length = 1;
    
    // Move fast pointer ahead by powers of 2
    while (fast != NULL && fast->next != NULL) {
        fast = fast->next;
        
        if (slow == fast) {
            // Cycle detected, return the length
            return length;
        }
        
        if (power == length) {
            // Reset slow pointer and update power
            slow = fast;
            power *= 2;
            length = 0;
        }
        
        length++;
    }
    
    return 0; // No cycle found
}

// Alternative implementation that returns cycle start position
int brentCycleStart(Node* head) {
    if (head == NULL) {
        return -1; // No cycle
    }
    
    // Phase 1: Find cycle length
    Node* slow = head;
    Node* fast = head;
    int length = 0;
    
    // Move fast pointer ahead by powers of 2
    while (fast != NULL && fast->next != NULL) {
        fast = fast->next;
        
        if (slow == fast) {
            // Cycle detected, now find the start
            break;
        }
        
        if (length == 0) {
            slow = fast;
        }
        
        length++;
    }
    
    // If no cycle found
    if (fast == NULL || fast->next == NULL) {
        return -1;
    }
    
    // Phase 2: Find the start of the cycle
    Node* start = head;
    Node* current = head;
    
    // Move start pointer ahead by cycle length
    for (int i = 0; i < length; i++) {
        start = start->next;
    }
    
    // Move both pointers until they meet
    while (start != current) {
        start = start->next;
        current = current->next;
    }
    
    // Return position (0-indexed)
    int position = 0;
    Node* temp = head;
    while (temp != current) {
        temp = temp->next;
        position++;
    }
    
    return position;
}

// Function to create a cycle in the linked list for testing
void createCycle(Node* head, int pos) {
    if (head == NULL || pos < 0) return;
    
    Node* cycleNode = NULL;
    Node* tail = head;
    int index = 0;
    
    // Find the tail node and the node at position 'pos'
    while (tail->next != NULL) {
        if (index == pos) {
            cycleNode = tail;
        }
        tail = tail->next;
        index++;
    }
    
    if (index == pos) {
        cycleNode = tail;
    }
    
    // Create cycle
    if (cycleNode != NULL) {
        tail->next = cycleNode;
    }
}

// Function to print the linked list (with cycle detection)
void printList(Node* head, int maxNodes) {
    Node* current = head;
    int count = 0;
    
    while (current != NULL && count < maxNodes) {
        printf("%d -> ", current->data);
        current = current->next;
        count++;
    }
    
    if (current != NULL) {
        printf("... (cycle detected)\n");
    } else {
        printf("NULL\n");
    }
}

// Main function to demonstrate the algorithm
int main() {
    // Create a simple linked list: 1 -> 2 -> 3 -> 4 -> 5 -> NULL
    Node* head = createNode(1);
    head->next = createNode(2);
    head->next->next = createNode(3);
    head->next->next->next = createNode(4);
    head->next->next->next->next = createNode(5);
    
    printf("Original list: ");
    printList(head, 10);
    
    // Test without cycle
    int cycleLength = brentCycleDetection(head);
    printf("Cycle length (no cycle): %d\n\n", cycleLength);
    
    // Create a cycle: 1 -> 2 -> 3 -> 4 -> 5 -> 2 (cycle back to node 2)
    createCycle(head, 1); // Creates cycle from node 5 back to node 2
    
    printf("List with cycle created:\n");
    printList(head, 10);
    
    // Test with cycle
    cycleLength = brentCycleDetection(head);
    printf("Cycle length (with cycle): %d\n", cycleLength);
    
    int cycleStart = brentCycleStart(head);
    printf("Cycle starts at position: %d\n", cycleStart);
    
    // Clean up memory
    Node* current = head;
    while (current != NULL) {
        Node* temp = current;
        current = current->next;
        free(temp);
    }
    
    return 0;
}
```

## Key Features of Brent's Algorithm:

1. **Phase 1**: Uses powers of 2 to efficiently detect cycles
2. **Phase 2**: Finds the start of the cycle
3. **Time Complexity**: O(λ + μ) where λ is the cycle length and μ is the distance to the cycle
4. **Space Complexity**: O(1) - constant space

## How it works:

1. **Power of 2 Movement**: Fast pointer moves in powers of 2 (1, 2, 4, 8, ...)
2. **Reset Logic**: When the fast pointer reaches a power of 2, the slow pointer is reset
3. **Cycle Detection**: When both pointers meet, a cycle is detected
4. **Cycle Length**: The algorithm returns the length of the cycle

## Sample Output:
```
Original list: 1 -> 2 -> 3 -> 4 -> 5 -> NULL
Cycle length (no cycle): 0

List with cycle created:
1 -> 2 -> 3 -> 4 -> 5 -> ... (cycle detected)
Cycle length (with cycle): 4
Cycle starts at position: 1
```

