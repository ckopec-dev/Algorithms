# Brent's Cycle Detection Algorithm in C++

Brent's cycle detection algorithm is an efficient method for detecting cycles in linked lists. It's an improvement over Floyd's cycle detection algorithm, using a different approach to find the cycle length and starting point.

```cpp
#include <iostream>
#include <cmath>
using namespace std;

// Definition of a linked list node
struct ListNode {
    int val;
    ListNode* next;
    ListNode(int x) : val(x), next(nullptr) {}
};

class CycleDetection {
public:
    // Brent's cycle detection algorithm
    static bool hasCycle(ListNode* head) {
        if (!head || !head->next) {
            return false;
        }
        
        // Phase 1: Find if a cycle exists
        ListNode* slow = head;
        ListNode* fast = head;
        
        // Move fast pointer 2 steps and slow pointer 1 step
        // until they meet or fast reaches end
        while (fast && fast->next) {
            slow = slow->next;
            fast = fast->next->next;
            
            if (slow == fast) {
                return true; // Cycle detected
            }
        }
        
        return false; // No cycle
    }
    
    // Brent's algorithm to find cycle start and length
    static pair<int, int> findCycleInfo(ListNode* head) {
        if (!head || !head->next) {
            return make_pair(-1, -1); // No cycle
        }
        
        // Phase 1: Find if cycle exists and estimate cycle length
        ListNode* slow = head;
        ListNode* fast = head;
        int power = 1;
        int length = 0;
        
        // Move fast pointer exponentially (1, 2, 4, 8, ...)
        while (fast && fast->next) {
            slow = slow->next;
            fast = fast->next->next;
            
            if (slow == fast) {
                // Cycle detected, now find the actual length
                length = findCycleLength(slow);
                break;
            }
            
            if (power == length) {
                power *= 2;
                length = 0;
            }
            length++;
        }
        
        if (length == 0) {
            return make_pair(-1, -1); // No cycle
        }
        
        // Phase 2: Find the start of the cycle
        ListNode* start = head;
        ListNode* current = head;
        
        // Move start pointer to cycle start
        for (int i = 0; i < length; i++) {
            current = current->next;
        }
        
        while (start != current) {
            start = start->next;
            current = current->next;
        }
        
        return make_pair(start->val, length);
    }
    
private:
    // Helper function to find exact cycle length
    static int findCycleLength(ListNode* meetingPoint) {
        ListNode* current = meetingPoint->next;
        int length = 1;
        
        while (current != meetingPoint) {
            current = current->next;
            length++;
        }
        
        return length;
    }
};

// Helper function to create a cycle in the linked list for testing
ListNode* createCycle(ListNode* head, int pos) {
    if (pos == -1) return head;
    
    ListNode* tail = head;
    ListNode* cycleStart = nullptr;
    int index = 0;
    
    // Find the tail and cycle start node
    while (tail->next) {
        if (index == pos) {
            cycleStart = tail;
        }
        tail = tail->next;
        index++;
    }
    
    if (index == pos) {
        cycleStart = tail;
    }
    
    // Create cycle
    if (cycleStart) {
        tail->next = cycleStart;
    }
    
    return head;
}

// Helper function to print the linked list (with cycle detection)
void printList(ListNode* head, int maxNodes = 10) {
    ListNode* current = head;
    int count = 0;
    
    while (current && count < maxNodes) {
        cout << current->val;
        if (current->next) {
            cout << " -> ";
        } else {
            cout << " -> NULL";
        }
        current = current->next;
        count++;
    }
    
    if (count >= maxNodes && current) {
        cout << " ... (cycle detected)";
    }
    cout << endl;
}

// Example usage
int main() {
    // Create a linked list: 1 -> 2 -> 3 -> 4 -> 5 -> NULL
    ListNode* head = new ListNode(1);
    head->next = new ListNode(2);
    head->next->next = new ListNode(3);
    head->next->next->next = new ListNode(4);
    head->next->next->next->next = new ListNode(5);
    
    cout << "Original list: ";
    printList(head);
    
    // Test without cycle
    cout << "Has cycle: " << (CycleDetection::hasCycle(head) ? "Yes" : "No") << endl;
    
    // Create a cycle: 1 -> 2 -> 3 -> 4 -> 5 -> 2 (cycle back to node 2)
    head = createCycle(head, 1); // Create cycle at position 1 (node with value 2)
    
    cout << "\nList with cycle: ";
    printList(head);
    
    // Test with cycle
    cout << "Has cycle: " << (CycleDetection::hasCycle(head) ? "Yes" : "No") << endl;
    
    // Find cycle information
    pair<int, int> cycleInfo = CycleDetection::findCycleInfo(head);
    if (cycleInfo.first != -1) {
        cout << "Cycle starts at node with value: " << cycleInfo.first << endl;
        cout << "Cycle length: " << cycleInfo.second << endl;
    } else {
        cout << "No cycle found." << endl;
    }
    
    return 0;
}
```

## Key Features of Brent's Algorithm:

1. **Phase 1**: Uses exponential stepping (powers of 2) to detect cycles
2. **Phase 2**: Finds the exact cycle start point and length
3. **Time Complexity**: O(λ + μ) where λ is the cycle length and μ is the distance to cycle start
4. **Space Complexity**: O(1) - constant space

## How it works:

1. **Exponential Search**: Fast pointer moves in powers of 2 (1, 2, 4, 8, ...)
2. **Cycle Detection**: When slow and fast pointers meet, a cycle exists
3. **Cycle Length**: The distance between the meeting point and the cycle start gives the length
4. **Cycle Start**: Uses the relationship between the meeting point and cycle start to find the exact location

This implementation provides both basic cycle detection and detailed cycle information including the starting point and length of the cycle.

