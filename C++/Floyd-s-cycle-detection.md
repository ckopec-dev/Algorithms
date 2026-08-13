# Floyd's Cycle Detection Algorithm (Tortoise and Hare)

Floyd's cycle detection algorithm is used to detect cycles in a linked list. It uses two pointers moving at different speeds to detect if there's a cycle.

## C++ Implementation

```cpp
#include <iostream>
using namespace std;

// Definition for singly-linked list node
struct ListNode {
    int val;
    ListNode* next;
    ListNode(int x) : val(x), next(nullptr) {}
};

class Solution {
public:
    // Floyd's Cycle Detection Algorithm
    bool hasCycle(ListNode* head) {
        // Handle edge cases: empty list or single node
        if (head == nullptr || head->next == nullptr) {
            return false;
        }
        
        // Initialize two pointers
        ListNode* slow = head;      // Tortoise - moves 1 step
        ListNode* fast = head;      // Hare - moves 2 steps
        
        // Traverse the list
        while (fast != nullptr && fast->next != nullptr) {
            slow = slow->next;          // Move slow pointer one step
            fast = fast->next->next;    // Move fast pointer two steps
            
            // If pointers meet, there's a cycle
            if (slow == fast) {
                return true;
            }
        }
        
        // If we reach here, no cycle exists
        return false;
    }
    
    // Optional: Find the starting node of the cycle
    ListNode* detectCycle(ListNode* head) {
        if (head == nullptr || head->next == nullptr) {
            return nullptr;
        }
        
        ListNode* slow = head;
        ListNode* fast = head;
        
        // Phase 1: Detect if cycle exists
        while (fast != nullptr && fast->next != nullptr) {
            slow = slow->next;
            fast = fast->next->next;
            
            if (slow == fast) {
                break;  // Cycle detected
            }
        }
        
        // If no cycle found
        if (fast == nullptr || fast->next == nullptr) {
            return nullptr;
        }
        
        // Phase 2: Find the start of cycle
        // Move one pointer to head, keep other at meeting point
        slow = head;
        while (slow != fast) {
            slow = slow->next;
            fast = fast->next;
        }
        
        return slow;  // Return the starting node of cycle
    }
};

// Helper function to create a linked list with cycle for testing
ListNode* createCycleList() {
    ListNode* head = new ListNode(3);
    ListNode* node2 = new ListNode(2);
    ListNode* node3 = new ListNode(0);
    ListNode* node4 = new ListNode(-4);
    
    head->next = node2;
    node2->next = node3;
    node3->next = node4;
    node4->next = node2;  // Creates cycle: -4 -> 2
    
    return head;
}

// Helper function to create a normal linked list
ListNode* createNormalList() {
    ListNode* head = new ListNode(1);
    ListNode* node2 = new ListNode(2);
    ListNode* node3 = new ListNode(3);
    
    head->next = node2;
    node2->next = node3;
    
    return head;
}

// Helper function to print list (with cycle detection)
void printList(ListNode* head, int maxNodes = 10) {
    ListNode* current = head;
    int count = 0;
    
    while (current && count < maxNodes) {
        cout << current->val;
        if (current->next) {
            cout << " -> ";
        }
        current = current->next;
        count++;
    }
    cout << endl;
}

// Test the algorithm
int main() {
    Solution solution;
    
    // Test 1: List with cycle
    cout << "Test 1: List with cycle" << endl;
    ListNode* cycleList = createCycleList();
    cout << "List structure: 3 -> 2 -> 0 -> -4 -> 2 (cycle)" << endl;
    cout << "Has cycle: " << (solution.hasCycle(cycleList) ? "Yes" : "No") << endl;
    
    // Test 2: Normal list without cycle
    cout << "\nTest 2: Normal list without cycle" << endl;
    ListNode* normalList = createNormalList();
    cout << "List structure: 1 -> 2 -> 3" << endl;
    cout << "Has cycle: " << (solution.hasCycle(normalList) ? "Yes" : "No") << endl;
    
    // Test 3: Single node with self loop
    cout << "\nTest 3: Single node with self loop" << endl;
    ListNode* singleNode = new ListNode(1);
    singleNode->next = singleNode;  // Self loop
    cout << "Has cycle: " << (solution.hasCycle(singleNode) ? "Yes" : "No") << endl;
    
    return 0;
}
```

## How It Works

1. **Two Pointers Approach**:
   - `slow` pointer moves 1 step at a time (tortoise)
   - `fast` pointer moves 2 steps at a time (hare)

2. **Cycle Detection Logic**:
   - If there's no cycle, the fast pointer will reach the end
   - If there's a cycle, the fast pointer will eventually "lap" the slow pointer

3. **Time Complexity**: O(n)
4. **Space Complexity**: O(1)

## Key Points

- The algorithm uses the mathematical property that if there's a cycle, the fast pointer will eventually catch up to the slow pointer
- The meeting point proves the existence of a cycle
- This method is memory-efficient as it only uses two pointers
- It's widely used in practice for cycle detection in linked lists