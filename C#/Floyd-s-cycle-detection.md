# Floyd's Cycle Detection Algorithm in C#

Floyd's Cycle Detection Algorithm (also known as the "Tortoise and Hare" algorithm) is used to detect cycles in a linked list. Here's an implementation in C#:

```csharp
using System;

// Definition for singly-linked list node
public class ListNode
{
    public int val;
    public ListNode next;
    
    public ListNode(int val = 0, ListNode next = null)
    {
        this.val = val;
        this.next = next;
    }
}

public class Solution
{
    /// <summary>
    /// Detects if there is a cycle in the linked list using Floyd's algorithm
    /// </summary>
    /// <param name="head">Head of the linked list</param>
    /// <returns>True if cycle exists, false otherwise</returns>
    public bool HasCycle(ListNode head)
    {
        // Handle edge cases
        if (head == null || head.next == null)
            return false;
        
        // Initialize two pointers
        ListNode slow = head;    // Tortoise - moves one step
        ListNode fast = head;    // Hare - moves two steps
        
        // Traverse the list
        while (fast != null && fast.next != null)
        {
            slow = slow.next;          // Move slow pointer one step
            fast = fast.next.next;     // Move fast pointer two steps
            
            // If they meet, there's a cycle
            if (slow == fast)
                return true;
        }
        
        // If fast pointer reaches the end, no cycle
        return false;
    }
    
    /// <summary>
    /// Returns the node where the cycle begins (if exists)
    /// </summary>
    /// <param name="head">Head of the linked list</param>
    /// <returns>Node where cycle begins, or null if no cycle</returns>
    public ListNode DetectCycle(ListNode head)
    {
        if (head == null || head.next == null)
            return null;
        
        // Phase 1: Detect if cycle exists
        ListNode slow = head;
        ListNode fast = head;
        
        while (fast != null && fast.next != null)
        {
            slow = slow.next;
            fast = fast.next.next;
            
            if (slow == fast)
                break;  // Cycle detected
        }
        
        // If no cycle found
        if (fast == null || fast.next == null)
            return null;
        
        // Phase 2: Find the start of cycle
        // Move one pointer to head, keep other at meeting point
        slow = head;
        while (slow != fast)
        {
            slow = slow.next;
            fast = fast.next;
        }
        
        return slow;  // This is the start of the cycle
    }
}

// Example usage
public class Program
{
    public static void Main()
    {
        Solution solution = new Solution();
        
        // Example 1: Linked list with cycle
        // 3 -> 2 -> 0 -> -4 -> 2 (cycle back to index 1)
        ListNode node1 = new ListNode(3);
        ListNode node2 = new ListNode(2);
        ListNode node3 = new ListNode(0);
        ListNode node4 = new ListNode(-4);
        
        node1.next = node2;
        node2.next = node3;
        node3.next = node4;
        node4.next = node2;  // Creates cycle
        
        bool hasCycle = solution.HasCycle(node1);
        Console.WriteLine($"Has cycle: {hasCycle}");  // Output: True
        
        ListNode cycleStart = solution.DetectCycle(node1);
        Console.WriteLine($"Cycle starts at value: {cycleStart?.val}");  // Output: 2
        
        // Example 2: Linked list without cycle
        // 1 -> 2 -> 3 -> 4 -> null
        ListNode nodeA = new ListNode(1);
        ListNode nodeB = new ListNode(2);
        ListNode nodeC = new ListNode(3);
        ListNode nodeD = new ListNode(4);
        
        nodeA.next = nodeB;
        nodeB.next = nodeC;
        nodeC.next = nodeD;
        
        bool hasCycle2 = solution.HasCycle(nodeA);
        Console.WriteLine($"Has cycle: {hasCycle2}");  // Output: False
    }
}
```

## How it works:

1. **Phase 1 - Cycle Detection**: 
   - Use two pointers: slow (tortoise) moves one step at a time, fast (hare) moves two steps
   - If there's a cycle, they will eventually meet
   - If fast reaches the end, there's no cycle

2. **Phase 2 - Finding Cycle Start**:
   - Once a cycle is detected, reset one pointer to head
   - Move both pointers one step at a time
   - They will meet at the start of the cycle

## Time and Space Complexity:
- **Time Complexity**: O(n)
- **Space Complexity**: O(1)

## Key Points:
- The algorithm uses the mathematical property that if there's a cycle, the fast pointer will eventually "lap" the slow pointer
- This is an elegant solution that requires only constant extra space
- The cycle detection is particularly useful in problems involving linked list manipulation

