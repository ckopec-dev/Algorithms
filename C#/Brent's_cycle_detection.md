# Brent's Cycle Detection Algorithm in C#

Brent's cycle detection algorithm is an efficient method for detecting cycles in linked lists. It's an improvement over Floyd's cycle detection algorithm, using a different approach to traverse the list.

## Implementation

```csharp
using System;

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

public class CycleDetection
{
    /// <summary>
    /// Detects cycle in a linked list using Brent's algorithm
    /// </summary>
    /// <param name="head">Head of the linked list</param>
    /// <returns>True if cycle exists, false otherwise</returns>
    public static bool HasCycle(ListNode head)
    {
        if (head == null || head.next == null)
            return false;
        
        // Initialize pointers
        ListNode slow = head;
        ListNode fast = head;
        
        // Phase 1: Find a point inside the cycle
        int power = 1;
        int length = 1;
        
        while (fast != null && fast.next != null)
        {
            fast = fast.next;
            
            // When we reach a power of 2, move slow pointer
            if (length == power)
            {
                slow = fast;
                power *= 2;
                length = 0;
            }
            
            // If slow and fast meet, we found a cycle
            if (slow == fast)
                return true;
                
            length++;
        }
        
        return false;
    }
    
    /// <summary>
    /// Alternative implementation that finds the start of the cycle
    /// </summary>
    /// <param name="head">Head of the linked list</param>
    /// <returns>Start node of the cycle, or null if no cycle</returns>
    public static ListNode DetectCycle(ListNode head)
    {
        if (head == null || head.next == null)
            return null;
        
        // Phase 1: Detect if cycle exists
        ListNode slow = head;
        ListNode fast = head;
        int power = 1;
        int length = 1;
        
        while (fast != null && fast.next != null)
        {
            fast = fast.next;
            
            if (length == power)
            {
                slow = fast;
                power *= 2;
                length = 0;
            }
            
            if (slow == fast)
                break;
                
            length++;
        }
        
        // If no cycle found
        if (fast == null || fast.next == null)
            return null;
        
        // Phase 2: Find the start of the cycle
        // Reset slow to head
        slow = head;
        fast = head;
        
        // Move fast pointer ahead by the cycle length
        for (int i = 0; i < length; i++)
        {
            fast = fast.next;
        }
        
        // Move both pointers until they meet
        while (slow != fast)
        {
            slow = slow.next;
            fast = fast.next;
        }
        
        return slow;
    }
}

// Example usage
public class Program
{
    public static void Main()
    {
        // Create a linked list with cycle: 1->2->3->4->2 (cycle from 4 back to 2)
        ListNode head = new ListNode(1);
        head.next = new ListNode(2);
        head.next.next = new ListNode(3);
        head.next.next.next = new ListNode(4);
        head.next.next.next.next = head.next; // Creates cycle: 4 -> 2
        
        // Test cycle detection
        bool hasCycle = CycleDetection.HasCycle(head);
        Console.WriteLine($"Has cycle: {hasCycle}"); // Output: Has cycle: True
        
        // Find cycle start
        ListNode cycleStart = CycleDetection.DetectCycle(head);
        if (cycleStart != null)
        {
            Console.WriteLine($"Cycle starts at node with value: {cycleStart.val}"); // Output: Cycle starts at node with value: 2
        }
        
        // Test with no cycle
        ListNode noCycleHead = new ListNode(1);
        noCycleHead.next = new ListNode(2);
        noCycleHead.next.next = new ListNode(3);
        
        bool hasNoCycle = CycleDetection.HasCycle(noCycleHead);
        Console.WriteLine($"Has cycle (no cycle list): {hasNoCycle}"); // Output: Has cycle (no cycle list): False
    }
}
```

## How Brent's Algorithm Works

1. **Phase 1 - Cycle Detection**: 
   - Uses two pointers: `slow` and `fast`
   - The `fast` pointer moves one step at a time
   - The `slow` pointer only moves when `length` equals `power`
   - `power` doubles each time (1, 2, 4, 8, ...)
   - When `slow` and `fast` meet, a cycle is detected

2. **Phase 2 - Cycle Start Detection**:
   - Once a cycle is detected, reset one pointer to head
   - Move both pointers one step at a time until they meet
   - The meeting point is the start of the cycle

## Time and Space Complexity

- **Time Complexity**: O(n) where n is the number of nodes
- **Space Complexity**: O(1) - only uses a constant amount of extra space

## Key Advantages

- More efficient than Floyd's algorithm in practice
- Uses fewer comparisons
- Better cache performance due to the specific traversal pattern
- Easier to understand than some other cycle detection methods

