```csharp
using System;
using System.Collections.Generic;

public class BrentsCycleDetection
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

        // Phase 1: Find a point inside the cycle
        ListNode slow = head;
        ListNode fast = head.next;

        int power = 1;
        int length = 1;

        while (slow != fast)
        {
            if (length == power)
            {
                // Move to next power of 2
                slow = fast;
                power *= 2;
                length = 0;
            }

            if (fast.next == null)
                return false;

            fast = fast.next;
            length++;
        }

        // Phase 2: Find the start of the cycle
        slow = head;
        fast = head;

        // Move fast pointer to the cycle start
        for (int i = 0; i < length; i++)
        {
            fast = fast.next;
        }

        // Move both pointers until they meet at cycle start
        while (slow != fast)
        {
            slow = slow.next;
            fast = fast.next;
        }

        return true;
    }

    /// <summary>
    /// Alternative implementation that returns the cycle start node
    /// </summary>
    /// <param name="head">Head of the linked list</param>
    /// <returns>Node where cycle starts, or null if no cycle</returns>
    public static ListNode DetectCycle(ListNode head)
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
                break;
        }

        // No cycle found
        if (fast == null || fast.next == null)
            return null;

        // Phase 2: Find the start of the cycle
        slow = head;
        while (slow != fast)
        {
            slow = slow.next;
            fast = fast.next;
        }

        return slow;
    }

    /// <summary>
    /// Helper class for linked list node
    /// </summary>
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

    // Example usage
    public static void Main()
    {
        // Create a linked list with cycle: 1 -> 2 -> 3 -> 4 -> 2 (cycle)
        ListNode head = new ListNode(1);
        head.next = new ListNode(2);
        head.next.next = new ListNode(3);
        head.next.next.next = new ListNode(4);
        head.next.next.next.next = head.next; // Creates cycle: 4 -> 2

        // Test Brent's algorithm
        bool hasCycle = HasCycle(head);
        Console.WriteLine($"Has cycle: {hasCycle}"); // Output: True

        // Find cycle start
        ListNode cycleStart = DetectCycle(head);
        if (cycleStart != null)
        {
            Console.WriteLine($"Cycle starts at node with value: {cycleStart.val}"); // Output: 2
        }
    }
}
```

## Algorithm Explanation

Brent's Cycle Detection Algorithm is an improvement over Floyd's Tortoise and Hare algorithm with better performance characteristics:

### Key Features:
- **Time Complexity**: O(λ + μ) where λ is cycle length and μ is distance to cycle start
- **Space Complexity**: O(1)
- **Advantages**: Fewer pointer movements, better cache locality

### How it works:
1. **Phase 1**: Uses a "power-of-2" approach to find a point inside the cycle
2. **Phase 2**: Determines the cycle start by moving both pointers at the same speed

### Key Differences from Floyd's:
- Uses exponential search (powers of 2) instead of linear search
- Reduces the number of steps needed in most cases
- More efficient in practice despite similar theoretical complexity