# Brent's Cycle Detection Algorithm

Brent's cycle detection algorithm is an improved version of Floyd's cycle detection algorithm that uses a different approach to detect cycles in linked lists or sequences.

## Algorithm Explanation

The algorithm works by:
1. Using two pointers (slow and fast)
2. The fast pointer moves at twice the speed of the slow pointer
3. When they meet, a cycle is detected
4. Uses a different method to find the cycle start point

## Python Implementation

```python
def brent_cycle_detection(head):
    """
    Detect cycle in a linked list using Brent's algorithm
    
    Args:
        head: Head node of the linked list
        
    Returns:
        tuple: (has_cycle, cycle_start_node) or (False, None) if no cycle
    """
    if not head or not head.next:
        return False, None
    
    # Phase 1: Detect if cycle exists
    # Start with slow = head, fast = head.next
    slow = head
    fast = head.next
    
    # Move fast pointer ahead by one step initially
    steps = 1
    
    while fast and fast != slow:
        # If we've moved enough steps, reset the slow pointer
        if steps == 0:
            slow = fast
            steps = 1
        else:
            steps -= 1
        
        fast = fast.next
    
    # If fast reached None, no cycle
    if not fast:
        return False, None
    
    # Phase 2: Find the start of the cycle
    # Reset slow to head and move both pointers one step at a time
    slow = head
    fast = fast.next  # Move fast one step ahead
    
    while slow != fast:
        slow = slow.next
        fast = fast.next
    
    return True, slow

# Alternative implementation with step counting
def brent_cycle_detection_v2(head):
    """
    Alternative implementation with explicit step counting
    """
    if not head or not head.next:
        return False, None
    
    # Phase 1: Find cycle using Brent's method
    power = 1
    length = 1
    slow = head
    fast = head
    
    while True:
        # Move fast pointer ahead by one step
        fast = fast.next
        length += 1
        
        # If we've moved enough steps, reset the slow pointer
        if power == length:
            slow = fast
            power *= 2
            length = 0
            
        # Check if cycle detected
        if slow == fast:
            break
    
    # If no cycle found
    if not fast or not fast.next:
        return False, None
    
    # Phase 2: Find the start of the cycle
    slow = head
    fast = fast.next
    
    while slow != fast:
        slow = slow.next
        fast = fast.next
    
    return True, slow

# Example usage with a simple linked list
class ListNode:
    def __init__(self, val=0, next=None):
        self.val = val
        self.next = next

# Create a cycle: 1 -> 2 -> 3 -> 4 -> 2 (cycle)
node1 = ListNode(1)
node2 = ListNode(2)
node3 = ListNode(3)
node4 = ListNode(4)

node1.next = node2
node2.next = node3
node3.next = node4
node4.next = node2  # Creates cycle

# Test the algorithm
has_cycle, cycle_start = brent_cycle_detection(node1)
print(f"Has cycle: {has_cycle}")
if has_cycle:
    print(f"Cycle starts at node with value: {cycle_start.val}")

# Test with no cycle
node5 = ListNode(5)
node6 = ListNode(6)
node7 = ListNode(7)

node5.next = node6
node6.next = node7

has_cycle2, cycle_start2 = brent_cycle_detection(node5)
print(f"Has cycle (no cycle test): {has_cycle2}")
```

## Key Features

1. **Efficiency**: O(n) time complexity and O(1) space complexity
2. **Better Performance**: Often faster than Floyd's algorithm in practice
3. **Cycle Detection**: Can detect both existence and start point of cycles
4. **Memory Efficient**: Uses only a constant amount of extra memory

## Time Complexity
- **Time**: O(n) where n is the number of nodes in the list
- **Space**: O(1) - constant space usage

## Applications
- Detecting cycles in linked lists
- Finding cycle start points in sequences
- Algorithm optimization problems
- Mathematical sequence analysis

