# Floyd's Cycle Detection Algorithm in R

Floyd's Cycle Detection Algorithm (also known as the "tortoise and hare" algorithm) is used to detect cycles in linked lists. Here's an implementation in R:

```r
# Define a node structure for a linked list
Node <- function(value = NULL, next = NULL) {
  list(value = value, next = next)
}

# Floyd's Cycle Detection Algorithm
floyd_cycle_detection <- function(head) {
  # Handle empty list
  if (is.null(head) || is.null(head$next)) {
    return(FALSE)
  }
  
  # Initialize two pointers
  slow <- head
  fast <- head
  
  # Move pointers through the list
  while (!is.null(fast) && !is.null(fast$next)) {
    slow <- slow$next          # Move one step
    fast <- fast$next$next     # Move two steps
    
    # If pointers meet, there's a cycle
    if (slow === fast) {
      return(TRUE)
    }
  }
  
  # No cycle found
  return(FALSE)
}

# Helper function to create a cycle in a linked list for testing
create_cycle <- function(head, cycle_start_index) {
  # Find the last node
  current <- head
  while (!is.null(current$next)) {
    current <- current$next
  }
  
  # Find the node at cycle_start_index
  cycle_start <- head
  for (i in 1:(cycle_start_index - 1)) {
    if (!is.null(cycle_start$next)) {
      cycle_start <- cycle_start$next
    }
  }
  
  # Create cycle by connecting last node to cycle_start
  current$next <- cycle_start
  return(head)
}

# Example usage
cat("Floyd's Cycle Detection Algorithm in R\n")
cat("=====================================\n\n")

# Create a simple linked list: 1 -> 2 -> 3 -> 4 -> 5
node1 <- Node(1)
node2 <- Node(2)
node3 <- Node(3)
node4 <- Node(4)
node5 <- Node(5)

node1$next <- node2
node2$next <- node3
node3$next <- node4
node4$next <- node5

# Test with no cycle (should return FALSE)
cat("Test 1 - No cycle:\n")
result1 <- floyd_cycle_detection(node1)
cat("Cycle detected:", result1, "\n\n")

# Create a cycle: 1 -> 2 -> 3 -> 4 -> 5 -> 2 (cycle back to node 2)
cat("Creating cycle in list...\n")
node1_with_cycle <- create_cycle(node1, 2)

# Test with cycle (should return TRUE)
cat("Test 2 - With cycle:\n")
result2 <- floyd_cycle_detection(node1_with_cycle)
cat("Cycle detected:", result2, "\n\n")

# Test with single node (no cycle)
cat("Test 3 - Single node (no cycle):\n")
single_node <- Node(1)
result3 <- floyd_cycle_detection(single_node)
cat("Cycle detected:", result3, "\n\n")

# Test with single node pointing to itself (cycle)
cat("Test 4 - Single node pointing to itself (cycle):\n")
single_node_cycle <- Node(1)
single_node_cycle$next <- single_node_cycle
result4 <- floyd_cycle_detection(single_node_cycle)
cat("Cycle detected:", result4, "\n")
```

## How it works:

1. **Two Pointers**: Uses two pointers moving at different speeds
2. **Slow Pointer**: Moves one step at a time
3. **Fast Pointer**: Moves two steps at a time
4. **Cycle Detection**: If there's a cycle, the fast pointer will eventually "lap" the slow pointer
5. **Time Complexity**: O(n)
6. **Space Complexity**: O(1)

## Key Points:

- The algorithm works by leveraging the fact that in a cyclic structure, a faster-moving pointer will eventually catch up to a slower one
- It's particularly useful for detecting cycles in linked lists
- The algorithm is efficient and uses constant extra space
- It can be extended to find the start of the cycle if needed

This implementation demonstrates the classic application of Floyd's algorithm for cycle detection in linked list structures.

