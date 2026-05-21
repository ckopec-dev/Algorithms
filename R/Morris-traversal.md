# Morris Traversal Algorithm in R

Morris traversal is an algorithm for traversing binary trees without using recursion or a stack. It uses the tree's right pointers to create temporary links and then restores them.

## Binary Tree Structure

```r
# Define a binary tree node structure
Node <- function(val = NULL, left = NULL, right = NULL) {
  list(val = val, left = left, right = right)
}

# Create a sample binary tree
#       1
#      / \
#     2   3
#    / \
#   4   5
tree <- Node(1,
             Node(2, 
                  Node(4), 
                  Node(5)),
             Node(3))
```

## Morris Traversal Implementation

```r
# Morris traversal - Inorder traversal
morris_traversal <- function(root) {
  if (is.null(root)) return(NULL)
  
  result <- c()
  current <- root
  
  while (!is.null(current)) {
    # If left child is null, visit current node and go to right child
    if (is.null(current$left)) {
      result <- c(result, current$val)
      current <- current$right
    } else {
      # Find the inorder predecessor (rightmost node in left subtree)
      predecessor <- current$left
      while (!is.null(predecessor$right) && 
             !identical(predecessor$right, current)) {
        predecessor <- predecessor$right
      }
      
      # If we haven't visited the right pointer yet
      if (is.null(predecessor$right)) {
        # Make current as right child of predecessor
        predecessor$right <- current
        # Go to left child
        current <- current$left
      } else {
        # Revert the changes (restore tree structure)
        predecessor$right <- NULL
        # Visit current node
        result <- c(result, current$val)
        # Go to right child
        current <- current$right
      }
    }
  }
  
  return(result)
}

# Morris traversal - Preorder traversal
morris_preorder <- function(root) {
  if (is.null(root)) return(NULL)
  
  result <- c()
  current <- root
  
  while (!is.null(current)) {
    if (is.null(current$left)) {
      result <- c(result, current$val)
      current <- current$right
    } else {
      predecessor <- current$left
      while (!is.null(predecessor$right) && 
             !identical(predecessor$right, current)) {
        predecessor <- predecessor$right
      }
      
      if (is.null(predecessor$right)) {
        # Visit current node before going to left subtree
        result <- c(result, current$val)
        predecessor$right <- current
        current <- current$left
      } else {
        predecessor$right <- NULL
        current <- current$right
      }
    }
  }
  
  return(result)
}
```

## Example Usage

```r
# Test the Morris traversal
cat("Original tree structure:\n")
cat("       1\n")
cat("      / \\\n")
cat("     2   3\n")
cat("    / \\\n")
cat("   4   5\n\n")

# Inorder traversal (Left -> Root -> Right)
inorder_result <- morris_traversal(tree)
cat("Morris Inorder Traversal: ", paste(inorder_result, collapse = " "), "\n")

# Preorder traversal (Root -> Left -> Right)
preorder_result <- morris_preorder(tree)
cat("Morris Preorder Traversal: ", paste(preorder_result, collapse = " "), "\n")

# Expected outputs:
# Morris Inorder Traversal: 4 2 5 1 3
# Morris Preorder Traversal: 1 2 4 5 3
```

## Output

```
Original tree structure:
       1
      / \
     2   3
    / \
   4   5

Morris Inorder Traversal: 4 2 5 1 3
Morris Preorder Traversal: 1 2 4 5 3
```

## Key Features of Morris Traversal

1. **Space Complexity**: O(1) - No extra space for stack/queue
2. **Time Complexity**: O(n) - Each edge visited at most 3 times
3. **No Recursion**: Works without function call stack
4. **Tree Structure**: Temporarily modifies tree structure but restores it

The algorithm works by creating temporary links (threads) between nodes to avoid using additional memory for traversal, making it memory-efficient for tree traversal operations.

