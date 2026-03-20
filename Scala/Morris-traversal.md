# Morris Traversal in Scala

Morris traversal is an algorithm for traversing binary trees without using recursion or a stack. It uses the tree's structure itself to create temporary links for traversal.

## Implementation

```scala
// Definition for a binary tree node
class TreeNode(var _value: Int = 0) {
  var value: Int = _value
  var left: TreeNode = null
  var right: TreeNode = null
}

object MorrisTraversal {
  
  /**
   * Morris inorder traversal
   * Time Complexity: O(n)
   * Space Complexity: O(1)
   */
  def morrisInorder(root: TreeNode): List[Int] = {
    val result = scala.collection.mutable.ListBuffer[Int]()
    var current = root
    
    while (current != null) {
      if (current.left == null) {
        // Process current node
        result += current.value
        current = current.right
      } else {
        // Find the inorder predecessor
        var predecessor = current.left
        while (predecessor.right != null && predecessor.right != current) {
          predecessor = predecessor.right
        }
        
        if (predecessor.right == null) {
          // Make current as right child of predecessor
          predecessor.right = current
          current = current.left
        } else {
          // Revert the changes (restore tree structure)
          predecessor.right = null
          // Process current node
          result += current.value
          current = current.right
        }
      }
    }
    
    result.toList
  }
  
  /**
   * Morris preorder traversal
   * Time Complexity: O(n)
   * Space Complexity: O(1)
   */
  def morrisPreorder(root: TreeNode): List[Int] = {
    val result = scala.collection.mutable.ListBuffer[Int]()
    var current = root
    
    while (current != null) {
      if (current.left == null) {
        // Process current node
        result += current.value
        current = current.right
      } else {
        // Find the inorder predecessor
        var predecessor = current.left
        while (predecessor.right != null && predecessor.right != current) {
          predecessor = predecessor.right
        }
        
        if (predecessor.right == null) {
          // Process current node (preorder)
          result += current.value
          // Make current as right child of predecessor
          predecessor.right = current
          current = current.left
        } else {
          // Revert the changes (restore tree structure)
          predecessor.right = null
          current = current.right
        }
      }
    }
    
    result.toList
  }
}

// Example usage
object Main extends App {
  // Create a sample binary tree:
  //       1
  //      / \
  //     2   3
  //    / \
  //   4   5
  
  val root = new TreeNode(1)
  root.left = new TreeNode(2)
  root.right = new TreeNode(3)
  root.left.left = new TreeNode(4)
  root.left.right = new TreeNode(5)
  
  println("Inorder traversal: " + MorrisTraversal.morrisInorder(root))
  // Output: List(4, 2, 5, 1, 3)
  
  println("Preorder traversal: " + MorrisTraversal.morrisPreorder(root))
  // Output: List(1, 2, 4, 5, 3)
}
```

## How it works:

1. **Inorder Traversal**:
   - If current node has no left child, process it and move to right
   - If it has a left child, find the inorder predecessor
   - Create a temporary link from predecessor to current node
   - Move to left subtree
   - When returning, remove the temporary link and process current node

2. **Preorder Traversal**:
   - Similar logic but processes the current node before moving to left subtree

## Key Advantages:

- **Space Efficient**: O(1) space complexity (no stack or recursion)
- **Time Efficient**: O(n) time complexity
- **No Extra Memory**: Uses existing tree structure for temporary links

## Time and Space Complexity:

- **Time Complexity**: O(n) where n is the number of nodes
- **Space Complexity**: O(1) - constant extra space

This implementation demonstrates the elegant use of tree's own structure to avoid additional data structures for traversal.

