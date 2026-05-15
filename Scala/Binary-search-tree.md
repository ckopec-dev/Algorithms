# Binary Search Tree in Scala

Here's an implementation of a Binary Search Tree algorithm in Scala:

```scala
// Define the TreeNode class
case class TreeNode(value: Int, left: Option[TreeNode] = None, right: Option[TreeNode] = None)

// Binary Search Tree implementation
class BinarySearchTree {
  private var root: Option[TreeNode] = None
  
  // Insert a value into the BST
  def insert(value: Int): Unit = {
    root = insertHelper(root, value)
  }
  
  private def insertHelper(node: Option[TreeNode], value: Int): Option[TreeNode] = {
    node match {
      case None => Some(TreeNode(value))
      case Some(currentNode) =>
        if (value < currentNode.value) {
          val newLeft = insertHelper(currentNode.left, value)
          Some(TreeNode(currentNode.value, newLeft, currentNode.right))
        } else if (value > currentNode.value) {
          val newRight = insertHelper(currentNode.right, value)
          Some(TreeNode(currentNode.value, currentNode.left, newRight))
        } else {
          // Value already exists, don't insert duplicates
          node
        }
    }
  }
  
  // Search for a value in the BST
  def search(value: Int): Boolean = {
    searchHelper(root, value)
  }
  
  private def searchHelper(node: Option[TreeNode], value: Int): Boolean = {
    node match {
      case None => false
      case Some(currentNode) =>
        if (value == currentNode.value) true
        else if (value < currentNode.value) searchHelper(currentNode.left, value)
        else searchHelper(currentNode.right, value)
    }
  }
  
  // In-order traversal (returns sorted values)
  def inorderTraversal(): List[Int] = {
    inorderHelper(root)
  }
  
  private def inorderHelper(node: Option[TreeNode]): List[Int] = {
    node match {
      case None => List()
      case Some(currentNode) =>
        inorderHelper(currentNode.left) ++ List(currentNode.value) ++ inorderHelper(currentNode.right)
    }
  }
  
  // Print the tree structure
  def printTree(): Unit = {
    printTreeHelper(root, "", true)
  }
  
  private def printTreeHelper(node: Option[TreeNode], prefix: String, isLast: Boolean): Unit = {
    node match {
      case None =>
      case Some(currentNode) =>
        println(prefix + (if (isLast) "└── " else "├── ") + currentNode.value)
        
        val children = List(currentNode.left, currentNode.right).filter(_.isDefined)
        children.zipWithIndex.foreach { case (child, index) =>
          val isLastChild = index == children.length - 1
          val newPrefix = prefix + (if (isLast) "    " else "│   ")
          printTreeHelper(child, newPrefix, isLastChild)
        }
    }
  }
}

// Example usage
object BSTExample extends App {
  val bst = new BinarySearchTree()
  
  // Insert values
  val values = List(50, 30, 70, 20, 40, 60, 80)
  values.foreach(bst.insert)
  
  // Print the tree
  println("Binary Search Tree:")
  bst.printTree()
  
  // Search for values
  println(s"\nSearch 40: ${bst.search(40)}")  // true
  println(s"Search 25: ${bst.search(25)}")  // false
  
  // In-order traversal (should be sorted)
  println(s"\nIn-order traversal: ${bst.inorderTraversal()}")
}
```

## Output Example:
```
Binary Search Tree:
└── 50
    ├── 30
    │   ├── 20
    │   └── 40
    └── 70
        ├── 60
        └── 80

Search 40: true
Search 25: false

In-order traversal: List(20, 30, 40, 50, 60, 70, 80)
```

## Key Features:

1. **Insert Operation**: Maintains BST property (left subtree < node < right subtree)
2. **Search Operation**: Efficient O(log n) search time in balanced trees
3. **In-order Traversal**: Returns elements in sorted order
4. **Tree Visualization**: Pretty-print the tree structure
5. **Immutable Design**: Uses case classes for clean, functional-style code

## Time Complexities:
- **Search**: O(log n) average, O(n) worst case
- **Insert**: O(log n) average, O(n) worst case
- **Traversal**: O(n)

