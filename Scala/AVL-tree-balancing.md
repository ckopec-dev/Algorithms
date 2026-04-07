# AVL Tree Balancing Algorithm in Scala

Here's a complete implementation of an AVL tree with balancing operations in Scala:

```scala
// AVL Tree Node definition
case class AVLNode[T](var value: T, var height: Int = 1) {
  var left: Option[AVLNode[T]] = None
  var right: Option[AVLNode[T]] = None
}

class AVLTree[T <: Ordered[T]] {
  private var root: Option[AVLNode[T]] = None
  
  // Get height of a node
  private def getHeight(node: Option[AVLNode[T]]): Int = {
    node.map(_.height).getOrElse(0)
  }
  
  // Get balance factor of a node
  private def getBalance(node: Option[AVLNode[T]]): Int = {
    node.map(n => getHeight(n.left) - getHeight(n.right)).getOrElse(0)
  }
  
  // Update height of a node
  private def updateHeight(node: Option[AVLNode[T]]): Unit = {
    node.foreach(n => n.height = 1 + math.max(getHeight(n.left), getHeight(n.right)))
  }
  
  // Right rotation
  private def rotateRight(y: AVLNode[T]): AVLNode[T] = {
    val x = y.left.get
    val T2 = x.right
    
    // Perform rotation
    x.right = Some(y)
    y.left = T2
    
    // Update heights
    updateHeight(Some(y))
    updateHeight(Some(x))
    
    x
  }
  
  // Left rotation
  private def rotateLeft(x: AVLNode[T]): AVLNode[T] = {
    val y = x.right.get
    val T2 = y.left
    
    // Perform rotation
    y.left = Some(x)
    x.right = T2
    
    // Update heights
    updateHeight(Some(x))
    updateHeight(Some(y))
    
    y
  }
  
  // Insert a value into the AVL tree
  def insert(value: T): Unit = {
    root = insertNode(root, value)
  }
  
  private def insertNode(node: Option[AVLNode[T]], value: T): Option[AVLNode[T]] = {
    // Step 1: Perform normal BST insertion
    val newNode = node match {
      case None => Some(new AVLNode(value))
      case Some(n) =>
        if (value < n.value) {
          n.left = insertNode(n.left, value)
        } else if (value > n.value) {
          n.right = insertNode(n.right, value)
        } else {
          // Duplicate values not allowed
          return Some(n)
        }
        Some(n)
    }
    
    // Step 2: Update height of current node
    updateHeight(newNode)
    
    // Step 3: Get balance factor
    val balance = getBalance(newNode)
    
    // Step 4: Perform rotations if unbalanced
    
    // Left Left Case
    if (balance > 1 && value < newNode.get.left.get.value) {
      return Some(rotateRight(newNode.get))
    }
    
    // Right Right Case
    if (balance < -1 && value > newNode.get.right.get.value) {
      return Some(rotateLeft(newNode.get))
    }
    
    // Left Right Case
    if (balance > 1 && value > newNode.get.left.get.value) {
      newNode.get.left = Some(rotateLeft(newNode.get.left.get))
      return Some(rotateRight(newNode.get))
    }
    
    // Right Left Case
    if (balance < -1 && value < newNode.get.right.get.value) {
      newNode.get.right = Some(rotateRight(newNode.get.right.get))
      return Some(rotateLeft(newNode.get))
    }
    
    newNode
  }
  
  // In-order traversal to display tree
  def inorderTraversal(): List[T] = {
    def inorder(node: Option[AVLNode[T]]): List[T] = {
      node match {
        case None => List.empty[T]
        case Some(n) => inorder(n.left) ++ List(n.value) ++ inorder(n.right)
      }
    }
    inorder(root)
  }
  
  // Print tree structure
  def printTree(): Unit = {
    def printNode(node: Option[AVLNode[T]], prefix: String, isLast: Boolean): Unit = {
      node.foreach { n =>
        println(s"$prefix${if (isLast) "└── " else "├── "}Value: ${n.value}, Height: ${n.height}")
        val children = List(n.left, n.right).flatten
        children.zipWithIndex.foreach { case (child, index) =>
          val isLastChild = index == children.length - 1
          val newPrefix = if (isLast) "    " else "│   "
          printNode(Some(child), prefix + newPrefix, isLastChild)
        }
      }
    }
    
    printNode(root, "", true)
  }
}

// Example usage
object AVLTreeExample extends App {
  val avl = new AVLTree[Int]()
  
  println("Inserting values: 10, 20, 30, 40, 50, 25")
  
  // Insert values
  val values = List(10, 20, 30, 40, 50, 25)
  values.foreach { value =>
    avl.insert(value)
    println(s"Inserted $value")
    println("Tree structure:")
    avl.printTree()
    println("=" * 30)
  }
  
  println("In-order traversal:")
  println(avl.inorderTraversal())
}
```

## Key Features of the Implementation:

### 1. **Rotation Operations**
- **Right Rotation**: Used for Left-Left case
- **Left Rotation**: Used for Right-Right case

### 2. **Balance Factor Calculation**
```scala
def getBalance(node: Option[AVLNode[T]]): Int = {
  node.map(n => getHeight(n.left) - getHeight(n.right)).getOrElse(0)
}
```

### 3. **Four Balancing Cases**
1. **Left-Left Case**: Right rotation
2. **Right-Right Case**: Left rotation  
3. **Left-Right Case**: Left rotation on left child, then right rotation
4. **Right-Left Case**: Right rotation on right child, then left rotation

### 4. **Height Management**
- Automatic height updates after each insertion
- Proper balance factor calculation

### 5. **Time Complexity**
- **Insertion**: O(log n)
- **Search**: O(log n)
- **Deletion**: O(log n)

This implementation maintains the AVL property (balance factor ≤ 1) after every insertion, ensuring optimal tree performance.

