# AVL Tree Balancing Algorithm in Pascal

Here's a complete implementation of an AVL tree with balancing operations in Pascal:

```pascal
program AVLTreeExample;

type
  PNode = ^TNode;
  TNode = record
    data: integer;
    left, right: PNode;
    height: integer;
  end;

  TAVLTree = record
    root: PNode;
  end;

// Function to get height of node
function getHeight(node: PNode): integer;
begin
  if node = nil then
    getHeight := 0
  else
    getHeight := node^.height;
end;

// Function to get balance factor
function getBalance(node: PNode): integer;
begin
  if node = nil then
    getBalance := 0
  else
    getBalance := getHeight(node^.left) - getHeight(node^.right);
end;

// Right rotation
function rotateRight(y: PNode): PNode;
var
  x: PNode;
  T2: PNode;
begin
  x := y^.left;
  T2 := x^.right;

  // Perform rotation
  x^.right := y;
  y^.left := T2;

  // Update heights
  y^.height := max(getHeight(y^.left), getHeight(y^.right)) + 1;
  x^.height := max(getHeight(x^.left), getHeight(x^.right)) + 1;

  rotateRight := x; // Return new root
end;

// Left rotation
function rotateLeft(x: PNode): PNode;
var
  y: PNode;
  T2: PNode;
begin
  y := x^.right;
  T2 := y^.left;

  // Perform rotation
  y^.left := x;
  x^.right := T2;

  // Update heights
  x^.height := max(getHeight(x^.left), getHeight(x^.right)) + 1;
  y^.height := max(getHeight(y^.left), getHeight(y^.right)) + 1;

  rotateLeft := y; // Return new root
end;

// Function to insert a node
function insertNode(node: PNode; data: integer): PNode;
begin
  // 1. Perform normal BST insertion
  if node = nil then
  begin
    new(node);
    node^.data := data;
    node^.left := nil;
    node^.right := nil;
    node^.height := 1;
    insertNode := node;
    exit;
  end;

  if data < node^.data then
    node^.left := insertNode(node^.left, data)
  else if data > node^.data then
    node^.right := insertNode(node^.right, data)
  else
  begin
    insertNode := node; // Duplicate values not allowed
    exit;
  end;

  // 2. Update height of current node
  node^.height := 1 + max(getHeight(node^.left), getHeight(node^.right));

  // 3. Get balance factor
  var balance := getBalance(node);

  // 4. Perform rotations if needed

  // Left Left Case
  if (balance > 1) and (data < node^.left^.data) then
  begin
    insertNode := rotateRight(node);
    exit;
  end;

  // Right Right Case
  if (balance < -1) and (data > node^.right^.data) then
  begin
    insertNode := rotateLeft(node);
    exit;
  end;

  // Left Right Case
  if (balance > 1) and (data > node^.left^.data) then
  begin
    node^.left := rotateLeft(node^.left);
    insertNode := rotateRight(node);
    exit;
  end;

  // Right Left Case
  if (balance < -1) and (data < node^.right^.data) then
  begin
    node^.right := rotateRight(node^.right);
    insertNode := rotateLeft(node);
    exit;
  end;

  insertNode := node;
end;

// Function to search for a node
function searchNode(node: PNode; data: integer): boolean;
begin
  if node = nil then
  begin
    searchNode := false;
    exit;
  end;

  if data = node^.data then
  begin
    searchNode := true;
    exit;
  end;

  if data < node^.data then
    searchNode := searchNode(node^.left, data)
  else
    searchNode := searchNode(node^.right, data);
end;

// In-order traversal
procedure inorderTraversal(node: PNode);
begin
  if node <> nil then
  begin
    inorderTraversal(node^.left);
    write(node^.data, ' ');
    inorderTraversal(node^.right);
  end;
end;

// Pre-order traversal
procedure preorderTraversal(node: PNode);
begin
  if node <> nil then
  begin
    write(node^.data, ' ');
    preorderTraversal(node^.left);
    preorderTraversal(node^.right);
  end;
end;

// Main program
var
  avl: TAVLTree;
  i: integer;
  data: integer;

begin
  avl.root := nil;
  
  writeln('AVL Tree Balancing Example');
  writeln('==========================');
  
  // Insert nodes
  writeln('Inserting nodes: 10, 20, 30, 40, 50, 25');
  
  avl.root := insertNode(avl.root, 10);
  avl.root := insertNode(avl.root, 20);
  avl.root := insertNode(avl.root, 30);
  avl.root := insertNode(avl.root, 40);
  avl.root := insertNode(avl.root, 50);
  avl.root := insertNode(avl.root, 25);
  
  writeln('In-order traversal:');
  inorderTraversal(avl.root);
  writeln;
  
  writeln('Pre-order traversal:');
  preorderTraversal(avl.root);
  writeln;
  
  // Search for values
  writeln('Searching for 25: ', searchNode(avl.root, 25));
  writeln('Searching for 15: ', searchNode(avl.root, 15));
  
  writeln('AVL Tree built successfully with balancing operations!');
end.
```

## Key Features of this AVL Implementation:

### 1. **Balance Factor Calculation**
```pascal
function getBalance(node: PNode): integer;
begin
  if node = nil then
    getBalance := 0
  else
    getBalance := getHeight(node^.left) - getHeight(node^.right);
end;
```

### 2. **Rotation Operations**
- **Right Rotation**: Used for Left-Left case
- **Left Rotation**: Used for Right-Right case

### 3. **Four Balancing Cases**
1. **Left-Left Case**: Right rotation
2. **Right-Right Case**: Left rotation  
3. **Left-Right Case**: Left rotation on left child, then right rotation
4. **Right-Left Case**: Right rotation on right child, then left rotation

### 4. **Height Maintenance**
- Each node maintains its height
- Heights are updated after every insertion
- Balance factor is calculated to determine required rotations

### 5. **Time Complexity**
- **Insertion**: O(log n)
- **Search**: O(log n)
- **Deletion**: O(log n)

This implementation demonstrates how AVL trees automatically maintain balance through rotations during insertions, ensuring optimal performance for all operations.

