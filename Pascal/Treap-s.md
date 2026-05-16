# Treap Implementation in Pascal

```pascal
program TreapExample;

type
  PNode = ^TNode;
  TNode = record
    key: integer;
    priority: integer;
    left, right: PNode;
  end;

  TTreap = record
    root: PNode;
  end;

// Function to create a new node
function CreateNode(key, priority: integer): PNode;
begin
  New(CreateNode);
  CreateNode^.key := key;
  CreateNode^.priority := priority;
  CreateNode^.left := nil;
  CreateNode^.right := nil;
end;

// Right rotation
function RotateRight(y: PNode): PNode;
var
  x: PNode;
begin
  x := y^.left;
  y^.left := x^.right;
  x^.right := y;
  RotateRight := x;
end;

// Left rotation
function RotateLeft(x: PNode): PNode;
var
  y: PNode;
begin
  y := x^.right;
  x^.right := y^.left;
  y^.left := x;
  RotateLeft := y;
end;

// Insert operation with heap property maintenance
function Insert(root: PNode; key, priority: integer): PNode;
var
  newNode: PNode;
begin
  if root = nil then
  begin
    newNode := CreateNode(key, priority);
    Insert := newNode;
    exit;
  end;

  if key < root^.key then
  begin
    root^.left := Insert(root^.left, key, priority);
    if (root^.left <> nil) and (root^.left^.priority > root^.priority) then
      root := RotateRight(root);
  end
  else if key > root^.key then
  begin
    root^.right := Insert(root^.right, key, priority);
    if (root^.right <> nil) and (root^.right^.priority > root^.priority) then
      root := RotateLeft(root);
  end;

  Insert := root;
end;

// Search operation
function Search(root: PNode; key: integer): boolean;
begin
  if root = nil then
  begin
    Search := false;
    exit;
  end;

  if key = root^.key then
  begin
    Search := true;
    exit;
  end;

  if key < root^.key then
    Search := Search(root^.left, key)
  else
    Search := Search(root^.right, key);
end;

// Inorder traversal (for testing)
procedure InorderTraversal(root: PNode);
begin
  if root <> nil then
  begin
    InorderTraversal(root^.left);
    writeln('Key: ', root^.key, ' Priority: ', root^.priority);
    InorderTraversal(root^.right);
  end;
end;

// Delete operation
function Delete(root: PNode; key: integer): PNode;
begin
  if root = nil then
  begin
    Delete := nil;
    exit;
  end;

  if key < root^.key then
    root^.left := Delete(root^.left, key)
  else if key > root^.key then
    root^.right := Delete(root^.right, key)
  else
  begin
    // Node to be deleted found
    if (root^.left = nil) and (root^.right = nil) then
    begin
      // Leaf node
      Dispose(root);
      Delete := nil;
    end
    else if root^.left = nil then
    begin
      // Node with only right child
      Delete := root^.right;
      Dispose(root);
    end
    else if root^.right = nil then
    begin
      // Node with only left child
      Delete := root^.left;
      Dispose(root);
    end
    else
    begin
      // Node with two children
      if root^.left^.priority > root^.right^.priority then
      begin
        root := RotateRight(root);
        root^.right := Delete(root^.right, key);
      end
      else
      begin
        root := RotateLeft(root);
        root^.left := Delete(root^.left, key);
      end;
    end;
  end;
end;

var
  treap: TTreap;
  i: integer;

begin
  treap.root := nil;

  // Insert some elements
  writeln('Inserting elements into Treap:');
  treap.root := Insert(treap.root, 10, 50);
  treap.root := Insert(treap.root, 20, 30);
  treap.root := Insert(treap.root, 5, 70);
  treap.root := Insert(treap.root, 15, 20);
  treap.root := Insert(treap.root, 30, 60);

  writeln('Inorder traversal of Treap:');
  InorderTraversal(treap.root);

  // Search for elements
  writeln('Searching for elements:');
  if Search(treap.root, 15) then
    writeln('Found 15')
  else
    writeln('15 not found');

  if Search(treap.root, 25) then
    writeln('Found 25')
  else
    writeln('25 not found');

  // Delete an element
  writeln('Deleting element 10:');
  treap.root := Delete(treap.root, 10);
  writeln('Inorder traversal after deletion:');
  InorderTraversal(treap.root);
end.
```

## Explanation of the Treap Implementation

This Pascal implementation demonstrates a Treap (Tree + Heap) data structure that combines:

1. **Binary Search Tree Property**: Keys in left subtree are smaller, keys in right subtree are larger
2. **Heap Property**: Each node's priority is higher than its children's priorities

### Key Features:

- **Insert Operation**: Maintains both BST and heap properties through rotations
- **Search Operation**: Standard BST search with O(log n) average time complexity
- **Delete Operation**: Removes nodes while maintaining heap property
- **Rotations**: Right and left rotations to maintain heap property

### Time Complexities:
- **Insert**: O(log n) average, O(n) worst case
- **Search**: O(log n) average, O(n) worst case  
- **Delete**: O(log n) average, O(n) worst case

The treap maintains the balance of the tree through the random priority assignment, making it a probabilistically balanced structure.

