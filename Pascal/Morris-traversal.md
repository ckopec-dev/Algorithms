# Morris Traversal Algorithm in Pascal

Here's an implementation of the Morris traversal algorithm in Pascal for binary tree inorder traversal:

```pascal
program MorrisTraversal;

type
  TreeNode = record
    data: integer;
    left: ^TreeNode;
    right: ^TreeNode;
  end;
  
  PTreeNode = ^TreeNode;

// Morris Inorder Traversal
procedure MorrisTraversal(root: PTreeNode);
var
  current, predecessor: PTreeNode;
begin
  current := root;
  
  while current <> nil do
  begin
    // If left child is null, visit current node and go to right child
    if current^.left = nil then
    begin
      writeln(current^.data, ' ');
      current := current^.right;
    end
    else
    begin
      // Find inorder predecessor
      predecessor := current^.left;
      while (predecessor^.right <> nil) and (predecessor^.right <> current) do
        predecessor := predecessor^.right;
      
      // If right of predecessor is null, make current as right child of predecessor
      if predecessor^.right = nil then
      begin
        predecessor^.right := current;
        current := current^.left;
      end
      else
      begin
        // If right of predecessor is current, revert the changes
        predecessor^.right := nil;
        writeln(current^.data, ' ');
        current := current^.right;
      end;
    end;
  end;
end;

// Helper function to create a new node
function CreateNode(data: integer): PTreeNode;
begin
  new(CreateNode);
  CreateNode^.data := data;
  CreateNode^.left := nil;
  CreateNode^.right := nil;
end;

// Main program
var
  root: PTreeNode;
begin
  // Create the following binary tree:
  //       1
  //      / \
  //     2   3
  //    / \
  //   4   5
  
  root := CreateNode(1);
  root^.left := CreateNode(2);
  root^.right := CreateNode(3);
  root^.left^.left := CreateNode(4);
  root^.left^.right := CreateNode(5);
  
  writeln('Morris Traversal (Inorder):');
  MorrisTraversal(root);
end.
```

## How Morris Traversal Works:

1. **Start with the root node**
2. **If current node has no left child**: Visit the node and move to right child
3. **If current node has left child**: 
   - Find the inorder predecessor (rightmost node in left subtree)
   - If predecessor's right is null: Make current node as right child of predecessor, move to left child
   - If predecessor's right is current: Revert the changes, visit current node, move to right child

## Output:
```
Morris Traversal (Inorder):
4 2 5 1 3 
```

## Key Features:

- **Space Complexity**: O(1) - No additional space for stack/queue
- **Time Complexity**: O(n) - Each edge visited at most 3 times
- **No recursion**: Uses threading technique to traverse without explicit stack
- **Inorder traversal**: Visits nodes in left-root-right order

The algorithm works by temporarily modifying the tree structure to create "threads" that allow backtracking to parent nodes without using additional memory for a stack.

