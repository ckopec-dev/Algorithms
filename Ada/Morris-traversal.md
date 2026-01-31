# Morris Traversal Algorithm in Ada

```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Morris_Traversal is
   
   -- Define the binary tree node structure
   type Node is record
      Data   : Integer;
      Left   : access Node := null;
      Right  : access Node := null;
   end record;
   
   type Node_Access is access Node;
   
   -- Morris traversal procedure
   procedure Morris_Traverse(Root : Node_Access) is
      Current : Node_Access := Root;
      Predecessor : Node_Access;
   begin
      while Current /= null loop
         if Current.Left = null then
            -- Process current node
            Put(Integer'Image(Current.Data));
            Put(" ");
            Current := Current.Right;
         else
            -- Find the inorder predecessor
            Predecessor := Current.Left;
            while Predecessor.Right /= null and then Predecessor.Right /= Current loop
               Predecessor := Predecessor.Right;
            end loop;
            
            if Predecessor.Right = null then
               -- Make current the right child of predecessor
               Predecessor.Right := Current;
               Current := Current.Left;
            else
               -- Revert the changes made
               Predecessor.Right := null;
               -- Process current node
               Put(Integer'Image(Current.Data));
               Put(" ");
               Current := Current.Right;
            end if;
         end if;
      end loop;
      New_Line;
   end Morris_Traverse;
   
   -- Helper procedure to create a new node
   function Create_Node(Data : Integer) return Node_Access is
      New_Node : Node_Access := new Node'(Data => Data, Left => null, Right => null);
   begin
      return New_Node;
   end Create_Node;
   
begin
   -- Create a sample binary tree:
   --       1
   --      / \
   --     2   3
   --    / \
   --   4   5
   
   declare
      Root : Node_Access := Create_Node(1);
   begin
      Root.Left := Create_Node(2);
      Root.Right := Create_Node(3);
      Root.Left.Left := Create_Node(4);
      Root.Left.Right := Create_Node(5);
      
      Put_Line("Morris Traversal (Inorder):");
      Morris_Traverse(Root);
   end;
   
end Morris_Traversal;
```

## Explanation

This Ada implementation of Morris traversal demonstrates:

1. **Binary Tree Structure**: A simple binary tree node with data and left/right pointers
2. **Morris Traversal Logic**: 
   - Uses threading to avoid recursion or stack
   - Finds inorder predecessor for each node
   - Temporarily modifies tree structure to create links
   - Reverts changes after processing
3. **Inorder Traversal**: Outputs nodes in the order 4 2 5 1 3

## Key Features

- **Memory Efficient**: O(1) space complexity (no stack or recursion)
- **Threaded Tree**: Temporarily modifies tree structure to create predecessor links
- **Standard Output**: Prints traversal result to console
- **Ada Conventions**: Uses proper Ada syntax and conventions

The algorithm traverses the binary tree in inorder without using additional memory for a stack or recursion, making it space-efficient for large trees.

