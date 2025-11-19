# Morris Traversal Algorithm in Ada

Morris traversal is an algorithm for traversing binary trees without using recursion or a stack. Here's an implementation in Ada:

```ada
-- Binary Tree Node Definition
package Tree_Package is
   type Node;
   type Node_Access is access Node;
   
   type Node is record
      Data   : Integer;
      Left   : Node_Access;
      Right  : Node_Access;
   end record;
   
   procedure Print_Inorder(Morris_Root : Node_Access);
   function Create_Node(Data : Integer) return Node_Access;
end Tree_Package;

package body Tree_Package is
   
   function Create_Node(Data : Integer) return Node_Access is
      New_Node : Node_Access := new Node;
   begin
      New_Node.Data := Data;
      New_Node.Left := null;
      New_Node.Right := null;
      return New_Node;
   end Create_Node;
   
   procedure Print_Inorder(Morris_Root : Node_Access) is
      Current : Node_Access := Morris_Root;
      Predecessor : Node_Access;
   begin
      while Current /= null loop
         -- If left child is null, visit current node and go to right child
         if Current.Left = null then
            Put_Line(Integer'Image(Current.Data));
            Current := Current.Right;
         else
            -- Find the inorder predecessor of current node
            Predecessor := Current.Left;
            while Predecessor.Right /= null and then Predecessor.Right /= Current loop
               Predecessor := Predecessor.Right;
            end loop;
            
            -- Make current node as right child of predecessor
            if Predecessor.Right = null then
               Predecessor.Right := Current;
               Current := Current.Left;
            else
               -- Revert the changes, i.e., remove the link
               Predecessor.Right := null;
               Put_Line(Integer'Image(Current.Data));
               Current := Current.Right;
            end if;
         end if;
      end loop;
   end Print_Inorder;
   
end Tree_Package;

-- Main Program
with Tree_Package;
procedure Morris_Traversal is
   use Tree_Package;
   
   -- Create a sample binary tree:
   --       1
   --      / \
   --     2   3
   --    / \
   --   4   5
   Root : Node_Access;
   
begin
   -- Build the tree
   Root := Create_Node(1);
   Root.Left := Create_Node(2);
   Root.Right := Create_Node(3);
   Root.Left.Left := Create_Node(4);
   Root.Left.Right := Create_Node(5);
   
   -- Perform Morris inorder traversal
   Put_Line("Morris Inorder Traversal:");
   Print_Inorder(Root);
   
end Morris_Traversal;
```

## How it works:

1. **Initialization**: Start with the root node
2. **Left child check**: If current node has no left child, visit it and move to right child
3. **Find predecessor**: If left child exists, find the inorder predecessor (rightmost node in left subtree)
4. **Create link**: Make current node the right child of predecessor
5. **Move left**: Move to left child
6. **Revert changes**: When returning to a node that was modified, remove the link and visit the node

## Output:
```
Morris Inorder Traversal:
 4
 2
 5
 1
 3
```

This implementation performs an inorder traversal without using additional space for a stack or recursion, making it memory efficient for tree traversal operations.

