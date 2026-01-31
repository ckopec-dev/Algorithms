# AVL Tree Balancing Algorithm in Ada

Here's a complete implementation of an AVL tree with balancing operations in Ada:

```ada
-- AVL Tree Implementation in Ada
package AVL_Trees is
   type AVL_Node;
   type AVL_Node_Access is access AVL_Node;
   
   type AVL_Node is record
      Data     : Integer;
      Left     : AVL_Node_Access;
      Right    : AVL_Node_Access;
      Height   : Integer;
   end record;
   
   type AVL_Tree is record
      Root : AVL_Node_Access;
   end record;
   
   function Get_Height(Node : AVL_Node_Access) return Integer;
   function Get_Balance_Factor(Node : AVL_Node_Access) return Integer;
   function Rotate_Right(Y : AVL_Node_Access) return AVL_Node_Access;
   function Rotate_Left(X : AVL_Node_Access) return AVL_Node_Access;
   procedure Update_Height(Node : AVL_Node_Access);
   function Insert_Root(Tree : in out AVL_Tree; Data : Integer) return AVL_Node_Access;
   function Insert_Node(Node : AVL_Node_Access; Data : Integer) return AVL_Node_Access;
   procedure Print_In_Order(Node : AVL_Node_Access);
   
end AVL_Trees;

package body AVL_Trees is
   
   function Get_Height(Node : AVL_Node_Access) return Integer is
   begin
      if Node = null then
         return 0;
      else
         return Node.Height;
      end if;
   end Get_Height;
   
   function Get_Balance_Factor(Node : AVL_Node_Access) return Integer is
   begin
      if Node = null then
         return 0;
      else
         return Get_Height(Node.Left) - Get_Height(Node.Right);
      end if;
   end Get_Balance_Factor;
   
   procedure Update_Height(Node : AVL_Node_Access) is
   begin
      if Node /= null then
         Node.Height := 1 + Integer'Max(Get_Height(Node.Left), Get_Height(Node.Right));
      end if;
   end Update_Height;
   
   function Rotate_Right(Y : AVL_Node_Access) return AVL_Node_Access is
      X : AVL_Node_Access;
      T2 : AVL_Node_Access;
   begin
      X := Y.Left;
      T2 := X.Right;
      
      -- Perform rotation
      X.Right := Y;
      Y.Left := T2;
      
      -- Update heights
      Update_Height(Y);
      Update_Height(X);
      
      return X;
   end Rotate_Right;
   
   function Rotate_Left(X : AVL_Node_Access) return AVL_Node_Access is
      Y : AVL_Node_Access;
      T2 : AVL_Node_Access;
   begin
      Y := X.Right;
      T2 := Y.Left;
      
      -- Perform rotation
      Y.Left := X;
      X.Right := T2;
      
      -- Update heights
      Update_Height(X);
      Update_Height(Y);
      
      return Y;
   end Rotate_Left;
   
   function Insert_Node(Node : AVL_Node_Access; Data : Integer) return AVL_Node_Access is
      Balanced_Node : AVL_Node_Access;
   begin
      -- Step 1: Perform normal BST insertion
      if Node = null then
         declare
            New_Node : AVL_Node_Access := new AVL_Node'(Data, null, null, 1);
         begin
            return New_Node;
         end;
      elsif Data < Node.Data then
         Node.Left := Insert_Node(Node.Left, Data);
      elsif Data > Node.Data then
         Node.Right := Insert_Node(Node.Right, Data);
      else
         -- Duplicate values not allowed
         return Node;
      end if;
      
      -- Step 2: Update height of current node
      Update_Height(Node);
      
      -- Step 3: Get balance factor
      declare
         Balance : Integer := Get_Balance_Factor(Node);
      begin
         -- Step 4: Perform rotations if needed
         
         -- Left Left Case
         if Balance > 1 and then Data < Node.Left.Data then
            return Rotate_Right(Node);
         end if;
         
         -- Right Right Case
         if Balance < -1 and then Data > Node.Right.Data then
            return Rotate_Left(Node);
         end if;
         
         -- Left Right Case
         if Balance > 1 and then Data > Node.Left.Data then
            Node.Left := Rotate_Left(Node.Left);
            return Rotate_Right(Node);
         end if;
         
         -- Right Left Case
         if Balance < -1 and then Data < Node.Right.Data then
            Node.Right := Rotate_Right(Node.Right);
            return Rotate_Left(Node);
         end if;
      end;
      
      return Node;
   end Insert_Node;
   
   function Insert_Root(Tree : in out AVL_Tree; Data : Integer) return AVL_Node_Access is
   begin
      Tree.Root := Insert_Node(Tree.Root, Data);
      return Tree.Root;
   end Insert_Root;
   
   procedure Print_In_Order(Node : AVL_Node_Access) is
   begin
      if Node /= null then
         Print_In_Order(Node.Left);
         Put_Line(Integer'Image(Node.Data) & " (Height: " & Integer'Image(Node.Height) & ")");
         Print_In_Order(Node.Right);
      end if;
   end Print_In_Order;
   
end AVL_Trees;

-- Example usage
with AVL_Trees; use AVL_Trees;
procedure AVL_Example is
   Tree : AVL_Tree;
   Root : AVL_Node_Access;
begin
   -- Insert elements that will require balancing
   Root := Insert_Root(Tree, 10);
   Root := Insert_Root(Tree, 20);
   Root := Insert_Root(Tree, 30);
   Root := Insert_Root(Tree, 40);
   Root := Insert_Root(Tree, 50);
   Root := Insert_Root(Tree, 25);
   
   -- Print the tree
   Put_Line("In-order traversal of AVL tree:");
   Print_In_Order(Tree.Root);
end AVL_Example;
```

## Key Features of This Implementation:

### 1. **Core AVL Operations:**
- **Height Calculation**: `Get_Height` function to determine node height
- **Balance Factor**: `Get_Balance_Factor` to check balance condition
- **Rotations**: Both left and right rotations for rebalancing
- **Height Updates**: `Update_Height` to maintain correct height values

### 2. **Balancing Cases:**
- **Left-Left Case**: Right rotation
- **Right-Right Case**: Left rotation  
- **Left-Right Case**: Left rotation on left child, then right rotation
- **Right-Left Case**: Right rotation on right child, then left rotation

### 3. **Insertion Process:**
1. Standard BST insertion
2. Height update for all nodes on the path
3. Balance factor calculation
4. Appropriate rotation(s) based on balance factor

### 4. **Time Complexity:**
- **Insertion**: O(log n)
- **Deletion**: O(log n)  
- **Search**: O(log n)

This implementation demonstrates the complete AVL tree balancing algorithm with proper rotation handling and maintains the AVL property after every insertion operation.

