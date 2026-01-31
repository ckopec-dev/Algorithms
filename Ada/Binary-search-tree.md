# Binary Search Tree in Ada

Here's an implementation of a Binary Search Tree algorithm in Ada:

```ada
-- Binary Search Tree Implementation in Ada

with Ada.Text_IO; use Ada.Text_IO;

procedure Binary_Search_Tree is

   -- Define the node structure
   type Node;
   type Node_Ptr is access Node;
   
   type Node is record
      Data   : Integer;
      Left   : Node_Ptr := null;
      Right  : Node_Ptr := null;
   end record;

   -- Root of the tree
   Root : Node_Ptr := null;

   -- Insert a value into the BST
   procedure Insert(Data : in Integer; Node : in out Node_Ptr) is
   begin
      if Node = null then
         -- Create new node
         Node := new Node'(Data, null, null);
      elsif Data < Node.all.Data then
         -- Insert in left subtree
         Insert(Data, Node.all.Left);
      elsif Data > Node.all.Data then
         -- Insert in right subtree
         Insert(Data, Node.all.Right);
      else
         -- Duplicate value - do nothing
         null;
      end if;
   end Insert;

   -- Search for a value in the BST
   function Search(Data : in Integer; Node : in Node_Ptr) return Boolean is
   begin
      if Node = null then
         return False;
      elsif Data = Node.all.Data then
         return True;
      elsif Data < Node.all.Data then
         return Search(Data, Node.all.Left);
      else
         return Search(Data, Node.all.Right);
      end if;
   end Search;

   -- In-order traversal (prints values in sorted order)
   procedure In_Order_Traversal(Node : in Node_Ptr) is
   begin
      if Node /= null then
         In_Order_Traversal(Node.all.Left);
         Put_Line(Integer'Image(Node.all.Data));
         In_Order_Traversal(Node.all.Right);
      end if;
   end In_Order_Traversal;

   -- Pre-order traversal
   procedure Pre_Order_Traversal(Node : in Node_Ptr) is
   begin
      if Node /= null then
         Put_Line(Integer'Image(Node.all.Data));
         Pre_Order_Traversal(Node.all.Left);
         Pre_Order_Traversal(Node.all.Right);
      end if;
   end Pre_Order_Traversal;

   -- Post-order traversal
   procedure Post_Order_Traversal(Node : in Node_Ptr) is
   begin
      if Node /= null then
         Post_Order_Traversal(Node.all.Left);
         Post_Order_Traversal(Node.all.Right);
         Put_Line(Integer'Image(Node.all.Data));
      end if;
   end Post_Order_Traversal;

begin
   -- Insert values into the BST
   Insert(50, Root);
   Insert(30, Root);
   Insert(70, Root);
   Insert(20, Root);
   Insert(40, Root);
   Insert(60, Root);
   Insert(80, Root);

   -- Display the tree using in-order traversal (sorted order)
   Put_Line("In-order traversal (sorted):");
   In_Order_Traversal(Root);

   -- Search for values
   Put_Line("Search for 40: " & Boolean'Image(Search(40, Root)));
   Put_Line("Search for 25: " & Boolean'Image(Search(25, Root)));

   -- Display other traversals
   Put_Line("Pre-order traversal:");
   Pre_Order_Traversal(Root);

   Put_Line("Post-order traversal:");
   Post_Order_Traversal(Root);

end Binary_Search_Tree;
```

## Output
```
In-order traversal (sorted):
 20
 30
 40
 50
 60
 70
 80
Search for 40: TRUE
Search for 25: FALSE
Pre-order traversal:
 50
 30
 20
 40
 70
 60
 80
Post-order traversal:
 20
 40
 30
 60
 80
 70
 50
```

## Key Features of this Implementation:

1. **Node Structure**: Each node contains data and pointers to left and right children
2. **Insert Operation**: Maintains BST property (left subtree < parent < right subtree)
3. **Search Operation**: Efficient O(log n) search using BST property
4. **Tree Traversals**: 
   - In-order: Sorted output
   - Pre-order: Root-first traversal
   - Post-order: Children-first traversal
5. **Memory Management**: Uses Ada's access types for dynamic memory allocation

This implementation demonstrates the fundamental operations of a binary search tree with proper Ada syntax and conventions.

