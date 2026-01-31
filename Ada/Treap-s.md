# Treap Algorithm in Ada

Here's an implementation of a Treap (Tree + Heap) data structure in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Treap_Example is

   -- Treap Node Record
   type Treap_Node is record
      Key     : Integer;
      Priority : Integer;
      Left    : access Treap_Node;
      Right   : access Treap_Node;
   end record;
   
   type Treap_Node_Access is access Treap_Node;
   
   -- Treap Root
   type Treap is record
      Root : Treap_Node_Access;
   end record;
   
   -- Function to create a new node
   function Create_Node(Key_Val, Priority_Val : Integer) return Treap_Node_Access is
      Node : Treap_Node_Access := new Treap_Node'(Key_Val, Priority_Val, null, null);
   begin
      return Node;
   end Create_Node;
   
   -- Right rotation
   procedure Right_Rotate(Node : in out Treap_Node_Access) is
      Temp : Treap_Node_Access := Node.all.Left;
   begin
      Node.all.Left := Temp.all.Right;
      Temp.all.Right := Node;
      Node := Temp;
   end Right_Rotate;
   
   -- Left rotation
   procedure Left_Rotate(Node : in out Treap_Node_Access) is
      Temp : Treap_Node_Access := Node.all.Right;
   begin
      Node.all.Right := Temp.all.Left;
      Temp.all.Left := Node;
      Node := Temp;
   end Left_Rotate;
   
   -- Insert operation
   procedure Insert(T : in out Treap; Key_Val, Priority_Val : Integer) is
      procedure Insert_Helper(Current : in out Treap_Node_Access) is
      begin
         if Current = null then
            Current := Create_Node(Key_Val, Priority_Val);
         elsif Key_Val < Current.all.Key then
            Insert_Helper(Current.all.Left);
            -- Heap property check
            if Current.all.Left /= null and then
               Current.all.Left.all.Priority < Current.all.Priority then
               Right_Rotate(Current);
            end if;
         elsif Key_Val > Current.all.Key then
            Insert_Helper(Current.all.Right);
            -- Heap property check
            if Current.all.Right /= null and then
               Current.all.Right.all.Priority < Current.all.Priority then
               Left_Rotate(Current);
            end if;
         end if;
      end Insert_Helper;
   begin
      Insert_Helper(T.Root);
   end Insert;
   
   -- Search operation
   function Search(T : Treap; Key_Val : Integer) return Boolean is
      Current : Treap_Node_Access := T.Root;
   begin
      while Current /= null loop
         if Key_Val < Current.all.Key then
            Current := Current.all.Left;
         elsif Key_Val > Current.all.Key then
            Current := Current.all.Right;
         else
            return True;
         end if;
      end loop;
      return False;
   end Search;
   
   -- In-order traversal
   procedure In_Order_Traversal(Node : Treap_Node_Access) is
   begin
      if Node /= null then
         In_Order_Traversal(Node.all.Left);
         Put(Integer'Image(Node.all.Key) & " (P:" & Integer'Image(Node.all.Priority) & ") ");
         In_Order_Traversal(Node.all.Right);
      end if;
   end In_Order_Traversal;
   
   -- Print tree
   procedure Print_Tree(T : Treap) is
   begin
      Put("In-order traversal: ");
      In_Order_Traversal(T.Root);
      New_Line;
   end Print_Tree;
   
   -- Main program
   T : Treap := (null);
   
begin
   Put_Line("Treap Example:");
   New_Line;
   
   -- Insert elements with priorities
   Insert(T, 10, 5);
   Insert(T, 5, 15);
   Insert(T, 15, 10);
   Insert(T, 3, 20);
   Insert(T, 7, 8);
   Insert(T, 12, 3);
   Insert(T, 18, 12);
   
   Put_Line("Tree after insertions:");
   Print_Tree(T);
   
   -- Search operations
   Put_Line("Search for 7: " & Boolean'Image(Search(T, 7)));
   Put_Line("Search for 20: " & Boolean'Image(Search(T, 20)));
   
end Treap_Example;
```

## Key Features of this Treap Implementation:

1. **Node Structure**: Each node contains key, priority, and left/right child pointers
2. **Insertion**: Maintains both BST property (keys) and heap property (priorities)
3. **Rotations**: Right and left rotations to maintain heap property
4. **Search**: Standard binary search tree search
5. **Traversal**: In-order traversal to display tree structure

## How it works:

1. **BST Property**: Keys are arranged in binary search tree order
2. **Heap Property**: Priorities form a min-heap (smaller priorities closer to root)
3. **Insertion**: Insert like BST, then rotate to maintain heap property
4. **Rotation**: Rotations are performed to restore heap property when violated

This implementation demonstrates the core concepts of Treaps as a data structure that combines the benefits of binary search trees and heaps.

