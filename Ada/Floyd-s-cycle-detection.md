```ada
-- Floyd's Cycle Detection Algorithm in Ada
-- Also known as the "Tortoise and Hare" algorithm

with Ada.Text_IO; use Ada.Text_IO;

procedure Floyd_Cycle_Detection is
   
   -- Define a node structure for a linked list
   type Node;
   type Node_Ptr is access Node;
   
   type Node is record
      Value : Integer;
      Next  : Node_Ptr;
   end record;
   
   -- Function to detect cycle in a linked list
   function Has_Cycle(Head : Node_Ptr) return Boolean is
      Tortoise : Node_Ptr := Head;
      Hare     : Node_Ptr := Head;
   begin
      -- Handle empty list or single node
      if Head = null or else Head.Next = null then
         return False;
      end if;
      
      -- Move tortoise one step, hare two steps
      loop
         -- Move tortoise one step
         Tortoise := Tortoise.Next;
         
         -- Move hare two steps
         Hare := Hare.Next;
         if Hare /= null then
            Hare := Hare.Next;
         else
            return False;  -- No cycle
         end if;
         
         -- If pointers meet, there's a cycle
         if Tortoise = Hare then
            return True;
         end if;
         
         -- Prevent infinite loop (optional safety check)
         exit when Hare = null;
      end loop;
      
      return False;
   end Has_Cycle;
   
   -- Function to find the start of the cycle (optional enhancement)
   function Find_Cycle_Start(Head : Node_Ptr) return Node_Ptr is
      Tortoise : Node_Ptr := Head;
      Hare     : Node_Ptr := Head;
      Meeting_Point : Node_Ptr;
   begin
      -- Phase 1: Detect if cycle exists
      if Head = null or else Head.Next = null then
         return null;
      end if;
      
      loop
         Tortoise := Tortoise.Next;
         Hare := Hare.Next;
         if Hare /= null then
            Hare := Hare.Next;
         else
            return null;
         end if;
         
         if Tortoise = Hare then
            Meeting_Point := Tortoise;
            exit;
         end if;
      end loop;
      
      -- Phase 2: Find the start of the cycle
      Tortoise := Head;
      while Tortoise /= Meeting_Point loop
         Tortoise := Tortoise.Next;
         Meeting_Point := Meeting_Point.Next;
      end loop;
      
      return Tortoise;  -- Start of cycle
   end Find_Cycle_Start;
   
   -- Helper function to create a new node
   function Create_Node(Value : Integer) return Node_Ptr is
      New_Node : Node_Ptr := new Node'(Value => Value, Next => null);
   begin
      return New_Node;
   end Create_Node;
   
   -- Test the algorithm
   procedure Test_Cycle_Detection is
      -- Create nodes for a list without cycle: 1 -> 2 -> 3 -> 4 -> 5
      Node1 : Node_Ptr := Create_Node(1);
      Node2 : Node_Ptr := Create_Node(2);
      Node3 : Node_Ptr := Create_Node(3);
      Node4 : Node_Ptr := Create_Node(4);
      Node5 : Node_Ptr := Create_Node(5);
      
      -- Create nodes for a list with cycle: 1 -> 2 -> 3 -> 4 -> 2 (cycle)
      NodeA : Node_Ptr := Create_Node(1);
      NodeB : Node_Ptr := Create_Node(2);
      NodeC : Node_Ptr := Create_Node(3);
      NodeD : Node_Ptr := Create_Node(4);
      
   begin
      -- Build list without cycle: 1 -> 2 -> 3 -> 4 -> 5
      Node1.Next := Node2;
      Node2.Next := Node3;
      Node3.Next := Node4;
      Node4.Next := Node5;
      Node5.Next := null;
      
      -- Build list with cycle: 1 -> 2 -> 3 -> 4 -> 2 (back to node 2)
      NodeA.Next := NodeB;
      NodeB.Next := NodeC;
      NodeC.Next := NodeD;
      NodeD.Next := NodeB;  -- Creates cycle: D points back to B
      
      -- Test without cycle
      Put_Line("Testing list without cycle:");
      if Has_Cycle(Node1) then
         Put_Line("Cycle detected");
      else
         Put_Line("No cycle detected");
      end if;
      
      -- Test with cycle
      Put_Line("Testing list with cycle:");
      if Has_Cycle(NodeA) then
         Put_Line("Cycle detected");
      else
         Put_Line("No cycle detected");
      end if;
      
      -- Test cycle start detection
      Put_Line("Finding cycle start:");
      declare
         Start_Point : Node_Ptr := Find_Cycle_Start(NodeA);
      begin
         if Start_Point /= null then
            Put_Line("Cycle starts at node with value: " & Integer'Image(Start_Point.Value));
         else
            Put_Line("No cycle found");
         end if;
      end;
   end Test_Cycle_Detection;
   
begin
   Test_Cycle_Detection;
end Floyd_Cycle_Detection;
```

This Ada implementation demonstrates Floyd's cycle detection algorithm with:

1. **Node Structure**: A basic linked list node with value and next pointer
2. **Has_Cycle Function**: Implements the core tortoise and hare algorithm
3. **Find_Cycle_Start Function**: Optional enhancement to find where the cycle begins
4. **Test Cases**: Shows both cycle-free and cycle-containing linked lists
5. **Proper Memory Management**: Uses Ada's access types and explicit memory allocation

The algorithm works by using two pointers moving at different speeds - when they meet, a cycle exists. The time complexity is O(n) and space complexity is O(1).

