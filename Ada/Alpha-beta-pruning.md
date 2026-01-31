# Alpha-Beta Pruning in Ada

Here's an implementation of the Alpha-Beta pruning algorithm in Ada for a minimax game tree:

```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Alpha_Beta_Example is
   
   -- Game tree node definition
   type Node_Type is record
      Value     : Integer;     -- Node value (for leaf nodes)
      Is_Leaf   : Boolean;     -- True if this is a leaf node
      Children  : array (1..4) of access Node_Type; -- Child nodes
      Num_Children : Natural;  -- Number of children
   end record;
   
   type Node_Access is access Node_Type;
   
   -- Alpha-Beta pruning function
   function Alpha_Beta(Node : Node_Access; Depth : Integer; 
                       Alpha : Integer; Beta : Integer; 
                       Is_Maximizing : Boolean) return Integer is
      Current_Value : Integer;
      New_Alpha     : Integer := Alpha;
      New_Beta      : Integer := Beta;
   begin
      -- Base case: if we've reached maximum depth or leaf node
      if Depth = 0 or else Node.all.Is_Leaf then
         return Node.all.Value;
      end if;
      
      if Is_Maximizing then
         -- Maximizing player: try to maximize the value
         Current_Value := Integer'First; -- Initialize to minimum possible value
         
         for I in 1..Node.all.Num_Children loop
            if Node.all.Children(I) /= null then
               Current_Value := Integer'Max(Current_Value, 
                                          Alpha_Beta(Node.all.Children(I),
                                                   Depth - 1,
                                                   New_Alpha,
                                                   New_Beta,
                                                   False));
               
               -- Update alpha
               New_Alpha := Integer'Max(New_Alpha, Current_Value);
               
               -- Alpha-beta pruning check
               if New_Alpha >= New_Beta then
                  -- Prune remaining branches
                  exit;
               end if;
            end if;
         end loop;
         
         return Current_Value;
         
      else
         -- Minimizing player: try to minimize the value
         Current_Value := Integer'Last; -- Initialize to maximum possible value
         
         for I in 1..Node.all.Num_Children loop
            if Node.all.Children(I) /= null then
               Current_Value := Integer'Min(Current_Value,
                                          Alpha_Beta(Node.all.Children(I),
                                                   Depth - 1,
                                                   New_Alpha,
                                                   New_Beta,
                                                   True));
               
               -- Update beta
               New_Beta := Integer'Min(New_Beta, Current_Value);
               
               -- Alpha-beta pruning check
               if New_Alpha >= New_Beta then
                  -- Prune remaining branches
                  exit;
               end if;
            end if;
         end loop;
         
         return Current_Value;
      end if;
   end Alpha_Beta;
   
   -- Helper function to create a new node
   function Create_Node(Value : Integer; Is_Leaf : Boolean) return Node_Access is
      New_Node : Node_Access := new Node_Type;
   begin
      New_Node.all.Value := Value;
      New_Node.all.Is_Leaf := Is_Leaf;
      New_Node.all.Num_Children := 0;
      for I in 1..4 loop
         New_Node.all.Children(I) := null;
      end loop;
      return New_Node;
   end Create_Node;
   
   -- Create a sample game tree
   procedure Create_Sample_Tree is
      Root_Node : Node_Access;
      Child1, Child2, Child3, Child4 : Node_Access;
      Grandchild1, Grandchild2, Grandchild3, Grandchild4 : Node_Access;
   begin
      -- Create root node
      Root_Node := Create_Node(0, False);
      
      -- Create first level children
      Child1 := Create_Node(0, False);
      Child2 := Create_Node(0, False);
      Child3 := Create_Node(0, False);
      Child4 := Create_Node(0, False);
      
      -- Set children of root
      Root_Node.all.Children(1) := Child1;
      Root_Node.all.Children(2) := Child2;
      Root_Node.all.Children(3) := Child3;
      Root_Node.all.Children(4) := Child4;
      Root_Node.all.Num_Children := 4;
      
      -- Create second level children (leaf nodes)
      Grandchild1 := Create_Node(3, True);
      Grandchild2 := Create_Node(5, True);
      Grandchild3 := Create_Node(2, True);
      Grandchild4 := Create_Node(9, True);
      
      -- Set children of Child1
      Child1.all.Children(1) := Grandchild1;
      Child1.all.Children(2) := Grandchild2;
      Child1.all.Num_Children := 2;
      
      -- Set children of Child2
      Child2.all.Children(1) := Grandchild3;
      Child2.all.Children(2) := Grandchild4;
      Child2.all.Num_Children := 2;
      
      -- Create more children for demonstration
      Grandchild3 := Create_Node(1, True);
      Grandchild4 := Create_Node(8, True);
      
      -- Set children of Child3
      Child3.all.Children(1) := Grandchild3;
      Child3.all.Children(2) := Grandchild4;
      Child3.all.Num_Children := 2;
      
      -- Set children of Child4
      Grandchild3 := Create_Node(4, True);
      Grandchild4 := Create_Node(7, True);
      Child4.all.Children(1) := Grandchild3;
      Child4.all.Children(2) := Grandchild4;
      Child4.all.Num_Children := 2;
      
      -- Perform alpha-beta pruning
      Put_Line("Starting Alpha-Beta Pruning...");
      Put_Line("Optimal value: " & Integer'Image(Alpha_Beta(Root_Node, 3, 
                                                         Integer'First, 
                                                         Integer'Last, 
                                                         True)));
   end Create_Sample_Tree;
   
begin
   Put_Line("Alpha-Beta Pruning Example in Ada");
   Put_Line("===============================");
   Create_Sample_Tree;
end Alpha_Beta_Example;
```

## Key Features of this Implementation:

1. **Node Structure**: Defines a game tree node with value, leaf status, and children
2. **Alpha-Beta Function**: Implements the core alpha-beta pruning algorithm
3. **Pruning Logic**: 
   - Alpha (maximum lower bound) for maximizing player
   - Beta (minimum upper bound) for minimizing player
   - Early termination when α ≥ β
4. **Minimax Integration**: Alternates between maximizing and minimizing players
5. **Depth Control**: Limits search depth to prevent infinite recursion

## How Alpha-Beta Pruning Works:

1. **Initialization**: Start with α = -∞ and β = +∞
2. **Maximizing Player**: Try to maximize the value, update α
3. **Minimizing Player**: Try to minimize the value, update β
4. **Pruning**: When α ≥ β, prune remaining branches
5. **Return Value**: The optimal value for the current player

This implementation demonstrates how alpha-beta pruning reduces the number of nodes evaluated compared to plain minimax, making it much more efficient for game tree searches.

