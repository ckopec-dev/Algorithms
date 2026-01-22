# Minimax Algorithm in Ada

Here's an implementation of the Minimax algorithm in Ada for a simple game tree:

```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Minimax_Example is
   
   -- Define game tree node structure
   type Node_Type is record
      Value     : Integer;     -- For leaf nodes
      Children  : access array (1..4) of Node_Type; -- For internal nodes
      Is_Leaf   : Boolean;
      Depth     : Integer;
   end record;
   
   type Node_Access is access Node_Type;
   
   -- Minimax function
   function Minimax(Node : Node_Access; Depth : Integer; Is_Maximizing : Boolean) return Integer is
   begin
      -- Base case: if we reach leaf node or max depth
      if Node.Is_Leaf or Depth = 0 then
         return Node.Value;
      end if;
      
      if Is_Maximizing then
         -- Maximizer's turn - maximize the value
         declare
            Best_Value : Integer := -1000;
         begin
            for I in 1..4 loop
               if Node.Children(I).Children /= null then
                  declare
                     Eval : Integer := Minimax(Node.Children(I).Children(1), Depth - 1, False);
                  begin
                     if Eval > Best_Value then
                        Best_Value := Eval;
                     end if;
                  end;
               end if;
            end loop;
            return Best_Value;
         end;
      else
         -- Minimizer's turn - minimize the value
         declare
            Best_Value : Integer := 1000;
         begin
            for I in 1..4 loop
               if Node.Children(I).Children /= null then
                  declare
                     Eval : Integer := Minimax(Node.Children(I).Children(1), Depth - 1, True);
                  begin
                     if Eval < Best_Value then
                        Best_Value := Eval;
                     end if;
                  end;
               end if;
            end loop;
            return Best_Value;
         end;
      end if;
   end Minimax;
   
   -- Create a simple game tree for demonstration
   procedure Create_Game_Tree is
      Root : Node_Access := new Node_Type;
   begin
      -- Root node (maximizing player)
      Root.Value := 0;
      Root.Is_Leaf := False;
      Root.Depth := 0;
      
      -- Create children nodes
      Root.Children := new array (1..4) of Node_Type;
      
      -- First level children (minimizing player)
      for I in 1..4 loop
         Root.Children(I).Value := 0;
         Root.Children(I).Is_Leaf := True;
         Root.Children(I).Depth := 1;
         
         -- Second level children (maximizing player)
         if I = 1 then
            Root.Children(I).Value := 3;
         elsif I = 2 then
            Root.Children(I).Value := 5;
         elsif I = 3 then
            Root.Children(I).Value := 2;
         else
            Root.Children(I).Value := 9;
         end if;
      end loop;
      
      -- Calculate minimax value
      declare
         Result : Integer := Minimax(Root, 2, True);
      begin
         Put_Line("Minimax value: " & Integer'Image(Result));
      end;
   end Create_Game_Tree;
   
begin
   Put_Line("Minimax Algorithm Example");
   Put_Line("========================");
   Create_Game_Tree;
end Minimax_Example;
```

## Key Features of this Implementation:

1. **Node Structure**: Defines game tree nodes with values, children, and leaf status
2. **Recursive Minimax**: Implements the core minimax algorithm with alternating maximizing and minimizing turns
3. **Depth Limiting**: Includes depth parameter to control search depth
4. **Turn Management**: Properly alternates between maximizing (True) and minimizing (False) players
5. **Base Case Handling**: Correctly handles leaf nodes and maximum depth

## How it Works:

- The algorithm traverses the game tree recursively
- At each maximizing node, it chooses the maximum value from children
- At each minimizing node, it chooses the minimum value from children
- The process continues until reaching leaf nodes or maximum depth
- Returns the optimal value for the current player

This example demonstrates the fundamental concepts of minimax algorithm in Ada, suitable for game-playing AI applications like tic-tac-toe or other turn-based games.

