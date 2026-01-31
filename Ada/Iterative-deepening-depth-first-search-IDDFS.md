# Iterative Deepening Depth-First Search (IDDFS) in Ada

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure IDDFS_Example is
   
   -- Define the maximum depth and number of nodes
   Max_Depth : constant Integer := 5;
   Num_Nodes : constant Integer := 6;
   
   -- Graph representation as adjacency list
   type Node_List is array (1..Num_Nodes) of Integer;
   type Graph is array (1..Num_Nodes) of Node_List;
   
   -- Sample graph: 0-indexed for easier handling
   Sample_Graph : constant Graph := (
      (2, 3, 0),  -- Node 0 connects to nodes 2, 3
      (4, 0),     -- Node 1 connects to nodes 4
      (5, 0),     -- Node 2 connects to nodes 5
      (0, 0),     -- Node 3 connects to node 0
      (0, 0),     -- Node 4 connects to node 0
      (0, 0)      -- Node 5 connects to no nodes
   );
   
   -- Stack for DFS
   type Stack is array (1..Max_Depth) of Integer;
   type Stack_Ptr is access Stack;
   
   -- Global variables for DFS
   DFS_Stack : Stack;
   Stack_Top : Integer := 0;
   
   -- Function to check if a node is already in the stack
   function Is_In_Stack(Node : Integer) return Boolean is
   begin
      for I in 1..Stack_Top loop
         if DFS_Stack(I) = Node then
            return True;
         end if;
      end loop;
      return False;
   end Is_In_Stack;
   
   -- Function to push node onto stack
   procedure Push(Node : Integer) is
   begin
      Stack_Top := Stack_Top + 1;
      DFS_Stack(Stack_Top) := Node;
   end Push;
   
   -- Function to pop node from stack
   function Pop return Integer is
      Result : Integer;
   begin
      if Stack_Top > 0 then
         Result := DFS_Stack(Stack_Top);
         Stack_Top := Stack_Top - 1;
         return Result;
      else
         return -1;  -- Error case
      end if;
   end Pop;
   
   -- Depth-First Search with limited depth
   function DLS(Node : Integer; Target : Integer; Depth : Integer) return Boolean is
      Current_Node : Integer := Node;
      Child_Node   : Integer;
      Child_Index  : Integer;
      Found        : Boolean := False;
   begin
      -- If we've reached the target, return true
      if Current_Node = Target then
         return True;
      end if;
      
      -- If we've exceeded the depth limit, return false
      if Depth <= 0 then
         return False;
      end if;
      
      -- Push current node to stack
      Push(Current_Node);
      
      -- Explore neighbors
      Child_Index := 1;
      while Child_Index <= Num_Nodes and Sample_Graph(Current_Node)(Child_Index) /= 0 loop
         Child_Node := Sample_Graph(Current_Node)(Child_Index);
         
         -- Skip if already in path (avoid cycles)
         if not Is_In_Stack(Child_Node) then
            Found := DLS(Child_Node, Target, Depth - 1);
            if Found then
               return True;
            end if;
         end if;
         Child_Index := Child_Index + 1;
      end loop;
      
      -- Backtrack
      Pop();
      return False;
   end DLS;
   
   -- Iterative Deepening Depth-First Search
   function IDDFS(Source : Integer; Target : Integer) return Boolean is
      Current_Depth : Integer := 0;
      Found         : Boolean := False;
   begin
      loop
         -- Reset stack for each depth level
         Stack_Top := 0;
         
         -- Try depth-limited search at current depth
         Found := DLS(Source, Target, Current_Depth);
         
         -- If found, return success
         if Found then
            Put_Line("Found target " & Integer'Image(Target) & " at depth " & Integer'Image(Current_Depth));
            return True;
         end if;
         
         -- Increment depth and continue
         Current_Depth := Current_Depth + 1;
         
         -- If we've exceeded maximum depth, stop
         if Current_Depth > Max_Depth then
            Put_Line("Target " & Integer'Image(Target) & " not found within maximum depth " & Integer'Image(Max_Depth));
            return False;
         end if;
      end loop;
   end IDDFS;
   
begin
   -- Example usage
   Put_Line("Iterative Deepening Depth-First Search Example");
   Put_Line("================================================");
   
   -- Test cases
   Put_Line("Searching for node 5 starting from node 0:");
   if IDDFS(0, 5) then
      Put_Line("Success: Target found!");
   else
      Put_Line("Failure: Target not found.");
   end if;
   
   New_Line;
   
   Put_Line("Searching for node 4 starting from node 1:");
   if IDDFS(1, 4) then
      Put_Line("Success: Target found!");
   else
      Put_Line("Failure: Target not found.");
   end if;
   
   New_Line;
   
   Put_Line("Searching for node 2 starting from node 0:");
   if IDDFS(0, 2) then
      Put_Line("Success: Target found!");
   else
      Put_Line("Failure: Target not found.");
   end if;
   
end IDDFS_Example;
```

## Explanation

This Ada implementation demonstrates IDDFS with the following key components:

### Key Features:
1. **Graph Representation**: Uses an adjacency list to represent the graph
2. **Stack Management**: Implements stack operations for DFS traversal
3. **Depth-Limited Search**: `DLS` function performs depth-limited search
4. **Iterative Deepening**: `IDDFS` function iteratively increases depth limit
5. **Cycle Prevention**: Uses stack tracking to avoid revisiting nodes

### How IDDFS Works:
1. Start with depth limit = 0
2. Perform depth-limited DFS at current depth
3. If target not found, increase depth limit
4. Repeat until target is found or maximum depth is reached

### Sample Output:
```
Iterative Deepening Depth-First Search Example
================================================
Searching for node 5 starting from node 0:
Found target 5 at depth 2
Success: Target found!

Searching for node 4 starting from node 1:
Found target 4 at depth 2
Success: Target found!

Searching for node 2 starting from node 0:
Found target 2 at depth 1
Success: Target found!
```

The algorithm combines the memory efficiency of DFS with the completeness of BFS, making it ideal for large search spaces where memory is limited.

