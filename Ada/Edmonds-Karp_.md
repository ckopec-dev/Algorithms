# Edmonds-Karp Algorithm in Ada

Here's a complete implementation of the Edmonds-Karp algorithm for finding maximum flow in a flow network using Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Edmonds_Karp is
   
   -- Constants
   Max_Vertices : constant := 100;
   INF          : constant := 1000000;
   
   -- Flow network representation
   type Flow_Matrix is array (1..Max_Vertices, 1..Max_Vertices) of Integer;
   type Visited_Array is array (1..Max_Vertices) of Boolean;
   
   -- Global variables
   Graph     : Flow_Matrix := (others => (others => 0));
   Capacity  : Flow_Matrix := (others => (others => 0));
   Num_Vertices : Integer := 0;
   Source     : Integer := 0;
   Sink       : Integer := 0;
   
   -- Function to find minimum of two integers
   function Min(A, B : Integer) return Integer is
   begin
      if A < B then
         return A;
      else
         return B;
      end if;
   end Min;
   
   -- BFS to find augmenting path
   function BFS(Visited : in out Visited_Array; Parent : in out array (1..Max_Vertices) of Integer) return Boolean is
      type Queue_Type is array (1..Max_Vertices) of Integer;
      Queue : Queue_Type := (others => 0);
      Head, Tail : Integer := 0;
      
      procedure Enqueue(Element : Integer) is
      begin
         Tail := Tail + 1;
         Queue(Tail) := Element;
      end Enqueue;
      
      function Dequeue return Integer is
         Result : Integer := Queue(Head);
      begin
         Head := Head + 1;
         return Result;
      end Dequeue;
      
      procedure Clear_Queue is
      begin
         Head := 0;
         Tail := 0;
      end Clear_Queue;
      
   begin
      -- Initialize visited array
      for I in 1..Num_Vertices loop
         Visited(I) := False;
         Parent(I) := 0;
      end loop;
      
      -- Start BFS from source
      Visited(Source) := True;
      Parent(Source) := -1;
      Enqueue(Source);
      
      while Head < Tail loop
         declare
            U : constant Integer := Dequeue;
         begin
            for V in 1..Num_Vertices loop
               if not Visited(V) and Graph(U, V) > 0 then
                  Visited(V) := True;
                  Parent(V) := U;
                  if V = Sink then
                     return True;
                  end if;
                  Enqueue(V);
               end if;
            end loop;
         end;
      end loop;
      
      return False;
   end BFS;
   
   -- Main Edmonds-Karp algorithm
   function Max_Flow return Integer is
      Parent : array (1..Max_Vertices) of Integer := (others => 0);
      Visited : Visited_Array := (others => False);
      Max_Flow_Value : Integer := 0;
      Path_Flow : Integer;
      
   begin
      loop
         -- Find augmenting path using BFS
         if not BFS(Visited, Parent) then
            exit;
         end if;
         
         -- Find minimum capacity along the path
         Path_Flow := INF;
         declare
            Current : Integer := Sink;
         begin
            while Current /= Source loop
               Path_Flow := Min(Path_Flow, Graph(Parent(Current), Current));
               Current := Parent(Current);
            end loop;
         end;
         
         -- Update residual capacities
         declare
            Current : Integer := Sink;
         begin
            while Current /= Source loop
               Graph(Parent(Current), Current) := Graph(Parent(Current), Current) - Path_Flow;
               Graph(Current, Parent(Current)) := Graph(Current, Parent(Current)) + Path_Flow;
               Current := Parent(Current);
            end loop;
         end;
         
         Max_Flow_Value := Max_Flow_Value + Path_Flow;
      end loop;
      
      return Max_Flow_Value;
   end Max_Flow;

begin
   -- Example graph setup:
   -- 0 -> 1 (16)
   -- 0 -> 2 (13)
   -- 1 -> 2 (10)
   -- 1 -> 3 (12)
   -- 2 -> 1 (4)
   -- 2 -> 4 (14)
   -- 3 -> 2 (9)
   -- 3 -> 5 (20)
   -- 4 -> 3 (7)
   -- 4 -> 5 (4)
   
   Num_Vertices := 6;
   Source := 1;
   Sink := 6;
   
   -- Initialize capacity matrix
   Capacity(1, 2) := 16; Capacity(1, 3) := 13;
   Capacity(2, 3) := 10; Capacity(2, 4) := 12;
   Capacity(3, 2) := 4;  Capacity(3, 5) := 14;
   Capacity(4, 3) := 9;  Capacity(4, 6) := 20;
   Capacity(5, 4) := 7;  Capacity(5, 6) := 4;
   
   -- Build residual graph
   for I in 1..Num_Vertices loop
      for J in 1..Num_Vertices loop
         Graph(I, J) := Capacity(I, J);
      end loop;
   end loop;
   
   Put_Line("Maximum flow using Edmonds-Karp algorithm:");
   Put(Integer'Image(Max_Flow));
   New_Line;
   
end Edmonds_Karp;
```

## How it works:

1. **Graph Representation**: Uses adjacency matrices to represent the flow network
2. **BFS Search**: Finds augmenting paths from source to sink using breadth-first search
3. **Path Augmentation**: For each path found, determines the minimum capacity and updates residual graph
4. **Loop Until No Path**: Continues until no more augmenting paths exist

## Key Features:

- **BFS-based**: Uses breadth-first search to find shortest augmenting paths
- **Residual Graph**: Maintains both forward and backward edges in residual graph
- **Path Flow Calculation**: Computes minimum capacity along each path
- **Efficient Updates**: Updates residual capacities after each augmentation

## Time Complexity:
- O(VE²) where V is number of vertices and E is number of edges
- This implementation uses BFS to find augmenting paths, making it efficient for most practical cases

The algorithm finds the maximum flow from source to sink in a flow network with given edge capacities.

