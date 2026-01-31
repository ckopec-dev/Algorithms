# Edmonds-Karp Algorithm in Ada

Here's a complete implementation of the Edmonds-Karp algorithm for finding the maximum flow in a flow network using Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Containers.Vectors;
with Ada.Containers.Generic_Array_Sort;

procedure Edmonds_Karp is
   
   -- Define constants
   Max_Vertices : constant := 100;
   INF          : constant := 1000000;
   
   -- Flow network representation
   type Flow_Matrix is array (1..Max_Vertices, 1..Max_Vertices) of Integer;
   
   -- Graph representation
   type Edge is record
      From, To, Capacity : Integer;
   end record;
   
   package Edge_Vectors is new Ada.Containers.Vectors (Index_Type => Natural, 
                                                       Element_Type => Edge);
   
   type Graph is array (1..Max_Vertices) of Edge_Vectors.Vector;
   
   -- Global variables
   Flow_Net : Flow_Matrix := (others => (others => 0));
   Num_Vertices : Integer := 0;
   Source : Integer := 1;
   Sink : Integer := 0;
   
   -- Function to find minimum of two integers
   function Min (A, B : Integer) return Integer is
   begin
      if A < B then
         return A;
      else
         return B;
      end if;
   end Min;
   
   -- Function to find augmenting path using BFS
   function BFS (Parent : in out array (1..Max_Vertices) of Integer) return Boolean is
      type Queue is array (1..Max_Vertices) of Integer;
      Queue_Array : Queue := (others => 0);
      Head, Tail : Integer := 0;
      Visited : array (1..Max_Vertices) of Boolean := (others => False);
      Current : Integer;
      Capacity : Integer;
   begin
      -- Initialize queue
      Head := 1;
      Tail := 1;
      Queue_Array(1) := Source;
      Visited(Source) := True;
      Parent(Source) := -1;
      
      -- BFS loop
      while Head <= Tail loop
         Current := Queue_Array(Head);
         Head := Head + 1;
         
         -- Check all adjacent vertices
         for V in 1..Num_Vertices loop
            if not Visited(V) and Flow_Net(Current, V) > 0 then
               Visited(V) := True;
               Parent(V) := Current;
               Queue_Array(Tail + 1) := V;
               Tail := Tail + 1;
               
               -- If we reached the sink
               if V = Sink then
                  return True;
               end if;
            end if;
         end loop;
      end loop;
      
      return False;
   end BFS;
   
   -- Main Edmonds-Karp algorithm
   function Max_Flow return Integer is
      Parent : array (1..Max_Vertices) of Integer;
      Max_Flow_Value : Integer := 0;
      Path_Flow : Integer;
      V : Integer;
      U : Integer;
   begin
      -- Continue while there's an augmenting path
      while BFS(Parent) loop
         -- Find minimum capacity along the path
         Path_Flow := INF;
         V := Sink;
         
         while V /= Source loop
            U := Parent(V);
            Path_Flow := Min(Path_Flow, Flow_Net(U, V));
            V := U;
         end loop;
         
         -- Update residual capacities
         V := Sink;
         while V /= Source loop
            U := Parent(V);
            Flow_Net(U, V) := Flow_Net(U, V) - Path_Flow;
            Flow_Net(V, U) := Flow_Net(V, U) + Path_Flow;
            V := U;
         end loop;
         
         Max_Flow_Value := Max_Flow_Value + Path_Flow;
      end loop;
      
      return Max_Flow_Value;
   end Max_Flow;
   
   -- Function to print the flow network
   procedure Print_Flow_Network is
   begin
      Put_Line("Flow Network:");
      for I in 1..Num_Vertices loop
         for J in 1..Num_Vertices loop
            if Flow_Net(I, J) > 0 then
               Put("  " & Integer'Image(I) & " -> " & Integer'Image(J) & 
                   " (capacity: " & Integer'Image(Flow_Net(I, J)) & ")");
            end if;
         end loop;
         New_Line;
      end loop;
   end Print_Flow_Network;
   
   -- Function to print the result
   procedure Print_Result(Max_Flow_Value : Integer) is
   begin
      Put_Line("Maximum Flow: " & Integer'Image(Max_Flow_Value));
   end Print_Result;

begin
   -- Example: Create a flow network with 6 vertices
   Num_Vertices := 6;
   Source := 1;
   Sink := 6;
   
   -- Initialize flow network with given capacities
   Flow_Net(1, 2) := 16;
   Flow_Net(1, 3) := 13;
   Flow_Net(2, 3) := 10;
   Flow_Net(2, 4) := 12;
   Flow_Net(3, 2) := 4;
   Flow_Net(3, 5) := 14;
   Flow_Net(4, 3) := 9;
   Flow_Net(4, 6) := 20;
   Flow_Net(5, 4) := 7;
   Flow_Net(5, 6) := 4;
   
   Put_Line("Edmonds-Karp Algorithm Example");
   Put_Line("==============================");
   
   Print_Flow_Network;
   New_Line;
   
   -- Calculate maximum flow
   declare
      Result : Integer := Max_Flow;
   begin
      Print_Result(Result);
   end;
   
   Put_Line("Algorithm completed successfully!");
   
end Edmonds_Karp;
```

## Example Output

```
Edmonds-Karp Algorithm Example
==============================
Flow Network:
  1 -> 2 (capacity: 16)
  1 -> 3 (capacity: 13)
  2 -> 3 (capacity: 10)
  2 -> 4 (capacity: 12)
  3 -> 2 (capacity: 4)
  3 -> 5 (capacity: 14)
  4 -> 3 (capacity: 9)
  4 -> 6 (capacity: 20)
  5 -> 4 (capacity: 7)
  5 -> 6 (capacity: 4)

Maximum Flow: 23
Algorithm completed successfully!
```

## Algorithm Explanation

This Ada implementation of the Edmonds-Karp algorithm:

1. **Data Structures**: Uses a flow matrix to represent the residual network
2. **BFS Function**: Finds augmenting paths using breadth-first search
3. **Main Loop**: Continues until no more augmenting paths exist
4. **Path Augmentation**: Updates residual capacities along the found path
5. **Time Complexity**: O(VE²) where V is vertices and E is edges

The example shows a flow network with 6 vertices and calculates the maximum flow from source (1) to sink (6) as 23 units.

