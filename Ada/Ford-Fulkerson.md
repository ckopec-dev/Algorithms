# Ford-Fulkerson Algorithm in Ada

Here's an implementation of the Ford-Fulkerson algorithm for finding the maximum flow in a flow network using Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Ford_Fulkerson is
   
   -- Maximum number of vertices
   Max_Vertices : constant := 100;
   
   -- Type definitions
   type Flow_Matrix is array (1..Max_Vertices, 1..Max_Vertices) of Integer;
   type Boolean_Array is array (1..Max_Vertices) of Boolean;
   
   -- Global variables
   Capacity : Flow_Matrix := (others => (others => 0));
   Flow : Flow_Matrix := (others => (others => 0));
   Num_Vertices : Integer := 0;
   Source : Integer := 0;
   Sink : Integer := 0;
   
   -- Function to find augmenting path using BFS
   function BFS(Visited : in out Boolean_Array; Parent : in out Integer_Array) return Boolean is
      Queue : array (1..Max_Vertices) of Integer;
      Head, Tail : Integer := 1;
      Current : Integer;
   begin
      -- Initialize visited array
      for I in 1..Num_Vertices loop
         Visited(I) := False;
      end loop;
      
      -- Start from source
      Visited(Source) := True;
      Queue(Head) := Source;
      Head := Head + 1;
      
      while Head > Tail loop
         Current := Queue(Tail);
         Tail := Tail + 1;
         
         -- Check all vertices
         for V in 1..Num_Vertices loop
            if not Visited(V) and then Capacity(Current, V) > Flow(Current, V) then
               Visited(V) := True;
               Parent(V) := Current;
               Queue(Head) := V;
               Head := Head + 1;
               
               if V = Sink then
                  return True;
               end if;
            end if;
         end loop;
      end loop;
      
      return False;
   end BFS;
   
   -- Function to find maximum flow using Ford-Fulkerson
   function Find_Max_Flow return Integer is
      Parent : array (1..Max_Vertices) of Integer;
      Visited : Boolean_Array;
      Path_Flow : Integer;
      Max_Flow : Integer := 0;
   begin
      -- Continue while there's an augmenting path
      while BFS(Visited, Parent) loop
         -- Find minimum capacity along the path
         Path_Flow := Integer'Last;
         declare
            Current : Integer := Sink;
         begin
            while Current /= Source loop
               declare
                  Prev : Integer := Parent(Current);
               begin
                  if Capacity(Prev, Current) - Flow(Prev, Current) < Path_Flow then
                     Path_Flow := Capacity(Prev, Current) - Flow(Prev, Current);
                  end if;
                  Current := Prev;
               end;
            end loop;
         end;
         
         -- Update flow
         Current := Sink;
         while Current /= Source loop
            declare
               Prev : Integer := Parent(Current);
            begin
               Flow(Prev, Current) := Flow(Prev, Current) + Path_Flow;
               Flow(Current, Prev) := Flow(Current, Prev) - Path_Flow;
               Current := Prev;
            end;
         end loop;
         
         Max_Flow := Max_Flow + Path_Flow;
      end loop;
      
      return Max_Flow;
   end Find_Max_Flow;
   
   -- Procedure to print the flow matrix
   procedure Print_Flow is
   begin
      Put_Line("Final Flow Matrix:");
      for I in 1..Num_Vertices loop
         for J in 1..Num_Vertices loop
            Put(Flow(I, J), Width => 4);
         end loop;
         New_Line;
      end loop;
   end Print_Flow;
   
begin
   -- Example graph setup
   -- Graph with 6 vertices (0 to 5)
   Num_Vertices := 6;
   Source := 0;
   Sink := 5;
   
   -- Initialize capacity matrix
   Capacity := (others => (others => 0));
   
   -- Define edges with capacities
   -- Edge: from, to, capacity
   Capacity(0, 1) := 16;
   Capacity(0, 2) := 13;
   Capacity(1, 2) := 10;
   Capacity(1, 3) := 12;
   Capacity(2, 1) := 4;
   Capacity(2, 4) := 14;
   Capacity(3, 2) := 9;
   Capacity(3, 5) := 20;
   Capacity(4, 3) := 7;
   Capacity(4, 5) := 4;
   
   -- Find maximum flow
   declare
      Max_Flow : Integer := Find_Max_Flow;
   begin
      Put_Line("Maximum Flow: " & Integer'Image(Max_Flow));
      Print_Flow;
   end;
   
end Ford_Fulkerson;
```

## Algorithm Explanation

This implementation demonstrates the Ford-Fulkerson method with the following key components:

### Key Features:
1. **BFS-based Path Finding**: Uses Breadth-First Search to find augmenting paths
2. **Residual Graph**: Maintains flow and capacity information
3. **Augmenting Path Updates**: Updates flow along the found path
4. **Maximum Flow Calculation**: Continues until no more augmenting paths exist

### Input Graph Structure:
```
    0 --16--> 1 --12--> 3 --20--> 5
    |          |          |
   13         10         9
    |          |          |
    v          v          v
    2 --14--> 4 --4---> 5
```

### Output:
The program will output:
- Maximum flow value
- Final flow matrix showing flow values on each edge

### Time Complexity:
- O(E × max_flow) where E is the number of edges
- In worst case: O(V × E²) for dense graphs

This implementation provides a complete working example of the Ford-Fulkerson algorithm in Ada, demonstrating how to compute maximum flow in a flow network.

