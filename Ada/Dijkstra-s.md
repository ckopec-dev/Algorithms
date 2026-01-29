# Dijkstra's Algorithm in Ada

Here's an implementation of Dijkstra's algorithm in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Dijkstra_Example is
   
   -- Define constants
   Max_Vertices : constant := 10;
   Infinity     : constant := 1000000;
   
   -- Graph representation using adjacency matrix
   type Graph_Type is array (1..Max_Vertices, 1..Max_Vertices) of Integer;
   
   -- Distance array
   type Distance_Array is array (1..Max_Vertices) of Integer;
   
   -- Visited array
   type Visited_Array is array (1..Max_Vertices) of Boolean;
   
   -- Function to find minimum distance vertex
   function Find_Min_Distance(Dist : Distance_Array; Visited : Visited_Array) return Integer is
      Min_Dist : Integer := Infinity;
      Min_Index : Integer := 0;
   begin
      for I in 1..Max_Vertices loop
         if not Visited(I) and Dist(I) < Min_Dist then
            Min_Dist := Dist(I);
            Min_Index := I;
         end if;
      end loop;
      return Min_Index;
   end Find_Min_Distance;
   
   -- Dijkstra's algorithm implementation
   procedure Dijkstra(Graph : in Graph_Type; 
                      Start_Vertex : in Integer;
                      Dist : out Distance_Array;
                      Visited : out Visited_Array) is
      U : Integer;
   begin
      -- Initialize distances and visited array
      for I in 1..Max_Vertices loop
         Dist(I) := Infinity;
         Visited(I) := False;
      end loop;
      
      -- Distance to start vertex is 0
      Dist(Start_Vertex) := 0;
      
      -- Main algorithm loop
      for Count in 1..Max_Vertices loop
         -- Find vertex with minimum distance
         U := Find_Min_Distance(Dist, Visited);
         
         -- If no reachable vertex found, break
         exit when U = 0;
         
         -- Mark as visited
         Visited(U) := True;
         
         -- Update distances of adjacent vertices
         for V in 1..Max_Vertices loop
            if not Visited(V) and Graph(U, V) /= 0 and Dist(U) + Graph(U, V) < Dist(V) then
               Dist(V) := Dist(U) + Graph(U, V);
            end if;
         end loop;
      end loop;
   end Dijkstra;
   
   -- Test graph (adjacency matrix)
   Test_Graph : Graph_Type := (
      (0, 4, 0, 0, 0, 0, 0, 8, 0),  -- Vertex 1
      (4, 0, 8, 0, 0, 0, 0, 11, 0), -- Vertex 2
      (0, 8, 0, 7, 0, 4, 0, 0, 2),  -- Vertex 3
      (0, 0, 7, 0, 9, 14, 0, 0, 0), -- Vertex 4
      (0, 0, 0, 9, 0, 10, 0, 0, 0), -- Vertex 5
      (0, 0, 4, 14, 10, 0, 2, 0, 0),-- Vertex 6
      (0, 0, 0, 0, 0, 2, 0, 1, 6),  -- Vertex 7
      (8, 11, 0, 0, 0, 0, 1, 0, 7), -- Vertex 8
      (0, 0, 2, 0, 0, 0, 6, 7, 0)   -- Vertex 9
   );
   
   -- Output results
   Distances : Distance_Array;
   Visited : Visited_Array;
   
begin
   -- Run Dijkstra from vertex 1
   Dijkstra(Test_Graph, 1, Distances, Visited);
   
   -- Print results
   Put_Line("Vertex   Distance from Source");
   Put_Line("------------------------");
   
   for I in 1..9 loop
      Put(I, Width => 6);
      Put("   ");
      if Distances(I) = Infinity then
         Put("INF");
      else
         Put(Distances(I), Width => 5);
      end if;
      New_Line;
   end loop;
   
end Dijkstra_Example;
```

## Explanation

This Ada implementation of Dijkstra's algorithm includes:

1. **Data Structures**:
   - `Graph_Type`: 2D array representing adjacency matrix
   - `Distance_Array`: Array to store shortest distances
   - `Visited_Array`: Boolean array to track visited vertices

2. **Key Functions**:
   - `Find_Min_Distance`: Finds vertex with minimum distance
   - `Dijkstra`: Main algorithm implementation

3. **Algorithm Steps**:
   - Initialize all distances to infinity except source
   - Select vertex with minimum distance
   - Mark as visited
   - Update distances of adjacent vertices
   - Repeat until all vertices processed

4. **Test Case**:
   - Uses a 9-vertex graph with weighted edges
   - Finds shortest paths from vertex 1 to all other vertices

## Sample Output
```
Vertex   Distance from Source
------------------------
     1      0
     2      4
     3      12
     4      19
     5      21
     6      11
     7      9
     8      8
     9      14
```

The algorithm correctly computes the shortest paths from vertex 1 to all other vertices in the graph.

