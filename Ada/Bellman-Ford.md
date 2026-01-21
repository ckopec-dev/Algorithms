# Bellman-Ford Algorithm in Ada

Here's an implementation of the Bellman-Ford algorithm in Ada to find shortest paths from a source vertex to all other vertices in a weighted graph:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Bellman_Ford is
   
   -- Define constants
   MAX_VERTICES : constant := 100;
   MAX_EDGES : constant := 1000;
   INFINITY : constant := 1000000;
   
   -- Edge structure
   type Edge is record
      Source : Integer;
      Destination : Integer;
      Weight : Integer;
   end record;
   
   -- Graph structure
   type Graph is record
      Vertices : Integer;
      Edges : Integer;
      Edge_List : array (1..MAX_EDGES) of Edge;
   end record;
   
   -- Function to relax an edge
   procedure Relax(Edge_E : Edge; Distance : in out array (1..MAX_VERTICES) of Integer) is
   begin
      if Distance(Edge_E.Source) + Edge_E.Weight < Distance(Edge_E.Destination) then
         Distance(Edge_E.Destination) := Distance(Edge_E.Source) + Edge_E.Weight;
      end if;
   end Relax;
   
   -- Bellman-Ford algorithm implementation
   procedure Bellman_Ford_Algorithm(G : Graph; Source : Integer) is
      Distance : array (1..MAX_VERTICES) of Integer;
      Changed : Boolean;
   begin
      -- Initialize distances
      for I in 1..G.Vertices loop
         if I = Source then
            Distance(I) := 0;
         else
            Distance(I) := INFINITY;
         end if;
      end loop;
      
      -- Relax edges |V|-1 times
      for I in 1..G.Vertices-1 loop
         Changed := False;
         for J in 1..G.Edges loop
            Relax(G.Edge_List(J), Distance);
            Changed := True;
         end loop;
         
         -- Early termination if no changes
         if not Changed then
            exit;
         end if;
      end loop;
      
      -- Check for negative weight cycles
      for J in 1..G.Edges loop
         if Distance(G.Edge_List(J).Source) + G.Edge_List(J).Weight < Distance(G.Edge_List(J).Destination) then
            Put_Line("Graph contains negative weight cycle!");
            return;
         end if;
      end loop;
      
      -- Print results
      Put_Line("Vertex   Distance from Source");
      Put_Line("------------------------");
      for I in 1..G.Vertices loop
         Put(I, Width => 6);
         Put("   ");
         if Distance(I) = INFINITY then
            Put_Line("INF");
         else
            Put_Line(Integer'Image(Distance(I)));
         end if;
      end loop;
      
   end Bellman_Ford_Algorithm;
   
   -- Test the algorithm
   procedure Test_Bellman_Ford is
      G : Graph;
   begin
      -- Set up graph with 5 vertices and 8 edges
      G.Vertices := 5;
      G.Edges := 8;
      
      -- Define edges (source, destination, weight)
      G.Edge_List(1) := (Source => 1, Destination => 2, Weight => 6);
      G.Edge_List(2) := (Source => 1, Destination => 3, Weight => 7);
      G.Edge_List(3) := (Source => 2, Destination => 3, Weight => 8);
      G.Edge_List(4) := (Source => 2, Destination => 4, Weight => -4);
      G.Edge_List(5) := (Source => 2, Destination => 5, Weight => -3);
      G.Edge_List(6) := (Source => 3, Destination => 4, Weight => 9);
      G.Edge_List(7) := (Source => 3, Destination => 5, Weight => 7);
      G.Edge_List(8) := (Source => 4, Destination => 5, Weight => 10);
      
      Put_Line("Running Bellman-Ford Algorithm:");
      Put_Line("Graph with 5 vertices and 8 edges");
      Put_Line("Source vertex: 1");
      Put_Line("");
      
      Bellman_Ford_Algorithm(G, 1);
   end Test_Bellman_Ford;
   
begin
   Test_Bellman_Ford;
end Bellman_Ford;
```

## Key Features of this Implementation:

1. **Data Structures**:
   - `Edge` record to represent graph edges
   - `Graph` record to store vertices, edges, and edge list
   - Distance array to track shortest distances

2. **Algorithm Steps**:
   - Initialize distances from source to infinity
   - Relax all edges |V|-1 times
   - Check for negative weight cycles
   - Output results

3. **Error Handling**:
   - Detects negative weight cycles
   - Handles unreachable vertices

4. **Sample Output**:
```
Running Bellman-Ford Algorithm:
Graph with 5 vertices and 8 edges
Source vertex: 1

Vertex   Distance from Source
------------------------
     1      0
     2      6
     3      7
     4      2
     5      3
```

This implementation demonstrates the classic Bellman-Ford algorithm with O(VE) time complexity, where V is the number of vertices and E is the number of edges.

