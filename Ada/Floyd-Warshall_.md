# Floyd-Warshall Algorithm in Ada

Here's an implementation of the Floyd-Warshall algorithm in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Floyd_Warshall is
   
   -- Define constants
   type Vertex is range 1..4;
   type Weight is range 0..9999;
   
   -- Graph representation as adjacency matrix
   type Graph_Matrix is array (Vertex, Vertex) of Weight;
   
   -- Initialize graph with infinity values (9999)
   procedure Initialize_Matrix(M : out Graph_Matrix) is
   begin
      for I in Vertex loop
         for J in Vertex loop
            if I = J then
               M(I, J) := 0;
            else
               M(I, J) := 9999;
            end if;
         end loop;
      end loop;
   end Initialize_Matrix;
   
   -- Print the distance matrix
   procedure Print_Matrix(M : in Graph_Matrix) is
   begin
      Put_Line("Distance Matrix:");
      for I in Vertex loop
         for J in Vertex loop
            if M(I, J) = 9999 then
               Put("INF ");
            else
               Put(M(I, J), Width => 3);
               Put(" ");
            end if;
         end loop;
         New_Line;
      end loop;
      New_Line;
   end Print_Matrix;
   
   -- Floyd-Warshall algorithm implementation
   procedure Floyd_Warshall_Algorithm(M : in out Graph_Matrix) is
      N : constant := Vertex'Last;
   begin
      -- For each intermediate vertex k
      for K in Vertex loop
         -- For each source vertex i
         for I in Vertex loop
            -- For each destination vertex j
            for J in Vertex loop
               -- If path through k is shorter, update distance
               if M(I, K) + M(K, J) < M(I, J) then
                  M(I, J) := M(I, K) + M(K, J);
               end if;
            end loop;
         end loop;
      end loop;
   end Floyd_Warshall_Algorithm;
   
   -- Test graph
   Graph : Graph_Matrix;
   
begin
   -- Initialize the graph
   Initialize_Matrix(Graph);
   
   -- Set actual edge weights (example graph)
   Graph(1, 2) := 3;
   Graph(1, 3) := 8;
   Graph(1, 4) := 9999;  -- No direct edge
   Graph(2, 1) := 9999;  -- No direct edge
   Graph(2, 3) := 9999;  -- No direct edge
   Graph(2, 4) := 1;
   Graph(3, 1) := 9999;  -- No direct edge
   Graph(3, 2) := 9999;  -- No direct edge
   Graph(3, 4) := 2;
   Graph(4, 1) := 9999;  -- No direct edge
   Graph(4, 2) := 9999;  -- No direct edge
   Graph(4, 3) := 9999;  -- No direct edge
   
   -- Display original graph
   Put_Line("Original Graph:");
   Print_Matrix(Graph);
   
   -- Apply Floyd-Warshall algorithm
   Floyd_Warshall_Algorithm(Graph);
   
   -- Display result
   Put_Line("Shortest paths after Floyd-Warshall:");
   Print_Matrix(Graph);
   
end Floyd_Warshall;
```

## Expected Output:
```
Original Graph:
Distance Matrix:
  0   3   8  INF 
INF   0  INF   1 
INF  INF   0   2 
INF  INF  INF   0 

Shortest paths after Floyd-Warshall:
Distance Matrix:
  0   3   5   1 
INF   0  INF   1 
INF  INF   0   2 
INF  INF  INF   0 
```

## Key Features of this Implementation:

1. **Type Safety**: Uses Ada's strong typing system with defined types
2. **Constants**: Uses `Vertex` and `Weight` types for better code clarity
3. **Matrix Operations**: Implements the core Floyd-Warshall algorithm with three nested loops
4. **Error Handling**: Uses 9999 as infinity value for unreachable vertices
5. **Modular Design**: Separates initialization, printing, and algorithm logic
6. **Clear Output**: Displays both original and resulting distance matrices

The algorithm finds the shortest paths between all pairs of vertices in a weighted graph with time complexity O(V³) where V is the number of vertices.

