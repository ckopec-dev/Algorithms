# Floyd-Warshall Algorithm in Ada

Here's an implementation of the Floyd-Warshall algorithm in Ada to find the shortest paths between all pairs of vertices in a weighted graph:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Floyd_Warshall is
   
   -- Define constants
   Max_Vertices : constant := 4;
   Infinity     : constant := 999999;
   
   -- Graph representation as adjacency matrix
   type Matrix is array (1..Max_Vertices, 1..Max_Vertices) of Integer;
   
   -- Function to print the matrix
   procedure Print_Matrix(M : Matrix; Name : String) is
   begin
      Put_Line(Name & ":");
      for I in 1..Max_Vertices loop
         for J in 1..Max_Vertices loop
            if M(I, J) = Infinity then
               Put("  ∞ ");
            else
               Put(M(I, J), Width => 3);
            end if;
         end loop;
         New_Line;
      end loop;
      New_Line;
   end Print_Matrix;
   
   -- Floyd-Warshall algorithm implementation
   procedure Floyd_Warshall_Algorithm(G : in out Matrix) is
      D : Matrix := G;  -- Copy of the original matrix
   begin
      -- Floyd-Warshall algorithm
      for K in 1..Max_Vertices loop
         for I in 1..Max_Vertices loop
            for J in 1..Max_Vertices loop
               if D(I, K) + D(K, J) < D(I, J) then
                  D(I, J) := D(I, K) + D(K, J);
               end if;
            end loop;
         end loop;
      end loop;
      
      -- Print the result
      Print_Matrix(D, "Shortest distances");
   end Floyd_Warshall_Algorithm;
   
   -- Main graph - adjacency matrix
   Graph : Matrix := (
      (0, 3, Infinity, 7),   -- Vertex 1
      (Infinity, 0, 1, Infinity), -- Vertex 2
      (Infinity, Infinity, 0, 2), -- Vertex 3
      (Infinity, Infinity, Infinity, 0)  -- Vertex 4
   );
   
begin
   Put_Line("Floyd-Warshall Algorithm Example");
   Put_Line("===============================");
   
   -- Print original graph
   Print_Matrix(Graph, "Original graph");
   
   -- Apply Floyd-Warshall algorithm
   Floyd_Warshall_Algorithm(Graph);
   
end Floyd_Warshall;
```

## Expected Output:
```
Floyd-Warshall Algorithm Example
===============================
Original graph:
  0   3  ∞   7 
 ∞   0   1  ∞ 
 ∞  ∞   0   2 
 ∞  ∞  ∞   0 

Shortest distances:
  0   3   4   6 
 ∞   0   1   3 
 ∞  ∞   0   2 
 ∞  ∞  ∞   0 
```

## Explanation:

1. **Matrix Representation**: The graph is represented as a 4x4 adjacency matrix where `Infinity` represents no direct connection between vertices.

2. **Algorithm Steps**:
   - Initialize distance matrix with the original graph
   - For each intermediate vertex `k`, update all pairs of vertices `(i,j)` 
   - If the path through `k` is shorter than the current path, update the distance

3. **Time Complexity**: O(V³) where V is the number of vertices
4. **Space Complexity**: O(V²) for the distance matrix

5. **Result Interpretation**:
   - `D[i][j]` represents the shortest distance from vertex `i` to vertex `j`
   - The final matrix shows all shortest paths between every pair of vertices

This implementation demonstrates the classic Floyd-Warshall algorithm for finding the shortest paths in a weighted graph with both positive and negative edge weights (but no negative cycles).

