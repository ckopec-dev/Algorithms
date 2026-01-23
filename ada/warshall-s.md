# Warshall's Algorithm in Ada

Here's an implementation of Warshall's algorithm (transitive closure) in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Warshall_Algorithm is
   
   -- Define matrix size
   type Matrix is array (1..4, 1..4) of Integer;
   
   -- Function to print matrix
   procedure Print_Matrix(M : Matrix) is
   begin
      for I in 1..4 loop
         for J in 1..4 loop
            Put(M(I,J), 3);
         end loop;
         New_Line;
      end loop;
      New_Line;
   end Print_Matrix;
   
   -- Warshall's Algorithm implementation
   procedure Warshall(M : in out Matrix) is
      N : constant Integer := 4;
   begin
      -- For each intermediate vertex k
      for K in 1..N loop
         -- For each source vertex i
         for I in 1..N loop
            -- For each destination vertex j
            for J in 1..N loop
               -- If there's a path from i to k and from k to j,
               -- then there's a path from i to j
               if M(I,K) = 1 and M(K,J) = 1 then
                  M(I,J) := 1;
               end if;
            end loop;
         end loop;
      end loop;
   end Warshall;
   
   -- Test matrix (adjacency matrix)
   A : Matrix := 
     ((0, 1, 0, 0),
      (0, 0, 1, 0),
      (0, 0, 0, 1),
      (1, 0, 0, 0));
   
begin
   Put_Line("Original adjacency matrix:");
   Print_Matrix(A);
   
   -- Apply Warshall's algorithm
   Warshall(A);
   
   Put_Line("Transitive closure matrix:");
   Print_Matrix(A);
   
end Warshall_Algorithm;
```

## Expected Output:
```
Original adjacency matrix:
  0  1  0  0
  0  0  1  0
  0  0  0  1
  1  0  0  0

Transitive closure matrix:
  1  1  1  1
  1  1  1  1
  1  1  1  1
  1  1  1  1
```

## Explanation:

1. **Matrix Representation**: The algorithm works on an adjacency matrix where `M(i,j) = 1` indicates there's a direct edge from vertex `i` to vertex `j`.

2. **Algorithm Steps**:
   - For each intermediate vertex `k` from 1 to N
   - For each source vertex `i` from 1 to N  
   - For each destination vertex `j` from 1 to N
   - If there's a path from `i` to `k` AND from `k` to `j`, then set path from `i` to `j`

3. **Time Complexity**: O(N³) where N is the number of vertices
4. **Space Complexity**: O(1) - modifies the input matrix in-place

The example shows a graph with vertices 1,2,3,4 where 1→2→3→4→1 forms a cycle, and the transitive closure shows that all vertices are reachable from all other vertices.

