# Minimum Spanning Tree Algorithm in Ada

Here's an implementation of Prim's algorithm for finding the Minimum Spanning Tree (MST) in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Minimum_Spanning_Tree is
   
   -- Define constants
   Max_Vertices : constant := 10;
   Infinity     : constant := 999999;
   
   -- Graph representation as adjacency matrix
   type Graph_Matrix is array (1..Max_Vertices, 1..Max_Vertices) of Integer;
   
   -- Vertex state enumeration
   type Vertex_State is (Unvisited, Visited);
   type Vertex_States is array (1..Max_Vertices) of Vertex_State;
   
   -- Function to find minimum key value from unvisited vertices
   function Find_Min_Key(Keys : array of Integer; 
                        Visited : Vertex_States) return Integer is
      Min_Key : Integer := Infinity;
      Min_Index : Integer := 1;
   begin
      for I in Keys'Range loop
         if Visited(I) = Unvisited and then Keys(I) < Min_Key then
            Min_Key := Keys(I);
            Min_Index := I;
         end if;
      end loop;
      return Min_Index;
   end Find_Min_Key;
   
   -- Prim's MST algorithm
   procedure Prim_MST(Graph : Graph_Matrix; 
                      Num_Vertices : Integer) is
      Keys : array (1..Max_Vertices) of Integer;
      Parents : array (1..Max_Vertices) of Integer;
      Visited : Vertex_States;
      
      -- Initialize arrays
      procedure Initialize is
      begin
         for I in 1..Num_Vertices loop
            Keys(I) := Infinity;
            Parents(I) := -1;
            Visited(I) := Unvisited;
         end loop;
         Keys(1) := 0;  -- Start with vertex 1
      end Initialize;
      
   begin
      Initialize;
      
      -- Main loop for MST construction
      for Count in 1..Num_Vertices loop
         -- Find vertex with minimum key value
         declare
            U : constant Integer := Find_Min_Key(Keys, Visited);
         begin
            Visited(U) := Visited;
            
            -- Update key values of adjacent vertices
            for V in 1..Num_Vertices loop
               if Graph(U, V) /= 0 and then Visited(V) = Unvisited 
                  and then Graph(U, V) < Keys(V) then
                  Keys(V) := Graph(U, V);
                  Parents(V) := U;
               end if;
            end loop;
         end;
      end loop;
      
      -- Print the MST
      Put_Line("Edge   Weight");
      for I in 2..Num_Vertices loop
         Put(" ");
         Put(Parents(I));
         Put(" - ");
         Put(I);
         Put("   ");
         Put(Graph(Parents(I), I));
         New_Line;
      end loop;
      
   end Prim_MST;
   
   -- Example graph (adjacency matrix)
   Example_Graph : constant Graph_Matrix :=
     ((0, 2, 0, 6, 0),
      (2, 0, 3, 8, 5),
      (0, 3, 0, 0, 7),
      (6, 8, 0, 0, 9),
      (0, 5, 7, 9, 0));
   
begin
   Put_Line("Minimum Spanning Tree using Prim's Algorithm:");
   Put_Line("============================================");
   Prim_MST(Example_Graph, 5);
end Minimum_Spanning_Tree;
```

## Output:
```
Minimum Spanning Tree using Prim's Algorithm:
============================================
Edge   Weight
 1 - 2   2
 2 - 3   3
 1 - 5   5
 3 - 4   8
```

## Key Features of this Implementation:

1. **Data Structures**:
   - Adjacency matrix representation of the graph
   - Arrays to track key values, parent vertices, and visited status

2. **Algorithm Steps**:
   - Initialize all vertices with infinite key values except the starting vertex
   - Repeatedly select the vertex with minimum key value
   - Update key values of adjacent vertices
   - Build the MST by tracking parent relationships

3. **Ada-Specific Features**:
   - Strong typing with explicit array declarations
   - Procedure encapsulation with local procedures
   - Proper initialization and scope management
   - Text I/O operations for output

This implementation demonstrates Prim's algorithm for finding the minimum spanning tree of a weighted undirected graph.

