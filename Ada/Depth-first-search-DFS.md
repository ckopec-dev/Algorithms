# Depth-First Search (DFS) in Ada

Here's a complete example of a Depth-First Search algorithm implemented in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure DFS_Example is
   
   -- Define the maximum number of vertices
   Max_Vertices : constant := 10;
   
   -- Graph representation using adjacency matrix
   type Graph is array (1..Max_Vertices, 1..Max_Vertices) of Boolean;
   
   -- Visited array to track visited vertices
   type Visited_Array is array (1..Max_Vertices) of Boolean;
   
   -- Graph instance
   My_Graph : Graph := (others => (others => False));
   Visited  : Visited_Array := (others => False);
   
   -- DFS procedure
   procedure DFS(Vertex : in Integer) is
   begin
      -- Mark current vertex as visited
      Visited(Vertex) := True;
      Put("Visited: ");
      Put(Vertex);
      New_Line;
      
      -- Visit all adjacent vertices
      for V in 1..Max_Vertices loop
         if My_Graph(Vertex, V) and not Visited(V) then
            DFS(V);
         end if;
      end loop;
   end DFS;
   
   -- Function to add edge to graph
   procedure Add_Edge(From, To : in Integer) is
   begin
      My_Graph(From, To) := True;
      My_Graph(To, From) := True; -- For undirected graph
   end Add_Edge;
   
begin
   -- Create a sample graph
   -- Graph structure:
   -- 1-2-4-5
   -- | |/
   -- 3-6
   Add_Edge(1, 2);
   Add_Edge(1, 3);
   Add_Edge(2, 4);
   Add_Edge(2, 6);
   Add_Edge(4, 5);
   Add_Edge(3, 6);
   
   Put_Line("Starting DFS from vertex 1:");
   DFS(1);
   
   Put_Line("DFS completed!");
   
end DFS_Example;
```

## Output
```
Starting DFS from vertex 1:
Visited: 1
Visited: 2
Visited: 4
Visited: 5
Visited: 6
Visited: 3
DFS completed!
```

## Key Features of this Implementation:

1. **Graph Representation**: Uses an adjacency matrix to represent the graph
2. **Visited Tracking**: Maintains a boolean array to track visited vertices
3. **Recursive DFS**: Implements the classic recursive DFS algorithm
4. **Undirected Graph**: Edges are added in both directions
5. **Clear Output**: Shows the order in which vertices are visited

## Algorithm Steps:

1. Start with the initial vertex
2. Mark it as visited
3. For each unvisited adjacent vertex, recursively call DFS
4. Continue until all reachable vertices are visited

This implementation demonstrates the core principles of DFS traversal in Ada, showing how to handle graph representation, recursion, and vertex tracking.

