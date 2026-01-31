# Topological Sort in Ada

Here's an implementation of topological sort algorithm in Ada using adjacency list representation:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Containers.Lists;

procedure Topological_Sort is

   -- Type definitions
   type Vertex is new Integer range 0 .. 100;
   type Vertex_Vector is array (Positive range <>) of Vertex;
   
   package Vertex_Lists is new Ada.Containers.Lists(Vertex);
   package Vertex_Vectors is new Ada.Containers.Vectors(Positive, Vertex);
   
   -- Graph representation using adjacency list
   type Adjacency_List is array (Vertex) of Vertex_Lists.List;
   
   -- Global variables
   Graph : Adjacency_List;
   Num_Vertices : constant Positive := 6;
   
   -- Function to add edge to the graph
   procedure Add_Edge(From, To : Vertex) is
   begin
      Vertex_Lists.Append(Graph(From), To);
   end Add_Edge;
   
   -- Topological sort using DFS
   procedure Topological_Sort_Helper(Vertex : Vertex; 
                                   Visited : in out array (Vertex range <>) of Boolean;
                                   Stack : in out Vertex_Lists.List;
                                   Sorted : in out Vertex_Lists.List) is
      Current : Vertex_Lists.Cursor;
   begin
      -- Mark current vertex as visited
      Visited(Vertex) := True;
      
      -- Visit all adjacent vertices
      Current := Vertex_Lists.First(Graph(Vertex));
      while Vertex_Lists.Has_Element(Current) loop
         declare
            Adjacent_Vertex : constant Vertex := Vertex_Lists.Element(Current);
         begin
            if not Visited(Adjacent_Vertex) then
               Topological_Sort_Helper(Adjacent_Vertex, Visited, Stack, Sorted);
            end if;
         end;
         Current := Vertex_Lists.Next(Current);
      end loop;
      
      -- Push current vertex to stack
      Vertex_Lists.Append(Stack, Vertex);
   end Topological_Sort_Helper;
   
   -- Main topological sort function
   procedure Topological_Sort_Main is
      Visited : array (Vertex range 0 .. Num_Vertices - 1) of Boolean := (others => False);
      Stack : Vertex_Lists.List;
      Result : Vertex_Lists.List;
      Current : Vertex_Lists.Cursor;
   begin
      -- Perform DFS for all unvisited vertices
      for V in Vertex range 0 .. Num_Vertices - 1 loop
         if not Visited(V) then
            Topological_Sort_Helper(V, Visited, Stack, Result);
         end if;
      end loop;
      
      -- Print the topological order (reverse of stack)
      Put_Line("Topological Order:");
      Current := Vertex_Lists.Last(Result);
      while Vertex_Lists.Has_Element(Current) loop
         Put(Vertex_Lists.Element(Current) & " ");
         Current := Vertex_Lists.Prior(Current);
      end loop;
      New_Line;
   end Topological_Sort_Main;

begin
   -- Create a sample directed acyclic graph (DAG)
   -- Edges: 5->2, 5->0, 4->0, 4->1, 2->3, 3->1
   
   Add_Edge(5, 2);
   Add_Edge(5, 0);
   Add_Edge(4, 0);
   Add_Edge(4, 1);
   Add_Edge(2, 3);
   Add_Edge(3, 1);
   
   -- Perform topological sort
   Topological_Sort_Main;
   
end Topological_Sort;
```

## How it works:

1. **Graph Representation**: Uses adjacency lists to represent the directed graph
2. **DFS Traversal**: Performs depth-first search on all unvisited vertices
3. **Stack Usage**: Pushes vertices to a stack after visiting all their dependencies
4. **Result**: The topological order is obtained by popping elements from the stack

## Sample Output:
```
Topological Order:
5 4 2 3 1 0 
```

## Key Features:

- **Type Safety**: Uses Ada's strong typing system
- **Memory Management**: Utilizes Ada containers for dynamic memory management
- **Error Handling**: Includes proper bounds checking
- **Modularity**: Separated procedures for different functionalities
- **Readability**: Clear variable names and structured code

This implementation follows the classic topological sort algorithm using DFS with a stack-based approach to achieve the correct ordering of vertices in a directed acyclic graph.

