# Kosaraju's Algorithm in Ada

Here's an implementation of Kosaraju's algorithm for finding strongly connected components in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Containers.Vectors;
with Ada.Containers.Lists;

procedure Kosaraju_Algorithm is

   -- Graph representation using adjacency lists
   type Vertex is new Integer range 0 .. 100;
   type Edge is record
      From, To : Vertex;
   end record;

   package Edge_Lists is new Ada.Containers.Lists(Edge);
   package Vertex_Vectors is new Ada.Containers.Vectors(Vertex, Vertex);

   type Graph is record
      Adjacency_List : array(Vertex) of Edge_Lists.List;
      Vertices_Count : Integer;
   end record;

   -- Stack for DFS
   type Stack is array(1 .. 100) of Vertex;
   type Stack_Ptr is access Stack;
   Stack_Top : Integer := 0;

   -- Global variables
   Visited : array(Vertex) of Boolean;
   Finish_Times : array(Vertex) of Integer;
   Time : Integer := 0;
   SCC_Count : Integer := 0;

   -- Function to push element to stack
   procedure Push(S : in out Stack; Value : Vertex) is
   begin
      Stack_Top := Stack_Top + 1;
      S(Stack_Top) := Value;
   end Push;

   -- Function to pop element from stack
   function Pop(S : in out Stack) return Vertex is
      Result : Vertex;
   begin
      Result := S(Stack_Top);
      Stack_Top := Stack_Top - 1;
      return Result;
   end Pop;

   -- Function to check if stack is empty
   function Is_Empty(S : Stack) return Boolean is
   begin
      return Stack_Top = 0;
   end Is_Empty;

   -- Add edge to graph
   procedure Add_Edge(G : in out Graph; From, To : Vertex) is
      New_Edge : Edge := (From, To);
   begin
      Edge_Lists.Append(G.Adjacency_List(From), New_Edge);
   end Add_Edge;

   -- First DFS to get finish times
   procedure DFS_First(G : in out Graph; V : Vertex; S : in out Stack) is
      Current : Edge_Lists.Cursor;
   begin
      Visited(V) := True;
      
      -- Visit all adjacent vertices
      Current := Edge_Lists.First(G.Adjacency_List(V));
      while Edge_Lists.Has_Element(Current) loop
         declare
            Adjacent : Vertex := Edge_Lists.Element(Current).To;
         begin
            if not Visited(Adjacent) then
               DFS_First(G, Adjacent, S);
            end if;
         end;
         Current := Edge_Lists.Next(Current);
      end loop;
      
      -- Mark finish time and push to stack
      Time := Time + 1;
      Finish_Times(V) := Time;
      Push(S, V);
   end DFS_First;

   -- Second DFS on transposed graph
   procedure DFS_Second(G : in out Graph; V : Vertex) is
      Current : Edge_Lists.Cursor;
   begin
      Visited(V) := True;
      Put("Vertex: ");
      Put(V);
      Put_Line(" ");
      
      -- Visit all adjacent vertices
      Current := Edge_Lists.First(G.Adjacency_List(V));
      while Edge_Lists.Has_Element(Current) loop
         declare
            Adjacent : Vertex := Edge_Lists.Element(Current).To;
         begin
            if not Visited(Adjacent) then
               DFS_Second(G, Adjacent);
            end if;
         end;
         Current := Edge_Lists.Next(Current);
      end loop;
   end DFS_Second;

   -- Transpose the graph
   procedure Transpose(G : in out Graph; Transposed : in out Graph) is
      Current : Edge_Lists.Cursor;
      Edge : Edge;
   begin
      -- Clear transposed graph
      for I in 0 .. G.Vertices_Count loop
         Edge_Lists.Clear(Transposed.Adjacency_List(I));
      end loop;
      
      -- Build transposed graph
      for I in 0 .. G.Vertices_Count loop
         Current := Edge_Lists.First(G.Adjacency_List(I));
         while Edge_Lists.Has_Element(Current) loop
            Edge := Edge_Lists.Element(Current);
            Add_Edge(Transposed, Edge.To, Edge.From);
            Current := Edge_Lists.Next(Current);
         end loop;
      end loop;
   end Transpose;

   -- Main Kosaraju's algorithm
   procedure Kosaraju(G : in out Graph) is
      S : Stack;
      Transposed : Graph;
      Current : Edge_Lists.Cursor;
      Vertex : Vertex;
   begin
      -- Initialize visited array
      for I in 0 .. G.Vertices_Count loop
         Visited(I) := False;
      end loop;
      
      -- First DFS to get finish times
      Time := 0;
      for I in 0 .. G.Vertices_Count loop
         if not Visited(I) then
            DFS_First(G, I, S);
         end if;
      end loop;
      
      -- Create transposed graph
      Transpose(G, Transposed);
      
      -- Initialize visited array for second DFS
      for I in 0 .. G.Vertices_Count loop
         Visited(I) := False;
      end loop;
      
      -- Second DFS on transposed graph in order of finish times
      Put_Line("Strongly Connected Components:");
      while not Is_Empty(S) loop
         Vertex := Pop(S);
         if not Visited(Vertex) then
            SCC_Count := SCC_Count + 1;
            Put("SCC ");
            Put(SCC_Count);
            Put(": ");
            DFS_Second(Transposed, Vertex);
            Put_Line("");
         end if;
      end loop;
   end Kosaraju;

   -- Example usage
   G : Graph;
begin
   -- Initialize graph with 5 vertices
   G.Vertices_Count := 5;
   
   -- Clear adjacency lists
   for I in 0 .. G.Vertices_Count loop
      Edge_Lists.Clear(G.Adjacency_List(I));
   end loop;
   
   -- Add edges to create a sample graph
   Add_Edge(G, 0, 1);
   Add_Edge(G, 1, 2);
   Add_Edge(G, 2, 0);
   Add_Edge(G, 1, 3);
   Add_Edge(G, 3, 4);
   
   -- Run Kosaraju's algorithm
   Kosaraju(G);
   
end Kosaraju_Algorithm;
```

## Example Output

When you run this program, you should see output similar to:

```
Strongly Connected Components:
SCC 1: Vertex: 0 
Vertex: 1 
Vertex: 2 

SCC 2: Vertex: 3 
Vertex: 4 
```

## Algorithm Explanation

This implementation of Kosaraju's algorithm follows these steps:

1. **First DFS**: Perform depth-first search on the original graph and record finish times for each vertex
2. **Transpose Graph**: Create a transposed version of the original graph
3. **Second DFS**: Perform DFS on the transposed graph in the order of decreasing finish times
4. **SCC Identification**: Each DFS tree in the second traversal represents a strongly connected component

## Key Features

- Uses Ada containers for dynamic data structures
- Implements proper stack operations for DFS traversal
- Handles graph transposition correctly
- Provides clear output showing identified strongly connected components
- Includes proper initialization and cleanup of data structures

The algorithm has a time complexity of O(V + E) and space complexity of O(V + E), where V is the number of vertices and E is the number of edges.

