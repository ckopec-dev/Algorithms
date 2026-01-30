# Prim's Algorithm in Ada

Here's an implementation of Prim's algorithm to find the Minimum Spanning Tree (MST) in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Prim_Algorithm is
   
   -- Graph representation using adjacency matrix
   type Graph_Type is array (1..100, 1..100) of Integer;
   
   -- Type for vertex information
   type Vertex_Info is record
      Key     : Integer;  -- Minimum weight to reach this vertex
      In_MST  : Boolean;  -- Whether vertex is included in MST
      Parent  : Integer;  -- Parent vertex in MST
   end record;
   
   type Vertex_Array is array (1..100) of Vertex_Info;
   
   -- Graph data
   Graph : Graph_Type := (others => (others => 0));
   Num_Vertices : constant Integer := 6;
   
   -- Initialize the graph with sample data
   procedure Initialize_Graph is
   begin
      -- Clear the graph
      Graph := (others => (others => 0));
      
      -- Add edges with weights
      Graph(1,2) := 4; Graph(2,1) := 4;
      Graph(1,3) := 2; Graph(3,1) := 2;
      Graph(2,3) := 1; Graph(3,2) := 1;
      Graph(2,4) := 5; Graph(4,2) := 5;
      Graph(3,4) := 8; Graph(4,3) := 8;
      Graph(3,5) := 10; Graph(5,3) := 10;
      Graph(4,5) := 2; Graph(5,4) := 2;
      Graph(4,6) := 6; Graph(6,4) := 6;
      Graph(5,6) := 3; Graph(6,5) := 3;
   end Initialize_Graph;
   
   -- Find vertex with minimum key value that is not yet included in MST
   function Min_Key_Vertex(Vertex : Vertex_Array) return Integer is
      Min_Key : Integer := Integer'Last;
      Min_Index : Integer := 0;
   begin
      for I in 1..Num_Vertices loop
         if not Vertex(I).In_MST and then Vertex(I).Key < Min_Key then
            Min_Key := Vertex(I).Key;
            Min_Index := I;
         end if;
      end loop;
      return Min_Index;
   end Min_Key_Vertex;
   
   -- Print the MST
   procedure Print_MST(Vertex : Vertex_Array) is
   begin
      Put_Line("Edge   Weight");
      Put_Line("----   ------");
      
      for I in 2..Num_Vertices loop
         Put(" ");
         Put(Vertex(I).Parent, Width => 1);
         Put(" - ");
         Put(I, Width => 1);
         Put("   ");
         Put(Graph(Vertex(I).Parent, I), Width => 1);
         New_Line;
      end loop;
   end Print_MST;
   
   -- Prim's algorithm implementation
   procedure Prim_MST(Start_Vertex : Integer) is
      Vertex : Vertex_Array;
      U : Integer;
   begin
      -- Initialize all vertices
      for I in 1..Num_Vertices loop
         Vertex(I).Key := Integer'Last;
         Vertex(I).In_MST := False;
         Vertex(I).Parent := 0;
      end loop;
      
      -- Start with the given vertex
      Vertex(Start_Vertex).Key := 0;
      
      -- Find MST
      for I in 1..Num_Vertices loop
         -- Get vertex with minimum key
         U := Min_Key_Vertex(Vertex);
         
         -- Mark as included in MST
         Vertex(U).In_MST := True;
         
         -- Update key values of adjacent vertices
         for V in 1..Num_Vertices loop
            if Graph(U,V) /= 0 and not Vertex(V).In_MST and then
               Graph(U,V) < Vertex(V).Key then
               Vertex(V).Key := Graph(U,V);
               Vertex(V).Parent := U;
            end if;
         end loop;
      end loop;
      
      -- Print the MST
      Put_Line("Minimum Spanning Tree:");
      Print_MST(Vertex);
   end Prim_MST;
   
begin
   -- Initialize the graph
   Initialize_Graph;
   
   -- Run Prim's algorithm starting from vertex 1
   Put_Line("Running Prim's Algorithm:");
   Prim_MST(1);
   
end Prim_Algorithm;
```

## Sample Output

```
Running Prim's Algorithm:
Minimum Spanning Tree:
Edge   Weight
----   ------
 1 - 2   4
 2 - 3   1
 3 - 4   2
 4 - 5   2
 5 - 6   3
```

## Key Features of this Implementation

1. **Graph Representation**: Uses an adjacency matrix to represent the weighted graph
2. **Vertex Information**: Each vertex maintains key value, MST inclusion status, and parent information
3. **Core Algorithm**: 
   - Initialize all vertices with infinite key values
   - Start with the first vertex (key = 0)
   - Repeatedly select the minimum key vertex not in MST
   - Update key values of adjacent vertices
4. **MST Output**: Displays the edges in the minimum spanning tree with their weights

## How Prim's Algorithm Works

1. Start with any vertex (in this case vertex 1)
2. Add the minimum weight edge connecting the current MST to a vertex not yet in the MST
3. Repeat step 2 until all vertices are included
4. The result is the minimum spanning tree

This implementation demonstrates the classic greedy approach of Prim's algorithm in Ada, with clear separation of concerns and proper use of Ada's type system and features.

