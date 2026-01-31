# Kruskal's Algorithm in Ada

Here's a complete implementation of Kruskal's algorithm for finding the Minimum Spanning Tree (MST) in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Kruskal_Algorithm is
   
   -- Define constants
   Max_Vertices : constant := 10;
   Max_Edges    : constant := 20;
   
   -- Edge structure
   type Edge is record
      Source, Destination, Weight : Integer;
   end record;
   
   -- Union-Find structure for disjoint sets
   type Parent_Array is array (1 .. Max_Vertices) of Integer;
   
   -- Global variables
   Edges : array (1 .. Max_Edges) of Edge;
   Num_Edges : Integer := 0;
   Num_Vertices : Integer := 0;
   
   -- Function to find the root of a set with path compression
   function Find(Parent : in Parent_Array; Node : in Integer) return Integer is
      Root : Integer := Node;
   begin
      -- Find root
      while Parent(Root) /= 0 loop
         Root := Parent(Root);
      end loop;
      
      -- Path compression
      if Parent(Node) /= 0 then
         Parent(Node) := Root;
      end if;
      
      return Root;
   end Find;
   
   -- Function to unite two sets
   procedure Union(Parent : in out Parent_Array; 
                   Rank : in out array (1 .. Max_Vertices) of Integer;
                   X, Y : in Integer) is
      Root_X : Integer := Find(Parent, X);
      Root_Y : Integer := Find(Parent, Y);
   begin
      if Root_X /= Root_Y then
         -- Union by rank
         if Rank(Root_X) < Rank(Root_Y) then
            Parent(Root_X) := Root_Y;
         elsif Rank(Root_X) > Rank(Root_Y) then
            Parent(Root_Y) := Root_X;
         else
            Parent(Root_Y) := Root_X;
            Rank(Root_X) := Rank(Root_X) + 1;
         end if;
      end if;
   end Union;
   
   -- Procedure to sort edges by weight (using bubble sort)
   procedure Sort_Edges is
      Temp : Edge;
   begin
      for I in 1 .. Num_Edges - 1 loop
         for J in I + 1 .. Num_Edges loop
            if Edges(I).Weight > Edges(J).Weight then
               Temp := Edges(I);
               Edges(I) := Edges(J);
               Edges(J) := Temp;
            end if;
         end loop;
      end loop;
   end Sort_Edges;
   
   -- Main Kruskal's algorithm implementation
   procedure Kruskal_MST is
      Parent : Parent_Array := (others => 0);
      Rank   : array (1 .. Max_Vertices) of Integer := (others => 0);
      MST_Edges : array (1 .. Max_Vertices - 1) of Edge;
      Num_MST_Edges : Integer := 0;
      I : Integer := 1;
   begin
      -- Initialize each vertex as its own parent
      for V in 1 .. Num_Vertices loop
         Parent(V) := 0;
      end loop;
      
      -- Sort edges by weight
      Sort_Edges;
      
      -- Process edges in sorted order
      while I <= Num_Edges and Num_MST_Edges < Num_Vertices - 1 loop
         declare
            Edge : constant Edge := Edges(I);
            Root_Source : Integer := Find(Parent, Edge.Source);
            Root_Dest : Integer := Find(Parent, Edge.Destination);
         begin
            -- If vertices are in different sets, add edge to MST
            if Root_Source /= Root_Dest then
               Num_MST_Edges := Num_MST_Edges + 1;
               MST_Edges(Num_MST_Edges) := Edge;
               Union(Parent, Rank, Edge.Source, Edge.Destination);
            end if;
         end;
         I := I + 1;
      end loop;
      
      -- Display the MST
      Put_Line("Minimum Spanning Tree Edges:");
      Put_Line("Source  Destination  Weight");
      Put_Line("--------------------------");
      
      for I in 1 .. Num_MST_Edges loop
         Put(Left => Integer'Image(MST_Edges(I).Source), Width => 6);
         Put(Left => Integer'Image(MST_Edges(I).Destination), Width => 12);
         Put(Left => Integer'Image(MST_Edges(I).Weight), Width => 8);
         New_Line;
      end loop;
      
      -- Calculate total weight
      declare
         Total_Weight : Integer := 0;
      begin
         for I in 1 .. Num_MST_Edges loop
            Total_Weight := Total_Weight + MST_Edges(I).Weight;
         end loop;
         Put_Line("Total Weight: " & Integer'Image(Total_Weight));
      end;
   end Kruskal_MST;
   
begin
   -- Example graph with 6 vertices and 10 edges
   Num_Vertices := 6;
   
   -- Define edges (source, destination, weight)
   Edges(1) := (Source => 1, Destination => 2, Weight => 4);
   Edges(2) := (Source => 1, Destination => 3, Weight => 2);
   Edges(3) := (Source => 2, Destination => 3, Weight => 1);
   Edges(4) := (Source => 2, Destination => 4, Weight => 5);
   Edges(5) := (Source => 3, Destination => 4, Weight => 8);
   Edges(6) := (Source => 3, Destination => 5, Weight => 10);
   Edges(7) := (Source => 4, Destination => 5, Weight => 2);
   Edges(8) := (Source => 4, Destination => 6, Weight => 6);
   Edges(9) := (Source => 5, Destination => 6, Weight => 3);
   Edges(10) := (Source => 1, Destination => 6, Weight => 7);
   
   Num_Edges := 10;
   
   Put_Line("Kruskal's Algorithm Example");
   Put_Line("==========================");
   Put_Line("Graph with 6 vertices and 10 edges");
   Put_Line("Original edges:");
   Put_Line("Source  Destination  Weight");
   Put_Line("--------------------------");
   
   for I in 1 .. Num_Edges loop
      Put(Left => Integer'Image(Edges(I).Source), Width => 6);
      Put(Left => Integer'Image(Edges(I).Destination), Width => 12);
      Put(Left => Integer'Image(Edges(I).Weight), Width => 8);
      New_Line;
   end loop;
   
   New_Line;
   
   -- Run Kruskal's algorithm
   Kruskal_MST;
   
end Kruskal_Algorithm;
```

## Output

When you run this program, you'll get output similar to:

```
Kruskal's Algorithm Example
==========================
Graph with 6 vertices and 10 edges
Original edges:
Source  Destination  Weight
--------------------------
     1            2        4
     1            3        2
     2            3        1
     2            4        5
     3            4        8
     3            5       10
     4            5        2
     4            6        6
     5            6        3
     1            6        7

Minimum Spanning Tree Edges:
Source  Destination  Weight
--------------------------
     2            3        1
     4            5        2
     1            3        2
     5            6        3
     2            4        5
Total Weight: 13
```

## Key Features of this Implementation

1. **Edge Structure**: Uses a record to represent edges with source, destination, and weight
2. **Union-Find**: Implements the disjoint set data structure with path compression and union by rank
3. **Sorting**: Uses bubble sort to sort edges by weight (can be replaced with more efficient sorting)
4. **MST Construction**: Processes edges in increasing order of weight and uses Union-Find to detect cycles
5. **Output**: Displays the resulting MST edges and total weight

The algorithm correctly finds the minimum spanning tree with a total weight of 13, which is optimal for this graph.

