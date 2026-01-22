# Bron-Kerbosch Algorithm in Ada

Here's an implementation of the Bron-Kerbosch algorithm for finding maximal cliques in a graph using Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Sets;

procedure Bron_Kerbosch is

   -- Define a simple graph representation using adjacency lists
   type Vertex is range 1 .. 100;
   type Vertex_Set is tagged limited record
      Vertices : Ada.Containers.Ordered_Sets.Ordered_Set(Vertex);
   end record;

   -- Vector of vertices
   package Vertex_Vector is new Ada.Containers.Vectors(Vertex, Vertex);
   type Vertex_Vector_Access is access all Vertex_Vector.Vector;

   -- Graph representation
   type Graph is record
      Adjacency_List : array(Vertex range <>) of Vertex_Set;
      Vertex_Count   : Natural;
   end record;

   -- Function to add an edge to the graph
   procedure Add_Edge(G : in out Graph; V1, V2 : Vertex) is
   begin
      G.Adjacency_List(V1).Vertices.Include(V2);
      G.Adjacency_List(V2).Vertices.Include(V1);
   end Add_Edge;

   -- Function to check if a vertex is in a set
   function Is_In_Set(Set : Vertex_Set; V : Vertex) return Boolean is
   begin
      return Set.Vertices.Contains(V);
   end Is_In_Set;

   -- Function to get intersection of two vertex sets
   function Intersection(Set1, Set2 : Vertex_Set) return Vertex_Set is
      Result : Vertex_Set;
   begin
      for V in Set1.Vertices.Iterate loop
         if Is_In_Set(Set2, Set1.Vertices.Element(V)) then
            Result.Vertices.Include(Set1.Vertices.Element(V));
         end if;
      end loop;
      return Result;
   end Intersection;

   -- Function to get difference of two vertex sets
   function Difference(Set1, Set2 : Vertex_Set) return Vertex_Set is
      Result : Vertex_Set;
   begin
      for V in Set1.Vertices.Iterate loop
         if not Is_In_Set(Set2, Set1.Vertices.Element(V)) then
            Result.Vertices.Include(Set1.Vertices.Element(V));
         end if;
      end loop;
      return Result;
   end Difference;

   -- Bron-Kerbosch algorithm with pivoting
   procedure Bron_Kerbosch(R, P, X : Vertex_Set; Max_Cliques : in out Vertex_Vector.Vector) is
      P_Iterator : Ada.Containers.Ordered_Sets.Cursor;
      V : Vertex;
      R_New, P_New, X_New : Vertex_Set;
   begin
      -- If P and X are both empty, R is a maximal clique
      if P.Vertices.Is_Empty and then X.Vertices.Is_Empty then
         -- Add current clique to results
         Max_Cliques.Append(R.Vertices.To_Vector);
         return;
      end if;

      -- Choose pivot vertex u from P union X
      -- For simplicity, we'll use the first vertex in P
      if not P.Vertices.Is_Empty then
         P_Iterator := P.Vertices.First;
         V := P.Vertices.Element(P_Iterator);
      else
         P_Iterator := X.Vertices.First;
         V := X.Vertices.Element(P_Iterator);
      end if;

      -- For each vertex u in P - N(v) do
      declare
         P_Minus_N : Vertex_Set;
         P_Iter : Ada.Containers.Ordered_Sets.Cursor;
      begin
         -- Calculate P - N(v) where N(v) is neighbors of v
         P_Minus_N := Difference(P, G.Adjacency_List(V));
         
         P_Iter := P_Minus_N.Vertices.First;
         while Ada.Containers.Ordered_Sets.Has_Element(P_Iter) loop
            declare
               U : Vertex := P_Minus_N.Vertices.Element(P_Iter);
            begin
               -- Add u to R
               R_New := R;
               R_New.Vertices.Include(U);
               
               -- Calculate P intersect N(u)
               P_New := Intersection(P, G.Adjacency_List(U));
               
               -- Calculate X intersect N(u)
               X_New := Intersection(X, G.Adjacency_List(U));
               
               -- Recursively call Bron-Kerbosch
               Bron_Kerbosch(R_New, P_New, X_New, Max_Cliques);
               
               -- Remove u from P and add to X
               P.Vertices.Exclude(U);
               X.Vertices.Include(U);
            end;
            
            P_Iter := P_Minus_N.Vertices.Next(P_Iter);
         end loop;
      end;
   end Bron_Kerbosch;

   -- Function to print a clique
   procedure Print_Clique(Clique : Vertex_Vector.Vector) is
   begin
      Put("Clique: ");
      for I in Clique.First_Index .. Clique.Last_Index loop
         Put(Clique.Element(I), Width => 3);
      end loop;
      New_Line;
   end Print_Clique;

   -- Main program
   G : Graph(1 .. 6);
   Max_Cliques : Vertex_Vector.Vector;
   R : Vertex_Set;
   P : Vertex_Set;
   X : Vertex_Set;

begin
   -- Initialize graph with 6 vertices
   G.Vertex_Count := 6;
   
   -- Add edges to create a sample graph
   -- This creates a graph with edges: 1-2, 1-3, 2-3, 2-4, 3-4, 4-5, 4-6, 5-6
   Add_Edge(G, 1, 2);
   Add_Edge(G, 1, 3);
   Add_Edge(G, 2, 3);
   Add_Edge(G, 2, 4);
   Add_Edge(G, 3, 4);
   Add_Edge(G, 4, 5);
   Add_Edge(G, 4, 6);
   Add_Edge(G, 5, 6);

   -- Initialize P with all vertices
   for I in 1 .. G.Vertex_Count loop
      P.Vertices.Include(Vertex(I));
   end loop;

   -- Call Bron-Kerbosch algorithm
   Bron_Kerbosch(R, P, X, Max_Cliques);

   -- Print all maximal cliques
   Put_Line("Maximal cliques found:");
   for I in Max_Cliques.First_Index .. Max_Cliques.Last_Index loop
      Print_Clique(Max_Cliques.Element(I));
   end loop;

end Bron_Kerbosch;
```

## Explanation

This Ada implementation of the Bron-Kerbosch algorithm includes:

1. **Graph Representation**: Uses adjacency lists with `Vertex_Set` to represent neighbors
2. **Core Algorithm**: Implements the recursive Bron-Kerbosch with pivoting
3. **Helper Functions**:
   - `Add_Edge`: Adds edges to the graph
   - `Is_In_Set`: Checks if a vertex is in a set
   - `Intersection`: Finds intersection of two vertex sets
   - `Difference`: Finds difference of two vertex sets

## Key Features

- **Type Safety**: Uses Ada's strong typing system
- **Memory Management**: Uses Ada's built-in containers for automatic memory management
- **Modularity**: Separate procedures for different functionality
- **Error Handling**: Uses Ada's built-in safety features

## Sample Output

For the example graph with edges 1-2, 1-3, 2-3, 2-4, 3-4, 4-5, 4-6, 5-6, the algorithm would find maximal cliques like:
- Clique:   1   2   3
- Clique:   2   3   4
- Clique:   4   5   6

The algorithm efficiently finds all maximal cliques in the graph using the Bron-Kerbosch recursive approach with pivoting optimization.

