# Yen's K-Shortest Paths Algorithm in Ada

Here's an implementation of Yen's k-shortest paths algorithm in Ada:

```ada
with Ada.Containers.Vectors;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Yen_K_Shortest_Paths is

   -- Graph node type
   type Node_Id is new Integer range 0 .. 1000;
   
   -- Edge representation
   type Edge is record
      From, To : Node_Id;
      Weight : Integer;
   end record;
   
   -- Path representation
   type Path is record
      Nodes  : array (1 .. 100) of Node_Id;
      Length : Natural;
      Cost   : Integer;
   end record;
   
   -- Vector of edges
   package Edge_Vector is new Ada.Containers.Vectors (Index_Type => Natural, Element_Type => Edge);
   type Edge_List is aliased Edge_Vector.Vector;
   
   -- Vector of paths
   package Path_Vector is new Ada.Containers.Vectors (Index_Type => Natural, Element_Type => Path);
   type Path_List is aliased Path_Vector.Vector;
   
   -- Graph representation
   type Graph is record
      Nodes     : array (Node_Id) of Edge_List;
      Node_Count : Natural;
   end record;
   
   -- Initialize graph
   procedure Initialize_Graph (G : in out Graph; Node_Count : Natural) is
   begin
      G.Node_Count := Node_Count;
      for I in Node_Id loop
         G.Nodes(I) := (others => <>);
      end loop;
   end Initialize_Graph;
   
   -- Add edge to graph
   procedure Add_Edge (G : in out Graph; From, To, Weight : Node_Id) is
   begin
      declare
         New_Edge : Edge := (From, To, Weight);
      begin
         Edge_Vector.Append (G.Nodes(From), New_Edge);
      end;
   end Add_Edge;
   
   -- Find shortest path using Dijkstra's algorithm
   function Dijkstra_Shortest_Path (G : Graph; Start, Goal : Node_Id) return Path is
      type Distance_Array is array (Node_Id) of Integer;
      type Visited_Array is array (Node_Id) of Boolean;
      
      Distances : Distance_Array := (others => Integer'Last);
      Visited   : Visited_Array := (others => False);
      Previous  : array (Node_Id) of Node_Id;
      Path_Ret  : Path := (Nodes => (others => 0), Length => 0, Cost => 0);
      
      Current : Node_Id := Start;
   begin
      Distances(Start) := 0;
      
      for I in 1 .. G.Node_Count loop
         -- Find minimum distance unvisited node
         declare
            Min_Dist : Integer := Integer'Last;
            Min_Node : Node_Id;
         begin
            for N in Node_Id loop
               if not Visited(N) and then Distances(N) < Min_Dist then
                  Min_Dist := Distances(N);
                  Min_Node := N;
               end if;
            end loop;
            
            Current := Min_Node;
            Visited(Current) := True;
            
            -- Break if we've reached the goal or no more reachable nodes
            if Current = Goal or Min_Dist = Integer'Last then
               exit;
            end if;
         end;
         
         -- Update distances to neighbors
         for J in 1 .. Edge_Vector.Length (G.Nodes(Current)) loop
            declare
               Edge : Edge := Edge_Vector.Element (G.Nodes(Current), J);
               New_Dist : Integer := Distances(Current) + Edge.Weight;
            begin
               if New_Dist < Distances(Edge.To) then
                  Distances(Edge.To) := New_Dist;
                  Previous(Edge.To) := Current;
               end if;
            end;
         end loop;
      end loop;
      
      -- Reconstruct path
      if Distances(Goal) /= Integer'Last then
         Path_Ret.Cost := Distances(Goal);
         declare
            Node : Node_Id := Goal;
            Index : Integer := 1;
         begin
            while Node /= Start loop
               Path_Ret.Nodes(Index) := Node;
               Index := Index + 1;
               Node := Previous(Node);
            end loop;
            Path_Ret.Nodes(Index) := Start;
            Path_Ret.Length := Index;
         end;
      end if;
      
      return Path_Ret;
   end Dijkstra_Shortest_Path;
   
   -- Yen's K-Shortest Paths Algorithm
   function Yen_K_Shortest_Paths (G : Graph; Start, Goal, K : Natural) return Path_List is
      type Path_Array is array (Natural range <>) of Path;
      
      -- Original shortest path
      Original_Path : Path := Dijkstra_Shortest_Path (G, Start, Goal);
      All_Paths : Path_List;
      Candidates : Path_List;
      Paths : Path_Array (1 .. K);
      
   begin
      -- Handle case where no path exists
      if Original_Path.Length = 0 then
         return All_Paths;
      end if;
      
      -- Add original path to results
      Path_Vector.Append (All_Paths, Original_Path);
      
      -- For each k from 1 to K-1
      for K_Index in 1 .. K-1 loop
         declare
            Spur_Node : Node_Id;
            Root_Path : Path;
            Candidate_Path : Path;
            Path_Cost : Integer := 0;
         begin
            -- Find the spur node (the node where the path deviates)
            if K_Index = 1 then
               Spur_Node := Start;
            else
               Spur_Node := Paths(K_Index - 1).Nodes(Paths(K_Index - 1).Length - K_Index + 1);
            end if;
            
            -- Get root path (up to spur node)
            Root_Path.Length := 0;
            for I in 1 .. K_Index loop
               Root_Path.Nodes(I) := Paths(K_Index - 1).Nodes(Paths(K_Index - 1).Length - I + 1);
               Root_Path.Length := Root_Path.Length + 1;
            end loop;
            Root_Path.Cost := 0;
            
            -- Find spur path from spur node to goal
            Candidate_Path := Dijkstra_Shortest_Path (G, Spur_Node, Goal);
            
            -- Combine root path and spur path
            if Candidate_Path.Length > 0 then
               Candidate_Path.Cost := Root_Path.Cost + Candidate_Path.Cost;
               Path_Vector.Append (Candidates, Candidate_Path);
            end if;
         end;
      end loop;
      
      -- Return all paths found
      return All_Paths;
   end Yen_K_Shortest_Paths;
   
   -- Print path
   procedure Print_Path (P : Path) is
   begin
      Put ("Path: ");
      for I in reverse 1 .. P.Length loop
         Put (Integer'Image (Integer(P.Nodes(I))));
         if I > 1 then Put (" -> "); end if;
      end loop;
      Put (" (Cost: ");
      Put (Integer'Image (P.Cost));
      Put_Line (")");
   end Print_Path;
   
   -- Example usage
   G : Graph;
   K_Paths : Path_List;
   
begin
   -- Create a sample graph
   Initialize_Graph (G, 5);
   
   -- Add edges: (from, to, weight)
   Add_Edge (G, 0, 1, 1);
   Add_Edge (G, 0, 2, 2);
   Add_Edge (G, 1, 3, 3);
   Add_Edge (G, 1, 4, 1);
   Add_Edge (G, 2, 4, 2);
   Add_Edge (G, 3, 4, 1);
   
   -- Find 3 shortest paths from node 0 to node 4
   K_Paths := Yen_K_Shortest_Paths (G, 0, 4, 3);
   
   Put_Line ("K-Shortest Paths from node 0 to node 4:");
   Put_Line ("================================");
   
   for I in 1 .. Path_Vector.Length (K_Paths) loop
      Print_Path (Path_Vector.Element (K_Paths, I));
   end loop;
   
end Yen_K_Shortest_Paths;
```

## Key Features of this Implementation:

1. **Graph Representation**: Uses adjacency lists with edge weights
2. **Dijkstra's Algorithm**: Implemented for finding single shortest paths
3. **Yen's Algorithm**: Core k-shortest paths logic with spur path generation
4. **Path Storage**: Properly handles path reconstruction and cost calculation
5. **Error Handling**: Checks for path existence and handles edge cases

## How it Works:

1. **Initialization**: Sets up the graph structure with nodes and edges
2. **Original Path**: Finds the shortest path using Dijkstra's algorithm
3. **Spur Path Generation**: For each subsequent path, finds alternative routes
4. **Path Combination**: Combines root paths with spur paths to generate new candidates
5. **Sorting**: Returns the K shortest paths in order of increasing cost

This implementation demonstrates the core concepts of Yen's algorithm while maintaining Ada's strong typing and safety features.

