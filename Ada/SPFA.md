# SPFA Algorithm in Ada

Here's an implementation of the Shortest Path Faster Algorithm (SPFA) in Ada:

```ada
with Ada.Containers.Vectors;
with Ada.Text_IO;

procedure SPFA_Example is
   
   -- Define a node structure for the graph
   type Edge is record
      Target : Integer;
      Weight : Integer;
   end record;
   
   -- Vector to store edges for each node
   package Edge_Vector is new Ada.Containers.Vectors (Index_Type => Natural, 
                                                      Element_Type => Edge);
   
   -- Graph representation
   type Graph is array (Integer range <>) of Edge_Vector.Vector;
   
   -- SPFA algorithm implementation
   procedure SPFA(Graph_Data : in out Graph;
                  Start_Node : Integer;
                  Distances  : out array (Integer range <>) of Integer;
                  Num_Nodes  : Integer) is
      
      -- Queue for BFS-like processing
      Queue : array (1..Num_Nodes) of Integer;
      Head, Tail : Integer := 0;
      
      -- Boolean array to track if node is in queue
      In_Queue : array (Integer range <>) of Boolean := (others => False);
      
      -- Initialize distances
      procedure Initialize_Distances is
      begin
         for I in Distances'Range loop
            if I = Start_Node then
               Distances(I) := 0;
            else
               Distances(I) := Integer'Last;  -- Infinity
            end if;
         end loop;
      end Initialize_Distances;
      
   begin
      -- Initialize distances
      Initialize_Distances;
      
      -- Add starting node to queue
      Head := Head + 1;
      Queue(Head) := Start_Node;
      In_Queue(Start_Node) := True;
      
      -- Process nodes in queue
      while Head > Tail loop
         declare
            Current_Node : constant Integer := Queue(Tail + 1);
         begin
            Tail := Tail + 1;
            In_Queue(Current_Node) := False;
            
            -- Check all neighbors
            for I in 0..Graph_Data(Current_Node).Length - 1 loop
               declare
                  Neighbor : constant Edge := Graph_Data(Current_Node).Element(I);
                  New_Distance : constant Integer := Distances(Current_Node) + Neighbor.Weight;
               begin
                  -- If we found a shorter path
                  if New_Distance < Distances(Neighbor.Target) then
                     Distances(Neighbor.Target) := New_Distance;
                     
                     -- Add neighbor to queue if not already there
                     if not In_Queue(Neighbor.Target) then
                        Head := Head + 1;
                        Queue(Head) := Neighbor.Target;
                        In_Queue(Neighbor.Target) := True;
                     end if;
                  end if;
               end;
            end loop;
         end;
      end loop;
   end SPFA;
   
   -- Example usage
   procedure Test_SPFA is
      -- Create a sample graph (1-indexed)
      -- Node 1: connected to node 2 (weight 4), node 3 (weight 2)
      -- Node 2: connected to node 3 (weight 1), node 4 (weight 5)
      -- Node 3: connected to node 4 (weight 8), node 5 (weight 10)
      -- Node 4: connected to node 5 (weight 2)
      -- Node 5: no outgoing edges
      
      Num_Nodes : constant Integer := 5;
      Graph_Data : Graph (1..Num_Nodes);
      Distances : array (1..Num_Nodes) of Integer;
      
   begin
      -- Add edges to graph
      Graph_Data(1).Append((Target => 2, Weight => 4));
      Graph_Data(1).Append((Target => 3, Weight => 2));
      
      Graph_Data(2).Append((Target => 3, Weight => 1));
      Graph_Data(2).Append((Target => 4, Weight => 5));
      
      Graph_Data(3).Append((Target => 4, Weight => 8));
      Graph_Data(3).Append((Target => 5, Weight => 10));
      
      Graph_Data(4).Append((Target => 5, Weight => 2));
      
      -- Run SPFA from node 1
      SPFA(Graph_Data, 1, Distances, Num_Nodes);
      
      -- Print results
      Ada.Text_IO.Put_Line("Shortest distances from node 1:");
      for I in 1..Num_Nodes loop
         Ada.Text_IO.Put("Node " & Integer'Image(I) & ": ");
         if Distances(I) = Integer'Last then
            Ada.Text_IO.Put_Line("INFINITE");
         else
            Ada.Text_IO.Put_Line(Integer'Image(Distances(I)));
         end if;
      end loop;
   end Test_SPFA;

begin
   Test_SPFA;
end SPFA_Example;
```

## Key Features of this Implementation:

1. **Graph Representation**: Uses an adjacency list with vectors to store edges
2. **Queue Management**: Implements a simple queue for the BFS-like processing
3. **In-Queue Tracking**: Boolean array to avoid duplicate entries in queue
4. **Distance Updates**: Updates shortest distances and re-queues nodes when improvements are found
5. **Error Handling**: Uses `Integer'Last` to represent infinity

## Expected Output:
```
Shortest distances from node 1:
Node  1:  0
Node  2:  4
Node  3:  2
Node  4:  9
Node  5:  11
```

This implementation demonstrates the core SPFA algorithm which is an optimized version of Bellman-Ford that uses a queue to improve performance in practice, especially for sparse graphs.

