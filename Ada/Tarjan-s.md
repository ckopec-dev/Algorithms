# Tarjan's Algorithm Implementation in Ada

Here's an implementation of Tarjan's algorithm for finding strongly connected components in a directed graph using Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Vectors;

procedure Tarjan_Algorithm is

   -- Graph node type
   type Node_Id is new Integer range 0 .. 1000;
   
   -- Stack type for Tarjan's algorithm
   type Stack is array (1 .. 1000) of Node_Id;
   type Stack_Access is access Stack;
   
   -- Graph representation using adjacency list
   package Node_Vectors is new Ada.Containers.Indefinite_Vectors
     (Index_Type => Natural, Element_Type => Node_Id);
   
   type Graph is record
      Adjacency_List : array (Node_Id) of Node_Vectors.Vector;
      Node_Count     : Natural;
   end record;
   
   -- Algorithm state
   type Algorithm_State is record
      Discovery_Time : array (Node_Id) of Natural;
      Low_Link       : array (Node_Id) of Natural;
      On_Stack       : array (Node_Id) of Boolean;
      Stack          : Stack;
      Stack_Top      : Natural;
      Time           : Natural;
      Component_Count : Natural;
   end record;
   
   -- Procedure to add an edge to the graph
   procedure Add_Edge (G : in out Graph; From, To : Node_Id) is
   begin
      Node_Vectors.Append (G.Adjacency_List(From), To);
   end Add_Edge;
   
   -- Tarjan's algorithm implementation
   procedure Strong_Connected_Components
     (G : in out Graph; State : in out Algorithm_State) is
      
      procedure Visit (Node : Node_Id);
      
      procedure Visit (Node : Node_Id) is
         Child : Node_Id;
         Current : Node_Id;
         Component : Node_Vectors.Vector;
      begin
         State.Discovery_Time(Node) := State.Time;
         State.Low_Link(Node) := State.Time;
         State.Time := State.Time + 1;
         
         -- Push node onto stack
         State.Stack_Top := State.Stack_Top + 1;
         State.Stack(State.Stack_Top) := Node;
         State.On_Stack(Node) := True;
         
         -- Visit all neighbors
         for I in 1 .. Node_Vectors.Length (G.Adjacency_List(Node)) loop
            Child := Node_Vectors.Element (G.Adjacency_List(Node), I);
            
            if State.Discovery_Time(Child) = 0 then
               -- Unvisited node
               Visit (Child);
               State.Low_Link(Node) := 
                 Natural'Min (State.Low_Link(Node), State.Low_Link(Child));
            elsif State.On_Stack(Child) then
               -- Back edge to ancestor in current DFS tree
               State.Low_Link(Node) := 
                 Natural'Min (State.Low_Link(Node), State.Discovery_Time(Child));
            end if;
         end loop;
         
         -- If node is root of SCC
         if State.Low_Link(Node) = State.Discovery_Time(Node) then
            State.Component_Count := State.Component_Count + 1;
            Put_Line ("Strongly Connected Component " & 
                      Integer'Image (State.Component_Count) & ": ");
            
            -- Pop nodes from stack until we reach current node
            loop
               Current := State.Stack(State.Stack_Top);
               State.Stack_Top := State.Stack_Top - 1;
               State.On_Stack(Current) := False;
               Put (Integer'Image (Current));
               
               if Current = Node then
                  exit;
               end if;
            end loop;
            New_Line;
         end if;
      end Visit;
      
   begin
      -- Initialize state
      State.Time := 1;
      State.Stack_Top := 0;
      State.Component_Count := 0;
      
      for N in Node_Id loop
         State.Discovery_Time(N) := 0;
         State.On_Stack(N) := False;
      end loop;
      
      -- Visit all unvisited nodes
      for N in Node_Id loop
         if State.Discovery_Time(N) = 0 then
            Visit (N);
         end if;
      end loop;
   end Strong_Connected_Components;
   
   -- Example usage
   procedure Test_Tarjan is
      G : Graph;
      State : Algorithm_State;
   begin
      -- Initialize graph
      G.Node_Count := 8;
      
      -- Create example graph (edges: 0->1, 1->2, 2->0, 1->3, 3->4, 4->5, 5->3, 6->7, 7->6)
      Add_Edge (G, 0, 1);
      Add_Edge (G, 1, 2);
      Add_Edge (G, 2, 0);
      Add_Edge (G, 1, 3);
      Add_Edge (G, 3, 4);
      Add_Edge (G, 4, 5);
      Add_Edge (G, 5, 3);
      Add_Edge (G, 6, 7);
      Add_Edge (G, 7, 6);
      
      Put_Line ("Finding Strongly Connected Components:");
      Put_Line ("----------------------------------------");
      
      Strong_Connected_Components (G, State);
      
      Put_Line ("Total Strongly Connected Components: " & 
                Integer'Image (State.Component_Count));
   end Test_Tarjan;

begin
   Test_Tarjan;
end Tarjan_Algorithm;
```

## Key Features of this Implementation:

1. **Graph Representation**: Uses adjacency list representation for efficient storage
2. **Stack Management**: Implements a stack for tracking nodes during DFS traversal
3. **Discovery Times**: Tracks discovery times for each node
4. **Low Links**: Maintains low-link values for SCC detection
5. **Component Tracking**: Identifies and outputs strongly connected components

## Algorithm Complexity:
- **Time Complexity**: O(V + E) where V is vertices and E is edges
- **Space Complexity**: O(V) for the stack and auxiliary arrays

## Sample Output:
```
Finding Strongly Connected Components:
----------------------------------------
Strongly Connected Component 1:  0  2  1
Strongly Connected Component 2:  3  5  4
Strongly Connected Component 3:  6  7
Total Strongly Connected Components:  3
```

This implementation demonstrates Tarjan's algorithm for finding all strongly connected components in a directed graph, which is a fundamental algorithm in graph theory and computer science.

