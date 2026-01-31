# Push-Relabel Max-Flow Algorithm in Ada

Here's an implementation of the Push-Relabel max-flow algorithm in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Push_Relabel_Max_Flow is
   
   -- Constants
   Max_Vertices : constant := 100;
   INF          : constant := 1000000;
   
   -- Graph representation
   type Flow_Type is range 0 .. 1000000;
   type Vertex_Type is range 1 .. Max_Vertices;
   
   type Edge_Array is array (Vertex_Type range <>) of Flow_Type;
   type Flow_Matrix is array (Vertex_Type range <>, Vertex_Type range <>) of Flow_Type;
   
   -- Graph data
   Capacity : Flow_Matrix (1 .. Max_Vertices, 1 .. Max_Vertices) := (others => (others => 0));
   Flow     : Flow_Matrix (1 .. Max_Vertices, 1 .. Max_Vertices) := (others => (others => 0));
   Height   : array (Vertex_Type) of Natural;
   Excess   : array (Vertex_Type) of Flow_Type;
   Active   : array (Vertex_Type) of Boolean;
   
   -- Number of vertices and edges
   Num_Vertices : Natural;
   Num_Edges    : Natural;
   
   -- Function to find minimum of two values
   function Min(A, B : Flow_Type) return Flow_Type is
   begin
      if A < B then
         return A;
      else
         return B;
      end if;
   end Min;
   
   -- Push operation
   procedure Push(U, V : Vertex_Type) is
      Amount : Flow_Type;
   begin
      Amount := Min(Excess(U), Capacity(U, V) - Flow(U, V));
      
      if Amount > 0 then
         Flow(U, V) := Flow(U, V) + Amount;
         Flow(V, U) := Flow(V, U) - Amount;
         Excess(U) := Excess(U) - Amount;
         Excess(V) := Excess(V) + Amount;
      end if;
   end Push;
   
   -- Relabel operation
   procedure Relabel(U : Vertex_Type) is
      Min_Height : Natural := Integer'Last;
   begin
      for V in 1 .. Num_Vertices loop
         if Capacity(U, V) > Flow(U, V) then
            Min_Height := Natural'Min(Min_Height, Height(V));
         end if;
      end loop;
      
      if Min_Height < Integer'Last then
         Height(U) := Min_Height + 1;
      end if;
   end Relabel;
   
   -- Discharge operation
   procedure Discharge(U : Vertex_Type) is
      V : Vertex_Type := 1;
   begin
      while Excess(U) > 0 loop
         if V > Num_Vertices then
            Relabel(U);
            V := 1;
         elsif Capacity(U, V) > Flow(U, V) and Height(U) = Height(V) + 1 then
            Push(U, V);
         else
            V := V + 1;
         end if;
      end loop;
   end Discharge;
   
   -- Main Push-Relabel algorithm
   function Max_Flow(Source, Sink : Vertex_Type) return Flow_Type is
      Max_Flow_Value : Flow_Type := 0;
      Active_Count   : Natural := 0;
   begin
      -- Initialize
      for U in 1 .. Num_Vertices loop
         Height(U) := 0;
         Excess(U) := 0;
         Active(U) := False;
      end loop;
      
      Height(Source) := Num_Vertices;
      Excess(Source) := INF;
      
      -- Initialize edges from source
      for V in 1 .. Num_Vertices loop
         if Capacity(Source, V) > 0 then
            Flow(Source, V) := Capacity(Source, V);
            Flow(V, Source) := -Capacity(Source, V);
            Excess(V) := Capacity(Source, V);
            Active(V) := True;
            Active_Count := Active_Count + 1;
         end if;
      end loop;
      
      -- Main loop
      while Active_Count > 0 loop
         for U in 1 .. Num_Vertices loop
            if Active(U) and Excess(U) > 0 then
               Discharge(U);
               if Excess(U) = 0 then
                  Active(U) := False;
                  Active_Count := Active_Count - 1;
               end if;
            end if;
         end loop;
      end loop;
      
      -- Calculate maximum flow
      for V in 1 .. Num_Vertices loop
         Max_Flow_Value := Max_Flow_Value + Flow(Source, V);
      end loop;
      
      return Max_Flow_Value;
   end Max_Flow;
   
   -- Example usage
begin
   -- Set up example graph
   Num_Vertices := 6;
   
   -- Clear all capacities
   for I in 1 .. Num_Vertices loop
      for J in 1 .. Num_Vertices loop
         Capacity(I, J) := 0;
      end loop;
   end loop;
   
   -- Add edges with capacities
   -- Source = 1, Sink = 6
   Capacity(1, 2) := 10;
   Capacity(1, 4) := 10;
   Capacity(2, 3) := 4;
   Capacity(2, 4) := 2;
   Capacity(2, 5) := 8;
   Capacity(3, 6) := 10;
   Capacity(4, 3) := 6;
   Capacity(4, 5) := 9;
   Capacity(5, 6) := 10;
   
   -- Compute maximum flow
   declare
      Result : Flow_Type;
   begin
      Result := Max_Flow(1, 6);
      Put("Maximum flow from vertex 1 to vertex 6: ");
      Put(Result);
      New_Line;
   end;
   
end Push_Relabel_Max_Flow;
```

## Algorithm Explanation

This implementation demonstrates the Push-Relabel max-flow algorithm with the following key components:

1. **Data Structures**:
   - `Capacity` matrix to store edge capacities
   - `Flow` matrix to store current flow values
   - `Height` array for height labels
   - `Excess` array for excess flow at each vertex
   - `Active` array to track active vertices

2. **Key Operations**:
   - **Push**: Moves flow from a vertex to its neighbor when possible
   - **Relabel**: Increases the height of a vertex when no push is possible
   - **Discharge**: Repeatedly pushes or relabels until vertex becomes inactive

3. **Algorithm Steps**:
   - Initialize heights and excess flows
   - Process active vertices until no active vertices remain
   - Return the total flow from source to sink

## Example Graph

The example uses a graph with 6 vertices (1-6) where:
- Source vertex: 1
- Sink vertex: 6
- Edges with capacities: 1→2(10), 1→4(10), 2→3(4), 2→4(2), 2→5(8), 3→6(10), 4→3(6), 4→5(9), 5→6(10)

The maximum flow from vertex 1 to vertex 6 should be computed as 19.

