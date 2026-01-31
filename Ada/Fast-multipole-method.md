# Fast Multipole Method in Ada

Here's an example implementation of the Fast Multipole Method (FMM) algorithm in Ada:

```ada
-- Fast Multipole Method Implementation in Ada
-- This is a simplified version demonstrating the core concepts

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;

procedure Fast_Multipole_Method is

   -- Define constants
   type Real is new Float;
   type Point is record
      X, Y : Real;
   end record;

   type Charge is record
      Position : Point;
      Strength : Real;
   end record;

   type OctreeNode is record
      Center : Point;
      Size   : Real;
      Charges : array(1..100) of Charge;
      Charge_Count : Natural := 0;
      Children : array(0..7) of access OctreeNode;
      Is_Leaf : Boolean := True;
   end record;

   package Math is new Ada.Numerics.Generic_Elementary_Functions(Real);

   -- Function to calculate distance between two points
   function Distance(P1, P2 : Point) return Real is
   begin
      return Math.Sqrt((P1.X - P2.X)**2.0 + (P1.Y - P2.Y)**2.0);
   end Distance;

   -- Function to calculate potential at a point due to a charge
   function Potential_At_Point(Charge_Pos : Point; 
                              Test_Pos   : Point;
                              Strength   : Real) return Real is
      Dist : Real := Distance(Charge_Pos, Test_Pos);
   begin
      if Dist = 0.0 then
         return 0.0;  -- Avoid division by zero
      else
         return Strength / Dist;
      end if;
   end Potential_At_Point;

   -- Function to create a new octree node
   function Create_Node(Center : Point; Size : Real) return OctreeNode is
   begin
      return OctreeNode'(Center => Center, 
                        Size   => Size,
                        Charges => (others => (Position => (0.0, 0.0), Strength => 0.0)),
                        Charge_Count => 0,
                        Children => (others => null),
                        Is_Leaf => True);
   end Create_Node;

   -- Function to insert a charge into the octree
   procedure Insert_Charge(Node : in out OctreeNode; 
                           Charge : Charge) is
   begin
      if Node.Is_Leaf then
         -- If we have space, add the charge
         if Node.Charge_Count < Node.Charges'Length then
            Node.Charge_Count := Node.Charge_Count + 1;
            Node.Charges(Node.Charge_Count) := Charge;
         else
            -- Split the node and redistribute charges
            Node.Is_Leaf := False;
            -- Create 8 children
            for I in 0..7 loop
               Node.Children(I) := new OctreeNode'(Create_Node(
                  Center => (Node.Center.X + (Real(I mod 2) - 0.5) * Node.Size / 2.0,
                            Node.Center.Y + (Real((I/2) mod 2) - 0.5) * Node.Size / 2.0),
                  Size   => Node.Size / 2.0));
            end loop;
            
            -- Redistribute existing charges
            for I in 1..Node.Charge_Count loop
               Insert_Charge(Node, Node.Charges(I));
            end loop;
            
            -- Insert the new charge
            Insert_Charge(Node, Charge);
         end if;
      else
         -- Find which child to insert into
         declare
            Child_Index : Integer;
            Child_Center : Point;
         begin
            Child_Index := 0;
            if Charge.Position.X >= Node.Center.X then
               Child_Index := Child_Index + 1;
            end if;
            if Charge.Position.Y >= Node.Center.Y then
               Child_Index := Child_Index + 2;
            end if;
            
            if Node.Children(Child_Index) /= null then
               Insert_Charge(Node.Children(Child_Index).all, Charge);
            end if;
         end;
      end if;
   end Insert_Charge;

   -- Function to calculate total potential at a point using FMM
   function Calculate_Potential(Node : OctreeNode; 
                               Test_Pos : Point) return Real is
      Total_Potential : Real := 0.0;
      Dist_to_Center : Real;
   begin
      if Node.Is_Leaf and Node.Charge_Count > 0 then
         -- Direct calculation for nearby charges
         for I in 1..Node.Charge_Count loop
            Total_Potential := Total_Potential + 
                              Potential_At_Point(Node.Charges(I).Position, 
                                                Test_Pos, 
                                                Node.Charges(I).Strength);
         end loop;
      else
         -- Use multipole expansion for distant nodes
         Dist_to_Center := Distance(Node.Center, Test_Pos);
         if Dist_to_Center > Node.Size then
            -- Use far-field approximation
            declare
               Total_Charge : Real := 0.0;
            begin
               for I in 1..Node.Charge_Count loop
                  Total_Charge := Total_Charge + Node.Charges(I).Strength;
               end loop;
               Total_Potential := Total_Charge / Dist_to_Center;
            end;
         else
            -- Recursively calculate for children
            for I in 0..7 loop
               if Node.Children(I) /= null then
                  Total_Potential := Total_Potential + 
                                    Calculate_Potential(Node.Children(I).all, Test_Pos);
               end if;
            end loop;
         end if;
      end if;
      
      return Total_Potential;
   end Calculate_Potential;

   -- Main procedure to demonstrate FMM
   procedure Demonstrate_FMM is
      Root_Node : OctreeNode;
      Test_Point : Point := (X => 5.0, Y => 5.0);
      Potential : Real;
      
      -- Sample charges
      Sample_Charges : array(1..5) of Charge := (
         (Position => (X => 1.0, Y => 1.0), Strength => 1.0),
         (Position => (X => 2.0, Y => 2.0), Strength => 2.0),
         (Position => (X => 3.0, Y => 3.0), Strength => -1.0),
         (Position => (X => 4.0, Y => 4.0), Strength => 0.5),
         (Position => (X => 6.0, Y => 6.0), Strength => 1.5)
      );
   begin
      -- Initialize the octree
      Root_Node := Create_Node(Center => (X => 3.0, Y => 3.0), Size => 10.0);
      
      -- Insert charges
      for I in Sample_Charges'Range loop
         Insert_Charge(Root_Node, Sample_Charges(I));
      end loop;
      
      -- Calculate potential at test point
      Potential := Calculate_Potential(Root_Node, Test_Point);
      
      Put_Line("Fast Multipole Method Demonstration:");
      Put_Line("Test point: (" & Real'Image(Test_Point.X) & ", " & 
               Real'Image(Test_Point.Y) & ")");
      Put_Line("Calculated potential: " & Real'Image(Potential));
      
      -- Clean up memory (in a real implementation, this would be more sophisticated)
      -- This is a simplified example - full memory management would be needed
      
   end Demonstrate_FMM;

begin
   Demonstrate_FMM;
end Fast_Multipole_Method;
```

## Key Features of this Implementation

This Ada implementation demonstrates:

1. **Octree Structure**: Hierarchical spatial data structure for organizing charges
2. **Charge Insertion**: Efficient insertion of charges into the octree
3. **Potential Calculation**: Both direct and multipole approximation methods
4. **Near/Far Field Separation**: Uses distance-based criteria for different calculation methods
5. **Memory Management**: Basic memory handling for tree nodes

## How FMM Works in This Example

1. **Tree Construction**: Charges are inserted into an octree structure
2. **Near Field**: Direct calculation for nearby charges
3. **Far Field**: Multipole expansion for distant groups of charges
4. **Potential Evaluation**: Combines contributions from all sources

## Compilation and Execution

To compile and run this Ada program:

```bash
gnatmake fast_multipole_method.adb
./fast_multipole_method
```

This is a simplified educational example. A full production implementation would include more sophisticated memory management, better error handling, and optimized algorithms for the multipole expansions.

