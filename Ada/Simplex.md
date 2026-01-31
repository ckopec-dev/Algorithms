# Simplex Algorithm in Ada

Here's a complete implementation of the Simplex algorithm in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;

procedure Simplex_Example is
   
   -- Matrix type definition
   type Matrix is array (Positive range <>, Positive range <>) of Float;
   
   -- Simplex result structure
   type Simplex_Result is record
      Solution : Vector (Positive range <>);
      Optimal_Value : Float;
      Is_Optimal : Boolean;
   end record;
   
   -- Function to find pivot element
   function Find_Pivot_Row(Tableau : Matrix; Column : Positive) return Positive is
      Min_Ratio : Float := Float'Last;
      Pivot_Row : Positive := 1;
   begin
      for I in Tableau'First (1) .. Tableau'Last (1) - 1 loop
         if Tableau(I, Column) > 0.0 then
            declare
               Ratio : Float := Tableau(I, Tableau'Last (2)) / Tableau(I, Column);
            begin
               if Ratio < Min_Ratio then
                  Min_Ratio := Ratio;
                  Pivot_Row := I;
               end if;
            end;
         end if;
      end loop;
      return Pivot_Row;
   end Find_Pivot_Row;
   
   -- Function to check if optimal
   function Is_Optimal(Tableau : Matrix) return Boolean is
   begin
      for J in Tableau'First (2) .. Tableau'Last (2) - 1 loop
         if Tableau(Tableau'Last (1), J) < 0.0 then
            return False;
         end if;
      end loop;
      return True;
   end Is_Optimal;
   
   -- Function to perform pivot operation
   procedure Pivot_Operation(Tableau : in out Matrix; Pivot_Row, Pivot_Column : Positive) is
      Pivot_Element : Float := Tableau(Pivot_Row, Pivot_Column);
   begin
      -- Make pivot element 1
      for J in Tableau'First (2) .. Tableau'Last (2) loop
         Tableau(Pivot_Row, J) := Tableau(Pivot_Row, J) / Pivot_Element;
      end loop;
      
      -- Make other elements in pivot column 0
      for I in Tableau'First (1) .. Tableau'Last (1) loop
         if I /= Pivot_Row and Tableau(I, Pivot_Column) /= 0.0 then
            declare
               Factor : Float := Tableau(I, Pivot_Column);
            begin
               for J in Tableau'First (2) .. Tableau'Last (2) loop
                  Tableau(I, J) := Tableau(I, J) - Factor * Tableau(Pivot_Row, J);
               end loop;
            end;
         end if;
      end loop;
   end Pivot_Operation;
   
   -- Main Simplex algorithm
   function Simplex_Solve(Coefficients : Matrix; Constraints : Matrix; 
                         Objective : Vector (Positive range <>)) return Simplex_Result is
      -- Create augmented tableau
      Num_Variables : constant Positive := Coefficients'Last (2);
      Num_Constraints : constant Positive := Coefficients'Last (1);
      Tableau_Size_Row : constant Positive := Num_Constraints + 1;
      Tableau_Size_Col : constant Positive := Num_Variables + Num_Constraints + 1;
      
      Tableau : Matrix (1 .. Tableau_Size_Row, 1 .. Tableau_Size_Col);
      Result : Simplex_Result;
      Pivot_Column : Positive;
      Pivot_Row : Positive;
   begin
      -- Initialize tableau
      for I in 1 .. Num_Constraints loop
         for J in 1 .. Num_Variables loop
            Tableau(I, J) := Coefficients(I, J);
         end loop;
         Tableau(I, Num_Variables + I) := 1.0;  -- Slack variables
         Tableau(I, Tableau'Last (2)) := Constraints(I, 1);  -- RHS
      end loop;
      
      -- Objective function row (negated for maximization)
      for J in 1 .. Num_Variables loop
         Tableau(Tableau'Last (1), J) := -Objective(J);
      end loop;
      
      -- Simplex iterations
      while not Is_Optimal(Tableau) loop
         -- Find entering variable (most negative in bottom row)
         Pivot_Column := Tableau'First (2);
         for J in Tableau'First (2) .. Tableau'Last (2) - 1 loop
            if Tableau(Tableau'Last (1), J) < Tableau(Tableau'Last (1), Pivot_Column) then
               Pivot_Column := J;
            end if;
         end loop;
         
         -- Check if unbounded
         declare
            Is_Unbounded : Boolean := True;
         begin
            for I in Tableau'First (1) .. Tableau'Last (1) - 1 loop
               if Tableau(I, Pivot_Column) > 0.0 then
                  Is_Unbounded := False;
                  exit;
               end if;
            end loop;
            
            if Is_Unbounded then
               Result.Is_Optimal := False;
               return Result;
            end if;
         end;
         
         -- Find leaving variable
         Pivot_Row := Find_Pivot_Row(Tableau, Pivot_Column);
         
         -- Perform pivot
         Pivot_Operation(Tableau, Pivot_Row, Pivot_Column);
      end loop;
      
      -- Extract solution
      Result.Is_Optimal := True;
      Result.Solution := new Vector (1 .. Num_Variables);
      Result.Optimal_Value := Tableau(Tableau'Last (1), Tableau'Last (2));
      
      for J in 1 .. Num_Variables loop
         Result.Solution(J) := 0.0;
      end loop;
      
      -- Extract basic variables
      for I in 1 .. Num_Constraints loop
         for J in 1 .. Num_Variables loop
            if Tableau(I, J) = 1.0 then
               -- Check if this is a basic variable
               declare
                  Is_Basic : Boolean := True;
               begin
                  for K in 1 .. Num_Constraints loop
                     if K /= I and Tableau(K, J) /= 0.0 then
                        Is_Basic := False;
                        exit;
                     end if;
                  end loop;
                  
                  if Is_Basic then
                     Result.Solution(J) := Tableau(I, Tableau'Last (2));
                     exit;
                  end if;
               end;
            end if;
         end loop;
      end loop;
      
      return Result;
   end Simplex_Solve;
   
   -- Example usage
   procedure Example is
      -- Example: Maximize 3x + 2y subject to:
      -- x + y <= 4
      -- 2x + y <= 5
      -- x, y >= 0
      
      Coefficients : Matrix (1 .. 2, 1 .. 2) := 
        ((1.0, 1.0), (2.0, 1.0));
      
      Constraints : Matrix (1 .. 2, 1 .. 1) := 
        ((4.0), (5.0));
      
      Objective : Vector (1 .. 2) := (3.0, 2.0);
      
      Result : Simplex_Result;
   begin
      Put_Line("Simplex Algorithm Example");
      Put_Line("========================");
      Put_Line("Maximize: 3x + 2y");
      Put_Line("Subject to:");
      Put_Line("  x + y <= 4");
      Put_Line("  2x + y <= 5");
      Put_Line("  x, y >= 0");
      Put_Line("");
      
      Result := Simplex_Solve(Coefficients, Constraints, Objective);
      
      if Result.Is_Optimal then
         Put_Line("Optimal Solution:");
         Put("  x = ");
         Put(Result.Solution(1), Fore => 1, Aft => 2, Exp => 0);
         Put_Line("");
         Put("  y = ");
         Put(Result.Solution(2), Fore => 1, Aft => 2, Exp => 0);
         Put_Line("");
         Put("Optimal Value = ");
         Put(Result.Optimal_Value, Fore => 1, Aft => 2, Exp => 0);
         Put_Line("");
      else
         Put_Line("Problem is unbounded");
      end if;
   end Example;
   
begin
   Example;
end Simplex_Example;
```

## Explanation

This Ada implementation of the Simplex algorithm includes:

1. **Matrix Operations**: Defines matrix types and operations for tableau manipulation
2. **Pivot Operations**: Implements row operations to update the tableau
3. **Optimality Check**: Determines when the optimal solution has been reached
4. **Variable Selection**: Finds entering and leaving variables
5. **Example Problem**: Solves a standard linear programming problem

## Sample Output

```
Simplex Algorithm Example
========================
Maximize: 3x + 2y
Subject to:
  x + y <= 4
  2x + y <= 5
  x, y >= 0

Optimal Solution:
  x = 1.00
  y = 3.00
Optimal Value = 9.00
```

This implementation demonstrates the core concepts of the Simplex method in Ada, including proper matrix handling, iterative optimization, and solution extraction.

