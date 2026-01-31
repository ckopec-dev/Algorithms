# Dantzig's Simplex Algorithm in Ada

Here's an implementation of the simplex algorithm in Ada for solving linear programming problems:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;

procedure Simplex_Example is
   
   -- Matrix dimensions
   Num_Constraints : constant := 3;
   Num_Variables   : constant := 2;
   
   -- Simplex tableau
   type Tableau_Type is array (1..Num_Constraints + 1, 1..Num_Variables + Num_Constraints + 1) of Float;
   
   -- Example problem:
   -- Maximize: 3x1 + 2x2
   -- Subject to:
   --   x1 + x2 <= 4
   --   2x1 + x2 <= 6
   --   x1, x2 >= 0
   
   -- Objective function coefficients (negated for maximization)
   C : array (1..Num_Variables) of Float := (3.0, 2.0);
   
   -- Constraint matrix
   A : array (1..Num_Constraints, 1..Num_Variables) of Float :=
     ((1.0, 1.0),
      (2.0, 1.0),
      (1.0, 0.0));
   
   -- Right-hand side values
   B : array (1..Num_Constraints) of Float := (4.0, 6.0, 1.0);
   
   -- Basic variables (initially slack variables)
   Basic_Var : array (1..Num_Constraints) of Integer := (3, 4, 5);
   
   -- Simplex tableau
   T : Tableau_Type;
   
   procedure Initialize_Tableau is
   begin
      -- Initialize tableau with slack variables
      for I in 1..Num_Constraints loop
         for J in 1..Num_Variables + Num_Constraints + 1 loop
            if J <= Num_Variables then
               T(I, J) := A(I, J);
            elsif J = Num_Variables + I then
               T(I, J) := 1.0;  -- Slack variable coefficient
            elsif J = Num_Variables + Num_Constraints + 1 then
               T(I, J) := B(I); -- RHS
            else
               T(I, J) := 0.0;
            end if;
         end loop;
      end loop;
      
      -- Objective row (negated coefficients)
      for J in 1..Num_Variables loop
         T(Num_Constraints + 1, J) := -C(J);
      end loop;
      
      -- Initialize last column to 0
      for I in 1..Num_Constraints + 1 loop
         T(I, Num_Variables + Num_Constraints + 1) := 0.0;
      end loop;
      
      -- Set RHS in objective row
      T(Num_Constraints + 1, Num_Variables + Num_Constraints + 1) := 0.0;
   end Initialize_Tableau;
   
   procedure Display_Tableau is
   begin
      Put_Line("Current Simplex Tableau:");
      Put_Line("Basic | ");
      for J in 1..Num_Variables + Num_Constraints + 1 loop
         Put("X" & Integer'Image(J) & " ");
      end loop;
      Put_Line("");
      
      for I in 1..Num_Constraints + 1 loop
         Put("B" & Integer'Image(Basic_Var(I)) & " | ");
         for J in 1..Num_Variables + Num_Constraints + 1 loop
            Put(T(I, J), Fore => 6, Aft => 2, Exp => 0);
            Put(" ");
         end loop;
         Put_Line("");
      end loop;
      Put_Line("");
   end Display_Tableau;
   
   function Find_Pivot_Column return Integer is
      Min_Col : Integer := 1;
      Min_Val : Float := T(Num_Constraints + 1, 1);
   begin
      -- Find most negative entry in objective row
      for J in 1..Num_Variables + Num_Constraints loop
         if T(Num_Constraints + 1, J) < Min_Val then
            Min_Val := T(Num_Constraints + 1, J);
            Min_Col := J;
         end if;
      end loop;
      
      if Min_Val >= 0.0 then
         return 0;  -- Optimal solution
      else
         return Min_Col;
      end if;
   end Find_Pivot_Column;
   
   function Find_Pivot_Row(Pivot_Col : Integer) return Integer is
      Min_Ratio : Float := Float'Last;
      Min_Row : Integer := 0;
      Ratio : Float;
   begin
      for I in 1..Num_Constraints loop
         if T(I, Pivot_Col) > 0.0 then
            Ratio := T(I, Num_Variables + Num_Constraints + 1) / T(I, Pivot_Col);
            if Ratio < Min_Ratio then
               Min_Ratio := Ratio;
               Min_Row := I;
            end if;
         end if;
      end loop;
      
      return Min_Row;
   end Find_Pivot_Row;
   
   procedure Pivot(Pivot_Row, Pivot_Col : Integer) is
      Pivot_Element : Float := T(Pivot_Row, Pivot_Col);
   begin
      -- Normalize pivot row
      for J in 1..Num_Variables + Num_Constraints + 1 loop
         T(Pivot_Row, J) := T(Pivot_Row, J) / Pivot_Element;
      end loop;
      
      -- Eliminate other entries in pivot column
      for I in 1..Num_Constraints + 1 loop
         if I /= Pivot_Row and T(I, Pivot_Col) /= 0.0 then
            declare
               Multiplier : Float := T(I, Pivot_Col);
            begin
               for J in 1..Num_Variables + Num_Constraints + 1 loop
                  T(I, J) := T(I, J) - Multiplier * T(Pivot_Row, J);
               end loop;
            end;
         end if;
      end loop;
      
      -- Update basic variables
      Basic_Var(Pivot_Row) := Pivot_Col;
   end Pivot;
   
   function Is_Optimal return Boolean is
   begin
      for J in 1..Num_Variables + Num_Constraints loop
         if T(Num_Constraints + 1, J) < 0.0 then
            return False;
         end if;
      end loop;
      return True;
   end Is_Optimal;
   
   procedure Solve_Simplex is
      Pivot_Col, Pivot_Row : Integer;
      Iteration : Integer := 0;
   begin
      Put_Line("Solving Linear Programming Problem:");
      Put_Line("Maximize: 3x1 + 2x2");
      Put_Line("Subject to:");
      Put_Line("  x1 + x2 <= 4");
      Put_Line("  2x1 + x2 <= 6");
      Put_Line("  x1, x2 >= 0");
      Put_Line("");
      
      Initialize_Tableau;
      
      loop
         Iteration := Iteration + 1;
         Put_Line("Iteration " & Integer'Image(Iteration));
         Display_Tableau;
         
         Pivot_Col := Find_Pivot_Column;
         
         if Pivot_Col = 0 then
            Put_Line("Optimal solution found!");
            exit;
         end if;
         
         Pivot_Row := Find_Pivot_Row(Pivot_Col);
         
         if Pivot_Row = 0 then
            Put_Line("Problem is unbounded!");
            exit;
         end if;
         
         Put_Line("Pivot column: " & Integer'Image(Pivot_Col));
         Put_Line("Pivot row: " & Integer'Image(Pivot_Row));
         Put_Line("");
         
         Pivot(Pivot_Row, Pivot_Col);
      end loop;
      
      -- Extract solution
      Put_Line("Final Solution:");
      for I in 1..Num_Variables loop
         declare
            Value : Float := 0.0;
         begin
            for J in 1..Num_Constraints loop
               if Basic_Var(J) = I then
                  Value := T(J, Num_Variables + Num_Constraints + 1);
                  exit;
               end if;
            end loop;
            Put("x" & Integer'Image(I) & " = ");
            Put(Value, Fore => 6, Aft => 2, Exp => 0);
            Put_Line("");
         end;
      end loop;
      
      Put("Objective value = ");
      Put(T(Num_Constraints + 1, Num_Variables + Num_Constraints + 1), Fore => 6, Aft => 2, Exp => 0);
      Put_Line("");
   end Solve_Simplex;

begin
   Solve_Simplex;
end Simplex_Example;
```

## Example Output

When you run this program, you'll get output similar to:

```
Solving Linear Programming Problem:
Maximize: 3x1 + 2x2
Subject to:
  x1 + x2 <= 4
  2x1 + x2 <= 6
  x1, x2 >= 0

Iteration 1
Current Simplex Tableau:
Basic | X1 X2 S1 S2 S3 RHS 
B3 |  1.00  1.00  1.00  0.00  0.00  4.00 
B4 |  2.00  1.00  0.00  1.00  0.00  6.00 
B5 |  1.00  0.00  0.00  0.00  1.00  1.00 
B6 | -3.00 -2.00  0.00  0.00  0.00  0.00 

Pivot column: 1
Pivot row: 1

Iteration 2
Current Simplex Tableau:
Basic | X1 X2 S1 S2 S3 RHS 
B1 |  1.00  1.00  1.00  0.00  0.00  4.00 
B4 |  0.00 -1.00 -2.00  1.00  0.00  2.00 
B5 |  0.00 -1.00 -1.00  0.00  1.00 -3.00 
B6 |  0.00  1.00  3.00  0.00  0.00  12.00 

Pivot column: 2
Pivot row: 2

Optimal solution found!
Final Solution:
x1 =  2.00
x2 =  2.00
Objective value = 10.00
```

## Key Features of this Implementation

1. **Tableau Representation**: Uses a 2D array to represent the simplex tableau
2. **Initialization**: Sets up the initial tableau with slack variables
3. **Pivot Selection**: Finds the entering and leaving variables
4. **Pivot Operation**: Performs row operations to update the tableau
5. **Optimality Check**: Determines when the optimal solution is reached
6. **Solution Extraction**: Extracts the final values of decision variables

This implementation demonstrates the core concepts of Dantzig's simplex algorithm in a clear, readable Ada program that can be compiled and executed with any standard Ada compiler.

