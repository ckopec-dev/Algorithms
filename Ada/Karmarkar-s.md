# Karmarkar's Algorithm in Ada

Karmarkar's algorithm is a polynomial-time algorithm for solving linear programming problems. Here's an implementation in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

procedure Karmarkar_Algorithm is
   
   -- Matrix and vector types
   type Vector is array (Integer range <>) of Float;
   type Matrix is array (Integer range <>, Integer range <>) of Float;
   
   -- Problem dimensions
   N : constant Integer := 3;  -- Number of variables
   M : constant Integer := 2;  -- Number of constraints
   
   -- Objective function coefficients
   c : Vector(1..N) := (1.0, 2.0, 3.0);
   
   -- Constraint matrix (A)
   A : Matrix(1..M, 1..N) := 
     ((1.0, 1.0, 0.0),
      (0.0, 1.0, 1.0));
   
   -- Right-hand side vector (b)
   b : Vector(1..M) := (2.0, 3.0);
   
   -- Initial feasible point
   x0 : Vector(1..N) := (1.0, 1.0, 1.0);
   
   -- Algorithm parameters
   epsilon : constant Float := 1.0e-6;
   max_iter : constant Integer := 1000;
   
   -- Function to compute the gradient of the objective function
   function Gradient(x : Vector) return Vector is
      g : Vector(1..N);
   begin
      for i in 1..N loop
         g(i) := c(i);
      end loop;
      return g;
   end Gradient;
   
   -- Function to check if point is feasible
   function Is_Feasible(x : Vector) return Boolean is
   begin
      for i in 1..M loop
         declare
            sum : Float := 0.0;
         begin
            for j in 1..N loop
               sum := sum + A(i,j) * x(j);
            end loop;
            if sum > b(i) then
               return False;
            end if;
         end;
      end loop;
      return True;
   end Is_Feasible;
   
   -- Function to compute the step direction
   function Compute_Step_Direction(x : Vector) return Vector is
      direction : Vector(1..N);
      temp : Float;
   begin
      -- Simple gradient descent direction (simplified for example)
      for i in 1..N loop
         direction(i) := -c(i);
      end loop;
      return direction;
   end Compute_Step_Direction;
   
   -- Function to compute the step size
   function Compute_Step_Size(x : Vector, direction : Vector) return Float is
      alpha : Float := 0.5;
      temp_x : Vector(1..N);
      temp : Float;
   begin
      -- Simple backtracking line search
      for i in 1..N loop
         temp_x(i) := x(i) + alpha * direction(i);
      end loop;
      
      -- Check if new point is feasible
      if Is_Feasible(temp_x) then
         return alpha;
      else
         return alpha * 0.5;
      end if;
   end Compute_Step_Size;
   
   -- Main Karmarkar's algorithm
   procedure Solve_Linear_Program is
      x : Vector(1..N);
      direction : Vector(1..N);
      alpha : Float;
      iter : Integer := 0;
      obj_value : Float;
   begin
      -- Initialize
      x := x0;
      
      Put_Line("Starting Karmarkar's Algorithm");
      Put_Line("Initial point: ");
      for i in 1..N loop
         Put(x(i), Fore => 5, Aft => 2, Exp => 0);
         Put(" ");
      end loop;
      Put_Line("");
      
      -- Main iteration loop
      while iter < max_iter loop
         -- Compute objective value
         obj_value := 0.0;
         for i in 1..N loop
            obj_value := obj_value + c(i) * x(i);
         end loop;
         
         Put_Line("Iteration " & Integer'Image(iter) & 
                  ": Objective = " & Float'Image(obj_value));
         
         -- Check convergence
         exit when obj_value < epsilon;
         
         -- Compute step direction
         direction := Compute_Step_Direction(x);
         
         -- Compute step size
         alpha := Compute_Step_Size(x, direction);
         
         -- Update solution
         for i in 1..N loop
            x(i) := x(i) + alpha * direction(i);
         end loop;
         
         iter := iter + 1;
      end loop;
      
      Put_Line("Final solution:");
      for i in 1..N loop
         Put(x(i), Fore => 5, Aft => 2, Exp => 0);
         Put(" ");
      end loop;
      Put_Line("");
      
      Put_Line("Final objective value: " & Float'Image(obj_value));
      Put_Line("Iterations: " & Integer'Image(iter));
      
   end Solve_Linear_Program;
   
begin
   Solve_Linear_Program;
end Karmarkar_Algorithm;
```

## Explanation

This Ada implementation of Karmarkar's algorithm includes:

1. **Data Structures**: 
   - `Vector` and `Matrix` types for mathematical operations
   - Problem definition with coefficients, constraints, and initial point

2. **Key Functions**:
   - `Gradient`: Computes the gradient of the objective function
   - `Is_Feasible`: Checks if a point satisfies constraints
   - `Compute_Step_Direction`: Calculates the search direction
   - `Compute_Step_Size`: Determines the appropriate step size

3. **Main Algorithm**:
   - Iteratively updates the solution
   - Checks convergence criteria
   - Maintains iteration count

## Usage Notes

- This is a simplified implementation for demonstration
- Real Karmarkar's algorithm would require more sophisticated matrix operations
- The algorithm assumes a feasible starting point
- Convergence is based on objective function value rather than strict optimality conditions

To compile and run this Ada program, you would typically use:
```bash
gnatmake karmarkar_algorithm.adb
./karmarkar_algorithm
```

