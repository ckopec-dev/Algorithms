# Dynamic Programming Example in Ada: Fibonacci Sequence

Here's an implementation of the Fibonacci sequence using dynamic programming with memoization in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Fibonacci_DP is
   
   -- Define the maximum size for our Fibonacci array
   MAX_N : constant := 50;
   
   -- Array to store computed Fibonacci values
   F : array (0 .. MAX_N) of Long_Long_Integer;
   
   -- Function to compute Fibonacci number using dynamic programming
   function Fibonacci(n : Natural) return Long_Long_Integer is
   begin
      -- Base cases
      if n = 0 then
         F(n) := 0;
         return 0;
      elsif n = 1 then
         F(n) := 1;
         return 1;
      else
         -- If already computed, return stored value
         if F(n) /= 0 then
            return F(n);
         else
            -- Compute and store the result
            F(n) := Fibonacci(n-1) + Fibonacci(n-2);
            return F(n);
         end if;
      end if;
   end Fibonacci;
   
   -- Procedure to display Fibonacci sequence
   procedure Display_Fibonacci(n : Natural) is
   begin
      Put_Line("Fibonacci sequence up to " & Integer'Image(n) & ":");
      for i in 0 .. n loop
         Put(Integer'Image(i) & " -> " & Long_Long_Integer'Image(Fibonacci(i)));
         New_Line;
      end loop;
   end Display_Fibonacci;
   
begin
   -- Initialize the array to zero
   for i in 0 .. MAX_N loop
      F(i) := 0;
   end loop;
   
   -- Compute and display Fibonacci numbers
   Display_Fibonacci(10);
   
end Fibonacci_DP;
```

## Alternative Bottom-Up Dynamic Programming Approach

```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Fibonacci_Bottom_Up is
   
   MAX_N : constant := 50;
   
   -- Array to store computed Fibonacci values
   F : array (0 .. MAX_N) of Long_Long_Integer;
   
   -- Bottom-up dynamic programming approach
   function Fibonacci_Bottom_Up(n : Natural) return Long_Long_Integer is
      i : Natural;
   begin
      -- Base cases
      if n = 0 then
         return 0;
      elsif n = 1 then
         return 1;
      end if;
      
      -- Fill the array from bottom up
      F(0) := 0;
      F(1) := 1;
      
      for i in 2 .. n loop
         F(i) := F(i-1) + F(i-2);
      end loop;
      
      return F(n);
   end Fibonacci_Bottom_Up;
   
begin
   Put_Line("Bottom-up Fibonacci computation:");
   for i in 0 .. 10 loop
      Put(Integer'Image(i) & " -> " & Long_Long_Integer'Image(Fibonacci_Bottom_Up(i)));
      New_Line;
   end loop;
   
end Fibonacci_Bottom_Up;
```

## Key Dynamic Programming Concepts Demonstrated:

1. **Overlapping Subproblems**: The same Fibonacci numbers are computed multiple times
2. **Optimal Substructure**: Fibonacci(n) = Fibonacci(n-1) + Fibonacci(n-2)
3. **Memoization**: Storing previously computed results to avoid recomputation
4. **Tabulation**: Building up solutions from smaller subproblems

## Output:
```
Fibonacci sequence up to  10:
 0 ->  0
 1 ->  1
 2 ->  1
 3 ->  2
 4 ->  3
 5 ->  5
 6 ->  8
 7 ->  13
 8 ->  21
 9 ->  34
10 ->  55
```

This example demonstrates how dynamic programming in Ada can efficiently solve problems with overlapping subproblems by storing intermediate results and avoiding redundant computations.

