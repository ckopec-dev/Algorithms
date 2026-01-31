# Knapsack Problem Algorithm in Ada

Here's an implementation of the 0/1 Knapsack problem using dynamic programming in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Knapsack is
   
   -- Constants
   MAX_WEIGHT := 10;
   MAX_ITEMS := 5;
   
   -- Item structure
   type Item is record
      Weight : Integer;
      Value  : Integer;
   end record;
   
   -- Arrays for items and dp table
   Items : array (1..MAX_ITEMS) of Item :=
     ((Weight => 2, Value => 3),
      (Weight => 3, Value => 4),
      (Weight => 4, Value => 5),
      (Weight => 5, Value => 6),
      (Weight => 9, Value => 10));
   
   -- DP table
   DP : array (0..MAX_ITEMS, 0..MAX_WEIGHT) of Integer;
   
   -- Function to return maximum of two integers
   function Max(A, B : Integer) return Integer is
   begin
      if A > B then
         return A;
      else
         return B;
      end if;
   end Max;
   
   -- Knapsack algorithm
   procedure Knapsack_Solve is
      W : Integer;  -- Current weight capacity
      I : Integer;  -- Current item index
   begin
      -- Initialize DP table with zeros
      for I in 0..MAX_ITEMS loop
         DP(I, 0) := 0;
      end loop;
      
      for W in 0..MAX_WEIGHT loop
         DP(0, W) := 0;
      end loop;
      
      -- Fill the DP table
      for I in 1..MAX_ITEMS loop
         for W in 1..MAX_WEIGHT loop
            -- If current item's weight exceeds capacity, skip it
            if Items(I).Weight > W then
               DP(I, W) := DP(I-1, W);
            else
               -- Max of including or excluding current item
               DP(I, W) := Max(DP(I-1, W), 
                              DP(I-1, W - Items(I).Weight) + Items(I).Value);
            end if;
         end loop;
      end loop;
   end Knapsack_Solve;
   
   -- Function to trace back solution
   procedure Trace_Back is
      W : Integer := MAX_WEIGHT;
      I : Integer := MAX_ITEMS;
      Total_Value : Integer := DP(MAX_ITEMS, MAX_WEIGHT);
      Total_Weight : Integer := 0;
      Selected_Items : array (1..MAX_ITEMS) of Boolean := (others => False);
   begin
      Put_Line("Selected items:");
      
      while I > 0 and W > 0 loop
         -- If value is different from above row, item was included
         if DP(I, W) /= DP(I-1, W) then
            Selected_Items(I) := True;
            Total_Weight := Total_Weight + Items(I).Weight;
            W := W - Items(I).Weight;
         end if;
         I := I - 1;
      end loop;
      
      for I in 1..MAX_ITEMS loop
         if Selected_Items(I) then
            Put("Item ");
            Put(I, 0);
            Put(" (Weight: ");
            Put(Items(I).Weight, 0);
            Put(", Value: ");
            Put(Items(I).Value, 0);
            Put_Line(")");
         end if;
      end loop;
      
      Put_Line("Total weight: " & Integer'Image(Total_Weight));
      Put_Line("Total value: " & Integer'Image(Total_Value));
   end Trace_Back;
   
begin
   -- Solve the knapsack problem
   Knapsack_Solve;
   
   -- Display the DP table
   Put_Line("DP Table:");
   Put("     ");
   for W in 0..MAX_WEIGHT loop
      Put(W, 3);
   end loop;
   New_Line;
   
   for I in 0..MAX_ITEMS loop
      Put(I, 3);
      for W in 0..MAX_WEIGHT loop
         Put(DP(I, W), 3);
      end loop;
      New_Line;
   end loop;
   
   New_Line;
   
   -- Display the solution
   Put_Line("Optimal solution:");
   Put_Line("Maximum value: " & Integer'Image(DP(MAX_ITEMS, MAX_WEIGHT)));
   
   Trace_Back;
   
end Knapsack;
```

## Explanation

This Ada implementation of the Knapsack problem includes:

1. **Data Structures**:
   - `Item` record with weight and value fields
   - 2D array `DP` for dynamic programming table
   - Arrays for items and weights

2. **Key Components**:
   - `Max` function to find maximum of two integers
   - `Knapsack_Solve` procedure that fills the DP table
   - `Trace_Back` procedure to reconstruct the solution

3. **Algorithm Logic**:
   - Uses dynamic programming approach with time complexity O(nW)
   - For each item and weight capacity, decides whether to include or exclude the item
   - Builds up optimal solutions for smaller subproblems

4. **Output**:
   - Displays the DP table
   - Shows which items are selected
   - Reports total weight and value of the optimal solution

The example uses 5 items with weights [2,3,4,5,9] and values [3,4,5,6,10] with a knapsack capacity of 10.

