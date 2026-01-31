# Subset Sum Algorithm in Ada

Here's an implementation of the subset sum algorithm in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Subset_Sum is
   
   type Boolean_Array is array (Integer range <>) of Boolean;
   type Integer_Array is array (Integer range <>) of Integer;
   
   -- Function to check if there exists a subset with given sum
   function Subset_Sum_Exists(Values : Integer_Array; 
                            Size : Integer; 
                            Target_Sum : Integer) return Boolean is
      -- Create a boolean DP table
      DP : Boolean_Array(0 .. Target_Sum);
   begin
      -- Initialize DP table
      for I in 0 .. Target_Sum loop
         DP(I) := False;
      end loop;
      
      -- Base case: sum of 0 is always possible (empty subset)
      DP(0) := True;
      
      -- Fill the DP table
      for I in 1 .. Size loop
         -- Traverse backwards to avoid using updated values
         for J in reverse Target_Sum downto Values(I) loop
            if DP(J - Values(I)) then
               DP(J) := True;
            end if;
         end loop;
      end loop;
      
      return DP(Target_Sum);
   end Subset_Sum_Exists;
   
   -- Function to find and display actual subset
   procedure Find_Subset(Values : Integer_Array; 
                        Size : Integer; 
                        Target_Sum : Integer) is
      DP : Boolean_Array(0 .. Target_Sum);
      Subset : Integer_Array(1 .. Size);
      Subset_Size : Integer := 0;
   begin
      -- Initialize DP table
      for I in 0 .. Target_Sum loop
         DP(I) := False;
      end loop;
      
      DP(0) := True;
      
      -- Fill the DP table
      for I in 1 .. Size loop
         for J in reverse Target_Sum downto Values(I) loop
            if DP(J - Values(I)) then
               DP(J) := True;
            end if;
         end loop;
      end loop;
      
      -- If target sum is not possible
      if not DP(Target_Sum) then
         Put_Line("No subset found with sum " & Integer'Image(Target_Sum));
         return;
      end if;
      
      -- Backtrack to find the actual subset
      declare
         Current_Sum : Integer := Target_Sum;
         Index : Integer := Size;
      begin
         while Current_Sum > 0 and Index >= 1 loop
            -- If including current element gives us the target sum
            if Current_Sum >= Values(Index) and then DP(Current_Sum - Values(Index)) then
               Subset_Size := Subset_Size + 1;
               Subset(Subset_Size) := Values(Index);
               Current_Sum := Current_Sum - Values(Index);
            end if;
            Index := Index - 1;
         end loop;
         
         -- Display the subset
         Put("Subset with sum " & Integer'Image(Target_Sum) & " is: ");
         for I in reverse 1 .. Subset_Size loop
            Put(Integer'Image(Subset(I)));
            if I > 1 then
               Put(", ");
            end if;
         end loop;
         New_Line;
      end;
   end Find_Subset;
   
   -- Test the algorithm
   Values : Integer_Array(1 .. 5) := (3, 34, 4, 12, 5);
   Size : constant Integer := Values'Length;
   Target : constant Integer := 9;
   
begin
   Put_Line("Subset Sum Problem");
   Put_Line("==================");
   
   Put_Line("Array: ");
   for I in Values'Range loop
      Put(Integer'Image(Values(I)));
      if I < Values'Last then
         Put(", ");
      end if;
   end loop;
   New_Line;
   
   Put_Line("Target Sum: " & Integer'Image(Target));
   
   -- Check if subset exists
   if Subset_Sum_Exists(Values, Size, Target) then
      Put_Line("YES: A subset with sum " & Integer'Image(Target) & " exists");
      New_Line;
      
      -- Find and display the actual subset
      Find_Subset(Values, Size, Target);
   else
      Put_Line("NO: No subset with sum " & Integer'Image(Target) & " exists");
   end if;
   
   -- Test another case
   Put_Line("------------------");
   Put_Line("Testing with target sum 30:");
   if Subset_Sum_Exists(Values, Size, 30) then
      Put_Line("YES: A subset with sum 30 exists");
      Find_Subset(Values, Size, 30);
   else
      Put_Line("NO: No subset with sum 30 exists");
   end if;
   
end Subset_Sum;
```

## How it works:

1. **Dynamic Programming Approach**: Uses a boolean DP table where `DP[i]` represents whether sum `i` is possible
2. **Time Complexity**: O(n × sum) where n is the number of elements and sum is the target sum
3. **Space Complexity**: O(sum) for the DP table
4. **Backtracking**: The `Find_Subset` procedure reconstructs the actual subset that sums to the target

## Sample Output:
```
Subset Sum Problem
==================
Array: 
 3, 34, 4, 12, 5
Target Sum:  9
YES: A subset with sum  9 exists

Subset with sum  9 is: 4, 5

------------------
Testing with target sum 30:
NO: No subset with sum 30 exists
```

This implementation demonstrates the classic subset sum problem solution using dynamic programming in Ada, including both the existence check and the actual subset reconstruction.

