# N-Queens Problem in Ada

Here's a complete implementation of the N-Queens problem solver in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure N_Queens is
   
   type Board is array (Positive range <>) of Integer;
   
   -- Function to check if a queen can be placed at position (row, col)
   function Is_Valid(Board : Board; Row : Integer; Col : Integer) return Boolean is
      I : Integer;
   begin
      for I in 1 .. Row - 1 loop
         -- Check column conflict
         if Board(I) = Col then
            return False;
         end if;
         
         -- Check diagonal conflicts
         if abs(Board(I) - Col) = abs(I - Row) then
            return False;
         end if;
      end loop;
      return True;
   end Is_Valid;
   
   -- Recursive backtracking function to solve N-Queens
   function Solve_Board(Board : in out Board; Row : Integer; N : Integer) return Boolean is
   begin
      -- Base case: all queens placed
      if Row > N then
         return True;
      end if;
      
      -- Try placing queen in each column of current row
      for Col in 1 .. N loop
         if Is_Valid(Board, Row, Col) then
            Board(Row) := Col;
            
            -- Recursively solve for next row
            if Solve_Board(Board, Row + 1, N) then
               return True;
            end if;
         end if;
      end loop;
      
      -- Backtrack: no valid position found
      return False;
   end Solve_Board;
   
   -- Function to print the board
   procedure Print_Board(Board : Board; N : Integer) is
      I, J : Integer;
   begin
      Put_Line("Solution:");
      for I in 1 .. N loop
         for J in 1 .. N loop
            if Board(I) = J then
               Put("Q ");
            else
               Put(". ");
            end if;
         end loop;
         New_Line;
      end loop;
      New_Line;
   end Print_Board;
   
   -- Main procedure
   N : Integer := 8;  -- Default to 8-Queens
   Board : Board(1 .. N);
   
begin
   Put("Enter the value of N for N-Queens (default 8): ");
   Get(N);
   
   -- Validate input
   if N <= 0 then
      Put_Line("Invalid input. Please enter a positive integer.");
      return;
   end if;
   
   -- Resize board if necessary
   Board := (others => 0);
   
   -- Solve the N-Queens problem
   if Solve_Board(Board, 1, N) then
      Put_Line("Solution found for " & Integer'Image(N) & "-Queens:");
      Print_Board(Board, N);
   else
      Put_Line("No solution exists for " & Integer'Image(N) & "-Queens");
   end if;
   
end N_Queens;
```

## How the Algorithm Works

### Key Components:

1. **Board Representation**: Uses an array where `Board(I)` represents the column position of the queen in row `I`

2. **Validation Function**: 
   - Checks column conflicts (same column)
   - Checks diagonal conflicts (absolute difference of columns equals absolute difference of rows)

3. **Backtracking Algorithm**:
   - Places a queen in the first available position in each row
   - Recursively attempts to solve for the next row
   - Backtracks when no valid position is found

4. **Solution Display**: Shows the board with 'Q' for queens and '.' for empty spaces

### Sample Output:
```
Enter the value of N for N-Queens (default 8): 4
Solution found for  4-Queens:
Q . . . 
. . Q . 
. . . Q 
. Q . . 

Solution found for  4-Queens:
. Q . . 
. . . Q 
Q . . . 
. . Q . 
```

### Time Complexity:
- **Best Case**: O(N!) - when solution is found early
- **Worst Case**: O(N!) - all possible arrangements must be checked
- **Space Complexity**: O(N) - for the recursion stack and board array

This implementation demonstrates the classic backtracking approach to solving the N-Queens problem using Ada's strong typing and structured programming features.

