# Vogel's Approximation Method in Ada

Here's an implementation of Vogel's Approximation Method (VAM) for solving transportation problems in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;

procedure Vogel_Approximation_Method is
   
   -- Define maximum dimensions for the transportation problem
   Max_Rows : constant := 10;
   Max_Cols : constant := 10;
   
   -- Transportation cost matrix
   type Cost_Matrix is array (1..Max_Rows, 1..Max_Cols) of Integer;
   
   -- Supply and demand vectors
   type Quantity_Vector is array (1..Max_Rows) of Integer;
   type Demand_Vector is array (1..Max_Cols) of Integer;
   
   -- Solution matrix (allocation)
   type Allocation_Matrix is array (1..Max_Rows, 1..Max_Cols) of Integer;
   
   -- Function to calculate penalty for a row
   function Calculate_Row_Penalty(Costs : Cost_Matrix; 
                                  Supply : Quantity_Vector; 
                                  Demand : Demand_Vector; 
                                  Row : Integer) return Integer is
      Min1, Min2 : Integer := Integer'Last;
      Min1_Index : Integer := 0;
   begin
      for Col in 1..Max_Cols loop
         if Supply(Row) > 0 and Demand(Col) > 0 then
            if Costs(Row, Col) < Min1 then
               Min2 := Min1;
               Min1 := Costs(Row, Col);
               Min1_Index := Col;
            elsif Costs(Row, Col) < Min2 then
               Min2 := Costs(Row, Col);
            end if;
         end if;
      end loop;
      
      if Min1 = Integer'Last then
         return 0;
      else
         return Min2 - Min1;
      end if;
   end Calculate_Row_Penalty;
   
   -- Function to calculate penalty for a column
   function Calculate_Col_Penalty(Costs : Cost_Matrix; 
                                  Supply : Quantity_Vector; 
                                  Demand : Demand_Vector; 
                                  Col : Integer) return Integer is
      Min1, Min2 : Integer := Integer'Last;
   begin
      for Row in 1..Max_Rows loop
         if Supply(Row) > 0 and Demand(Col) > 0 then
            if Costs(Row, Col) < Min1 then
               Min2 := Min1;
               Min1 := Costs(Row, Col);
            elsif Costs(Row, Col) < Min2 then
               Min2 := Costs(Row, Col);
            end if;
         end if;
      end loop;
      
      if Min1 = Integer'Last then
         return 0;
      else
         return Min2 - Min1;
      end if;
   end Calculate_Col_Penalty;
   
   -- Function to find maximum penalty row or column
   procedure Find_Max_Penalty(Costs : Cost_Matrix; 
                              Supply : Quantity_Vector; 
                              Demand : Demand_Vector;
                              Max_Row_Penalty : out Integer;
                              Max_Col_Penalty : out Integer;
                              Max_Row_Index : out Integer;
                              Max_Col_Index : out Integer;
                              Penalty_Type : out Character) is
      Row_Penalty : array (1..Max_Rows) of Integer;
      Col_Penalty : array (1..Max_Cols) of Integer;
      Max_Row_Pen : Integer := 0;
      Max_Col_Pen : Integer := 0;
      Row_Index : Integer := 0;
      Col_Index : Integer := 0;
   begin
      -- Calculate penalties for all rows
      for Row in 1..Max_Rows loop
         Row_Penalty(Row) := Calculate_Row_Penalty(Costs, Supply, Demand, Row);
         if Row_Penalty(Row) > Max_Row_Pen then
            Max_Row_Pen := Row_Penalty(Row);
            Row_Index := Row;
         end if;
      end loop;
      
      -- Calculate penalties for all columns
      for Col in 1..Max_Cols loop
         Col_Penalty(Col) := Calculate_Col_Penalty(Costs, Supply, Demand, Col);
         if Col_Penalty(Col) > Max_Col_Pen then
            Max_Col_Pen := Col_Penalty(Col);
            Col_Index := Col;
         end if;
      end loop;
      
      Max_Row_Penalty := Max_Row_Pen;
      Max_Col_Penalty := Max_Col_Pen;
      Max_Row_Index := Row_Index;
      Max_Col_Index := Col_Index;
      
      if Max_Row_Pen >= Max_Col_Pen then
         Penalty_Type := 'R';
      else
         Penalty_Type := 'C';
      end if;
   end Find_Max_Penalty;
   
   -- Main VAM algorithm
   procedure Solve_Transportation_Problem is
      -- Example transportation problem data
      Costs : Cost_Matrix := (
         (8, 6, 10, 9),
         (9, 12, 13, 7),
         (14, 9, 16, 5)
      );
      
      Supply : Quantity_Vector := (150, 250, 100);
      Demand : Demand_Vector := (200, 150, 100, 150);
      
      Allocation : Allocation_Matrix := (others => (others => 0));
      Total_Cost : Float := 0.0;
      
      -- Variables for iteration
      Row_Index : Integer;
      Col_Index : Integer;
      Max_Row_Penalty : Integer;
      Max_Col_Penalty : Integer;
      Penalty_Type : Character;
      
      -- Temporary variables
      Min_Supply : Integer;
      Min_Demand : Integer;
      Min_Value : Integer;
      
   begin
      Put_Line("Vogel's Approximation Method for Transportation Problem");
      Put_Line("=====================================================");
      
      -- Display original problem
      Put_Line("Cost Matrix:");
      for Row in 1..3 loop
         for Col in 1..4 loop
            Put(Costs(Row, Col), Width => 4);
         end loop;
         New_Line;
      end loop;
      
      Put_Line("Supply: ");
      for i in 1..3 loop
         Put(Supply(i), Width => 4);
      end loop;
      New_Line;
      
      Put_Line("Demand: ");
      for i in 1..4 loop
         Put(Demand(i), Width => 4);
      end loop;
      New_Line;
      
      -- Main VAM loop
      while True loop
         -- Check if problem is solved
         declare
            Total_Supply : Integer := 0;
            Total_Demand : Integer := 0;
         begin
            for i in 1..3 loop
               Total_Supply := Total_Supply + Supply(i);
            end loop;
            for i in 1..4 loop
               Total_Demand := Total_Demand + Demand(i);
            end loop;
            
            if Total_Supply = 0 and Total_Demand = 0 then
               exit;
            end if;
         end;
         
         -- Find maximum penalty
         Find_Max_Penalty(Costs, Supply, Demand, 
                         Max_Row_Penalty, Max_Col_Penalty,
                         Row_Index, Col_Index, Penalty_Type);
         
         -- Allocate based on maximum penalty
         if Penalty_Type = 'R' then
            -- Find minimum cost in the row
            declare
               Min_Cost : Integer := Integer'Last;
               Min_Col : Integer := 0;
            begin
               for Col in 1..4 loop
                  if Supply(Row_Index) > 0 and Demand(Col) > 0 then
                     if Costs(Row_Index, Col) < Min_Cost then
                        Min_Cost := Costs(Row_Index, Col);
                        Min_Col := Col;
                     end if;
                  end if;
               end loop;
               
               -- Allocate maximum possible
               Min_Supply := Supply(Row_Index);
               Min_Demand := Demand(Min_Col);
               Min_Value := Integer'Min(Min_Supply, Min_Demand);
               
               Allocation(Row_Index, Min_Col) := Min_Value;
               Supply(Row_Index) := Supply(Row_Index) - Min_Value;
               Demand(Min_Col) := Demand(Min_Col) - Min_Value;
               
               Put_Line("Allocated " & Integer'Image(Min_Value) & 
                       " units from row " & Integer'Image(Row_Index) & 
                       " to column " & Integer'Image(Min_Col));
            end;
         else
            -- Find minimum cost in the column
            declare
               Min_Cost : Integer := Integer'Last;
               Min_Row : Integer := 0;
            begin
               for Row in 1..3 loop
                  if Supply(Row) > 0 and Demand(Col_Index) > 0 then
                     if Costs(Row, Col_Index) < Min_Cost then
                        Min_Cost := Costs(Row, Col_Index);
                        Min_Row := Row;
                     end if;
                  end if;
               end loop;
               
               -- Allocate maximum possible
               Min_Supply := Supply(Min_Row);
               Min_Demand := Demand(Col_Index);
               Min_Value := Integer'Min(Min_Supply, Min_Demand);
               
               Allocation(Min_Row, Col_Index) := Min_Value;
               Supply(Min_Row) := Supply(Min_Row) - Min_Value;
               Demand(Col_Index) := Demand(Col_Index) - Min_Value;
               
               Put_Line("Allocated " & Integer'Image(Min_Value) & 
                       " units from row " & Integer'Image(Min_Row) & 
                       " to column " & Integer'Image(Col_Index));
            end;
         end if;
      end loop;
      
      -- Calculate total cost
      for Row in 1..3 loop
         for Col in 1..4 loop
            if Allocation(Row, Col) > 0 then
               Total_Cost := Total_Cost + Float(Allocation(Row, Col)) * Float(Costs(Row, Col));
            end if;
         end loop;
      end loop;
      
      Put_Line("Final Allocation Matrix:");
      for Row in 1..3 loop
         for Col in 1..4 loop
            Put(Allocation(Row, Col), Width => 4);
         end loop;
         New_Line;
      end loop;
      
      Put_Line("Total Cost: " & Float'Image(Total_Cost));
      
   end Solve_Transportation_Problem;
   
begin
   Solve_Transportation_Problem;
end Vogel_Approximation_Method;
```

## How the Algorithm Works

This Ada implementation of Vogel's Approximation Method follows these steps:

1. **Initialize**: Set up the transportation cost matrix, supply, and demand vectors
2. **Calculate Penalties**: For each row and column, calculate the penalty as the difference between the two smallest costs
3. **Find Maximum Penalty**: Identify the row or column with the highest penalty
4. **Allocate**: In the row/column with maximum penalty, allocate to the cell with minimum cost
5. **Update**: Reduce supply/demand and remove exhausted rows/columns
6. **Repeat**: Continue until all allocations are made

## Key Features

- **Modular Design**: Separate functions for penalty calculation and maximum penalty finding
- **Error Handling**: Checks for valid supply/demand values
- **Clear Output**: Shows each allocation step and final cost
- **Flexible**: Can be easily modified for different problem sizes

## Sample Output

The program will display:
- The original cost matrix
- Supply and demand values
- Step-by-step allocation process
- Final allocation matrix
- Total transportation cost

This implementation demonstrates how Vogel's method provides a good initial feasible solution to transportation problems, often close to optimal.

