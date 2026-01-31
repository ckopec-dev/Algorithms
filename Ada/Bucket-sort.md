# Bucket Sort Algorithm in Ada

Here's an implementation of the Bucket Sort algorithm in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;

procedure Bucket_Sort is
   
   -- Define the array type
   type Float_Array is array (Natural range <>) of Float;
   
   -- Bucket sort procedure
   procedure Bucket_Sort_Array(A : in out Float_Array) is
      -- Maximum number of buckets
      Num_Buckets : constant := 10;
      
      -- Create buckets
      type Bucket_Array is array (0 .. Num_Buckets - 1) of Float_Array(0 .. 100);
      Buckets : Bucket_Array;
      
      -- Bucket sizes
      Bucket_Sizes : array (0 .. Num_Buckets - 1) of Natural := (others => 0);
      
      -- Find maximum value to normalize
      Max_Val : Float := 0.0;
      
      -- Helper function to find maximum
      function Find_Max(Arr : Float_Array) return Float is
      begin
         for I in Arr'Range loop
            if Arr(I) > Max_Val then
               Max_Val := Arr(I);
            end if;
         end loop;
         return Max_Val;
      end Find_Max;
      
   begin
      -- Find maximum value
      Max_Val := Find_Max(A);
      
      -- Handle empty array
      if Max_Val = 0.0 then
         return;
      end if;
      
      -- Distribute elements into buckets
      for I in A'Range loop
         -- Calculate bucket index
         declare
            Bucket_Index : constant Natural := Natural(A(I) * Float(Num_Buckets) / Max_Val);
            -- Adjust for edge case
            Adjusted_Index : constant Natural := 
               (if Bucket_Index >= Num_Buckets then Num_Buckets - 1 else Bucket_Index);
         begin
            Buckets(Adjusted_Index)(Bucket_Sizes(Adjusted_Index)) := A(I);
            Bucket_Sizes(Adjusted_Index) := Bucket_Sizes(Adjusted_Index) + 1;
         end;
      end loop;
      
      -- Sort each bucket (using bubble sort for simplicity)
      for I in 0 .. Num_Buckets - 1 loop
         if Bucket_Sizes(I) > 1 then
            for J in 0 .. Bucket_Sizes(I) - 2 loop
               for K in 0 .. Bucket_Sizes(I) - 2 - J loop
                  if Buckets(I)(K) > Buckets(I)(K + 1) then
                     declare
                        Temp : constant Float := Buckets(I)(K);
                     begin
                        Buckets(I)(K) := Buckets(I)(K + 1);
                        Buckets(I)(K + 1) := Temp;
                     end;
                  end if;
               end loop;
            end loop;
         end if;
      end loop;
      
      -- Collect sorted elements back to original array
      declare
         Index : Natural := 0;
      begin
         for I in 0 .. Num_Buckets - 1 loop
            for J in 0 .. Bucket_Sizes(I) - 1 loop
               A(Index) := Buckets(I)(J);
               Index := Index + 1;
            end loop;
         end loop;
      end;
      
   end Bucket_Sort_Array;
   
   -- Procedure to print array
   procedure Print_Array(A : Float_Array) is
   begin
      for I in A'Range loop
         Put(A(I), Fore => 1, Aft => 2, Exp => 0);
         Put(" ");
      end loop;
      New_Line;
   end Print_Array;
   
   -- Test the algorithm
   Test_Array : Float_Array(0 .. 9) := (0.42, 0.32, 0.33, 0.52, 0.37, 0.47, 0.51, 0.23, 0.87, 0.91);
   
begin
   Put_Line("Original array:");
   Print_Array(Test_Array);
   
   Bucket_Sort_Array(Test_Array);
   
   Put_Line("Sorted array:");
   Print_Array(Test_Array);
   
end Bucket_Sort;
```

## How it works:

1. **Bucket Creation**: Creates a fixed number of buckets (10 in this example)
2. **Distribution**: Each element is placed into a bucket based on its value
3. **Sorting**: Each bucket is sorted individually (using bubble sort in this implementation)
4. **Collection**: Elements are collected back into the original array in sorted order

## Sample Output:
```
Original array:
0.42 0.32 0.33 0.52 0.37 0.47 0.51 0.23 0.87 0.91 
Sorted array:
0.23 0.32 0.33 0.37 0.42 0.47 0.51 0.52 0.87 0.91 
```

This implementation demonstrates the bucket sort algorithm's approach of distributing elements into buckets and then sorting each bucket individually before merging them back together.

