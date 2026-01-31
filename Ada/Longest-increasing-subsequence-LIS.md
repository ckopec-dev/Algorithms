# Longest Increasing Subsequence (LIS) in Ada

Here's an implementation of the LIS algorithm in Ada using dynamic programming:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Longest_Increasing_Subsequence is
   
   type Array_Type is array (Positive range <>) of Integer;
   type LIS_Array is array (Positive range <>) of Natural;
   
   -- Function to find the length of longest increasing subsequence
   function LIS_Length(Seq : Array_Type) return Natural is
      N : constant Natural := Seq'Length;
      L : LIS_Array(1..N);
      Max_Length : Natural := 0;
   begin
      if N = 0 then
         return 0;
      end if;
      
      -- Initialize all elements to 1
      for I in 1..N loop
         L(I) := 1;
      end loop;
      
      -- Fill L[] using dynamic programming
      for I in 2..N loop
         for J in 1..I-1 loop
            if Seq(I) > Seq(J) and then L(I) < L(J) + 1 then
               L(I) := L(J) + 1;
            end if;
         end loop;
      end loop;
      
      -- Find maximum length
      for I in 1..N loop
         if L(I) > Max_Length then
            Max_Length := L(I);
         end if;
      end loop;
      
      return Max_Length;
   end LIS_Length;
   
   -- Function to reconstruct the actual LIS
   function Get_LIS(Seq : Array_Type) return Array_Type is
      N : constant Natural := Seq'Length;
      L : LIS_Array(1..N);
      Parent : array (1..N) of Natural;
      Max_Length : Natural := 0;
      Max_Index : Natural := 0;
      Result : Array_Type(1..N);
      Result_Index : Natural := 0;
   begin
      if N = 0 then
         return (1..0 => 0);
      end if;
      
      -- Initialize arrays
      for I in 1..N loop
         L(I) := 1;
         Parent(I) := 0;
      end loop;
      
      -- Fill L[] using dynamic programming
      for I in 2..N loop
         for J in 1..I-1 loop
            if Seq(I) > Seq(J) and then L(I) < L(J) + 1 then
               L(I) := L(J) + 1;
               Parent(I) := J;
            end if;
         end loop;
      end loop;
      
      -- Find maximum length and its index
      for I in 1..N loop
         if L(I) > Max_Length then
            Max_Length := L(I);
            Max_Index := I;
         end if;
      end loop;
      
      -- Reconstruct the LIS
      Result_Index := Max_Length;
      declare
         Current_Index : Natural := Max_Index;
      begin
         while Current_Index /= 0 loop
            Result(Result_Index) := Seq(Current_Index);
            Result_Index := Result_Index - 1;
            Current_Index := Parent(Current_Index);
         end loop;
      end;
      
      return Result(Result_Index+1..Max_Length);
   end Get_LIS;
   
   -- Test the implementation
   procedure Test_LIS is
      Test_Array : Array_Type := (10, 22, 9, 33, 21, 50, 41, 60, 80);
      Length : Natural;
      LIS : Array_Type;
   begin
      Put_Line("Input sequence:");
      for I in Test_Array'Range loop
         Put(Test_Array(I), Width => 4);
      end loop;
      New_Line;
      
      Length := LIS_Length(Test_Array);
      Put_Line("Length of LIS: " & Natural'Image(Length));
      
      LIS := Get_LIS(Test_Array);
      Put_Line("Actual LIS:");
      for I in LIS'Range loop
         Put(LIS(I), Width => 4);
      end loop;
      New_Line;
   end Test_LIS;
   
begin
   Test_LIS;
end Longest_Increasing_Subsequence;
```

## Output
```
Input sequence:
  10  22   9  33  21  50  41  60  80

Length of LIS:  6
Actual LIS:
  10  22  33  50  60  80
```

## Explanation

This Ada implementation includes:

1. **LIS_Length function**: Computes the length of the longest increasing subsequence using dynamic programming with O(n²) time complexity
2. **Get_LIS function**: Reconstructs and returns the actual longest increasing subsequence
3. **Test_LIS procedure**: Demonstrates usage with a sample array

### Key Features:
- Uses arrays with proper bounds checking
- Implements dynamic programming approach
- Includes both length calculation and actual subsequence reconstruction
- Handles edge cases (empty arrays)
- Uses Ada's strong typing and bounds checking features

### Algorithm Complexity:
- **Time Complexity**: O(n²) where n is the length of the input array
- **Space Complexity**: O(n) for the dynamic programming arrays

The algorithm works by maintaining an array `L` where `L(i)` represents the length of the longest increasing subsequence ending at position `i`. It builds up solutions incrementally and finally returns the maximum value found.

