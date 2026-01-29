# Reservoir Sampling Algorithm in Ada

Here's an implementation of the Reservoir Sampling algorithm in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;

procedure Reservoir_Sampling_Example is
   
   type Sample_Type is new Float;
   type Reservoir_Array is array (Positive range <>) of Sample_Type;
   type Reservoir_Access is access all Reservoir_Array;
   
   -- Reservoir sampling procedure
   procedure Sample_Into_Reservoir(Samples : in out Reservoir_Array;
                                  Stream : in out Ada.Numerics.Float_Random.Generator;
                                  Max_Size : Positive) is
      Index : Positive := 1;
      Random_Value : Float;
   begin
      -- Initialize reservoir with first k elements
      for I in Samples'First .. Samples'Last loop
         Samples(I) := Random_Value;
         Index := Index + 1;
      end loop;
      
      -- Process remaining elements
      while not Stream.End_Of_File loop
         Random_Value := Random(Stream);
         Index := Index + 1;
         
         -- With probability 1/Idx, replace a random element in reservoir
         if Random_Value < Float(Max_Size) / Float(Index) then
            declare
               Replace_Index : constant Positive := 
                  Positive(Random_Value * Float(Max_Size)) + 1;
            begin
               Samples(Replace_Index) := Random_Value;
            end;
         end if;
      end loop;
   end Sample_Into_Reservoir;
   
   -- Alternative simpler version that samples from a known stream
   procedure Simple_Reservoir_Sample(Max_Size : Positive) is
      Reservoir : Reservoir_Array(1 .. Max_Size);
      Stream : Ada.Numerics.Float_Random.Generator;
      Count : Natural := 0;
      Random_Value : Float;
   begin
      -- Initialize random generator
      Reset(Stream);
      
      -- Fill reservoir with first k elements
      for I in Reservoir'First .. Reservoir'Last loop
         Reservoir(I) := Random(Stream);
         Count := Count + 1;
      end loop;
      
      -- Process remaining elements
      while Count < 1000 loop  -- Sample 1000 elements total
         Random_Value := Random(Stream);
         Count := Count + 1;
         
         -- With probability k/Count, replace a random element
         if Random_Value < Float(Max_Size) / Float(Count) then
            declare
               Replace_Index : constant Positive := 
                  Positive(Random_Value * Float(Max_Size)) + 1;
            begin
               Reservoir(Replace_Index) := Random_Value;
            end;
         end if;
      end loop;
      
      -- Display results
      Put_Line("Reservoir samples:");
      for I in Reservoir'First .. Reservoir'Last loop
         Put_Line("Sample " & Integer'Image(I) & ": " & 
                 Sample_Type'Image(Reservoir(I)));
      end loop;
   end Simple_Reservoir_Sample;
   
begin
   Put_Line("Reservoir Sampling Example");
   Put_Line("==========================");
   Simple_Reservoir_Sample(5);
end Reservoir_Sampling_Example;
```

## Key Features of this Implementation:

1. **Generic Structure**: The algorithm uses a reservoir array to store k samples
2. **Random Sampling**: Uses Ada's built-in random number generator
3. **Probability Logic**: Implements the core reservoir sampling formula
4. **Memory Efficient**: Only stores k elements regardless of input size
5. **Stream Processing**: Can handle data streams of unknown length

## How it Works:

1. **Initialization**: Fill the reservoir with the first k elements
2. **Processing**: For each subsequent element i (where i > k):
   - Generate a random number between 0 and 1
   - If the random number < k/i, replace a random element in the reservoir
3. **Result**: After processing all elements, the reservoir contains a random sample

## Usage Notes:

- The algorithm maintains uniform probability distribution across all elements
- Time complexity: O(n) where n is the total number of elements
- Space complexity: O(k) where k is the reservoir size
- Works with infinite streams (when the end is unknown)

This implementation demonstrates the fundamental reservoir sampling technique in Ada, suitable for applications like sampling large datasets, online algorithms, and data stream processing.

