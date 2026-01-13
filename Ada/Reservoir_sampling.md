# Reservoir Sampling Algorithm in Ada

Reservoir sampling is a family of algorithms for randomly sampling k items from a large dataset of unknown size. Here's an implementation in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;

procedure Reservoir_Sampling is
   
   type Item_Type is record
      Value : Float;
      Index : Integer;
   end record;
   
   type Reservoir_Array is array (Positive range <>) of Item_Type;
   
   -- Function to initialize random number generator
   function Random_Float return Float is
      Gen : Generator;
   begin
      Reset(Gen);
      return Random(Gen);
   end Random_Float;
   
   -- Reservoir sampling algorithm
   procedure Sample_With_Reservoir(Data : in out Reservoir_Array;
                                  Stream_Size : Integer;
                                  Sample_Size : Integer) is
      -- Initialize reservoir with first k items
      Reservoir : Reservoir_Array(1..Sample_Size);
      Current_Index : Integer := 0;
      Random_Value : Float;
   begin
      -- Fill reservoir with first k items
      for I in 1..Sample_Size loop
         if I <= Stream_Size then
            Reservoir(I) := Data(I);
            Current_Index := I;
         end if;
      end loop;
      
      -- Process remaining items
      for I in (Sample_Size + 1)..Stream_Size loop
         Random_Value := Random_Float;
         
         -- With probability k/i, replace a random element in reservoir
         if Random_Value < Float(Sample_Size) / Float(I) then
            -- Choose random position in reservoir
            declare
               Replace_Index : Integer := Integer(Random_Float * Float(Sample_Size)) + 1;
            begin
               Reservoir(Replace_Index) := Data(I);
            end;
         end if;
      end loop;
      
      -- Output the final reservoir
      Put_Line("Sampled items:");
      for I in 1..Sample_Size loop
         Put_Line("Item " & Integer'Image(I) & ": " & Float'Image(Reservoir(I).Value));
      end loop;
   end Sample_With_Reservoir;
   
   -- Test data
   Test_Data : Reservoir_Array(1..10) := 
     ((Value => 1.0, Index => 1),
      (Value => 2.0, Index => 2),
      (Value => 3.0, Index => 3),
      (Value => 4.0, Index => 4),
      (Value => 5.0, Index => 5),
      (Value => 6.0, Index => 6),
      (Value => 7.0, Index => 7),
      (Value => 8.0, Index => 8),
      (Value => 9.0, Index => 9),
      (Value => 10.0, Index => 10));
   
begin
   Put_Line("Reservoir Sampling Example");
   Put_Line("==========================");
   
   -- Sample 3 items from 10 items
   Sample_With_Reservoir(Test_Data, 10, 3);
   
end Reservoir_Sampling;
```

## Key Features of this Implementation:

1. **Generic Structure**: Uses arrays to store data items with their values and indices
2. **Random Number Generation**: Utilizes Ada's built-in random number facilities
3. **Algorithm Logic**: Implements the core reservoir sampling algorithm where:
   - First k items are placed in the reservoir
   - For each subsequent item i (where i > k), it replaces a random item in the reservoir with probability k/i
4. **Proper Probability**: Ensures each item has equal probability of being selected

## How it Works:

1. **Initialization**: Fill the reservoir with the first k items from the stream
2. **Sampling**: For each subsequent item, decide whether to include it in the reservoir based on probability
3. **Replacement**: When replacing, choose a random position in the reservoir

This algorithm is particularly useful when dealing with large datasets that cannot fit in memory, or when the total size of the dataset is unknown in advance.

