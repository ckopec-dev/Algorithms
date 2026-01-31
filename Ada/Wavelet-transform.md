# Wavelet Transform Algorithm in Ada

Here's an example implementation of a simple 1D Haar wavelet transform in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;

procedure Wavelet_Transform is

   -- Define array type for signal processing
   type Signal_Array is array (Positive range <>) of Float;
   type Coefficient_Array is array (Positive range <>) of Float;

   -- Haar wavelet transform - forward transform
   procedure Haar_Transform(Signal : in Signal_Array; 
                           Coefficients : out Coefficient_Array) is
      N : constant Positive := Signal'Length;
      Half_N : constant Positive := N / 2;
   begin
      -- Initialize coefficients array
      Coefficients := (others => 0.0);
      
      -- Perform Haar wavelet transform
      for i in 1..Half_N loop
         -- Approximation coefficients (averages)
         Coefficients(i) := (Signal(2*i-1) + Signal(2*i)) / 2.0;
         
         -- Detail coefficients (differences)
         Coefficients(Half_N + i) := (Signal(2*i-1) - Signal(2*i)) / 2.0;
      end loop;
   end Haar_Transform;

   -- Inverse Haar wavelet transform
   procedure Inverse_Haar_Transform(Coefficients : in Coefficient_Array;
                                   Signal : out Signal_Array) is
      N : constant Positive := Signal'Length;
      Half_N : constant Positive := N / 2;
   begin
      -- Perform inverse transform
      for i in 1..Half_N loop
         -- Reconstruct original signal
         Signal(2*i-1) := Coefficients(i) + Coefficients(Half_N + i);
         Signal(2*i) := Coefficients(i) - Coefficients(Half_N + i);
      end loop;
   end Inverse_Haar_Transform;

   -- Test data
   Test_Signal : constant Signal_Array(1..8) := 
     (1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0);
   
   Coefficients : Coefficient_Array(1..8);
   Reconstructed : Signal_Array(1..8);

begin
   -- Display original signal
   Put_Line("Original Signal:");
   for i in Test_Signal'Range loop
      Put(Test_Signal(i), Fore => 4, Aft => 1, Exp => 0);
      Put(" ");
   end loop;
   New_Line;

   -- Perform forward wavelet transform
   Haar_Transform(Test_Signal, Coefficients);
   
   -- Display coefficients
   Put_Line("Wavelet Coefficients:");
   for i in Coefficients'Range loop
      Put(Coefficients(i), Fore => 6, Aft => 2, Exp => 0);
      Put(" ");
   end loop;
   New_Line;

   -- Perform inverse transform
   Inverse_Haar_Transform(Coefficients, Reconstructed);
   
   -- Display reconstructed signal
   Put_Line("Reconstructed Signal:");
   for i in Reconstructed'Range loop
      Put(Reconstructed(i), Fore => 4, Aft => 1, Exp => 0);
      Put(" ");
   end loop;
   New_Line;

end Wavelet_Transform;
```

## Key Features of this Implementation:

1. **Type Definitions**: Uses Ada's strong typing with custom array types for signals and coefficients
2. **Forward Transform**: Implements the basic Haar wavelet transform algorithm
3. **Inverse Transform**: Reconstructs the original signal from coefficients
4. **Modular Design**: Separate procedures for forward and inverse transforms
5. **Array Handling**: Proper use of Ada array bounds and indexing
6. **Output Formatting**: Clean display of results using Ada.Text_IO

## Expected Output:
```
Original Signal:
   1.0   2.0   3.0   4.0   5.0   6.0   7.0   8.0 
Wavelet Coefficients:
   1.50   3.50   5.50   7.50  -0.50  -0.50  -0.50  -0.50 
Reconstructed Signal:
   1.0   2.0   3.0   4.0   5.0   6.0   7.0   8.0 
```

This example demonstrates the core concepts of wavelet transforms in Ada, showing how to work with arrays, implement mathematical algorithms, and handle signal processing operations in the Ada programming language.

