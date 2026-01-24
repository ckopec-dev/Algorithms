# Fast Fourier Transform (FFT) in Ada

Here's an implementation of the Cooley-Tukey FFT algorithm in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Complex_Types; use Ada.Numerics.Complex_Types;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

procedure FFT_Example is
   
   type Complex_Array is array (Positive range <>) of Complex;
   
   -- FFT function implementation
   procedure FFT(Data : in out Complex_Array) is
      N : constant Natural := Data'Length;
      M : constant Natural := Integer(Log(Float(N)));
      
      -- Check if N is a power of 2
      procedure Check_Power_of_2 is
      begin
         if N /= 2**M then
            raise Constraint_Error with "FFT requires input size to be a power of 2";
         end if;
      end Check_Power_of_2;
      
      -- Bit-reversal permutation
      function Bit_Reverse(I : Natural; Bits : Natural) return Natural is
         Result : Natural := 0;
      begin
         for J in 0 .. Bits - 1 loop
            if (I and (2**J)) /= 0 then
               Result := Result or (2**(Bits - 1 - J));
            end if;
         end loop;
         return Result;
      end Bit_Reverse;
      
      -- Twiddle factors
      procedure Compute_Twiddle_Factors(
         W : out Complex_Array;
         N : Natural)
      is
         Two_Pi : constant Float := 2.0 * Ada.Numerics.Pi;
      begin
         for I in W'First .. W'Last loop
            W(I) := Complex(Real(1.0), 
                           -Real(Sin(Two_Pi * Float(I) / Float(N))));
         end loop;
      end Compute_Twiddle_Factors;
      
   begin
      Check_Power_of_2;
      
      -- Bit-reversal permutation
      for I in Data'First .. Data'Last loop
         declare
            J : constant Natural := Bit_Reverse(I - Data'First, M);
         begin
            if J > I - Data'First then
               declare
                  Temp : constant Complex := Data(I);
               begin
                  Data(I) := Data(J + Data'First);
                  Data(J + Data'First) := Temp;
               end;
            end if;
         end;
      end loop;
      
      -- FFT computation
      for L in 1 .. M loop
         declare
            L2 : constant Natural := 2**L;
            W : Complex_Array(0 .. L2/2 - 1);
         begin
            Compute_Twiddle_Factors(W, L2);
            
            for I in 0 .. Data'Last - Data'First by L2 loop
               for J in 0 .. L2/2 - 1 loop
                  declare
                     T : constant Complex := Data(I + J + Data'First) +
                                            W(J) * Data(I + J + L2/2 + Data'First);
                     U : constant Complex := Data(I + J + Data'First) -
                                            W(J) * Data(I + J + L2/2 + Data'First);
                  begin
                     Data(I + J + Data'First) := T;
                     Data(I + J + L2/2 + Data'First) := U;
                  end;
               end loop;
            end loop;
         end;
      end loop;
   end FFT;
   
   -- Test function
   procedure Test_FFT is
      Input : Complex_Array(0 .. 7) := (
         Complex(1.0, 0.0),  -- 1 + 0i
         Complex(1.0, 0.0),  -- 1 + 0i
         Complex(1.0, 0.0),  -- 1 + 0i
         Complex(1.0, 0.0),  -- 1 + 0i
         Complex(0.0, 0.0),  -- 0 + 0i
         Complex(0.0, 0.0),  -- 0 + 0i
         Complex(0.0, 0.0),  -- 0 + 0i
         Complex(0.0, 0.0)   -- 0 + 0i
      );
      
      Output : Complex_Array(0 .. 7);
   begin
      Output := Input;
      FFT(Output);
      
      Put_Line("Input:");
      for I in Input'Range loop
         Put_Line("  " & Float'Image(Real(Input(I))) & 
                  " + " & Float'Image(Imaginary(Input(I))) & "i");
      end loop;
      
      Put_Line("Output:");
      for I in Output'Range loop
         Put_Line("  " & Float'Image(Real(Output(I))) & 
                  " + " & Float'Image(Imaginary(Output(I))) & "i");
      end loop;
   end Test_FFT;
   
begin
   Test_FFT;
end FFT_Example;
```

## Key Features of this Implementation:

1. **Complex Number Support**: Uses Ada's built-in complex number types
2. **Bit-Reversal Permutation**: Implements the standard bit-reversal ordering
3. **Cooley-Tukey Algorithm**: Uses the classic divide-and-conquer approach
4. **Power-of-2 Requirement**: Ensures input size is a power of 2
5. **Error Handling**: Includes constraint checking for valid input sizes

## Usage Notes:

- The input array size must be a power of 2 (2, 4, 8, 16, etc.)
- The algorithm works in-place, modifying the input array
- Output represents the Discrete Fourier Transform (DFT)
- Uses the standard mathematical convention for FFT computation

This implementation demonstrates Ada's strong typing, modularity, and support for mathematical operations while providing a complete FFT algorithm suitable for signal processing applications.

