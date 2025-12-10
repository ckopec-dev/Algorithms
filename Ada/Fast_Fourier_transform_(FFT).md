# Fast Fourier Transform (FFT) in Ada

Here's an implementation of the Cooley-Tukey FFT algorithm in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Complex_Types; use Ada.Numerics.Complex_Types;

package FFT is
   type Complex_Array is array (Positive range <>) of Complex;
   type Index_Type is range 1..2**20;
   
   procedure FFT(X : in out Complex_Array);
   procedure IFFT(X : in out Complex_Array);
   
private
   procedure Bit_Reverse_Copy(X : in out Complex_Array);
   procedure FFT_Recursive(X : in out Complex_Array; 
                          Start : Index_Type; 
                          Step : Index_Type; 
                          N : Index_Type);
end FFT;

package body FFT is

   procedure Bit_Reverse_Copy(X : in out Complex_Array) is
      N : constant Index_Type := X'Length;
      Y : Complex_Array(1..N);
   begin
      for I in 1..N loop
         declare
            Rev : Index_Type := 0;
            Temp : Index_Type := I - 1;
            Bit_Count : Index_Type := 0;
         begin
            while Temp > 0 loop
               Rev := Rev * 2 + (Temp mod 2);
               Temp := Temp / 2;
               Bit_Count := Bit_Count + 1;
            end loop;
            
            -- Adjust for bit length
            for J in Bit_Count..(Integer'Max(X'Length) - 1) loop
               Rev := Rev * 2;
            end loop;
            
            Y(Rev + 1) := X(I);
         end;
      end loop;
      
      X := Y;
   end Bit_Reverse_Copy;

   procedure FFT_Recursive(X : in out Complex_Array; 
                          Start : Index_Type; 
                          Step : Index_Type; 
                          N : Index_Type) is
      W : constant Complex := (Real => 1.0, Imaginary => 0.0);
      Temp : Complex;
   begin
      if N = 1 then
         return;
      end if;
      
      -- Recursive calls for even and odd elements
      FFT_Recursive(X, Start, Step * 2, N / 2);
      FFT_Recursive(X, Start + Step, Step * 2, N / 2);
      
      -- Butterfly operations
      for I in 0..(N/2 - 1) loop
         declare
            Angle : constant Float := -2.0 * Float(I) * 3.14159265358979323846 / Float(N);
            W : constant Complex := (Real => Cos(Angle), Imaginary => Sin(Angle));
            T : constant Complex := X(Start + I * Step + 1) - X(Start + (I + N/2) * Step + 1);
         begin
            X(Start + I * Step + 1) := X(Start + I * Step + 1) + X(Start + (I + N/2) * Step + 1);
            X(Start + (I + N/2) * Step + 1) := W * T;
         end;
      end loop;
   end FFT_Recursive;

   procedure FFT(X : in out Complex_Array) is
      N : constant Index_Type := X'Length;
   begin
      if N <= 1 then
         return;
      end if;
      
      -- Ensure N is a power of 2
      if N and (N - 1) /= 0 then
         raise Constraint_Error with "FFT length must be a power of 2";
      end if;
      
      Bit_Reverse_Copy(X);
      FFT_Recursive(X, 1, 1, N);
   end FFT;

   procedure IFFT(X : in out Complex_Array) is
      N : constant Index_Type := X'Length;
   begin
      if N <= 1 then
         return;
      end if;
      
      -- Ensure N is a power of 2
      if N and (N - 1) /= 0 then
         raise Constraint_Error with "FFT length must be a power of 2";
      end if;
      
      Bit_Reverse_Copy(X);
      FFT_Recursive(X, 1, 1, N);
      
      -- Normalize by dividing by N
      for I in 1..N loop
         X(I) := X(I) / Complex(N);
      end loop;
   end IFFT;

end FFT;

-- Example usage
procedure Test_FFT is
   use FFT;
   
   -- Create test signal (simple sine wave)
   Signal : Complex_Array(1..8) := (
      (Real => 1.0, Imaginary => 0.0),
      (Real => 0.0, Imaginary => 0.0),
      (Real => -1.0, Imaginary => 0.0),
      (Real => 0.0, Imaginary => 0.0),
      (Real => 1.0, Imaginary => 0.0),
      (Real => 0.0, Imaginary => 0.0),
      (Real => -1.0, Imaginary => 0.0),
      (Real => 0.0, Imaginary => 0.0)
   );
   
   procedure Print_Complex(C : Complex) is
   begin
      Put(Real(C), 0, 2);
      if Imaginary(C) >= 0.0 then
         Put(" + ");
      else
         Put(" - ");
      end if;
      Put(Abs(Imaginary(C)), 0, 2);
      Put_Line("i");
   end Print_Complex;
   
begin
   Put_Line("Original signal:");
   for I in Signal'Range loop
      Put("X(");
      Put(I);
      Put(") = ");
      Print_Complex(Signal(I));
   end loop;
   
   FFT(Signal);
   
   Put_Line("FFT result:");
   for I in Signal'Range loop
      Put("X(");
      Put(I);
      Put(") = ");
      Print_Complex(Signal(I));
   end loop;
   
end Test_FFT;
```

## Key Features of this Implementation:

1. **Complex Number Support**: Uses Ada's built-in complex number types
2. **Bit-Reversal Permutation**: Efficiently reorders input data
3. **Recursive Implementation**: Implements the Cooley-Tukey algorithm
4. **Butterfly Operations**: Core computation steps of FFT
5. **Error Handling**: Validates input length requirements
6. **Inverse FFT Support**: Includes both FFT and IFFT procedures

## Usage Notes:

- The input array length must be a power of 2 (2, 4, 8, 16, etc.)
- The `FFT` procedure modifies the input array in-place
- The `IFFT` procedure performs inverse FFT and normalizes the result
- Complex numbers are represented as `(Real => real_part, Imaginary => imaginary_part)`

This implementation demonstrates Ada's strong typing, modular design, and built-in mathematical capabilities while providing a complete FFT algorithm suitable for signal processing applications.

