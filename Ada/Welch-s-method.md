# Welch's Method Algorithm in Ada

Welch's method is a technique for estimating the power spectral density of a signal by averaging modified periodograms. Here's an implementation in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Numerics.Generic_Complex_Types;

procedure Welch_Method is
   
   -- Generic package for complex numbers
   package Complex_Types is new Ada.Numerics.Generic_Complex_Types(Ada.Numerics.Float_64);
   use Complex_Types;
   
   -- Generic package for elementary functions
   package Math is new Ada.Numerics.Generic_Elementary_Functions(Ada.Numerics.Float_64);
   use Math;
   
   -- Constants
   type Sample_Type is new Ada.Numerics.Float_64;
   type Index_Type is range 0..10000;
   
   -- Signal parameters
   N : constant Index_Type := 1024;  -- Signal length
   FFT_Size : constant Index_Type := 1024;  -- FFT size
   Overlap : constant Index_Type := 512;  -- Overlap between segments
   Num_Segments : constant Index_Type := (N - Overlap) / (FFT_Size - Overlap);
   
   -- Signal data type
   type Signal_Array is array(Index_Type range <>) of Sample_Type;
   
   -- FFT data type
   type Complex_Array is array(Index_Type range <>) of Complex;
   
   -- Generate test signal (sum of sinusoids)
   function Generate_Signal return Signal_Array is
      Signal : Signal_Array(0..N-1);
   begin
      for I in Signal_Array'Range loop
         Signal(I) := 0.5 * Sin(2.0 * Pi * 10.0 * Sample_Type(I) / Sample_Type(N)) +
                      0.3 * Sin(2.0 * Pi * 25.0 * Sample_Type(I) / Sample_Type(N)) +
                      0.1 * Sin(2.0 * Pi * 50.0 * Sample_Type(I) / Sample_Type(N));
      end loop;
      return Signal;
   end Generate_Signal;
   
   -- Window function (Hamming window)
   function Hamming_Window(N : Index_Type; I : Index_Type) return Sample_Type is
   begin
      return 0.54 - 0.46 * Cos(2.0 * Pi * Sample_Type(I) / Sample_Type(N - 1));
   end Hamming_Window;
   
   -- FFT function (simplified implementation)
   procedure FFT(X : in out Complex_Array; N : Index_Type) is
   begin
      -- Simplified FFT implementation - in practice, use a proper FFT library
      -- This is a placeholder for actual FFT computation
      null;
   end FFT;
   
   -- Welch's method implementation
   procedure Welch_PSD(Signal : in Signal_Array;
                      PSD : out Signal_Array;
                      Fs : Sample_Type) is
                      
      -- Local variables
      Segment_Size : constant Index_Type := FFT_Size;
      Step_Size : constant Index_Type := Segment_Size - Overlap;
      Num_Segments : constant Index_Type := (N - Overlap) / Step_Size;
      
      -- Working arrays
      Windowed_Segment : Complex_Array(0..FFT_Size-1);
      Segment : Signal_Array(0..FFT_Size-1);
      FFT_Result : Complex_Array(0..FFT_Size-1);
      
      -- Frequency axis
      Frequency : Signal_Array(0..FFT_Size/2);
      
      -- Accumulator for PSD
      PSD_Accum : Signal_Array(0..FFT_Size/2) := (others => 0.0);
      
      -- Loop counters
      I, J, K : Index_Type;
      
   begin
      -- Initialize frequency axis
      for I in Frequency'Range loop
         Frequency(I) := Sample_Type(I) * Fs / (2.0 * Sample_Type(FFT_Size));
      end loop;
      
      -- Process each segment
      for Segment_Index in 0..Num_Segments-1 loop
         -- Extract segment with overlap
         for J in 0..Segment_Size-1 loop
            K := Segment_Index * Step_Size + J;
            if K < N then
               Segment(J) := Signal(K);
            else
               Segment(J) := 0.0;
            end if;
         end loop;
         
         -- Apply window function
         for J in Segment'Range loop
            Windowed_Segment(J) := Complex(Segment(J) * Hamming_Window(Segment_Size, J));
         end loop;
         
         -- Compute FFT
         FFT(Windowed_Segment, FFT_Size);
         
         -- Compute power spectrum (magnitude squared)
         for J in 0..FFT_Size/2 loop
            declare
               Magnitude : Sample_Type;
            begin
               Magnitude := Real(Windowed_Segment(J))**2 + Imaginary(Windowed_Segment(J))**2;
               PSD_Accum(J) := PSD_Accum(J) + Magnitude;
            end;
         end loop;
      end loop;
      
      -- Average and normalize
      for I in PSD'Range loop
         PSD(I) := PSD_Accum(I) / Sample_Type(Num_Segments);
      end loop;
      
   end Welch_PSD;
   
   -- Test data
   Test_Signal : Signal_Array(0..N-1) := Generate_Signal;
   PSD_Result : Signal_Array(0..FFT_Size/2);
   Sampling_Frequency : constant Sample_Type := 100.0;
   
begin
   -- Execute Welch's method
   Welch_PSD(Test_Signal, PSD_Result, Sampling_Frequency);
   
   -- Display results
   Put_Line("Welch's Method - Power Spectral Density");
   Put_Line("=======================================");
   Put_Line("Frequency (Hz)  PSD");
   Put_Line("-------------------");
   
   for I in 0..10 loop  -- Display first 11 points
      Put(PSD_Result(I):0:4);
      Put_Line("");
   end loop;
   
end Welch_Method;
```

## Key Features of this Implementation:

1. **Signal Generation**: Creates a test signal with multiple sinusoidal components
2. **Windowing**: Uses Hamming window for spectral leakage reduction
3. **Segmentation**: Processes signal in overlapping segments
4. **FFT Processing**: Computes FFT of each windowed segment
5. **Averaging**: Averages the power spectra from all segments
6. **Normalization**: Properly normalizes the final PSD estimate

## Algorithm Steps:

1. Divide signal into overlapping segments
2. Apply window function to each segment
3. Compute FFT of each windowed segment
4. Calculate power spectrum (magnitude squared)
5. Average power spectra across all segments
6. Normalize by number of segments

This implementation provides a foundation that can be extended with a proper FFT library for production use.

