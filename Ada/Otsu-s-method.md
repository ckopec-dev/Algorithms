# Otsu's Method Algorithm in Ada

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;

procedure Otsu_Method is
   
   -- Image data (256 gray levels)
   type Pixel_Array is array (0..255) of Natural;
   
   -- Histogram data
   type Histogram_Array is array (0..255) of Natural;
   
   -- Function to calculate Otsu's threshold
   function Otsu_Threshold(Hist : Histogram_Array) return Natural is
      Total_Pixels : Natural := 0;
      Max_Threshold : Natural := 0;
      Max_Variance : Float := 0.0;
      
      -- Class probabilities
      W0, W1 : Float := 0.0;
      
      -- Class means
      M0, M1 : Float := 0.0;
      
      -- Between-class variance
      Between_Variance : Float := 0.0;
      
      -- Global mean
      Global_Mean : Float := 0.0;
      
      -- Temporary sums
      Sum0, Sum1 : Float := 0.0;
      
   begin
      -- Calculate total number of pixels
      for I in Hist'Range loop
         Total_Pixels := Total_Pixels + Hist(I);
      end loop;
      
      -- Calculate global mean
      for I in Hist'Range loop
         Global_Mean := Global_Mean + Float(I) * Float(Hist(I));
      end loop;
      
      if Total_Pixels > 0 then
         Global_Mean := Global_Mean / Float(Total_Pixels);
      end if;
      
      -- Try each possible threshold
      for Threshold in Hist'Range loop
         -- Calculate probabilities for classes
         W0 := 0.0;
         W1 := 0.0;
         
         Sum0 := 0.0;
         Sum1 := 0.0;
         
         -- Calculate W0 (background)
         for I in 0..Threshold loop
            W0 := W0 + Float(Hist(I));
            Sum0 := Sum0 + Float(I) * Float(Hist(I));
         end loop;
         
         -- Calculate W1 (foreground)
         for I in Threshold+1..Hist'Last loop
            W1 := W1 + Float(Hist(I));
            Sum1 := Sum1 + Float(I) * Float(Hist(I));
         end loop;
         
         -- Normalize probabilities
         if Total_Pixels > 0 then
            W0 := W0 / Float(Total_Pixels);
            W1 := W1 / Float(Total_Pixels);
         end if;
         
         -- Calculate class means
         M0 := 0.0;
         M1 := 0.0;
         
         if W0 > 0.0 then
            M0 := Sum0 / (W0 * Float(Total_Pixels));
         end if;
         
         if W1 > 0.0 then
            M1 := Sum1 / (W1 * Float(Total_Pixels));
         end if;
         
         -- Calculate between-class variance
         Between_Variance := W0 * W1 * (M0 - M1) * (M0 - M1);
         
         -- Update maximum variance and threshold
         if Between_Variance > Max_Variance then
            Max_Variance := Between_Variance;
            Max_Threshold := Threshold;
         end if;
      end loop;
      
      return Max_Threshold;
   end Otsu_Threshold;
   
   -- Sample histogram data (simulated image histogram)
   Sample_Histogram : Histogram_Array :=
     (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
   
   -- Add some sample data to demonstrate
   procedure Initialize_Histogram is
   begin
      -- Create a sample histogram with two peaks (simulating bimodal distribution)
      for I in 0..30 loop
         Sample_Histogram(I) := 10;
      end loop;
      
      for I in 100..150 loop
         Sample_Histogram(I) := 20;
      end loop;
      
      for I in 180..200 loop
         Sample_Histogram(I) := 15;
      end loop;
   end Initialize_Histogram;
   
   Threshold : Natural;
   
begin
   -- Initialize histogram with sample data
   Initialize_Histogram;
   
   -- Calculate Otsu's threshold
   Threshold := Otsu_Threshold(Sample_Histogram);
   
   -- Output results
   Put_Line("Otsu's Method Threshold Calculation");
   Put_Line("==================================");
   Put_Line("Calculated threshold: " & Natural'Image(Threshold));
   
   -- Display some histogram values for verification
   Put_Line("Sample histogram values:");
   for I in 0..30 loop
      Put("  " & Natural'Image(I) & ": " & Natural'Image(Sample_Histogram(I)));
      if I mod 5 = 4 then
         New_Line;
      end if;
   end loop;
   New_Line;
   
end Otsu_Method;
```

## Algorithm Explanation

This Ada implementation of Otsu's method:

1. **Input**: A histogram array representing pixel intensity distribution
2. **Process**: 
   - Calculates total pixels and global mean
   - Tests each possible threshold value (0-255)
   - For each threshold, computes between-class variance
   - Selects the threshold that maximizes the variance
3. **Output**: Optimal threshold value for image binarization

## Key Features

- **Modular Design**: Separate function for threshold calculation
- **Proper Type Declarations**: Uses appropriate array types for image data
- **Error Handling**: Checks for zero total pixels
- **Clear Documentation**: Comments explain each step
- **Sample Data**: Includes initialization of histogram data

The algorithm is particularly useful for automatic threshold selection in image processing applications where you want to separate foreground from background objects.

