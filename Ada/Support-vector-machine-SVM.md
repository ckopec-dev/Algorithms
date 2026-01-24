# Support Vector Machine (SVM) in Ada

Here's an example implementation of a simple SVM algorithm in Ada. This implementation demonstrates the core concepts of SVM classification with a basic kernel function.

```ada
-- SVM Implementation in Ada
-- Simple linear SVM classifier

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Numerics.Generic_Elementary_Functions;

procedure SVM_Example is
   
   -- Define a simple 2D point structure
   type Point is record
      X, Y : Float;
   end record;
   
   -- Define a data point with label
   type Data_Point is record
      Features : Point;
      Label    : Integer;  -- +1 or -1
   end record;
   
   -- Define a simple SVM model
   type SVM_Model is record
      Weights : array (1..2) of Float;  -- w1, w2
      Bias    : Float;
      Kernel  : String(1..10);          -- Simple kernel type
   end record;
   
   -- Simple linear kernel function
   function Linear_Kernel(P1, P2 : Point) return Float is
   begin
      return P1.X * P2.X + P1.Y * P2.Y;
   end Linear_Kernel;
   
   -- Calculate dot product of two points
   function Dot_Product(P1, P2 : Point) return Float is
   begin
      return P1.X * P2.X + P1.Y * P2.Y;
   end Dot_Product;
   
   -- Predict function for SVM
   function Predict(Model : SVM_Model; Point : Point) return Integer is
      Result : Float;
   begin
      Result := Model.Weights(1) * Point.X + 
                Model.Weights(2) * Point.Y + 
                Model.Bias;
      
      if Result >= 0.0 then
         return 1;
      else
         return -1;
      end if;
   end Predict;
   
   -- Simple training function (simplified version)
   procedure Train_SVM(Model : in out SVM_Model; 
                       Training_Data : in out array (1..4) of Data_Point) is
      -- Simple weight calculation for demonstration
      -- In a real SVM, this would involve solving optimization problems
      W1, W2, B : Float := 0.0;
      Total_Pos, Total_Neg : Integer := 0;
      
      -- Simple averaging approach for demonstration
   begin
      for I in Training_Data'Range loop
         if Training_Data(I).Label = 1 then
            W1 := W1 + Training_Data(I).Features.X;
            W2 := W2 + Training_Data(I).Features.Y;
            Total_Pos := Total_Pos + 1;
         else
            W1 := W1 - Training_Data(I).Features.X;
            W2 := W2 - Training_Data(I).Features.Y;
            Total_Neg := Total_Neg + 1;
         end if;
      end loop;
      
      -- Normalize weights
      if Total_Pos > 0 then
         W1 := W1 / Float(Total_Pos);
         W2 := W2 / Float(Total_Pos);
      end if;
      
      -- Simple bias calculation
      B := (Float(Total_Pos) - Float(Total_Neg)) / 2.0;
      
      Model.Weights(1) := W1;
      Model.Weights(2) := W2;
      Model.Bias := B;
      Model.Kernel := "Linear";
      
      Put_Line("SVM Model Trained:");
      Put_Line("Weights: (" & Float'Image(W1) & ", " & Float'Image(W2) & ")");
      Put_Line("Bias: " & Float'Image(B));
      Put_Line("Kernel: " & Model.Kernel);
   end Train_SVM;
   
   -- Test function
   procedure Test_SVM(Model : SVM_Model; Test_Point : Point) is
      Prediction : Integer;
   begin
      Prediction := Predict(Model, Test_Point);
      Put_Line("Test Point (" & Float'Image(Test_Point.X) & 
               ", " & Float'Image(Test_Point.Y) & 
               ") -> Predicted Label: " & Integer'Image(Prediction));
   end Test_SVM;
   
   -- Sample training data (simple linearly separable data)
   Training_Set : array (1..4) of Data_Point := (
      (Features => (X => 1.0, Y => 1.0), Label => 1),
      (Features => (X => 2.0, Y => 2.0), Label => 1),
      (Features => (X => -1.0, Y => -1.0), Label => -1),
      (Features => (X => -2.0, Y => -2.0), Label => -1)
   );
   
   -- SVM Model
   My_Model : SVM_Model;
   
   -- Test points
   Test_Points : array (1..2) of Point := (
      (X => 1.5, Y => 1.5),
      (X => -1.5, Y => -1.5)
   );
   
begin
   -- Train the SVM model
   Put_Line("Training SVM Model...");
   Train_SVM(My_Model, Training_Set);
   
   Put_Line("");
   Put_Line("Testing SVM Model:");
   
   -- Test the model
   for I in Test_Points'Range loop
      Test_SVM(My_Model, Test_Points(I));
   end loop;
   
   Put_Line("");
   Put_Line("SVM Implementation Complete!");
   
end SVM_Example;
```

## Key Features of this Ada SVM Implementation:

1. **Data Structures**:
   - `Point` type for 2D coordinates
   - `Data_Point` with features and labels
   - `SVM_Model` to store weights, bias, and kernel type

2. **Core Functions**:
   - `Linear_Kernel` - Simple dot product kernel
   - `Predict` - Makes predictions using the trained model
   - `Train_SVM` - Simple training algorithm (simplified for demonstration)

3. **Simplified Training**:
   - Uses averaging approach for weight calculation
   - Demonstrates the basic concept without complex optimization

4. **Usage**:
   - Trains on simple 2D data points
   - Tests predictions on new data
   - Shows model parameters after training

## Notes:

- This is a **simplified demonstration** of SVM concepts
- A full SVM implementation would require:
  - Support for different kernel functions (RBF, polynomial)
  - Proper optimization algorithms (Sequential Minimal Optimization)
  - Handling of non-linearly separable data
  - More sophisticated training procedures
- The Ada implementation follows standard Ada conventions with proper types and procedures

To compile and run this code, you would need an Ada compiler like GNAT:
```bash
gnatmake svm_example.adb
./svm_example
```

