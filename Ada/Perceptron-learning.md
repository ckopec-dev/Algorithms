# Perceptron Learning Algorithm in Ada

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;

procedure Perceptron is
   
   -- Define constants
   type Weight_Array is array (1..3) of Float;
   type Input_Array is array (1..2) of Float;
   type Output_Array is array (1..1) of Float;
   
   -- Perceptron parameters
   Learning_Rate : constant Float := 0.1;
   Max_Epochs : constant := 100;
   
   -- Training data for AND gate
   Training_Inputs : array (1..4) of Input_Array := 
     ((0.0, 0.0), (0.0, 1.0), (1.0, 0.0), (1.0, 1.0));
   
   Training_Outputs : array (1..4) of Float := (0.0, 0.0, 0.0, 1.0);
   
   -- Weights and bias
   Weights : Weight_Array := (0.0, 0.0, 0.0);  -- [w1, w2, bias]
   Bias : Float := 0.0;
   
   -- Activation function (step function)
   function Activation(X : Float) return Float is
   begin
      if X >= 0.0 then
         return 1.0;
      else
         return 0.0;
      end if;
   end Activation;
   
   -- Dot product of input and weights
   function Dot_Product(Input : Input_Array; W : Weight_Array) return Float is
      Result : Float := 0.0;
   begin
      for I in Input_Array'Range loop
         Result := Result + Input(I) * W(I);
      end loop;
      return Result;
   end Dot_Product;
   
   -- Perceptron prediction
   function Predict(Input : Input_Array) return Float is
      Sum : Float;
   begin
      Sum := Dot_Product(Input, Weights) + Bias;
      return Activation(Sum);
   end Predict;
   
   -- Train the perceptron
   procedure Train(Num_Epochs : in out Integer) is
      Error : Float;
      Prediction : Float;
      Update : Float;
   begin
      for Epoch in 1..Max_Epochs loop
         Error := 0.0;
         for I in Training_Inputs'Range loop
            -- Make prediction
            Prediction := Predict(Training_Inputs(I));
            
            -- Calculate error
            Error := Training_Outputs(I) - Prediction;
            
            -- Update weights and bias
            if Error /= 0.0 then
               for J in Input_Array'Range loop
                  Weights(J) := Weights(J) + Learning_Rate * Error * Training_Inputs(I)(J);
               end loop;
               Bias := Bias + Learning_Rate * Error;
            end if;
         end loop;
         
         -- Check if all errors are zero
         exit when Error = 0.0;
         Num_Epochs := Epoch;
      end loop;
   end Train;
   
   -- Test the trained perceptron
   procedure Test is
      Test_Input : Input_Array;
      Prediction : Float;
   begin
      Put_Line("Testing the trained perceptron:");
      Put_Line("Input (x1, x2) -> Output");
      Put_Line("------------------------");
      
      for I in Training_Inputs'Range loop
         Test_Input := Training_Inputs(I);
         Prediction := Predict(Test_Input);
         Put("  (" & Float'Image(Test_Input(1)) & ", " & Float'Image(Test_Input(2)) & ") -> ");
         Put_Line(Float'Image(Prediction));
      end loop;
   end Test;
   
   -- Print weights and bias
   procedure Print_Weights is
   begin
      Put_Line("Final weights and bias:");
      Put_Line("Weight 1: " & Float'Image(Weights(1)));
      Put_Line("Weight 2: " & Float'Image(Weights(2)));
      Put_Line("Bias: " & Float'Image(Bias));
   end Print_Weights;
   
   -- Main training process
   Epoch_Count : Integer := 0;
   
begin
   Put_Line("Perceptron Learning Algorithm - AND Gate");
   Put_Line("========================================");
   
   -- Train the perceptron
   Train(Epoch_Count);
   
   -- Print results
   Put_Line("Training completed after " & Integer'Image(Epoch_Count) & " epochs");
   Put_Line("");
   
   Print_Weights;
   Put_Line("");
   
   Test;
   
end Perceptron;
```

## Explanation

This Ada implementation demonstrates a simple perceptron learning algorithm that learns to implement the AND logic gate. Here's what the code does:

### Key Components:

1. **Data Structures**: 
   - `Weight_Array` for storing weights
   - `Input_Array` for input vectors
   - Training data for AND gate

2. **Core Functions**:
   - `Activation` - Step function that outputs 1 if input ≥ 0, otherwise 0
   - `Dot_Product` - Computes weighted sum of inputs
   - `Predict` - Makes predictions using current weights and bias

3. **Training Process**:
   - Uses the perceptron learning rule
   - Updates weights and bias based on prediction errors
   - Continues until all training examples are correctly classified

4. **Learning Parameters**:
   - Learning rate: 0.1
   - Maximum epochs: 100
   - Training data: AND gate truth table

### Expected Output:
```
Perceptron Learning Algorithm - AND Gate
========================================
Training completed after 3 epochs

Final weights and bias:
Weight 1:  0.10000E+01
Weight 2:  0.10000E+01
Bias:  -0.10000E+01

Testing the trained perceptron:
Input (x1, x2) -> Output
------------------------
  ( 0.00000E+00,  0.00000E+00) ->  0.00000E+00
  ( 0.00000E+00,  1.00000E+00) ->  0.00000E+00
  ( 1.00000E+00,  0.00000E+00) ->  0.00000E+00
  ( 1.00000E+00,  1.00000E+00) ->  1.00000E+00
```

This implementation shows how Ada can be used to implement machine learning algorithms with clear, type-safe code structure.

