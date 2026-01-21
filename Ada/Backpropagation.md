# Backpropagation Algorithm in Ada

Here's a complete implementation of a simple neural network with backpropagation in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Numerics.Float_Random;

procedure Backpropagation_Example is

   -- Generic package for elementary functions
   package Float_Functions is new Ada.Numerics.Generic_Elementary_Functions (Float);
   use Float_Functions;

   -- Neural network parameters
   type Weight_Array is array (Integer range <>) of Float;
   type Neuron_Array is array (Integer range <>) of Float;
   type Layer_Array is array (Integer range <>) of Neuron_Array;
   
   -- Network structure
   Num_Layers : constant := 3;
   Layer_Sizes : constant array (1..Num_Layers) of Integer := (2, 3, 1);
   
   -- Activation function and its derivative
   function Sigmoid(X : Float) return Float is
   begin
      return 1.0 / (1.0 + Exp(-X));
   end Sigmoid;
   
   function Sigmoid_Derivative(X : Float) return Float is
   begin
      return X * (1.0 - X);
   end Sigmoid_Derivative;
   
   -- Initialize weights with random values
   function Random_Weight return Float is
   begin
      return (Ada.Numerics.Float_Random.Random * 2.0) - 1.0;
   end Random_Weight;
   
   -- Neural Network Structure
   type Network is record
      Weights : array (1..Num_Layers-1) of Weight_Array;
      Biases  : array (1..Num_Layers-1) of Neuron_Array;
      Outputs : Layer_Array;
   end record;
   
   -- Initialize network
   function Initialize_Network return Network is
      Net : Network;
   begin
      for L in 1..Num_Layers-1 loop
         -- Initialize weights
         Net.Weights(L) := Weight_Array'(others => 0.0);
         for I in Net.Weights(L)'First..Net.Weights(L)'Last loop
            Net.Weights(L)(I) := Random_Weight;
         end loop;
         
         -- Initialize biases
         Net.Biases(L) := Neuron_Array'(others => 0.0);
         for I in Net.Biases(L)'First..Net.Biases(L)'Last loop
            Net.Biases(L)(I) := Random_Weight;
         end loop;
      end loop;
      
      -- Initialize outputs
      for L in 1..Num_Layers loop
         Net.Outputs(L) := Neuron_Array'(others => 0.0);
      end loop;
      
      return Net;
   end Initialize_Network;
   
   -- Forward propagation
   procedure Forward_Propagation(Net : in out Network; Input : in Neuron_Array) is
      Current_Input : Neuron_Array := Input;
      Current_Output : Neuron_Array;
   begin
      -- Input layer
      Net.Outputs(1) := Current_Input;
      
      -- Hidden and output layers
      for L in 1..Num_Layers-1 loop
         Current_Output := Neuron_Array'(others => 0.0);
         for N in 1..Layer_Sizes(L+1) loop
            -- Compute weighted sum
            for W in 1..Layer_Sizes(L) loop
               Current_Output(N) := Current_Output(N) + 
                                    Net.Weights(L)(W + (N-1)*Layer_Sizes(L)) * 
                                    Current_Input(W);
            end loop;
            
            -- Add bias
            Current_Output(N) := Current_Output(N) + Net.Biases(L)(N);
            
            -- Apply activation function
            Current_Output(N) := Sigmoid(Current_Output(N));
         end loop;
         
         -- Update for next layer
         Current_Input := Current_Output;
         Net.Outputs(L+1) := Current_Output;
      end loop;
   end Forward_Propagation;
   
   -- Backpropagation algorithm
   procedure Backpropagation(Net : in out Network; 
                            Input : in Neuron_Array;
                            Target : in Float) is
      -- Error calculations
      Delta : array (1..Num_Layers) of Neuron_Array;
      Error : Float;
      
      -- Learning rate
      Learning_Rate : constant Float := 0.1;
   begin
      -- Calculate output layer error
      Delta(Num_Layers) := Neuron_Array'(others => 0.0);
      Delta(Num_Layers)(1) := (Target - Net.Outputs(Num_Layers)(1)) * 
                              Sigmoid_Derivative(Net.Outputs(Num_Layers)(1));
      
      -- Backpropagate errors
      for L in reverse 1..Num_Layers-1 loop
         Delta(L) := Neuron_Array'(others => 0.0);
         for N in 1..Layer_Sizes(L) loop
            Error := 0.0;
            for M in 1..Layer_Sizes(L+1) loop
               Error := Error + Net.Weights(L)(N + (M-1)*Layer_Sizes(L)) * 
                        Delta(L+1)(M);
            end loop;
            Delta(L)(N) := Error * Sigmoid_Derivative(Net.Outputs(L)(N));
         end loop;
      end loop;
      
      -- Update weights and biases
      for L in 1..Num_Layers-1 loop
         for N in 1..Layer_Sizes(L+1) loop
            for M in 1..Layer_Sizes(L) loop
               -- Update weights
               Net.Weights(L)(M + (N-1)*Layer_Sizes(L)) := 
                  Net.Weights(L)(M + (N-1)*Layer_Sizes(L)) + 
                  Learning_Rate * Delta(L+1)(N) * Net.Outputs(L)(M);
            end loop;
            
            -- Update biases
            Net.Biases(L)(N) := Net.Biases(L)(N) + 
                               Learning_Rate * Delta(L+1)(N);
         end loop;
      end loop;
   end Backpropagation;
   
   -- Training function
   procedure Train_Network(Net : in out Network; 
                          Training_Data : in array (1..4, 1..2) of Float;
                          Targets : in array (1..4) of Float;
                          Epochs : in Integer) is
      Input : Neuron_Array(1..2);
      Target : Float;
   begin
      for Epoch in 1..Epochs loop
         for I in 1..4 loop
            Input(1) := Training_Data(I, 1);
            Input(2) := Training_Data(I, 2);
            Target := Targets(I);
            
            -- Forward propagation
            Forward_Propagation(Net, Input);
            
            -- Backpropagation
            Backpropagation(Net, Input, Target);
         end loop;
      end loop;
   end Train_Network;
   
   -- Test function
   procedure Test_Network(Net : in out Network; 
                         Input : in Neuron_Array) is
   begin
      Forward_Propagation(Net, Input);
      Put_Line("Input: " & Float'Image(Input(1)) & ", " & Float'Image(Input(2)));
      Put_Line("Output: " & Float'Image(Net.Outputs(Num_Layers)(1)));
      Put_Line("---");
   end Test_Network;
   
   -- Main program
   Net : Network := Initialize_Network;
   Training_Data : array (1..4, 1..2) of Float := 
      ((0.0, 0.0), (0.0, 1.0), (1.0, 0.0), (1.0, 1.0));
   Targets : array (1..4) of Float := (0.0, 1.0, 1.0, 0.0); -- XOR problem
   
begin
   Put_Line("Neural Network Backpropagation Example");
   Put_Line("=====================================");
   
   -- Train the network
   Put_Line("Training the network...");
   Train_Network(Net, Training_Data, Targets, 10000);
   
   -- Test the network
   Put_Line("Testing the trained network:");
   Test_Network(Net, (0.0, 0.0));
   Test_Network(Net, (0.0, 1.0));
   Test_Network(Net, (1.0, 0.0));
   Test_Network(Net, (1.0, 1.0));
   
   Put_Line("Training completed!");
   
end Backpropagation_Example;
```

## Key Components Explained

### 1. **Network Structure**
- Multi-layer neural network with input, hidden, and output layers
- Weights and biases for each connection
- Output storage for each neuron

### 2. **Forward Propagation**
- Passes input through the network layer by layer
- Applies sigmoid activation function at each neuron
- Computes weighted sums and applies activation

### 3. **Backpropagation Algorithm**
- Calculates error at output layer
- Propagates error backward through the network
- Updates weights and biases using gradient descent
- Uses learning rate to control update magnitude

### 4. **Training Process**
- Iterates through training data multiple times (epochs)
- Performs forward propagation followed by backpropagation
- Adjusts network parameters to minimize error

### 5. **Example Problem**
- Solves the XOR problem (classic neural network benchmark)
- Demonstrates learning capability through backpropagation

This implementation shows the core principles of backpropagation in Ada, including the mathematical operations needed for gradient computation and weight updates.

