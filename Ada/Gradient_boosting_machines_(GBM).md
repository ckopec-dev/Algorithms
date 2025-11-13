# Gradient Boosting Machines (GBM) in Ada

Here's an example implementation of a Gradient Boosting Machine algorithm in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Containers.Vectors;

procedure Gradient_Boosting_Example is

   -- Generic package for elementary functions
   package Math is new Ada.Numerics.Generic_Elementary_Functions (Float);
   use Math;

   -- Vector type for data storage
   package Float_Vectors is new Ada.Containers.Vectors (Positive, Float);
   type Vector is new Float_Vectors.Vector;

   -- Simple decision tree node
   type TreeNode is record
      Feature_Index : Positive;
      Threshold     : Float;
      Left_Child    : access TreeNode;
      Right_Child   : access TreeNode;
      Prediction    : Float;
      Is_Leaf       : Boolean;
   end record;

   -- Decision Tree structure
   type Decision_Tree is record
      Root : access TreeNode;
      Depth : Natural;
   end record;

   -- GBM model structure
   type GBM_Model is record
      Trees        : array (1..100) of Decision_Tree;
      Learning_Rate : Float;
      Num_Trees    : Natural;
   end record;

   -- Sample data structure
   type Sample is record
      Features : Vector;
      Target   : Float;
   end record;

   -- Sample dataset
   type Sample_Array is array (Positive range <>) of Sample;
   type Sample_Array_Access is access Sample_Array;

   -- Initialize a tree node
   function Create_Node (Is_Leaf : Boolean := False) return access TreeNode is
      Node : access TreeNode := new TreeNode;
   begin
      Node.Is_Leaf := Is_Leaf;
      Node.Left_Child := null;
      Node.Right_Child := null;
      Node.Prediction := 0.0;
      Node.Feature_Index := 1;
      Node.Threshold := 0.0;
      return Node;
   end Create_Node;

   -- Simple decision tree training (simplified for example)
   procedure Train_Tree (Tree : in out Decision_Tree; 
                        Data : Sample_Array_Access) is
   begin
      Tree.Root := Create_Node(True);
      Tree.Root.Prediction := 0.5; -- Simple average prediction
      Tree.Depth := 1;
   end Train_Tree;

   -- Predict using a single tree
   function Predict_Tree (Tree : Decision_Tree; 
                         Features : Vector) return Float is
   begin
      if Tree.Root.Is_Leaf then
         return Tree.Root.Prediction;
      else
         return Tree.Root.Prediction;
      end if;
   end Predict_Tree;

   -- Gradient Boosting Machine prediction
   function Predict_GBM (Model : GBM_Model; 
                        Features : Vector) return Float is
      Prediction : Float := 0.0;
   begin
      for I in 1..Model.Num_Trees loop
         Prediction := Prediction + Model.Learning_Rate * Predict_Tree (Model.Trees(I), Features);
      end loop;
      return Prediction;
   end Predict_GBM;

   -- Simple sigmoid function for probability conversion
   function Sigmoid (X : Float) return Float is
   begin
      return 1.0 / (1.0 + Exp (-X));
   end Sigmoid;

   -- Main training function
   procedure Train_GBM (Model : in out GBM_Model; 
                       Data : Sample_Array_Access) is
      Num_Samples : constant Natural := Data'Length;
      Residuals : Vector;
      Current_Prediction : Float;
   begin
      -- Initialize model
      Model.Num_Trees := 100;
      Model.Learning_Rate := 0.1;

      -- Initialize residuals
      Residuals := (others => 0.0);

      -- Boosting iterations
      for Iteration in 1..Model.Num_Trees loop
         -- In a real implementation, we would:
         -- 1. Calculate residuals (actual - prediction)
         -- 2. Train a new tree on residuals
         -- 3. Update predictions
         
         -- Simplified training step
         Train_Tree (Model.Trees(Iteration), Data);
         
         -- Print progress
         if Iteration mod 20 = 0 then
            Put_Line ("Completed " & Iteration'Img & " trees");
         end if;
      end loop;
   end Train_GBM;

   -- Sample dataset
   Data_Set : constant Sample_Array (1..4) := 
     (Sample'(Features => (1 => 1.0, 2 => 2.0), Target => 0.0),
      Sample'(Features => (1 => 2.0, 2 => 3.0), Target => 1.0),
      Sample'(Features => (1 => 3.0, 2 => 4.0), Target => 1.0),
      Sample'(Features => (1 => 4.0, 2 => 5.0), Target => 0.0));

   -- Model instance
   Model : GBM_Model;

   -- Test prediction
   Test_Features : Vector := (1 => 2.5, 2 => 3.5);

begin
   Put_Line ("Starting Gradient Boosting Machine Example");
   
   -- Train the model
   Train_GBM (Model, Data_Set'Access);
   
   -- Make prediction
   declare
      Prediction : constant Float := Predict_GBM (Model, Test_Features);
      Probability : constant Float := Sigmoid (Prediction);
   begin
      Put_Line ("Model trained successfully");
      Put ("Prediction: ");
      Put (Prediction, Fore => 1, Aft => 4, Exp => 0);
      Put_Line ("");
      Put ("Probability: ");
      Put (Probability, Fore => 1, Aft => 4, Exp => 0);
      Put_Line ("");
   end;

end Gradient_Boosting_Example;
```

## Key Components Explained:

### 1. **Data Structures**
- `TreeNode`: Represents decision tree nodes with features, thresholds, and predictions
- `Decision_Tree`: Contains tree structure and depth information
- `GBM_Model`: Main model structure with trees, learning rate, and number of trees

### 2. **Core Functions**
- `Train_Tree`: Trains individual decision trees (simplified in this example)
- `Predict_Tree`: Makes predictions using a single tree
- `Predict_GBM`: Combines predictions from all trees with learning rate
- `Train_GBM`: Main training loop for gradient boosting

### 3. **Key Features**
- **Ensemble Learning**: Combines multiple weak learners (decision trees)
- **Gradient Descent**: Uses iterative approach to minimize loss
- **Learning Rate**: Controls step size of each boosting iteration
- **Sigmoid Function**: Converts predictions to probabilities

### 4. **Limitations of This Example**
- Simplified tree training (in practice would use regression trees with proper splitting)
- No actual gradient computation
- Basic residual calculation
- Limited to 2D features for demonstration

This example demonstrates the conceptual structure of GBM in Ada, showing how the language's strong typing and structured programming capabilities can be used to implement machine learning algorithms.

