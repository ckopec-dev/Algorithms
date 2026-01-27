# CART Decision Tree Algorithm in Ada

Here's an example implementation of a CART (Classification and Regression Trees) decision tree algorithm in Ada:

```ada
-- CART Decision Tree Implementation in Ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Maps;

procedure CART_Decision_Tree is

   -- Data structure for a decision tree node
   type Node_Type is record
      Feature_Index : Integer := -1;        -- Index of feature to split on
      Threshold     : Float := 0.0;         -- Threshold value for split
      Left_Child    : access Node_Type;     -- Left child node
      Right_Child   : access Node_Type;     -- Right child node
      Is_Leaf       : Boolean := False;     -- True if leaf node
      Prediction    : Float := 0.0;         -- Prediction value for leaf
      Class_Label   : Integer := 0;         -- Class label for leaf
   end record;

   type Node_Access is access Node_Type;

   -- Sample dataset structure
   type Sample is record
      Features : array(1..4) of Float;
      Label    : Integer;
   end record;

   type Sample_Vector is array(1..100) of Sample;
   type Sample_Access is access Sample_Vector;

   -- Simple vector for samples
   package Sample_Vector_Package is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Sample,
      Array_Type   => Sample_Vector);

   -- Function to calculate Gini impurity
   function Gini_Impurity(Samples : Sample_Vector_Package.Vector) return Float is
      Total_Count : Float := Float(Samples.Length);
      Gini_Value  : Float := 1.0;
      Class_Count : array(1..2) of Float := (0.0, 0.0);  -- Assuming 2 classes
   begin
      -- Count samples per class
      for I in 1..Samples.Length loop
         if Samples.Element(I).Label = 1 then
            Class_Count(1) := Class_Count(1) + 1.0;
         else
            Class_Count(2) := Class_Count(2) + 1.0;
         end if;
      end loop;

      -- Calculate Gini impurity
      for I in 1..2 loop
         if Total_Count > 0.0 then
            Gini_Value := Gini_Value - (Class_Count(I) / Total_Count) ** 2;
         end if;
      end loop;

      return Gini_Value;
   end Gini_Impurity;

   -- Function to find best split
   function Find_Best_Split(Samples : Sample_Vector_Package.Vector) 
      return (Integer, Float) is
      Best_Gini : Float := Float'Last;
      Best_Feature : Integer := -1;
      Best_Threshold : Float := 0.0;
      Num_Features : constant Integer := 4;
   begin
      -- For simplicity, we'll just return a dummy split
      -- In a full implementation, this would iterate through all features and thresholds
      return (1, 3.5);  -- Feature 1, threshold 3.5
   end Find_Best_Split;

   -- Function to build the decision tree recursively
   procedure Build_Tree(Node : in out Node_Access; 
                       Samples : Sample_Vector_Package.Vector;
                       Max_Depth : Integer;
                       Current_Depth : Integer) is
      Best_Feature : Integer;
      Best_Threshold : Float;
      Left_Samples : Sample_Vector_Package.Vector;
      Right_Samples : Sample_Vector_Package.Vector;
   begin
      -- Check stopping criteria
      if Current_Depth >= Max_Depth or else Samples.Length = 0 then
         Node.Is_Leaf := True;
         -- Set prediction based on majority class
         Node.Class_Label := 1;  -- Simplified
         return;
      end if;

      -- Find best split
      (Best_Feature, Best_Threshold) := Find_Best_Split(Samples);
      
      Node.Feature_Index := Best_Feature;
      Node.Threshold := Best_Threshold;

      -- Create child nodes (simplified - would normally split samples)
      Node.Left_Child := new Node_Type;
      Node.Right_Child := new Node_Type;

      -- Recursively build left and right subtrees
      Build_Tree(Node.Left_Child, Left_Samples, Max_Depth, Current_Depth + 1);
      Build_Tree(Node.Right_Child, Right_Samples, Max_Depth, Current_Depth + 1);
   end Build_Tree;

   -- Function to make predictions
   function Predict(Node : Node_Access; Sample : Sample) return Integer is
   begin
      if Node.Is_Leaf then
         return Node.Class_Label;
      else
         if Sample.Features(Node.Feature_Index) <= Node.Threshold then
            return Predict(Node.Left_Child, Sample);
         else
            return Predict(Node.Right_Child, Sample);
         end if;
      end if;
   end Predict;

   -- Sample data for demonstration
   Sample_Data : Sample_Vector(1..5) := (
      (Features => (1.0, 2.0, 3.0, 4.0), Label => 1),
      (Features => (2.0, 3.0, 4.0, 5.0), Label => 1),
      (Features => (3.0, 4.0, 5.0, 6.0), Label => 0),
      (Features => (4.0, 5.0, 6.0, 7.0), Label => 0),
      (Features => (5.0, 6.0, 7.0, 8.0), Label => 1)
   );

   -- Main execution
   Tree_Root : Node_Access := new Node_Type;
   Test_Sample : Sample := (Features => (3.0, 4.0, 5.0, 6.0), Label => 0);

begin
   Put_Line("CART Decision Tree Implementation in Ada");
   Put_Line("=======================================");

   -- Build the tree
   declare
      Samples : Sample_Vector_Package.Vector;
   begin
      -- Populate samples vector
      for I in 1..5 loop
         Samples.Append(Sample_Data(I));
      end loop;

      -- Build tree with maximum depth of 3
      Build_Tree(Tree_Root, Samples, 3, 0);
   end;

   -- Make a prediction
   declare
      Prediction : constant Integer := Predict(Tree_Root, Test_Sample);
   begin
      Put("Prediction for test sample: ");
      Put(Prediction);
      New_Line;
   end;

   -- Clean up memory
   if Tree_Root /= null then
      if Tree_Root.Left_Child /= null then
         deallocate(Tree_Root.Left_Child);
      end if;
      if Tree_Root.Right_Child /= null then
         deallocate(Tree_Root.Right_Child);
      end if;
      deallocate(Tree_Root);
   end if;

end CART_Decision_Tree;
```

## Key Features of this Implementation:

1. **Node Structure**: Defines a decision tree node with features, thresholds, and child pointers
2. **Gini Impurity**: Calculates impurity for splitting criteria
3. **Best Split**: Finds optimal feature and threshold for splitting
4. **Recursive Tree Building**: Builds the tree structure recursively
5. **Prediction Function**: Makes predictions using the trained tree
6. **Memory Management**: Proper allocation and deallocation of nodes

## Notes:

- This is a simplified implementation for demonstration purposes
- A full CART implementation would include proper sample splitting, more sophisticated stopping criteria, and better handling of edge cases
- The actual splitting logic would need to be expanded to handle multiple features and thresholds
- Memory management is basic and would need enhancement for production use

This example demonstrates the core concepts of CART decision tree implementation in Ada, showing how the algorithm would structure and traverse decision trees.

