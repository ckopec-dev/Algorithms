# Random Forest Algorithm in Ada

Here's an example implementation of a Random Forest algorithm in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with Ada.Directories; use Ada.Directories;
with Ada.Containers.Vectors;
with Ada.Random_Floating;

procedure Random_Forest_Example is

   -- Define a simple data point structure
   type Data_Point is record
      Features : array (1 .. 4) of Float;
      Class    : Integer;
   end record;

   -- Vector type for data points
   package Data_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Data_Point);

   -- Simple decision tree node
   type Node is record
      Is_Leaf     : Boolean := False;
      Feature     : Integer := 0;
      Threshold   : Float   := 0.0;
      Left_Child  : access Node := null;
      Right_Child : access Node := null;
      Prediction  : Integer := 0;
   end record;

   -- Decision tree structure
   type Decision_Tree is record
      Root : access Node := null;
      Depth : Integer := 0;
   end record;

   -- Random Forest structure
   type Random_Forest is record
      Trees : array (1 .. 10) of Decision_Tree;
      N_Trees : Integer := 10;
   end record;

   -- Global random number generator
   Rand : Ada.Random_Floating.Random_Number;

   -- Function to calculate Gini impurity
   function Gini_Impurity(Values : array of Integer) return Float is
      Total : Integer := 0;
      Gini  : Float := 0.0;
   begin
      for I in Values'Range loop
         Total := Total + Values(I);
      end loop;
      
      if Total = 0 then
         return 0.0;
      end if;
      
      for I in Values'Range loop
         if Total > 0 then
            declare
               P : Float := Float(Values(I)) / Float(Total);
            begin
               Gini := Gini + P * P;
            end;
         end if;
      end loop;
      
      return 1.0 - Gini;
   end Gini_Impurity;

   -- Function to find best split point
   function Find_Best_Split(Points : Data_Vectors.Vector;
                           Features : array of Integer) return Integer is
      Best_Gini : Float := Float'Last;
      Best_Feature : Integer := 0;
   begin
      for F in Features'Range loop
         -- Simple approach: find median as threshold
         declare
            Feature_Values : array (Points.First_Index .. Points.Last_Index) of Float;
            Median_Value : Float;
         begin
            for I in Points.First_Index .. Points.Last_Index loop
               Feature_Values(I) := Points(I).Features(F);
            end loop;
            
            -- Sort values (simplified approach)
            -- In a real implementation, you would sort properly
            Median_Value := Feature_Values(Points.First_Index + Points.Last_Index) / 2.0;
            
            -- Calculate Gini for this split
            -- This is a simplified version
            if Feature_Values(Points.First_Index) < Best_Gini then
               Best_Gini := Feature_Values(Points.First_Index);
               Best_Feature := F;
            end if;
         end;
      end loop;
      
      return Best_Feature;
   end Find_Best_Split;

   -- Function to build a decision tree
   procedure Build_Tree(Tree : in out Decision_Tree;
                       Points : Data_Vectors.Vector;
                       Max_Depth : Integer := 5) is
      -- Simplified tree building
   begin
      Tree.Depth := Max_Depth;
      Tree.Root := new Node'(Is_Leaf => True, Prediction => 1);
   end Build_Tree;

   -- Function to make prediction using a single tree
   function Predict(Tree : Decision_Tree; Point : Data_Point) return Integer is
   begin
      if Tree.Root = null then
         return 0;
      end if;
      return Tree.Root.Prediction;
   end Predict;

   -- Function to make prediction using random forest
   function Predict_Forest(Forest : Random_Forest; Point : Data_Point) return Integer is
      Votes : array (1 .. 3) of Integer := (0, 0, 0);
      Max_Vote : Integer := 0;
      Max_Class : Integer := 1;
   begin
      for I in 1 .. Forest.N_Trees loop
         declare
            Prediction : Integer := Predict(Forest.Trees(I), Point);
         begin
            if Prediction >= 1 and Prediction <= 3 then
               Votes(Prediction) := Votes(Prediction) + 1;
               if Votes(Prediction) > Max_Vote then
                  Max_Vote := Votes(Prediction);
                  Max_Class := Prediction;
               end if;
            end if;
         end;
      end loop;
      
      return Max_Class;
   end Predict_Forest;

   -- Main random forest training function
   procedure Train_Random_Forest(Forest : in out Random_Forest;
                                Training_Data : Data_Vectors.Vector) is
   begin
      for I in 1 .. Forest.N_Trees loop
         Build_Tree(Forest.Trees(I), Training_Data, 5);
      end loop;
   end Train_Random_Forest;

   -- Sample data points
   Training_Data : Data_Vectors.Vector;
   Sample_Data : array (1 .. 15) of Data_Point := (
      ((1.0, 2.0, 3.0, 4.0), 1),
      ((2.0, 3.0, 4.0, 5.0), 1),
      ((3.0, 4.0, 5.0, 6.0), 2),
      ((4.0, 5.0, 6.0, 7.0), 2),
      ((5.0, 6.0, 7.0, 8.0), 3),
      ((6.0, 7.0, 8.0, 9.0), 3),
      ((7.0, 8.0, 9.0, 10.0), 1),
      ((8.0, 9.0, 10.0, 11.0), 1),
      ((9.0, 10.0, 11.0, 12.0), 2),
      ((10.0, 11.0, 12.0, 13.0), 2),
      ((11.0, 12.0, 13.0, 14.0), 3),
      ((12.0, 13.0, 14.0, 15.0), 3),
      ((13.0, 14.0, 15.0, 16.0), 1),
      ((14.0, 15.0, 16.0, 17.0), 1),
      ((15.0, 16.0, 17.0, 18.0), 2)
   );

   -- Random Forest instance
   My_Forest : Random_Forest;

begin
   -- Initialize random number generator
   Rand := Ada.Random_Floating.Random_Number;
   
   -- Load training data
   for I in Sample_Data'Range loop
      Training_Data.Append(Sample_Data(I));
   end loop;
   
   -- Train the random forest
   Train_Random_Forest(My_Forest, Training_Data);
   
   -- Test prediction
   declare
      Test_Point : Data_Point := ((1.5, 2.5, 3.5, 4.5), 0);
      Prediction : Integer;
   begin
      Prediction := Predict_Forest(My_Forest, Test_Point);
      Put_Line("Prediction for test point: " & Integer'Image(Prediction));
   end;
   
   Put_Line("Random Forest implementation in Ada completed successfully!");
   
end Random_Forest_Example;
```

## Key Features of this Ada Implementation:

1. **Data Structures**: 
   - `Data_Point` record for storing features and class labels
   - `Decision_Tree` and `Random_Forest` structures
   - Vector containers for data handling

2. **Core Functions**:
   - `Gini_Impurity` - Calculates impurity for splitting
   - `Find_Best_Split` - Determines optimal split point
   - `Build_Tree` - Constructs decision trees
   - `Predict_Forest` - Makes ensemble predictions

3. **Ada-Specific Features**:
   - Proper memory management with access types
   - Generic containers for flexible data handling
   - Strong typing and bounds checking
   - Modular design with separate procedures

4. **Simplified Algorithm**:
   - This is a conceptual implementation showing the structure
   - Real-world implementation would include full tree building, feature sampling, and proper splitting criteria

This example demonstrates how to structure a machine learning algorithm in Ada while leveraging the language's strengths in safety, modularity, and type safety.

