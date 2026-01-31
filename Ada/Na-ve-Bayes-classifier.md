# Naïve Bayes Classifier in Ada

Here's an implementation of a Naïve Bayes classifier algorithm in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Containers.Vectors;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

procedure Naive_Bayes_Classifier is

   -- Define a record for data points
   type Data_Point is record
      Features : array (1 .. 4) of Float;  -- Example: sepal length, width, petal length, width
      Class    : String (1 .. 10);         -- Class label
   end record;

   -- Vector type for storing data points
   package Data_Vector is new Ada.Containers.Vectors (Index_Type => Positive, 
                                                      Element_Type => Data_Point);
   use Data_Vector;

   -- Type for probability calculations
   type Probability is new Float;

   -- Naive Bayes classifier
   type NB_Classifier is record
      Classes        : array (1 .. 10) of String (1 .. 10);
      Class_Count    : array (1 .. 10) of Natural;
      Feature_Means  : array (1 .. 10, 1 .. 4) of Float;
      Feature_Std    : array (1 .. 10, 1 .. 4) of Float;
      Total_Count    : Natural;
      Class_Prob     : array (1 .. 10) of Float;
   end record;

   -- Training function
   procedure Train (Classifier : in out NB_Classifier; 
                   Data       : in     Vector) is
      Feature_Sum : array (1 .. 10, 1 .. 4) of Float := (others => (others => 0.0));
      Feature_Sq  : array (1 .. 10, 1 .. 4) of Float := (others => (others => 0.0));
      Class_Index : Natural;
   begin
      -- Initialize classifier
      Classifier.Total_Count := Data.Length;
      Classifier.Class_Count := (others => 0);
      Classifier.Class_Prob  := (others => 0.0);
      Classifier.Feature_Means := (others => (others => 0.0));
      Classifier.Feature_Std   := (others => (others => 0.0));

      -- Count instances of each class
      for I in 1 .. Data.Length loop
         declare
            Class_Name : constant String := Data.Element (I).Class;
         begin
            for J in 1 .. 10 loop
               if Classifier.Classes (J) = Class_Name or Classifier.Classes (J) = "" then
                  if Classifier.Classes (J) = "" then
                     Classifier.Classes (J) := Class_Name;
                  end if;
                  Classifier.Class_Count (J) := Classifier.Class_Count (J) + 1;
                  exit;
               end if;
            end loop;
         end;
      end loop;

      -- Calculate feature means and standard deviations for each class
      for I in 1 .. Data.Length loop
         declare
            Point : constant Data_Point := Data.Element (I);
            Class_Name : constant String := Point.Class;
         begin
            -- Find class index
            Class_Index := 0;
            for J in 1 .. 10 loop
               if Classifier.Classes (J) = Class_Name then
                  Class_Index := J;
                  exit;
               end if;
            end loop;

            if Class_Index > 0 then
               -- Accumulate feature sums
               for F in 1 .. 4 loop
                  Feature_Sum (Class_Index, F) := Feature_Sum (Class_Index, F) + Point.Features (F);
                  Feature_Sq (Class_Index, F) := Feature_Sq (Class_Index, F) + Point.Features (F) ** 2.0;
               end loop;
            end if;
         end;
      end loop;

      -- Calculate means and standard deviations
      for C in 1 .. 10 loop
         if Classifier.Class_Count (C) > 0 then
            for F in 1 .. 4 loop
               Classifier.Feature_Means (C, F) := Feature_Sum (C, F) / Float (Classifier.Class_Count (C));
               -- Standard deviation calculation
               declare
                  Mean : constant Float := Feature_Sum (C, F) / Float (Classifier.Class_Count (C));
                  Variance : constant Float := (Feature_Sq (C, F) / Float (Classifier.Class_Count (C))) - (Mean ** 2.0);
               begin
                  if Variance > 0.0 then
                     Classifier.Feature_Std (C, F) := Sqrt (Variance);
                  else
                     Classifier.Feature_Std (C, F) := 1.0;  -- Avoid zero std dev
                  end if;
               end;
            end loop;
         end if;
      end loop;

      -- Calculate class probabilities
      for C in 1 .. 10 loop
         if Classifier.Class_Count (C) > 0 then
            Classifier.Class_Prob (C) := Float (Classifier.Class_Count (C)) / Float (Classifier.Total_Count);
         end if;
      end loop;
   end Train;

   -- Probability density function for Gaussian distribution
   function Gaussian_PDF (X, Mean, Std : Float) return Float is
      Pi : constant Float := 3.14159265358979323846;
      Denominator : constant Float := Std * Sqrt (2.0 * Pi);
      Exponent : constant Float := -0.5 * ((X - Mean) / Std) ** 2.0;
   begin
      if Denominator = 0.0 then
         return 0.0;
      else
         return (1.0 / Denominator) * Exp (Exponent);
      end if;
   end Gaussian_PDF;

   -- Prediction function
   function Predict (Classifier : NB_Classifier; 
                    Point      : Data_Point) return String is
      Max_Probability : Float := -1.0;
      Best_Class      : Natural := 1;
      Class_Prob      : array (1 .. 10) of Float;
   begin
      -- Calculate probability for each class
      for C in 1 .. 10 loop
         if Classifier.Class_Count (C) > 0 then
            -- Start with class prior probability
            Class_Prob (C) := Classifier.Class_Prob (C);
            
            -- Multiply by likelihood for each feature
            for F in 1 .. 4 loop
               declare
                  Likelihood : constant Float := Gaussian_PDF (Point.Features (F), 
                                                              Classifier.Feature_Means (C, F),
                                                              Classifier.Feature_Std (C, F));
               begin
                  Class_Prob (C) := Class_Prob (C) * Likelihood;
               end;
            end loop;
         else
            Class_Prob (C) := 0.0;
         end if;
      end loop;

      -- Find class with maximum probability
      for C in 1 .. 10 loop
         if Class_Prob (C) > Max_Probability then
            Max_Probability := Class_Prob (C);
            Best_Class := C;
         end if;
      end loop;

      return Classifier.Classes (Best_Class);
   end Predict;

   -- Sample data
   Sample_Data : Vector;
   Classifier  : NB_Classifier;

begin
   -- Create sample training data
   declare
      Point1 : Data_Point := ([5.1, 3.5, 1.4, 0.2], "setosa");
      Point2 : Data_Point := ([4.9, 3.0, 1.4, 0.2], "setosa");
      Point3 : Data_Point := ([6.2, 2.8, 4.8, 1.8], "versicolor");
      Point4 : Data_Point := ([5.6, 2.7, 4.2, 1.3], "versicolor");
      Point5 : Data_Point := ([7.2, 3.0, 5.8, 1.6], "virginica");
      Point6 : Data_Point := ([6.5, 3.2, 5.1, 2.0], "virginica");
   begin
      Append (Sample_Data, Point1);
      Append (Sample_Data, Point2);
      Append (Sample_Data, Point3);
      Append (Sample_Data, Point4);
      Append (Sample_Data, Point5);
      Append (Sample_Data, Point6);
   end;

   -- Train the classifier
   Train (Classifier, Sample_Data);

   -- Make a prediction
   declare
      Test_Point : Data_Point := ([5.0, 3.2, 1.5, 0.3], "");
   begin
      Put_Line ("Testing prediction:");
      Put_Line ("Features: " & Float'Image (Test_Point.Features (1)) & 
                ", " & Float'Image (Test_Point.Features (2)) & 
                ", " & Float'Image (Test_Point.Features (3)) & 
                ", " & Float'Image (Test_Point.Features (4)));
      
      Put_Line ("Predicted class: " & Predict (Classifier, Test_Point));
   end;

end Naive_Bayes_Classifier;
```

## Key Features of this Implementation:

1. **Data Structure**: Uses records for data points with features and class labels
2. **Vector Container**: Implements a vector to store training data
3. **Gaussian Probability**: Implements the Gaussian probability density function
4. **Training Phase**: Calculates mean and standard deviation for each feature per class
5. **Prediction Phase**: Computes posterior probabilities using Bayes' theorem
6. **Naïve Assumption**: Assumes feature independence for probability calculations

## How it Works:

1. **Training**: The classifier calculates mean and standard deviation for each feature in each class
2. **Prediction**: For a new data point, it calculates the probability of belonging to each class using:
   - Prior probability of the class
   - Likelihood of features given the class (using Gaussian distribution)
3. **Classification**: Assigns the data point to the class with the highest probability

This implementation demonstrates the core principles of Naïve Bayes classification while maintaining Ada's strong typing and structured programming features.

