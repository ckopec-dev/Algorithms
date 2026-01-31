# Expectation-Maximization (EM) Algorithm in Ada

Here's a complete implementation of the EM algorithm for Gaussian Mixture Models in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Numerics.Float_Random;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;

procedure EM_Algorithm is
   
   -- Generic package for elementary functions
   package Math is new Ada.Numerics.Generic_Elementary_Functions (Float);
   use Math;
   
   -- Constants
   type Real is new Float;
   type Vector is array (Positive range <>) of Real;
   type Matrix is array (Positive range <>, Positive range <>) of Real;
   
   -- Number of data points
   N : constant Positive := 1000;
   -- Number of mixture components
   K : constant Positive := 3;
   -- Number of features
   D : constant Positive := 2;
   
   -- Data points
   type Data_Array is array (Positive range <>) of Vector (1..D);
   
   -- Parameters for EM algorithm
   type Gaussian_Params is record
      Mean   : Vector (1..D);
      Cov    : Matrix (1..D, 1..D);
      Weight : Real;
   end record;
   
   type GMM_Params is array (1..K) of Gaussian_Params;
   
   -- Initialize random data
   procedure Initialize_Data (Data : out Data_Array) is
   begin
      for I in Data'Range loop
         Data (I) (1) := Real'Floor (Ada.Float_Text_IO.Get_Float * 10.0);
         Data (I) (2) := Real'Floor (Ada.Float_Text_IO.Get_Float * 10.0);
      end loop;
   end Initialize_Data;
   
   -- Calculate Gaussian probability density
   function Gaussian_PDF (X : Vector; Mu : Vector; Sigma : Matrix) return Real is
      Dif : Vector (1..D);
      Det : Real := 0.0;
      Exp_Arg : Real := 0.0;
   begin
      -- Calculate determinant
      Det := Sigma (1,1) * Sigma (2,2) - Sigma (1,2) * Sigma (2,1);
      
      -- Calculate difference
      for I in 1..D loop
         Dif (I) := X (I) - Mu (I);
      end loop;
      
      -- Calculate exponent argument
      Exp_Arg := -0.5 * (Dif (1) * (Sigma (2,2) * Dif (1) - Sigma (1,2) * Dif (2)) +
                         Dif (2) * (Sigma (1,1) * Dif (2) - Sigma (2,1) * Dif (1)));
      
      -- Return PDF value
      return 1.0 / (2.0 * 3.14159 * Sqrt (Det)) * Exp (Exp_Arg);
   end Gaussian_PDF;
   
   -- E-step: Calculate responsibilities
   procedure E_Step (Data : Data_Array; 
                     Params : GMM_Params;
                     Responsibilities : out Matrix) is
      Total_Prob : Real;
   begin
      for I in Data'Range loop
         Total_Prob := 0.0;
         for J in 1..K loop
            Responsibilities (I, J) := Params (J).Weight * 
                                      Gaussian_PDF (Data (I), Params (J).Mean, Params (J).Cov);
            Total_Prob := Total_Prob + Responsibilities (I, J);
         end loop;
         
         -- Normalize responsibilities
         for J in 1..K loop
            Responsibilities (I, J) := Responsibilities (I, J) / Total_Prob;
         end loop;
      end loop;
   end E_Step;
   
   -- M-step: Update parameters
   procedure M_Step (Data : Data_Array;
                     Responsibilities : Matrix;
                     Params : in out GMM_Params) is
      Nk : Vector (1..K) := (others => 0.0);
      New_Means : Vector (1..D);
      New_Covs : Matrix (1..D, 1..D);
      New_Weights : Vector (1..K);
   begin
      -- Calculate Nk (sum of responsibilities for each component)
      for J in 1..K loop
         Nk (J) := 0.0;
         for I in Data'Range loop
            Nk (J) := Nk (J) + Responsibilities (I, J);
         end loop;
      end loop;
      
      -- Update weights
      for J in 1..K loop
         New_Weights (J) := Nk (J) / Real (Data'Length);
      end loop;
      
      -- Update means
      for J in 1..K loop
         New_Means := (others => 0.0);
         for I in Data'Range loop
            New_Means (1) := New_Means (1) + Responsibilities (I, J) * Data (I) (1);
            New_Means (2) := New_Means (2) + Responsibilities (I, J) * Data (I) (2);
         end loop;
         New_Means (1) := New_Means (1) / Nk (J);
         New_Means (2) := New_Means (2) / Nk (J);
         Params (J).Mean := New_Means;
      end loop;
      
      -- Update covariances
      for J in 1..K loop
         New_Covs := (others => (others => 0.0));
         for I in Data'Range loop
            declare
               Diff : Vector (1..D);
            begin
               Diff (1) := Data (I) (1) - Params (J).Mean (1);
               Diff (2) := Data (I) (2) - Params (J).Mean (2);
               New_Covs (1,1) := New_Covs (1,1) + Responsibilities (I, J) * Diff (1) * Diff (1);
               New_Covs (1,2) := New_Covs (1,2) + Responsibilities (I, J) * Diff (1) * Diff (2);
               New_Covs (2,1) := New_Covs (2,1) + Responsibilities (I, J) * Diff (2) * Diff (1);
               New_Covs (2,2) := New_Covs (2,2) + Responsibilities (I, J) * Diff (2) * Diff (2);
            end;
         end loop;
         New_Covs (1,1) := New_Covs (1,1) / Nk (J);
         New_Covs (1,2) := New_Covs (1,2) / Nk (J);
         New_Covs (2,1) := New_Covs (2,1) / Nk (J);
         New_Covs (2,2) := New_Covs (2,2) / Nk (J);
         Params (J).Cov := New_Covs;
      end loop;
      
      -- Update weights
      for J in 1..K loop
         Params (J).Weight := New_Weights (J);
      end loop;
   end M_Step;
   
   -- Calculate log-likelihood
   function Log_Likelihood (Data : Data_Array; Params : GMM_Params) return Real is
      LL : Real := 0.0;
      Prob : Real;
   begin
      for I in Data'Range loop
         Prob := 0.0;
         for J in 1..K loop
            Prob := Prob + Params (J).Weight * 
                    Gaussian_PDF (Data (I), Params (J).Mean, Params (J).Cov);
         end loop;
         LL := LL + Log (Prob);
      end loop;
      return LL;
   end Log_Likelihood;
   
   -- Initialize parameters randomly
   procedure Initialize_Params (Params : out GMM_Params) is
   begin
      for J in 1..K loop
         Params (J).Mean (1) := Real'Floor (Ada.Float_Text_IO.Get_Float * 10.0);
         Params (J).Mean (2) := Real'Floor (Ada.Float_Text_IO.Get_Float * 10.0);
         Params (J).Cov (1,1) := 1.0;
         Params (J).Cov (1,2) := 0.0;
         Params (J).Cov (2,1) := 0.0;
         Params (J).Cov (2,2) := 1.0;
         Params (J).Weight := 1.0 / Real (K);
      end loop;
   end Initialize_Params;
   
   -- Main EM algorithm
   procedure EM_Algorithm_Main is
      Data : Data_Array (1..N);
      Params : GMM_Params;
      Responsibilities : Matrix (1..N, 1..K);
      Old_LL : Real := -1000000.0;
      New_LL : Real;
      Iteration : Positive := 0;
      Max_Iterations : constant Positive := 100;
      Tolerance : constant Real := 1e-6;
   begin
      -- Initialize data and parameters
      Initialize_Data (Data);
      Initialize_Params (Params);
      
      -- EM iterations
      loop
         Iteration := Iteration + 1;
         
         -- E-step
         E_Step (Data, Params, Responsibilities);
         
         -- M-step
         M_Step (Data, Responsibilities, Params);
         
         -- Calculate log-likelihood
         New_LL := Log_Likelihood (Data, Params);
         
         Put_Line ("Iteration: " & Iteration'Image & 
                  " Log-Likelihood: " & New_LL'Image);
         
         -- Check for convergence
         if Iteration > 1 and then Abs (New_LL - Old_LL) < Tolerance then
            Put_Line ("Converged after " & Iteration'Image & " iterations");
            exit;
         end if;
         
         Old_LL := New_LL;
         
         -- Check maximum iterations
         if Iteration >= Max_Iterations then
            Put_Line ("Maximum iterations reached");
            exit;
         end if;
      end loop;
      
      -- Print final parameters
      Put_Line ("Final Parameters:");
      for J in 1..K loop
         Put_Line ("Component " & J'Image);
         Put ("  Mean: (");
         Put (Params (J).Mean (1), Fore => 1, Aft => 2, Exp => 0);
         Put (", ");
         Put (Params (J).Mean (2), Fore => 1, Aft => 2, Exp => 0);
         Put_Line (")");
         Put ("  Weight: ");
         Put (Params (J).Weight, Fore => 1, Aft => 4, Exp => 0);
         Put_Line ("");
      end loop;
   end EM_Algorithm_Main;
   
begin
   EM_Algorithm_Main;
end EM_Algorithm;
```

## Key Features of this EM Implementation:

1. **Data Structure**: Uses arrays and records to represent data points, mixture components, and parameters
2. **E-step**: Calculates posterior probabilities (responsibilities) for each data point belonging to each component
3. **M-step**: Updates the mean, covariance, and weight parameters of each Gaussian component
4. **Convergence Check**: Monitors log-likelihood changes to determine when to stop
5. **Gaussian PDF**: Implements multivariate Gaussian probability density function
6. **Initialization**: Randomly initializes parameters for the algorithm

## How to Compile and Run:

```bash
gnatmake em_algorithm.adb
./em_algorithm
```

This implementation demonstrates the core concepts of the EM algorithm for fitting Gaussian Mixture Models, with proper Ada syntax and structure.

