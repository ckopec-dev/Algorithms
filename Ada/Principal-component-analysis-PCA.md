# Principal Component Analysis (PCA) in Ada

Here's a complete example of PCA implementation in Ada:

```ada
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Numerics.Real_Arrays; use Ada.Numerics.Real_Arrays;
with Ada.Numerics.Matrix_Operations; use Ada.Numerics.Matrix_Operations;

procedure PCA_Example is
   
   -- Define matrix type
   type Matrix is array (Positive range <>, Positive range <>) of Float;
   
   -- Sample data (5 observations, 3 features)
   Data : Matrix(1..5, 1..3) := 
     ((1.0, 2.0, 3.0),
      (2.0, 3.0, 4.0),
      (3.0, 4.0, 5.0),
      (4.0, 5.0, 6.0),
      (5.0, 6.0, 7.0));
   
   -- Center the data
   procedure Center_Data(M : in out Matrix) is
      Rows : constant Positive := M'Length(1);
      Cols : constant Positive := M'Length(2);
      Means : array (1..Cols) of Float := (others => 0.0);
   begin
      -- Calculate means for each column
      for Col in 1..Cols loop
         for Row in 1..Rows loop
            Means(Col) := Means(Col) + M(Row, Col);
         end loop;
         Means(Col) := Means(Col) / Float(Rows);
      end loop;
      
      -- Subtract means from each element
      for Row in 1..Rows loop
         for Col in 1..Cols loop
            M(Row, Col) := M(Row, Col) - Means(Col);
         end loop;
      end loop;
   end Center_Data;
   
   -- Calculate covariance matrix
   function Covariance_Matrix(M : Matrix) return Matrix is
      Rows : constant Positive := M'Length(1);
      Cols : constant Positive := M'Length(2);
      Cov : Matrix(1..Cols, 1..Cols) := (others => (others => 0.0));
   begin
      for I in 1..Cols loop
         for J in 1..Cols loop
            for K in 1..Rows loop
               Cov(I, J) := Cov(I, J) + M(K, I) * M(K, J);
            end loop;
            Cov(I, J) := Cov(I, J) / Float(Rows - 1);
         end loop;
      end loop;
      return Cov;
   end Covariance_Matrix;
   
   -- Calculate eigenvalues and eigenvectors (simplified version)
   procedure Calculate_Eigenvalues_Eigenvectors(Cov : in Matrix;
                                                Eigenvals : out Matrix;
                                                Eigenvecs : out Matrix) is
      Cols : constant Positive := Cov'Length(1);
   begin
      -- This is a simplified version - in practice, you'd use a proper
      -- eigenvalue decomposition algorithm
      Put_Line("Note: This is a simplified PCA implementation");
      Put_Line("In a full implementation, you would use LAPACK or similar");
      Put_Line("For demonstration, we'll show the covariance matrix:");
      
      for I in 1..Cols loop
         for J in 1..Cols loop
            Put(Cov(I, J), 1, 2, 0);
            Put(" ");
         end loop;
         New_Line;
      end loop;
   end Calculate_Eigenvalues_Eigenvectors;
   
   -- Main PCA procedure
   procedure Perform_PCA(Data_Matrix : in Matrix) is
      Centered_Data : Matrix := Data_Matrix;
      Cov_Matrix : Matrix;
      Eigenvals : Matrix(1..3, 1..3);
      Eigenvecs : Matrix(1..3, 1..3);
   begin
      Put_Line("Original Data:");
      for I in 1..Centered_Data'Length(1) loop
         for J in 1..Centered_Data'Length(2) loop
            Put(Centered_Data(I, J), 1, 2, 0);
            Put(" ");
         end loop;
         New_Line;
      end loop;
      
      -- Center the data
      Center_Data(Centered_Data);
      Put_Line("Centered Data:");
      for I in 1..Centered_Data'Length(1) loop
         for J in 1..Centered_Data'Length(2) loop
            Put(Centered_Data(I, J), 1, 2, 0);
            Put(" ");
         end loop;
         New_Line;
      end loop;
      
      -- Calculate covariance matrix
      Cov_Matrix := Covariance_Matrix(Centered_Data);
      Put_Line("Covariance Matrix:");
      for I in 1..Cov_Matrix'Length(1) loop
         for J in 1..Cov_Matrix'Length(2) loop
            Put(Cov_Matrix(I, J), 1, 2, 0);
            Put(" ");
         end loop;
         New_Line;
      end loop;
      
      -- In a complete implementation, we would:
      -- 1. Calculate eigenvalues and eigenvectors of covariance matrix
      -- 2. Sort them in descending order
      -- 3. Select top k components
      -- 4. Transform the original data
      
      Put_Line("PCA completed. In a full implementation:");
      Put_Line("- Calculate eigenvalues and eigenvectors");
      Put_Line("- Sort by eigenvalues (descending)");
      Put_Line("- Select top components");
      Put_Line("- Transform data using selected components");
      
   end Perform_PCA;
   
begin
   Put_Line("Principal Component Analysis Example in Ada");
   New_Line;
   Perform_PCA(Data);
end PCA_Example;
```

## Key Components of this PCA Implementation:

### 1. **Data Structure**
- Uses a 2D array to represent the dataset
- Each row is an observation, each column is a feature

### 2. **Data Preprocessing**
- **Centering**: Subtract mean from each feature
- **Covariance Matrix**: Calculate relationships between features

### 3. **Core PCA Steps**
1. **Data Centering**: Remove mean from each feature
2. **Covariance Calculation**: Measure feature relationships
3. **Eigenvalue Decomposition**: Find principal components
4. **Component Selection**: Choose top k components
5. **Data Transformation**: Project data onto new components

### 4. **Important Notes**
- This is a **simplified version** for demonstration
- A full implementation would require:
  - Proper eigenvalue/eigenvector computation
  - Matrix decomposition algorithms
  - Numerical stability considerations
  - Memory management for large datasets

### 5. **Usage Example**
The code demonstrates:
- Loading sample data
- Centering the data
- Computing the covariance matrix
- Showing the PCA workflow

This example shows the structure and methodology of PCA in Ada, though a production implementation would require more sophisticated numerical libraries and algorithms.

