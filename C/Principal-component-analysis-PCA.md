# Principal Component Analysis (PCA) in C

Here's a complete implementation of PCA algorithm in C, including data preprocessing, covariance matrix calculation, eigenvalue decomposition, and dimensionality reduction.

```c
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

#define MAX_ROWS 1000
#define MAX_COLS 100

// Structure to hold PCA results
typedef struct {
    double **eigenvectors;
    double *eigenvalues;
    double **transformed_data;
    int n_rows, n_cols, n_components;
} PCA_Result;

// Function to calculate mean of each column
void calculate_means(double data[][MAX_COLS], double means[], int n_rows, int n_cols) {
    for (int j = 0; j < n_cols; j++) {
        means[j] = 0.0;
        for (int i = 0; i < n_rows; i++) {
            means[j] += data[i][j];
        }
        means[j] /= n_rows;
    }
}

// Function to center the data (subtract mean)
void center_data(double data[][MAX_COLS], double means[], int n_rows, int n_cols) {
    for (int i = 0; i < n_rows; i++) {
        for (int j = 0; j < n_cols; j++) {
            data[i][j] -= means[j];
        }
    }
}

// Function to calculate covariance matrix
void calculate_covariance(double data[][MAX_COLS], double cov_matrix[][MAX_COLS], 
                         int n_rows, int n_cols) {
    // Initialize covariance matrix
    for (int i = 0; i < n_cols; i++) {
        for (int j = 0; j < n_cols; j++) {
            cov_matrix[i][j] = 0.0;
        }
    }
    
    // Calculate covariance matrix
    for (int i = 0; i < n_cols; i++) {
        for (int j = 0; j < n_cols; j++) {
            for (int k = 0; k < n_rows; k++) {
                cov_matrix[i][j] += data[k][i] * data[k][j];
            }
            cov_matrix[i][j] /= (n_rows - 1); // Sample covariance
        }
    }
}

// Function to perform eigenvalue decomposition using Jacobi method
void jacobi_eigenvalues(double matrix[][MAX_COLS], double eigenvals[], 
                       double eigenvecs[][MAX_COLS], int n) {
    const double tolerance = 1e-10;
    const int max_iterations = 50;
    
    // Initialize eigenvectors to identity matrix
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            eigenvecs[i][j] = (i == j) ? 1.0 : 0.0;
        }
    }
    
    // Initialize eigenvalues
    for (int i = 0; i < n; i++) {
        eigenvals[i] = matrix[i][i];
    }
    
    int iterations = 0;
    double max_off_diag;
    
    do {
        max_off_diag = 0.0;
        
        // Find maximum off-diagonal element
        for (int i = 0; i < n; i++) {
            for (int j = i + 1; j < n; j++) {
                if (fabs(matrix[i][j]) > max_off_diag) {
                    max_off_diag = fabs(matrix[i][j]);
                }
            }
        }
        
        if (max_off_diag < tolerance) break;
        
        // Find the largest off-diagonal element
        int p, q;
        for (p = 0; p < n; p++) {
            for (q = p + 1; q < n; q++) {
                if (fabs(matrix[p][q]) == max_off_diag) break;
            }
            if (fabs(matrix[p][q]) == max_off_diag) break;
        }
        
        // Calculate rotation angle
        double theta = 0.5 * atan2(2.0 * matrix[p][q], 
                                  eigenvals[p] - eigenvals[q]);
        double c = cos(theta);
        double s = sin(theta);
        
        // Update eigenvalues
        double app = eigenvals[p];
        double aqq = eigenvals[q];
        double apq = matrix[p][q];
        
        eigenvals[p] = c * c * app + s * s * aqq - 2.0 * c * s * apq;
        eigenvals[q] = s * s * app + c * c * aqq + 2.0 * c * s * apq;
        
        // Update off-diagonal elements
        for (int i = 0; i < n; i++) {
            if (i != p && i != q) {
                double api = matrix[i][p];
                double aqi = matrix[i][q];
                matrix[i][p] = c * api - s * aqi;
                matrix[p][i] = matrix[i][p];
                matrix[i][q] = c * aqi + s * api;
                matrix[q][i] = matrix[i][q];
            }
        }
        
        // Update eigenvectors
        for (int i = 0; i < n; i++) {
            double eip = eigenvecs[i][p];
            double eiq = eigenvecs[i][q];
            eigenvecs[i][p] = c * eip - s * eiq;
            eigenvecs[i][q] = s * eip + c * eiq;
        }
        
        iterations++;
    } while (iterations < max_iterations);
}

// Function to sort eigenvalues and eigenvectors in descending order
void sort_eigenvalues(double eigenvals[], double eigenvecs[][MAX_COLS], 
                     int n, int *sorted_indices) {
    // Simple bubble sort for eigenvalues
    for (int i = 0; i < n - 1; i++) {
        for (int j = 0; j < n - i - 1; j++) {
            if (eigenvals[j] < eigenvals[j + 1]) {
                // Swap eigenvalue
                double temp_val = eigenvals[j];
                eigenvals[j] = eigenvals[j + 1];
                eigenvals[j + 1] = temp_val;
                
                // Swap corresponding eigenvector
                for (int k = 0; k < n; k++) {
                    double temp_vec = eigenvecs[k][j];
                    eigenvecs[k][j] = eigenvecs[k][j + 1];
                    eigenvecs[k][j + 1] = temp_vec;
                }
                
                // Update sorted indices
                int temp_idx = sorted_indices[j];
                sorted_indices[j] = sorted_indices[j + 1];
                sorted_indices[j + 1] = temp_idx;
            }
        }
    }
}

// Function to perform PCA
PCA_Result* perform_pca(double data[][MAX_COLS], int n_rows, int n_cols, int n_components) {
    // Allocate memory for result structure
    PCA_Result *result = (PCA_Result*)malloc(sizeof(PCA_Result));
    result->n_rows = n_rows;
    result->n_cols = n_cols;
    result->n_components = n_components;
    
    // Allocate memory for results
    result->eigenvalues = (double*)malloc(n_cols * sizeof(double));
    result->eigenvectors = (double**)malloc(n_cols * sizeof(double*));
    for (int i = 0; i < n_cols; i++) {
        result->eigenvectors[i] = (double*)malloc(n_cols * sizeof(double));
    }
    
    // Allocate memory for transformed data
    result->transformed_data = (double**)malloc(n_rows * sizeof(double*));
    for (int i = 0; i < n_rows; i++) {
        result->transformed_data[i] = (double*)malloc(n_components * sizeof(double));
    }
    
    // Step 1: Calculate means
    double means[MAX_COLS];
    calculate_means(data, means, n_rows, n_cols);
    
    // Step 2: Center the data
    center_data(data, means, n_rows, n_cols);
    
    // Step 3: Calculate covariance matrix
    double cov_matrix[MAX_COLS][MAX_COLS];
    calculate_covariance(data, cov_matrix, n_rows, n_cols);
    
    // Step 4: Perform eigenvalue decomposition
    jacobi_eigenvalues(cov_matrix, result->eigenvalues, result->eigenvectors, n_cols);
    
    // Step 5: Sort eigenvalues and eigenvectors (descending order)
    int sorted_indices[MAX_COLS];
    for (int i = 0; i < n_cols; i++) {
        sorted_indices[i] = i;
    }
    sort_eigenvalues(result->eigenvalues, result->eigenvectors, n_cols, sorted_indices);
    
    // Step 6: Transform data to new space
    for (int i = 0; i < n_rows; i++) {
        for (int j = 0; j < n_components; j++) {
            result->transformed_data[i][j] = 0.0;
            for (int k = 0; k < n_cols; k++) {
                result->transformed_data[i][j] += data[i][k] * result->eigenvectors[k][j];
            }
        }
    }
    
    return result;
}

// Function to print matrix
void print_matrix(double matrix[][MAX_COLS], int rows, int cols, const char* name) {
    printf("%s:\n", name);
    for (int i = 0; i < rows; i++) {
        for (int j = 0; j < cols; j++) {
            printf("%.4f ", matrix[i][j]);
        }
        printf("\n");
    }
    printf("\n");
}

// Function to print eigenvalues
void print_eigenvalues(double eigenvals[], int n) {
    printf("Eigenvalues:\n");
    for (int i = 0; i < n; i++) {
        printf("%.4f ", eigenvals[i]);
    }
    printf("\n\n");
}

// Function to free PCA result memory
void free_pca_result(PCA_Result *result) {
    if (result != NULL) {
        free(result->eigenvalues);
        for (int i = 0; i < result->n_cols; i++) {
            free(result->eigenvectors[i]);
        }
        free(result->eigenvectors);
        for (int i = 0; i < result->n_rows; i++) {
            free(result->transformed_data[i]);
        }
        free(result->transformed_data);
        free(result);
    }
}

// Example usage
int main() {
    // Sample dataset (10x4)
    double data[MAX_ROWS][MAX_COLS] = {
        {2.5, 2.4, 1.0, 2.0},
        {0.5, 0.7, 3.0, 1.5},
        {2.2, 2.9, 2.5, 3.0},
        {1.9, 2.2, 1.8, 2.2},
        {3.1, 3.0, 2.8, 1.8},
        {2.3, 2.7, 3.2, 2.5},
        {2.0, 1.6, 1.5, 3.0},
        {1.0, 1.1, 2.0, 1.5},
        {1.5, 1.6, 2.5, 2.0},
        {1.1, 0.9, 1.0, 1.8}
    };
    
    int n_rows = 10;
    int n_cols = 4;
    int n_components = 2;
    
    printf("Original Data:\n");
    for (int i = 0; i < n_rows; i++) {
        for (int j = 0; j < n_cols; j++) {
            printf("%.2f ", data[i][j]);
        }
        printf("\n");
    }
    printf("\n");
    
    // Perform PCA
    PCA_Result *pca_result = perform_pca(data, n_rows, n_cols, n_components);
    
    // Print results
    printf("Eigenvalues:\n");
    for (int i = 0; i < n_cols; i++) {
        printf("%.4f ", pca_result->eigenvalues[i]);
    }
    printf("\n\n");
    
    printf("Eigenvectors:\n");
    for (int i = 0; i < n_cols; i++) {
        for (int j = 0; j < n_cols; j++) {
            printf("%.4f ", pca_result->eigenvectors[i][j]);
        }
        printf("\n");
    }
    printf("\n");
    
    printf("Transformed Data (First %d components):\n", n_components);
    for (int i = 0; i < n_rows; i++) {
        for (int j = 0; j < n_components; j++) {
            printf("%.4f ", pca_result->transformed_data[i][j]);
        }
        printf("\n");
    }
    printf("\n");
    
    // Calculate explained variance ratio
    double total_variance = 0.0;
    for (int i = 0; i < n_cols; i++) {
        total_variance += pca_result->eigenvalues[i];
    }
    
    printf("Explained Variance Ratio:\n");
    for (int i = 0; i < n_components; i++) {
        double ratio = pca_result->eigenvalues[i] / total_variance;
        printf("Component %d: %.4f (%.2f%%)\n", i + 1, ratio, ratio * 100);
    }
    
    // Free memory
    free_pca_result(pca_result);
    
    return 0;
}
```

## Key Features of this PCA Implementation:

### 1. **Data Preprocessing**
- Calculates column means
- Centers the data by subtracting means

### 2. **Covariance Matrix Calculation**
- Computes sample covariance matrix from centered data

### 3. **Eigenvalue Decomposition**
- Uses Jacobi method for finding eigenvalues and eigenvectors
- Sorts results in descending order of eigenvalues

### 4. **Dimensionality Reduction**
- Projects original data onto principal components
- Returns transformed data with specified number of components

### 5. **Output Statistics**
- Eigenvalues and eigenvectors
- Explained variance ratios
- Transformed data

## Compilation and Execution:

```bash
gcc -o pca pca.c -lm
./pca
```

This implementation provides a complete PCA solution in C, suitable for educational purposes and simple applications. For production use, you might want to add more robust error handling and optimization features.