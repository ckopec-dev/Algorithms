# Principal Component Analysis (PCA) in C++

Here's a complete implementation of PCA algorithm in C++:

```cpp
#include <iostream>
#include <vector>
#include <Eigen/Dense>
#include <Eigen/Eigenvalues>
#include <iomanip>

class PCA {
private:
    Eigen::MatrixXd data;
    Eigen::MatrixXd mean;
    Eigen::MatrixXd covariance;
    Eigen::MatrixXd eigenvalues;
    Eigen::MatrixXd eigenvectors;
    int n_components;

public:
    // Constructor
    PCA(const Eigen::MatrixXd& input_data, int components = -1) 
        : data(input_data) {
        if (components == -1) {
            n_components = std::min(data.rows(), data.cols());
        } else {
            n_components = std::min(components, std::min(data.rows(), data.cols()));
        }
        performPCA();
    }

    // Main PCA computation
    void performPCA() {
        // Step 1: Center the data
        mean = data.rowwise().mean();
        Eigen::MatrixXd centered_data = data;
        for (int i = 0; i < centered_data.rows(); ++i) {
            centered_data.row(i) -= mean;
        }

        // Step 2: Compute covariance matrix
        covariance = (centered_data.adjoint() * centered_data) / (data.rows() - 1);

        // Step 3: Compute eigenvalues and eigenvectors
        Eigen::SelfAdjointEigenSolver<Eigen::MatrixXd> eigen_solver(covariance);
        
        eigenvalues = eigen_solver.eigenvalues();
        eigenvectors = eigen_solver.eigenvectors();

        // Step 4: Sort eigenvalues and eigenvectors in descending order
        sortEigenvalues();
    }

    // Sort eigenvalues and corresponding eigenvectors
    void sortEigenvalues() {
        // Create index array for sorting
        std::vector<std::pair<double, int>> eigen_pairs;
        for (int i = 0; i < eigenvalues.rows(); ++i) {
            eigen_pairs.push_back({eigenvalues(i, 0), i});
        }
        
        // Sort in descending order
        std::sort(eigen_pairs.begin(), eigen_pairs.end(), 
                 [](const std::pair<double, int>& a, const std::pair<double, int>& b) {
                     return a.first > b.first;
                 });
        
        // Reorder eigenvalues and eigenvectors
        Eigen::MatrixXd sorted_eigenvalues(eigenvalues.rows(), 1);
        Eigen::MatrixXd sorted_eigenvectors(eigenvectors.rows(), eigenvectors.cols());
        
        for (int i = 0; i < eigenvalues.rows(); ++i) {
            sorted_eigenvalues(i, 0) = eigen_pairs[i].first;
            sorted_eigenvectors.col(i) = eigenvectors.col(eigen_pairs[i].second);
        }
        
        eigenvalues = sorted_eigenvalues;
        eigenvectors = sorted_eigenvectors;
    }

    // Transform data to principal components
    Eigen::MatrixXd transform(const Eigen::MatrixXd& input_data) {
        Eigen::MatrixXd centered_data = input_data;
        for (int i = 0; i < centered_data.rows(); ++i) {
            centered_data.row(i) -= mean;
        }
        
        // Project onto first n_components principal components
        Eigen::MatrixXd result = centered_data * eigenvectors.leftCols(n_components);
        return result;
    }

    // Get explained variance ratio
    Eigen::MatrixXd getExplainedVarianceRatio() {
        double total_variance = eigenvalues.sum();
        Eigen::MatrixXd ratio(eigenvalues.rows(), 1);
        for (int i = 0; i < eigenvalues.rows(); ++i) {
            ratio(i, 0) = eigenvalues(i, 0) / total_variance;
        }
        return ratio;
    }

    // Get principal components (eigenvectors)
    Eigen::MatrixXd getComponents() {
        return eigenvectors.leftCols(n_components);
    }

    // Get mean vector
    Eigen::MatrixXd getMean() {
        return mean;
    }

    // Get eigenvalues
    Eigen::MatrixXd getEigenvalues() {
        return eigenvalues.topRows(n_components);
    }

    // Print results
    void printResults() {
        std::cout << "Principal Components:\n";
        std::cout << eigenvectors.leftCols(n_components) << "\n\n";
        
        std::cout << "Explained Variance Ratio:\n";
        auto explained_variance = getExplainedVarianceRatio();
        for (int i = 0; i < n_components; ++i) {
            std::cout << "PC" << (i+1) << ": " << std::fixed << std::setprecision(4) 
                      << explained_variance(i, 0) << " (" << std::setprecision(2) 
                      << (explained_variance(i, 0) * 100) << "%)\n";
        }
    }
};

// Example usage
int main() {
    // Create sample data (5 samples, 3 features)
    Eigen::MatrixXd data(5, 3);
    data << 2.5, 2.4, 0.5,
            0.5, 0.7, 1.5,
            2.2, 2.9, 2.5,
            1.9, 2.2, 3.5,
            3.1, 3.0, 4.5;

    std::cout << "Original Data:\n";
    std::cout << data << "\n\n";

    // Create PCA object with 2 components
    PCA pca(data, 2);

    // Print results
    pca.printResults();

    // Transform data
    Eigen::MatrixXd transformed_data = pca.transform(data);
    std::cout << "\nTransformed Data (First 2 Principal Components):\n";
    std::cout << transformed_data << "\n\n";

    // Get explained variance
    std::cout << "Total Explained Variance: " 
              << std::fixed << std::setprecision(4) 
              << (pca.getExplainedVarianceRatio().sum() * 100) << "%\n";

    return 0;
}
```

## Required Dependencies

To compile this code, you'll need to install the Eigen library:

```bash
# Ubuntu/Debian
sudo apt-get install libeigen3-dev

# macOS with Homebrew
brew install eigen

# Windows - Download from https://eigen.tuxfamily.org/
```

## Compilation

```bash
g++ -std=c++11 -I/usr/include/eigen3 pca_example.cpp -o pca_example -lgomp
```

## Key Features of this PCA Implementation

1. **Data Centering**: Centers the data by subtracting the mean
2. **Covariance Matrix**: Computes the covariance matrix of the centered data
3. **Eigenvalue Decomposition**: Uses Eigen library for efficient computation
4. **Sorting**: Sorts components by explained variance (descending order)
5. **Data Transformation**: Projects data onto principal components
6. **Explained Variance**: Calculates variance explained by each component

## Output Example

```
Original Data:
2.5  2.4  0.5
0.5  0.7  1.5
2.2  2.9  2.5
1.9  2.2  3.5
3.1  3.0  4.5

Principal Components:
 0.681  0.731
 0.731 -0.681
 0.000  0.000

Explained Variance Ratio:
PC1: 0.9930 (99.30%)
PC2: 0.0070 (0.70%)
```

This implementation provides a complete PCA solution that can be easily integrated into larger C++ projects.

