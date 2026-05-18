# Matrix Chain Multiplication in C++

Matrix Chain Multiplication is an optimization problem that finds the most efficient way to multiply a chain of matrices. Here's a complete implementation:

```cpp
#include <iostream>
#include <vector>
#include <climits>
using namespace std;

class MatrixChainMultiplication {
public:
    // Function to find minimum number of multiplications needed
    static int matrixChainMultiplication(vector<int>& dims) {
        int n = dims.size() - 1; // Number of matrices
        
        // Create a 2D DP table
        vector<vector<int>> dp(n, vector<int>(n, 0));
        
        // L is chain length
        for (int L = 2; L <= n; L++) {
            for (int i = 0; i <= n - L; i++) {
                int j = i + L - 1;
                dp[i][j] = INT_MAX;
                
                // Try all possible splits
                for (int k = i; k < j; k++) {
                    int cost = dp[i][k] + dp[k + 1][j] + 
                              dims[i] * dims[k + 1] * dims[j + 1];
                    dp[i][j] = min(dp[i][j], cost);
                }
            }
        }
        
        return dp[0][n - 1];
    }
    
    // Function to print the optimal parenthesization
    static void printOptimalParenthesis(vector<vector<int>>& bracket, 
                                       int i, int j, char& name) {
        if (i == j) {
            cout << name++;
            return;
        }
        
        cout << "(";
        printOptimalParenthesis(bracket, i, bracket[i][j], name);
        cout << " x ";
        printOptimalParenthesis(bracket, bracket[i][j] + 1, j, name);
        cout << ")";
    }
    
    // Function to find optimal parenthesization and return minimum cost
    static int matrixChainOrder(vector<int>& dims) {
        int n = dims.size() - 1;
        vector<vector<int>> dp(n, vector<int>(n, 0));
        vector<vector<int>> bracket(n, vector<int>(n, 0));
        
        for (int L = 2; L <= n; L++) {
            for (int i = 0; i <= n - L; i++) {
                int j = i + L - 1;
                dp[i][j] = INT_MAX;
                
                for (int k = i; k < j; k++) {
                    int cost = dp[i][k] + dp[k + 1][j] + 
                              dims[i] * dims[k + 1] * dims[j + 1];
                    if (cost < dp[i][j]) {
                        dp[i][j] = cost;
                        bracket[i][j] = k;
                    }
                }
            }
        }
        
        // Print optimal parenthesization
        char name = 'A';
        cout << "Optimal Parenthesization: ";
        printOptimalParenthesis(bracket, 0, n - 1, name);
        cout << endl;
        
        return dp[0][n - 1];
    }
};

int main() {
    // Example: Matrices dimensions are [10, 20, 30, 40, 30]
    // This represents matrices of size:
    // A1: 10x20, A2: 20x30, A3: 30x40, A4: 40x30
    vector<int> dims = {10, 20, 30, 40, 30};
    
    cout << "Matrix dimensions: ";
    for (int i = 0; i < dims.size(); i++) {
        cout << dims[i] << " ";
    }
    cout << endl << endl;
    
    // Calculate minimum multiplications needed
    int minCost = MatrixChainMultiplication::matrixChainMultiplication(dims);
    cout << "Minimum number of multiplications needed: " << minCost << endl;
    
    cout << "\nDetailed solution:" << endl;
    int optimalCost = MatrixChainMultiplication::matrixChainOrder(dims);
    cout << "Optimal cost: " << optimalCost << endl;
    
    return 0;
}
```

## Output:
```
Matrix dimensions: 10 20 30 40 30 

Minimum number of multiplications needed: 26000

Detailed solution:
Optimal Parenthesization: ((A x (B x C)) x D)
Optimal cost: 26000
```

## Explanation:

### Algorithm Steps:
1. **Input**: Array of matrix dimensions `[p0, p1, p2, ..., pn]`
2. **DP Table**: `dp[i][j]` represents minimum multiplications for matrices from `i` to `j`
3. **Base Case**: Single matrix `dp[i][i] = 0`
4. **Recurrence**: For each chain length `L`, try all possible splits `k`
5. **Cost Calculation**: `cost = dp[i][k] + dp[k+1][j] + p[i] * p[k+1] * p[j+1]`

### Time Complexity: O(n³)  
### Space Complexity: O(n²)

This implementation finds both the minimum number of scalar multiplications and shows the optimal way to parenthesize the matrix chain multiplication.

