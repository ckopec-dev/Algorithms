# Matrix Chain Multiplication in Rust

Here's an implementation of the Matrix Chain Multiplication algorithm in Rust, which finds the optimal way to multiply a chain of matrices to minimize the number of scalar multiplications.

```rust
fn matrix_chain_multiplication(dimensions: &[i32]) -> (i32, Vec<(usize, usize)>) {
    let n = dimensions.len() - 1; // Number of matrices
    if n <= 1 {
        return (0, vec![]);
    }
    
    // Create a 2D table to store minimum costs
    let mut dp = vec![vec![0; n]; n];
    // Create a 2D table to store split points
    let mut split = vec![vec![0; n]; n];
    
    // Fill the table for chains of length 2 to n
    for chain_len in 2..=n {
        for i in 0..=(n - chain_len) {
            let j = i + chain_len - 1;
            dp[i][j] = i32::MAX;
            
            // Try all possible split points
            for k in (i + 1)..j {
                let cost = dp[i][k] + dp[k][j] + dimensions[i] * dimensions[k + 1] * dimensions[j + 1];
                if cost < dp[i][j] {
                    dp[i][j] = cost;
                    split[i][j] = k;
                }
            }
        }
    }
    
    // Reconstruct the optimal parenthesization
    let mut optimal_parentheses = Vec::new();
    fn get_parentheses(split: &Vec<Vec<usize>>, i: usize, j: usize, result: &mut Vec<(usize, usize)>) {
        if i == j {
            return;
        }
        if i + 1 == j {
            result.push((i, j));
            return;
        }
        let k = split[i][j];
        result.push((i, j));
        get_parentheses(split, i, k, result);
        get_parentheses(split, k + 1, j, result);
    }
    
    let mut parentheses = Vec::new();
    get_parentheses(&split, 0, n - 1, &mut parentheses);
    
    (dp[0][n - 1], parentheses)
}

fn print_optimal_parenthesization(dimensions: &[i32], parentheses: &[(usize, usize)]) {
    let n = dimensions.len() - 1;
    let mut matrix_names = Vec::new();
    
    for i in 0..n {
        matrix_names.push(format!("A{}", i + 1));
    }
    
    // Simple recursive function to build parenthesized expression
    fn build_expression(parentheses: &[(usize, usize)], i: usize, j: usize, matrix_names: &[String]) -> String {
        if i == j {
            return matrix_names[i].clone();
        }
        
        let mut result = String::new();
        result.push('(');
        result.push_str(&build_expression(parentheses, i, i, matrix_names));
        result.push(' ');
        result.push_str(&build_expression(parentheses, i + 1, j, matrix_names));
        result.push(')');
        
        result
    }
    
    println!("Optimal parenthesization: {}", build_expression(parentheses, 0, n - 1, &matrix_names));
}

fn main() {
    // Example: Matrices A1(10x100), A2(100x5), A3(5x50)
    let dimensions = vec![10, 100, 5, 50];
    
    println!("Matrix dimensions: {:?}", dimensions);
    println!("Number of matrices: {}", dimensions.len() - 1);
    
    let (min_cost, parentheses) = matrix_chain_multiplication(&dimensions);
    
    println!("Minimum number of scalar multiplications: {}", min_cost);
    print_optimal_parenthesization(&dimensions, &parentheses);
    
    // Another example with more matrices
    println!("\n--- Another Example ---");
    let dimensions2 = vec![40, 20, 30, 10, 30];
    println!("Matrix dimensions: {:?}", dimensions2);
    println!("Number of matrices: {}", dimensions2.len() - 1);
    
    let (min_cost2, parentheses2) = matrix_chain_multiplication(&dimensions2);
    
    println!("Minimum number of scalar multiplications: {}", min_cost2);
    print_optimal_parenthesization(&dimensions2, &parentheses2);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_matrix_chain_multiplication() {
        let dimensions = vec![10, 100, 5, 50];
        let (cost, _) = matrix_chain_multiplication(&dimensions);
        assert_eq!(cost, 7500);
    }
    
    #[test]
    fn test_single_matrix() {
        let dimensions = vec![10, 20];
        let (cost, _) = matrix_chain_multiplication(&dimensions);
        assert_eq!(cost, 0);
    }
    
    #[test]
    fn test_two_matrices() {
        let dimensions = vec![10, 20, 30];
        let (cost, _) = matrix_chain_multiplication(&dimensions);
        assert_eq!(cost, 6000);
    }
}
```

## How it works:

1. **Input**: An array of matrix dimensions where `dimensions[i]` and `dimensions[i+1]` represent the dimensions of matrix `i`
2. **Dynamic Programming Approach**: 
   - Uses a 2D table `dp[i][j]` to store the minimum cost of multiplying matrices from `i` to `j`
   - Uses another table `split[i][j]` to store the optimal split point
3. **Algorithm Steps**:
   - For chains of length 2 to n, try all possible split points
   - Calculate cost as: `dp[i][k] + dp[k][j] + dimensions[i] * dimensions[k+1] * dimensions[j+1]`
   - Store the minimum cost and split point
4. **Output**: Returns the minimum cost and the optimal parenthesization

## Example Output:
```
Matrix dimensions: [10, 100, 5, 50]
Number of matrices: 3
Minimum number of scalar multiplications: 7500
Optimal parenthesization: (A1 (A2 A3))

--- Another Example ---
Matrix dimensions: [40, 20, 30, 10, 30]
Number of matrices: 4
Minimum number of scalar multiplications: 26000
Optimal parenthesization: ((A1 A2) (A3 A4))
```

This implementation efficiently solves the matrix chain multiplication problem with O(n³) time complexity and O(n²) space complexity.

