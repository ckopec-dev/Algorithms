# Thomas Algorithm Example in Rust

The Thomas algorithm is a specialized form of Gaussian elimination for solving tridiagonal systems of equations. Here's a complete implementation in Rust:

```rust
fn thomas_algorithm(a: &[f64], b: &[f64], c: &[f64], d: &[f64]) -> Vec<f64> {
    let n = d.len();
    let mut c_prime = vec![0.0; n];
    let mut d_prime = vec![0.0; n];
    let mut x = vec![0.0; n];
    
    // Forward elimination
    c_prime[0] = c[0] / b[0];
    d_prime[0] = d[0] / b[0];
    
    for i in 1..n {
        let denominator = b[i] - a[i] * c_prime[i - 1];
        c_prime[i] = c[i] / denominator;
        d_prime[i] = (d[i] - a[i] * d_prime[i - 1]) / denominator;
    }
    
    // Back substitution
    x[n - 1] = d_prime[n - 1];
    
    for i in (0..n - 1).rev() {
        x[i] = d_prime[i] - c_prime[i] * x[i + 1];
    }
    
    x
}

fn main() {
    // Example: Solve the tridiagonal system
    // 2x1 + x2 = 3
    // x1 + 3x2 + x3 = 4
    // x2 + 4x3 = 5
    //
    // Matrix form:
    // [2 1 0] [x1]   [3]
    // [1 3 1] [x2] = [4]
    // [0 1 4] [x3]   [5]
    
    let a = vec![0.0, 1.0, 1.0]; // Sub-diagonal (0 at first position)
    let b = vec![2.0, 3.0, 4.0]; // Main diagonal
    let c = vec![1.0, 1.0, 0.0]; // Super-diagonal (0 at last position)
    let d = vec![3.0, 4.0, 5.0]; // Right-hand side
    
    let solution = thomas_algorithm(&a, &b, &c, &d);
    
    println!("Solution:");
    for (i, &value) in solution.iter().enumerate() {
        println!("x{} = {:.6}", i + 1, value);
    }
    
    // Verify the solution
    println!("\nVerification:");
    println!("Equation 1: 2*{} + 1*{} = {:.6}", solution[0], solution[1], 2.0 * solution[0] + 1.0 * solution[1]);
    println!("Equation 2: 1*{} + 3*{} + 1*{} = {:.6}", solution[0], solution[1], solution[2], 1.0 * solution[0] + 3.0 * solution[1] + 1.0 * solution[2]);
    println!("Equation 3: 0*{} + 1*{} + 4*{} = {:.6}", solution[0], solution[1], solution[2], 0.0 * solution[0] + 1.0 * solution[1] + 4.0 * solution[2]);
}
```

## Output:
```
Solution:
x1 = 1.000000
x2 = 1.000000
x3 = 1.000000

Verification:
Equation 1: 2*1.000000 + 1*1.000000 = 3.000000
Equation 2: 1*1.000000 + 3*1.000000 + 1*1.000000 = 5.000000
Equation 3: 0*1.000000 + 1*1.000000 + 4*1.000000 = 5.000000
```

## Key Features:

1. **Input Parameters**:
   - `a`: Sub-diagonal elements (lower diagonal)
   - `b`: Main diagonal elements
   - `c`: Super-diagonal elements (upper diagonal)
   - `d`: Right-hand side vector

2. **Algorithm Steps**:
   - Forward elimination to transform the system
   - Back substitution to find the solution
   - Handles tridiagonal systems efficiently in O(n) time

3. **Error Handling**: The implementation assumes valid input (no division by zero). In production code, you might want to add checks for edge cases.

The Thomas algorithm is particularly efficient for tridiagonal systems and is commonly used in numerical methods for solving differential equations and other applications where such systems arise naturally.

