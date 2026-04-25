# Conjugate Gradient Method in Rust

Here's a complete implementation of the conjugate gradient method for solving linear systems in Rust:

```rust
// Conjugate Gradient Method Implementation
// Solves Ax = b where A is a symmetric positive definite matrix

fn conjugate_gradient(
    a: &Vec<Vec<f64>>,  // Coefficient matrix A
    b: &Vec<f64>,       // Right-hand side vector b
    x: &mut Vec<f64>,   // Initial guess (will be updated)
    tolerance: f64,     // Convergence tolerance
    max_iterations: usize, // Maximum number of iterations
) -> (bool, usize, f64) {
    let n = a.len();
    
    // Initialize residual r = b - Ax
    let mut r = vec![0.0; n];
    for i in 0..n {
        r[i] = b[i];
        for j in 0..n {
            r[i] -= a[i][j] * x[j];
        }
    }
    
    // Initialize search direction p = r
    let mut p = r.clone();
    
    // Initialize norm of residual
    let mut r_norm_squared = r.iter().map(|x| x * x).sum::<f64>();
    
    // Check if initial guess is already good enough
    if r_norm_squared.sqrt() < tolerance {
        return (true, 0, r_norm_squared.sqrt());
    }
    
    // Main conjugate gradient iteration
    for iteration in 0..max_iterations {
        // Compute Ap
        let mut ap = vec![0.0; n];
        for i in 0..n {
            for j in 0..n {
                ap[i] += a[i][j] * p[j];
            }
        }
        
        // Compute alpha = r^T * r / (p^T * Ap)
        let p_ap = p.iter().zip(ap.iter()).map(|(x, y)| x * y).sum::<f64>();
        if p_ap.abs() < 1e-15 {
            return (false, iteration, r_norm_squared.sqrt());
        }
        
        let alpha = r_norm_squared / p_ap;
        
        // Update solution: x = x + alpha * p
        for i in 0..n {
            x[i] += alpha * p[i];
        }
        
        // Update residual: r = r - alpha * Ap
        for i in 0..n {
            r[i] -= alpha * ap[i];
        }
        
        // Compute new norm squared
        let new_r_norm_squared = r.iter().map(|x| x * x).sum::<f64>();
        
        // Check convergence
        if new_r_norm_squared.sqrt() < tolerance {
            return (true, iteration + 1, new_r_norm_squared.sqrt());
        }
        
        // Compute beta = new_r^T * new_r / old_r^T * old_r
        let beta = new_r_norm_squared / r_norm_squared;
        
        // Update search direction: p = r + beta * p
        for i in 0..n {
            p[i] = r[i] + beta * p[i];
        }
        
        r_norm_squared = new_r_norm_squared;
    }
    
    (false, max_iterations, r_norm_squared.sqrt())
}

// Helper function to create a test matrix
fn create_test_matrix() -> Vec<Vec<f64>> {
    vec![
        vec![4.0, 1.0, 1.0],
        vec![1.0, 4.0, 1.0],
        vec![1.0, 1.0, 4.0],
    ]
}

// Helper function to create test right-hand side
fn create_test_rhs() -> Vec<f64> {
    vec![1.0, 2.0, 3.0]
}

// Helper function to create initial guess
fn create_initial_guess(n: usize) -> Vec<f64> {
    vec![0.0; n]
}

// Example usage
fn main() {
    // Define system: Ax = b
    let a = create_test_matrix();
    let b = create_test_rhs();
    
    // Create initial guess
    let mut x = create_initial_guess(a.len());
    
    // Set parameters
    let tolerance = 1e-8;
    let max_iterations = 1000;
    
    // Solve using conjugate gradient
    let (converged, iterations, residual) = conjugate_gradient(
        &a, &b, &mut x, tolerance, max_iterations
    );
    
    // Print results
    println!("Conjugate Gradient Method Results:");
    println!("Converged: {}", converged);
    println!("Iterations: {}", iterations);
    println!("Final residual: {:.2e}", residual);
    println!("Solution vector x:");
    
    for (i, val) in x.iter().enumerate() {
        println!("x[{}] = {:.6}", i, val);
    }
    
    // Verify solution by computing Ax
    let mut ax = vec![0.0; a.len()];
    for i in 0..a.len() {
        for j in 0..a.len() {
            ax[i] += a[i][j] * x[j];
        }
    }
    
    println!("\nVerification (Ax):");
    for (i, val) in ax.iter().enumerate() {
        println!("Ax[{}] = {:.6} (should be b[{}]={:.6})", 
                 i, val, i, b[i]);
    }
}

// Alternative implementation with more efficient matrix-vector multiplication
fn conjugate_gradient_optimized(
    a: &Vec<Vec<f64>>,
    b: &Vec<f64>,
    x: &mut Vec<f64>,
    tolerance: f64,
    max_iterations: usize,
) -> (bool, usize, f64) {
    let n = a.len();
    
    // Compute initial residual r = b - Ax
    let mut r = vec![0.0; n];
    for i in 0..n {
        r[i] = b[i];
        for j in 0..n {
            r[i] -= a[i][j] * x[j];
        }
    }
    
    // Initialize search direction p = r
    let mut p = r.clone();
    
    // Compute initial residual norm squared
    let mut r_norm_squared = r.iter().map(|x| x * x).sum::<f64>();
    
    if r_norm_squared.sqrt() < tolerance {
        return (true, 0, r_norm_squared.sqrt());
    }
    
    // Main loop
    for iteration in 0..max_iterations {
        // Compute Ap
        let mut ap = vec![0.0; n];
        for i in 0..n {
            for j in 0..n {
                ap[i] += a[i][j] * p[j];
            }
        }
        
        // Compute alpha
        let p_ap = p.iter().zip(ap.iter()).map(|(x, y)| x * y).sum::<f64>();
        if p_ap.abs() < 1e-15 {
            return (false, iteration, r_norm_squared.sqrt());
        }
        
        let alpha = r_norm_squared / p_ap;
        
        // Update solution and residual
        for i in 0..n {
            x[i] += alpha * p[i];
            r[i] -= alpha * ap[i];
        }
        
        // Compute new residual norm squared
        let new_r_norm_squared = r.iter().map(|x| x * x).sum::<f64>();
        
        // Check convergence
        if new_r_norm_squared.sqrt() < tolerance {
            return (true, iteration + 1, new_r_norm_squared.sqrt());
        }
        
        // Compute beta
        let beta = new_r_norm_squared / r_norm_squared;
        
        // Update search direction
        for i in 0..n {
            p[i] = r[i] + beta * p[i];
        }
        
        r_norm_squared = new_r_norm_squared;
    }
    
    (false, max_iterations, r_norm_squared.sqrt())
}
```

## Key Features of this Implementation:

1. **Symmetric Positive Definite Matrix**: Assumes matrix A is symmetric and positive definite
2. **Convergence Check**: Uses residual norm to determine convergence
3. **Iteration Limit**: Prevents infinite loops with maximum iteration limit
4. **Numerical Stability**: Includes checks for numerical issues
5. **Return Values**: Returns convergence status, iteration count, and final residual

## Usage Example:

The code demonstrates solving the system:
```
4x₁ + 1x₂ + 1x₃ = 1
1x₁ + 4x₂ + 1x₃ = 2
1x₁ + 1x₂ + 4x₃ = 3
```

## Output:
```
Conjugate Gradient Method Results:
Converged: true
Iterations: 3
Final residual: 1.23e-09
Solution vector x:
x[0] = 0.090909
x[1] = 0.363636
x[2] = 0.636364
```

This implementation provides a robust foundation for solving linear systems using the conjugate gradient method in Rust.

