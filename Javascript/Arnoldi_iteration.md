# Arnoldi Iteration Algorithm in JavaScript

```javascript
class ArnoldiIteration {
    constructor(matrix, maxIterations = 100, tolerance = 1e-10) {
        this.matrix = matrix;
        this.maxIterations = maxIterations;
        this.tolerance = tolerance;
        this.n = matrix.length;
    }

    // Matrix-vector multiplication
    multiplyVector(matrix, vector) {
        const result = new Array(matrix.length).fill(0);
        for (let i = 0; i < matrix.length; i++) {
            for (let j = 0; j < matrix[i].length; j++) {
                result[i] += matrix[i][j] * vector[j];
            }
        }
        return result;
    }

    // Vector dot product
    dotProduct(vector1, vector2) {
        let sum = 0;
        for (let i = 0; i < vector1.length; i++) {
            sum += vector1[i] * vector2[i];
        }
        return sum;
    }

    // Vector normalization
    normalize(vector) {
        const norm = Math.sqrt(this.dotProduct(vector, vector));
        if (norm < this.tolerance) return vector;
        return vector.map(x => x / norm);
    }

    // Orthogonalize vector against all columns in matrix
    orthogonalize(vector, basis) {
        const result = [...vector];
        for (let i = 0; i < basis.length; i++) {
            const proj = this.dotProduct(result, basis[i]);
            for (let j = 0; j < result.length; j++) {
                result[j] -= proj * basis[i][j];
            }
        }
        return result;
    }

    // Arnoldi iteration algorithm
    run(startVector = null) {
        if (startVector === null) {
            startVector = new Array(this.n).fill(0);
            startVector[0] = 1; // Start with first standard basis vector
        }

        // Initialize
        const H = []; // Hessenberg matrix
        const V = []; // Basis vectors
        let v = this.normalize(startVector);
        V.push(v);

        // Arnoldi iteration
        for (let k = 0; k < this.maxIterations; k++) {
            // Matrix-vector multiplication
            const w = this.multiplyVector(this.matrix, v);
            
            // Orthogonalization against all previous basis vectors
            const h = new Array(k + 1).fill(0);
            for (let i = 0; i <= k; i++) {
                h[i] = this.dotProduct(w, V[i]);
                for (let j = 0; j < w.length; j++) {
                    w[j] -= h[i] * V[i][j];
                }
            }
            
            // Add new row to Hessenberg matrix
            H.push(h);
            
            // Check for convergence
            const norm = Math.sqrt(this.dotProduct(w, w));
            if (norm < this.tolerance) {
                console.log(`Converged after ${k + 1} iterations`);
                break;
            }
            
            // Normalize and add to basis
            v = this.normalize(w);
            V.push(v);
        }

        // Extract eigenvalues from Hessenberg matrix
        const eigenvalues = this.computeEigenvalues(H);
        
        return {
            basis: V,
            hessenberg: H,
            eigenvalues: eigenvalues
        };
    }

    // Simple eigenvalue computation (for demonstration)
    computeEigenvalues(H) {
        // This is a simplified version - in practice, you'd use QR algorithm
        const n = H.length;
        if (n === 0) return [];
        
        // Extract diagonal elements for simplicity
        const eigenvals = [];
        for (let i = 0; i < Math.min(n, 5); i++) { // Just first 5
            if (i < H[i].length) {
                eigenvals.push(H[i][i]);
            }
        }
        return eigenvals;
    }
}

// Example usage
function example() {
    // Example: 3x3 matrix
    const matrix = [
        [4, 1, 2],
        [1, 3, 1],
        [2, 1, 5]
    ];

    const arnoldi = new ArnoldiIteration(matrix, 20, 1e-10);
    const result = arnoldi.run();

    console.log("Arnoldi Iteration Result:");
    console.log("Basis vectors:", result.basis);
    console.log("Hessenberg matrix:", result.hessenberg);
    console.log("Computed eigenvalues:", result.eigenvalues);

    return result;
}

// Run example
const exampleResult = example();
```

## Key Features of this Implementation:

1. **Matrix Operations**: Includes matrix-vector multiplication and vector operations
2. **Orthogonalization**: Uses Gram-Schmidt process to maintain orthonormal basis
3. **Convergence Check**: Stops when residual norm is below tolerance
4. **Hessenberg Matrix**: Builds the upper Hessenberg matrix during iteration
5. **Eigenvalue Extraction**: Simple extraction (in practice, QR algorithm would be used)

## Usage:
```javascript
// Create Arnoldi instance with your matrix
const arnoldi = new ArnoldiIteration(yourMatrix, maxIterations, tolerance);
const result = arnoldi.run(startVector);

// Access results
console.log(result.eigenvalues);  // Computed eigenvalues
console.log(result.hessenberg);   // Hessenberg matrix
console.log(result.basis);        // Orthonormal basis vectors
```

This implementation demonstrates the core concepts of Arnoldi iteration for finding eigenvalues of large sparse matrices.

