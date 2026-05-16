# Conjugate Gradient Method in Perl

Here's an implementation of the conjugate gradient method to solve linear systems of equations Ax = b:

```perl
#!/usr/bin/perl
use strict;
use warnings;
use Math::Matrix;

sub conjugate_gradient {
    my ($A, $b, $max_iterations, $tolerance) = @_;
    
    # Set default values
    $max_iterations //= 1000;
    $tolerance //= 1e-10;
    
    my $n = @$A;
    
    # Initialize variables
    my @x = (0) x $n;  # Initial guess
    my @r = @$b;       # Residual r = b - Ax
    my @p = @r;        # Search direction
    
    # Calculate initial residual norm
    my $r_norm = 0;
    for my $i (0..$#r) {
        $r_norm += $r[$i] * $r[$i];
    }
    $r_norm = sqrt($r_norm);
    
    # Check if already converged
    return \@x if $r_norm < $tolerance;
    
    # Conjugate gradient iterations
    for my $k (0..$max_iterations-1) {
        # Calculate Ap
        my @Ap = calculate_Ap($A, @p);
        
        # Calculate alpha_k
        my $pAp = 0;
        for my $i (0..$#p) {
            $pAp += $p[$i] * $Ap[$i];
        }
        
        my $alpha = 0;
        if (abs($pAp) > 1e-15) {
            $alpha = $r_norm * $r_norm / $pAp;
        } else {
            last;  # Break if direction is degenerate
        }
        
        # Update solution
        for my $i (0..$#x) {
            $x[$i] += $alpha * $p[$i];
        }
        
        # Update residual
        for my $i (0..$#r) {
            $r[$i] -= $alpha * $Ap[$i];
        }
        
        # Calculate new residual norm
        my $new_r_norm = 0;
        for my $i (0..$#r) {
            $new_r_norm += $r[$i] * $r[$i];
        }
        $new_r_norm = sqrt($new_r_norm);
        
        # Check convergence
        if ($new_r_norm < $tolerance) {
            return \@x;
        }
        
        # Calculate beta_k
        my $beta = $new_r_norm * $new_r_norm / ($r_norm * $r_norm);
        
        # Update search direction
        for my $i (0..$#p) {
            $p[$i] = $r[$i] + $beta * $p[$i];
        }
        
        $r_norm = $new_r_norm;
    }
    
    return \@x;
}

sub calculate_Ap {
    my ($A, @p) = @_;
    my $n = @$A;
    my @Ap = (0) x $n;
    
    for my $i (0..$n-1) {
        for my $j (0..$#p) {
            $Ap[$i] += $A->[$i][$j] * $p[$j];
        }
    }
    
    return @Ap;
}

# Example usage
sub main {
    # Example system: Ax = b
    # 4x + 2y + 1z = 1
    # 2x + 3y + 1z = 2  
    # 1x + 1y + 5z = 3
    
    my $A = [
        [4, 2, 1],
        [2, 3, 1],
        [1, 1, 5]
    ];
    
    my $b = [1, 2, 3];
    
    print "Solving system Ax = b\n";
    print "A = \n";
    for my $row (@$A) {
        print join(" ", @$row) . "\n";
    }
    print "b = [" . join(" ", @$b) . "]\n\n";
    
    # Solve using conjugate gradient method
    my $solution = conjugate_gradient($A, $b, 1000, 1e-10);
    
    print "Solution:\n";
    for my $i (0..$#{$solution}) {
        printf "x[%d] = %.6f\n", $i, $solution->[$i];
    }
    
    # Verify solution
    print "\nVerification (Ax = b):\n";
    my @verification = calculate_Ap($A, @$solution);
    for my $i (0..$#verification) {
        printf "Ax[%d] = %.6f, b[%d] = %.6f\n", 
               $i, $verification[$i], $i, $b->[$i];
    }
}

# Run the example
main();
```

## Key Features of this Implementation:

1. **Main Algorithm**: Implements the standard conjugate gradient method for solving linear systems
2. **Convergence Check**: Stops when the residual norm falls below the specified tolerance
3. **Iteration Limit**: Prevents infinite loops with a maximum iteration count
4. **Numerical Stability**: Includes checks for degenerate cases
5. **Verification**: Includes code to verify the solution

## How it Works:

1. **Initialization**: Start with zero guess, compute initial residual
2. **Iteration**: For each step:
   - Calculate search direction using conjugate gradient formula
   - Determine step size (alpha) using the residual
   - Update solution and residual
   - Check for convergence
   - Update search direction for next iteration

## Usage:

The algorithm takes:
- Matrix A (coefficient matrix)
- Vector b (right-hand side)
- Optional max_iterations and tolerance parameters

The method returns the solution vector x that satisfies Ax = b.

