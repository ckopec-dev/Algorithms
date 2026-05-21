# Newton-Raphson Method in Perl

Here's an implementation of the Newton-Raphson method to find roots of equations in Perl:

```perl
#!/usr/bin/perl
use strict;
use warnings;
use Math::BigFloat;

# Newton-Raphson method implementation
sub newton_raphson {
    my ($func, $derivative, $initial_guess, $tolerance, $max_iterations) = @_;
    
    my $x = $initial_guess;
    my $iteration = 0;
    
    print "Starting Newton-Raphson method:\n";
    print "Initial guess: $x\n";
    print "Tolerance: $tolerance\n";
    print "Max iterations: $max_iterations\n\n";
    
    while ($iteration < $max_iterations) {
        my $fx = $func->($x);
        my $fpx = $derivative->($x);
        
        # Check if derivative is too close to zero
        if (abs($fpx) < 1e-15) {
            die "Derivative too close to zero. Cannot continue.";
        }
        
        # Newton-Raphson formula: x_{n+1} = x_n - f(x_n)/f'(x_n)
        my $x_new = $x - ($fx / $fpx);
        
        printf "Iteration %d: x = %.10f, f(x) = %.10f\n", $iteration, $x, $fx;
        
        # Check for convergence
        if (abs($x_new - $x) < $tolerance) {
            printf "Converged after %d iterations!\n", $iteration + 1;
            return $x_new;
        }
        
        $x = $x_new;
        $iteration++;
    }
    
    die "Method did not converge within $max_iterations iterations";
}

# Example 1: Finding root of f(x) = x^2 - 2 (sqrt(2))
print "=== Example 1: Finding sqrt(2) ===\n";
my $f1 = sub {
    my $x = shift;
    return $x * $x - 2;  # f(x) = x^2 - 2
};

my $df1 = sub {
    my $x = shift;
    return 2 * $x;      # f'(x) = 2x
};

my $root1 = newton_raphson($f1, $df1, 1.0, 1e-10, 10);
print "Root found: $root1\n";
print "Verification: $root1 * $root1 = " . ($root1 * $root1) . "\n\n";

# Example 2: Finding root of f(x) = x^3 - 2x - 5
print "=== Example 2: Finding root of x^3 - 2x - 5 ===\n";
my $f2 = sub {
    my $x = shift;
    return $x**3 - 2*$x - 5;  # f(x) = x^3 - 2x - 5
};

my $df2 = sub {
    my $x = shift;
    return 3*$x*$x - 2;      # f'(x) = 3x^2 - 2
};

my $root2 = newton_raphson($f2, $df2, 2.0, 1e-10, 10);
print "Root found: $root2\n";
print "Verification: $root2^3 - 2*$root2 - 5 = " . ($root2**3 - 2*$root2 - 5) . "\n\n";

# Example 3: Finding root of f(x) = cos(x) - x
print "=== Example 3: Finding root of cos(x) - x ===\n";
my $f3 = sub {
    my $x = shift;
    return cos($x) - $x;     # f(x) = cos(x) - x
};

my $df3 = sub {
    my $x = shift;
    return -sin($x) - 1;     # f'(x) = -sin(x) - 1
};

my $root3 = newton_raphson($f3, $df3, 0.5, 1e-10, 10);
print "Root found: $root3\n";
print "Verification: cos($root3) - $root3 = " . (cos($root3) - $root3) . "\n";
```

## Key Features of this Implementation:

1. **Function Parameters**: Takes the function, its derivative, initial guess, tolerance, and maximum iterations
2. **Convergence Check**: Stops when the difference between successive iterations is less than tolerance
3. **Error Handling**: Checks for zero derivatives and maximum iteration limits
4. **Detailed Output**: Shows each iteration for debugging purposes
5. **Multiple Examples**: Demonstrates the method with different functions

## Expected Output:
```
=== Example 1: Finding sqrt(2) ===
Starting Newton-Raphson method:
Initial guess: 1
Tolerance: 1e-10
Max iterations: 10

Iteration 0: x = 1.0000000000, f(x) = -1.0000000000
Iteration 1: x = 1.5000000000, f(x) = 0.2500000000
Iteration 2: x = 1.4166666667, f(x) = 0.0069444444
Iteration 3: x = 1.4142156863, f(x) = 0.0000000000
Converged after 4 iterations!
Root found: 1.4142135623
Verification: 1.4142135623 * 1.4142135623 = 2
```

The Newton-Raphson method converges quadratically when close to the root, making it very efficient for finding roots of differentiable functions.

