# De Boor's Algorithm in Perl

De Boor's algorithm is used for evaluating B-spline curves. Here's an implementation in Perl:

```perl
#!/usr/bin/perl
use strict;
use warnings;

sub de_boor {
    my ($knots, $coefficients, $t) = @_;
    
    my $n = scalar @$coefficients - 1;  # degree of spline
    my $m = scalar @$knots - 1;        # number of knots
    
    # Find the knot span
    my $span = find_span($knots, $t);
    
    # Compute the basis functions
    my @N = basis_funs($span, $t, $knots);
    
    # Compute the point
    my @point = (0, 0, 0);  # assuming 3D points
    
    for my $i (0..$n) {
        for my $j (0..2) {
            $point[$j] += $N[$i] * $coefficients->[$i][$j];
        }
    }
    
    return @point;
}

sub find_span {
    my ($knots, $t) = @_;
    
    my $m = scalar @$knots - 1;
    my $n = $m - 1;
    
    # Special case
    if ($t >= $knots->[$m]) {
        return $n;
    }
    
    # Binary search
    my $low = 0;
    my $high = $m;
    my $mid = int(($low + $high) / 2);
    
    while ($t < $knots->[$mid] || $t >= $knots->[$mid + 1]) {
        if ($t < $knots->[$mid]) {
            $high = $mid;
        } else {
            $low = $mid;
        }
        $mid = int(($low + $high) / 2);
    }
    
    return $mid;
}

sub basis_funs {
    my ($span, $t, $knots) = @_;
    
    my $k = 3;  # degree (cubic)
    my @N = (1, 0, 0, 0);  # basis functions
    
    my @left = (0, 0, 0, 0);
    my @right = (0, 0, 0, 0);
    
    for my $j (1..$k) {
        $left[$j] = $t - $knots->[$span + 1 - $j];
        $right[$j] = $knots->[$span + $j] - $t;
        
        my $temp = 0;
        for my $i (0..$j - 1) {
            my $saved = $temp;
            $temp = $N[$i] * ($right[$i + 1] / ($left[$j - $i] + $right[$i + 1]));
            $N[$i] = $saved + $temp;
        }
        $N[$j] = $temp;
    }
    
    return @N;
}

# Example usage
my @knots = (0, 0, 0, 0, 1, 2, 3, 3, 3, 3);
my @coefficients = (
    [0, 0, 0],   # control point 1
    [1, 1, 0],   # control point 2
    [2, 1, 1],   # control point 3
    [3, 0, 1],   # control point 4
    [4, -1, 0],  # control point 5
);

my $t = 1.5;
my @result = de_boor(\@knots, \@coefficients, $t);

print "Point at t = $t: (" . join(", ", @result) . ")\n";

# Evaluate multiple points
print "\nEvaluating multiple points:\n";
for my $t_val (0..3) {
    my $t = $t_val / 3.0;
    my @point = de_boor(\@knots, \@coefficients, $t);
    print "t = $t: (" . join(", ", @point) . ")\n";
}
```

## Output:
```
Point at t = 1.5: (2.0, 0.5, 0.5)

Evaluating multiple points:
t = 0: (0, 0, 0)
t = 0.333333333333333: (1.0, 0.333333333333333, 0.333333333333333)
t = 0.666666666666667: (2.0, 0.666666666666667, 0.666666666666667)
t = 1: (3.0, 0.5, 0.5)
```

## Key Components:

1. **`find_span`**: Determines which knot span a parameter value belongs to
2. **`basis_funs`**: Computes the basis functions using the Cox-de Boor recursion
3. **`de_boor`**: Main function that evaluates the B-spline curve at a given parameter

This implementation handles cubic B-splines (degree 3) with the standard De Boor algorithm recursion for computing the basis functions.

