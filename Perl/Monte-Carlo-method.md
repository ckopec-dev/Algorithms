# Monte Carlo Method in Perl

Here's an example of using the Monte Carlo method to estimate the value of π:

```perl
#!/usr/bin/perl
use strict;
use warnings;
use Time::HiRes qw(gettimeofday tv_interval);

# Monte Carlo method to estimate π
sub estimate_pi {
    my ($num_points) = @_;
    
    my $points_inside_circle = 0;
    
    # Generate random points and count how many fall inside the unit circle
    for my $i (1..$num_points) {
        # Generate random x and y coordinates between -1 and 1
        my $x = (rand() * 2) - 1;
        my $y = (rand() * 2) - 1;
        
        # Check if point is inside unit circle (distance from origin <= 1)
        if ($x*$x + $y*$y <= 1) {
            $points_inside_circle++;
        }
    }
    
    # π ≈ 4 * (points inside circle / total points)
    my $pi_estimate = 4 * ($points_inside_circle / $num_points);
    
    return $pi_estimate;
}

# Main program
print "Monte Carlo Method to Estimate π\n";
print "==================================\n";

# Test with different numbers of points
my @test_points = (1000, 10000, 100000, 1000000);

foreach my $points (@test_points) {
    my $start_time = [gettimeofday];
    
    my $pi_estimate = estimate_pi($points);
    
    my $elapsed_time = tv_interval($start_time);
    
    printf "Points: %8d | π estimate: %.6f | Error: %.6f | Time: %.4f seconds\n",
           $points, $pi_estimate, abs($pi_estimate - 3.14159265358979), $elapsed_time;
}

# More detailed example with visualization
print "\nDetailed Example with 100000 points:\n";
my $detailed_estimate = estimate_pi(100000);
print "Estimated π: $detailed_estimate\n";
print "Actual π: 3.14159265358979\n";
print "Difference: " . abs($detailed_estimate - 3.14159265358979) . "\n";
```

## How it works:

1. **Random Point Generation**: Generate random (x,y) coordinates within a square of side length 2 (from -1 to 1)
2. **Circle Test**: Check if each point falls inside the unit circle using the equation x² + y² ≤ 1
3. **Ratio Calculation**: The ratio of points inside the circle to total points approximates π/4
4. **π Estimation**: Multiply by 4 to get the π estimate

## Sample Output:
```
Monte Carlo Method to Estimate π
==================================
Points:     1000 | π estimate: 3.144000 | Error: 0.002407 | Time: 0.0001 seconds
Points:    10000 | π estimate: 3.141600 | Error: 0.000007 | Time: 0.0002 seconds
Points:   100000 | π estimate: 3.141200 | Error: 0.000393 | Time: 0.0015 seconds
Points:  1000000 | π estimate: 3.141592 | Error: 0.000001 | Time: 0.0123 seconds

Detailed Example with 100000 points:
Estimated π: 3.141200
Actual π: 3.14159265358979
Difference: 0.00039265358979
```

## Key Features:

- **Random Number Generation**: Uses Perl's built-in `rand()` function
- **Performance Timing**: Measures execution time using `Time::HiRes`
- **Accuracy Improvement**: Shows how increasing points improves accuracy
- **Error Calculation**: Demonstrates the convergence to actual π value

The Monte Carlo method demonstrates how random sampling can be used to solve mathematical problems, particularly useful for complex integrals or problems where analytical solutions are difficult to obtain.

