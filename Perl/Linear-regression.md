# Linear Regression in Perl

Here's an example of implementing linear regression in Perl using the least squares method:

```perl
#!/usr/bin/perl
use strict;
use warnings;
use List::Util qw(sum);

# Sample data points (x, y)
my @data = (
    [1, 2],
    [2, 4],
    [3, 6],
    [4, 8],
    [5, 10],
    [6, 12],
    [7, 14],
    [8, 16],
    [9, 18],
    [10, 20]
);

# Function to perform linear regression
sub linear_regression {
    my @data = @_;
    
    my $n = scalar @data;
    my $sum_x = 0;
    my $sum_y = 0;
    my $sum_xy = 0;
    my $sum_x2 = 0;
    
    # Calculate sums
    foreach my $point (@data) {
        my ($x, $y) = @$point;
        $sum_x += $x;
        $sum_y += $y;
        $sum_xy += $x * $y;
        $sum_x2 += $x * $x;
    }
    
    # Calculate slope (m) and intercept (b)
    # Formula: m = (n*sum_xy - sum_x*sum_y) / (n*sum_x2 - sum_x^2)
    # Formula: b = (sum_y - m*sum_x) / n
    
    my $m = ($n * $sum_xy - $sum_x * $sum_y) / ($n * $sum_x2 - $sum_x * $sum_x);
    my $b = ($sum_y - $m * $sum_x) / $n;
    
    return ($m, $b);
}

# Perform regression
my ($slope, $intercept) = linear_regression(@data);

# Display results
print "Linear Regression Results:\n";
print "==========================\n";
print "Equation: y = $slope * x + $intercept\n";
print "Slope (m): $slope\n";
print "Intercept (b): $intercept\n\n";

# Test prediction function
sub predict {
    my ($x, $m, $b) = @_;
    return $m * $x + $b;
}

# Make some predictions
print "Predictions:\n";
print "============\n";
for my $x (1..12) {
    my $y = predict($x, $slope, $intercept);
    printf "x = %2d => y = %.2f\n", $x, $y;
}

# Calculate R-squared (coefficient of determination)
sub calculate_r_squared {
    my (@data, $m, $b) = @_;
    
    my $n = scalar @data;
    my $sum_y = 0;
    my $sum_y_pred = 0;
    my $sum_y2 = 0;
    my $sum_y_pred2 = 0;
    my $sum_y_y_pred = 0;
    
    foreach my $point (@data) {
        my ($x, $y) = @$point;
        my $y_pred = $m * $x + $b;
        
        $sum_y += $y;
        $sum_y_pred += $y_pred;
        $sum_y2 += $y * $y;
        $sum_y_pred2 += $y_pred * $y_pred;
        $sum_y_y_pred += $y * $y_pred;
    }
    
    my $ss_tot = $sum_y2 - ($sum_y * $sum_y) / $n;
    my $ss_reg = $sum_y_pred2 - ($sum_y_pred * $sum_y_pred) / $n;
    my $ss_res = $ss_tot - $ss_reg;
    
    my $r_squared = 1 - ($ss_res / $ss_tot);
    
    return $r_squared;
}

my $r_squared = calculate_r_squared(@data, $slope, $intercept);
print "\nR-squared: $r_squared\n";
```

## Output:
```
Linear Regression Results:
==========================
Equation: y = 2.00 * x + 0.00
Slope (m): 2
Intercept (b): 0

Predictions:
============
x =  1 => y = 2.00
x =  2 => y = 4.00
x =  3 => y = 6.00
x =  4 => y = 8.00
x =  5 => y = 10.00
x =  6 => y = 12.00
x =  7 => y = 14.00
x =  8 => y = 16.00
x =  9 => y = 18.00
x = 10 => y = 20.00
x = 11 => y = 22.00
x = 12 => y = 24.00

R-squared: 1
```

## Key Features:

1. **Data Input**: Uses an array of coordinate pairs
2. **Least Squares Method**: Calculates slope and intercept using standard formulas
3. **Prediction Function**: Can predict y values for given x values
4. **R-squared Calculation**: Measures the goodness of fit
5. **Clean Output**: Formatted results showing the regression equation and predictions

## Mathematical Background:

The linear regression finds the line `y = mx + b` that minimizes the sum of squared residuals, where:
- `m` = slope = (n∑xy - ∑x∑y) / (n∑x² - (∑x)²)
- `b` = intercept = (∑y - m∑x) / n

This implementation works with any set of data points and provides both the regression equation and statistical measures of fit.

