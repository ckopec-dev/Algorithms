# Karatsuba Multiplication Algorithm in Perl

Here's an implementation of the Karatsuba multiplication algorithm in Perl:

```perl
#!/usr/bin/perl
use strict;
use warnings;

sub karatsuba {
    my ($x, $y) = @_;
    
    # Convert to strings to work with digits
    my $x_str = "$x";
    my $y_str = "$y";
    
    # Handle base case: if either number is single digit
    if (length($x_str) == 1 && length($y_str) == 1) {
        return $x * $y;
    }
    
    # Make both strings the same length by padding with zeros
    my $max_len = length($x_str) > length($y_str) ? length($x_str) : length($y_str);
    $max_len = $max_len % 2 == 0 ? $max_len : $max_len + 1;
    
    # Pad with leading zeros
    $x_str = sprintf("%0${max_len}d", $x_str);
    $y_str = sprintf("%0${max_len}d", $y_str);
    
    # Split the numbers
    my $n = $max_len;
    my $n2 = int($n / 2);
    
    my $high1 = substr($x_str, 0, $n2);
    my $low1 = substr($x_str, $n2);
    my $high2 = substr($y_str, 0, $n2);
    my $low2 = substr($y_str, $n2);
    
    # Recursive calls
    my $z0 = karatsuba($low1, $low2);
    my $z1 = karatsuba(($low1 + $high1), ($low2 + $high2));
    my $z2 = karatsuba($high1, $high2);
    
    # Combine results
    return ($z2 * (10 ** (2 * $n2))) + (($z1 - $z2 - $z0) * (10 ** $n2)) + $z0;
}

# Alternative cleaner implementation
sub karatsuba_clean {
    my ($x, $y) = @_;
    
    # Base case
    if ($x < 10 || $y < 10) {
        return $x * $y;
    }
    
    # Determine the size of the numbers
    my $n = length($x) > length($y) ? length($x) : length($y);
    $n = $n % 2 == 0 ? $n : $n + 1;  # Make even
    
    # Pad with leading zeros
    $x = sprintf("%0${n}d", $x);
    $y = sprintf("%0${n}d", $y);
    
    # Split numbers
    my $n2 = int($n / 2);
    my $high1 = substr($x, 0, $n2);
    my $low1 = substr($x, $n2);
    my $high2 = substr($y, 0, $n2);
    my $low2 = substr($y, $n2);
    
    # Karatsuba multiplication
    my $z0 = karatsuba_clean($low1, $low2);
    my $z1 = karatsuba_clean(($low1 + $high1), ($low2 + $high2));
    my $z2 = karatsuba_clean($high1, $high2);
    
    return ($z2 * (10 ** (2 * $n2))) + (($z1 - $z2 - $z0) * (10 ** $n2)) + $z0;
}

# Test the implementation
print "Karatsuba Multiplication Examples:\n";
print "=" x 40 . "\n";

my @test_cases = (
    [1234, 5678],
    [123, 456],
    [12, 34],
    [5, 7],
    [123456789, 987654321]
);

foreach my $case (@test_cases) {
    my ($a, $b) = @$case;
    my $result = karatsuba_clean($a, $b);
    my $expected = $a * $b;
    
    print "  $a × $b = $result\n";
    print "  Expected: $expected\n";
    print "  Match: " . ($result == $expected ? "YES" : "NO") . "\n\n";
}

# Simple comparison function
sub compare_methods {
    my ($a, $b) = @_;
    
    my $start = time();
    my $karatsuba_result = karatsuba_clean($a, $b);
    my $karatsuba_time = time() - $start;
    
    $start = time();
    my $regular_result = $a * $b;
    my $regular_time = time() - $start;
    
    print "Comparison for $a × $b:\n";
    print "  Karatsuba: $karatsuba_result (Time: $karatsuba_time)\n";
    print "  Regular:   $regular_result (Time: $regular_time)\n\n";
}

# Compare methods
compare_methods(123456, 789012);
compare_methods(123456789, 987654321);
```

## How it works:

1. **Base Case**: If either number is a single digit, multiply directly
2. **Padding**: Make both numbers the same length by padding with leading zeros
3. **Splitting**: Divide each number into two halves
4. **Recursive Calls**: 
   - `z0 = low1 × low2`
   - `z1 = (low1 + high1) × (low2 + high2)`
   - `z2 = high1 × high2`
5. **Combine**: Result = `z2 × 10^(2n) + (z1 - z2 - z0) × 10^n + z0`

## Example Output:
```
Karatsuba Multiplication Examples:
========================================
  1234 × 5678 = 7006652
  Expected: 7006652
  Match: YES

  123 × 456 = 56088
  Expected: 56088
  Match: YES
```

This implementation demonstrates the divide-and-conquer approach of Karatsuba multiplication, which has a time complexity of O(n^1.585) compared to O(n²) for traditional multiplication.

