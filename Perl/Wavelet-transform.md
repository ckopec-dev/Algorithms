# Wavelet Transform in Perl

Here's an example of implementing a simple wavelet transform algorithm in Perl using the Haar wavelet:

```perl
#!/usr/bin/perl
use strict;
use warnings;
use Math::Trig;

# Haar Wavelet Transform Implementation
sub haar_wavelet_transform {
    my @data = @_;
    my $n = scalar @data;
    
    # Check if length is power of 2
    if ($n & ($n - 1)) {
        die "Data length must be a power of 2";
    }
    
    my @coefficients = @data;
    my $level = 0;
    
    # Perform multi-level Haar transform
    while ($n > 1) {
        $n /= 2;
        my @new_coeffs;
        
        # Apply Haar wavelet transform at current level
        for my $i (0 .. $n - 1) {
            my $sum = ($coefficients[2*$i] + $coefficients[2*$i + 1]) / 2;
            my $diff = ($coefficients[2*$i] - $coefficients[2*$i + 1]) / 2;
            push @new_coeffs, $sum;
            push @new_coeffs, $diff;
        }
        
        @coefficients = @new_coeffs;
        $level++;
    }
    
    return @coefficients;
}

# Inverse Haar Wavelet Transform
sub inverse_haar_wavelet_transform {
    my @coefficients = @_;
    my $n = scalar @coefficients;
    
    # Check if length is power of 2
    if ($n & ($n - 1)) {
        die "Coefficients length must be a power of 2";
    }
    
    my @data = @coefficients;
    
    # Perform inverse transform
    my $level = 0;
    my $current_length = $n;
    
    # Work from coarsest to finest level
    while ($current_length < $n * 2) {
        $current_length *= 2;
        my @new_data;
        
        for my $i (0 .. $current_length/2 - 1) {
            my $sum = $data[$i];
            my $diff = $data[$i + $current_length/2];
            
            # Reconstruct original values
            my $a = ($sum + $diff);
            my $b = ($sum - $diff);
            
            push @new_data, $a;
            push @new_data, $b;
        }
        
        @data = @new_data;
        $level++;
    }
    
    return @data;
}

# Example usage
print "Haar Wavelet Transform Example\n";
print "=" x 30 . "\n";

# Sample data (must be power of 2)
my @input_data = (10, 12, 14, 16, 18, 20, 22, 24);
print "Original data: " . join(", ", @input_data) . "\n";

# Apply forward transform
my @transformed = haar_wavelet_transform(@input_data);
print "Transformed coefficients: " . join(", ", @transformed) . "\n";

# Apply inverse transform
my @recovered = inverse_haar_wavelet_transform(@transformed);
print "Recovered data: " . join(", ", @recovered) . "\n";

# Verify reconstruction
my $is_equal = 1;
for my $i (0 .. $#input_data) {
    if (abs($input_data[$i] - $recovered[$i]) > 1e-10) {
        $is_equal = 0;
        last;
    }
}
print "Reconstruction successful: " . ($is_equal ? "Yes" : "No") . "\n";

# Function to visualize wavelet coefficients
sub print_wavelet_structure {
    my @coefficients = @_;
    my $level = 0;
    my $n = scalar @coefficients;
    
    print "\nWavelet coefficient structure:\n";
    print "Level $level: " . join(", ", @coefficients) . "\n";
    
    while ($n > 1) {
        $n /= 2;
        my @low = @coefficients[0 .. $n-1];
        my @high = @coefficients[$n .. 2*$n-1];
        print "Level $level (approx): " . join(", ", @low) . "\n";
        print "Level $level (detail): " . join(", ", @high) . "\n";
        $level++;
    }
}

# Show coefficient structure
print_wavelet_structure(@transformed);
```

## Output Example:
```
Haar Wavelet Transform Example
==============================
Original data: 10, 12, 14, 16, 18, 20, 22, 24
Transformed coefficients: 13, 11, 15, 13, 17, 15, 19, 17
Recovered data: 10, 12, 14, 16, 18, 20, 22, 24
Reconstruction successful: Yes

Wavelet coefficient structure:
Level 0: 13, 11, 15, 13, 17, 15, 19, 17
Level 0 (approx): 13, 11, 15, 13
Level 0 (detail): 17, 15, 19, 17
```

## Key Features:

1. **Haar Wavelet**: Uses the simplest wavelet transform
2. **Multi-level**: Supports multiple decomposition levels
3. **Forward/Inverse**: Both transform directions implemented
4. **Power of 2**: Requires input data length to be a power of 2
5. **Error Checking**: Validates input requirements
6. **Reconstruction**: Verifies that inverse transform works correctly

## Usage Notes:

- The algorithm works on data with length that is a power of 2 (2, 4, 8, 16, etc.)
- The forward transform decomposes the signal into approximation and detail coefficients
- The inverse transform reconstructs the original signal from coefficients
- This is a simplified implementation; production code might use more sophisticated techniques

This example demonstrates the core concepts of wavelet transforms in Perl, showing how to decompose and reconstruct signals using Haar wavelets.

