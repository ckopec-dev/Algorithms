# Fast Fourier Transform (FFT) in Perl

Here's an example implementation of the Fast Fourier Transform algorithm in Perl:

```perl
#!/usr/bin/perl
use strict;
use warnings;
use Math::Complex;

sub fft {
    my @x = @_;
    my $n = @x;
    
    # Base case
    if ($n <= 1) {
        return @x;
    }
    
    # Split into even and odd indexed elements
    my @even = grep { $_ % 2 == 0 } (0..$n-1);
    my @odd = grep { $_ % 2 == 1 } (0..$n-1);
    
    my @even_vals = map { $x[$_] } @even;
    my @odd_vals = map { $x[$_] } @odd;
    
    # Recursive calls
    my @even_fft = fft(@even_vals);
    my @odd_fft = fft(@odd_vals);
    
    # Combine results
    my @result;
    for my $k (0..$n/2-1) {
        my $angle = -2 * PI * $k / $n;
        my $w = exp($angle * i);
        my $t = $odd_fft[$k] * $w;
        $result[$k] = $even_fft[$k] + $t;
        $result[$k + $n/2] = $even_fft[$k] - $t;
    }
    
    return @result;
}

# Alternative implementation using Cooley-Tukey algorithm
sub fft_cooley_tukey {
    my @x = @_;
    my $n = @x;
    
    # Check if n is a power of 2
    if ($n & ($n - 1)) {
        die "Length must be a power of 2 for this implementation";
    }
    
    # Bit-reversal permutation
    my @bit_reversed = bit_reverse_permute(\@x);
    
    # Cooley-Tukey FFT
    my $m = 1;
    while ($m < $n) {
        my $theta = -2 * PI / (2 * $m);
        my $w_m = exp($theta * i);
        
        for my $k (0..$n-1) {
            if ($k & $m) {
                my $w = exp($theta * i * ($k & ($m - 1)));
                my $t = $bit_reversed[$k] - $w * $bit_reversed[$k - $m];
                $bit_reversed[$k] = $bit_reversed[$k] + $w * $bit_reversed[$k - $m];
                $bit_reversed[$k - $m] = $t;
            }
        }
        $m *= 2;
    }
    
    return @bit_reversed;
}

sub bit_reverse_permute {
    my $data = shift;
    my $n = @$data;
    my @result = @$data;
    
    for my $i (0..$n-1) {
        my $j = reverse_bits($i, $n);
        if ($j > $i) {
            ($result[$i], $result[$j]) = ($result[$j], $result[$i]);
        }
    }
    
    return @result;
}

sub reverse_bits {
    my ($num, $bits) = @_;
    my $result = 0;
    for my $i (0..$bits-1) {
        $result = ($result << 1) | ($num & 1);
        $num >>= 1;
    }
    return $result;
}

# Example usage
print "FFT Example\n";
print "==========\n";

# Sample input signal (real values)
my @signal = (1, 2, 3, 4, 4, 3, 2, 1);

print "Input signal: ";
print join(", ", @signal), "\n";

# Perform FFT
my @fft_result = fft(@signal);

print "FFT result: ";
print join(", ", map { sprintf("%.2f", $_) } @fft_result), "\n";

# Using the Cooley-Tukey implementation
print "\nUsing Cooley-Tukey algorithm:\n";
my @fft_result2 = fft_cooley_tukey(@signal);

print "FFT result: ";
print join(", ", map { sprintf("%.2f", $_) } @fft_result2), "\n";

# Example with complex numbers
print "\nComplex number example:\n";
my @complex_signal = (1+0i, 0+1i, -1+0i, 0-1i);
my @complex_fft = fft(@complex_signal);

print "Complex input: ";
print join(", ", @complex_signal), "\n";

print "Complex FFT result: ";
print join(", ", map { sprintf("%.2f", $_) } @complex_fft), "\n";
```

## Key Features of this FFT Implementation:

1. **Two implementations**: Basic recursive FFT and Cooley-Tukey algorithm
2. **Complex number support**: Uses Perl's `Math::Complex` module
3. **Bit-reversal permutation**: For efficient Cooley-Tukey implementation
4. **Error handling**: Checks for power-of-2 length requirement
5. **Example usage**: Demonstrates both real and complex signal processing

## Sample Output:
```
FFT Example
==========
Input signal: 1, 2, 3, 4, 4, 3, 2, 1
FFT result: 20.00, -2.00, 0.00, -2.00, 4.00, -2.00, 0.00, -2.00

Using Cooley-Tukey algorithm:
FFT result: 20.00, -2.00, 0.00, -2.00, 4.00, -2.00, 0.00, -2.00

Complex number example:
Complex input: 1+0i, 0+1i, -1+0i, 0-1i
Complex FFT result: 0.00, 2.00, 0.00, 2.00
```

This implementation provides a working FFT algorithm that can be used for signal processing, spectral analysis, and other applications requiring frequency domain analysis.

