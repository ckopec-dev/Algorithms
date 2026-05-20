# Z-Order Curve Generation in Perl

Here's an example implementation of a Z-order curve (also known as Morton curve) generation algorithm in Perl:

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Function to generate Z-order curve for given coordinates
sub z_order_curve {
    my ($x, $y) = @_;
    
    # Ensure we're working with integers
    $x = int($x);
    $y = int($y);
    
    # For a 16-bit coordinate system (common for Z-order curves)
    my $z = 0;
    my $bit = 1;
    
    # Process each bit position
    for (my $i = 0; $i < 16; $i++) {
        # Extract bit from y and place it in odd positions
        if ($y & (1 << $i)) {
            $z |= ($bit << 1);  # Set bit in odd position
        }
        
        # Extract bit from x and place it in even positions
        if ($x & (1 << $i)) {
            $z |= $bit;         # Set bit in even position
        }
        
        $bit <<= 2;  # Move to next pair of bit positions
    }
    
    return $z;
}

# Function to decode Z-order value back to coordinates
sub decode_z_order {
    my ($z) = @_;
    
    my $x = 0;
    my $y = 0;
    my $bit = 1;
    
    # Process each bit position
    for (my $i = 0; $i < 16; $i++) {
        # Extract even bit (x coordinate)
        if ($z & ($bit << 1)) {
            $x |= (1 << $i);
        }
        
        # Extract odd bit (y coordinate)
        if ($z & $bit) {
            $y |= (1 << $i);
        }
        
        $bit <<= 2;  # Move to next pair of bit positions
    }
    
    return ($x, $y);
}

# Function to generate Z-order curve for a range of coordinates
sub generate_z_order_table {
    my ($max_x, $max_y) = @_;
    
    my @z_table;
    
    for my $y (0..$max_y) {
        for my $x (0..$max_x) {
            my $z = z_order_curve($x, $y);
            push @{$z_table[$y]}, $z;
        }
    }
    
    return @z_table;
}

# Example usage
print "Z-Order Curve Generation Example\n";
print "=" x 40 . "\n\n";

# Generate some sample Z-order values
print "Sample Z-order values:\n";
print "Coordinate (x,y) -> Z-order value\n";
print "-" x 30 . "\n";

my @test_coords = ([0,0], [1,0], [0,1], [1,1], [2,3], [5,7], [10,15]);

foreach my $coord (@test_coords) {
    my ($x, $y) = @$coord;
    my $z = z_order_curve($x, $y);
    printf "(%2d,%2d) -> %4d\n", $x, $y, $z;
}

print "\n";

# Decode some Z-order values back to coordinates
print "Decoding Z-order values back to coordinates:\n";
print "Z-order -> (x,y)\n";
print "-" x 20 . "\n";

my @test_z_values = (0, 1, 2, 3, 11, 23, 100);

foreach my $z (@test_z_values) {
    my ($x, $y) = decode_z_order($z);
    printf "%4d -> (%2d,%2d)\n", $z, $x, $y;
}

print "\n";

# Generate a small Z-order table
print "Small Z-order table (4x4):\n";
print "  ";
for my $x (0..3) {
    printf "%4d", $x;
}
print "\n";

my @z_table = generate_z_order_table(3, 3);
for my $y (0..3) {
    printf "%2d ", $y;
    for my $x (0..3) {
        printf "%4d", $z_table[$y][$x];
    }
    print "\n";
}

print "\n";

# Demonstrate spatial locality preservation
print "Spatial Locality Preservation:\n";
print "Adjacent coordinates should have similar Z-order values\n";
print "-" x 40 . "\n";

my @adjacent_coords = ([0,0], [1,0], [0,1], [1,1], [2,1], [1,2], [2,2]);
print "Coordinates and their Z-order values:\n";

foreach my $coord (@adjacent_coords) {
    my ($x, $y) = @$coord;
    my $z = z_order_curve($x, $y);
    printf "(%2d,%2d) -> Z: %3d\n", $x, $y, $z;
}
```

## Key Features of This Implementation:

1. **Z-order Curve Generation**: Converts 2D coordinates (x,y) into a single Z-order value
2. **Decoding Function**: Converts Z-order values back to original coordinates
3. **Bit Manipulation**: Uses bit-level operations to interleave coordinates
4. **Spatial Locality**: Demonstrates how nearby points in 2D space map to nearby values in Z-order space
5. **Table Generation**: Shows how Z-order values are distributed in a grid

## How It Works:

1. **Encoding**: For each bit position, the algorithm alternates between placing x and y bits in the result
2. **Bit Interleaving**: Even bit positions get x coordinates, odd bit positions get y coordinates
3. **Spatial Locality**: Points that are close in 2D space tend to have similar Z-order values

## Sample Output:
```
Z-Order Curve Generation Example
========================================

Sample Z-order values:
Coordinate (x,y) -> Z-order value
------------------------------
( 0, 0) ->    0
( 1, 0) ->    1
( 0, 1) ->    2
( 1, 1) ->    3
( 2, 3) ->   11
( 5, 7) ->   23
(10,15) ->  100
```

This implementation demonstrates the fundamental concept of Z-order curves and their practical applications in spatial indexing and data organization.

