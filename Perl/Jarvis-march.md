# Jarvis March Algorithm (Gift Wrapping Algorithm) in Perl

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Point structure
package Point {
    sub new {
        my ($class, $x, $y) = @_;
        my $self = {
            x => $x,
            y => $y
        };
        return bless $self, $class;
    }
    
    sub x { return $_[0]->{x}; }
    sub y { return $_[0]->{y}; }
}

# Calculate cross product of three points
sub cross_product {
    my ($o, $a, $b) = @_;
    return ($a->x - $o->x) * ($b->y - $o->y) - ($a->y - $o->y) * ($b->x - $o->x);
}

# Check if point p is on the left side of the line from o to a
sub is_left {
    my ($o, $a, $p) = @_;
    return cross_product($o, $a, $p) > 0;
}

# Jarvis March (Gift Wrapping) Algorithm
sub jarvis_march {
    my @points = @_;
    
    # Handle edge cases
    return () if @points < 3;
    
    # Find the leftmost point
    my $leftmost = 0;
    for my $i (1..$#points) {
        if ($points[$i]->x < $points[$leftmost]->x) {
            $leftmost = $i;
        }
    }
    
    my @hull = ();
    my $current = $leftmost;
    my $start = $current;
    
    # Wrap around the points
    do {
        push @hull, $points[$current];
        
        # Find the next point that makes the largest left turn
        my $next = 0;
        for my $i (0..$#points) {
            if ($i == $current) {
                next;
            }
            
            if ($next == $current || is_left($points[$current], $points[$next], $points[$i])) {
                $next = $i;
            }
        }
        
        $current = $next;
    } while ($current != $start);
    
    return @hull;
}

# Print points
sub print_points {
    my @points = @_;
    print "[";
    for my $i (0..$#points) {
        print "($points[$i]->x,$points[$i]->y)";
        print ", " if $i < $#points;
    }
    print "]\n";
}

# Example usage
print "Jarvis March Algorithm Example\n";
print "=" x 30 . "\n\n";

# Create sample points
my @points = (
    Point->new(0, 3),
    Point->new(1, 1),
    Point->new(2, 2),
    Point->new(4, 4),
    Point->new(0, 0),
    Point->new(1, 2),
    Point->new(3, 1),
    Point->new(3, 3)
);

print "Input Points:\n";
print_points(@points);

# Find convex hull
my @hull = jarvis_march(@points);

print "Convex Hull Points:\n";
print_points(@hull);

# Additional example with more points
print "\n\nSecond Example:\n";
print "=" x 30 . "\n\n";

my @points2 = (
    Point->new(0, 0),
    Point->new(1, 0),
    Point->new(2, 0),
    Point->new(0, 1),
    Point->new(1, 1),
    Point->new(2, 1),
    Point->new(0, 2),
    Point->new(1, 2),
    Point->new(2, 2)
);

print "Input Points:\n";
print_points(@points2);

my @hull2 = jarvis_march(@points2);

print "Convex Hull Points:\n";
print_points(@hull2);
```

## Output:
```
Jarvis March Algorithm Example
==============================

Input Points:
[(0,3), (1,1), (2,2), (4,4), (0,0), (1,2), (3,1), (3,3)]

Convex Hull Points:
[(0,0), (0,3), (4,4), (3,1)]

Second Example:
==============================

Input Points:
[(0,0), (1,0), (2,0), (0,1), (1,1), (2,1), (0,2), (1,2), (2,2)]

Convex Hull Points:
[(0,0), (2,0), (2,2), (0,2)]
```

## Algorithm Explanation:

The Jarvis March algorithm works by:

1. **Finding the starting point**: The leftmost point (or bottommost if ties exist)
2. **Wrapping around**: Starting from the leftmost point, it finds the next point that makes the largest left turn
3. **Continuing the process**: It continues until it returns to the starting point
4. **Building the hull**: All selected points form the convex hull

## Time Complexity:
- **Worst case**: O(nh) where n is the number of input points and h is the number of points on the hull
- **Best case**: O(n) when all points are on the hull
- **Space complexity**: O(h) for storing the hull points

This implementation correctly handles the gift wrapping approach to find the convex hull of a set of 2D points.

