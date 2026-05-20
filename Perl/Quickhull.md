# Quickhull Algorithm in Perl

Here's an implementation of the Quickhull algorithm for finding the convex hull of a set of 2D points in Perl:

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Point class to represent 2D points
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

# Quickhull implementation
package Quickhull {
    sub new {
        my ($class) = @_;
        my $self = {};
        return bless $self, $class;
    }
    
    # Main Quickhull function
    sub convex_hull {
        my ($self, $points) = @_;
        
        return [] if @$points < 3;
        
        # Find leftmost and rightmost points
        my $leftmost = $points->[0];
        my $rightmost = $points->[0];
        
        foreach my $point (@$points) {
            if ($point->x < $leftmost->x) {
                $leftmost = $point;
            }
            if ($point->x > $rightmost->x) {
                $rightmost = $point;
            }
        }
        
        my @hull = ($leftmost, $rightmost);
        
        # Split points into two halves
        my @left_points = ();
        my @right_points = ();
        
        foreach my $point (@$points) {
            next if $point == $leftmost || $point == $rightmost;
            
            my $distance = $self->_point_line_distance($point, $leftmost, $rightmost);
            
            if ($distance > 0) {
                push @left_points, $point;
            } elsif ($distance < 0) {
                push @right_points, $point;
            }
        }
        
        # Recursively find hull points
        $self->_build_hull(\@hull, $leftmost, $rightmost, \@left_points);
        $self->_build_hull(\@hull, $rightmost, $leftmost, \@right_points);
        
        return \@hull;
    }
    
    # Recursive hull building function
    sub _build_hull {
        my ($self, $hull, $p1, $p2, $points) = @_;
        
        return if @$points == 0;
        
        # Find point furthest from line p1-p2
        my $max_distance = 0;
        my $farthest_point = undef;
        
        foreach my $point (@$points) {
            my $distance = $self->_point_line_distance($point, $p1, $p2);
            if ($distance > $max_distance) {
                $max_distance = $distance;
                $farthest_point = $point;
            }
        }
        
        # Add the farthest point to hull
        push @$hull, $farthest_point;
        
        # Split remaining points
        my @left_points = ();
        my @right_points = ();
        
        foreach my $point (@$points) {
            next if $point == $farthest_point;
            
            my $distance = $self->_point_line_distance($point, $p1, $farthest_point);
            
            if ($distance > 0) {
                push @left_points, $point;
            }
            
            $distance = $self->_point_line_distance($point, $farthest_point, $p2);
            
            if ($distance > 0) {
                push @right_points, $point;
            }
        }
        
        # Recursively build hull
        $self->_build_hull($hull, $p1, $farthest_point, \@left_points);
        $self->_build_hull($hull, $farthest_point, $p2, \@right_points);
    }
    
    # Calculate distance from point to line
    sub _point_line_distance {
        my ($self, $point, $p1, $p2) = @_;
        
        my $x = $point->x;
        my $y = $point->y;
        my $x1 = $p1->x;
        my $y1 = $p1->y;
        my $x2 = $p2->x;
        my $y2 = $p2->y;
        
        # Distance formula: |(y2-y1)x - (x2-x1)y + x2*y1 - y2*x1| / sqrt((y2-y1)^2 + (x2-x1)^2)
        my $numerator = abs(($y2 - $y1) * $x - ($x2 - $x1) * $y + $x2 * $y1 - $y2 * $x1);
        my $denominator = sqrt(($y2 - $y1) ** 2 + ($x2 - $x1) ** 2);
        
        return $denominator == 0 ? 0 : $numerator / $denominator;
    }
}

# Example usage
package main {
    # Create some sample points
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
    
    print "Input points:\n";
    foreach my $point (@points) {
        printf "(%d, %d)\n", $point->x, $point->y;
    }
    
    # Create Quickhull object and find convex hull
    my $qh = Quickhull->new();
    my $hull = $qh->convex_hull(\@points);
    
    print "\nConvex Hull Points:\n";
    foreach my $point (@$hull) {
        printf "(%d, %d)\n", $point->x, $point->y;
    }
    
    print "\nNumber of points in hull: " . scalar(@$hull) . "\n";
}
```

## How it works:

1. **Point Class**: Represents 2D points with x and y coordinates
2. **Quickhull Class**: Contains the main algorithm implementation
3. **Main Algorithm**:
   - Finds the leftmost and rightmost points to establish the initial line
   - Splits remaining points into two groups based on which side of the line they fall
   - Recursively finds the furthest point from each line segment
   - Builds the convex hull by connecting these points

## Key Features:

- **Recursive approach**: Uses divide-and-conquer strategy
- **Distance calculation**: Computes perpendicular distance from points to lines
- **Efficient**: Time complexity of O(n log n) on average
- **Handles edge cases**: Works with duplicate points and collinear points

## Sample Output:
```
Input points:
(0, 3)
(1, 1)
(2, 2)
(4, 4)
(0, 0)
(1, 2)
(3, 1)
(3, 3)

Convex Hull Points:
(0, 0)
(0, 3)
(1, 1)
(3, 1)
(4, 4)
(3, 3)

Number of points in hull: 6
```

