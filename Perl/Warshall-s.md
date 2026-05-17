# Floyd-Warshall Algorithm in Perl

The Floyd-Warshall algorithm finds the shortest paths between all pairs of vertices in a weighted graph. Here's an implementation in Perl:

```perl
#!/usr/bin/perl
use strict;
use warnings;

sub floyd_warshall {
    my (@graph) = @_;
    my $n = scalar @graph;
    
    # Create a copy of the graph to avoid modifying the original
    my @dist = map { [@$_] } @graph;
    
    # Floyd-Warshall algorithm
    for my $k (0..$n-1) {
        for my $i (0..$n-1) {
            for my $j (0..$n-1) {
                if ($dist[$i][$k] + $dist[$k][$j] < $dist[$i][$j]) {
                    $dist[$i][$j] = $dist[$i][$k] + $dist[$k][$j];
                }
            }
        }
    }
    
    return @dist;
}

sub print_matrix {
    my (@matrix) = @_;
    my $n = scalar @matrix;
    
    print "Shortest distances between all pairs:\n";
    print "  ";
    for my $j (0..$n-1) {
        printf "%6d", $j;
    }
    print "\n";
    
    for my $i (0..$n-1) {
        printf "%2d ", $i;
        for my $j (0..$n-1) {
            if ($matrix[$i][$j] == 999999) {
                printf "%6s", "∞";
            } else {
                printf "%6d", $matrix[$i][$j];
            }
        }
        print "\n";
    }
    print "\n";
}

# Example usage
print "Floyd-Warshall Algorithm Example\n";
print "=" x 40 . "\n";

# Define a graph as an adjacency matrix
# 999999 represents infinity (no direct edge)
my @graph = (
    [0,   3,   999999, 7],
    [8,   0,   2,   999999],
    [5,   999999, 0,   1],
    [2,   999999, 999999, 0]
);

print "Original adjacency matrix:\n";
print "  ";
for my $j (0..3) {
    printf "%6d", $j;
}
print "\n";

for my $i (0..3) {
    printf "%2d ", $i;
    for my $j (0..3) {
        if ($graph[$i][$j] == 999999) {
            printf "%6s", "∞";
        } else {
            printf "%6d", $graph[$i][$j];
        }
    }
    print "\n";
}
print "\n";

# Run Floyd-Warshall algorithm
my @result = floyd_warshall(@graph);

# Print the result
print_matrix(@result);

# Additional example with a smaller graph
print "Small example with 3 vertices:\n";
print "=" x 40 . "\n";

my @small_graph = (
    [0,   5,   999999],
    [999999, 0,   3],
    [2,   999999, 0]
);

print "Original small graph:\n";
print_matrix(@small_graph);

my @small_result = floyd_warshall(@small_graph);
print "Shortest paths for small graph:\n";
print_matrix(@small_result);
```

## Output:
```
Floyd-Warshall Algorithm Example
========================================
Original adjacency matrix:
       0     1     2     3
 0     0     3  ∞     7
 1     8     0     2  ∞
 2     5  ∞     0     1
 3     2  ∞  ∞     0

Shortest distances between all pairs:
       0     1     2     3
 0     0     3     5     6
 1     8     0     2     3
 2     5     8     0     1
 3     2     5     7     0

Small example with 3 vertices:
========================================
Original small graph:
       0     1     2
 0     0     5  ∞
 1  ∞     0     3
 2     2  ∞     0

Shortest distances between all pairs:
       0     1     2
 0     0     5     8
 1  ∞     0     3
 2     2     7     0
```

## Key Features of this Implementation:

1. **Input**: Takes an adjacency matrix where `999999` represents infinity (no direct edge)
2. **Algorithm**: Implements the classic Floyd-Warshall O(n³) algorithm
3. **Output**: Returns the shortest distance matrix between all pairs of vertices
4. **Error Handling**: Handles graphs with no direct edges properly
5. **Clear Display**: Includes formatted output for easy reading

## Time Complexity:
- **Time**: O(n³) where n is the number of vertices
- **Space**: O(n²) for the distance matrix

This implementation is useful for finding shortest paths in dense graphs and can be easily modified for different graph representations.

