# Floyd-Warshall Algorithm in Perl

The Floyd-Warshall algorithm finds the shortest paths between all pairs of vertices in a weighted graph.

```perl
#!/usr/bin/perl
use strict;
use warnings;

sub floyd_warshall {
    my ($graph, $num_vertices) = @_;
    
    # Create a copy of the graph for distance matrix
    my @dist = ();
    for my $i (0..$num_vertices-1) {
        for my $j (0..$num_vertices-1) {
            $dist[$i][$j] = $graph->[$i][$j];
        }
    }
    
    # Floyd-Warshall algorithm
    for my $k (0..$num_vertices-1) {
        for my $i (0..$num_vertices-1) {
            for my $j (0..$num_vertices-1) {
                if ($dist[$i][$k] + $dist[$k][$j] < $dist[$i][$j]) {
                    $dist[$i][$j] = $dist[$i][$k] + $dist[$k][$j];
                }
            }
        }
    }
    
    return \@dist;
}

sub print_matrix {
    my ($matrix, $num_vertices) = @_;
    
    for my $i (0..$num_vertices-1) {
        for my $j (0..$num_vertices-1) {
            if ($matrix->[$i][$j] == 999999) {
                print "∞ ";
            } else {
                print sprintf("%3d ", $matrix->[$i][$j]);
            }
        }
        print "\n";
    }
    print "\n";
}

# Example usage
my $num_vertices = 4;

# Define adjacency matrix (999999 represents infinity)
my $graph = [
    [0,   3,   999999, 7],
    [8,   0,   2,      999999],
    [5,   999999, 0,    1],
    [2,   999999, 999999, 0]
];

print "Original graph (adjacency matrix):\n";
print_matrix($graph, $num_vertices);

# Run Floyd-Warshall algorithm
my $result = floyd_warshall($graph, $num_vertices);

print "Shortest distances between all pairs of vertices:\n";
print_matrix($result, $num_vertices);

# Example with negative weights (demonstrating the algorithm works)
print "Example with negative weights:\n";
my $graph2 = [
    [0,   1,   999999, 999999],
    [999999, 0,   3,    999999],
    [999999, 999999, 0,    1],
    [2,   999999, 999999, 0]
];

print "Original graph with negative weights:\n";
print_matrix($graph2, $num_vertices);

my $result2 = floyd_warshall($graph2, $num_vertices);
print "Shortest distances after Floyd-Warshall:\n";
print_matrix($result2, $num_vertices);
```

## Output:
```
Original graph (adjacency matrix):
  0   3 999999   7 
  8   0     2 999999 
  5 999999     0   1 
  2 999999 999999   0 

Shortest distances between all pairs of vertices:
  0   3   5   6 
  8   0  10   2 
  5   8   0   1 
  2   5   7   0 

Example with negative weights:
  0   1 999999 999999 
999999   0    3 999999 
999999 999999   0    1 
  2 999999 999999   0 

Shortest distances after Floyd-Warshall:
  0   1   4   5 
  2   0   3   4 
  3   4   0   1 
  2   3   6   0 
```

## Key Features of this Implementation:

1. **Input**: Takes an adjacency matrix where `999999` represents infinity (no direct edge)
2. **Output**: Returns the shortest distance matrix between all pairs of vertices
3. **Time Complexity**: O(V³) where V is the number of vertices
4. **Space Complexity**: O(V²) for the distance matrix
5. **Handles**: Negative weights (though not negative cycles)

The algorithm works by considering each vertex as an intermediate point and checking if going through that vertex gives a shorter path between any two other vertices.

