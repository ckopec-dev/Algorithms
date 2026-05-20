# Bellman-Ford Algorithm in Perl

Here's an implementation of the Bellman-Ford algorithm in Perl to find shortest paths from a source vertex to all other vertices in a weighted graph:

```perl
#!/usr/bin/perl
use strict;
use warnings;

sub bellman_ford {
    my ($graph, $source, $num_vertices) = @_;
    
    # Initialize distances array with infinity (represented as a large number)
    my @distances = (1e9) x $num_vertices;
    $distances[$source] = 0;
    
    # Relax edges repeatedly
    for my $i (0..$num_vertices - 2) {
        for my $edge (@$graph) {
            my ($u, $v, $weight) = @$edge;
            if ($distances[$u] != 1e9 && $distances[$u] + $weight < $distances[$v]) {
                $distances[$v] = $distances[$u] + $weight;
            }
        }
    }
    
    # Check for negative weight cycles
    for my $edge (@$graph) {
        my ($u, $v, $weight) = @$edge;
        if ($distances[$u] != 1e9 && $distances[$u] + $weight < $distances[$v]) {
            die "Graph contains negative weight cycle!";
        }
    }
    
    return \@distances;
}

# Example usage
sub main {
    # Define graph as array of edges [source, destination, weight]
    my $graph = [
        [0, 1, 4],
        [0, 2, 2],
        [1, 2, 1],
        [1, 3, 5],
        [2, 3, 8],
        [2, 4, 10],
        [3, 4, 2]
    ];
    
    my $num_vertices = 5;
    my $source = 0;
    
    print "Running Bellman-Ford algorithm...\n";
    print "Graph edges: ";
    for my $edge (@$graph) {
        print "($edge->[0],$edge->[1],$edge->[2]) ";
    }
    print "\n";
    
    my $distances = bellman_ford($graph, $source, $num_vertices);
    
    print "\nShortest distances from vertex $source:\n";
    for my $i (0..$#{$distances}) {
        if ($distances->[$i] == 1e9) {
            print "Vertex $i: INF\n";
        } else {
            print "Vertex $i: $distances->[$i]\n";
        }
    }
}

main();
```

## Output
```
Running Bellman-Ford algorithm...
Graph edges: (0,1,4) (0,2,2) (1,2,1) (1,3,5) (2,3,8) (2,4,10) (3,4,2) 

Shortest distances from vertex 0:
Vertex 0: 0
Vertex 1: 4
Vertex 2: 2
Vertex 3: 9
Vertex 4: 11
```

## How it works:

1. **Initialization**: Set distance to source vertex as 0, all others as infinity
2. **Relaxation**: Repeat V-1 times, where V is number of vertices
3. **Negative Cycle Detection**: Check if any more improvements can be made
4. **Return**: Final distances from source to all vertices

## Key Features:

- Handles negative edge weights
- Detects negative weight cycles
- Returns shortest paths from source to all vertices
- Time complexity: O(VE) where V is vertices and E is edges
- Space complexity: O(V)

The algorithm is particularly useful when dealing with graphs that may contain negative edge weights, unlike Dijkstra's algorithm which requires non-negative weights.

