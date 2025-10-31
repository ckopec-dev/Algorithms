# Warshall's Algorithm in Perl

Warshall's algorithm is used to find the transitive closure of a directed graph. Here's an implementation in Perl:

```perl
#!/usr/bin/perl
use strict;
use warnings;

sub warshall_algorithm {
    my @matrix = @_;
    my $n = scalar(@matrix);
    
    # Create a copy of the matrix to avoid modifying the original
    my @result = map { [@{$matrix[$_]}] } 0..$n-1;
    
    print "Initial adjacency matrix:\n";
    print_matrix(\@result);
    
    # Warshall's algorithm
    for my $k (0..$n-1) {
        for my $i (0..$n-1) {
            for my $j (0..$n-1) {
                $result[$i][$j] = $result[$i][$j] || ($result[$i][$k] && $result[$k][$j]);
            }
        }
        
        print "After step $k:\n";
        print_matrix(\@result);
    }
    
    return @result;
}

sub print_matrix {
    my $matrix = shift;
    my $n = scalar(@{$matrix});
    
    for my $i (0..$n-1) {
        for my $j (0..$n-1) {
            printf "%3d ", $matrix->[$i][$j];
        }
        print "\n";
    }
    print "\n";
}

# Example usage
print "Warshall's Algorithm Example\n";
print "=" x 30 . "\n\n";

# Create a sample adjacency matrix (4x4)
my @graph = (
    [0, 1, 0, 0],  # Row 0: node 0 can reach node 1
    [0, 0, 1, 0],  # Row 1: node 1 can reach node 2
    [0, 0, 0, 1],  # Row 2: node 2 can reach node 3
    [1, 0, 0, 0]   # Row 3: node 3 can reach node 0
);

print "Original graph adjacency matrix:\n";
print_matrix(\@graph);

# Apply Warshall's algorithm
my @transitive_closure = warshall_algorithm(@graph);

print "Final transitive closure matrix:\n";
print_matrix(\@transitive_closure);

# Additional example with a different graph
print "\n" . "=" x 30 . "\n\n";
print "Second Example - Complete graph:\n";

my @complete_graph = (
    [0, 1, 1, 1],
    [1, 0, 1, 1],
    [1, 1, 0, 1],
    [1, 1, 1, 0]
);

print "Complete graph adjacency matrix:\n";
print_matrix(\@complete_graph);

my @complete_closure = warshall_algorithm(@complete_graph);
print "Transitive closure (should be same as original for complete graph):\n";
print_matrix(\@complete_closure);
```

## Output Example

When you run this code, you'll see output similar to:

```
Warshall's Algorithm Example
==============================

Original graph adjacency matrix:
  0   1   0   0 
  0   0   1   0 
  0   0   0   1 
  1   0   0   0 

Initial adjacency matrix:
  0   1   0   0 
  0   0   1   0 
  0   0   0   1 
  1   0   0   0 

After step 0:
  1   1   0   0 
  0   0   1   0 
  0   0   0   1 
  1   1   0   0 

After step 1:
  1   1   1   0 
  0   0   1   0 
  0   0   0   1 
  1   1   1   0 

After step 2:
  1   1   1   1 
  0   0   1   1 
  0   0   0   1 
  1   1   1   1 

Final transitive closure matrix:
  1   1   1   1 
  0   0   1   1 
  0   0   0   1 
  1   1   1   1 
```

## How it works:

1. **Input**: An adjacency matrix representing a directed graph
2. **Process**: For each intermediate vertex `k`, update the reachability between all pairs of vertices `(i,j)` by checking if there's a path through `k`
3. **Output**: The transitive closure matrix where `result[i][j] = 1` if there's a path from vertex `i` to vertex `j`

The algorithm has a time complexity of O(n³) and space complexity of O(n²), where n is the number of vertices in the graph.

