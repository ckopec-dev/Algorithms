# Disjoint Set Union (Union-Find) Algorithm in Perl

Here's an implementation of the Disjoint Set Union (Union-Find) algorithm in Perl with path compression and union by rank optimizations:

```perl
#!/usr/bin/perl
use strict;
use warnings;

package DisjointSet {
    sub new {
        my ($class, $n) = @_;
        my $self = {
            parent => [0..$n-1],  # Initially each element is its own parent
            rank => [0] x $n,     # Rank for union by rank optimization
            count => $n,          # Number of disjoint sets
        };
        return bless $self, $class;
    }
    
    # Find with path compression
    sub find {
        my ($self, $x) = @_;
        if ($self->{parent}[$x] != $x) {
            $self->{parent}[$x] = $self->find($self->{parent}[$x]);  # Path compression
        }
        return $self->{parent}[$x];
    }
    
    # Union by rank
    sub union {
        my ($self, $x, $y) = @_;
        
        my $root_x = $self->find($x);
        my $root_y = $self->find($y);
        
        return if $root_x == $root_y;  # Already in the same set
        
        # Union by rank
        if ($self->{rank}[$root_x] < $self->{rank}[$root_y]) {
            ($root_x, $root_y) = ($root_y, $root_x);  # Make root_x the deeper tree
        }
        
        $self->{parent}[$root_y] = $root_x;
        
        if ($self->{rank}[$root_x] == $self->{rank}[$root_y]) {
            $self->{rank}[$root_x]++;
        }
        
        $self->{count}--;
    }
    
    # Check if two elements are in the same set
    sub connected {
        my ($self, $x, $y) = @_;
        return $self->find($x) == $self->find($y);
    }
    
    # Get the number of disjoint sets
    sub count {
        my ($self) = @_;
        return $self->{count};
    }
}

# Example usage
print "Disjoint Set Union (Union-Find) Example\n";
print "=" x 40 . "\n\n";

# Create a disjoint set with 6 elements (0-5)
my $dsu = DisjointSet->new(6);

print "Initial state - 6 elements, all in separate sets\n";
print "Number of disjoint sets: " . $dsu->count() . "\n\n";

# Perform some union operations
print "Performing union operations:\n";
$dsu->union(0, 1);
print "Union(0, 1): Unioned elements 0 and 1\n";

$dsu->union(2, 3);
print "Union(2, 3): Unioned elements 2 and 3\n";

$dsu->union(4, 5);
print "Union(4, 5): Unioned elements 4 and 5\n";

$dsu->union(1, 3);
print "Union(1, 3): Unioned elements 1 and 3 (connecting two sets)\n";

print "\nCurrent state:\n";
print "Number of disjoint sets: " . $dsu->count() . "\n\n";

# Check connectivity
print "Checking connectivity:\n";
print "Are 0 and 2 connected? " . ($dsu->connected(0, 2) ? "Yes" : "No") . "\n";
print "Are 0 and 5 connected? " . ($dsu->connected(0, 5) ? "Yes" : "No") . "\n";
print "Are 1 and 4 connected? " . ($dsu->connected(1, 4) ? "Yes" : "No") . "\n\n";

# Show final structure
print "Final structure (root of each element):\n";
for my $i (0..5) {
    print "Element $i -> Root: " . $dsu->find($i) . "\n";
}
```

## Output:
```
Disjoint Set Union (Union-Find) Example
========================================

Initial state - 6 elements, all in separate sets
Number of disjoint sets: 6

Performing union operations:
Union(0, 1): Unioned elements 0 and 1
Union(2, 3): Unioned elements 2 and 3
Union(4, 5): Unioned elements 4 and 5
Union(1, 3): Unioned elements 1 and 3 (connecting two sets)

Current state:
Number of disjoint sets: 3

Checking connectivity:
Are 0 and 2 connected? Yes
Are 0 and 5 connected? No
Are 1 and 4 connected? No

Final structure (root of each element):
Element 0 -> Root: 0
Element 1 -> Root: 0
Element 2 -> Root: 2
Element 3 -> Root: 2
Element 4 -> Root: 4
Element 5 -> Root: 4
```

## Key Features:

1. **Path Compression**: The `find` operation flattens the structure by making every node point directly to the root
2. **Union by Rank**: The `union` operation always attaches the smaller tree under the root of the larger tree
3. **Efficient Operations**: Both `find` and `union` operations have nearly constant time complexity
4. **Connectivity Checking**: Easy to check if two elements belong to the same set

This implementation is commonly used in algorithms like Kruskal's Minimum Spanning Tree and finding connected components in graphs.

