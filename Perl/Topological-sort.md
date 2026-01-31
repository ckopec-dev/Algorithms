# Topological Sort in Perl

Here's an example implementation of topological sort using Kahn's algorithm in Perl:

```perl
#!/usr/bin/perl
use strict;
use warnings;

sub topological_sort {
    my @graph = @_;
    
    # Build adjacency list and in-degree count
    my %adj_list = ();
    my %in_degree = ();
    
    # Initialize in-degrees for all nodes
    foreach my $node (@graph) {
        my ($from, @to) = split /\s+/, $node;
        $in_degree{$from} = 0 unless exists $in_degree{$from};
        
        foreach my $to_node (@to) {
            $in_degree{$to_node} = 0 unless exists $in_degree{$to_node};
            push @{$adj_list{$from}}, $to_node;
            $in_degree{$to_node}++;
        }
    }
    
    # Find nodes with no incoming edges
    my @queue = ();
    foreach my $node (keys %in_degree) {
        push @queue, $node if $in_degree{$node} == 0;
    }
    
    my @result = ();
    
    # Process nodes in topological order
    while (@queue) {
        my $current = shift @queue;
        push @result, $current;
        
        # Remove current node and update in-degrees
        if (exists $adj_list{$current}) {
            foreach my $neighbor (@{$adj_list{$current}}) {
                $in_degree{$neighbor}--;
                if ($in_degree{$neighbor} == 0) {
                    push @queue, $neighbor;
                }
            }
        }
    }
    
    # Check for cycles
    if (@result != scalar keys %in_degree) {
        die "Cycle detected in the graph!";
    }
    
    return @result;
}

# Example usage
print "Topological Sort Example\n";
print "=" x 30 . "\n";

# Define dependencies as "node1 node2 node3 ..." where node1 depends on node2, node3, etc.
my @dependencies = (
    "A B C",
    "B D",
    "C D",
    "D E",
    "E F"
);

print "Input dependencies:\n";
foreach my $dep (@dependencies) {
    print "  $dep\n";
}

print "\nTopological order: ";
my @sorted = topological_sort(@dependencies);
print join(" -> ", @sorted) . "\n";

# Another example with a more complex graph
print "\n\nSecond Example:\n";
print "=" x 30 . "\n";

my @complex_deps = (
    "A B C D",
    "B E",
    "C E F",
    "D F",
    "E G",
    "F G"
);

print "Input dependencies:\n";
foreach my $dep (@complex_deps) {
    print "  $dep\n";
}

print "\nTopological order: ";
my @sorted2 = topological_sort(@complex_deps);
print join(" -> ", @sorted2) . "\n";
```

## Output:
```
Topological Sort Example
==============================
Input dependencies:
  A B C
  B D
  C D
  D E
  E F

Topological order: A -> B -> C -> D -> E -> F

Second Example:
==============================
Input dependencies:
  A B C D
  B E
  C E F
  D F
  E G
  F G

Topological order: A -> B -> C -> D -> E -> F -> G
```

## How it works:

1. **Input parsing**: The algorithm takes dependency strings where each line represents a node and its dependencies
2. **Graph construction**: Builds an adjacency list and calculates in-degrees for each node
3. **Queue initialization**: Finds all nodes with zero in-degree (no prerequisites)
4. **Processing**: Removes nodes from queue, adds to result, and reduces in-degrees of neighbors
5. **Cycle detection**: If not all nodes are processed, a cycle exists

## Key Features:

- **Kahn's Algorithm**: Uses in-degree counting approach
- **Cycle Detection**: Throws error if cycle detected
- **Flexible Input**: Accepts dependencies in simple string format
- **Clear Output**: Returns nodes in proper topological order

The time complexity is O(V + E) where V is vertices and E is edges, and space complexity is O(V + E).

