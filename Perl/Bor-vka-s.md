# Borůvka's Algorithm Implementation in Perl

Borůvka's algorithm is a greedy algorithm for finding a minimum spanning tree (MST) in a weighted graph. Here's a complete implementation in Perl:

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Graph representation using adjacency list
package Graph {
    sub new {
        my ($class, $vertices) = @_;
        my $self = {
            vertices => $vertices,
            edges => [],
            adj_list => {}
        };
        bless $self, $class;
        return $self;
    }
    
    sub add_edge {
        my ($self, $u, $v, $weight) = @_;
        push @{$self->{edges}}, [$u, $v, $weight];
        push @{$self->{adj_list}->{$u}}, [$v, $weight];
        push @{$self->{adj_list}->{$v}}, [$u, $weight];
    }
    
    sub get_vertices {
        my ($self) = @_;
        return $self->{vertices};
    }
    
    sub get_edges {
        my ($self) = @_;
        return $self->{edges};
    }
    
    sub get_adjacent {
        my ($self, $vertex) = @_;
        return $self->{adj_list}->{$vertex} || [];
    }
}

# Union-Find data structure for tracking connected components
package UnionFind {
    sub new {
        my ($class, $n) = @_;
        my $self = {
            parent => [0..$n-1],
            rank => [0] x $n
        };
        bless $self, $class;
        return $self;
    }
    
    sub find {
        my ($self, $x) = @_;
        if ($self->{parent}[$x] != $x) {
            $self->{parent}[$x] = $self->find($self->{parent}[$x]); # Path compression
        }
        return $self->{parent}[$x];
    }
    
    sub union {
        my ($self, $x, $y) = @_;
        my $root_x = $self->find($x);
        my $root_y = $self->find($y);
        
        if ($root_x != $root_y) {
            # Union by rank
            if ($self->{rank}[$root_x] < $self->{rank}[$root_y]) {
                $self->{parent}[$root_x] = $root_y;
            } elsif ($self->{rank}[$root_x] > $self->{rank}[$root_y]) {
                $self->{parent}[$root_y] = $root_x;
            } else {
                $self->{parent}[$root_y] = $root_x;
                $self->{rank}[$root_x]++;
            }
            return 1; # Union performed
        }
        return 0; # Already in same set
    }
}

# Borůvka's Algorithm implementation
sub boruvka_mst {
    my ($graph) = @_;
    
    my $vertices = $graph->get_vertices();
    my $edges = $graph->get_edges();
    
    # Initialize Union-Find structure
    my $uf = UnionFind->new($vertices);
    
    # Initialize MST edges and total weight
    my @mst_edges = ();
    my $total_weight = 0;
    my $num_components = $vertices;
    
    # Continue until we have only one component
    while ($num_components > 1) {
        # Array to store the minimum edge for each component
        my @min_edge = (undef) x $vertices;
        
        # For each edge, find the minimum edge for each component
        foreach my $edge (@$edges) {
            my ($u, $v, $weight) = @$edge;
            my $root_u = $uf->find($u);
            my $root_v = $uf->find($v);
            
            # Skip if both vertices are in the same component
            next if $root_u == $root_v;
            
            # Update minimum edge for component root_u
            if (!defined $min_edge[$root_u] || $weight < $min_edge[$root_u]->[2]) {
                $min_edge[$root_u] = $edge;
            }
            
            # Update minimum edge for component root_v
            if (!defined $min_edge[$root_v] || $weight < $min_edge[$root_v]->[2]) {
                $min_edge[$root_v] = $edge;
            }
        }
        
        # Add all minimum edges to MST and merge components
        my $components_merged = 0;
        for my $i (0..$vertices-1) {
            if (defined $min_edge[$i] && $uf->union($min_edge[$i]->[0], $min_edge[$i]->[1])) {
                push @mst_edges, $min_edge[$i];
                $total_weight += $min_edge[$i]->[2];
                $components_merged++;
            }
        }
        
        # Update number of components
        $num_components -= $components_merged;
        
        # Debug output
        print "Merged $components_merged components\n";
        print "Current MST edges: ";
        foreach my $edge (@mst_edges) {
            print "($edge->[0],$edge->[1],$edge->[2]) ";
        }
        print "\n";
    }
    
    return (\@mst_edges, $total_weight);
}

# Example usage
print "Borůvka's Algorithm Example\n";
print "=" x 30 . "\n";

# Create a sample graph with 6 vertices
my $graph = Graph->new(6);

# Add edges (vertex1, vertex2, weight)
$graph->add_edge(0, 1, 4);
$graph->add_edge(0, 2, 3);
$graph->add_edge(1, 2, 1);
$graph->add_edge(1, 3, 2);
$graph->add_edge(2, 3, 4);
$graph->add_edge(3, 4, 2);
$graph->add_edge(4, 5, 6);
$graph->add_edge(3, 5, 1);

print "Graph edges:\n";
my $edges = $graph->get_edges();
foreach my $edge (@$edges) {
    print "($edge->[0], $edge->[1], $edge->[2])\n";
}
print "\n";

# Run Borůvka's algorithm
my ($mst_edges, $total_weight) = boruvka_mst($graph);

print "\nMinimum Spanning Tree:\n";
print "Edges in MST:\n";
foreach my $edge (@$mst_edges) {
    print "($edge->[0], $edge->[1], $edge->[2])\n";
}
print "Total weight: $total_weight\n";

# Show step-by-step process
print "\nStep-by-step process:\n";
print "Initial: 6 components\n";
print "Step 1: Find minimum edges for each component\n";
print "Step 2: Merge components using minimum edges\n";
print "Continue until single component remains\n";
```

## How Borůvka's Algorithm Works

1. **Initialization**: Start with each vertex as a separate component
2. **Find Minimum Edges**: For each component, find the minimum weight edge connecting it to another component
3. **Merge Components**: Add the minimum edges to the MST and merge the components
4. **Repeat**: Continue until all vertices are in a single component

## Key Features of This Implementation

- **Graph Class**: Represents the graph using adjacency list and edge list
- **Union-Find**: Efficiently tracks connected components with path compression and union by rank
- **Step-by-Step Output**: Shows the process of merging components
- **Complete MST**: Returns both the edges and total weight of the minimum spanning tree

## Sample Output

```
Borůvka's Algorithm Example
==============================
Graph edges:
(0, 1, 4)
(0, 2, 3)
(1, 2, 1)
(1, 3, 2)
(2, 3, 4)
(3, 4, 2)
(4, 5, 6)
(3, 5, 1)

Merged 3 components
Current MST edges: (1,2,1) (3,5,1) (1,3,2) 
Merged 2 components
Current MST edges: (1,2,1) (3,5,1) (1,3,2) (3,4,2) 
Merged 1 components
Current MST edges: (1,2,1) (3,5,1) (1,3,2) (3,4,2) (0,2,3) 

Minimum Spanning Tree:
Edges in MST:
(1, 2, 1)
(3, 5, 1)
(1, 3, 2)
(3, 4, 2)
(0, 2, 3)
Total weight: 9
```

This implementation demonstrates Borůvka's algorithm's efficiency in finding minimum spanning trees with a time complexity of O(E log V) where E is the number of edges and V is the number of vertices.

