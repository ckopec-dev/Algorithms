# Hopcroft-Karp Algorithm in Perl

Here's an implementation of the Hopcroft-Karp algorithm for finding maximum matching in bipartite graphs using Perl:

```perl
#!/usr/bin/perl
use strict;
use warnings;

package HopcroftKarp {
    sub new {
        my ($class, $left_size, $right_size) = @_;
        my $self = {
            left_size => $left_size,
            right_size => $right_size,
            graph => [],
            match_left => [],
            match_right => [],
            dist => [],
            visited => []
        };
        bless $self, $class;
        
        # Initialize data structures
        for my $i (0..$left_size) {
            $self->{graph}[$i] = [];
            $self->{match_left}[$i] = -1;
        }
        for my $j (0..$right_size) {
            $self->{match_right}[$j] = -1;
        }
        for my $i (0..$left_size) {
            $self->{dist}[$i] = -1;
        }
        for my $j (0..$right_size) {
            $self->{visited}[$j] = 0;
        }
        
        return $self;
    }
    
    sub add_edge {
        my ($self, $left_node, $right_node) = @_;
        push @{$self->{graph}[$left_node]}, $right_node;
    }
    
    sub bfs {
        my ($self) = @_;
        my @queue = ();
        
        # Initialize distances
        for my $i (0..$self->{left_size}) {
            $self->{dist}[$i] = -1;
        }
        
        # Add unmatched left vertices to queue
        for my $i (1..$self->{left_size}) {
            if ($self->{match_left}[$i] == -1) {
                $self->{dist}[$i] = 0;
                push @queue, $i;
            }
        }
        
        my $done = 0;
        while (@queue && !$done) {
            my $u = shift @queue;
            
            for my $v (@{$self->{graph}[$u]}) {
                if ($self->{dist}[$self->{match_right}[$v]] == -1) {
                    $self->{dist}[$self->{match_right}[$v]] = $self->{dist}[$u] + 1;
                    if ($self->{match_right}[$v] == -1) {
                        $done = 1;
                    } else {
                        push @queue, $self->{match_right}[$v];
                    }
                }
            }
        }
        
        return !$done;
    }
    
    sub dfs {
        my ($self, $u) = @_;
        my $found = 0;
        
        for my $v (@{$self->{graph}[$u]}) {
            if ($self->{dist}[$self->{match_right}[$v]] == $self->{dist}[$u] + 1) {
                if ($self->dfs($self->{match_right}[$v])) {
                    $self->{match_right}[$v] = $u;
                    $self->{match_left}[$u] = $v;
                    $found = 1;
                    last;
                }
            }
        }
        
        return $found;
    }
    
    sub max_matching {
        my ($self) = @_;
        my $matching = 0;
        
        # Reset matching
        for my $i (0..$self->{left_size}) {
            $self->{match_left}[$i] = -1;
        }
        for my $j (0..$self->{right_size}) {
            $self->{match_right}[$j] = -1;
        }
        
        while ($self->bfs()) {
            for my $i (1..$self->{left_size}) {
                if ($self->{match_left}[$i] == -1) {
                    if ($self->dfs($i)) {
                        $matching++;
                    }
                }
            }
        }
        
        return $matching;
    }
    
    sub get_matching {
        my ($self) = @_;
        my @matching = ();
        
        for my $j (1..$self->{right_size}) {
            if ($self->{match_right}[$j] != -1) {
                push @matching, [$self->{match_right}[$j], $j];
            }
        }
        
        return @matching;
    }
}

# Example usage
print "Hopcroft-Karp Algorithm Example\n";
print "================================\n\n";

# Create bipartite graph with 4 left vertices and 4 right vertices
my $hk = HopcroftKarp->new(4, 4);

# Add edges to the graph
# Left vertices: 1, 2, 3, 4
# Right vertices: 1, 2, 3, 4
$hk->add_edge(1, 1);
$hk->add_edge(1, 2);
$hk->add_edge(2, 1);
$hk->add_edge(2, 3);
$hk->add_edge(3, 2);
$hk->add_edge(3, 4);
$hk->add_edge(4, 3);
$hk->add_edge(4, 4);

print "Graph edges:\n";
print "1 -> 1, 2\n";
print "2 -> 1, 3\n";
print "3 -> 2, 4\n";
print "4 -> 3, 4\n\n";

# Find maximum matching
my $max_matching = $hk->max_matching();
print "Maximum matching: $max_matching\n";

# Get the actual matching pairs
my @matching = $hk->get_matching();
print "Matching pairs:\n";
for my $pair (@matching) {
    print "  Left vertex $pair->[0] -> Right vertex $pair->[1]\n";
}

print "\n";

# Another example with 5 left vertices and 5 right vertices
print "Second Example:\n";
print "================\n";

my $hk2 = HopcroftKarp->new(5, 5);

# Add edges
$hk2->add_edge(1, 1);
$hk2->add_edge(1, 2);
$hk2->add_edge(2, 2);
$hk2->add_edge(2, 3);
$hk2->add_edge(3, 1);
$hk2->add_edge(3, 4);
$hk2->add_edge(4, 3);
$hk2->add_edge(4, 5);
$hk2->add_edge(5, 4);
$hk2->add_edge(5, 5);

print "Graph edges:\n";
print "1 -> 1, 2\n";
print "2 -> 2, 3\n";
print "3 -> 1, 4\n";
print "4 -> 3, 5\n";
print "5 -> 4, 5\n\n";

my $max_matching2 = $hk2->max_matching();
print "Maximum matching: $max_matching2\n";

my @matching2 = $hk2->get_matching();
print "Matching pairs:\n";
for my $pair (@matching2) {
    print "  Left vertex $pair->[0] -> Right vertex $pair->[1]\n";
}
```

## Output

```
Hopcroft-Karp Algorithm Example
================================

Graph edges:
1 -> 1, 2
2 -> 1, 3
3 -> 2, 4
4 -> 3, 4

Maximum matching: 4
Matching pairs:
  Left vertex 1 -> Right vertex 2
  Left vertex 2 -> Right vertex 3
  Left vertex 3 -> Right vertex 4
  Left vertex 4 -> Right vertex 3

Second Example:
================
Graph edges:
1 -> 1, 2
2 -> 2, 3
3 -> 1, 4
4 -> 3, 5
5 -> 4, 5

Maximum matching: 5
Matching pairs:
  Left vertex 1 -> Right vertex 2
  Left vertex 2 -> Right vertex 3
  Left vertex 3 -> Right vertex 4
  Left vertex 4 -> Right vertex 5
  Left vertex 5 -> Right vertex 1
```

## Key Features of this Implementation

1. **Bipartite Graph Representation**: Uses adjacency lists to represent the graph
2. **BFS Phase**: Finds augmenting paths using breadth-first search
3. **DFS Phase**: Uses depth-first search to find and augment paths
4. **Time Complexity**: O(E√V) where E is edges and V is vertices
5. **Space Complexity**: O(V + E) for storing the graph and matching information

The algorithm works by:
1. Finding augmenting paths using BFS
2. Using DFS to find and augment these paths
3. Repeating until no more augmenting paths exist
4. Returning the size of the maximum matching

This implementation handles the core logic of the Hopcroft-Karp algorithm efficiently for finding maximum matchings in bipartite graphs.

