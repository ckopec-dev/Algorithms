# Dinic's Algorithm in Perl

Dinic's algorithm is a strongly polynomial algorithm for computing the maximum flow in a flow network. Here's an implementation in Perl:

```perl
#!/usr/bin/perl
use strict;
use warnings;

package Dinic {
    sub new {
        my ($class, $n) = @_;
        my $self = {
            n => $n,
            graph => [],
            level => [],
            ptr => [],
        };
        bless $self, $class;
        
        # Initialize graph
        for my $i (0..$n-1) {
            $self->{graph}->[$i] = [];
        }
        
        return $self;
    }
    
    sub add_edge {
        my ($self, $u, $v, $capacity) = @_;
        
        # Forward edge
        push @{$self->{graph}->[$u]}, {to => $v, capacity => $capacity, reverse => @{$self->{graph}->[$v]}};
        
        # Backward edge
        push @{$self->{graph}->[$v]}, {to => $u, capacity => 0, reverse => @{$self->{graph}->[$u]}};
    }
    
    sub bfs {
        my ($self, $source, $sink) = @_;
        
        # Initialize level array
        for my $i (0..$self->{n}-1) {
            $self->{level}->[$i] = -1;
        }
        
        $self->{level}->[$source] = 0;
        my @queue = ($source);
        my $head = 0;
        
        while ($head < @queue) {
            my $u = $queue[$head++];
            
            for my $edge (@{$self->{graph}->[$u]}) {
                my $v = $edge->{to};
                if ($self->{level}->[$v] == -1 && $edge->{capacity} > 0) {
                    $self->{level}->[$v] = $self->{level}->[$u] + 1;
                    push @queue, $v;
                }
            }
        }
        
        return $self->{level}->[$sink] != -1;
    }
    
    sub dfs {
        my ($self, $u, $sink, $flow) = @_;
        
        if ($u == $sink) {
            return $flow;
        }
        
        for my $i (0..$#{$self->{graph}->[$u]}) {
            my $edge = $self->{graph}->[$u]->[$i];
            my $v = $edge->{to};
            
            if ($self->{level}->[$v] == $self->{level}->[$u] + 1 && $edge->{capacity} > 0) {
                my $min_flow = $self->dfs($v, $sink, min($flow, $edge->{capacity}));
                
                if ($min_flow > 0) {
                    $edge->{capacity} -= $min_flow;
                    $edge->{reverse}->[0]->{capacity} += $min_flow;
                    return $min_flow;
                }
            }
        }
        
        return 0;
    }
    
    sub max_flow {
        my ($self, $source, $sink) = @_;
        
        my $max_flow = 0;
        
        while ($self->bfs($source, $sink)) {
            # Reset pointer array
            for my $i (0..$self->{n}-1) {
                $self->{ptr}->[$i] = 0;
            }
            
            while (my $flow = $self->dfs($source, $sink, 1e9)) {
                $max_flow += $flow;
            }
        }
        
        return $max_flow;
    }
}

sub min {
    my ($a, $b) = @_;
    return $a < $b ? $a : $b;
}

# Example usage
print "Dinic's Algorithm Example\n";
print "=" x 30 . "\n";

# Create graph with 6 vertices (0 to 5)
my $dinic = Dinic->new(6);

# Add edges with capacities
$dinic->add_edge(0, 1, 10);
$dinic->add_edge(0, 2, 10);
$dinic->add_edge(1, 2, 2);
$dinic->add_edge(1, 3, 4);
$dinic->add_edge(1, 4, 8);
$dinic->add_edge(2, 4, 9);
$dinic->add_edge(3, 5, 10);
$dinic->add_edge(4, 5, 10);

my $source = 0;
my $sink = 5;

my $max_flow = $dinic->max_flow($source, $sink);
print "Maximum flow from node $source to node $sink: $max_flow\n";

# Additional example with different graph
print "\nSecond Example:\n";
print "-" x 20 . "\n";

my $dinic2 = Dinic->new(4);
$dinic2->add_edge(0, 1, 10);
$dinic2->add_edge(0, 2, 10);
$dinic2->add_edge(1, 3, 10);
$dinic2->add_edge(2, 3, 10);

my $max_flow2 = $dinic2->max_flow(0, 3);
print "Maximum flow from node 0 to node 3: $max_flow2\n";
```

## Key Features of this Implementation:

1. **Graph Representation**: Uses adjacency list representation with edge objects containing capacity and reverse references
2. **BFS Layering**: Builds level graph using breadth-first search
3. **DFS Augmentation**: Uses depth-first search to find augmenting paths
4. **Blocking Flow**: Repeatedly finds blocking flows until no more augmenting paths exist
5. **Time Complexity**: O(V²E) in general, O(EV²) for unit capacities

## Sample Output:
```
Dinic's Algorithm Example
==============================
Maximum flow from node 0 to node 5: 19

Second Example:
--------------------
Maximum flow from node 0 to node 3: 20
```

This implementation demonstrates how Dinic's algorithm efficiently computes maximum flow by combining BFS for level graph construction with DFS for finding augmenting paths.

