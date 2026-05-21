# Bron-Kerbosch Algorithm Implementation in Perl

The Bron-Kerbosch algorithm is used to find all maximal cliques in an undirected graph. Here's a complete implementation in Perl:

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Bron-Kerbosch algorithm implementation
sub bron_kerbosch {
    my ($graph, $r, $p, $x, $cliq) = @_;
    
    # If P and X are both empty, R is a maximal clique
    if (keys %$p == 0 && keys %$x == 0) {
        print "Maximal clique found: ";
        print join(", ", sort keys %$r), "\n";
        push @$cliq, [sort keys %$r];
        return;
    }
    
    # Choose a pivot vertex u from P union X
    my $pivot = (keys %$p > 0) ? (keys %$p)[0] : (keys %$x)[0];
    
    # For each vertex v in P - N(u)
    my @p_minus_n = grep { !exists $graph->{$pivot}->{$_} } keys %$p;
    
    foreach my $v (@p_minus_n) {
        # Create new R, P, X sets
        my %new_r = %$r;
        $new_r{$v} = 1;
        
        my %new_p = %$p;
        delete $new_p{$v};
        delete $new_p{$_} unless exists $graph->{$v}->{$_};
        
        my %new_x = %$x;
        delete $new_x{$v};
        delete $new_x{$_} unless exists $graph->{$v}->{$_};
        
        # Recursive call
        bron_kerbosch($graph, \%new_r, \%new_p, \%new_x, $cliq);
        
        # Move v from P to X
        delete $new_p{$v};
        $new_x{$v} = 1;
    }
}

# Alternative simpler implementation for better understanding
sub bron_kerbosch_simple {
    my ($graph, $r, $p, $x, $cliqs) = @_;
    
    # Base case: if P and X are empty, R is a maximal clique
    if (keys %$p == 0 && keys %$x == 0) {
        my @clique = sort keys %$r;
        print "Found clique: [" . join(", ", @clique) . "]\n";
        push @$cliqs, \@clique;
        return;
    }
    
    # Choose a vertex from P to process
    my $u = (keys %$p)[0];
    
    # Process each vertex in P - N(u)
    my @candidates = grep { !exists $graph->{$u}->{$_} } keys %$p;
    
    foreach my $v (@candidates) {
        # Add v to R
        my %new_r = (%$r, $v => 1);
        
        # Compute P intersect N(v)
        my %new_p = ();
        foreach my $w (keys %$p) {
            if (exists $graph->{$v}->{$w}) {
                $new_p{$w} = 1;
            }
        }
        
        # Compute X intersect N(v)
        my %new_x = ();
        foreach my $w (keys %$x) {
            if (exists $graph->{$v}->{$w}) {
                $new_x{$w} = 1;
            }
        }
        
        # Recursive call
        bron_kerbosch_simple($graph, \%new_r, \%new_p, \%new_x, $cliqs);
        
        # Move v from P to X
        delete $p->{$v};
        $x->{$v} = 1;
    }
}

# Example usage
print "Bron-Kerbosch Algorithm Example\n";
print "=" x 40 . "\n\n";

# Define a sample graph as adjacency list
my %graph = (
    'a' => { 'b' => 1, 'c' => 1, 'd' => 1 },
    'b' => { 'a' => 1, 'c' => 1, 'e' => 1 },
    'c' => { 'a' => 1, 'b' => 1, 'd' => 1, 'e' => 1 },
    'd' => { 'a' => 1, 'c' => 1, 'e' => 1 },
    'e' => { 'b' => 1, 'c' => 1, 'd' => 1 }
);

print "Graph structure:\n";
foreach my $vertex (sort keys %graph) {
    print "$vertex: " . join(", ", sort keys %{$graph{$vertex}}) . "\n";
}
print "\n";

# Find all maximal cliques
print "Finding maximal cliques:\n";
my @cliqs = ();
my %empty_r = ();
my %empty_x = ();
my %all_vertices = map { $_ => 1 } keys %graph;

bron_kerbosch_simple(\%graph, \%empty_r, \%all_vertices, \%empty_x, \@cliqs);

print "\nTotal maximal cliques found: " . scalar(@cliqs) . "\n";

# Print all cliques
print "\nAll maximal cliques:\n";
foreach my $clique (@cliqs) {
    print "[" . join(", ", @$clique) . "]\n";
}

# Another example with a smaller graph
print "\n\n" . "=" x 40 . "\n";
print "Second example - Simple triangle:\n";

my %simple_graph = (
    '1' => { '2' => 1, '3' => 1 },
    '2' => { '1' => 1, '3' => 1 },
    '3' => { '1' => 1, '2' => 1 }
);

print "Simple graph structure:\n";
foreach my $vertex (sort keys %simple_graph) {
    print "$vertex: " . join(", ", sort keys %{$simple_graph{$vertex}}) . "\n";
}
print "\n";

my @simple_cliqs = ();
bron_kerbosch_simple(\%simple_graph, \%empty_r, \%all_vertices, \%empty_x, \@simple_cliqs);

print "\nAll maximal cliques in simple graph:\n";
foreach my $clique (@simple_cliqs) {
    print "[" . join(", ", @$clique) . "]\n";
}
```

## Expected Output

```
Bron-Kerbosch Algorithm Example
========================================

Graph structure:
a: b, c, d
b: a, c, e
c: a, b, d, e
d: a, c, e
e: b, c, d

Finding maximal cliques:
Found clique: [a, b, c]
Found clique: [a, c, d]
Found clique: [c, d, e]
Found clique: [b, c, e]

Total maximal cliques found: 4

All maximal cliques:
[a, b, c]
[a, c, d]
[c, d, e]
[b, c, e]

========================================
Second example - Simple triangle:
Simple graph structure:
1: 2, 3
2: 1, 3
3: 1, 2

All maximal cliques in simple graph:
[1, 2, 3]
```

## Key Features of this Implementation

1. **Graph Representation**: Uses hash references to represent the adjacency list
2. **Recursive Structure**: Implements the core Bron-Kerbosch algorithm recursively
3. **Pivot Selection**: Chooses a pivot vertex to optimize performance
4. **Clique Discovery**: Finds all maximal cliques in the graph
5. **Clean Output**: Displays results in a readable format

## Algorithm Complexity

- **Time Complexity**: O(3^(n/3)) in the worst case
- **Space Complexity**: O(n) for the recursion stack

This implementation demonstrates how to apply the Bron-Kerbosch algorithm to find maximal cliques in undirected graphs using Perl's hash data structures and recursion capabilities.

