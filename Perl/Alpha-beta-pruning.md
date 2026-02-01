# Alpha-Beta Pruning Algorithm in Perl

Here's a complete implementation of the Alpha-Beta pruning algorithm in Perl for a minimax game tree:

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Node structure for the game tree
package GameNode {
    sub new {
        my ($class, $value, $is_maximizing) = @_;
        my $self = {
            value => $value,
            is_maximizing => $is_maximizing,
            children => [],
            alpha => -1000000,
            beta => 1000000
        };
        return bless $self, $class;
    }
    
    sub add_child {
        my ($self, $child) = @_;
        push @{$self->{children}}, $child;
    }
    
    sub get_children {
        my ($self) = @_;
        return @{$self->{children}};
    }
    
    sub get_value {
        my ($self) = @_;
        return $self->{value};
    }
    
    sub set_alpha {
        my ($self, $alpha) = @_;
        $self->{alpha} = $alpha;
    }
    
    sub set_beta {
        my ($self, $beta) = @_;
        $self->{beta} = $beta;
    }
    
    sub get_alpha {
        my ($self) = @_;
        return $self->{alpha};
    }
    
    sub get_beta {
        my ($self) = @_;
        return $self->{beta};
    }
}

# Alpha-Beta Pruning Algorithm
sub alpha_beta_pruning {
    my ($node, $depth, $alpha, $beta, $is_maximizing) = @_;
    
    # Base case: if we've reached the maximum depth or leaf node
    if ($depth == 0 || @{$node->get_children()} == 0) {
        return $node->get_value();
    }
    
    if ($is_maximizing) {
        my $max_eval = -1000000;
        
        foreach my $child (@{$node->get_children()}) {
            my $eval = alpha_beta_pruning($child, $depth - 1, $alpha, $beta, 0);
            $max_eval = $eval if $eval > $max_eval;
            
            # Update alpha
            $alpha = $eval if $eval > $alpha;
            
            # Alpha-Beta Pruning: if alpha >= beta, prune remaining branches
            if ($alpha >= $beta) {
                # Optional: print pruning information
                # print "Pruned branch at depth $depth\n";
                last;
            }
        }
        
        return $max_eval;
    } else {
        my $min_eval = 1000000;
        
        foreach my $child (@{$node->get_children()}) {
            my $eval = alpha_beta_pruning($child, $depth - 1, $alpha, $beta, 1);
            $min_eval = $eval if $eval < $min_eval;
            
            # Update beta
            $beta = $eval if $eval < $beta;
            
            # Alpha-Beta Pruning: if alpha >= beta, prune remaining branches
            if ($alpha >= $beta) {
                # Optional: print pruning information
                # print "Pruned branch at depth $depth\n";
                last;
            }
        }
        
        return $min_eval;
    }
}

# Alternative implementation with full node tracking
sub alpha_beta_with_tracking {
    my ($node, $depth, $alpha, $beta, $is_maximizing, $pruning_count) = @_;
    
    # Base case
    if ($depth == 0 || @{$node->get_children()} == 0) {
        return $node->get_value();
    }
    
    if ($is_maximizing) {
        my $max_eval = -1000000;
        
        foreach my $child (@{$node->get_children()}) {
            my $eval = alpha_beta_with_tracking($child, $depth - 1, $alpha, $beta, 0, $pruning_count);
            $max_eval = $eval if $eval > $max_eval;
            
            $alpha = $eval if $eval > $alpha;
            
            if ($alpha >= $beta) {
                $$pruning_count++;
                last;
            }
        }
        
        return $max_eval;
    } else {
        my $min_eval = 1000000;
        
        foreach my $child (@{$node->get_children()}) {
            my $eval = alpha_beta_with_tracking($child, $depth - 1, $alpha, $beta, 1, $pruning_count);
            $min_eval = $eval if $eval < $min_eval;
            
            $beta = $eval if $eval < $beta;
            
            if ($alpha >= $beta) {
                $$pruning_count++;
                last;
            }
        }
        
        return $min_eval;
    }
}

# Example usage
sub create_sample_tree {
    # Create a sample game tree:
    #           A (Max)
    #          / \
    #         B   C (Min)
    #        /|   |\
    #       D E   F G
    #      3 5   2 9
    
    my $node_a = GameNode->new(0, 1);  # Max node
    my $node_b = GameNode->new(0, 1);  # Max node
    my $node_c = GameNode->new(0, 0);  # Min node
    my $node_d = GameNode->new(3, 0);  # Leaf node
    my $node_e = GameNode->new(5, 0);  # Leaf node
    my $node_f = GameNode->new(2, 0);  # Leaf node
    my $node_g = GameNode->new(9, 0);  # Leaf node
    
    # Build tree structure
    $node_a->add_child($node_b);
    $node_a->add_child($node_c);
    $node_b->add_child($node_d);
    $node_b->add_child($node_e);
    $node_c->add_child($node_f);
    $node_c->add_child($node_g);
    
    return $node_a;
}

# Main execution
print "Alpha-Beta Pruning Algorithm in Perl\n";
print "=" x 40 . "\n\n";

# Create sample tree
my $root = create_sample_tree();

# Count pruning operations
my $pruning_count = 0;

# Run alpha-beta pruning
print "Starting Alpha-Beta Pruning...\n";
print "Tree structure:\n";
print "        A (Max)\n";
print "       / \\\n";
print "      B   C (Min)\n";
print "     /|   |\\\n";
print "    D E   F G\n";
print "   3 5   2 9\n\n";

my $result = alpha_beta_pruning($root, 3, -1000000, 1000000, 1);
print "Optimal value: $result\n";
print "Pruning operations: $pruning_count\n\n";

# Demonstrate with a more complex example
print "Complex example with deeper tree:\n";
print "-" x 30 . "\n";

# Create a more complex tree for demonstration
#           Root (Max)
#          / | \
#         A  B  C (Min)
#        /|  |  |\
#       D E F G H I
#      3 5 2 9 1 8

my $root2 = GameNode->new(0, 1);
my $node_a = GameNode->new(0, 1);
my $node_b = GameNode->new(0, 0);
my $node_c = GameNode->new(0, 0);
my $node_d = GameNode->new(3, 0);
my $node_e = GameNode->new(5, 0);
my $node_f = GameNode->new(2, 0);
my $node_g = GameNode->new(9, 0);
my $node_h = GameNode->new(1, 0);
my $node_i = GameNode->new(8, 0);

$root2->add_child($node_a);
$root2->add_child($node_b);
$root2->add_child($node_c);
$node_a->add_child($node_d);
$node_a->add_child($node_e);
$node_b->add_child($node_f);
$node_b->add_child($node_g);
$node_c->add_child($node_h);
$node_c->add_child($node_i);

my $result2 = alpha_beta_pruning($root2, 3, -1000000, 1000000, 1);
print "Optimal value for complex tree: $result2\n";

print "\n";
print "Algorithm Analysis:\n";
print "• Time Complexity: O(b^(d/2)) where b is branching factor and d is depth\n";
print "• Space Complexity: O(bd) for the recursion stack\n";
print "• Pruning reduces the number of nodes evaluated\n";
print "• Alpha-Beta pruning is optimal for minimax decision making\n";
```

## Key Features of this Implementation:

1. **GameNode Class**: Represents nodes in the game tree with value, children, and alpha-beta tracking
2. **Alpha-Beta Pruning Function**: Implements the core algorithm with proper pruning conditions
3. **Recursive Structure**: Handles both maximizing and minimizing player turns
4. **Pruning Detection**: Stops evaluating branches when alpha ≥ beta
5. **Sample Trees**: Demonstrates usage with simple and complex examples

## How it Works:

1. **Maximizing Player**: Tries to maximize the value
2. **Minimizing Player**: Tries to minimize the value  
3. **Alpha**: Best value the maximizing player can guarantee
4. **Beta**: Best value the minimizing player can guarantee
5. **Pruning**: When alpha ≥ beta, further branches are pruned

This implementation shows how Alpha-Beta pruning significantly reduces the number of nodes that need to be evaluated compared to plain minimax, making it much more efficient for game tree searches.

