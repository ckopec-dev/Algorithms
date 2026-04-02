# AVL Tree Balancing Algorithm in Perl

Here's a complete implementation of an AVL tree with balancing algorithms in Perl:

```perl
#!/usr/bin/perl
use strict;
use warnings;

# AVL Tree Node Class
package AVLNode {
    sub new {
        my ($class, $value) = @_;
        my $self = {
            value => $value,
            left  => undef,
            right => undef,
            height => 1
        };
        return bless $self, $class;
    }
    
    sub get_height {
        my ($self) = @_;
        return $self->{height} || 0;
    }
    
    sub set_height {
        my ($self, $height) = @_;
        $self->{height} = $height;
    }
}

# AVL Tree Class
package AVLTree {
    sub new {
        my ($class) = @_;
        my $self = {
            root => undef
        };
        return bless $self, $class;
    }
    
    sub get_height {
        my ($self, $node) = @_;
        return $node ? $node->get_height() : 0;
    }
    
    sub get_balance_factor {
        my ($self, $node) = @_;
        return $node ? $self->get_height($node->{left}) - $self->get_height($node->{right}) : 0;
    }
    
    sub update_height {
        my ($self, $node) = @_;
        if ($node) {
            my $left_height = $self->get_height($node->{left});
            my $right_height = $self->get_height($node->{right});
            $node->set_height(1 + ($left_height > $right_height ? $left_height : $right_height));
        }
    }
    
    sub rotate_right {
        my ($self, $y) = @_;
        my $x = $y->{left};
        my $T2 = $x->{right};
        
        # Perform rotation
        $x->{right} = $y;
        $y->{left} = $T2;
        
        # Update heights
        $self->update_height($y);
        $self->update_height($x);
        
        return $x;  # New root
    }
    
    sub rotate_left {
        my ($self, $x) = @_;
        my $y = $x->{right};
        my $T2 = $y->{left};
        
        # Perform rotation
        $y->{left} = $x;
        $x->{right} = $T2;
        
        # Update heights
        $self->update_height($x);
        $self->update_height($y);
        
        return $y;  # New root
    }
    
    sub insert {
        my ($self, $value) = @_;
        $self->{root} = $self->_insert_node($self->{root}, $value);
    }
    
    sub _insert_node {
        my ($self, $node, $value) = @_;
        
        # Step 1: Perform normal BST insertion
        if (!$node) {
            return AVLNode->new($value);
        }
        
        if ($value < $node->{value}) {
            $node->{left} = $self->_insert_node($node->{left}, $value);
        } elsif ($value > $node->{value}) {
            $node->{right} = $self->_insert_node($node->{right}, $value);
        } else {
            # Duplicate values not allowed
            return $node;
        }
        
        # Step 2: Update height of current node
        $self->update_height($node);
        
        # Step 3: Get balance factor
        my $balance = $self->get_balance_factor($node);
        
        # Step 4: Perform rotations if unbalanced
        
        # Left Left Case
        if ($balance > 1 && $value < $node->{left}->{value}) {
            return $self->rotate_right($node);
        }
        
        # Right Right Case
        if ($balance < -1 && $value > $node->{right}->{value}) {
            return $self->rotate_left($node);
        }
        
        # Left Right Case
        if ($balance > 1 && $value > $node->{left}->{value}) {
            $node->{left} = $self->rotate_left($node->{left});
            return $self->rotate_right($node);
        }
        
        # Right Left Case
        if ($balance < -1 && $value < $node->{right}->{value}) {
            $node->{right} = $self->rotate_right($node->{right});
            return $self->rotate_left($node);
        }
        
        return $node;
    }
    
    sub inorder_traversal {
        my ($self) = @_;
        my @result = ();
        $self->_inorder_helper($self->{root}, \@result);
        return @result;
    }
    
    sub _inorder_helper {
        my ($self, $node, $result) = @_;
        if ($node) {
            $self->_inorder_helper($node->{left}, $result);
            push @$result, $node->{value};
            $self->_inorder_helper($node->{right}, $result);
        }
    }
    
    sub print_tree {
        my ($self) = @_;
        $self->_print_tree_helper($self->{root}, "", 1);
    }
    
    sub _print_tree_helper {
        my ($self, $node, $prefix, $is_last) = @_;
        if ($node) {
            print $prefix;
            print $is_last ? "└── " : "├── ";
            print $node->{value} . " (h:" . $node->get_height() . ")\n";
            
            my $children_count = 0;
            $children_count++ if $node->{left};
            $children_count++ if $node->{right};
            
            if ($children_count > 0) {
                if ($node->{left}) {
                    my $next_prefix = $prefix . ($is_last ? "    " : "│   ");
                    $self->_print_tree_helper($node->{left}, $next_prefix, $children_count == 1);
                }
                if ($node->{right}) {
                    my $next_prefix = $prefix . ($is_last ? "    " : "│   ");
                    $self->_print_tree_helper($node->{right}, $next_prefix, 1);
                }
            }
        }
    }
}

# Example usage
print "=== AVL Tree Balancing Example ===\n\n";

my $avl = AVLTree->new();

# Insert values that will cause rotations
my @values = (10, 20, 30, 40, 50, 25);

print "Inserting values: " . join(", ", @values) . "\n\n";

foreach my $value (@values) {
    print "Inserting $value:\n";
    $avl->insert($value);
    print "AVL Tree structure:\n";
    $avl->print_tree();
    print "\n";
}

print "Inorder traversal: " . join(", ", $avl->inorder_traversal()) . "\n";

# Test with more unbalanced insertions
print "\n=== Testing with more unbalanced insertions ===\n";
my $avl2 = AVLTree->new();
my @unbalanced_values = (100, 50, 200, 25, 75, 150, 300, 10, 35, 60, 90);

print "Inserting values: " . join(", ", @unbalanced_values) . "\n\n";

foreach my $value (@unbalanced_values) {
    $avl2->insert($value);
}

print "Final AVL Tree structure:\n";
$avl2->print_tree();
print "\nInorder traversal: " . join(", ", $avl2->inorder_traversal()) . "\n";
```

## Key Features of this AVL Implementation:

### 1. **Node Structure**
- Each node contains value, left/right children, and height
- Height is maintained for balance calculations

### 2. **Balance Operations**
- **Height Calculation**: Computes height of nodes
- **Balance Factor**: Difference between left and right subtree heights
- **Rotation Methods**: 
  - `rotate_right()` for right-heavy trees
  - `rotate_left()` for left-heavy trees

### 3. **Rebalancing Cases**
- **Left-Left Case**: Single right rotation
- **Right-Right Case**: Single left rotation  
- **Left-Right Case**: Left rotation on left child, then right rotation
- **Right-Left Case**: Right rotation on right child, then left rotation

### 4. **Insertion Process**
1. Perform normal BST insertion
2. Update heights of affected nodes
3. Calculate balance factor
4. Apply appropriate rotations if unbalanced

### 5. **Output Example**
The program demonstrates how the tree automatically rebalances itself during insertions, maintaining the AVL property where the height difference between left and right subtrees is at most 1.

This implementation shows the core AVL balancing algorithm in action, automatically maintaining tree balance through rotations whenever insertions cause the balance factor to exceed ±1.

