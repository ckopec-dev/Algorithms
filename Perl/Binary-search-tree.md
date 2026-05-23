# Binary Search Tree in Perl

Here's a complete implementation of a Binary Search Tree algorithm in Perl:

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Define the TreeNode class
package TreeNode {
    sub new {
        my ($class, $value) = @_;
        my $self = {
            value => $value,
            left  => undef,
            right => undef
        };
        return bless $self, $class;
    }
    
    sub value { return $_[0]->{value}; }
    sub left  { return $_[0]->{left}; }
    sub right { return $_[0]->{right}; }
    
    sub set_left  { $_[0]->{left}  = $_[1]; }
    sub set_right { $_[0]->{right} = $_[1]; }
}

# Define the Binary Search Tree class
package BinarySearchTree {
    sub new {
        my ($class) = @_;
        my $self = {
            root => undef
        };
        return bless $self, $class;
    }
    
    # Insert a value into the BST
    sub insert {
        my ($self, $value) = @_;
        $self->{root} = $self->_insert_node($self->{root}, $value);
    }
    
    sub _insert_node {
        my ($self, $node, $value) = @_;
        
        # If tree is empty, create new node
        if (!$node) {
            return TreeNode->new($value);
        }
        
        # Recursively insert in left or right subtree
        if ($value < $node->value) {
            $node->set_left($self->_insert_node($node->left, $value));
        } elsif ($value > $node->value) {
            $node->set_right($self->_insert_node($node->right, $value));
        }
        # If value equals node value, we don't insert (no duplicates)
        
        return $node;
    }
    
    # Search for a value in the BST
    sub search {
        my ($self, $value) = @_;
        return $self->_search_node($self->{root}, $value);
    }
    
    sub _search_node {
        my ($self, $node, $value) = @_;
        
        # Base case: node not found
        if (!$node) {
            return 0;
        }
        
        # Value found
        if ($value == $node->value) {
            return 1;
        }
        
        # Search in left subtree
        if ($value < $node->value) {
            return $self->_search_node($node->left, $value);
        }
        # Search in right subtree
        else {
            return $self->_search_node($node->right, $value);
        }
    }
    
    # In-order traversal (prints values in sorted order)
    sub inorder_traversal {
        my ($self) = @_;
        print "In-order traversal: ";
        $self->_inorder($self->{root});
        print "\n";
    }
    
    sub _inorder {
        my ($self, $node) = @_;
        
        if ($node) {
            $self->_inorder($node->left);
            print $node->value . " ";
            $self->_inorder($node->right);
        }
    }
    
    # Pre-order traversal
    sub preorder_traversal {
        my ($self) = @_;
        print "Pre-order traversal: ";
        $self->_preorder($self->{root});
        print "\n";
    }
    
    sub _preorder {
        my ($self, $node) = @_;
        
        if ($node) {
            print $node->value . " ";
            $self->_preorder($node->left);
            $self->_preorder($node->right);
        }
    }
    
    # Post-order traversal
    sub postorder_traversal {
        my ($self) = @_;
        print "Post-order traversal: ";
        $self->_postorder($self->{root});
        print "\n";
    }
    
    sub _postorder {
        my ($self, $node) = @_;
        
        if ($node) {
            $self->_postorder($node->left);
            $self->_postorder($node->right);
            print $node->value . " ";
        }
    }
}

# Example usage
print "=== Binary Search Tree Example ===\n";

# Create a new BST
my $bst = BinarySearchTree->new();

# Insert values
my @values = (50, 30, 70, 20, 40, 60, 80, 10, 25, 35, 45);
print "Inserting values: " . join(", ", @values) . "\n";

foreach my $value (@values) {
    $bst->insert($value);
}

# Display traversals
$bst->inorder_traversal();
$bst->preorder_traversal();
$bst->postorder_traversal();

# Search for values
print "\n=== Search Results ===\n";
my @search_values = (25, 50, 100);
foreach my $value (@search_values) {
    if ($bst->search($value)) {
        print "Value $value found in BST\n";
    } else {
        print "Value $value NOT found in BST\n";
    }
}
```

## Output:
```
=== Binary Search Tree Example ===
Inserting values: 50, 30, 70, 20, 40, 60, 80, 10, 25, 35, 45
In-order traversal: 10 20 25 30 35 40 45 50 60 70 80 
Pre-order traversal: 50 30 20 10 25 40 35 45 70 60 80 
Post-order traversal: 10 25 20 35 45 40 30 60 80 70 50 

=== Search Results ===
Value 25 found in BST
Value 50 found in BST
Value 100 NOT found in BST
```

## Key Features:

1. **TreeNode Class**: Represents individual nodes with value, left, and right pointers
2. **BinarySearchTree Class**: Manages the tree operations
3. **Insert Method**: Maintains BST property (left < parent < right)
4. **Search Method**: Efficient O(log n) search using BST property
5. **Traversal Methods**: In-order, pre-order, and post-order traversals
6. **Error Handling**: Proper handling of empty trees and edge cases

The time complexity for search, insert, and delete operations in a balanced BST is O(log n), while in the worst case (unbalanced tree), it can degrade to O(n).

