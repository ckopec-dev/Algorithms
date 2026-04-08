**Morris In‑order Traversal in Perl**

Below is a self‑contained Perl script that builds a small binary tree, performs a Morris inorder traversal (O(1) extra space, O(n) time), and prints the visited node values.

```perl
#!/usr/bin/env perl
use strict;
use warnings;
use feature 'say';

# ---------- Simple binary‑tree node ----------
{
    package Node;
    sub new {
        my ($class, $value, $left, $right) = @_;
        bless { val => $value,
                left => $left,
                right => $right }, $class;
    }
    sub value   { shift->{val} }
    sub left    { shift->{left} }
    sub right   { shift->{right} }
    sub set_left  { $_[0]{left}  = $_[1] }
    sub set_right { $_[0]{right} = $_[1] }
}

# ---------- Morris In‑order Traversal ----------
sub morris_inorder {
    my ($root) = @_;
    my $curr = $root;
    my @result;   # collect values for demonstration

    while (defined $curr) {
        if (not defined $curr->left) {          # no left subtree
            push @result, $curr->value;
            $curr = $curr->right;
        } else {
            # Find the inorder predecessor of current
            my $pred = $curr->left;
            while (defined $pred->right && $pred->right != $curr) {
                $pred = $pred->right;
            }

            if (not defined $pred->right) {     # first time we see predecessor
                $pred->set_right($curr);        # make temporary thread
                $curr = $curr->left;
            } else {                            # thread already exists
                $pred->set_right(undef);        # remove the thread
                push @result, $curr->value;
                $curr = $curr->right;
            }
        }
    }
    return @result;
}

# ---------- Build a sample tree ----------
#        4
#      /   \
#     2     6
#    / \   / \
#   1   3 5   7
my $root = Node->new(4,
            Node->new(2,
                Node->new(1),
                Node->new(3)),
            Node->new(6,
                Node->new(5),
                Node->new(7)));

# ---------- Run Morris traversal ----------
my @inorder = morris_inorder($root);
say "Inorder traversal (Morris): @inorder";

# Expected output:
# Inorder traversal (Morris): 1 2 3 4 5 6 7
```

### How It Works
1. **Thread creation** – When a node has a left child, we locate its inorder predecessor (the right‑most node in the left subtree) and temporarily link the predecessor’s `right` pointer back to the current node.
2. **Visit** – If the predecessor’s `right` is already pointing to the current node, the left subtree has been processed; we remove the thread, visit the current node, and move to the right subtree.
3. **No left child** – If there is no left subtree, we simply visit the node and proceed right.

The algorithm uses only a few extra pointers (`$curr`, `$pred`) → **O(1) auxiliary space**, while still visiting each node a constant number of times → **O(n) time**. Feel free to replace the sample tree with any other binary‑tree structure; the `morris_inorder` subroutine will work unchanged.