# Ukkonen's Suffix Tree Algorithm in Perl

Here's an implementation of Ukkonen's algorithm for building suffix trees in Perl:

```perl
#!/usr/bin/perl
use strict;
use warnings;

package SuffixTreeNode {
    sub new {
        my ($class, $start, $end) = @_;
        my $self = {
            start => $start,
            end => $end,
            children => {},
            suffix_index => -1,
            suffix_link => undef
        };
        return bless $self, $class;
    }
    
    sub get_length {
        my ($self) = @_;
        return $self->{end} - $self->{start} + 1;
    }
    
    sub get_char {
        my ($self, $text, $index) = @_;
        return $text->[$self->{start} + $index];
    }
}

package SuffixTree {
    sub new {
        my ($class, $text) = @_;
        my $self = {
            text => [split //, $text],
            root => SuffixTreeNode->new(0, -1),
            active_node => undef,
            active_edge => -1,
            active_length => 0,
            remainder => 0,
            leaf_end => 0,
            suffix_root => undef
        };
        $self->{active_node} = $self->{root};
        return bless $self, $class;
    }
    
    sub extend_suffix_tree {
        my ($self, $pos) = @_;
        
        $self->{remainder}++;
        $self->{leaf_end} = $pos;
        
        my $last_new_node = undef;
        
        while ($self->{remainder} > 0) {
            if ($self->{active_length} == 0) {
                $self->{active_edge} = $pos;
            }
            
            my $active_node = $self->{active_node};
            my $active_edge_char = $self->{text}->[$self->{active_edge}];
            
            if (!exists $active_node->{children}->{$active_edge_char}) {
                # Create new leaf node
                my $new_node = SuffixTreeNode->new($pos, $self->{leaf_end});
                $active_node->{children}->{$active_edge_char} = $new_node;
                
                # Rule 2: Create suffix link
                if ($last_new_node ne undef) {
                    $last_new_node->{suffix_link} = $active_node;
                }
                $last_new_node = $new_node;
            } else {
                my $next_node = $active_node->{children}->{$active_edge_char};
                my $edge_length = $next_node->get_length();
                
                if ($self->{active_length} >= $edge_length) {
                    # Move to next node
                    $self->{active_edge} += $edge_length;
                    $self->{active_length} -= $edge_length;
                    $self->{active_node} = $next_node;
                    next;
                }
                
                # Check if we're at the end of the edge
                my $edge_char = $self->{text}->[$next_node->{start} + $self->{active_length}];
                if ($self->{text}->[$pos] eq $edge_char) {
                    # Rule 3: Character matches
                    if ($last_new_node ne undef) {
                        $last_new_node->{suffix_link} = $active_node;
                    }
                    $self->{active_length}++;
                    last;
                }
                
                # Rule 2: Split the edge
                my $split_end = $next_node->{start} + $self->{active_length} - 1;
                my $split_node = SuffixTreeNode->new($next_node->{start}, $split_end);
                $active_node->{children}->{$active_edge_char} = $split_node;
                
                my $new_leaf = SuffixTreeNode->new($pos, $self->{leaf_end});
                $split_node->{children}->{$self->{text}->[$pos]} = $new_leaf;
                
                $next_node->{start} += $self->{active_length};
                $split_node->{children}->{$self->{text}->[$next_node->{start}]} = $next_node;
                
                if ($last_new_node ne undef) {
                    $last_new_node->{suffix_link} = $split_node;
                }
                $last_new_node = $split_node;
            }
            
            $self->{remainder}--;
            if ($self->{active_node} eq $self->{root} && $self->{active_length} > 0) {
                $self->{active_length}--;
                $self->{active_edge} = $pos - $self->{remainder} + 1;
            } elsif ($self->{active_node} ne $self->{root}) {
                $self->{active_node} = $self->{active_node}->{suffix_link} || $self->{root};
            }
        }
    }
    
    sub build_suffix_tree {
        my ($self) = @_;
        my $text_length = @{$self->{text}};
        
        for my $i (0..$text_length-1) {
            $self->extend_suffix_tree($i);
        }
    }
    
    sub print_tree {
        my ($self, $node, $prefix) = @_;
        $prefix //= "";
        
        if ($node) {
            if ($node->{suffix_index} >= 0) {
                print $prefix . " [Leaf: " . $node->{suffix_index} . "]\n";
            } else {
                print $prefix . " [Internal]\n";
            }
            
            for my $char (sort keys %{$node->{children}}) {
                my $child = $node->{children}->{$char};
                my $edge_text = join("", @{$self->{text}}[$child->{start}..$child->{end}]);
                print $prefix . "  $char -> ";
                $self->print_tree($child, $prefix . "    ");
            }
        }
    }
    
    sub search {
        my ($self, $pattern) = @_;
        my @pattern_chars = split //, $pattern;
        my $current_node = $self->{root};
        my $i = 0;
        
        while ($i < @pattern_chars) {
            my $char = $pattern_chars[$i];
            if (!exists $current_node->{children}->{$char}) {
                return 0;  # Pattern not found
            }
            
            my $child = $current_node->{children}->{$char};
            my $edge_length = $child->get_length();
            
            for my $j (0..$edge_length-1) {
                if ($i + $j >= @pattern_chars) {
                    return 0;
                }
                if ($self->{text}->[$child->{start} + $j] ne $pattern_chars[$i + $j]) {
                    return 0;
                }
            }
            
            $i += $edge_length;
            $current_node = $child;
        }
        
        return 1;  # Pattern found
    }
}

# Example usage
print "Building suffix tree for 'banana$\n";
my $suffix_tree = SuffixTree->new("banana$");
$suffix_tree->build_suffix_tree();

print "\nSuffix Tree Structure:\n";
$suffix_tree->print_tree($suffix_tree->{root});

print "\nSearching for patterns:\n";
print "Pattern 'ana': " . ($suffix_tree->search("ana") ? "Found" : "Not found") . "\n";
print "Pattern 'ban': " . ($suffix_tree->search("ban") ? "Found" : "Not found") . "\n";
print "Pattern 'xyz': " . ($suffix_tree->search("xyz") ? "Found" : "Not found") . "\n";
```

## Key Features of This Implementation

1. **SuffixTreeNode Class**: Represents individual nodes in the suffix tree with:
   - Start and end positions in the text
   - Children nodes
   - Suffix link for efficient traversal
   - Suffix index for leaf nodes

2. **SuffixTree Class**: Implements Ukkonen's algorithm with:
   - Active point management
   - Rule 1, 2, and 3 handling
   - Suffix link creation
   - Proper edge splitting and node creation

3. **Algorithm Steps**:
   - **Phase 1**: Build the first suffix
   - **Phase 2**: Build the second suffix, and so on
   - **Rule 1**: No extension needed
   - **Rule 2**: Create new leaf or split edge
   - **Rule 3**: Match found, extend active point

4. **Time Complexity**: O(n) where n is the length of the input string

5. **Space Complexity**: O(n) for storing the suffix tree

This implementation demonstrates the core principles of Ukkonen's algorithm for building suffix trees efficiently in linear time.

