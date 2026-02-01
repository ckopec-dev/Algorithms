# Floyd's Cycle Detection Algorithm in Perl

Floyd's Cycle Detection Algorithm (also known as the "Tortoise and Hare" algorithm) is used to detect cycles in linked lists. Here's an implementation in Perl:

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Define a simple linked list node structure
package ListNode {
    sub new {
        my ($class, $val) = @_;
        my $self = {
            val => $val,
            next => undef
        };
        return bless $self, $class;
    }
    
    sub get_val {
        my $self = shift;
        return $self->{val};
    }
    
    sub get_next {
        my $self = shift;
        return $self->{next};
    }
    
    sub set_next {
        my ($self, $next_node) = @_;
        $self->{next} = $next_node;
    }
}

# Floyd's Cycle Detection Algorithm
sub has_cycle {
    my ($head) = @_;
    
    # Handle edge cases
    return 0 unless $head;
    return 0 unless $head->{next};
    
    # Initialize two pointers
    my $slow = $head;
    my $fast = $head;
    
    # Move pointers through the list
    while ($fast && $fast->{next}) {
        $slow = $slow->{next};          # Move one step
        $fast = $fast->{next}->{next};  # Move two steps
        
        # If they meet, there's a cycle
        if ($slow == $fast) {
            return 1;
        }
    }
    
    # If fast reaches the end, no cycle
    return 0;
}

# Alternative version that returns the cycle start node
sub detect_cycle_start {
    my ($head) = @_;
    
    # Handle edge cases
    return undef unless $head;
    return undef unless $head->{next};
    
    # Phase 1: Detect if there's a cycle
    my $slow = $head;
    my $fast = $head;
    
    while ($fast && $fast->{next}) {
        $slow = $slow->{next};
        $fast = $fast->{next}->{next};
        
        if ($slow == $fast) {
            # Cycle detected, now find the start
            my $ptr1 = $head;
            my $ptr2 = $slow;
            
            while ($ptr1 != $ptr2) {
                $ptr1 = $ptr1->{next};
                $ptr2 = $ptr2->{next};
            }
            
            return $ptr1;  # Return the start of cycle
        }
    }
    
    return undef;  # No cycle found
}

# Helper function to create a cycle for testing
sub create_cycle {
    my ($head, $cycle_start_index) = @_;
    
    my $current = $head;
    my $cycle_start_node;
    my $index = 0;
    
    # Find the last node and the cycle start node
    while ($current->{next}) {
        if ($index == $cycle_start_index) {
            $cycle_start_node = $current;
        }
        $current = $current->{next};
        $index++;
    }
    
    # Create cycle by pointing last node to cycle_start_node
    if ($cycle_start_node) {
        $current->{next} = $cycle_start_node;
    }
    
    return $head;
}

# Example usage
print "Floyd's Cycle Detection Algorithm Demo\n";
print "=" x 40 . "\n\n";

# Create a simple linked list: 1 -> 2 -> 3 -> 4 -> 5
my $node1 = ListNode->new(1);
my $node2 = ListNode->new(2);
my $node3 = ListNode->new(3);
my $node4 = ListNode->new(4);
my $node5 = ListNode->new(5);

$node1->set_next($node2);
$node2->set_next($node3);
$node3->set_next($node4);
$node4->set_next($node5);

print "1. Testing with no cycle:\n";
print "List: 1 -> 2 -> 3 -> 4 -> 5\n";
print "Has cycle: " . (has_cycle($node1) ? "Yes" : "No") . "\n\n";

# Create a cycle: 1 -> 2 -> 3 -> 4 -> 5 -> 2 (back to node 2)
print "2. Testing with cycle:\n";
my $node1_with_cycle = ListNode->new(1);
my $node2_with_cycle = ListNode->new(2);
my $node3_with_cycle = ListNode->new(3);
my $node4_with_cycle = ListNode->new(4);
my $node5_with_cycle = ListNode->new(5);

$node1_with_cycle->set_next($node2_with_cycle);
$node2_with_cycle->set_next($node3_with_cycle);
$node3_with_cycle->set_next($node4_with_cycle);
$node4_with_cycle->set_next($node5_with_cycle);
$node5_with_cycle->set_next($node2_with_cycle);  # Creates cycle

print "List with cycle: 1 -> 2 -> 3 -> 4 -> 5 -> 2\n";
print "Has cycle: " . (has_cycle($node1_with_cycle) ? "Yes" : "No") . "\n";

# Find cycle start
my $cycle_start = detect_cycle_start($node1_with_cycle);
if ($cycle_start) {
    print "Cycle starts at node with value: " . $cycle_start->get_val() . "\n";
}

print "\n3. Testing with single node (no cycle):\n";
my $single_node = ListNode->new(1);
print "Has cycle: " . (has_cycle($single_node) ? "Yes" : "No") . "\n";

print "\n4. Testing with single node (with self-cycle):\n";
$single_node->set_next($single_node);  # Point to itself
print "Has cycle: " . (has_cycle($single_node) ? "Yes" : "No") . "\n";
```

## How it works:

1. **Two Pointers**: Uses two pointers moving at different speeds - a slow pointer (tortoise) that moves one step at a time, and a fast pointer (hare) that moves two steps at a time.

2. **Cycle Detection**: If there's a cycle, the fast pointer will eventually "lap" the slow pointer and they'll meet at some point.

3. **Finding Cycle Start**: Once a cycle is detected, a second phase finds the starting node of the cycle by placing one pointer back at the head and moving both one step at a time until they meet.

## Key Features:

- **Time Complexity**: O(n)
- **Space Complexity**: O(1)
- **Handles edge cases**: Empty lists, single nodes, etc.
- **Returns meaningful results**: Boolean for cycle detection, node reference for cycle start

This algorithm is particularly useful for detecting cycles in linked lists and is a fundamental technique in computer science.

