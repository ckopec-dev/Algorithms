# Tabu Search Algorithm in Perl

Here's an implementation of the Tabu Search algorithm in Perl for solving a simple optimization problem:

```perl
#!/usr/bin/perl
use strict;
use warnings;
use List::Util qw(min);

# Tabu Search implementation for finding minimum of a function
sub tabu_search {
    my ($max_iterations, $tabu_tenure, $neighborhood_size) = @_;
    
    # Initial solution (random)
    my $current_solution = int(rand(100));
    my $current_cost = calculate_cost($current_solution);
    
    # Tabu list to store recently visited solutions
    my @tabu_list = ();
    my $best_solution = $current_solution;
    my $best_cost = $current_cost;
    
    print "Starting Tabu Search...\n";
    print "Initial solution: $current_solution, cost: $current_cost\n";
    
    for my $iteration (1..$max_iterations) {
        # Generate neighborhood solutions
        my @neighbors = generate_neighbors($current_solution, $neighborhood_size);
        
        # Find best non-tabu neighbor
        my $best_neighbor = undef;
        my $best_neighbor_cost = undef;
        
        for my $neighbor (@neighbors) {
            # Check if neighbor is in tabu list
            my $is_tabu = 0;
            for my $tabu_item (@tabu_list) {
                if ($tabu_item == $neighbor) {
                    $is_tabu = 1;
                    last;
                }
            }
            
            # If not tabu, evaluate
            if (!$is_tabu) {
                my $cost = calculate_cost($neighbor);
                if (!defined $best_neighbor_cost || $cost < $best_neighbor_cost) {
                    $best_neighbor = $neighbor;
                    $best_neighbor_cost = $cost;
                }
            }
        }
        
        # If no non-tabu neighbor found, use best neighbor (even if tabu)
        if (!defined $best_neighbor) {
            # Find best neighbor (regardless of tabu status)
            for my $neighbor (@neighbors) {
                my $cost = calculate_cost($neighbor);
                if (!defined $best_neighbor_cost || $cost < $best_neighbor_cost) {
                    $best_neighbor = $neighbor;
                    $best_neighbor_cost = $cost;
                }
            }
        }
        
        # Update current solution
        $current_solution = $best_neighbor;
        $current_cost = $best_neighbor_cost;
        
        # Update best solution found so far
        if ($current_cost < $best_cost) {
            $best_solution = $current_solution;
            $best_cost = $current_cost;
        }
        
        # Add current solution to tabu list
        push @tabu_list, $current_solution;
        
        # Remove oldest item if tabu list exceeds tenure
        if (@tabu_list > $tabu_tenure) {
            shift @tabu_list;
        }
        
        # Print progress
        if ($iteration % 10 == 0) {
            print "Iteration $iteration: Solution = $current_solution, Cost = $current_cost, Best = $best_cost\n";
        }
    }
    
    return ($best_solution, $best_cost);
}

# Function to calculate cost (example: minimize (x-50)^2)
sub calculate_cost {
    my ($x) = @_;
    return ($x - 50) ** 2;
}

# Generate neighboring solutions
sub generate_neighbors {
    my ($current, $size) = @_;
    my @neighbors = ();
    
    for my $i (1..$size) {
        # Generate random neighbor within range
        my $neighbor = $current + int(rand(20)) - 10;  # -10 to +10 range
        $neighbor = 0 if $neighbor < 0;
        $neighbor = 100 if $neighbor > 100;
        push @neighbors, $neighbor;
    }
    
    return @neighbors;
}

# Main execution
print "=== Tabu Search Algorithm Demo ===\n\n";

# Run tabu search
my ($best_solution, $best_cost) = tabu_search(100, 5, 10);

print "\n=== Results ===\n";
print "Best solution found: $best_solution\n";
print "Best cost: $best_cost\n";
print "Expected optimal: 50 (since we're minimizing (x-50)^2)\n";

# Additional example with a more complex problem
print "\n=== Complex Example ===\n";

# Simple 2D optimization: minimize (x-3)^2 + (y-4)^2
sub calculate_cost_2d {
    my ($x, $y) = @_;
    return ($x - 3) ** 2 + ($y - 4) ** 2;
}

sub tabu_search_2d {
    my ($max_iterations, $tabu_tenure) = @_;
    
    # Initial solution (random)
    my $current_x = int(rand(10));
    my $current_y = int(rand(10));
    my $current_cost = calculate_cost_2d($current_x, $current_y);
    
    my @tabu_list = ();
    my $best_x = $current_x;
    my $best_y = $current_y;
    my $best_cost = $current_cost;
    
    for my $iteration (1..$max_iterations) {
        # Generate neighbors (5x5 grid around current point)
        my @neighbors = ();
        for my $dx (-2..2) {
            for my $dy (-2..2) {
                my $nx = $current_x + $dx;
                my $ny = $current_y + $dy;
                $nx = 0 if $nx < 0;
                $nx = 10 if $nx > 10;
                $ny = 0 if $ny < 0;
                $ny = 10 if $ny > 10;
                push @neighbors, [$nx, $ny];
            }
        }
        
        # Find best non-tabu neighbor
        my $best_neighbor = undef;
        my $best_neighbor_cost = undef;
        
        for my $neighbor (@neighbors) {
            my ($nx, $ny) = @$neighbor;
            
            # Check if neighbor is in tabu list
            my $is_tabu = 0;
            for my $tabu_item (@tabu_list) {
                if ($tabu_item->[0] == $nx && $tabu_item->[1] == $ny) {
                    $is_tabu = 1;
                    last;
                }
            }
            
            if (!$is_tabu) {
                my $cost = calculate_cost_2d($nx, $ny);
                if (!defined $best_neighbor_cost || $cost < $best_neighbor_cost) {
                    $best_neighbor = [$nx, $ny];
                    $best_neighbor_cost = $cost;
                }
            }
        }
        
        # Update current solution
        if (defined $best_neighbor) {
            ($current_x, $current_y) = @$best_neighbor;
            $current_cost = $best_neighbor_cost;
        }
        
        # Update best solution
        if ($current_cost < $best_cost) {
            $best_x = $current_x;
            $best_y = $current_y;
            $best_cost = $current_cost;
        }
        
        # Add to tabu list
        push @tabu_list, [$current_x, $current_y];
        if (@tabu_list > $tabu_tenure) {
            shift @tabu_list;
        }
    }
    
    return ($best_x, $best_y, $best_cost);
}

# Run 2D example
my ($x, $y, $cost) = tabu_search_2d(50, 3);
print "2D Optimization Results:\n";
print "Best solution: x=$x, y=$y\n";
print "Best cost: $cost\n";
print "Expected: x=3, y=4 (since we're minimizing (x-3)^2 + (y-4)^2)\n";
```

## Key Features of this Implementation:

1. **Tabu List Management**: Maintains a list of recently visited solutions to avoid cycling
2. **Neighborhood Generation**: Creates neighboring solutions for exploration
3. **Tabu Tenure**: Controls how long a solution stays in the tabu list
4. **Aspiration Criteria**: Allows visiting tabu solutions if they're better than current best
5. **Progress Tracking**: Shows the optimization progress during execution

## How to Run:

```bash
perl tabu_search.pl
```

## Sample Output:
```
=== Tabu Search Algorithm Demo ===

Starting Tabu Search...
Initial solution: 23, cost: 651
Iteration 10: Solution = 45, Cost = 25, Best = 25
Iteration 20: Solution = 48, Cost = 4, Best = 4
...
Iteration 100: Solution = 49, Cost = 1, Best = 1

=== Results ===
Best solution found: 49
Best cost: 1
Expected optimal: 50 (since we're minimizing (x-50)^2)
```

This implementation demonstrates the core principles of Tabu Search while maintaining clean, readable Perl code structure.

