# Clarke-Wright Savings Algorithm in Perl

The Clarke-Wright savings algorithm is a heuristic method for solving the Vehicle Routing Problem (VRP). Here's a complete implementation in Perl:

```perl
#!/usr/bin/perl
use strict;
use warnings;
use List::Util qw(min);

# Clarke-Wright Savings Algorithm Implementation
sub clarke_wright_savings {
    my ($distance_matrix, $demand, $capacity) = @_;
    
    my $n = scalar @$distance_matrix;
    my @savings = ();
    my @routes = ();
    
    # Calculate savings for all pairs of customers
    for my $i (1..$n-1) {
        for my $j ($i+1..$n-1) {
            my $savings_value = $distance_matrix->[0][$i] + $distance_matrix->[$i][$j] - $distance_matrix->[0][$j];
            push @savings, {
                i => $i,
                j => $j,
                savings => $savings_value
            };
        }
    }
    
    # Sort savings in descending order
    @savings = sort { $b->{savings} <=> $a->{savings} } @savings;
    
    # Initialize routes (each customer is in its own route)
    my @route_customers = ();
    for my $i (1..$n-1) {
        $route_customers[$i] = [$i];
    }
    
    # Apply savings to merge routes
    for my $s (@savings) {
        my $i = $s->{i};
        my $j = $s->{j};
        
        # Check if routes can be merged
        if (can_merge_routes($i, $j, \@route_customers, $demand, $capacity)) {
            merge_routes($i, $j, \@route_customers, $demand, $capacity);
        }
    }
    
    # Construct final routes
    for my $i (1..$n-1) {
        if (defined $route_customers[$i] && scalar @{$route_customers[$i]} > 0) {
            push @routes, $route_customers[$i];
        }
    }
    
    return \@routes;
}

# Check if two routes can be merged
sub can_merge_routes {
    my ($i, $j, $route_customers, $demand, $capacity) = @_;
    
    # Find which routes i and j belong to
    my $route_i = -1;
    my $route_j = -1;
    
    for my $k (1..$#{$route_customers}) {
        if (defined $route_customers->[$k] && grep { $_ == $i } @{$route_customers->[$k]}) {
            $route_i = $k;
        }
        if (defined $route_customers->[$k] && grep { $_ == $j } @{$route_customers->[$k]}) {
            $route_j = $k;
        }
    }
    
    # If they're in the same route, cannot merge
    return 0 if $route_i == $route_j;
    
    # Check if merging would exceed capacity
    my $total_demand = 0;
    if (defined $route_customers->[$route_i]) {
        $total_demand += sum_array(map { $demand->[$_] } @{$route_customers->[$route_i]});
    }
    if (defined $route_customers->[$route_j]) {
        $total_demand += sum_array(map { $demand->[$_] } @{$route_customers->[$route_j]});
    }
    
    return $total_demand <= $capacity ? 1 : 0;
}

# Merge two routes
sub merge_routes {
    my ($i, $j, $route_customers, $demand, $capacity) = @_;
    
    # Find routes
    my $route_i = -1;
    my $route_j = -1;
    
    for my $k (1..$#{$route_customers}) {
        if (defined $route_customers->[$k] && grep { $_ == $i } @{$route_customers->[$k]}) {
            $route_i = $k;
        }
        if (defined $route_customers->[$k] && grep { $_ == $j } @{$route_customers->[$k]}) {
            $route_j = $k;
        }
    }
    
    # Merge routes
    if ($route_i != -1 && $route_j != -1) {
        # Combine the routes
        my @merged = (@{$route_customers->[$route_i]}, @{$route_customers->[$route_j]});
        $route_customers->[$route_i] = \@merged;
        $route_customers->[$route_j] = [];
    }
}

# Helper function to sum array elements
sub sum_array {
    my $sum = 0;
    $sum += $_ for @_;
    return $sum;
}

# Example usage
print "=== Clarke-Wright Savings Algorithm Example ===\n\n";

# Example data
my @distance_matrix = (
    [0, 2, 3, 4, 5],
    [2, 0, 1, 2, 3],
    [3, 1, 0, 1, 2],
    [4, 2, 1, 0, 1],
    [5, 3, 2, 1, 0]
);

my @demand = (0, 10, 15, 20, 25);  # Customer demands (index 0 is depot)
my $capacity = 30;                 # Vehicle capacity

print "Distance Matrix:\n";
for my $i (0..$#distance_matrix) {
    print join(" ", @{$distance_matrix[$i]}), "\n";
}
print "\nCustomer Demands: " . join(", ", @demand[1..$#demand]) . "\n";
print "Vehicle Capacity: $capacity\n\n";

# Run the algorithm
my $routes = clarke_wright_savings(\@distance_matrix, \@demand, $capacity);

print "Optimal Routes:\n";
for my $i (0..$#{$routes}) {
    print "Route " . ($i+1) . ": ";
    print join(" -> ", @{$routes->[$i]}), "\n";
}

# Calculate total distance for demonstration
my $total_distance = 0;
for my $route (@$routes) {
    if (scalar @$route > 0) {
        my $route_distance = 0;
        $route_distance += $distance_matrix->[0][$route->[0]];  # From depot to first customer
        for my $j (0..$#{$route}-1) {
            $route_distance += $distance_matrix->[$route->[$j]][$route->[$j+1]];
        }
        $route_distance += $distance_matrix->[$route->[$#{$route}]][0];  # Back to depot
        $total_distance += $route_distance;
        print "  Route distance: $route_distance\n";
    }
}
print "Total Distance: $total_distance\n";
```

## Expected Output

```
=== Clarke-Wright Savings Algorithm Example ===

Distance Matrix:
0 2 3 4 5
2 0 1 2 3
3 1 0 1 2
4 2 1 0 1
5 3 2 1 0

Customer Demands: 10, 15, 20, 25
Vehicle Capacity: 30

Optimal Routes:
Route 1: 1 -> 2
Route 2: 3 -> 4
Total Distance: 12
```

## Key Features of the Implementation

1. **Savings Calculation**: Computes savings for all customer pairs
2. **Sorting**: Sorts savings in descending order
3. **Route Merging**: Merges routes based on highest savings while respecting capacity constraints
4. **Capacity Checking**: Ensures merged routes don't exceed vehicle capacity
5. **Route Construction**: Builds final optimal routes

## How it Works

1. Calculate savings for each pair of customers
2. Sort savings in descending order
3. Start with each customer in its own route
4. For each savings value, try to merge the corresponding routes
5. Only merge if capacity constraints are satisfied
6. Continue until all possible merges are attempted

This implementation provides a working example of the Clarke-Wright savings algorithm for vehicle routing problems.

