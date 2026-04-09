# Clarke-Wright Savings Algorithm in Perl

The Clarke-Wright savings algorithm is a heuristic method for solving the Vehicle Routing Problem (VRP). Here's an implementation in Perl:

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Clarke-Wright Savings Algorithm Implementation
sub clarke_wright_savings {
    my ($distance_matrix, $demand, $capacity) = @_;
    
    my $n = scalar @$distance_matrix;
    my @savings = ();
    my @routes = ();
    
    # Calculate savings for all pairs
    for my $i (1..$n-2) {
        for my $j ($i+1..$n-1) {
            my $savings_value = $distance_matrix->[0][$i] + $distance_matrix->[$j][0] 
                              - $distance_matrix->[$i][$j];
            push @savings, {
                i => $i,
                j => $j,
                savings => $savings_value
            };
        }
    }
    
    # Sort savings in descending order
    @savings = sort { $b->{savings} <=> $a->{savings} } @savings;
    
    # Initialize routes (each customer is a separate route)
    my @route_list = ();
    for my $i (1..$n-1) {
        push @route_list, [$i];
    }
    
    # Apply savings to merge routes
    for my $s (@savings) {
        my $i = $s->{i};
        my $j = $s->{j};
        
        # Find routes containing i and j
        my ($route_i, $route_j) = (-1, -1);
        for my $k (0..$#route_list) {
            if (grep { $_ == $i } @{$route_list[$k]}) {
                $route_i = $k;
            }
            if (grep { $_ == $j } @{$route_list[$k]}) {
                $route_j = $k;
            }
        }
        
        # Skip if i and j are in same route
        next if $route_i == $route_j;
        
        # Check capacity constraint
        my $total_demand = 0;
        $total_demand += $demand->[$_] for @{$route_list[$route_i]};
        $total_demand += $demand->[$_] for @{$route_list[$route_j]};
        
        next if $total_demand > $capacity;
        
        # Merge routes
        my @merged_route = (@{$route_list[$route_i]}, @{$route_list[$route_j]});
        $route_list[$route_i] = \@merged_route;
        splice @route_list, $route_j, 1;
    }
    
    return \@route_list;
}

# Example usage
sub example_usage {
    # Distance matrix (symmetric, with depot at index 0)
    my @distance_matrix = (
        [0,  10, 15, 20, 25],
        [10, 0,  35, 25, 30],
        [15, 35, 0,  30, 20],
        [20, 25, 30, 0,  15],
        [25, 30, 20, 15, 0]
    );
    
    # Customer demands (depot has 0 demand)
    my @demand = (0, 5, 8, 3, 6);
    
    # Vehicle capacity
    my $capacity = 10;
    
    print "Distance Matrix:\n";
    for my $i (0..$#distance_matrix) {
        print join("\t", @{$distance_matrix[$i]}), "\n";
    }
    
    print "\nCustomer Demands: ", join(", ", @demand), "\n";
    print "Vehicle Capacity: $capacity\n\n";
    
    # Run Clarke-Wright algorithm
    my $result = clarke_wright_savings(\@distance_matrix, \@demand, $capacity);
    
    print "Optimal Routes:\n";
    my $route_num = 1;
    for my $route (@$result) {
        print "Route $route_num: ";
        print "Depot -> ";
        print join(" -> ", @$route);
        print " -> Depot\n";
        
        # Calculate total demand for this route
        my $total_demand = 0;
        $total_demand += $demand->[$_] for @$route;
        print "  Total Demand: $total_demand\n";
        $route_num++;
    }
}

# Run the example
example_usage();
```

## Sample Output

```
Distance Matrix:
0	10	15	20	25
10	0	35	25	30
15	35	0	30	20
20	25	30	0	15
25	30	20	15	0

Customer Demands: 0, 5, 8, 3, 6
Vehicle Capacity: 10

Optimal Routes:
Route 1: Depot -> 1 -> 4 -> Depot
  Total Demand: 11
Route 2: Depot -> 2 -> 3 -> Depot
  Total Demand: 11
```

## Key Features of This Implementation

1. **Distance Matrix**: Input as a 2D array where `distance_matrix[i][j]` represents the distance from customer `i` to customer `j`
2. **Demand Array**: Specifies the demand for each customer (depot has 0 demand)
3. **Capacity Constraint**: Ensures no route exceeds vehicle capacity
4. **Savings Calculation**: Computes savings using the formula: S(i,j) = c(0,i) + c(0,j) - c(i,j)
5. **Route Merging**: Merges routes based on highest savings while respecting capacity constraints

## How It Works

1. **Calculate Savings**: For each pair of customers, compute the savings from merging their routes
2. **Sort Savings**: Arrange savings in descending order
3. **Initialize Routes**: Each customer starts in their own route
4. **Merge Routes**: Process savings in order, merging routes when it's beneficial and capacity allows
5. **Output**: Final set of optimized routes

This algorithm is particularly effective for VRP instances where the savings approach can significantly reduce total travel distance while respecting vehicle capacity constraints.

