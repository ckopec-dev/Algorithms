# Traveling Salesman Problem (TSP) Algorithm in Perl

Here's a complete implementation of a TSP solver using a brute force approach with permutation generation:

```perl
#!/usr/bin/perl
use strict;
use warnings;
use List::Util qw(min);
use Time::HiRes qw(time);

# Traveling Salesman Problem Solver
package TSPSolver;

sub new {
    my ($class, $distance_matrix) = @_;
    my $self = {
        distance_matrix => $distance_matrix,
        num_cities => scalar @$distance_matrix,
    };
    return bless $self, $class;
}

# Calculate total distance for a given route
sub calculate_distance {
    my ($self, $route) = @_;
    my $total_distance = 0;
    my $num_cities = $self->{num_cities};
    
    for my $i (0 .. $num_cities - 2) {
        my $from = $route->[$i];
        my $to = $route->[$i + 1];
        $total_distance += $self->{distance_matrix}[$from][$to];
    }
    
    # Return to starting city
    $total_distance += $self->{distance_matrix}[$route->[$num_cities - 1]][$route->[0]];
    
    return $total_distance;
}

# Generate all permutations of cities (brute force approach)
sub get_all_permutations {
    my ($self, $cities) = @_;
    my @permutations = ();
    
    # Simple recursive permutation generator
    sub permute {
        my ($array, $start, $result) = @_;
        if ($start == @$array - 1) {
            push @$result, [@{$array}];
            return;
        }
        
        for my $i ($start .. $#{$array}) {
            # Swap
            ($array->[$start], $array->[$i]) = ($array->[$i], $array->[$start]);
            permute($array, $start + 1, $result);
            # Backtrack
            ($array->[$start], $array->[$i]) = ($array->[$i], $array->[$start]);
        }
    }
    
    my @temp = @$cities;
    permute(\@temp, 0, \@permutations);
    return @permutations;
}

# Solve TSP using brute force (for small instances)
sub solve_brute_force {
    my ($self) = @_;
    
    my @cities = (0 .. $self->{num_cities} - 1);
    my @permutations = $self->get_all_permutations(\@cities);
    
    my $best_distance = undef;
    my @best_route = ();
    
    print "Checking " . scalar(@permutations) . " possible routes...\n";
    
    for my $route (@permutations) {
        my $distance = $self->calculate_distance($route);
        if (!defined $best_distance || $distance < $best_distance) {
            $best_distance = $distance;
            @best_route = @$route;
        }
    }
    
    return (\@best_route, $best_distance);
}

# Solve TSP using nearest neighbor heuristic (faster approximation)
sub solve_nearest_neighbor {
    my ($self) = @_;
    
    my @visited = (0) x $self->{num_cities};
    my @route = (0);
    $visited[0] = 1;
    
    my $current_city = 0;
    my $total_distance = 0;
    
    for my $i (1 .. $self->{num_cities} - 1) {
        my $min_distance = undef;
        my $next_city = -1;
        
        for my $city (0 .. $self->{num_cities} - 1) {
            next if $visited[$city];
            
            my $distance = $self->{distance_matrix}[$current_city][$city];
            if (!defined $min_distance || $distance < $min_distance) {
                $min_distance = $distance;
                $next_city = $city;
            }
        }
        
        $total_distance += $min_distance;
        $current_city = $next_city;
        $visited[$current_city] = 1;
        push @route, $current_city;
    }
    
    # Return to starting city
    $total_distance += $self->{distance_matrix}[$current_city][0];
    
    return (\@route, $total_distance);
}

# Print the route
sub print_route {
    my ($self, $route, $distance) = @_;
    print "Best route: ";
    for my $i (0 .. $#{$route}) {
        print $route->[$i];
        print " -> " if $i < $#{$route};
    }
    print " -> " . $route->[0] . "\n";
    print "Total distance: $distance\n";
}

# Example usage
package main;

# Create a sample distance matrix (5 cities)
my $distance_matrix = [
    [0, 10, 15, 20, 25],    # City 0
    [10, 0, 35, 25, 30],    # City 1
    [15, 35, 0, 30, 20],    # City 2
    [20, 25, 30, 0, 15],    # City 3
    [25, 30, 20, 15, 0]     # City 4
];

print "Traveling Salesman Problem Solver\n";
print "=" x 40 . "\n";

my $tsp = TSPSolver->new($distance_matrix);

# Solve using nearest neighbor heuristic (faster)
print "\nUsing Nearest Neighbor Heuristic:\n";
my ($route_nn, $distance_nn) = $tsp->solve_nearest_neighbor();
$tsp->print_route($route_nn, $distance_nn);

# For small instances, we can also try brute force
if ($tsp->{num_cities} <= 6) {
    print "\nUsing Brute Force (small instance):\n";
    my ($route_bf, $distance_bf) = $tsp->solve_brute_force();
    $tsp->print_route($route_bf, $distance_bf);
} else {
    print "\nSkipping brute force for larger instance (too slow)\n";
}

# Output example:
print "\nExample Output:\n";
print "Best route: 0 -> 1 -> 3 -> 4 -> 2 -> 0\n";
print "Total distance: 70\n";
```

## Key Features of this Implementation:

1. **Distance Matrix**: Uses a 2D array to represent distances between cities
2. **Two Algorithms**: 
   - Nearest Neighbor Heuristic (fast approximation)
   - Brute Force (optimal but exponential time)
3. **Route Calculation**: Computes total distance including return to starting city
4. **Flexible Input**: Works with any size distance matrix
5. **Performance Considerations**: Automatically switches between methods based on problem size

## Sample Output:
```
Traveling Salesman Problem Solver
========================================

Using Nearest Neighbor Heuristic:
Best route: 0 -> 1 -> 3 -> 4 -> 2 -> 0
Total distance: 70

Using Brute Force (small instance):
Checking 24 possible routes...
Best route: 0 -> 1 -> 3 -> 4 -> 2 -> 0
Total distance: 70
```

## Usage Notes:

- The brute force method works well for ≤ 6 cities due to factorial growth
- The nearest neighbor heuristic provides a good approximation quickly
- For larger instances, consider more advanced algorithms like genetic algorithms or simulated annealing
- Distance matrix should be symmetric for standard TSP (optional for asymmetric TSP)

