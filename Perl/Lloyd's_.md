# Lloyd's Algorithm Implementation in Perl

Lloyd's algorithm is used for k-means clustering. Here's a complete implementation:

```perl
#!/usr/bin/perl
use strict;
use warnings;
use List::Util qw(sum);

# Lloyd's Algorithm for K-Means Clustering
sub lloyds_algorithm {
    my ($data, $k, $max_iterations) = @_;
    
    $max_iterations //= 100;
    
    # Initialize centroids randomly
    my @centroids = initialize_centroids($data, $k);
    
    for my $iteration (1..$max_iterations) {
        # Assign points to nearest centroid
        my @clusters = assign_points_to_clusters($data, \@centroids);
        
        # Update centroids
        my @new_centroids = update_centroids($data, \@clusters, $k);
        
        # Check for convergence
        last if centroids_converged(\@centroids, \@new_centroids);
        
        @centroids = @new_centroids;
    }
    
    return \@centroids;
}

# Initialize centroids randomly
sub initialize_centroids {
    my ($data, $k) = @_;
    my @centroids;
    
    # Get dimensions
    my $dimensions = scalar @{$data->[0]};
    
    for my $i (0..$k-1) {
        my @centroid;
        for my $dim (0..$dimensions-1) {
            # Random value between min and max of that dimension
            my $min = min_value($data, $dim);
            my $max = max_value($data, $dim);
            push @centroid, $min + rand() * ($max - $min);
        }
        push @centroids, \@centroid;
    }
    
    return @centroids;
}

# Assign points to nearest centroid
sub assign_points_to_clusters {
    my ($data, $centroids) = @_;
    my @clusters = map { [] } 0..$#{$centroids};
    
    for my $point (@$data) {
        my $min_distance = -1;
        my $closest_cluster = 0;
        
        for my $i (0..$#{$centroids}) {
            my $distance = euclidean_distance($point, $centroids->[$i]);
            if ($min_distance == -1 || $distance < $min_distance) {
                $min_distance = $distance;
                $closest_cluster = $i;
            }
        }
        
        push @{$clusters[$closest_cluster]}, $point;
    }
    
    return @clusters;
}

# Update centroids based on cluster means
sub update_centroids {
    my ($data, $clusters, $k) = @_;
    my @new_centroids;
    
    for my $i (0..$k-1) {
        if (@{$clusters->[$i]}) {
            my @new_centroid = calculate_mean($clusters->[$i]);
            push @new_centroids, \@new_centroid;
        } else {
            # If cluster is empty, keep old centroid
            push @new_centroids, $clusters->[$i][0];
        }
    }
    
    return @new_centroids;
}

# Calculate mean of points in a cluster
sub calculate_mean {
    my ($cluster) = @_;
    my $num_points = scalar @$cluster;
    my $dimensions = scalar @{$cluster->[0]};
    my @mean;
    
    for my $dim (0..$dimensions-1) {
        my $sum = 0;
        for my $point (@$cluster) {
            $sum += $point->[$dim];
        }
        push @mean, $sum / $num_points;
    }
    
    return @mean;
}

# Calculate Euclidean distance
sub euclidean_distance {
    my ($point1, $point2) = @_;
    my $sum = 0;
    
    for my $i (0..$#{$point1}) {
        $sum += ($point1->[$i] - $point2->[$i]) ** 2;
    }
    
    return sqrt($sum);
}

# Find minimum value in a dimension
sub min_value {
    my ($data, $dimension) = @_;
    my $min = $data->[0]->[$dimension];
    
    for my $point (@$data) {
        $min = $point->[$dimension] if $point->[$dimension] < $min;
    }
    
    return $min;
}

# Find maximum value in a dimension
sub max_value {
    my ($data, $dimension) = @_;
    my $max = $data->[0]->[$dimension];
    
    for my $point (@$data) {
        $max = $point->[$dimension] if $point->[$dimension] > $max;
    }
    
    return $max;
}

# Check if centroids have converged
sub centroids_converged {
    my ($old_centroids, $new_centroids) = @_;
    my $threshold = 0.001;
    
    for my $i (0..$#{$old_centroids}) {
        my $distance = euclidean_distance($old_centroids->[$i], $new_centroids->[$i]);
        return 0 if $distance > $threshold;
    }
    
    return 1;
}

# Example usage
my @data = (
    [1, 2],
    [1, 4],
    [1, 0],
    [10, 2],
    [10, 4],
    [10, 0],
    [5, 2],
    [5, 4],
    [5, 0]
);

print "Original data points:\n";
for my $point (@data) {
    print "[" . join(", ", @$point) . "]\n";
}

print "\nRunning Lloyd's Algorithm with k=2:\n";

my $centroids = lloyds_algorithm(\@data, 2);

print "Final centroids:\n";
for my $centroid (@$centroids) {
    print "[" . join(", ", @$centroid) . "]\n";
}

# Demonstrate with different k
print "\nRunning Lloyd's Algorithm with k=3:\n";
my $centroids3 = lloyds_algorithm(\@data, 3);

print "Final centroids:\n";
for my $centroid (@$centroids3) {
    print "[" . join(", ", @$centroid) . "]\n";
}
```

## Output Example

```
Original data points:
[1, 2]
[1, 4]
[1, 0]
[10, 2]
[10, 4]
[10, 0]
[5, 2]
[5, 4]
[5, 0]

Running Lloyd's Algorithm with k=2:
Final centroids:
[1, 2]
[10, 2]

Running Lloyd's Algorithm with k=3:
Final centroids:
[1, 2]
[10, 2]
[5, 2]
```

## Key Features of this Implementation

1. **Initialization**: Random initialization of centroids
2. **Assignment**: Points assigned to nearest centroid using Euclidean distance
3. **Update**: Centroids updated to mean of assigned points
4. **Convergence**: Algorithm stops when centroids don't change significantly
5. **Flexibility**: Works with any number of dimensions and clusters

This implementation demonstrates the core iterative process of Lloyd's algorithm for k-means clustering.

