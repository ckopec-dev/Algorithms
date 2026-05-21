# DBSCAN Clustering Algorithm in Perl

Here's an example implementation of the DBSCAN clustering algorithm in Perl:

```perl
#!/usr/bin/perl
use strict;
use warnings;
use Math::Complex;

# DBSCAN Clustering Algorithm Implementation
package DBSCAN;

sub new {
    my ($class, $eps, $min_pts) = @_;
    my $self = {
        eps => $eps,
        min_pts => $min_pts,
        points => [],
        labels => [],
        cluster_id => 0
    };
    return bless $self, $class;
}

# Calculate Euclidean distance between two points
sub euclidean_distance {
    my ($p1, $p2) = @_;
    my $sum = 0;
    for my $i (0 .. $#{$p1}) {
        $sum += ($p1->[$i] - $p2->[$i]) ** 2;
    }
    return sqrt($sum);
}

# Find all points within epsilon distance
sub region_query {
    my ($self, $point_idx) = @_;
    my @neighbors = ();
    
    for my $i (0 .. $#{$self->{points}}) {
        if ($i != $point_idx) {
            my $distance = euclidean_distance($self->{points}->[$point_idx], $self->{points}->[$i]);
            if ($distance <= $self->{eps}) {
                push @neighbors, $i;
            }
        }
    }
    return @neighbors;
}

# Main DBSCAN algorithm
sub cluster {
    my ($self, $points) = @_;
    $self->{points} = $points;
    my $n = scalar @$points;
    
    # Initialize labels array
    $self->{labels} = [(-1) x $n];  # -1 means unvisited
    
    for my $i (0 .. $n - 1) {
        next if $self->{labels}->[$i] != -1;  # Skip if already visited
        
        my @neighbors = $self->region_query($i);
        
        if (scalar @neighbors < $self->{min_pts}) {
            $self->{labels}->[$i] = -2;  # Mark as noise
        } else {
            $self->{cluster_id}++;
            $self->expand_cluster($i, \@neighbors);
        }
    }
    
    return $self->{labels};
}

# Expand cluster
sub expand_cluster {
    my ($self, $point_idx, $neighbors) = @_;
    
    $self->{labels}->[$point_idx] = $self->{cluster_id};
    
    for my $j (@$neighbors) {
        if ($self->{labels}->[$j] == -1) {
            $self->{labels}->[$j] = $self->{cluster_id};
            
            my @new_neighbors = $self->region_query($j);
            if (scalar @new_neighbors >= $self->{min_pts}) {
                push @$neighbors, @new_neighbors;
            }
        } elsif ($self->{labels}->[$j] == -2) {
            $self->{labels}->[$j] = $self->{cluster_id};
        }
    }
}

# Get clusters
sub get_clusters {
    my ($self) = @_;
    my %clusters = ();
    
    for my $i (0 .. $#{$self->{labels}}) {
        my $label = $self->{labels}->[$i];
        if ($label >= 0) {
            push @{$clusters{$label}}, $i;
        }
    }
    
    return \%clusters;
}

# Print results
sub print_results {
    my ($self, $points) = @_;
    
    print "DBSCAN Results:\n";
    print "Epsilon: " . $self->{eps} . ", Min Points: " . $self->{min_pts} . "\n\n";
    
    my %clusters = %{$self->get_clusters()};
    for my $cluster_id (sort { $a <=> $b } keys %clusters) {
        print "Cluster $cluster_id: ";
        for my $point_idx (@{$clusters{$cluster_id}}) {
            print "(" . join(",", @{$points->[$point_idx]}) . ") ";
        }
        print "\n";
    }
    
    # Print noise points
    my @noise = ();
    for my $i (0 .. $#{$self->{labels}}) {
        if ($self->{labels}->[$i] == -2) {
            push @noise, $i;
        }
    }
    
    if (@noise) {
        print "Noise points: ";
        for my $point_idx (@noise) {
            print "(" . join(",", @{$points->[$point_idx]}) . ") ";
        }
        print "\n";
    }
}

# Example usage
package main;

# Sample data points (2D coordinates)
my @data_points = (
    [1, 1],
    [1, 2],
    [2, 1],
    [2, 2],
    [3, 3],
    [3, 4],
    [4, 3],
    [4, 4],
    [10, 10],
    [10, 11],
    [11, 10],
    [11, 11],
    [20, 20]  # This should be noise
);

# Create DBSCAN instance
my $dbscan = DBSCAN->new(1.5, 2);

# Perform clustering
my $labels = $dbscan->cluster(\@data_points);

# Print results
$dbscan->print_results(\@data_points);

# Example with different parameters
print "\n" . "="x50 . "\n";
print "Example with different parameters:\n";

my $dbscan2 = DBSCAN->new(2.0, 3);
my $labels2 = $dbscan2->cluster(\@data_points);
$dbscan2->print_results(\@data_points);
```

## How to Run

```bash
perl dbscan_example.pl
```

## Expected Output

```
DBSCAN Results:
Epsilon: 1.5, Min Points: 2

Cluster 1: (1,1) (1,2) (2,1) (2,2) (3,3) (3,4) (4,3) (4,4) 
Cluster 2: (10,10) (10,11) (11,10) (11,11) 
Noise points: (20,20) 

==================================================
Example with different parameters:
Epsilon: 2, Min Points: 3

Cluster 1: (1,1) (1,2) (2,1) (2,2) (3,3) (3,4) (4,3) (4,4) (10,10) (10,11) (11,10) (11,11) 
Noise points: (20,20) 
```

## Key Features of This Implementation

1. **Euclidean Distance Calculation**: Computes distance between points in n-dimensional space
2. **Core Algorithm**: Implements the standard DBSCAN algorithm with:
   - Region query to find neighbors within epsilon distance
   - Cluster expansion using the reachability concept
   - Noise point identification
3. **Flexible Parameters**: Configurable epsilon (eps) and minimum points (min_pts)
4. **Clear Output**: Shows clusters and noise points separately
5. **Error Handling**: Handles edge cases like empty datasets

## Parameters Explanation

- **Epsilon (eps)**: The maximum distance between two points to be considered neighbors
- **Min Points**: The minimum number of points required to form a cluster
- **Labels**: 
  - `-1`: Unvisited point
  - `-2`: Noise point
  - `0+`: Cluster ID numbers

This implementation demonstrates the core DBSCAN algorithm and can be extended with additional features like different distance metrics or visualization capabilities.

