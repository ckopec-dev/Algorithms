# K-Means Clustering in Perl

Here's a complete implementation of the K-means clustering algorithm in Perl:

```perl
#!/usr/bin/perl
use strict;
use warnings;
use Math::Complex;

# K-Means Clustering Algorithm in Perl
package KMeans;

sub new {
    my ($class, $k, $max_iterations) = @_;
    my $self = {
        k => $k,
        max_iterations => $max_iterations || 100,
        centroids => [],
        clusters => []
    };
    return bless $self, $class;
}

# Calculate Euclidean distance between two points
sub euclidean_distance {
    my ($point1, $point2) = @_;
    my $sum = 0;
    for my $i (0 .. $#{$point1}) {
        $sum += ($point1->[$i] - $point2->[$i]) ** 2;
    }
    return sqrt($sum);
}

# Initialize centroids randomly
sub initialize_centroids {
    my ($self, $data) = @_;
    my $n_points = scalar @$data;
    my $n_dimensions = scalar @{$data->[0]};
    
    # Simple random initialization
    my @centroids;
    for my $i (0 .. $self->{k} - 1) {
        my $random_index = int(rand($n_points));
        push @centroids, [@{$data->[$random_index]}];
    }
    
    $self->{centroids} = \@centroids;
}

# Assign points to nearest centroid
sub assign_clusters {
    my ($self, $data) = @_;
    my @clusters = map { [] } (0 .. $self->{k} - 1);
    
    for my $point (@$data) {
        my $min_distance = 999999;
        my $closest_cluster = 0;
        
        for my $i (0 .. $self->{k} - 1) {
            my $distance = $self->euclidean_distance($point, $self->{centroids}->[$i]);
            if ($distance < $min_distance) {
                $min_distance = $distance;
                $closest_cluster = $i;
            }
        }
        
        push @{$clusters[$closest_cluster]}, $point;
    }
    
    $self->{clusters} = \@clusters;
}

# Update centroids based on cluster means
sub update_centroids {
    my ($self, $data) = @_;
    my @new_centroids;
    
    for my $i (0 .. $self->{k} - 1) {
        my $cluster = $self->{clusters}->[$i];
        next if scalar @$cluster == 0;
        
        my $n_dimensions = scalar @{$cluster->[0]};
        my @centroid = (0) x $n_dimensions;
        
        for my $point (@$cluster) {
            for my $j (0 .. $n_dimensions - 1) {
                $centroid[$j] += $point->[$j];
            }
        }
        
        for my $j (0 .. $n_dimensions - 1) {
            $centroid[$j] /= scalar @$cluster;
        }
        
        push @new_centroids, \@centroid;
    }
    
    $self->{centroids} = \@new_centroids;
}

# Check if centroids have converged
sub has_converged {
    my ($self, $old_centroids) = @_;
    return 0 if !$old_centroids;
    
    for my $i (0 .. $self->{k} - 1) {
        my $distance = $self->euclidean_distance($self->{centroids}->[$i], $old_centroids->[$i]);
        return 0 if $distance > 0.001;  # Threshold for convergence
    }
    return 1;
}

# Main K-means algorithm
sub cluster {
    my ($self, $data) = @_;
    
    # Initialize centroids
    $self->initialize_centroids($data);
    
    for my $iteration (1 .. $self->{max_iterations}) {
        # Store old centroids for convergence check
        my $old_centroids = [map { [@{$_}] } @{$self->{centroids}}];
        
        # Assign points to clusters
        $self->assign_clusters($data);
        
        # Update centroids
        $self->update_centroids($data);
        
        # Check for convergence
        if ($self->has_converged($old_centroids)) {
            print "Converged after $iteration iterations\n";
            last;
        }
    }
    
    return $self->{clusters};
}

# Print results
sub print_results {
    my ($self, $data) = @_;
    print "K-Means Clustering Results:\n";
    print "=" x 40 . "\n";
    
    for my $i (0 .. $self->{k} - 1) {
        print "Cluster $i (centroid: [" . join(", ", @{$self->{centroids}->[$i]}) . "]):\n";
        for my $point (@{$self->{clusters}->[$i]}) {
            print "  [" . join(", ", @$point) . "]\n";
        }
        print "\n";
    }
}

# Example usage
package main;

# Sample 2D data points
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

# Create K-means object with k=3 clusters
my $kmeans = KMeans->new(3);

# Perform clustering
my $clusters = $kmeans->cluster(\@data);

# Print results
$kmeans->print_results(\@data);

# Additional example with more data points
print "\n" . "=" x 50 . "\n";
print "Example with more data points:\n";

my @more_data = (
    [2, 3],
    [2, 4],
    [3, 3],
    [3, 4],
    [8, 7],
    [8, 8],
    [9, 7],
    [9, 8],
    [1, 1],
    [1, 2],
    [2, 1],
    [2, 2]
);

my $kmeans2 = KMeans->new(2);
my $clusters2 = $kmeans2->cluster(\@more_data);
$kmeans2->print_results(\@more_data);
```

## How to Run

Save the code to a file (e.g., `kmeans.pl`) and run it:

```bash
perl kmeans.pl
```

## Key Features

1. **Object-Oriented Design**: Clean class structure for K-means implementation
2. **Euclidean Distance**: Standard distance calculation between points
3. **Random Initialization**: Random selection of initial centroids
4. **Convergence Detection**: Stops when centroids don't change significantly
5. **Flexible Input**: Works with any number of dimensions
6. **Clear Output**: Shows cluster assignments and centroids

## Sample Output

The algorithm will output cluster assignments showing which data points belong to which cluster, along with the final centroid positions for each cluster.

## Algorithm Steps

1. Initialize k centroids randomly
2. Assign each data point to the nearest centroid
3. Update centroids to be the mean of assigned points
4. Repeat steps 2-3 until convergence or max iterations reached

This implementation provides a solid foundation for K-means clustering that can be extended with additional features like different distance metrics or initialization methods.

