# Artificial Bee Colony Algorithm in Perl

Here's an implementation of the Artificial Bee Colony (ABC) algorithm in Perl for solving optimization problems:

```perl
#!/usr/bin/perl
use strict;
use warnings;
use Math::Random::MT::Auto;

package ABCAlgorithm;

sub new {
    my ($class, %args) = @_;
    my $self = {
        population_size => $args{population_size} || 20,
        max_iterations  => $args{max_iterations} || 100,
        limit           => $args{limit} || 10,
        search_space    => $args{search_space} || [-5, 5],
        dimension       => $args{dimension} || 2,
        fitness_func    => $args{fitness_func},
        bee_count       => $args{population_size} || 20,
        best_solution   => undef,
        best_fitness    => undef,
    };
    return bless $self, $class;
}

sub optimize {
    my ($self) = @_;
    
    # Initialize population
    my @population = $self->_initialize_population();
    my $best_global = $self->_get_best_solution(\@population);
    
    # Main optimization loop
    for my $iteration (1..$self->{max_iterations}) {
        # Employed bees phase
        @population = $self->_employed_bees_phase(\@population);
        
        # Onlooker bees phase
        @population = $self->_onlooker_bees_phase(\@population);
        
        # Scout bees phase
        @population = $self->_scout_bees_phase(\@population);
        
        # Update global best
        my $current_best = $self->_get_best_solution(\@population);
        if (!$self->{best_fitness} || $current_best->{fitness} < $self->{best_fitness}) {
            $self->{best_solution} = $current_best->{position};
            $self->{best_fitness} = $current_best->{fitness};
        }
    }
    
    return ($self->{best_solution}, $self->{best_fitness});
}

sub _initialize_population {
    my ($self) = @_;
    my @population;
    
    for my $i (0..$self->{population_size} - 1) {
        my @position;
        for my $d (0..$self->{dimension} - 1) {
            my $min = $self->{search_space}[0];
            my $max = $self->{search_space}[1];
            my $random_pos = $min + (rand() * ($max - $min));
            push @position, $random_pos;
        }
        
        my $fitness = $self->{fitness_func}->(\@position);
        push @population, {
            position => \@position,
            fitness  => $fitness,
            trial    => 0
        };
    }
    
    return @population;
}

sub _employed_bees_phase {
    my ($self, $population) = @_;
    my @new_population = @$population;
    
    for my $i (0..$#{$population}) {
        my $current = $population->[$i];
        my @new_position = @{$current->{position}};
        
        # Select a random dimension
        my $dimension = int(rand($self->{dimension}));
        
        # Select a random bee (not itself)
        my $k = int(rand($self->{population_size}));
        $k = $i if $k == $i;
        
        # Generate new position using formula
        my $r = rand() * 2 - 1;  # Random value between -1 and 1
        $new_position[$dimension] = $current->{position}[$dimension] + 
                                   $r * ($current->{position}[$dimension] - 
                                         $population->[$k]->{position}[$dimension]);
        
        # Boundary check
        my $min = $self->{search_space}[0];
        my $max = $self->{search_space}[1];
        $new_position[$dimension] = $min if $new_position[$dimension] < $min;
        $new_position[$dimension] = $max if $new_position[$dimension] > $max;
        
        # Evaluate new solution
        my $new_fitness = $self->{fitness_func}->(\@new_position);
        
        # Greedy selection
        if ($new_fitness < $current->{fitness}) {
            $new_population[$i] = {
                position => \@new_position,
                fitness  => $new_fitness,
                trial    => 0
            };
        } else {
            $new_population[$i]->{trial}++;
        }
    }
    
    return @new_population;
}

sub _onlooker_bees_phase {
    my ($self, $population) = @_;
    my @new_population = @$population;
    
    # Calculate probabilities for each solution
    my @probabilities;
    my $total_fitness = 0;
    
    for my $bee (@$population) {
        $total_fitness += 1 / (1 + $bee->{fitness});
    }
    
    for my $i (0..$#{$population}) {
        my $prob = (1 / (1 + $population->[$i]->{fitness})) / $total_fitness;
        push @probabilities, $prob;
    }
    
    # Select solutions based on probability
    for my $i (0..$self->{population_size} - 1) {
        my $r = rand();
        my $selected_index = 0;
        my $cumulative = 0;
        
        for my $j (0..$#probabilities) {
            $cumulative += $probabilities[$j];
            if ($r <= $cumulative) {
                $selected_index = $j;
                last;
            }
        }
        
        # Generate new solution (same as employed bees phase)
        my $current = $population->[$selected_index];
        my @new_position = @{$current->{position}};
        my $dimension = int(rand($self->{dimension}));
        my $k = int(rand($self->{population_size}));
        $k = $selected_index if $k == $selected_index;
        
        my $r = rand() * 2 - 1;
        $new_position[$dimension] = $current->{position}[$dimension] + 
                                   $r * ($current->{position}[$dimension] - 
                                         $population->[$k]->{position}[$dimension]);
        
        # Boundary check
        my $min = $self->{search_space}[0];
        my $max = $self->{search_space}[1];
        $new_position[$dimension] = $min if $new_position[$dimension] < $min;
        $new_position[$dimension] = $max if $new_position[$dimension] > $max;
        
        # Evaluate new solution
        my $new_fitness = $self->{fitness_func}->(\@new_position);
        
        # Greedy selection
        if ($new_fitness < $current->{fitness}) {
            $new_population[$selected_index] = {
                position => \@new_position,
                fitness  => $new_fitness,
                trial    => 0
            };
        } else {
            $new_population[$selected_index]->{trial}++;
        }
    }
    
    return @new_population;
}

sub _scout_bees_phase {
    my ($self, $population) = @_;
    my @new_population = @$population;
    
    for my $i (0..$#{$population}) {
        if ($population->[$i]->{trial} >= $self->{limit}) {
            # Scout bee creates new random solution
            my @new_position;
            for my $d (0..$self->{dimension} - 1) {
                my $min = $self->{search_space}[0];
                my $max = $self->{search_space}[1];
                my $random_pos = $min + (rand() * ($max - $min));
                push @new_position, $random_pos;
            }
            
            my $new_fitness = $self->{fitness_func}->(\@new_position);
            $new_population[$i] = {
                position => \@new_position,
                fitness  => $new_fitness,
                trial    => 0
            };
        }
    }
    
    return @new_population;
}

sub _get_best_solution {
    my ($self, $population) = @_;
    my $best = $population->[0];
    
    for my $bee (@$population) {
        if ($bee->{fitness} < $best->{fitness}) {
            $best = $bee;
        }
    }
    
    return $best;
}

# Example usage
package main;

# Define objective function (minimize Sphere function)
my $sphere_function = sub {
    my ($position) = @_;
    my $sum = 0;
    for my $x (@$position) {
        $sum += $x * $x;
    }
    return $sum;
};

# Create ABC instance
my $abc = ABCAlgorithm->new(
    population_size => 20,
    max_iterations  => 100,
    search_space    => [-5, 5],
    dimension       => 2,
    fitness_func    => $sphere_function
);

# Run optimization
my ($best_solution, $best_fitness) = $abc->optimize();

print "Best solution found:\n";
for my $i (0..$#{$best_solution}) {
    printf "x[%d] = %.6f\n", $i, $best_solution->[$i];
}
print "Best fitness: $best_fitness\n";

# Example with Rastrigin function
my $rastrigin_function = sub {
    my ($position) = @_;
    my $sum = 0;
    my $A = 10;
    my $n = scalar(@$position);
    
    for my $x (@$position) {
        $sum += $x * $x - $A * cos(2 * 3.14159 * $x);
    }
    
    return $A * $n + $sum;
};

print "\n--- Optimizing Rastrigin Function ---\n";

my $abc_rastrigin = ABCAlgorithm->new(
    population_size => 30,
    max_iterations  => 150,
    search_space    => [-5, 5],
    dimension       => 2,
    fitness_func    => $rastrigin_function
);

my ($best_solution_rastrigin, $best_fitness_rastrigin) = $abc_rastrigin->optimize();

print "Best solution found:\n";
for my $i (0..$#{$best_solution_rastrigin}) {
    printf "x[%d] = %.6f\n", $i, $best_solution_rastrigin->[$i];
}
print "Best fitness: $best_fitness_rastrigin\n";
```

## Key Features of this Implementation:

1. **Complete ABC Algorithm Structure**: Implements all three phases - employed bees, onlooker bees, and scout bees
2. **Flexible Configuration**: Adjustable population size, iterations, search space, and dimensions
3. **Boundary Handling**: Properly handles search space boundaries
4. **Multiple Objective Functions**: Includes examples for Sphere and Rastrigin functions
5. **Probabilistic Selection**: Uses fitness-based probability for onlooker bees selection
6. **Scout Bee Mechanism**: Automatically replaces poor solutions after exceeding trial limits

## Usage Example:

The script demonstrates optimization of two common benchmark functions:
- **Sphere Function**: Simple quadratic function (easier optimization)
- **Rastrigin Function**: More complex multimodal function (harder optimization)

The algorithm returns the best solution found and its corresponding fitness value, making it suitable for various optimization problems in engineering, economics, and scientific computing applications.

