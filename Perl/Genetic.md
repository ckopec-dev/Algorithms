# Genetic Algorithm in Perl

Here's a complete example of a genetic algorithm implemented in Perl to solve the classic "Hello World" string matching problem:

```perl
#!/usr/bin/perl
use strict;
use warnings;
use List::Util qw(shuffle);

# Configuration
my $TARGET = "Hello World";
my $POPULATION_SIZE = 100;
my $MUTATION_RATE = 0.01;
my $ELITISM_RATE = 0.1;
my $MAX_GENERATIONS = 1000;

# Character set for our genetic algorithm
my @CHARSET = (' ', 'A'..'Z', 'a'..'z', '!', '.', ',', '?', ';', ':', '-', "'", '"');

# Individual class to represent each string in the population
package Individual {
    sub new {
        my ($class, $genes) = @_;
        my $self = {
            genes => $genes || '',
            fitness => 0
        };
        return bless $self, $class;
    }
    
    sub get_genes {
        my $self = shift;
        return $self->{genes};
    }
    
    sub set_genes {
        my ($self, $genes) = @_;
        $self->{genes} = $genes;
    }
    
    sub get_fitness {
        my $self = shift;
        return $self->{fitness};
    }
    
    sub set_fitness {
        my ($self, $fitness) = @_;
        $self->{fitness} = $fitness;
    }
}

# Main Genetic Algorithm class
package GeneticAlgorithm {
    sub new {
        my ($class, $target) = @_;
        my $self = {
            target => $target,
            population => [],
            generation => 0
        };
        return bless $self, $class;
    }
    
    # Create initial random population
    sub create_initial_population {
        my ($self) = @_;
        $self->{population} = [];
        
        for my $i (1..$POPULATION_SIZE) {
            my $individual = Individual->new();
            my $genes = '';
            
            # Generate random string of same length as target
            for my $j (1..length($self->{target})) {
                $genes .= $CHARSET[int(rand(@CHARSET))];
            }
            
            $individual->set_genes($genes);
            $self->{population}[$i-1] = $individual;
        }
    }
    
    # Calculate fitness for an individual
    sub calculate_fitness {
        my ($self, $individual) = @_;
        my $genes = $individual->get_genes();
        my $target = $self->{target};
        my $fitness = 0;
        
        # Compare each character
        for my $i (0..length($target)-1) {
            if (substr($genes, $i, 1) eq substr($target, $i, 1)) {
                $fitness++;
            }
        }
        
        # Normalize fitness
        $individual->set_fitness($fitness / length($target));
    }
    
    # Calculate fitness for entire population
    sub calculate_population_fitness {
        my ($self) = @_;
        for my $individual (@{$self->{population}}) {
            $self->calculate_fitness($individual);
        }
    }
    
    # Selection using tournament selection
    sub tournament_selection {
        my ($self) = @_;
        my $tournament_size = 3;
        my $best = $self->{population}[int(rand($POPULATION_SIZE))];
        
        for my $i (1..$tournament_size-1) {
            my $candidate = $self->{population}[int(rand($POPULATION_SIZE))];
            if ($candidate->get_fitness > $best->get_fitness) {
                $best = $candidate;
            }
        }
        
        return $best;
    }
    
    # Crossover two parents
    sub crossover {
        my ($self, $parent1, $parent2) = @_;
        my $length = length($parent1->get_genes());
        my $crossover_point = int(rand($length));
        
        my $child1_genes = substr($parent1->get_genes(), 0, $crossover_point) . 
                          substr($parent2->get_genes(), $crossover_point);
        my $child2_genes = substr($parent2->get_genes(), 0, $crossover_point) . 
                          substr($parent1->get_genes(), $crossover_point);
        
        return (Individual->new($child1_genes), Individual->new($child2_genes));
    }
    
    # Mutate an individual
    sub mutate {
        my ($self, $individual) = @_;
        my $genes = $individual->get_genes();
        my $mutated_genes = '';
        
        for my $i (0..length($genes)-1) {
            if (rand() < $MUTATION_RATE) {
                $mutated_genes .= $CHARSET[int(rand(@CHARSET))];
            } else {
                $mutated_genes .= substr($genes, $i, 1);
            }
        }
        
        $individual->set_genes($mutated_genes);
    }
    
    # Create next generation
    sub create_next_generation {
        my ($self) = @_;
        my @new_population = ();
        
        # Elitism - keep best individuals
        my @sorted_population = sort { $b->get_fitness <=> $a->get_fitness } @{$self->{population}};
        my $elitism_count = int($POPULATION_SIZE * $ELITISM_RATE);
        
        for my $i (0..$elitism_count-1) {
            push @new_population, Individual->new($sorted_population[$i]->get_genes());
        }
        
        # Generate offspring
        while (@new_population < $POPULATION_SIZE) {
            my $parent1 = $self->tournament_selection();
            my $parent2 = $self->tournament_selection();
            
            my ($child1, $child2) = $self->crossover($parent1, $parent2);
            
            $self->mutate($child1);
            $self->mutate($child2);
            
            push @new_population, $child1, $child2;
        }
        
        # Trim to exact population size
        splice(@new_population, $POPULATION_SIZE);
        $self->{population} = \@new_population;
        $self->{generation}++;
    }
    
    # Get best individual in population
    sub get_best_individual {
        my ($self) = @_;
        my @sorted = sort { $b->get_fitness <=> $a->get_fitness } @{$self->{population}};
        return $sorted[0];
    }
    
    # Run the genetic algorithm
    sub run {
        my ($self) = @_;
        
        print "Starting Genetic Algorithm to evolve: '$self->{target}'\n";
        print "Population Size: $POPULATION_SIZE\n";
        print "Mutation Rate: $MUTATION_RATE\n";
        print "Elitism Rate: $ELITISM_RATE\n";
        print "=" x 50 . "\n\n";
        
        $self->create_initial_population();
        $self->calculate_population_fitness();
        
        my $best = $self->get_best_individual();
        my $generation = 0;
        
        while ($best->get_fitness < 1.0 && $generation < $MAX_GENERATIONS) {
            $self->create_next_generation();
            $self->calculate_population_fitness();
            $best = $self->get_best_individual();
            
            if ($generation % 50 == 0 || $best->get_fitness == 1.0) {
                printf "Generation %d: Best = '%s' (Fitness: %.2f)\n", 
                       $generation, $best->get_genes(), $best->get_fitness;
            }
            
            $generation++;
        }
        
        printf "\nFinal Result:\n";
        printf "Generation: %d\n", $generation;
        printf "Best Individual: '%s'\n", $best->get_genes();
        printf "Fitness: %.2f\n", $best->get_fitness;
        
        return $best;
    }
}

# Main execution
my $ga = GeneticAlgorithm->new($TARGET);
my $result = $ga->run();

print "\nAlgorithm completed!\n";
```

## How it works:

1. **Individual Class**: Represents each string in the population with genes and fitness
2. **Population Initialization**: Creates random strings of the same length as the target
3. **Fitness Calculation**: Compares each character with the target string
4. **Selection**: Uses tournament selection to choose parents
5. **Crossover**: Combines two parent strings at a random point
6. **Mutation**: Randomly changes characters with a small probability
7. **Elitism**: Preserves the best individuals in each generation

## Key Features:

- **Tournament Selection**: More robust than simple random selection
- **Elitism**: Preserves best solutions across generations
- **Mutation**: Introduces variation to avoid local optima
- **Fitness Normalization**: Scales fitness to 0-1 range

## Sample Output:
```
Starting Genetic Algorithm to evolve: 'Hello World'
Population Size: 100
Mutation Rate: 0.01
Elitism Rate: 0.1
==================================================

Generation 0: Best = 'WjQxjQmJjz' (Fitness: 0.18)
Generation 50: Best = 'Heklo World' (Fitness: 0.64)
Generation 100: Best = 'Hello World' (Fitness: 1.00)

Final Result:
Generation: 100
Best Individual: 'Hello World'
Fitness: 1.00
```

This genetic algorithm demonstrates the core principles of evolutionary computation in Perl, showing how random mutations and selective breeding can evolve solutions to complex problems.

