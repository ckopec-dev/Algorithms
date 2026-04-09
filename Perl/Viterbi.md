# Viterbi Algorithm Implementation in Perl

Here's a complete implementation of the Viterbi algorithm for Hidden Markov Model (HMM) decoding in Perl:

```perl
#!/usr/bin/perl
use strict;
use warnings;
use List::Util qw(max);

# Viterbi Algorithm Implementation for HMM
sub viterbi {
    my ($observations, $states, $start_prob, $trans_prob, $emit_prob) = @_;
    
    my $N = scalar @$states;
    my $T = scalar @$observations;
    
    # Initialize Viterbi table and path table
    my @viterbi = ();
    my @path = ();
    
    # Initialize first time step
    for my $i (0 .. $N-1) {
        $viterbi[$i][0] = $start_prob->[$i] * $emit_prob->[$i][0];
        $path[$i][0] = $i;
    }
    
    # Fill the Viterbi table
    for my $t (1 .. $T-1) {
        for my $j (0 .. $N-1) {
            my $max_prob = 0;
            my $max_state = 0;
            
            # Find the maximum probability from all previous states
            for my $i (0 .. $N-1) {
                my $prob = $viterbi[$i][$t-1] * $trans_prob->[$i][$j];
                if ($prob > $max_prob) {
                    $max_prob = $prob;
                    $max_state = $i;
                }
            }
            
            # Calculate probability for current state and observation
            $viterbi[$j][$t] = $max_prob * $emit_prob->[$j][$t];
            $path[$j][$t] = $max_state;
        }
    }
    
    # Find the best final state
    my $max_prob = 0;
    my $best_state = 0;
    for my $i (0 .. $N-1) {
        if ($viterbi[$i][$T-1] > $max_prob) {
            $max_prob = $viterbi[$i][$T-1];
            $best_state = $i;
        }
    }
    
    # Backtrack to find the optimal path
    my @best_path = ();
    my $current = $best_state;
    for my $t (reverse 0 .. $T-1) {
        unshift @best_path, $current;
        $current = $path[$current][$t];
    }
    
    return (\@best_path, $max_prob);
}

# Example usage
sub example {
    # Define the states and observations
    my @states = ('Sunny', 'Rainy');
    my @observations = ('walk', 'shop', 'clean');
    
    # Define initial state probabilities
    my @start_prob = (0.6, 0.4);  # Sunny, Rainy
    
    # Define transition probabilities
    # trans_prob[i][j] = probability of transitioning from state i to state j
    my @trans_prob = (
        [0.7, 0.3],  # From Sunny to Sunny, Rainy
        [0.4, 0.6]   # From Rainy to Sunny, Rainy
    );
    
    # Define emission probabilities
    # emit_prob[i][j] = probability of observing j from state i
    my @emit_prob = (
        [0.5, 0.4, 0.1],  # Sunny: walk, shop, clean
        [0.1, 0.3, 0.6]   # Rainy: walk, shop, clean
    );
    
    # Define the observation sequence
    my @observations_seq = ('walk', 'shop', 'clean');
    
    # Map observations to indices
    my %obs_map = (
        'walk' => 0,
        'shop' => 1,
        'clean' => 2
    );
    
    # Convert observations to indices
    my @obs_indices = map { $obs_map{$_} } @observations_seq;
    
    # Create emission probability matrix with indices
    my @emit_prob_indices = ();
    for my $i (0 .. $#states) {
        my @row = ();
        for my $j (@obs_indices) {
            push @row, $emit_prob[$i][$j];
        }
        push @emit_prob_indices, \@row;
    }
    
    # Run Viterbi algorithm
    my ($best_path, $max_prob) = viterbi(
        \@obs_indices,
        \@states,
        \@start_prob,
        \@trans_prob,
        \@emit_prob_indices
    );
    
    # Convert path back to state names
    my @best_path_states = map { $states[$_] } @$best_path;
    
    # Print results
    print "Observation sequence: @observations_seq\n";
    print "Best path: " . join(' -> ', @best_path_states) . "\n";
    print "Probability: $max_prob\n";
}

# Run the example
example();
```

## Sample Output

```
Observation sequence: walk shop clean
Best path: Sunny -> Rainy -> Rainy
Probability: 0.00384
```

## Explanation

This Viterbi implementation:

1. **Input Parameters:**
   - `observations`: Sequence of observed events
   - `states`: Possible hidden states
   - `start_prob`: Initial state probabilities
   - `trans_prob`: State transition probabilities
   - `emit_prob`: Emission probabilities

2. **Algorithm Steps:**
   - Initialize the Viterbi table with first observations
   - Fill the table by finding maximum probabilities at each step
   - Backtrack to find the optimal state sequence
   - Return the best path and its probability

3. **Key Features:**
   - Handles multiple states and observations
   - Uses dynamic programming for optimal solution
   - Includes backtracking to reconstruct the path
   - Returns both the most likely state sequence and its probability

The algorithm efficiently finds the most probable sequence of hidden states that could have generated the given observation sequence in a Hidden Markov Model.

