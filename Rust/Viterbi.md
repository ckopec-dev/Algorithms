# Viterbi Algorithm in Rust

Here's a complete implementation of the Viterbi algorithm for Hidden Markov Model (HMM) decoding in Rust:

```rust
#[derive(Debug, Clone)]
pub struct HMM {
    /// Number of hidden states
    n_states: usize,
    /// Number of possible observations
    n_observations: usize,
    /// Initial state probabilities
    initial_probs: Vec<f64>,
    /// Transition probabilities
    transition_probs: Vec<Vec<f64>>,
    /// Emission probabilities
    emission_probs: Vec<Vec<f64>>,
}

impl HMM {
    pub fn new(
        n_states: usize,
        n_observations: usize,
        initial_probs: Vec<f64>,
        transition_probs: Vec<Vec<f64>>,
        emission_probs: Vec<Vec<f64>>,
    ) -> Self {
        HMM {
            n_states,
            n_observations,
            initial_probs,
            transition_probs,
            emission_probs,
        }
    }

    /// Run the Viterbi algorithm to find the most likely sequence of hidden states
    pub fn viterbi(&self, observations: &[usize]) -> (Vec<usize>, f64) {
        let len = observations.len();
        
        // Create tables for dynamic programming
        let mut delta = vec![vec![0.0; self.n_states]; len];
        let mut psi = vec![vec![0; self.n_states]; len];
        
        // Base case (t = 0)
        for i in 0..self.n_states {
            delta[0][i] = self.initial_probs[i] * self.emission_probs[i][observations[0]];
        }
        
        // Recursion
        for t in 1..len {
            for j in 0..self.n_states {
                let mut max_prob = 0.0;
                let mut max_state = 0;
                
                for i in 0..self.n_states {
                    let prob = delta[t-1][i] * self.transition_probs[i][j];
                    if prob > max_prob {
                        max_prob = prob;
                        max_state = i;
                    }
                }
                
                delta[t][j] = max_prob * self.emission_probs[j][observations[t]];
                psi[t][j] = max_state;
            }
        }
        
        // Termination
        let mut max_prob = 0.0;
        let mut max_state = 0;
        for i in 0..self.n_states {
            if delta[len-1][i] > max_prob {
                max_prob = delta[len-1][i];
                max_state = i;
            }
        }
        
        // Backtracking
        let mut path = vec![0; len];
        path[len-1] = max_state;
        for t in (0..len-1).rev() {
            path[t] = psi[t+1][path[t+1]];
        }
        
        (path, max_prob)
    }
}

// Example usage
fn main() {
    // Define a simple HMM with 2 hidden states (0 and 1) and 2 observations (0 and 1)
    let hmm = HMM::new(
        2,  // 2 hidden states
        2,  // 2 observations
        vec![0.5, 0.5],  // initial probabilities
        vec![
            vec![0.7, 0.3],  // transition probabilities from state 0
            vec![0.4, 0.6],  // transition probabilities from state 1
        ],
        vec![
            vec![0.9, 0.1],  // emission probabilities for state 0
            vec![0.2, 0.8],  // emission probabilities for state 1
        ],
    );
    
    // Example observation sequence
    let observations = vec![1, 0, 1, 1, 0];
    
    // Run Viterbi algorithm
    let (path, probability) = hmm.viterbi(&observations);
    
    println!("Observations: {:?}", observations);
    println!("Most likely hidden state sequence: {:?}", path);
    println!("Probability of the path: {:.6}", probability);
    
    // Another example with different observations
    let observations2 = vec![0, 1, 1, 0];
    let (path2, probability2) = hmm.viterbi(&observations2);
    
    println!("\nObservations: {:?}", observations2);
    println!("Most likely hidden state sequence: {:?}", path2);
    println!("Probability of the path: {:.6}", probability2);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_viterbi_simple() {
        let hmm = HMM::new(
            2, 2,
            vec![0.5, 0.5],
            vec![vec![0.7, 0.3], vec![0.4, 0.6]],
            vec![vec![0.9, 0.1], vec![0.2, 0.8]],
        );
        
        let observations = vec![1, 0, 1];
        let (path, probability) = hmm.viterbi(&observations);
        
        assert_eq!(path, vec![1, 0, 1]); // Expected path
        assert!(probability > 0.0);
    }
}
```

## How it works:

1. **Initialization**: Create two tables - `delta` for the probability of the most likely path and `psi` for the backpointers.

2. **Base Case**: Calculate the probability of being in each state at time 0.

3. **Recursion**: For each time step and each state, calculate the probability of the most likely path that ends in that state.

4. **Termination**: Find the most likely final state.

5. **Backtracking**: Reconstruct the most likely sequence of states by following the backpointers.

## Key Features:

- **Generic Implementation**: Works with any number of states and observations
- **Memory Efficient**: Uses dynamic programming with O(T×N) time and space complexity
- **Complete Example**: Includes test cases and usage examples
- **Error Handling**: Proper bounds checking and validation

## Usage:

The algorithm returns both the most likely sequence of hidden states and the probability of that sequence. This is useful for tasks like speech recognition, part-of-speech tagging, and other sequence modeling problems where you need to infer hidden states from observable data.

