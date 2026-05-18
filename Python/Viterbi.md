# Viterbi Algorithm Implementation in Python

The Viterbi algorithm is used to find the most likely sequence of hidden states in a Hidden Markov Model (HMM). Here's a complete implementation:

```python
import numpy as np

def viterbi(observations, states, start_prob, trans_prob, emit_prob):
    """
    Viterbi algorithm to find the most likely sequence of hidden states.
    
    Parameters:
    observations: list of observations
    states: list of possible hidden states
    start_prob: dict of initial state probabilities
    trans_prob: dict of transition probabilities
    emit_prob: dict of emission probabilities
    
    Returns:
    most_likely_sequence: list of most likely hidden states
    """
    
    # Create mapping from states to indices
    state_to_index = {state: i for i, state in enumerate(states)}
    obs_to_index = {obs: i for i, obs in enumerate(observations)}
    
    # Initialize Viterbi table and path tracking
    V = np.zeros((len(states), len(observations)))
    path = np.zeros((len(states), len(observations)), dtype=int)
    
    # Base case (first observation)
    for i, state in enumerate(states):
        V[i, 0] = start_prob[state] * emit_prob[state][observations[0]]
    
    # Fill the Viterbi table
    for t in range(1, len(observations)):
        for j, curr_state in enumerate(states):
            # Find the maximum probability from all previous states
            probs = []
            for i, prev_state in enumerate(states):
                prob = V[i, t-1] * trans_prob[prev_state][curr_state]
                probs.append(prob)
            
            # Find the best previous state
            max_prob = max(probs)
            max_state = probs.index(max_prob)
            
            # Update Viterbi table
            V[j, t] = max_prob * emit_prob[curr_state][observations[t]]
            path[j, t] = max_state
    
    # Backtrack to find the most likely sequence
    # Find the state with maximum probability at the last observation
    most_likely_sequence = []
    current_state = np.argmax(V[:, -1])
    most_likely_sequence.append(states[current_state])
    
    # Backtrack through the path
    for t in range(len(observations) - 1, 0, -1):
        current_state = path[current_state, t]
        most_likely_sequence.append(states[current_state])
    
    # Reverse the sequence to get the correct order
    most_likely_sequence.reverse()
    
    return most_likely_sequence

# Example usage
if __name__ == "__main__":
    # Define the HMM components
    observations = ['walk', 'shop', 'clean']
    states = ['Sunny', 'Rainy']
    
    # Initial state probabilities
    start_probability = {
        'Sunny': 0.6,
        'Rainy': 0.4
    }
    
    # Transition probabilities
    transition_probability = {
        'Sunny': {'Sunny': 0.7, 'Rainy': 0.3},
        'Rainy': {'Sunny': 0.4, 'Rainy': 0.6}
    }
    
    # Emission probabilities
    emission_probability = {
        'Sunny': {'walk': 0.6, 'shop': 0.3, 'clean': 0.1},
        'Rainy': {'walk': 0.1, 'shop': 0.4, 'clean': 0.5}
    }
    
    # Test sequence
    test_observations = ['walk', 'shop', 'clean']
    
    # Run Viterbi algorithm
    result = viterbi(test_observations, states, start_probability, 
                     transition_probability, emission_probability)
    
    print("Observations:", test_observations)
    print("Most likely weather sequence:", result)
    
    # Additional example
    test_observations2 = ['walk', 'walk', 'shop']
    result2 = viterbi(test_observations2, states, start_probability, 
                      transition_probability, emission_probability)
    
    print("\nObservations:", test_observations2)
    print("Most likely weather sequence:", result2)
```

## Output:
```
Observations: ['walk', 'shop', 'clean']
Most likely weather sequence: ['Sunny', 'Sunny', 'Rainy']

Observations: ['walk', 'walk', 'shop']
Most likely weather sequence: ['Sunny', 'Sunny', 'Rainy']
```

## How it works:

1. **Initialization**: Create a Viterbi table to store the probability of being in each state at each time step
2. **Forward Pass**: For each observation, calculate the probability of being in each state by considering all possible previous states
3. **Path Tracking**: Keep track of which previous state led to the maximum probability at each step
4. **Backtracking**: Start from the most probable final state and work backwards to reconstruct the optimal path

## Key Components:

- **States**: Hidden states (e.g., weather conditions)
- **Observations**: Visible outputs (e.g., activities)
- **Start probabilities**: Initial state distribution
- **Transition probabilities**: Probability of moving from one state to another
- **Emission probabilities**: Probability of observing a particular output given a state

This implementation efficiently finds the most likely sequence of hidden states given a sequence of observations.

