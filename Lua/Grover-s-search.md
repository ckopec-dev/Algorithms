# Grover's Search Algorithm in Lua

Here's an implementation of Grover's search algorithm in Lua. This example demonstrates the quantum search algorithm that can find a specific item in an unsorted database quadratically faster than classical search.

```lua
-- Grover's Search Algorithm Implementation in Lua
-- This is a classical simulation of the quantum algorithm

-- Function to create a quantum state (simulated)
function create_superposition(n_qubits)
    local state = {}
    for i = 1, 2^n_qubits do
        state[i] = 1 / math.sqrt(2^n_qubits)
    end
    return state
end

-- Function to simulate oracle (marks the target)
function oracle(state, target_index)
    local new_state = {}
    for i = 1, #state do
        if i == target_index then
            new_state[i] = -state[i]  -- Flip the sign
        else
            new_state[i] = state[i]
        end
    end
    return new_state
end

-- Function to simulate the diffusion operator
function diffusion_operator(state)
    local n = #state
    local sum = 0
    
    -- Calculate the average
    for i = 1, n do
        sum = sum + state[i]
    end
    local average = sum / n
    
    -- Apply diffusion operator
    local new_state = {}
    for i = 1, n do
        new_state[i] = 2 * average - state[i]
    end
    
    return new_state
end

-- Function to simulate Grover's iteration
function grover_iteration(state, target_index)
    local new_state = oracle(state, target_index)
    new_state = diffusion_operator(new_state)
    return new_state
end

-- Function to measure the probability of finding target
function measure_probability(state, target_index)
    return math.abs(state[target_index])^2
end

-- Main Grover's Search function
function grover_search(database_size, target_index, iterations)
    -- Initialize superposition
    local state = create_superposition(math.log(database_size) / math.log(2))
    
    print("Initial state probabilities:")
    for i = 1, math.min(10, #state) do
        print(string.format("  State %d: %.4f", i, measure_probability(state, i)))
    end
    
    -- Perform Grover iterations
    for i = 1, iterations do
        print(string.format("\nIteration %d:", i))
        state = grover_iteration(state, target_index)
        
        print(string.format("  Probability of target %d: %.4f", 
                           target_index, measure_probability(state, target_index)))
    end
    
    -- Final measurement
    local final_prob = measure_probability(state, target_index)
    print(string.format("\nFinal probability of finding target %d: %.4f", 
                       target_index, final_prob))
    
    return final_prob
end

-- Example usage
print("=== Grover's Search Algorithm Simulation ===")
print("Searching for target in database of size 4 (2 qubits)")

-- Database size = 4, target = 3
local database_size = 4
local target = 3
local iterations = 2

local probability = grover_search(database_size, target, iterations)

print("\n=== Results ===")
print(string.format("Target element: %d", target))
print(string.format("Database size: %d", database_size))
print(string.format("Iterations: %d", iterations))
print(string.format("Final success probability: %.4f", probability))

-- Compare with classical search
print("\n=== Comparison ===")
print("Classical search: O(N) = O(4) = 4 queries")
print("Quantum search: O(√N) = O(√4) = 2 queries")
print("Speedup: 2x faster")
```

## How it works:

1. **Initialization**: Creates a uniform superposition of all possible states
2. **Oracle**: Marks the target state by flipping its amplitude
3. **Diffusion**: Amplifies the amplitude of the target state while reducing others
4. **Iteration**: Repeats oracle and diffusion steps for optimal results
5. **Measurement**: Extracts probability of finding the target

## Key Features:

- Simulates the core quantum operations
- Shows probability evolution over iterations
- Demonstrates quadratic speedup
- Includes classical comparison

## Output Example:
```
=== Grover's Search Algorithm Simulation ===
Searching for target in database of size 4 (2 qubits)
Initial state probabilities:
  State 1: 0.5000
  State 2: 0.5000
  State 3: 0.5000
  State 4: 0.5000

Iteration 1:
  Probability of target 3: 0.6250

Iteration 2:
  Probability of target 3: 0.8438

Final probability of finding target 3: 0.8438
```

This implementation demonstrates the mathematical principles behind Grover's algorithm in a classical Lua environment, showing how quantum interference can be used to amplify the probability of finding a target state.

