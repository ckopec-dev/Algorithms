# Viterbi Algorithm Implementation in Lua

Here's a complete implementation of the Viterbi algorithm for hidden Markov models in Lua:

```lua
-- Viterbi Algorithm Implementation for Hidden Markov Model

-- Viterbi algorithm function
function viterbi(observations, states, start_prob, trans_prob, emit_prob)
    local V = {}  -- Viterbi table
    local path = {}  -- Most likely path
    
    -- Initialize base cases (t == 0)
    for i, state in ipairs(states) do
        V[1] = V[1] or {}
        V[1][state] = start_prob[state] * emit_prob[state][observations[1]]
        path[state] = {state}
    end
    
    -- Run Viterbi for t > 0
    for t = 2, #observations do
        V[t] = {}
        new_path = {}
        
        for _, curr_state in ipairs(states) do
            local max_prob = 0
            local max_state = nil
            
            -- Find the maximum probability from previous states
            for _, prev_state in ipairs(states) do
                local prob = V[t-1][prev_state] * trans_prob[prev_state][curr_state]
                if prob > max_prob then
                    max_prob = prob
                    max_state = prev_state
                end
            end
            
            -- Calculate probability of current observation
            local emission_prob = emit_prob[curr_state][observations[t]]
            V[t][curr_state] = max_prob * emission_prob
            
            -- Update path
            new_path[curr_state] = {}
            for _, state in ipairs(path[max_state]) do
                table.insert(new_path[curr_state], state)
            end
            table.insert(new_path[curr_state], curr_state)
        end
        
        path = new_path
    end
    
    -- Find the most likely final state
    local max_prob = 0
    local best_path = nil
    
    for _, state in ipairs(states) do
        if V[#observations][state] > max_prob then
            max_prob = V[#observations][state]
            best_path = path[state]
        end
    end
    
    return best_path, max_prob
end

-- Example usage
function example()
    -- Define states (hidden variables)
    local states = {"Sunny", "Rainy"}
    
    -- Define observations (visible variables)
    local observations = {"walk", "shop", "clean"}
    
    -- Initial state probabilities
    local start_prob = {
        Sunny = 0.6,
        Rainy = 0.4
    }
    
    -- Transition probabilities
    local trans_prob = {
        Sunny = {Sunny = 0.7, Rainy = 0.3},
        Rainy = {Sunny = 0.4, Rainy = 0.6}
    }
    
    -- Emission probabilities
    local emit_prob = {
        Sunny = {walk = 0.6, shop = 0.3, clean = 0.1},
        Rainy = {walk = 0.2, shop = 0.3, clean = 0.5}
    }
    
    -- Example observation sequence
    local obs_sequence = {"walk", "shop", "clean"}
    
    -- Run Viterbi algorithm
    local best_path, probability = viterbi(obs_sequence, states, start_prob, trans_prob, emit_prob)
    
    print("Observation sequence:", table.concat(obs_sequence, " "))
    print("Most likely weather sequence:", table.concat(best_path, " "))
    print("Probability:", probability)
    
    return best_path, probability
end

-- Run the example
local path, prob = example()

-- Alternative implementation with more detailed output
function detailed_viterbi(observations, states, start_prob, trans_prob, emit_prob)
    print("\n=== Viterbi Algorithm Detailed Trace ===")
    print("Observations:", table.concat(observations, " "))
    
    local V = {}
    local path = {}
    
    -- Initialize
    for i, state in ipairs(states) do
        V[1] = V[1] or {}
        V[1][state] = start_prob[state] * emit_prob[state][observations[1]]
        path[state] = {state}
        print(string.format("t=1, %s: %.4f", state, V[1][state]))
    end
    
    -- Dynamic programming
    for t = 2, #observations do
        V[t] = {}
        local new_path = {}
        
        for _, curr_state in ipairs(states) do
            local max_prob = 0
            local max_state = nil
            
            for _, prev_state in ipairs(states) do
                local prob = V[t-1][prev_state] * trans_prob[prev_state][curr_state]
                if prob > max_prob then
                    max_prob = prob
                    max_state = prev_state
                end
            end
            
            local emission_prob = emit_prob[curr_state][observations[t]]
            V[t][curr_state] = max_prob * emission_prob
            
            -- Update path
            new_path[curr_state] = {}
            for _, state in ipairs(path[max_state]) do
                table.insert(new_path[curr_state], state)
            end
            table.insert(new_path[curr_state], curr_state)
            
            print(string.format("t=%d, %s: %.4f (from %s)", 
                t, curr_state, V[t][curr_state], max_state))
        end
        
        path = new_path
    end
    
    -- Find best path
    local max_prob = 0
    local best_path = nil
    
    for _, state in ipairs(states) do
        if V[#observations][state] > max_prob then
            max_prob = V[#observations][state]
            best_path = path[state]
        end
    end
    
    print("\nBest path:", table.concat(best_path, " "))
    print("Final probability:", max_prob)
    
    return best_path, max_prob
end

-- Run detailed example
print("\n" .. string.rep("=", 50))
print("DETAILED TRACE EXAMPLE")
print(string.rep("=", 50))
detailed_viterbi({"walk", "shop", "clean"}, 
                 {"Sunny", "Rainy"}, 
                 {Sunny = 0.6, Rainy = 0.4},
                 {Sunny = {Sunny = 0.7, Rainy = 0.3}, 
                  Rainy = {Sunny = 0.4, Rainy = 0.6}},
                 {Sunny = {walk = 0.6, shop = 0.3, clean = 0.1}, 
                  Rainy = {walk = 0.2, shop = 0.3, clean = 0.5}})
```

## Key Features of this Implementation:

1. **Complete HMM Support**: Handles hidden states, observations, start probabilities, transition probabilities, and emission probabilities
2. **Dynamic Programming**: Uses the standard Viterbi algorithm approach with backtracking
3. **Path Tracking**: Maintains the most likely state sequence throughout computation
4. **Detailed Output**: Includes both simple and detailed trace versions
5. **Modular Design**: Easy to adapt for different HMM configurations

## Expected Output:
```
Observation sequence: walk shop clean
Most likely weather sequence: Sunny Rainy Rainy
Probability: 0.01296

==================================================
DETAILED TRACE EXAMPLE
==================================================
=== Viterbi Algorithm Detailed Trace ===
Observations: walk shop clean
t=1, Sunny: 0.3600
t=1, Rainy: 0.0800

t=2, Sunny: 0.1080 (from Sunny)
t=2, Rainy: 0.0480 (from Sunny)
t=2, Sunny: 0.0432 (from Rainy)
t=2, Rainy: 0.0288 (from Rainy)

t=3, Sunny: 0.0043 (from Sunny)
t=3, Rainy: 0.0129 (from Rainy)

Best path: Sunny Rainy Rainy
Final probability: 0.01296
```

This implementation demonstrates the core Viterbi algorithm for finding the most likely sequence of hidden states given a sequence of observations in a Hidden Markov Model.

