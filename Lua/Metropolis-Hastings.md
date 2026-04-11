# Metropolis-Hastings Algorithm in Lua

Here's a complete implementation of the Metropolis-Hastings algorithm in Lua to sample from a target distribution:

```lua
-- Metropolis-Hastings Algorithm Implementation in Lua
-- Sampling from a target distribution p(x) ∝ exp(-x^2/2)

math.randomseed(os.time())

-- Target distribution (unnormalized)
-- We want to sample from p(x) ∝ exp(-x^2/2) (standard normal)
local function target_distribution(x)
    return math.exp(-0.5 * x * x)
end

-- Proposal distribution (normal with standard deviation 1)
local function proposal_distribution(x, proposed_x)
    local sigma = 1.0
    return math.exp(-0.5 * (proposed_x - x)^2 / (sigma * sigma)) / (sigma * math.sqrt(2 * math.pi))
end

-- Metropolis-Hastings algorithm
local function metropolis_hastings(num_samples, initial_x, proposal_std)
    local samples = {}
    local current_x = initial_x
    local accepted_count = 0
    
    -- Initialize with the first sample
    table.insert(samples, current_x)
    
    for i = 2, num_samples do
        -- Generate proposal from current state
        local proposed_x = current_x + math.random() * proposal_std * 2 - proposal_std
        
        -- Calculate acceptance probability
        local acceptance_ratio = target_distribution(proposed_x) / target_distribution(current_x)
        local acceptance_prob = math.min(1.0, acceptance_ratio)
        
        -- Accept or reject the proposal
        local u = math.random()
        if u < acceptance_prob then
            current_x = proposed_x
            accepted_count = accepted_count + 1
        end
        
        -- Store the current sample
        table.insert(samples, current_x)
    end
    
    return samples, accepted_count / (num_samples - 1)
end

-- Example usage
print("Metropolis-Hastings Sampling Example")
print("===================================")

-- Parameters
local num_samples = 10000
local initial_x = 0.0
local proposal_std = 1.0

-- Run the algorithm
local samples, acceptance_rate = metropolis_hastings(num_samples, initial_x, proposal_std)

-- Display results
print(string.format("Generated %d samples", #samples))
print(string.format("Acceptance rate: %.3f", acceptance_rate))

-- Calculate basic statistics
local sum = 0
local sum_squares = 0
for _, x in ipairs(samples) do
    sum = sum + x
    sum_squares = sum_squares + x * x
end

local mean = sum / #samples
local variance = sum_squares / #samples - mean * mean

print(string.format("Sample mean: %.4f", mean))
print(string.format("Sample variance: %.4f", variance))

-- Show first 10 samples
print("\nFirst 10 samples:")
for i = 1, math.min(10, #samples) do
    print(string.format("Sample %d: %.4f", i, samples[i]))
end

-- Function to calculate histogram-like statistics
local function get_histogram(samples, bins)
    local hist = {}
    local min_val = samples[1]
    local max_val = samples[1]
    
    -- Find range
    for _, x in ipairs(samples) do
        if x < min_val then min_val = x end
        if x > max_val then max_val = x end
    end
    
    -- Initialize histogram
    for i = 1, bins do
        hist[i] = 0
    end
    
    -- Fill histogram
    local range = max_val - min_val
    for _, x in ipairs(samples) do
        local bin = math.floor((x - min_val) / range * bins) + 1
        bin = math.max(1, math.min(bins, bin))
        hist[bin] = hist[bin] + 1
    end
    
    return hist, min_val, max_val
end

-- Show histogram
print("\nHistogram (10 bins):")
local hist, min_val, max_val = get_histogram(samples, 10)
for i, count in ipairs(hist) do
    local bin_min = min_val + (i - 1) * (max_val - min_val) / 10
    local bin_max = min_val + i * (max_val - min_val) / 10
    print(string.format("[%6.2f, %6.2f): %d", bin_min, bin_max, count))
end

-- Function to run multiple chains for comparison
local function run_multiple_chains(num_chains, samples_per_chain)
    local all_samples = {}
    
    for i = 1, num_chains do
        local chain_samples, _ = metropolis_hastings(samples_per_chain, math.random() * 10 - 5, 1.0)
        table.insert(all_samples, chain_samples)
    end
    
    return all_samples
end

-- Run multiple chains
print("\nRunning multiple chains:")
local chains = run_multiple_chains(3, 1000)
for i = 1, #chains do
    local chain_mean = 0
    for _, x in ipairs(chains[i]) do
        chain_mean = chain_mean + x
    end
    chain_mean = chain_mean / #chains[i]
    print(string.format("Chain %d mean: %.4f", i, chain_mean))
end
```

## Key Components Explained:

### 1. **Target Distribution**
```lua
local function target_distribution(x)
    return math.exp(-0.5 * x * x)
end
```
This represents the unnormalized target distribution (standard normal distribution).

### 2. **Proposal Distribution**
```lua
local function proposal_distribution(x, proposed_x)
    local sigma = 1.0
    return math.exp(-0.5 * (proposed_x - x)^2 / (sigma * sigma)) / (sigma * math.sqrt(2 * math.pi))
end
```
Uses a normal distribution as the proposal mechanism.

### 3. **Main Algorithm**
The `metropolis_hastings` function:
- Starts with an initial value
- Generates proposals from the current state
- Calculates acceptance probability using the ratio of target distributions
- Accepts or rejects proposals based on a random draw
- Returns the samples and acceptance rate

### 4. **Usage Example**
The script demonstrates:
- Running the algorithm for 10,000 samples
- Calculating statistics (mean, variance)
- Displaying first 10 samples
- Creating a histogram of results
- Running multiple chains for comparison

This implementation provides a solid foundation that can be easily modified for different target distributions and proposal mechanisms.

