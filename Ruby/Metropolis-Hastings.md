# Metropolis-Hastings Algorithm in Ruby

Here's a complete implementation of the Metropolis-Hastings algorithm in Ruby to sample from a target distribution:

```ruby
class MetropolisHastings
  def initialize(target_distribution, proposal_distribution, initial_value)
    @target = target_distribution
    @proposal = proposal_distribution
    @current = initial_value
    @samples = []
  end

  def sample(n_samples, burn_in = 0)
    # Run the algorithm
    (n_samples + burn_in).times do |i|
      # Generate candidate from proposal distribution
      candidate = @proposal.call(@current)
      
      # Calculate acceptance probability
      acceptance_ratio = @target.call(candidate) / @target.call(@current)
      
      # Accept or reject the candidate
      if rand < acceptance_ratio
        @current = candidate
      end
      
      # Store sample after burn-in period
      if i >= burn_in
        @samples << @current
      end
    end
    
    @samples
  end

  def get_samples
    @samples
  end
end

# Example: Sampling from a target distribution
# Let's sample from a bimodal distribution: f(x) = 0.3 * exp(-0.2*(x-2)^2) + 0.7 * exp(-0.2*(x+2)^2)

# Define target distribution (unnormalized)
target_dist = lambda do |x|
  0.3 * Math.exp(-0.2 * (x - 2)**2) + 0.7 * Math.exp(-0.2 * (x + 2)**2)
end

# Define proposal distribution (normal with std deviation 1)
proposal_dist = lambda do |current|
  current + rand.normal(0, 1)
end

# Initialize the sampler
mh = MetropolisHastings.new(target_dist, proposal_dist, 0.0)

# Generate samples
samples = mh.sample(10000, 1000)  # 10000 samples, 1000 burn-in

# Calculate statistics
mean = samples.sum / samples.length
std_dev = Math.sqrt(samples.map { |x| (x - mean)**2 }.sum / samples.length)

puts "Sample statistics:"
puts "Mean: #{mean.round(4)}"
puts "Standard deviation: #{std_dev.round(4)}"
puts "Number of samples: #{samples.length}"

# Simple visualization (print first 20 samples)
puts "\nFirst 20 samples:"
puts samples.first(20).join(", ")
```

## How it works:

1. **Target Distribution**: The function we want to sample from (unnormalized)
2. **Proposal Distribution**: A distribution used to generate candidate samples
3. **Algorithm Steps**:
   - Start with an initial value
   - Generate a candidate sample from the proposal distribution
   - Calculate acceptance probability using the ratio of target densities
   - Accept or reject the candidate based on this probability
   - Repeat for desired number of samples

## Key Components:

- **`target_distribution`**: Lambda function representing the target density
- **`proposal_distribution`**: Lambda function generating candidates
- **`initial_value`**: Starting point for the Markov chain
- **`burn_in`**: Number of initial samples to discard
- **`acceptance_ratio`**: Ratio of target probabilities for acceptance decision

This implementation demonstrates how to use the Metropolis-Hastings algorithm to sample from complex distributions that might be difficult to sample from directly.

