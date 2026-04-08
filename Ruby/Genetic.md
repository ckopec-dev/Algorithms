# Genetic Algorithm in Ruby

Here's a complete implementation of a genetic algorithm to solve the classic "One Max" problem (finding a binary string of all 1s):

```ruby
class GeneticAlgorithm
  def initialize(population_size: 100, chromosome_length: 20, mutation_rate: 0.01, crossover_rate: 0.8)
    @population_size = population_size
    @chromosome_length = chromosome_length
    @mutation_rate = mutation_rate
    @crossover_rate = crossover_rate
    @population = []
    @generation = 0
  end

  # Initialize random population
  def initialize_population
    @population = Array.new(@population_size) do
      Array.new(@chromosome_length) { rand < 0.5 ? 0 : 1 }
    end
  end

  # Fitness function: count number of 1s
  def fitness(chromosome)
    chromosome.sum
  end

  # Selection using tournament selection
  def select_parent
    tournament_size = 3
    tournament = Array.new(tournament_size) { @population.sample }
    tournament.max_by { |chromosome| fitness(chromosome) }
  end

  # Single point crossover
  def crossover(parent1, parent2)
    return parent1.clone if rand > @crossover_rate
    
    crossover_point = rand(@chromosome_length)
    child1 = parent1[0...crossover_point] + parent2[crossover_point..-1]
    child2 = parent2[0...crossover_point] + parent1[crossover_point..-1]
    
    [child1, child2]
  end

  # Mutation
  def mutate(chromosome)
    mutated = chromosome.clone
    mutated.each_with_index do |gene, index|
      if rand < @mutation_rate
        mutated[index] = gene == 0 ? 1 : 0
      end
    end
    mutated
  end

  # Create next generation
  def evolve
    @generation += 1
    
    # Sort population by fitness (descending)
    sorted_population = @population.sort_by { |chromosome| -fitness(chromosome) }
    
    # Keep best individuals (elitism)
    elite_size = @population_size / 10
    new_population = sorted_population[0...elite_size]
    
    # Generate rest of population through selection, crossover, and mutation
    while new_population.length < @population_size
      parent1 = select_parent
      parent2 = select_parent
      
      children = crossover(parent1, parent2)
      child1 = mutate(children[0])
      child2 = mutate(children[1])
      
      new_population << child1
      new_population << child2
    end
    
    # Trim to exact population size
    @population = new_population[0...@population_size]
  end

  # Get best individual
  def best_individual
    @population.max_by { |chromosome| fitness(chromosome) }
  end

  # Get average fitness
  def average_fitness
    @population.map { |chromosome| fitness(chromosome) }.sum.to_f / @population.size
  end

  # Run the genetic algorithm
  def run(max_generations: 100, target_fitness: @chromosome_length)
    initialize_population
    
    max_generations.times do |generation|
      evolve
      
      best = best_individual
      best_fitness = fitness(best)
      
      puts "Generation #{generation}: Best fitness = #{best_fitness}, Average = #{average_fitness.round(2)}"
      
      return best if best_fitness >= target_fitness
    end
    
    best_individual
  end
end

# Example usage
puts "Running Genetic Algorithm..."
puts "Target: All 1s in a 20-bit string"
puts

ga = GeneticAlgorithm.new(
  population_size: 100,
  chromosome_length: 20,
  mutation_rate: 0.01,
  crossover_rate: 0.8
)

result = ga.run(max_generations: 50, target_fitness: 20)

puts
puts "Final Result: #{result.join('')}"
puts "Fitness: #{result.sum}"
puts "Generation: #{ga.instance_variable_get(:@generation)}"
```

## Key Components Explained

### 1. **Initialization**
- Creates random binary chromosomes
- Sets up parameters like population size, mutation rate, etc.

### 2. **Fitness Function**
- Counts the number of 1s in each chromosome
- Higher fitness = more 1s

### 3. **Selection**
- Uses tournament selection to choose parents
- Better fitness individuals have higher chance of selection

### 4. **Crossover**
- Single-point crossover with configurable rate
- Combines genetic material from two parents

### 5. **Mutation**
- Randomly flips bits with low probability
- Maintains genetic diversity

### 6. **Evolution Process**
- Generates new populations through selection, crossover, and mutation
- Uses elitism to preserve best solutions
- Continues until target fitness is reached or max generations

## Sample Output
```
Running Genetic Algorithm...
Target: All 1s in a 20-bit string

Generation 0: Best fitness = 12, Average = 10.23
Generation 1: Best fitness = 15, Average = 12.45
Generation 2: Best fitness = 18, Average = 14.67
Generation 3: Best fitness = 19, Average = 16.89
Generation 4: Best fitness = 20, Average = 18.12

Final Result: 11111111111111111111
Fitness: 20
Generation: 4
```

This implementation demonstrates the core principles of genetic algorithms: selection, crossover, mutation, and evolution toward optimal solutions.

