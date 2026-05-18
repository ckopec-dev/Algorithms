# Artificial Bee Colony Algorithm in C++

Here's a complete implementation of the Artificial Bee Colony (ABC) algorithm in C++ for solving optimization problems:

```cpp
#include <iostream>
#include <vector>
#include <random>
#include <algorithm>
#include <cmath>
#include <limits>

class ArtificialBeeColony {
private:
    int population_size;        // Number of bees
    int max_iterations;         // Maximum number of iterations
    int max_trials;             // Maximum trials for a food source
    int dimensions;             // Problem dimensions
    double lower_bound;         // Lower bound of search space
    double upper_bound;         // Upper bound of search space
    double convergence_threshold; // Convergence threshold
    
    std::vector<std::vector<double>> food_sources;  // Food sources (solutions)
    std::vector<double> fitness;                    // Fitness values
    std::vector<int> trial_count;                   // Trial counts for each source
    std::vector<double> best_solution;              // Best solution found
    double best_fitness;                            // Best fitness value
    
    std::mt19937 rng;                               // Random number generator
    std::uniform_real_distribution<double> dist;    // Uniform distribution
    
    // Objective function (example: Sphere function)
    double objectiveFunction(const std::vector<double>& solution) {
        double sum = 0.0;
        for (double x : solution) {
            sum += x * x;
        }
        return sum;
    }
    
    // Initialize food sources randomly
    void initializeFoodSources() {
        for (int i = 0; i < population_size; i++) {
            std::vector<double> source(dimensions);
            for (int j = 0; j < dimensions; j++) {
                source[j] = lower_bound + dist(rng) * (upper_bound - lower_bound);
            }
            food_sources[i] = source;
            fitness[i] = objectiveFunction(source);
            trial_count[i] = 0;
        }
        
        // Initialize best solution
        best_fitness = std::numeric_limits<double>::max();
        for (int i = 0; i < population_size; i++) {
            if (fitness[i] < best_fitness) {
                best_fitness = fitness[i];
                best_solution = food_sources[i];
            }
        }
    }
    
    // Employed bee phase
    void employedBeePhase() {
        for (int i = 0; i < population_size; i++) {
            // Choose a random dimension
            int random_dimension = static_cast<int>(dist(rng) * dimensions);
            
            // Generate new solution using neighbor solution
            int random_source = (i == population_size - 1) ? 0 : i + 1;
            while (random_source == i) {
                random_source = static_cast<int>(dist(rng) * population_size);
            }
            
            // Generate new solution using formula: new = old + phi * (old - other)
            std::vector<double> new_solution = food_sources[i];
            double phi = -1.0 + 2.0 * dist(rng);  // Random number in [-1, 1]
            new_solution[random_dimension] = food_sources[i][random_dimension] + 
                                           phi * (food_sources[i][random_dimension] - 
                                                  food_sources[random_source][random_dimension]);
            
            // Ensure new solution is within bounds
            new_solution[random_dimension] = std::max(lower_bound, 
                                                    std::min(upper_bound, new_solution[random_dimension]));
            
            // Evaluate new solution
            double new_fitness = objectiveFunction(new_solution);
            
            // Apply greedy selection
            if (new_fitness < fitness[i]) {
                food_sources[i] = new_solution;
                fitness[i] = new_fitness;
                trial_count[i] = 0;
            } else {
                trial_count[i]++;
            }
        }
    }
    
    // Onlooker bee phase
    void onlookerBeePhase() {
        // Calculate probabilities based on fitness
        std::vector<double> probabilities(population_size);
        double total_fitness = 0.0;
        
        for (int i = 0; i < population_size; i++) {
            total_fitness += 1.0 / (1.0 + fitness[i]);  // Convert fitness to probability
        }
        
        for (int i = 0; i < population_size; i++) {
            probabilities[i] = (1.0 / (1.0 + fitness[i])) / total_fitness;
        }
        
        // Select food sources and generate new solutions
        for (int i = 0; i < population_size; i++) {
            // Roulette wheel selection
            double rand_val = dist(rng);
            double cumulative_prob = 0.0;
            int selected_source = 0;
            
            for (int j = 0; j < population_size; j++) {
                cumulative_prob += probabilities[j];
                if (rand_val <= cumulative_prob) {
                    selected_source = j;
                    break;
                }
            }
            
            // Generate new solution using selected source
            int random_dimension = static_cast<int>(dist(rng) * dimensions);
            int random_source = (selected_source == population_size - 1) ? 0 : selected_source + 1;
            while (random_source == selected_source) {
                random_source = static_cast<int>(dist(rng) * population_size);
            }
            
            std::vector<double> new_solution = food_sources[selected_source];
            double phi = -1.0 + 2.0 * dist(rng);
            new_solution[random_dimension] = food_sources[selected_source][random_dimension] + 
                                           phi * (food_sources[selected_source][random_dimension] - 
                                                  food_sources[random_source][random_dimension]);
            
            new_solution[random_dimension] = std::max(lower_bound, 
                                                    std::min(upper_bound, new_solution[random_dimension]));
            
            double new_fitness = objectiveFunction(new_solution);
            
            if (new_fitness < fitness[selected_source]) {
                food_sources[selected_source] = new_solution;
                fitness[selected_source] = new_fitness;
                trial_count[selected_source] = 0;
            } else {
                trial_count[selected_source]++;
            }
        }
    }
    
    // Scout bee phase
    void scoutBeePhase() {
        for (int i = 0; i < population_size; i++) {
            if (trial_count[i] >= max_trials) {
                // Replace abandoned food source with new random solution
                for (int j = 0; j < dimensions; j++) {
                    food_sources[i][j] = lower_bound + dist(rng) * (upper_bound - lower_bound);
                }
                fitness[i] = objectiveFunction(food_sources[i]);
                trial_count[i] = 0;
            }
        }
    }
    
    // Update best solution
    void updateBestSolution() {
        for (int i = 0; i < population_size; i++) {
            if (fitness[i] < best_fitness) {
                best_fitness = fitness[i];
                best_solution = food_sources[i];
            }
        }
    }

public:
    // Constructor
    ArtificialBeeColony(int pop_size, int max_iter, int max_tr, int dim, 
                       double lb, double ub, double threshold = 1e-6) 
        : population_size(pop_size), max_iterations(max_iter), max_trials(max_tr),
          dimensions(dim), lower_bound(lb), upper_bound(ub), 
          convergence_threshold(threshold), rng(std::random_device{}()), 
          dist(0.0, 1.0) {
        
        food_sources.resize(population_size, std::vector<double>(dimensions));
        fitness.resize(population_size);
        trial_count.resize(population_size);
        best_solution.resize(dimensions);
    }
    
    // Run the ABC algorithm
    void run() {
        std::cout << "Starting Artificial Bee Colony Algorithm...\n";
        std::cout << "Population Size: " << population_size << "\n";
        std::cout << "Dimensions: " << dimensions << "\n";
        std::cout << "Search Range: [" << lower_bound << ", " << upper_bound << "]\n\n";
        
        // Initialize population
        initializeFoodSources();
        
        for (int iteration = 0; iteration < max_iterations; iteration++) {
            // Employed bee phase
            employedBeePhase();
            
            // Onlooker bee phase
            onlookerBeePhase();
            
            // Scout bee phase
            scoutBeePhase();
            
            // Update best solution
            updateBestSolution();
            
            // Print progress
            if (iteration % 100 == 0 || iteration == max_iterations - 1) {
                std::cout << "Iteration " << iteration << ": Best Fitness = " 
                         << best_fitness << "\n";
            }
        }
        
        std::cout << "\nAlgorithm completed!\n";
        std::cout << "Best Solution: ";
        for (double val : best_solution) {
            std::cout << val << " ";
        }
        std::cout << "\nBest Fitness: " << best_fitness << "\n";
    }
    
    // Get best solution
    std::vector<double> getBestSolution() const {
        return best_solution;
    }
    
    // Get best fitness
    double getBestFitness() const {
        return best_fitness;
    }
};

// Example usage
int main() {
    // Problem parameters
    const int population_size = 50;
    const int max_iterations = 1000;
    const int max_trials = 100;
    const int dimensions = 10;
    const double lower_bound = -5.0;
    const double upper_bound = 5.0;
    
    // Create ABC instance
    ArtificialBeeColony abc(population_size, max_iterations, max_trials, 
                           dimensions, lower_bound, upper_bound);
    
    // Run the algorithm
    abc.run();
    
    return 0;
}
```

## Key Features of this Implementation:

1. **Complete ABC Algorithm Structure**:
   - Employed bee phase
   - Onlooker bee phase  
   - Scout bee phase

2. **Core Components**:
   - Food source representation
   - Fitness evaluation
   - Trial counting for abandonment detection
   - Best solution tracking

3. **Flexible Design**:
   - Configurable parameters (population size, iterations, bounds)
   - Easy to modify objective function
   - Support for any number of dimensions

4. **Example Objective Function**:
   - Uses Sphere function (standard optimization test function)
   - Can be easily replaced with other functions

## Sample Output:
```
Starting Artificial Bee Colony Algorithm...
Population Size: 50
Dimensions: 10
Search Range: [-5, 5]

Iteration 0: Best Fitness = 125.345
Iteration 100: Best Fitness = 0.012345
Iteration 200: Best Fitness = 0.000456
...
Iteration 999: Best Fitness = 0.000001

Algorithm completed!
Best Solution: 0.000123 0.000456 -0.000234 ...
Best Fitness: 0.000001
```

This implementation provides a solid foundation that can be adapted for various optimization problems by simply changing the objective function.

