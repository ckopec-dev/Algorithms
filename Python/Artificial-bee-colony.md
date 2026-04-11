# Artificial Bee Colony Algorithm Implementation

Here's a complete implementation of the Artificial Bee Colony (ABC) algorithm in Python to solve optimization problems:

```python
import numpy as np
import matplotlib.pyplot as plt
from typing import Callable, Tuple

class ArtificialBeeColony:
    def __init__(self, objective_function: Callable, bounds: list, 
                 population_size: int = 50, max_iterations: int = 1000,
                 limit: int = 100):
        """
        Initialize the Artificial Bee Colony algorithm
        
        Args:
            objective_function: Function to minimize
            bounds: List of [min, max] bounds for each dimension
            population_size: Number of bees in the colony
            max_iterations: Maximum number of iterations
            limit: Maximum number of trials before abandoning a food source
        """
        self.objective_function = objective_function
        self.bounds = np.array(bounds)
        self.num_dimensions = len(bounds)
        self.population_size = population_size
        self.max_iterations = max_iterations
        self.limit = limit
        
        # Initialize parameters
        self.food_sources = None
        self.fitness = None
        self.trial = None
        self.best_solution = None
        self.best_fitness = float('inf')
        self.fitness_history = []
        
    def initialize_population(self):
        """Initialize the food sources (solutions) randomly"""
        self.food_sources = np.random.uniform(
            self.bounds[:, 0], 
            self.bounds[:, 1], 
            (self.population_size, self.num_dimensions)
        )
        self.fitness = np.zeros(self.population_size)
        self.trial = np.zeros(self.population_size)
        
        # Evaluate initial population
        for i in range(self.population_size):
            self.fitness[i] = 1.0 / (1.0 + self.objective_function(self.food_sources[i]))
            
        # Find the best solution
        best_idx = np.argmin(self.fitness)
        self.best_solution = self.food_sources[best_idx].copy()
        self.best_fitness = self.fitness[best_idx]
        
    def employed_bee_phase(self):
        """Employed bee phase - bees search for better food sources"""
        for i in range(self.population_size):
            # Choose a random dimension
            dimension = np.random.randint(self.num_dimensions)
            
            # Choose a random neighbor (other food source)
            neighbor = np.random.randint(self.population_size)
            while neighbor == i:
                neighbor = np.random.randint(self.population_size)
            
            # Generate new solution using mutation
            phi = np.random.uniform(-1, 1, self.num_dimensions)
            new_solution = self.food_sources[i] + phi * (self.food_sources[i] - self.food_sources[neighbor])
            
            # Apply bounds
            new_solution = np.clip(new_solution, self.bounds[:, 0], self.bounds[:, 1])
            
            # Evaluate new solution
            new_fitness = 1.0 / (1.0 + self.objective_function(new_solution))
            
            # Greedy selection
            if new_fitness > self.fitness[i]:
                self.food_sources[i] = new_solution
                self.fitness[i] = new_fitness
                self.trial[i] = 0
            else:
                self.trial[i] += 1
                
    def onlooker_bee_phase(self):
        """Onlooker bee phase - bees select food sources based on probability"""
        # Calculate probabilities based on fitness
        probabilities = self.fitness / np.sum(self.fitness)
        
        for i in range(self.population_size):
            # Select a food source based on probability
            selected_idx = np.random.choice(self.population_size, p=probabilities)
            
            # Choose a random dimension
            dimension = np.random.randint(self.num_dimensions)
            
            # Choose a random neighbor
            neighbor = np.random.randint(self.population_size)
            while neighbor == selected_idx:
                neighbor = np.random.randint(self.population_size)
            
            # Generate new solution using mutation
            phi = np.random.uniform(-1, 1, self.num_dimensions)
            new_solution = self.food_sources[selected_idx] + phi * (self.food_sources[selected_idx] - self.food_sources[neighbor])
            
            # Apply bounds
            new_solution = np.clip(new_solution, self.bounds[:, 0], self.bounds[:, 1])
            
            # Evaluate new solution
            new_fitness = 1.0 / (1.0 + self.objective_function(new_solution))
            
            # Greedy selection
            if new_fitness > self.fitness[selected_idx]:
                self.food_sources[selected_idx] = new_solution
                self.fitness[selected_idx] = new_fitness
                self.trial[selected_idx] = 0
            else:
                self.trial[selected_idx] += 1
                
    def scout_bee_phase(self):
        """Scout bee phase - abandon poor food sources and create new ones"""
        for i in range(self.population_size):
            if self.trial[i] >= self.limit:
                # Abandon the food source and create a new one
                self.food_sources[i] = np.random.uniform(
                    self.bounds[:, 0], 
                    self.bounds[:, 1], 
                    self.num_dimensions
                )
                self.fitness[i] = 1.0 / (1.0 + self.objective_function(self.food_sources[i]))
                self.trial[i] = 0
                
                # Update global best
                if self.fitness[i] > self.best_fitness:
                    self.best_solution = self.food_sources[i].copy()
                    self.best_fitness = self.fitness[i]
                    
    def optimize(self):
        """Run the ABC algorithm"""
        self.initialize_population()
        
        for iteration in range(self.max_iterations):
            # Employed bee phase
            self.employed_bee_phase()
            
            # Onlooker bee phase
            self.onlooker_bee_phase()
            
            # Scout bee phase
            self.scout_bee_phase()
            
            # Update best solution
            current_best_idx = np.argmax(self.fitness)
            if self.fitness[current_best_idx] > self.best_fitness:
                self.best_solution = self.food_sources[current_best_idx].copy()
                self.best_fitness = self.fitness[current_best_idx]
                
            # Store fitness history
            self.fitness_history.append(1.0 / (1.0 + self.objective_function(self.best_solution)))
            
            # Print progress
            if iteration % 100 == 0:
                print(f"Iteration {iteration}: Best fitness = {self.fitness_history[-1]:.6f}")
                
        return self.best_solution, 1.0 / (1.0 + self.objective_function(self.best_solution))

# Example usage with different test functions
def sphere_function(x):
    """Sphere function: f(x) = sum(x_i^2) - minimum at x = 0"""
    return np.sum(x**2)

def rosenbrock_function(x):
    """Rosenbrock function: f(x) = sum(100*(x_{i+1} - x_i^2)^2 + (1 - x_i)^2)"""
    return np.sum(100 * (x[1:] - x[:-1]**2)**2 + (1 - x[:-1])**2)

def rastrigin_function(x):
    """Rastrigin function: f(x) = 10*n + sum(x_i^2 - 10*cos(2*pi*x_i))"""
    return 10 * len(x) + np.sum(x**2 - 10 * np.cos(2 * np.pi * x))

# Example 1: Minimize Sphere function
print("=== Minimizing Sphere Function ===")
abc_sphere = ArtificialBeeColony(
    objective_function=sphere_function,
    bounds=[[-5, 5], [-5, 5]],  # 2D problem
    population_size=30,
    max_iterations=500
)

best_solution, best_fitness = abc_sphere.optimize()
print(f"Best solution: {best_solution}")
print(f"Best fitness: {best_fitness:.6f}")

# Example 2: Minimize Rosenbrock function
print("\n=== Minimizing Rosenbrock Function ===")
abc_rosenbrock = ArtificialBeeColony(
    objective_function=rosenbrock_function,
    bounds=[[-2, 2], [-1, 3]],  # 2D problem
    population_size=40,
    max_iterations=1000
)

best_solution, best_fitness = abc_rosenbrock.optimize()
print(f"Best solution: {best_solution}")
print(f"Best fitness: {best_fitness:.6f}")

# Example 3: Minimize Rastrigin function
print("\n=== Minimizing Rastrigin Function ===")
abc_rastrigin = ArtificialBeeColony(
    objective_function=rastrigin_function,
    bounds=[[-5.12, 5.12], [-5.12, 5.12]],  # 2D problem
    population_size=50,
    max_iterations=1000
)

best_solution, best_fitness = abc_rastrigin.optimize()
print(f"Best solution: {best_solution}")
print(f"Best fitness: {best_fitness:.6f}")

# Plot convergence
plt.figure(figsize=(10, 6))
plt.plot(abc_sphere.fitness_history, label='Sphere Function')
plt.plot(abc_rosenbrock.fitness_history, label='Rosenbrock Function')
plt.plot(abc_rastrigin.fitness_history, label='Rastrigin Function')
plt.xlabel('Iteration')
plt.ylabel('Best Fitness')
plt.title('ABC Algorithm Convergence')
plt.legend()
plt.grid(True)
plt.show()
```

## Key Features of this Implementation:

1. **Complete ABC Algorithm**: Implements all three phases - employed bees, onlooker bees, and scout bees
2. **Flexible Objective Functions**: Can optimize any mathematical function
3. **Parameter Control**: Configurable population size, iterations, and abandonment limits
4. **Multiple Test Functions**: Includes sphere, Rosenbrock, and Rastrigin functions
5. **Convergence Tracking**: Records and plots fitness evolution
6. **Proper Bounds Handling**: Ensures solutions stay within specified bounds

## How it Works:

1. **Initialization**: Randomly generate initial food sources (solutions)
2. **Employed Bee Phase**: Each employed bee searches for better solutions in its neighborhood
3. **Onlooker Bee Phase**: Onlooker bees probabilistically select good food sources and explore further
4. **Scout Bee Phase**: Poor solutions are abandoned and replaced with new random solutions
5. **Iteration**: Repeat until maximum iterations are reached

The algorithm is particularly effective for continuous optimization problems and can handle both unimodal and multimodal functions.

