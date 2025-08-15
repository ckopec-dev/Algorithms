# Artificial Bee Colony Algorithm Implementation

```python
import numpy as np
import random
import matplotlib.pyplot as plt

class ABCAlgorithm:
    def __init__(self, objective_function, bounds, population_size=50, max_iterations=1000):
        self.objective_function = objective_function
        self.bounds = bounds
        self.population_size = population_size
        self.max_iterations = max_iterations
        self.dimension = len(bounds)
        
        # Initialize parameters
        self.onlooker_bees = population_size // 2
        self employed_bees = population_size // 2
        self.limit = population_size * self.dimension
        
        # Initialize food sources (solutions)
        self.food_sources = np.random.uniform(
            low=[b[0] for b in bounds], 
            high=[b[1] for b in bounds], 
            size=(population_size, self.dimension)
        )
        
        # Initialize fitness values
        self.fitness = np.array([self.calculate_fitness(source) for source in self.food_sources])
        
        # Initialize trial counts
        self.trials = np.zeros(population_size)
        
        # Best solution tracking
        self.best_solution = None
        self.best_fitness = float('inf')
        self.fitness_history = []
    
    def calculate_fitness(self, solution):
        """Calculate fitness value for a solution"""
        fitness = 1.0 / (1.0 + self.objective_function(solution))
        return fitness
    
    def update_best_solution(self):
        """Update the best solution found so far"""
        current_best_idx = np.argmin(self.fitness)
        current_best_fitness = self.fitness[current_best_idx]
        
        if current_best_fitness < self.best_fitness:
            self.best_fitness = current_best_fitness
            self.best_solution = self.food_sources[current_best_idx].copy()
    
    def employed_bee_phase(self):
        """Employed bee phase - each employed bee searches for new solutions"""
        for i in range(self.population_size):
            # Choose a random dimension to modify
            k = random.randint(0, self.dimension - 1)
            
            # Choose a random employed bee (not the current one)
            j = random.randint(0, self.population_size - 1)
            while j == i:
                j = random.randint(0, self.population_size - 1)
            
            # Generate new solution using equation
            phi = random.uniform(-1, 1)
            new_solution = self.food_sources[i].copy()
            new_solution[k] = self.food_sources[i][k] + phi * (self.food_sources[i][k] - self.food_sources[j][k])
            
            # Ensure new solution is within bounds
            for d in range(self.dimension):
                if new_solution[d] < self.bounds[d][0]:
                    new_solution[d] = self.bounds[d][0]
                elif new_solution[d] > self.bounds[d][1]:
                    new_solution[d] = self.bounds[d][1]
            
            # Evaluate new solution
            new_fitness = self.calculate_fitness(new_solution)
            
            # Apply greedy selection
            if new_fitness > self.fitness[i]:
                self.food_sources[i] = new_solution
                self.fitness[i] = new_fitness
                self.trials[i] = 0
            else:
                self.trials[i] += 1
    
    def onlooker_bee_phase(self):
        """Onlooker bee phase - onlooker bees select solutions based on probability"""
        # Calculate probabilities for each food source
        max_fitness = np.max(self.fitness)
        if max_fitness == 0:
            probabilities = np.ones(self.population_size) / self.population_size
        else:
            probabilities = (0.9 * (self.fitness / max_fitness)) + 0.1
        
        # Select and improve solutions
        for _ in range(self.onlooker_bees):
            # Roulette wheel selection
            selected_idx = self.roulette_wheel_selection(probabilities)
            
            # Choose a random dimension to modify
            k = random.randint(0, self.dimension - 1)
            
            # Choose a random employed bee (not the selected one)
            j = random.randint(0, self.population_size - 1)
            while j == selected_idx:
                j = random.randint(0, self.population_size - 1)
            
            # Generate new solution
            phi = random.uniform(-1, 1)
            new_solution = self.food_sources[selected_idx].copy()
            new_solution[k] = self.food_sources[selected_idx][k] + phi * (self.food_sources[selected_idx][k] - self.food_sources[j][k])
            
            # Ensure new solution is within bounds
            for d in range(self.dimension):
                if new_solution[d] < self.bounds[d][0]:
                    new_solution[d] = self.bounds[d][0]
                elif new_solution[d] > self.bounds[d][1]:
                    new_solution[d] = self.bounds[d][1]
            
            # Evaluate new solution
            new_fitness = self.calculate_fitness(new_solution)
            
            # Apply greedy selection
            if new_fitness > self.fitness[selected_idx]:
                self.food_sources[selected_idx] = new_solution
                self.fitness[selected_idx] = new_fitness
                self.trials[selected_idx] = 0
            else:
                self.trials[selected_idx] += 1
    
    def roulette_wheel_selection(self, probabilities):
        """Select an index based on roulette wheel selection"""
        total = np.sum(probabilities)
        r = random.uniform(0, total)
        
        cumulative = 0
        for i, prob in enumerate(probabilities):
            cumulative += prob
            if cumulative >= r:
                return i
        return len(probabilities) - 1
    
    def scout_bee_phase(self):
        """Scout bee phase - replace abandoned food sources"""
        for i in range(self.population_size):
            if self.trials[i] > self.limit:
                # Replace with new random solution
                self.food_sources[i] = np.random.uniform(
                    low=[b[0] for b in self.bounds], 
                    high=[b[1] for b in self.bounds]
                )
                self.fitness[i] = self.calculate_fitness(self.food_sources[i])
                self.trials[i] = 0
    
    def optimize(self):
        """Main optimization loop"""
        for iteration in range(self.max_iterations):
            # Employed bee phase
            self.employed_bee_phase()
            
            # Onlooker bee phase
            self.onlooker_bee_phase()
            
            # Scout bee phase
            self.scout_bee_phase()
            
            # Update best solution
            self.update_best_solution()
            
            # Store fitness history for plotting
            self.fitness_history.append(self.best_fitness)
            
            # Print progress every 100 iterations
            if iteration % 100 == 0:
                print(f"Iteration {iteration}: Best Fitness = {self.best_fitness}")
        
        return self.best_solution, self.best_fitness
    
    def plot_convergence(self):
        """Plot the convergence curve"""
        plt.figure(figsize=(10, 6))
        plt.plot(self.fitness_history)
        plt.title('ABC Algorithm Convergence')
        plt.xlabel('Iteration')
        plt.ylabel('Best Fitness')
        plt.grid(True)
        plt.show()

# Example usage with a simple optimization problem
def sphere_function(x):
    """Sphere function: f(x) = sum(xi^2) - global minimum at x=0"""
    return np.sum(np.square(x))

def rosenbrock_function(x):
    """Rosenbrock function: f(x) = sum(100*(xi+1 - xi^2)^2 + (1-xi)^2)"""
    return np.sum(100 * np.square(x[1:] - x[:-1]**2) + np.square(1 - x[:-1]))

# Test with sphere function
print("Optimizing Sphere Function:")
abc_sphere = ABCAlgorithm(
    objective_function=sphere_function,
    bounds=[(-5, 5), (-5, 5)],  # 2D problem
    population_size=30,
    max_iterations=500
)

best_solution, best_fitness = abc_sphere.optimize()
print(f"Best Solution: {best_solution}")
print(f"Best Fitness: {best_fitness}")

# Test with Rosenbrock function
print("\nOptimizing Rosenbrock Function:")
abc_rosenbrock = ABCAlgorithm(
    objective_function=rosenbrock_function,
    bounds=[(-2, 2), (-2, 2)],  # 2D problem
    population_size=30,
    max_iterations=500
)

best_solution, best_fitness = abc_rosenbrock.optimize()
print(f"Best Solution: {best_solution}")
print(f"Best Fitness: {best_fitness}")

# Plot convergence (uncomment to show plots)
# abc_sphere.plot_convergence()
```

This implementation includes:

1. **Core ABC Algorithm Components**:
   - Employed bee phase
   - Onlooker bee phase  
   - Scout bee phase
   - Fitness calculation and selection

2. **Key Features**:
   - Configurable population size and iterations
   - Boundary constraint handling
   - Roulette wheel selection for onlooker bees
   - Trial count management for scout bees
   - Convergence tracking and plotting

3. **Example Problems**:
   - Sphere function (simple quadratic)
   - Rosenbrock function (classic optimization test)

4. **Usage**:
   - Define your objective function
   - Set bounds for each dimension
   - Initialize ABCAlgorithm with parameters
   - Call optimize() method to find solution

The algorithm follows the standard ABC procedure where employed bees explore neighborhood solutions, onlooker bees select promising solutions based on fitness, and scout bees replace abandoned solutions to maintain diversity.

