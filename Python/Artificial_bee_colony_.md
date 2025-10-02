# Artificial Bee Colony Algorithm Example

Here's a complete implementation of the Artificial Bee Colony (ABC) algorithm in Python to solve optimization problems:

```python
import numpy as np
import matplotlib.pyplot as plt
from typing import List, Tuple

class ArtificialBeeColony:
    def __init__(self, objective_function, bounds: List[Tuple[float, float]], 
                 population_size: int = 50, max_iterations: int = 1000,
                 limit: int = 100):
        """
        Initialize the Artificial Bee Colony algorithm
        
        Args:
            objective_function: Function to minimize
            bounds: List of (min, max) tuples for each dimension
            population_size: Number of bees in the colony
            max_iterations: Maximum number of iterations
            limit: Limit for abandoning a food source
        """
        self.objective_function = objective_function
        self.bounds = bounds
        self.population_size = population_size
        self.max_iterations = max_iterations
        self.limit = limit
        self.dimensions = len(bounds)
        
        # Initialize the colony
        self.employed_bees = []
        self.onlooker_bees = []
        self.best_solution = None
        self.best_fitness = float('inf')
        self.fitness_history = []
        
    def initialize_population(self):
        """Initialize the initial population of bees"""
        self.employed_bees = []
        for _ in range(self.population_size):
            # Generate random solution within bounds
            solution = [np.random.uniform(bound[0], bound[1]) 
                       for bound in self.bounds]
            fitness = self.objective_function(solution)
            
            if fitness < self.best_fitness:
                self.best_fitness = fitness
                self.best_solution = solution.copy()
                
            self.employed_bees.append({
                'position': solution,
                'fitness': fitness,
                'trial_counter': 0
            })
    
    def calculate_fitness(self, fitness_value):
        """Calculate fitness value for the bee"""
        if fitness_value >= 0:
            return 1 / (1 + fitness_value)
        else:
            return 1 + abs(fitness_value)
    
    def generate_new_solution(self, current_bee, reference_bee):
        """Generate new solution using neighbor search"""
        # Generate random index for dimension
        dimension = np.random.randint(0, self.dimensions)
        
        # Generate new position using equation: new_position = current + phi * (current - other)
        phi = np.random.uniform(-1, 1)
        new_position = current_bee['position'].copy()
        
        # Apply the formula for ABC algorithm
        new_position[dimension] = current_bee['position'][dimension] + \
                                phi * (current_bee['position'][dimension] - 
                                       reference_bee['position'][dimension])
        
        # Ensure bounds are respected
        new_position[dimension] = np.clip(new_position[dimension], 
                                        self.bounds[dimension][0], 
                                        self.bounds[dimension][1])
        
        return new_position
    
    def employed_bee_phase(self):
        """Employed bee phase - each employed bee searches for better food source"""
        for i in range(self.population_size):
            # Choose a random reference bee (not the current one)
            reference_bee_idx = np.random.randint(0, self.population_size)
            while reference_bee_idx == i:
                reference_bee_idx = np.random.randint(0, self.population_size)
            
            # Generate new solution
            new_position = self.generate_new_solution(self.employed_bees[i], 
                                                    self.employed_bees[reference_bee_idx])
            
            # Evaluate new solution
            new_fitness = self.objective_function(new_position)
            
            # Greedy selection
            if new_fitness < self.employed_bees[i]['fitness']:
                self.employed_bees[i]['position'] = new_position
                self.employed_bees[i]['fitness'] = new_fitness
                self.employed_bees[i]['trial_counter'] = 0
                
                # Update global best
                if new_fitness < self.best_fitness:
                    self.best_fitness = new_fitness
                    self.best_solution = new_position.copy()
            else:
                self.employed_bees[i]['trial_counter'] += 1
    
    def calculate_probabilities(self):
        """Calculate selection probabilities for onlooker bees"""
        fitness_values = [bee['fitness'] for bee in self.employed_bees]
        probabilities = []
        
        for fitness in fitness_values:
            # Convert fitness to probability (lower fitness = higher probability)
            prob = 1 / (fitness + 1e-10)  # Add small value to avoid division by zero
            probabilities.append(prob)
        
        # Normalize probabilities
        total_prob = sum(probabilities)
        if total_prob > 0:
            probabilities = [p/total_prob for p in probabilities]
        else:
            probabilities = [1.0/self.population_size] * self.population_size
            
        return probabilities
    
    def onlooker_bee_phase(self):
        """Onlooker bee phase - onlooker bees select food sources probabilistically"""
        probabilities = self.calculate_probabilities()
        
        for _ in range(self.population_size):
            # Select a food source based on probability
            selected_idx = np.random.choice(range(self.population_size), 
                                          p=probabilities)
            
            # Generate new solution from selected source
            reference_bee_idx = np.random.randint(0, self.population_size)
            while reference_bee_idx == selected_idx:
                reference_bee_idx = np.random.randint(0, self.population_size)
            
            new_position = self.generate_new_solution(self.employed_bees[selected_idx], 
                                                    self.employed_bees[reference_bee_idx])
            
            # Evaluate new solution
            new_fitness = self.objective_function(new_position)
            
            # Greedy selection
            if new_fitness < self.employed_bees[selected_idx]['fitness']:
                self.employed_bees[selected_idx]['position'] = new_position
                self.employed_bees[selected_idx]['fitness'] = new_fitness
                self.employed_bees[selected_idx]['trial_counter'] = 0
                
                # Update global best
                if new_fitness < self.best_fitness:
                    self.best_fitness = new_fitness
                    self.best_solution = new_position.copy()
            else:
                self.employed_bees[selected_idx]['trial_counter'] += 1
    
    def scout_bee_phase(self):
        """Scout bee phase - abandon poor food sources and create new ones"""
        for i in range(self.population_size):
            if self.employed_bees[i]['trial_counter'] >= self.limit:
                # Abandon the food source and create a new one
                new_position = [np.random.uniform(bound[0], bound[1]) 
                               for bound in self.bounds]
                new_fitness = self.objective_function(new_position)
                
                self.employed_bees[i]['position'] = new_position
                self.employed_bees[i]['fitness'] = new_fitness
                self.employed_bees[i]['trial_counter'] = 0
                
                # Update global best
                if new_fitness < self.best_fitness:
                    self.best_fitness = new_fitness
                    self.best_solution = new_position.copy()
    
    def optimize(self):
        """Main optimization loop"""
        # Initialize population
        self.initialize_population()
        
        print(f"Initial best fitness: {self.best_fitness}")
        
        for iteration in range(self.max_iterations):
            # Employed bee phase
            self.employed_bee_phase()
            
            # Onlooker bee phase
            self.onlooker_bee_phase()
            
            # Scout bee phase
            self.scout_bee_phase()
            
            # Store best fitness
            self.fitness_history.append(self.best_fitness)
            
            # Print progress
            if (iteration + 1) % 100 == 0:
                print(f"Iteration {iteration + 1}: Best fitness = {self.best_fitness:.6f}")
        
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

# Example usage with a test function
def sphere_function(x):
    """Sphere function - classic optimization test function"""
    return sum([xi**2 for xi in x])

def rosenbrock_function(x):
    """Rosenbrock function - another common test function"""
    return sum(100*(x[i+1] - x[i]**2)**2 + (1 - x[i])**2 
               for i in range(len(x)-1))

# Example 1: Sphere function optimization
print("=== Optimizing Sphere Function ===")
abc_sphere = ArtificialBeeColony(
    objective_function=sphere_function,
    bounds=[(-5, 5), (-5, 5)],  # 2D problem
    population_size=30,
    max_iterations=500,
    limit=50
)

best_solution_sphere, best_fitness_sphere = abc_sphere.optimize()
print(f"Best solution: {best_solution_sphere}")
print(f"Best fitness: {best_fitness_sphere:.6f}")

# Example 2: Rosenbrock function optimization
print("\n=== Optimizing Rosenbrock Function ===")
abc_rosenbrock = ArtificialBeeColony(
    objective_function=rosenbrock_function,
    bounds=[(-2, 2), (-1, 3)],  # 2D problem
    population_size=40,
    max_iterations=500,
    limit=60
)

best_solution_rosenbrock, best_fitness_rosenbrock = abc_rosenbrock.optimize()
print(f"Best solution: {best_solution_rosenbrock}")
print(f"Best fitness: {best_fitness_rosenbrock:.6f}")

# Plot convergence for the sphere function example
abc_sphere.plot_convergence()

# Example 3: 3D Sphere function
print("\n=== Optimizing 3D Sphere Function ===")
def sphere_function_3d(x):
    return sum([xi**2 for xi in x])

abc_3d = ArtificialBeeColony(
    objective_function=sphere_function_3d,
    bounds=[(-10, 10), (-10, 10), (-10, 10)],  # 3D problem
    population_size=50,
    max_iterations=1000,
    limit=100
)

best_solution_3d, best_fitness_3d = abc_3d.optimize()
print(f"Best solution: {best_solution_3d}")
print(f"Best fitness: {best_fitness_3d:.6f}")
```

## Key Features of This Implementation:

### 1. **Core Components**:
- Employed bees phase
- Onlooker bees phase  
- Scout bees phase
- Fitness calculation and selection

### 2. **Algorithm Phases**:
- **Employed Bee Phase**: Each employed bee searches for better food sources
- **Onlooker Bee Phase**: Onlooker bees probabilistically select good food sources
- **Scout Bee Phase**: Abandon poor solutions and create new ones

### 3. **Parameters**:
- `population_size`: Number of bees (food sources)
- `max_iterations`: Maximum number of algorithm iterations
- `limit`: Maximum trials before abandoning a food source
- `bounds`: Search space boundaries for each dimension

### 4. **Example Functions**:
- Sphere function (simplest optimization test)
- Rosenbrock function (classic optimization challenge)
- Support for multi-dimensional problems

### 5. **Output Features**:
- Convergence plotting
- Progress tracking
- Best solution and fitness reporting

This implementation demonstrates the complete ABC algorithm with proper phase management, convergence monitoring, and support for various optimization problems.

