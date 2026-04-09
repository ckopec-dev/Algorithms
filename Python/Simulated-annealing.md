# Simulated Annealing Algorithm Example

Here's a complete implementation of the Simulated Annealing algorithm in Python to solve the Traveling Salesman Problem (TSP):

```python
import random
import math
import matplotlib.pyplot as plt
import numpy as np

class SimulatedAnnealing:
    def __init__(self, cities, initial_temp=1000, cooling_rate=0.95, min_temp=1e-8):
        self.cities = cities
        self.n_cities = len(cities)
        self.initial_temp = initial_temp
        self.cooling_rate = cooling_rate
        self.min_temp = min_temp
        self.best_path = None
        self.best_distance = float('inf')
        self.temperatures = []
        self.best_distances = []
    
    def distance(self, city1, city2):
        """Calculate Euclidean distance between two cities"""
        return math.sqrt((city1[0] - city2[0])**2 + (city1[1] - city2[1])**2)
    
    def total_distance(self, path):
        """Calculate total distance of a path"""
        total = 0
        for i in range(len(path)):
            total += self.distance(self.cities[path[i]], self.cities[path[(i + 1) % len(path)]])
        return total
    
    def random_neighbor(self, path):
        """Generate a random neighbor by swapping two cities"""
        new_path = path.copy()
        i, j = random.sample(range(len(path)), 2)
        new_path[i], new_path[j] = new_path[j], new_path[i]
        return new_path
    
    def acceptance_probability(self, old_cost, new_cost, temperature):
        """Calculate acceptance probability"""
        if new_cost < old_cost:
            return 1.0
        return math.exp((old_cost - new_cost) / temperature)
    
    def solve(self, max_iterations=10000):
        """Run the simulated annealing algorithm"""
        # Initialize with a random path
        current_path = list(range(self.n_cities))
        random.shuffle(current_path)
        current_distance = self.total_distance(current_path)
        
        # Initialize best solution
        best_path = current_path.copy()
        best_distance = current_distance
        
        temperature = self.initial_temp
        
        for iteration in range(max_iterations):
            # Generate neighbor solution
            neighbor_path = self.random_neighbor(current_path)
            neighbor_distance = self.total_distance(neighbor_path)
            
            # Accept or reject the neighbor
            if self.acceptance_probability(current_distance, neighbor_distance, temperature) > random.random():
                current_path = neighbor_path
                current_distance = neighbor_distance
                
                # Update best solution if found
                if current_distance < best_distance:
                    best_path = current_path.copy()
                    best_distance = current_distance
            
            # Cool down the temperature
            temperature *= self.cooling_rate
            
            # Store for plotting
            self.temperatures.append(temperature)
            self.best_distances.append(best_distance)
            
            # Stop if temperature is too low
            if temperature < self.min_temp:
                break
        
        self.best_path = best_path
        self.best_distance = best_distance
        return best_path, best_distance

def plot_results(sa, cities):
    """Plot the results of the algorithm"""
    # Plot the best path
    plt.figure(figsize=(12, 5))
    
    # Plot 1: Best path
    plt.subplot(1, 2, 1)
    path = sa.best_path + [sa.best_path[0]]  # Close the loop
    x_coords = [cities[i][0] for i in path]
    y_coords = [cities[i][1] for i in path]
    
    plt.plot(x_coords, y_coords, 'o-', linewidth=2, markersize=8)
    plt.scatter([city[0] for city in cities], [city[1] for city in cities], 
                c='red', s=100, zorder=5)
    
    # Add city labels
    for i, (x, y) in enumerate(cities):
        plt.annotate(str(i), (x, y), xytext=(5, 5), textcoords='offset points')
    
    plt.title(f'Best Path (Distance: {sa.best_distance:.2f})')
    plt.xlabel('X Coordinate')
    plt.ylabel('Y Coordinate')
    plt.grid(True, alpha=0.3)
    
    # Plot 2: Convergence
    plt.subplot(1, 2, 2)
    plt.plot(sa.best_distances, 'b-', linewidth=1)
    plt.title('Convergence Plot')
    plt.xlabel('Iteration')
    plt.ylabel('Best Distance')
    plt.grid(True, alpha=0.3)
    
    plt.tight_layout()
    plt.show()

# Example usage
if __name__ == "__main__":
    # Generate random cities
    random.seed(42)  # For reproducible results
    cities = [(random.randint(0, 100), random.randint(0, 100)) for _ in range(15)]
    
    print("Cities coordinates:")
    for i, city in enumerate(cities):
        print(f"City {i}: {city}")
    
    # Run simulated annealing
    sa = SimulatedAnnealing(cities, initial_temp=1000, cooling_rate=0.95)
    best_path, best_distance = sa.solve(max_iterations=5000)
    
    print(f"\nBest path found: {best_path}")
    print(f"Best distance: {best_distance:.2f}")
    
    # Plot results
    plot_results(sa, cities)
```

## Key Components Explained:

### 1. **Distance Calculation**
```python
def distance(self, city1, city2):
    return math.sqrt((city1[0] - city2[0])**2 + (city1[1] - city2[1])**2)
```
Calculates Euclidean distance between two cities.

### 2. **Neighbor Generation**
```python
def random_neighbor(self, path):
    new_path = path.copy()
    i, j = random.sample(range(len(path)), 2)
    new_path[i], new_path[j] = new_path[j], new_path[i]
    return new_path
```
Creates a neighbor solution by swapping two random cities.

### 3. **Acceptance Probability**
```python
def acceptance_probability(self, old_cost, new_cost, temperature):
    if new_cost < old_cost:
        return 1.0
    return math.exp((old_cost - new_cost) / temperature)
```
Allows worse solutions to be accepted with decreasing probability as temperature cools.

### 4. **Main Algorithm Loop**
- Start with random solution
- Generate neighbor solution
- Accept or reject based on probability
- Cool down temperature
- Continue until stopping criteria met

## Parameters:
- **Initial Temperature**: Starting temperature (higher = more exploration)
- **Cooling Rate**: How quickly temperature decreases (typically 0.8-0.99)
- **Minimum Temperature**: When to stop cooling
- **Max Iterations**: Maximum number of iterations

This implementation demonstrates how simulated annealing can find near-optimal solutions to complex optimization problems like the TSP by balancing exploration and exploitation through the temperature parameter.

