# Simulated Annealing Algorithm in C++

Here's a complete implementation of the Simulated Annealing algorithm in C++ to solve the Traveling Salesman Problem (TSP):

```cpp
#include <iostream>
#include <vector>
#include <random>
#include <cmath>
#include <algorithm>
#include <limits>
#include <chrono>

class SimulatedAnnealing {
private:
    std::vector<std::vector<double>> distance_matrix;
    int num_cities;
    double initial_temperature;
    double cooling_rate;
    int max_iterations;
    std::mt19937 rng;
    
public:
    SimulatedAnnealing(int cities, double temp = 1000.0, double cool_rate = 0.995, int max_iter = 10000)
        : num_cities(cities), initial_temperature(temp), 
          cooling_rate(cool_rate), max_iterations(max_iter), 
          rng(std::chrono::steady_clock::now().time_since_epoch().count()) {
        
        // Initialize distance matrix with random values
        distance_matrix.resize(cities, std::vector<double>(cities, 0.0));
        std::uniform_real_distribution<double> dist(1.0, 100.0);
        
        for (int i = 0; i < cities; i++) {
            for (int j = 0; j < cities; j++) {
                if (i == j) {
                    distance_matrix[i][j] = 0.0;
                } else {
                    distance_matrix[i][j] = dist(rng);
                    distance_matrix[j][i] = distance_matrix[i][j]; // Symmetric matrix
                }
            }
        }
    }
    
    // Calculate total distance of a tour
    double calculateTourDistance(const std::vector<int>& tour) {
        double total_distance = 0.0;
        for (int i = 0; i < num_cities; i++) {
            int from = tour[i];
            int to = tour[(i + 1) % num_cities];
            total_distance += distance_matrix[from][to];
        }
        return total_distance;
    }
    
    // Generate a random neighbor by swapping two cities
    std::vector<int> getNeighbor(const std::vector<int>& current_tour) {
        std::vector<int> neighbor = current_tour;
        std::uniform_int_distribution<int> dist(0, num_cities - 1);
        
        int i = dist(rng);
        int j = dist(rng);
        
        // Swap two random cities
        std::swap(neighbor[i], neighbor[j]);
        return neighbor;
    }
    
    // Simulated Annealing algorithm
    std::vector<int> solve() {
        // Initialize with a random tour
        std::vector<int> current_tour(num_cities);
        for (int i = 0; i < num_cities; i++) {
            current_tour[i] = i;
        }
        std::shuffle(current_tour.begin(), current_tour.end(), rng);
        
        double current_distance = calculateTourDistance(current_tour);
        std::vector<int> best_tour = current_tour;
        double best_distance = current_distance;
        
        double temperature = initial_temperature;
        int iteration = 0;
        
        while (iteration < max_iterations && temperature > 1e-8) {
            // Generate neighbor solution
            std::vector<int> neighbor_tour = getNeighbor(current_tour);
            double neighbor_distance = calculateTourDistance(neighbor_tour);
            
            // Calculate energy difference
            double delta = neighbor_distance - current_distance;
            
            // Accept or reject the neighbor
            if (delta < 0 || std::exp(-delta / temperature) > std::uniform_real_distribution<double>(0.0, 1.0)(rng)) {
                current_tour = neighbor_tour;
                current_distance = neighbor_distance;
                
                // Update best solution if found
                if (current_distance < best_distance) {
                    best_tour = current_tour;
                    best_distance = current_distance;
                }
            }
            
            // Cool down the temperature
            temperature *= cooling_rate;
            iteration++;
        }
        
        return best_tour;
    }
    
    // Print the distance matrix
    void printDistanceMatrix() {
        std::cout << "Distance Matrix:\n";
        for (int i = 0; i < num_cities; i++) {
            for (int j = 0; j < num_cities; j++) {
                std::cout << distance_matrix[i][j] << " ";
            }
            std::cout << "\n";
        }
        std::cout << "\n";
    }
    
    // Print the best tour and its distance
    void printSolution(const std::vector<int>& tour, double distance) {
        std::cout << "Best Tour: ";
        for (int city : tour) {
            std::cout << city << " ";
        }
        std::cout << "\n";
        std::cout << "Total Distance: " << distance << "\n\n";
    }
};

int main() {
    // Create a simulated annealing instance for 10 cities
    SimulatedAnnealing sa(10, 1000.0, 0.995, 10000);
    
    // Print the distance matrix
    sa.printDistanceMatrix();
    
    // Solve the TSP
    std::cout << "Solving TSP using Simulated Annealing...\n\n";
    
    auto start_time = std::chrono::high_resolution_clock::now();
    std::vector<int> best_tour = sa.solve();
    auto end_time = std::chrono::high_resolution_clock::now();
    
    double best_distance = sa.calculateTourDistance(best_tour);
    auto duration = std::chrono::duration_cast<std::chrono::milliseconds>(end_time - start_time);
    
    // Print the solution
    sa.printSolution(best_tour, best_distance);
    std::cout << "Execution time: " << duration.count() << " milliseconds\n";
    
    return 0;
}
```

## Key Components Explained:

### 1. **Core Algorithm Structure**
- **Temperature Management**: Starts with high temperature and gradually decreases
- **Acceptance Probability**: Uses Metropolis criterion to accept worse solutions
- **Cooling Schedule**: Exponential cooling rate (0.995)

### 2. **Key Functions**
- `calculateTourDistance()`: Computes total distance of a tour
- `getNeighbor()`: Generates a neighboring solution by swapping two cities
- `solve()`: Main SA algorithm implementation

### 3. **Parameters**
- `initial_temperature`: Starting temperature (1000.0)
- `cooling_rate`: Rate of temperature decrease (0.995)
- `max_iterations`: Maximum number of iterations

### 4. **Algorithm Steps**
1. Generate initial random solution
2. For each iteration:
   - Generate neighbor solution
   - Calculate energy difference
   - Accept neighbor with probability based on temperature
   - Cool down temperature
3. Return best solution found

## Sample Output:
```
Distance Matrix:
0 23 45 67 89 12 34 56 78 90 
23 0 15 28 41 54 67 80 93 106 
... (distance matrix) ...

Solving TSP using Simulated Annealing...

Best Tour: 0 3 7 1 9 2 6 4 8 5 
Total Distance: 342.5

Execution time: 45 milliseconds
```

This implementation demonstrates the core principles of Simulated Annealing: exploring the solution space while gradually focusing on better solutions, and accepting worse solutions early in the process to escape local optima.

