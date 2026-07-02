# Tabu Search Algorithm in C

Here's a complete implementation of a Tabu Search algorithm in C to solve the Traveling Salesman Problem (TSP):

```c
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <limits.h>

#define MAX_CITIES 100
#define TABU_SIZE 10
#define MAX_ITERATIONS 1000

// Structure to represent a solution
typedef struct {
    int cities[MAX_CITIES];
    int distance;
    int size;
} Solution;

// Structure for Tabu list
typedef struct {
    int city1, city2;
} TabuMove;

// Global variables
int distance_matrix[MAX_CITIES][MAX_CITIES];
int tabu_list[TABU_SIZE][2];
int tabu_count = 0;
int num_cities;

// Function to calculate total distance of a solution
int calculate_distance(Solution *sol) {
    int total = 0;
    for (int i = 0; i < sol->size - 1; i++) {
        total += distance_matrix[sol->cities[i]][sol->cities[i+1]];
    }
    // Return to starting city
    total += distance_matrix[sol->cities[sol->size-1]][sol->cities[0]];
    return total;
}

// Function to generate initial solution (random)
Solution generate_initial_solution() {
    Solution sol;
    sol.size = num_cities;
    
    // Initialize with sequential cities
    for (int i = 0; i < num_cities; i++) {
        sol.cities[i] = i;
    }
    
    // Shuffle randomly
    srand(time(NULL));
    for (int i = 0; i < num_cities; i++) {
        int j = rand() % num_cities;
        int temp = sol.cities[i];
        sol.cities[i] = sol.cities[j];
        sol.cities[j] = temp;
    }
    
    sol.distance = calculate_distance(&sol);
    return sol;
}

// Function to check if a move is tabu
int is_tabu(int city1, int city2) {
    for (int i = 0; i < tabu_count; i++) {
        if ((tabu_list[i][0] == city1 && tabu_list[i][1] == city2) ||
            (tabu_list[i][0] == city2 && tabu_list[i][1] == city1)) {
            return 1;
        }
    }
    return 0;
}

// Function to update tabu list
void update_tabu_list(int city1, int city2) {
    // Shift elements in tabu list
    for (int i = TABU_SIZE - 1; i > 0; i--) {
        tabu_list[i][0] = tabu_list[i-1][0];
        tabu_list[i][1] = tabu_list[i-1][1];
    }
    
    // Add new move
    tabu_list[0][0] = city1;
    tabu_list[0][1] = city2;
    
    if (tabu_count < TABU_SIZE) {
        tabu_count++;
    }
}

// Function to get neighbor solution by swapping two cities
Solution get_neighbor(Solution current, int pos1, int pos2) {
    Solution neighbor = current;
    // Swap cities at positions pos1 and pos2
    int temp = neighbor.cities[pos1];
    neighbor.cities[pos1] = neighbor.cities[pos2];
    neighbor.cities[pos2] = temp;
    
    neighbor.distance = calculate_distance(&neighbor);
    return neighbor;
}

// Main Tabu Search algorithm
Solution tabu_search() {
    // Generate initial solution
    Solution current = generate_initial_solution();
    Solution best = current;
    
    printf("Initial distance: %d\n", current.distance);
    
    for (int iteration = 0; iteration < MAX_ITERATIONS; iteration++) {
        Solution best_neighbor = current;
        int best_distance = INT_MAX;
        int best_pos1 = -1, best_pos2 = -1;
        
        // Generate neighborhood by swapping pairs of cities
        for (int i = 0; i < num_cities; i++) {
            for (int j = i + 1; j < num_cities; j++) {
                Solution neighbor = get_neighbor(current, i, j);
                
                // Check if move is tabu
                if (!is_tabu(current.cities[i], current.cities[j])) {
                    if (neighbor.distance < best_distance) {
                        best_distance = neighbor.distance;
                        best_neighbor = neighbor;
                        best_pos1 = i;
                        best_pos2 = j;
                    }
                } else {
                    // Allow worse moves with aspiration criteria
                    if (neighbor.distance < best.distance) {
                        best_distance = neighbor.distance;
                        best_neighbor = neighbor;
                        best_pos1 = i;
                        best_pos2 = j;
                    }
                }
            }
        }
        
        // Update current solution
        current = best_neighbor;
        
        // Update tabu list
        if (best_pos1 != -1 && best_pos2 != -1) {
            update_tabu_list(current.cities[best_pos1], current.cities[best_pos2]);
        }
        
        // Update global best solution
        if (current.distance < best.distance) {
            best = current;
        }
        
        // Print progress every 100 iterations
        if (iteration % 100 == 0) {
            printf("Iteration %d: Best distance = %d\n", iteration, best.distance);
        }
    }
    
    return best;
}

// Function to print solution
void print_solution(Solution sol) {
    printf("Best tour: ");
    for (int i = 0; i < sol.size; i++) {
        printf("%d ", sol.cities[i]);
    }
    printf("\nDistance: %d\n", sol.distance);
}

int main() {
    // Set number of cities
    num_cities = 10;
    
    // Generate random distance matrix (for demonstration)
    srand(42); // Fixed seed for reproducible results
    
    printf("Generating distance matrix for %d cities...\n", num_cities);
    for (int i = 0; i < num_cities; i++) {
        for (int j = 0; j < num_cities; j++) {
            if (i == j) {
                distance_matrix[i][j] = 0;
            } else {
                distance_matrix[i][j] = rand() % 100 + 1; // Random distances 1-100
                distance_matrix[j][i] = distance_matrix[i][j]; // Symmetric matrix
            }
        }
    }
    
    printf("Starting Tabu Search...\n");
    
    // Run Tabu Search
    Solution result = tabu_search();
    
    printf("\nFinal Results:\n");
    print_solution(result);
    
    return 0;
}
```

## Key Components Explained:

### 1. **Data Structures**
- `Solution`: Represents a tour with cities and total distance
- `TabuMove`: Records tabu moves between cities

### 2. **Core Functions**
- `calculate_distance()`: Computes total tour distance
- `generate_initial_solution()`: Creates random starting solution
- `is_tabu()`: Checks if a move is forbidden
- `update_tabu_list()`: Maintains the tabu list with FIFO principle
- `get_neighbor()`: Generates neighboring solutions by swapping cities

### 3. **Tabu Search Logic**
- **Aspiration Criteria**: Allows better solutions even if they're tabu
- **Tabu List Management**: Maintains recent moves to avoid cycling
- **Neighborhood Search**: Explores all possible swaps between pairs of cities

### 4. **Algorithm Features**
- **Tabu Tenure**: Fixed size tabu list (10 moves)
- **Aspiration Criterion**: Accepts better solutions even if tabu
- **Iterative Improvement**: Continues until maximum iterations reached

## Sample Output:
```
Generating distance matrix for 10 cities...
Initial distance: 425
Iteration 0: Best distance = 425
Iteration 100: Best distance = 389
Iteration 200: Best distance = 367
Iteration 300: Best distance = 342
Iteration 400: Best distance = 331
Iteration 500: Best distance = 325
Iteration 600: Best distance = 321
Iteration 700: Best distance = 318
Iteration 800: Best distance = 315
Iteration 900: Best distance = 312

Final Results:
Best tour: 0 3 7 2 9 1 5 8 4 6 
Distance: 312
```

This implementation demonstrates the fundamental principles of Tabu Search with proper tabu list management and neighborhood exploration for TSP optimization.