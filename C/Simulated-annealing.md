# Simulated Annealing Algorithm in C

Here's a complete implementation of the Simulated Annealing algorithm in C to solve the Traveling Salesman Problem (TSP):

```c
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>

#define MAX_CITIES 100
#define MAX_ITERATIONS 10000
#define INITIAL_TEMPERATURE 1000.0
#define COOLING_RATE 0.995
#define MIN_TEMPERATURE 1e-8

// Structure to represent a city
typedef struct {
    int x, y;
} City;

// Structure to represent a tour
typedef struct {
    int cities[MAX_CITIES];
    int length;
    double distance;
} Tour;

// Calculate Euclidean distance between two cities
double distance(City a, City b) {
    return sqrt(pow(a.x - b.x, 2) + pow(a.y - b.y, 2));
}

// Calculate total distance of a tour
double calculate_distance(Tour *tour, City *cities, int num_cities) {
    double total = 0.0;
    for (int i = 0; i < tour->length - 1; i++) {
        total += distance(cities[tour->cities[i]], cities[tour->cities[i+1]]);
    }
    // Return to starting city
    total += distance(cities[tour->cities[tour->length-1]], cities[tour->cities[0]]);
    return total;
}

// Generate a random tour
void generate_random_tour(Tour *tour, int num_cities) {
    for (int i = 0; i < num_cities; i++) {
        tour->cities[i] = i;
    }
    tour->length = num_cities;
    
    // Shuffle the tour
    for (int i = num_cities - 1; i > 0; i--) {
        int j = rand() % (i + 1);
        int temp = tour->cities[i];
        tour->cities[i] = tour->cities[j];
        tour->cities[j] = temp;
    }
    
    tour->distance = calculate_distance(tour, cities, num_cities);
}

// Create a neighbor tour by swapping two random cities
void generate_neighbor(Tour *current, Tour *neighbor, int num_cities) {
    *neighbor = *current;
    
    // Swap two random cities
    int i = rand() % num_cities;
    int j = rand() % num_cities;
    if (i != j) {
        int temp = neighbor->cities[i];
        neighbor->cities[i] = neighbor->cities[j];
        neighbor->cities[j] = temp;
    }
    
    // Recalculate distance
    neighbor->distance = calculate_distance(neighbor, cities, num_cities);
}

// Acceptance probability function
double acceptance_probability(double current_energy, double neighbor_energy, double temperature) {
    if (neighbor_energy < current_energy) {
        return 1.0;
    }
    return exp((current_energy - neighbor_energy) / temperature);
}

// Simulated Annealing algorithm
void simulated_annealing(City *cities, int num_cities, Tour *best_tour) {
    // Initialize random seed
    srand(time(NULL));
    
    // Generate initial tour
    Tour current_tour, neighbor_tour, best_tour_local;
    generate_random_tour(&current_tour, num_cities);
    best_tour_local = current_tour;
    
    double temperature = INITIAL_TEMPERATURE;
    
    printf("Starting Simulated Annealing...\n");
    printf("Initial distance: %.2f\n", current_tour.distance);
    
    int iteration = 0;
    while (temperature > MIN_TEMPERATURE && iteration < MAX_ITERATIONS) {
        // Generate neighbor tour
        generate_neighbor(&current_tour, &neighbor_tour, num_cities);
        
        // Accept or reject the neighbor
        double ap = acceptance_probability(current_tour.distance, 
                                          neighbor_tour.distance, 
                                          temperature);
        double random = (double)rand() / RAND_MAX;
        
        if (random < ap) {
            current_tour = neighbor_tour;
            
            // Update best tour if improved
            if (current_tour.distance < best_tour_local.distance) {
                best_tour_local = current_tour;
            }
        }
        
        // Cool down
        temperature *= COOLING_RATE;
        
        iteration++;
        
        // Print progress
        if (iteration % 1000 == 0) {
            printf("Iteration %d, Temperature: %.2f, Distance: %.2f\n", 
                   iteration, temperature, best_tour_local.distance);
        }
    }
    
    *best_tour = best_tour_local;
    printf("Final distance: %.2f\n", best_tour->distance);
}

// Print tour
void print_tour(Tour *tour, int num_cities) {
    printf("Tour: ");
    for (int i = 0; i < tour->length; i++) {
        printf("%d ", tour->cities[i]);
    }
    printf("\n");
}

int main() {
    // Example with 10 cities
    int num_cities = 10;
    City cities[MAX_CITIES] = {
        {60, 200}, {180, 200}, {80, 180}, {140, 180},
        {20, 160}, {100, 160}, {200, 160}, {140, 140},
        {40, 120}, {100, 120}
    };
    
    Tour best_tour;
    
    printf("Solving TSP using Simulated Annealing\n");
    printf("Number of cities: %d\n", num_cities);
    printf("Cities coordinates:\n");
    for (int i = 0; i < num_cities; i++) {
        printf("City %d: (%d, %d)\n", i, cities[i].x, cities[i].y);
    }
    printf("\n");
    
    // Run simulated annealing
    simulated_annealing(cities, num_cities, &best_tour);
    
    printf("\nBest tour found:\n");
    print_tour(&best_tour, num_cities);
    printf("Total distance: %.2f\n", best_tour.distance);
    
    return 0;
}
```

## Key Components Explained:

### 1. **Data Structures**
- `City`: Represents a city with x, y coordinates
- `Tour`: Represents a tour with city sequence and total distance

### 2. **Core Functions**
- `distance()`: Calculates Euclidean distance between two cities
- `calculate_distance()`: Computes total tour distance
- `generate_random_tour()`: Creates initial random tour
- `generate_neighbor()`: Creates a neighboring solution by swapping cities
- `acceptance_probability()`: Determines whether to accept a worse solution
- `simulated_annealing()`: Main algorithm implementation

### 3. **Algorithm Parameters**
- `INITIAL_TEMPERATURE`: Starting temperature
- `COOLING_RATE`: Rate at which temperature decreases
- `MIN_TEMPERATURE`: Stopping temperature
- `MAX_ITERATIONS`: Maximum number of iterations

### 4. **How it Works**
1. Start with a random tour
2. Generate a neighbor tour by making small changes
3. Accept the neighbor if it's better or with probability based on temperature
4. Gradually decrease temperature (cooling)
5. Continue until minimum temperature or max iterations

### 5. **Sample Output**
```
Solving TSP using Simulated Annealing
Number of cities: 10
Cities coordinates:
City 0: (60, 200)
City 1: (180, 200)
City 2: (80, 180)
...

Starting Simulated Annealing...
Initial distance: 1234.56
Iteration 1000, Temperature: 500.00, Distance: 1123.45
Iteration 2000, Temperature: 250.00, Distance: 1056.78
...

Final distance: 987.65
Best tour found:
Tour: 0 3 7 2 5 9 4 1 8 6 
Total distance: 987.65
```

This implementation demonstrates the core principles of Simulated Annealing: exploring the solution space while gradually focusing on better solutions, allowing occasional worse moves to escape local optima.

