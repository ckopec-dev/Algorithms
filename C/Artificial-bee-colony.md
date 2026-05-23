# Artificial Bee Colony Algorithm in C

Here's a complete implementation of the Artificial Bee Colony (ABC) algorithm in C for function optimization:

```c
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>

#define MAX_DIMENSION 100
#define MAX_POPULATION_SIZE 50
#define MAX_ITERATIONS 1000
#define LIMIT 100

// Objective function (Sphere function for demonstration)
double objective_function(double *x, int dimension) {
    double sum = 0.0;
    for (int i = 0; i < dimension; i++) {
        sum += x[i] * x[i];
    }
    return sum;
}

// Initialize population
void initialize_population(double **population, int population_size, int dimension, double min_bound, double max_bound) {
    srand(time(NULL));
    for (int i = 0; i < population_size; i++) {
        for (int j = 0; j < dimension; j++) {
            double rand_val = (double)rand() / RAND_MAX;
            population[i][j] = min_bound + rand_val * (max_bound - min_bound);
        }
    }
}

// Calculate fitness
double calculate_fitness(double value) {
    if (value >= 0) {
        return 1.0 / (1.0 + value);
    } else {
        return 1.0 + fabs(value);
    }
}

// Employed bee phase
void employed_bee_phase(double **population, double **new_population, double *fitness, 
                       double *trial, int population_size, int dimension, 
                       double min_bound, double max_bound, double *best_solution, double *best_fitness) {
    
    for (int i = 0; i < population_size; i++) {
        // Choose a random dimension
        int random_dimension = rand() % dimension;
        
        // Choose a random employed bee
        int random_bee = rand() % population_size;
        
        // Generate new solution
        double new_solution[MAX_DIMENSION];
        for (int j = 0; j < dimension; j++) {
            new_solution[j] = population[i][j];
        }
        
        // Generate new solution using mutation
        double rand_val = (double)rand() / RAND_MAX;
        new_solution[random_dimension] = population[i][random_dimension] + 
                                       rand_val * (population[i][random_dimension] - population[random_bee][random_dimension]);
        
        // Boundary check
        if (new_solution[random_dimension] < min_bound) {
            new_solution[random_dimension] = min_bound;
        }
        if (new_solution[random_dimension] > max_bound) {
            new_solution[random_dimension] = max_bound;
        }
        
        // Evaluate new solution
        double new_fitness = calculate_fitness(objective_function(new_solution, dimension));
        
        // Apply selection
        if (new_fitness > fitness[i]) {
            for (int j = 0; j < dimension; j++) {
                new_population[i][j] = new_solution[j];
            }
            fitness[i] = new_fitness;
            trial[i] = 0;
            
            // Update global best
            if (new_fitness > *best_fitness) {
                *best_fitness = new_fitness;
                for (int j = 0; j < dimension; j++) {
                    best_solution[j] = new_solution[j];
                }
            }
        } else {
            trial[i]++;
        }
    }
}

// Onlooker bee phase
void onlooker_bee_phase(double **population, double **new_population, double *fitness, 
                       double *trial, int population_size, int dimension, 
                       double min_bound, double max_bound, double *best_solution, double *best_fitness) {
    
    double total_fitness = 0.0;
    for (int i = 0; i < population_size; i++) {
        total_fitness += fitness[i];
    }
    
    for (int i = 0; i < population_size; i++) {
        double probability = fitness[i] / total_fitness;
        double rand_val = (double)rand() / RAND_MAX;
        
        if (rand_val < probability) {
            // Choose a random dimension
            int random_dimension = rand() % dimension;
            
            // Choose a random employed bee
            int random_bee = rand() % population_size;
            
            // Generate new solution
            double new_solution[MAX_DIMENSION];
            for (int j = 0; j < dimension; j++) {
                new_solution[j] = population[i][j];
            }
            
            // Generate new solution using mutation
            double rand_val2 = (double)rand() / RAND_MAX;
            new_solution[random_dimension] = population[i][random_dimension] + 
                                           rand_val2 * (population[i][random_dimension] - population[random_bee][random_dimension]);
            
            // Boundary check
            if (new_solution[random_dimension] < min_bound) {
                new_solution[random_dimension] = min_bound;
            }
            if (new_solution[random_dimension] > max_bound) {
                new_solution[random_dimension] = max_bound;
            }
            
            // Evaluate new solution
            double new_fitness = calculate_fitness(objective_function(new_solution, dimension));
            
            // Apply selection
            if (new_fitness > fitness[i]) {
                for (int j = 0; j < dimension; j++) {
                    new_population[i][j] = new_solution[j];
                }
                fitness[i] = new_fitness;
                trial[i] = 0;
                
                // Update global best
                if (new_fitness > *best_fitness) {
                    *best_fitness = new_fitness;
                    for (int j = 0; j < dimension; j++) {
                        best_solution[j] = new_solution[j];
                    }
                }
            } else {
                trial[i]++;
            }
        }
    }
}

// Scout bee phase
void scout_bee_phase(double **population, double **new_population, double *fitness, 
                    double *trial, int population_size, int dimension, 
                    double min_bound, double max_bound, double *best_solution, double *best_fitness) {
    
    for (int i = 0; i < population_size; i++) {
        if (trial[i] >= LIMIT) {
            // Scout bee creates a new random solution
            for (int j = 0; j < dimension; j++) {
                double rand_val = (double)rand() / RAND_MAX;
                new_population[i][j] = min_bound + rand_val * (max_bound - min_bound);
            }
            fitness[i] = calculate_fitness(objective_function(new_population[i], dimension));
            trial[i] = 0;
            
            // Update global best
            if (fitness[i] > *best_fitness) {
                *best_fitness = fitness[i];
                for (int j = 0; j < dimension; j++) {
                    best_solution[j] = new_population[i][j];
                }
            }
        }
    }
}

// Main ABC algorithm
void artificial_bee_colony(int dimension, int population_size, int max_iterations, 
                          double min_bound, double max_bound, double *best_solution, double *best_fitness) {
    
    // Allocate memory for population
    double **population = (double**)malloc(population_size * sizeof(double*));
    double **new_population = (double**)malloc(population_size * sizeof(double*));
    double *fitness = (double*)malloc(population_size * sizeof(double));
    double *trial = (double*)malloc(population_size * sizeof(double));
    
    for (int i = 0; i < population_size; i++) {
        population[i] = (double*)malloc(dimension * sizeof(double));
        new_population[i] = (double*)malloc(dimension * sizeof(double));
    }
    
    // Initialize population
    initialize_population(population, population_size, dimension, min_bound, max_bound);
    
    // Evaluate initial population
    for (int i = 0; i < population_size; i++) {
        fitness[i] = calculate_fitness(objective_function(population[i], dimension));
        trial[i] = 0;
    }
    
    // Find initial best solution
    *best_fitness = fitness[0];
    for (int i = 0; i < dimension; i++) {
        best_solution[i] = population[0][i];
    }
    
    for (int iter = 0; iter < max_iterations; iter++) {
        // Employed bee phase
        employed_bee_phase(population, new_population, fitness, trial, 
                          population_size, dimension, min_bound, max_bound, 
                          best_solution, best_fitness);
        
        // Onlooker bee phase
        onlooker_bee_phase(population, new_population, fitness, trial, 
                          population_size, dimension, min_bound, max_bound, 
                          best_solution, best_fitness);
        
        // Scout bee phase
        scout_bee_phase(population, new_population, fitness, trial, 
                       population_size, dimension, min_bound, max_bound, 
                       best_solution, best_fitness);
        
        // Update population
        for (int i = 0; i < population_size; i++) {
            for (int j = 0; j < dimension; j++) {
                population[i][j] = new_population[i][j];
            }
        }
        
        // Print progress
        if (iter % 100 == 0) {
            printf("Iteration %d: Best fitness = %f\n", iter, *best_fitness);
        }
    }
    
    // Free memory
    for (int i = 0; i < population_size; i++) {
        free(population[i]);
        free(new_population[i]);
    }
    free(population);
    free(new_population);
    free(fitness);
    free(trial);
}

// Main function
int main() {
    int dimension = 10;           // Number of dimensions
    int population_size = 20;     // Population size
    int max_iterations = 500;     // Maximum iterations
    double min_bound = -5.0;      // Lower bound
    double max_bound = 5.0;       // Upper bound
    
    double best_solution[MAX_DIMENSION];
    double best_fitness = 0.0;
    
    printf("Running Artificial Bee Colony Algorithm...\n");
    printf("Dimensions: %d\n", dimension);
    printf("Population Size: %d\n", population_size);
    printf("Max Iterations: %d\n", max_iterations);
    printf("Bounds: [%f, %f]\n\n", min_bound, max_bound);
    
    // Run ABC algorithm
    artificial_bee_colony(dimension, population_size, max_iterations, 
                         min_bound, max_bound, best_solution, &best_fitness);
    
    // Print results
    printf("\n=== Results ===\n");
    printf("Best fitness: %f\n", best_fitness);
    printf("Best solution: ");
    for (int i = 0; i < dimension; i++) {
        printf("%f ", best_solution[i]);
    }
    printf("\n");
    
    // Convert fitness back to objective function value
    double objective_value = 1.0 / best_fitness - 1.0;
    if (best_fitness > 1.0) {
        objective_value = best_fitness - 1.0;
    }
    printf("Objective function value: %f\n", objective_value);
    
    return 0;
}
```

## Key Features of this Implementation:

### 1. **Core Components:**
- **Employed Bee Phase**: Each bee explores neighboring solutions
- **Onlooker Bee Phase**: Bees select solutions based on fitness probability
- **Scout Bee Phase**: Abandon poor solutions and generate new random ones

### 2. **Algorithm Steps:**
1. Initialize population with random solutions
2. Evaluate fitness of all solutions
3. Employed bees search for better solutions
4. Onlooker bees probabilistically select and improve solutions
5. Scout bees replace abandoned solutions
6. Repeat until maximum iterations

### 3. **Parameters:**
- `dimension`: Number of variables to optimize
- `population_size`: Number of bees (solutions)
- `max_iterations`: Maximum number of algorithm iterations
- `min_bound`, `max_bound`: Search space boundaries

### 4. **Functions:**
- **Objective Function**: Sphere function (can be replaced with any function)
- **Fitness Calculation**: Converts objective function values to fitness
- **Boundary Handling**: Ensures solutions stay within bounds

### 5. **Usage:**
Compile with: `gcc -o abc abc.c -lm`

This implementation provides a solid foundation that can be easily modified for different optimization problems by changing the objective function and parameters.

