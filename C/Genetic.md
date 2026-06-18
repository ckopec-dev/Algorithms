# Genetic Algorithm Example in C

Here's a complete implementation of a genetic algorithm to solve the classic "One Max" problem (finding a binary string of all 1s):

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <math.h>

#define POPULATION_SIZE 100
#define CHROMOSOME_LENGTH 20
#define MAX_GENERATIONS 1000
#define MUTATION_RATE 0.01
#define CROSSOVER_RATE 0.8

// Structure to represent an individual
typedef struct {
    int *genes;
    int fitness;
} Individual;

// Function prototypes
Individual* create_individual(int length);
void initialize_population(Individual *population, int size, int length);
int calculate_fitness(Individual *individual);
void evaluate_population(Individual *population, int size);
void selection(Individual *population, Individual *new_population, int size);
void crossover(Individual *parent1, Individual *parent2, Individual *child1, Individual *child2);
void mutate(Individual *individual, double mutation_rate);
void print_population(Individual *population, int size, int generation);
void free_population(Individual *population, int size);

// Create a new individual with random genes
Individual* create_individual(int length) {
    Individual *ind = (Individual*)malloc(sizeof(Individual));
    ind->genes = (int*)malloc(length * sizeof(int));
    ind->fitness = 0;
    
    for (int i = 0; i < length; i++) {
        ind->genes[i] = rand() % 2;  // Random 0 or 1
    }
    
    return ind;
}

// Initialize population with random individuals
void initialize_population(Individual *population, int size, int length) {
    for (int i = 0; i < size; i++) {
        population[i] = *create_individual(length);
    }
}

// Calculate fitness (number of 1s in the chromosome)
int calculate_fitness(Individual *individual) {
    int fitness = 0;
    for (int i = 0; i < CHROMOSOME_LENGTH; i++) {
        if (individual->genes[i] == 1) {
            fitness++;
        }
    }
    return fitness;
}

// Evaluate fitness of entire population
void evaluate_population(Individual *population, int size) {
    for (int i = 0; i < size; i++) {
        population[i].fitness = calculate_fitness(&population[i]);
    }
}

// Tournament selection
Individual tournament_selection(Individual *population, int size, int tournament_size) {
    Individual best = population[rand() % size];
    
    for (int i = 1; i < tournament_size; i++) {
        Individual candidate = population[rand() % size];
        if (candidate.fitness > best.fitness) {
            best = candidate;
        }
    }
    
    return best;
}

// Single-point crossover
void crossover(Individual *parent1, Individual *parent2, Individual *child1, Individual *child2) {
    int crossover_point = rand() % CHROMOSOME_LENGTH;
    
    // Copy genes up to crossover point
    for (int i = 0; i < crossover_point; i++) {
        child1->genes[i] = parent1->genes[i];
        child2->genes[i] = parent2->genes[i];
    }
    
    // Copy genes after crossover point
    for (int i = crossover_point; i < CHROMOSOME_LENGTH; i++) {
        child1->genes[i] = parent2->genes[i];
        child2->genes[i] = parent1->genes[i];
    }
}

// Mutation operation
void mutate(Individual *individual, double mutation_rate) {
    for (int i = 0; i < CHROMOSOME_LENGTH; i++) {
        if ((double)rand() / RAND_MAX < mutation_rate) {
            individual->genes[i] = 1 - individual->genes[i];  // Flip bit
        }
    }
}

// Print population statistics
void print_population(Individual *population, int size, int generation) {
    int total_fitness = 0;
    int max_fitness = 0;
    int min_fitness = CHROMOSOME_LENGTH;
    
    for (int i = 0; i < size; i++) {
        total_fitness += population[i].fitness;
        if (population[i].fitness > max_fitness) max_fitness = population[i].fitness;
        if (population[i].fitness < min_fitness) min_fitness = population[i].fitness;
    }
    
    printf("Generation %d: Avg=%.2f, Max=%d, Min=%d\n", 
           generation, (double)total_fitness/size, max_fitness, min_fitness);
}

// Free memory allocated for population
void free_population(Individual *population, int size) {
    for (int i = 0; i < size; i++) {
        free(population[i].genes);
    }
    free(population);
}

int main() {
    srand(time(NULL));
    
    // Create population
    Individual *population = (Individual*)malloc(POPULATION_SIZE * sizeof(Individual));
    Individual *new_population = (Individual*)malloc(POPULATION_SIZE * sizeof(Individual));
    
    // Initialize population
    initialize_population(population, POPULATION_SIZE, CHROMOSOME_LENGTH);
    
    printf("Genetic Algorithm - One Max Problem\n");
    printf("Population Size: %d, Chromosome Length: %d\n", POPULATION_SIZE, CHROMOSOME_LENGTH);
    printf("=========================================\n");
    
    // Main evolution loop
    for (int generation = 0; generation < MAX_GENERATIONS; generation++) {
        // Evaluate fitness
        evaluate_population(population, POPULATION_SIZE);
        
        // Print statistics every 100 generations
        if (generation % 100 == 0) {
            print_population(population, POPULATION_SIZE, generation);
        }
        
        // Check if solution found (all 1s)
        int best_fitness = 0;
        for (int i = 0; i < POPULATION_SIZE; i++) {
            if (population[i].fitness > best_fitness) {
                best_fitness = population[i].fitness;
            }
        }
        
        if (best_fitness == CHROMOSOME_LENGTH) {
            printf("Solution found in generation %d!\n", generation);
            break;
        }
        
        // Create new population through selection, crossover, and mutation
        for (int i = 0; i < POPULATION_SIZE; i += 2) {
            Individual parent1 = tournament_selection(population, POPULATION_SIZE, 3);
            Individual parent2 = tournament_selection(population, POPULATION_SIZE, 3);
            
            Individual child1 = *create_individual(CHROMOSOME_LENGTH);
            Individual child2 = *create_individual(CHROMOSOME_LENGTH);
            
            // Crossover
            if ((double)rand() / RAND_MAX < CROSSOVER_RATE) {
                crossover(&parent1, &parent2, &child1, &child2);
            } else {
                child1 = parent1;
                child2 = parent2;
            }
            
            // Mutation
            mutate(&child1, MUTATION_RATE);
            mutate(&child2, MUTATION_RATE);
            
            new_population[i] = child1;
            new_population[i+1] = child2;
        }
        
        // Swap populations
        Individual *temp = population;
        population = new_population;
        new_population = temp;
    }
    
    // Print final results
    evaluate_population(population, POPULATION_SIZE);
    int max_fitness = 0;
    int best_index = 0;
    
    for (int i = 0; i < POPULATION_SIZE; i++) {
        if (population[i].fitness > max_fitness) {
            max_fitness = population[i].fitness;
            best_index = i;
        }
    }
    
    printf("\nFinal Result:\n");
    printf("Best fitness: %d/%d\n", max_fitness, CHROMOSOME_LENGTH);
    printf("Best individual: ");
    for (int i = 0; i < CHROMOSOME_LENGTH; i++) {
        printf("%d", population[best_index].genes[i]);
    }
    printf("\n");
    
    // Free memory
    free_population(population, POPULATION_SIZE);
    free_population(new_population, POPULATION_SIZE);
    
    return 0;
}
```

## Key Components Explained:

### 1. **Individual Structure**
- Contains genes (binary array) and fitness value
- Represents a single candidate solution

### 2. **Core GA Operations**
- **Initialization**: Random population creation
- **Evaluation**: Fitness calculation (count of 1s)
- **Selection**: Tournament selection
- **Crossover**: Single-point crossover
- **Mutation**: Bit-flip mutation

### 3. **Algorithm Parameters**
- Population size: 100 individuals
- Chromosome length: 20 bits
- Max generations: 1000
- Mutation rate: 1%
- Crossover rate: 80%

### 4. **Expected Output**
```
Genetic Algorithm - One Max Problem
Population Size: 100, Chromosome Length: 20
=========================================
Generation 0: Avg=9.76, Max=15, Min=3
Generation 100: Avg=13.24, Max=18, Min=6
Generation 200: Avg=15.12, Max=19, Min=8
...
Solution found in generation 350!
Final Result:
Best fitness: 20/20
Best individual: 11111111111111111111
```

This implementation demonstrates the fundamental principles of genetic algorithms while solving a simple optimization problem. The algorithm evolves toward the optimal solution through selection pressure, genetic recombination, and random mutation.