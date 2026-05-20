# Genetic Algorithm Example in C++

Here's a complete implementation of a genetic algorithm to solve the classic "One Max" problem (finding a binary string of all 1s):

```cpp
#include <iostream>
#include <vector>
#include <random>
#include <algorithm>
#include <numeric>

class GeneticAlgorithm {
private:
    int populationSize;
    int chromosomeLength;
    double mutationRate;
    double crossoverRate;
    std::vector<std::vector<int>> population;
    std::mt19937 rng;
    
    // Initialize random number generator
    std::uniform_int_distribution<int> dist01{0, 1};
    std::uniform_real_distribution<double> dist01Double{0.0, 1.0};
    std::uniform_int_distribution<int> distIndex{0, 0};
    
public:
    GeneticAlgorithm(int popSize, int chromLength, double mutRate, double crossRate) 
        : populationSize(popSize), 
          chromosomeLength(chromLength), 
          mutationRate(mutRate), 
          crossoverRate(crossRate),
          rng(std::random_device{}()) {
        
        // Initialize population with random binary strings
        population.resize(populationSize);
        for (int i = 0; i < populationSize; i++) {
            population[i].resize(chromosomeLength);
            for (int j = 0; j < chromosomeLength; j++) {
                population[i][j] = dist01(rng);
            }
        }
    }
    
    // Fitness function: count number of 1s in chromosome
    int fitness(const std::vector<int>& chromosome) {
        return std::accumulate(chromosome.begin(), chromosome.end(), 0);
    }
    
    // Selection using tournament selection
    std::vector<int> tournamentSelection() {
        int tournamentSize = 3;
        int bestIndex = dist01(rng) * populationSize;
        int bestFitness = fitness(population[bestIndex]);
        
        for (int i = 1; i < tournamentSize; i++) {
            int currentIndex = dist01(rng) * populationSize;
            int currentFitness = fitness(population[currentIndex]);
            if (currentFitness > bestFitness) {
                bestIndex = currentIndex;
                bestFitness = currentFitness;
            }
        }
        
        return population[bestIndex];
    }
    
    // Single point crossover
    std::pair<std::vector<int>, std::vector<int>> crossover(const std::vector<int>& parent1, 
                                                           const std::vector<int>& parent2) {
        std::vector<int> child1 = parent1;
        std::vector<int> child2 = parent2;
        
        if (dist01Double(rng) < crossoverRate && chromosomeLength > 1) {
            int crossoverPoint = 1 + dist01(rng) * (chromosomeLength - 2);
            
            for (int i = crossoverPoint; i < chromosomeLength; i++) {
                std::swap(child1[i], child2[i]);
            }
        }
        
        return {child1, child2};
    }
    
    // Mutation
    void mutate(std::vector<int>& chromosome) {
        for (int i = 0; i < chromosomeLength; i++) {
            if (dist01Double(rng) < mutationRate) {
                chromosome[i] = 1 - chromosome[i]; // Flip bit
            }
        }
    }
    
    // Evolution step
    void evolve() {
        std::vector<std::vector<int>> newPopulation;
        newPopulation.reserve(populationSize);
        
        // Elitism: keep best individual
        int bestIndex = 0;
        int bestFitness = fitness(population[0]);
        for (int i = 1; i < populationSize; i++) {
            int currentFitness = fitness(population[i]);
            if (currentFitness > bestFitness) {
                bestIndex = i;
                bestFitness = currentFitness;
            }
        }
        newPopulation.push_back(population[bestIndex]);
        
        // Generate offspring
        while (newPopulation.size() < populationSize) {
            std::vector<int> parent1 = tournamentSelection();
            std::vector<int> parent2 = tournamentSelection();
            
            auto children = crossover(parent1, parent2);
            mutate(children.first);
            mutate(children.second);
            
            newPopulation.push_back(children.first);
            if (newPopulation.size() < populationSize) {
                newPopulation.push_back(children.second);
            }
        }
        
        population = newPopulation;
    }
    
    // Get best individual in current population
    std::vector<int> getBestIndividual() {
        int bestIndex = 0;
        int bestFitness = fitness(population[0]);
        for (int i = 1; i < populationSize; i++) {
            int currentFitness = fitness(population[i]);
            if (currentFitness > bestFitness) {
                bestIndex = i;
                bestFitness = currentFitness;
            }
        }
        return population[bestIndex];
    }
    
    // Print current population
    void printPopulation() {
        for (int i = 0; i < populationSize; i++) {
            for (int j = 0; j < chromosomeLength; j++) {
                std::cout << population[i][j];
            }
            std::cout << " (fitness: " << fitness(population[i]) << ")\n";
        }
        std::cout << "\n";
    }
};

int main() {
    // Parameters
    const int POPULATION_SIZE = 20;
    const int CHROMOSOME_LENGTH = 10;
    const double MUTATION_RATE = 0.01;
    const double CROSSOVER_RATE = 0.8;
    const int MAX_GENERATIONS = 100;
    
    // Create genetic algorithm instance
    GeneticAlgorithm ga(POPULATION_SIZE, CHROMOSOME_LENGTH, MUTATION_RATE, CROSSOVER_RATE);
    
    std::cout << "Genetic Algorithm - One Max Problem\n";
    std::cout << "Population Size: " << POPULATION_SIZE << "\n";
    std::cout << "Chromosome Length: " << CHROMOSOME_LENGTH << "\n";
    std::cout << "Mutation Rate: " << MUTATION_RATE << "\n";
    std::cout << "Crossover Rate: " << CROSSOVER_RATE << "\n\n";
    
    // Evolution loop
    for (int generation = 0; generation < MAX_GENERATIONS; generation++) {
        // Print population every 10 generations
        if (generation % 10 == 0) {
            std::cout << "Generation " << generation << ":\n";
            ga.printPopulation();
        }
        
        ga.evolve();
        
        // Check if solution found (all 1s)
        std::vector<int> best = ga.getBestIndividual();
        int bestFitness = ga.fitness(best);
        if (bestFitness == CHROMOSOME_LENGTH) {
            std::cout << "Solution found in generation " << generation << "!\n";
            std::cout << "Best individual: ";
            for (int bit : best) std::cout << bit;
            std::cout << " (fitness: " << bestFitness << ")\n";
            break;
        }
    }
    
    // Final result
    std::cout << "\nFinal result:\n";
    std::vector<int> finalBest = ga.getBestIndividual();
    int finalFitness = ga.fitness(finalBest);
    std::cout << "Best individual: ";
    for (int bit : finalBest) std::cout << bit;
    std::cout << " (fitness: " << finalFitness << "/" << CHROMOSOME_LENGTH << ")\n";
    
    return 0;
}
```

## Key Components Explained

### 1. **Chromosome Representation**
- Binary strings of fixed length
- Each bit represents a gene

### 2. **Genetic Operators**
- **Selection**: Tournament selection
- **Crossover**: Single-point crossover
- **Mutation**: Bit-flip mutation

### 3. **Algorithm Steps**
1. Initialize random population
2. Evaluate fitness of each individual
3. Select parents using tournament selection
4. Apply crossover and mutation to create offspring
5. Replace old population with new one
6. Repeat until solution found or max generations reached

### 4. **Parameters**
- Population size: 20 individuals
- Chromosome length: 10 bits
- Mutation rate: 1%
- Crossover rate: 80%
- Max generations: 100

This example demonstrates the core concepts of genetic algorithms: evolution through selection, crossover, and mutation to find optimal solutions to optimization problems.

