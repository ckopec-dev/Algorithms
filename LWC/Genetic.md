# Genetic Algorithm in Lightning Web Component

Here's a complete example of a Genetic Algorithm implemented in Lightning Web Components:

```javascript
// geneticAlgorithm.js
import { LightningElement, track } from 'lwc';

export default class GeneticAlgorithm extends LightningElement {
    @track population = [];
    @track bestIndividual = null;
    @track generation = 0;
    @track isRunning = false;
    @track fitnessHistory = [];

    // Configuration
    config = {
        populationSize: 50,
        chromosomeLength: 10,
        mutationRate: 0.01,
        crossoverRate: 0.8,
        elitismRate: 0.1
    };

    // Target string to evolve towards
    target = '1111111111';
    
    // Initialize population
    initializePopulation() {
        this.population = [];
        for (let i = 0; i < this.config.populationSize; i++) {
            const chromosome = this.generateRandomChromosome();
            this.population.push({
                chromosome: chromosome,
                fitness: this.calculateFitness(chromosome)
            });
        }
        this.sortPopulation();
        this.updateBestIndividual();
    }

    // Generate random binary chromosome
    generateRandomChromosome() {
        let chromosome = '';
        for (let i = 0; i < this.config.chromosomeLength; i++) {
            chromosome += Math.random() > 0.5 ? '1' : '0';
        }
        return chromosome;
    }

    // Calculate fitness (number of matching characters)
    calculateFitness(chromosome) {
        let fitness = 0;
        for (let i = 0; i < chromosome.length; i++) {
            if (chromosome[i] === this.target[i]) {
                fitness++;
            }
        }
        return fitness;
    }

    // Sort population by fitness (descending)
    sortPopulation() {
        this.population.sort((a, b) => b.fitness - a.fitness);
    }

    // Update best individual
    updateBestIndividual() {
        this.bestIndividual = this.population[0];
        this.fitnessHistory.push(this.bestIndividual.fitness);
    }

    // Selection using tournament selection
    tournamentSelection() {
        const tournamentSize = 3;
        let best = null;
        
        for (let i = 0; i < tournamentSize; i++) {
            const randomIndex = Math.floor(Math.random() * this.config.populationSize);
            const individual = this.population[randomIndex];
            
            if (best === null || individual.fitness > best.fitness) {
                best = individual;
            }
        }
        
        return best.chromosome;
    }

    // Crossover two parents
    crossover(parent1, parent2) {
        if (Math.random() > this.config.crossoverRate) {
            return [parent1, parent2];
        }

        const crossoverPoint = Math.floor(Math.random() * this.config.chromosomeLength);
        const child1 = parent1.substring(0, crossoverPoint) + parent2.substring(crossoverPoint);
        const child2 = parent2.substring(0, crossoverPoint) + parent1.substring(crossoverPoint);
        
        return [child1, child2];
    }

    // Mutation
    mutate(chromosome) {
        let mutated = '';
        for (let i = 0; i < chromosome.length; i++) {
            if (Math.random() < this.config.mutationRate) {
                mutated += chromosome[i] === '1' ? '0' : '1';
            } else {
                mutated += chromosome[i];
            }
        }
        return mutated;
    }

    // Evolve to next generation
    evolve() {
        const newPopulation = [];
        
        // Elitism - keep best individuals
        const elitismCount = Math.floor(this.config.populationSize * this.config.elitismRate);
        for (let i = 0; i < elitismCount; i++) {
            newPopulation.push({ ...this.population[i] });
        }
        
        // Generate offspring
        while (newPopulation.length < this.config.populationSize) {
            const parent1 = this.tournamentSelection();
            const parent2 = this.tournamentSelection();
            
            const [child1, child2] = this.crossover(parent1, parent2);
            
            const mutatedChild1 = this.mutate(child1);
            const mutatedChild2 = this.mutate(child2);
            
            newPopulation.push({
                chromosome: mutatedChild1,
                fitness: this.calculateFitness(mutatedChild1)
            });
            
            if (newPopulation.length < this.config.populationSize) {
                newPopulation.push({
                    chromosome: mutatedChild2,
                    fitness: this.calculateFitness(mutatedChild2)
                });
            }
        }
        
        this.population = newPopulation;
        this.sortPopulation();
        this.updateBestIndividual();
        this.generation++;
    }

    // Start the genetic algorithm
    start() {
        this.isRunning = true;
        this.generation = 0;
        this.fitnessHistory = [];
        this.initializePopulation();
        
        const interval = setInterval(() => {
            if (!this.isRunning) {
                clearInterval(interval);
                return;
            }
            
            this.evolve();
            
            // Stop if we've reached the target
            if (this.bestIndividual.fitness === this.config.chromosomeLength) {
                this.isRunning = false;
                clearInterval(interval);
            }
        }, 100);
    }

    // Stop the genetic algorithm
    stop() {
        this.isRunning = false;
    }

    // Reset the algorithm
    reset() {
        this.isRunning = false;
        this.generation = 0;
        this.fitnessHistory = [];
        this.population = [];
        this.bestIndividual = null;
    }

    // Handle start button click
    handleStart() {
        this.start();
    }

    // Handle stop button click
    handleStop() {
        this.stop();
    }

    // Handle reset button click
    handleReset() {
        this.reset();
    }
}
```

```html
<!-- geneticAlgorithm.html -->
<template>
    <div class="container">
        <h2>Genetic Algorithm Demo</h2>
        
        <div class="controls">
            <lightning-button 
                label="Start" 
                variant="brand" 
                onclick={handleStart}
                disabled={isRunning}>
            </lightning-button>
            
            <lightning-button 
                label="Stop" 
                variant="destructive" 
                onclick={handleStop}
                disabled={!isRunning}>
            </lightning-button>
            
            <lightning-button 
                label="Reset" 
                variant="neutral" 
                onclick={handleReset}>
            </lightning-button>
        </div>

        <div class="stats">
            <div class="stat-item">
                <span class="label">Generation:</span>
                <span class="value">{generation}</span>
            </div>
            <div class="stat-item">
                <span class="label">Best Fitness:</span>
                <span class="value">{bestIndividual.fitness}</span>
            </div>
            <div class="stat-item">
                <span class="label">Target:</span>
                <span class="value">{target}</span>
            </div>
            <div class="stat-item">
                <span class="label">Best Individual:</span>
                <span class="value">{bestIndividual.chromosome}</span>
            </div>
        </div>

        <div class="progress">
            <lightning-progress-bar 
                value={bestIndividual.fitness}
                size="medium"
                variant="success"
                label="Fitness Progress">
            </lightning-progress-bar>
        </div>

        <div class="population">
            <h3>Population (Top 10)</h3>
            <lightning-datatable
                data={population}
                columns={columns}
                hide-checkbox-column="true"
                max-row-selection="10">
            </lightning-datatable>
        </div>

        <div class="history">
            <h3>Fitness History</h3>
            <div class="chart">
                <template for:each={fitnessHistory} for:item="point" for:index="index">
                    <div key={point} class="bar" style={getBarStyle(point)}>
                        <span class="bar-value">{point}</span>
                    </div>
                </template>
            </div>
        </div>
    </div>
</template>
```

```css
/* geneticAlgorithm.css */
.container {
    padding: 20px;
    max-width: 1200px;
    margin: 0 auto;
}

.controls {
    margin-bottom: 20px;
    display: flex;
    gap: 10px;
    flex-wrap: wrap;
}

.stats {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
    gap: 15px;
    margin-bottom: 20px;
    background-color: #f8f9fa;
    padding: 15px;
    border-radius: 8px;
}

.stat-item {
    display: flex;
    flex-direction: column;
}

.label {
    font-weight: bold;
    color: #333;
    margin-bottom: 5px;
}

.value {
    color: #0070d2;
    font-weight: bold;
}

.progress {
    margin-bottom: 20px;
}

.population {
    margin-bottom: 20px;
}

.history {
    background-color: #f8f9fa;
    padding: 15px;
    border-radius: 8px;
}

.chart {
    display: flex;
    align-items: flex-end;
    height: 100px;
    gap: 2px;
    margin-top: 10px;
}

.bar {
    flex: 1;
    background-color: #0070d2;
    border-radius: 4px 4px 0 0;
    text-align: center;
    position: relative;
    min-width: 10px;
}

.bar-value {
    position: absolute;
    bottom: -20px;
    left: 50%;
    transform: translateX(-50%);
    font-size: 10px;
    color: #333;
}

.bar:hover {
    opacity: 0.8;
}
```

## Key Features of this Implementation:

1. **Population Management**: Creates and maintains a population of binary chromosomes
2. **Fitness Calculation**: Measures how close each individual is to the target
3. **Selection**: Uses tournament selection to choose parents
4. **Crossover**: Implements single-point crossover
5. **Mutation**: Applies bit-flip mutation
6. **Elitism**: Preserves the best individuals across generations
7. **Visualization**: Shows real-time progress and statistics
8. **Controls**: Start, stop, and reset functionality

## How to Use:

1. Create a new Lightning Web Component with this code
2. Deploy to your Salesforce org
3. Add the component to a Lightning page
4. Click "Start" to begin the genetic algorithm
5. Watch as it evolves towards the target string "1111111111"

The algorithm will continue evolving until it finds the exact target string, demonstrating the power of genetic algorithms in optimization problems.

