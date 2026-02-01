# Tabu Search Algorithm in Lightning Web Component

```javascript
// tabu-search.js
import { LightningElement } from 'lwc';

export default class TabuSearch extends LightningElement {
    // Algorithm parameters
    tabuList = [];
    bestSolution = null;
    bestFitness = Infinity;
    maxIterations = 1000;
    tabuTenure = 5;
    currentSolution = [];
    iteration = 0;
    isRunning = false;

    // Initialize random solution
    initializeSolution() {
        const solution = [];
        for (let i = 0; i < 10; i++) {
            solution.push(Math.floor(Math.random() * 100));
        }
        return solution;
    }

    // Calculate fitness (example: sum of squares)
    calculateFitness(solution) {
        return solution.reduce((sum, value) => sum + value * value, 0);
    }

    // Generate neighbors by perturbing current solution
    generateNeighbors(solution) {
        const neighbors = [];
        for (let i = 0; i < solution.length; i++) {
            // Create neighbor by changing one element
            const neighbor = [...solution];
            neighbor[i] = (neighbor[i] + 1) % 100; // Simple perturbation
            neighbors.push(neighbor);
        }
        return neighbors;
    }

    // Check if solution is in tabu list
    isTabu(solution) {
        return this.tabuList.some(tabuSolution => 
            JSON.stringify(tabuSolution) === JSON.stringify(solution)
        );
    }

    // Add solution to tabu list
    addToTabuList(solution) {
        this.tabuList.push([...solution]);
        if (this.tabuList.length > this.tabuTenure) {
            this.tabuList.shift();
        }
    }

    // Tabu search main algorithm
    tabuSearch() {
        if (this.isRunning) return;
        
        this.isRunning = true;
        this.iteration = 0;
        
        // Initialize
        this.currentSolution = this.initializeSolution();
        this.bestSolution = [...this.currentSolution];
        this.bestFitness = this.calculateFitness(this.currentSolution);
        this.tabuList = [];
        
        console.log('Starting Tabu Search...');
        
        // Main loop
        const searchInterval = setInterval(() => {
            if (this.iteration >= this.maxIterations || !this.isRunning) {
                clearInterval(searchInterval);
                this.isRunning = false;
                console.log('Tabu Search completed');
                console.log('Best solution:', this.bestSolution);
                console.log('Best fitness:', this.bestFitness);
                return;
            }
            
            // Generate neighbors
            const neighbors = this.generateNeighbors(this.currentSolution);
            
            // Find best non-tabu neighbor
            let bestNeighbor = null;
            let bestNeighborFitness = Infinity;
            
            for (const neighbor of neighbors) {
                if (!this.isTabu(neighbor)) {
                    const fitness = this.calculateFitness(neighbor);
                    if (fitness < bestNeighborFitness) {
                        bestNeighbor = neighbor;
                        bestNeighborFitness = fitness;
                    }
                }
            }
            
            // If no non-tabu neighbor found, pick best neighbor (even if tabu)
            if (bestNeighbor === null) {
                for (const neighbor of neighbors) {
                    const fitness = this.calculateFitness(neighbor);
                    if (fitness < bestNeighborFitness) {
                        bestNeighbor = neighbor;
                        bestNeighborFitness = fitness;
                    }
                }
            }
            
            // Update current solution
            this.currentSolution = [...bestNeighbor];
            
            // Update best solution if improved
            if (bestNeighborFitness < this.bestFitness) {
                this.bestSolution = [...bestNeighbor];
                this.bestFitness = bestNeighborFitness;
            }
            
            // Add current solution to tabu list
            this.addToTabuList(this.currentSolution);
            
            this.iteration++;
            
            // Update UI
            this.updateProgress();
            
        }, 10); // Update every 10ms
    }

    // Update progress in UI
    updateProgress() {
        // In a real implementation, this would update LWC UI elements
        console.log(`Iteration: ${this.iteration}, Best Fitness: ${this.bestFitness}`);
    }

    // Start the algorithm
    handleStartSearch() {
        this.tabuSearch();
    }

    // Stop the algorithm
    handleStopSearch() {
        this.isRunning = false;
    }

    // Reset the algorithm
    handleReset() {
        this.isRunning = false;
        this.currentSolution = [];
        this.bestSolution = null;
        this.bestFitness = Infinity;
        this.iteration = 0;
        this.tabuList = [];
    }
}
```

```html
<!-- tabu-search.html -->
<template>
    <div class="tabu-search-container">
        <h2>Tabu Search Algorithm</h2>
        
        <div class="controls">
            <lightning-button 
                label="Start Search" 
                variant="brand" 
                onclick={handleStartSearch}
                disabled={isRunning}>
            </lightning-button>
            
            <lightning-button 
                label="Stop Search" 
                variant="destructive" 
                onclick={handleStopSearch}
                disabled={!isRunning}>
            </lightning-button>
            
            <lightning-button 
                label="Reset" 
                variant="neutral" 
                onclick={handleReset}>
            </lightning-button>
        </div>

        <div class="status">
            <p>Iteration: {iteration}</p>
            <p>Best Fitness: {bestFitness}</p>
            <p>Running: {isRunning ? 'Yes' : 'No'}</p>
        </div>

        <div class="solution">
            <h3>Best Solution:</h3>
            <p>{bestSolution}</p>
        </div>

        <div class="tabu-list">
            <h3>Tabu List (last 5 entries):</h3>
            <p>{tabuList}</p>
        </div>
    </div>
</template>
```

```css
/* tabu-search.css */
.tabu-search-container {
    padding: 20px;
    max-width: 800px;
    margin: 0 auto;
}

.controls {
    margin-bottom: 20px;
    display: flex;
    gap: 10px;
    flex-wrap: wrap;
}

.status {
    background-color: #f5f5f5;
    padding: 15px;
    border-radius: 5px;
    margin-bottom: 20px;
}

.solution, .tabu-list {
    margin-bottom: 15px;
}

.solution p, .tabu-list p {
    font-family: monospace;
    background-color: #e8f4f8;
    padding: 10px;
    border-radius: 3px;
    margin: 5px 0;
}
```

```javascript
// tabu-search.js (with enhanced features)
import { LightningElement, track } from 'lwc';

export default class TabuSearch extends LightningElement {
    @track currentSolution = [];
    @track bestSolution = [];
    @track tabuList = [];
    @track bestFitness = Infinity;
    @track iteration = 0;
    @track isRunning = false;
    @track maxIterations = 1000;
    @track tabuTenure = 5;

    // Enhanced tabu search with better neighbor generation
    tabuSearch() {
        if (this.isRunning) return;
        
        this.isRunning = true;
        this.iteration = 0;
        this.bestFitness = Infinity;
        this.currentSolution = this.initializeSolution();
        this.bestSolution = [...this.currentSolution];
        this.tabuList = [];
        
        const searchInterval = setInterval(() => {
            if (this.iteration >= this.maxIterations || !this.isRunning) {
                clearInterval(searchInterval);
                this.isRunning = false;
                this.showResults();
                return;
            }
            
            // Generate multiple neighbors for better exploration
            const neighbors = this.generateEnhancedNeighbors(this.currentSolution);
            
            // Find best non-tabu neighbor
            let bestNeighbor = null;
            let bestNeighborFitness = Infinity;
            
            for (const neighbor of neighbors) {
                if (!this.isTabu(neighbor)) {
                    const fitness = this.calculateFitness(neighbor);
                    if (fitness < bestNeighborFitness) {
                        bestNeighbor = neighbor;
                        bestNeighborFitness = fitness;
                    }
                }
            }
            
            // Accept tabu neighbor if it's better than current best
            if (bestNeighbor === null && this.bestFitness < Infinity) {
                for (const neighbor of neighbors) {
                    const fitness = this.calculateFitness(neighbor);
                    if (fitness < bestNeighborFitness) {
                        bestNeighbor = neighbor;
                        bestNeighborFitness = fitness;
                    }
                }
            }
            
            // Update current solution
            this.currentSolution = [...bestNeighbor];
            
            // Update best solution
            if (bestNeighborFitness < this.bestFitness) {
                this.bestSolution = [...bestNeighbor];
                this.bestFitness = bestNeighborFitness;
            }
            
            // Add to tabu list
            this.addToTabuList(this.currentSolution);
            
            this.iteration++;
            
        }, 5); // Faster updates for better visualization
    }

    // Enhanced neighbor generation
    generateEnhancedNeighbors(solution) {
        const neighbors = [];
        const numNeighbors = 10; // Generate 10 neighbors
        
        for (let i = 0; i < numNeighbors; i++) {
            const neighbor = [...solution];
            
            // Randomly modify 1-3 elements
            const numModifications = Math.floor(Math.random() * 3) + 1;
            for (let j = 0; j < numModifications; j++) {
                const index = Math.floor(Math.random() * neighbor.length);
                neighbor[index] = (neighbor[index] + Math.floor(Math.random() * 20) - 10) % 100;
                neighbor[index] = Math.max(0, neighbor[index]); // Ensure non-negative
            }
            
            neighbors.push(neighbor);
        }
        
        return neighbors;
    }

    // Show results in console
    showResults() {
        console.log('=== Tabu Search Results ===');
        console.log('Best Solution:', this.bestSolution);
        console.log('Best Fitness:', this.bestFitness);
        console.log('Iterations:', this.iteration);
        console.log('Tabu List Size:', this.tabuList.length);
    }

    // Event handlers
    handleStartSearch() {
        this.tabuSearch();
    }

    handleStopSearch() {
        this.isRunning = false;
    }

    handleReset() {
        this.isRunning = false;
        this.currentSolution = [];
        this.bestSolution = [];
        this.tabuList = [];
        this.bestFitness = Infinity;
        this.iteration = 0;
    }

    // Initialize solution
    initializeSolution() {
        const solution = [];
        for (let i = 0; i < 10; i++) {
            solution.push(Math.floor(Math.random() * 50));
        }
        return solution;
    }

    // Fitness function
    calculateFitness(solution) {
        // Example: minimize sum of squares
        return solution.reduce((sum, value) => sum + value * value, 0);
    }

    // Check tabu status
    isTabu(solution) {
        return this.tabuList.some(tabuSolution => 
            JSON.stringify(tabuSolution) === JSON.stringify(solution)
        );
    }

    // Add to tabu list
    addToTabuList(solution) {
        this.tabuList.push([...solution]);
        if (this.tabuList.length > this.tabuTenure) {
            this.tabuList.shift();
        }
    }
}
```

## Key Features of this Tabu Search Implementation:

1. **Core Algorithm Components**:
   - Tabu list management
   - Solution generation and neighbor exploration
   - Fitness evaluation
   - Tabu tenure handling

2. **LWC Integration**:
   - Reactive properties using `@track`
   - Event handling for UI controls
   - Real-time progress updates

3. **Enhanced Features**:
   - Multiple neighbor generation
   - Flexible tabu tenure
   - Iteration tracking
   - Stop/Start/Reset functionality

4. **Visualization**:
   - Progress tracking
   - Best solution display
   - Tabu list monitoring

This implementation demonstrates how to translate a classic optimization algorithm into a Lightning Web Component while maintaining the core tabu search principles of memory-based avoidance of previously visited solutions.

