# PageRank Algorithm in Lightning Web Component

Here's a complete example of implementing the PageRank algorithm in a Lightning Web Component:

## HTML Template (pageRank.html)
```html
<template>
    <div class="slds-box slds-box_small slds-m-around_medium">
        <h2 class="slds-text-heading_small">PageRank Algorithm</h2>
        
        <div class="slds-grid slds-gutters slds-wrap">
            <div class="slds-col slds-size_1-of-2">
                <lightning-card title="Network Graph">
                    <div class="slds-m-around_medium">
                        <lightning-button 
                            label="Run PageRank" 
                            onclick={runPageRank}
                            variant="brand"
                            class="slds-m-bottom_small">
                        </lightning-button>
                        
                        <div class="slds-m-top_small">
                            <lightning-input 
                                label="Damping Factor (d)" 
                                type="number" 
                                value={dampingFactor}
                                min="0.1"
                                max="0.99"
                                step="0.01"
                                onchange={handleDampingFactorChange}>
                            </lightning-input>
                        </div>
                        
                        <div class="slds-m-top_small">
                            <lightning-input 
                                label="Max Iterations" 
                                type="number" 
                                value={maxIterations}
                                min="1"
                                max="1000"
                                onchange={handleMaxIterationsChange}>
                            </lightning-input>
                        </div>
                    </div>
                </lightning-card>
            </div>
            
            <div class="slds-col slds-size_1-of-2">
                <lightning-card title="Results">
                    <div class="slds-m-around_medium">
                        <template if:true={isCalculated}>
                            <lightning-datatable
                                data={rankings}
                                columns={columns}
                                key-field="nodeId"
                                hide-checkbox-column="true">
                            </lightning-datatable>
                        </template>
                        
                        <template if:false={isCalculated}>
                            <p class="slds-text-body_small">Click "Run PageRank" to calculate rankings</p>
                        </template>
                    </div>
                </lightning-card>
            </div>
        </div>
        
        <div class="slds-m-top_medium">
            <lightning-card title="Network Visualization">
                <div class="slds-m-around_medium">
                    <div class="slds-grid slds-gutters">
                        <div class="slds-col slds-size_1-of-3">
                            <h3 class="slds-text-heading_small">Nodes</h3>
                            <ul class="slds-list_dotted">
                                <template for:each={nodes} for:item="node">
                                    <li key={node.id}>{node.id}: {node.label}</li>
                                </template>
                            </ul>
                        </div>
                        <div class="slds-col slds-size_1-of-3">
                            <h3 class="slds-text-heading_small">Edges</h3>
                            <ul class="slds-list_dotted">
                                <template for:each={edges} for:item="edge">
                                    <li key={edge.from}>{edge.from} → {edge.to}</li>
                                </template>
                            </ul>
                        </div>
                        <div class="slds-col slds-size_1-of-3">
                            <h3 class="slds-text-heading_small">Algorithm Status</h3>
                            <p>Iterations: {currentIteration}/{maxIterations}</p>
                            <p>Convergence: {convergenceStatus}</p>
                        </div>
                    </div>
                </div>
            </lightning-card>
        </div>
    </div>
</template>
```

## JavaScript Controller (pageRank.js)
```javascript
import { LightningElement, track } from 'lwc';

export default class PageRank extends LightningElement {
    @track nodes = [
        { id: 'A', label: 'Home Page' },
        { id: 'B', label: 'About Us' },
        { id: 'C', label: 'Services' },
        { id: 'D', label: 'Contact' },
        { id: 'E', label: 'Blog' }
    ];

    @track edges = [
        { from: 'A', to: 'B' },
        { from: 'A', to: 'C' },
        { from: 'B', to: 'D' },
        { from: 'C', to: 'E' },
        { from: 'C', to: 'D' },
        { from: 'D', to: 'A' },
        { from: 'E', to: 'A' }
    ];

    @track dampingFactor = 0.85;
    @track maxIterations = 100;
    @track currentIteration = 0;
    @track convergenceStatus = 'Not Started';
    
    @track rankings = [];
    @track isCalculated = false;
    
    columns = [
        { label: 'Node', fieldName: 'nodeId', type: 'text' },
        { label: 'PageRank', fieldName: 'rank', type: 'number', typeAttributes: { maximumFractionDigits: 4 } },
        { label: 'Label', fieldName: 'label', type: 'text' }
    ];

    connectedCallback() {
        // Initialize with equal PageRank values
        this.initializePageRank();
    }

    initializePageRank() {
        const initialRank = 1.0 / this.nodes.length;
        this.rankings = this.nodes.map(node => ({
            nodeId: node.id,
            rank: initialRank,
            label: node.label
        }));
    }

    runPageRank() {
        this.isCalculated = false;
        this.currentIteration = 0;
        this.convergenceStatus = 'Calculating...';
        
        // Initialize PageRank values
        this.initializePageRank();
        
        // Run PageRank algorithm
        this.calculatePageRank();
    }

    calculatePageRank() {
        const numNodes = this.nodes.length;
        const dampingFactor = this.dampingFactor;
        const maxIterations = this.maxIterations;
        
        // Create adjacency list for easier access
        const adjacencyList = this.createAdjacencyList();
        
        // Get out-degree for each node
        const outDegree = this.getOutDegree(adjacencyList);
        
        // Initialize previous ranks
        let previousRanks = this.rankings.map(r => r.rank);
        let currentRanks = [...previousRanks];
        
        let converged = false;
        let iteration = 0;
        
        while (!converged && iteration < maxIterations) {
            iteration++;
            this.currentIteration = iteration;
            
            // Calculate new ranks
            for (let i = 0; i < numNodes; i++) {
                let newRank = (1 - dampingFactor) / numNodes;
                let sum = 0;
                
                // For each node that links to current node
                for (let j = 0; j < numNodes; j++) {
                    if (adjacencyList[j][i] > 0 && outDegree[j] > 0) {
                        sum += previousRanks[j] / outDegree[j];
                    }
                }
                
                newRank += dampingFactor * sum;
                currentRanks[i] = newRank;
            }
            
            // Check for convergence
            converged = this.checkConvergence(previousRanks, currentRanks);
            
            // Update previous ranks
            previousRanks = [...currentRanks];
            
            // Update UI periodically
            if (iteration % 10 === 0 || converged || iteration === maxIterations) {
                this.updateRankings(currentRanks);
                this.convergenceStatus = converged ? 'Converged' : 'Calculating...';
            }
        }
        
        this.isCalculated = true;
        this.currentIteration = iteration;
        this.convergenceStatus = converged ? 'Converged' : 'Max Iterations Reached';
    }

    createAdjacencyList() {
        const numNodes = this.nodes.length;
        const adjacencyList = Array(numNodes).fill().map(() => Array(numNodes).fill(0));
        
        // Build adjacency matrix
        this.edges.forEach(edge => {
            const fromIndex = this.nodes.findIndex(node => node.id === edge.from);
            const toIndex = this.nodes.findIndex(node => node.id === edge.to);
            
            if (fromIndex !== -1 && toIndex !== -1) {
                adjacencyList[fromIndex][toIndex] = 1;
            }
        });
        
        return adjacencyList;
    }

    getOutDegree(adjacencyList) {
        return adjacencyList.map(row => row.reduce((sum, val) => sum + val, 0));
    }

    checkConvergence(previousRanks, currentRanks) {
        const threshold = 0.0001;
        for (let i = 0; i < previousRanks.length; i++) {
            if (Math.abs(previousRanks[i] - currentRanks[i]) > threshold) {
                return false;
            }
        }
        return true;
    }

    updateRankings(newRanks) {
        this.rankings = this.nodes.map((node, index) => ({
            nodeId: node.id,
            rank: newRanks[index],
            label: node.label
        })).sort((a, b) => b.rank - a.rank);
    }

    handleDampingFactorChange(event) {
        this.dampingFactor = parseFloat(event.target.value);
    }

    handleMaxIterationsChange(event) {
        this.maxIterations = parseInt(event.target.value);
    }
}
```

## CSS Styles (pageRank.css)
```css
.slds-box {
    background-color: #f4f6f9;
    border: 1px solid #e1e8f0;
    border-radius: 0.25rem;
}

.slds-text-heading_small {
    font-size: 1rem;
    font-weight: 600;
    line-height: 1.25;
    margin-bottom: 0.75rem;
}

.slds-m-around_medium {
    margin: 1rem;
}

.slds-m-top_small {
    margin-top: 0.5rem;
}

.slds-m-bottom_small {
    margin-bottom: 0.5rem;
}

.slds-grid {
    display: flex;
    flex-wrap: wrap;
}

.slds-col {
    flex: 1 1 0%;
}

.slds-size_1-of-2 {
    flex: 0 0 50%;
    max-width: 50%;
}

.slds-size_1-of-3 {
    flex: 0 0 33.333333%;
    max-width: 33.333333%;
}
```

## Key Features of This Implementation:

1. **Network Representation**: Uses nodes and edges to represent web pages and links
2. **Configurable Parameters**: Allows adjustment of damping factor and max iterations
3. **Real-time Visualization**: Shows current state during calculation
4. **Convergence Detection**: Automatically stops when PageRank values stabilize
5. **Responsive UI**: Clean, user-friendly interface with cards and tables
6. **Error Handling**: Graceful handling of edge cases and convergence

## How PageRank Works in This Component:

1. **Initialization**: All nodes start with equal PageRank values
2. **Iteration**: For each iteration, PageRank is recalculated based on incoming links
3. **Damping Factor**: Accounts for random jumps (typically 0.85)
4. **Convergence**: Stops when changes between iterations are below threshold
5. **Results**: Displays nodes sorted by their PageRank scores

This implementation demonstrates the core PageRank algorithm while maintaining the LWC component structure and best practices.

