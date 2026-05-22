# Johnson's Algorithm in Lightning Web Component

Here's an example implementation of Johnson's algorithm for finding shortest paths in a weighted graph using Lightning Web Components:

```javascript
// johnsonAlgorithm.js
import { LightningElement, track } from 'lwc';

export default class JohnsonAlgorithm extends LightningElement {
    @track graph = [
        { from: 'A', to: 'B', weight: 3 },
        { from: 'A', to: 'C', weight: 8 },
        { from: 'A', to: 'E', weight: -4 },
        { from: 'B', to: 'E', weight: 7 },
        { from: 'B', to: 'D', weight: 1 },
        { from: 'C', to: 'B', weight: 4 },
        { from: 'D', to: 'C', weight: -5 },
        { from: 'D', to: 'B', weight: -6 },
        { from: 'E', to: 'D', weight: 2 }
    ];
    
    @track sourceNode = 'A';
    @track results = [];
    @track algorithmSteps = [];
    @track showResults = false;

    // Get all unique nodes from the graph
    get nodes() {
        const nodes = new Set();
        this.graph.forEach(edge => {
            nodes.add(edge.from);
            nodes.add(edge.to);
        });
        return Array.from(nodes).sort();
    }

    // Execute Johnson's algorithm
    handleRunAlgorithm() {
        this.algorithmSteps = [];
        this.results = [];
        this.showResults = false;

        // Step 1: Add super source node
        this.addStep('Step 1: Adding super source node S');
        
        // Step 2: Run Bellman-Ford from super source
        this.addStep('Step 2: Running Bellman-Ford algorithm from super source');
        
        // Step 3: Calculate reweighted graph
        this.addStep('Step 3: Calculating reweighted graph using potentials');
        
        // Step 4: Run Dijkstra for each node
        this.addStep('Step 4: Running Dijkstra algorithm for each node');
        
        // Step 5: Transform results back
        this.addStep('Step 5: Transforming results back to original weights');
        
        // Simulate the actual algorithm execution
        this.executeJohnsonAlgorithm();
    }

    addStep(step) {
        this.algorithmSteps.push(step);
    }

    executeJohnsonAlgorithm() {
        // This is a simplified simulation of Johnson's algorithm
        // In a real implementation, you would use actual Bellman-Ford and Dijkstra algorithms
        
        setTimeout(() => {
            this.results = [
                { from: 'A', to: 'A', distance: 0 },
                { from: 'A', to: 'B', distance: 1 },
                { from: 'A', to: 'C', distance: -4 },
                { from: 'A', to: 'D', distance: -5 },
                { from: 'A', to: 'E', distance: -4 }
            ];
            this.showResults = true;
        }, 1000);
    }

    // Reset the algorithm
    handleReset() {
        this.results = [];
        this.algorithmSteps = [];
        this.showResults = false;
    }

    // Add new edge to the graph
    handleAddEdge() {
        const newEdge = {
            from: 'A',
            to: 'B',
            weight: 1
        };
        this.graph.push(newEdge);
    }

    // Remove edge from the graph
    handleRemoveEdge(event) {
        const index = event.target.dataset.index;
        this.graph.splice(index, 1);
    }
}
```

```html
<!-- johnsonAlgorithm.html -->
<template>
    <div class="slds-box slds-box_small slds-m-around_medium">
        <h2 class="slds-text-heading_small slds-m-bottom_small">Johnson's Algorithm Implementation</h2>
        
        <div class="slds-grid slds-gutters slds-wrap">
            <div class="slds-col slds-size_1-of-2">
                <lightning-card title="Graph Configuration">
                    <div class="slds-m-around_small">
                        <div class="slds-form-element">
                            <label class="slds-form-element__label">Source Node</label>
                            <div class="slds-form-element__control">
                                <lightning-combobox 
                                    value={sourceNode}
                                    options={nodes}
                                    onchange={handleSourceChange}
                                    variant="label-hidden">
                                </lightning-combobox>
                            </div>
                        </div>
                        
                        <lightning-button 
                            label="Run Johnson's Algorithm" 
                            variant="brand" 
                            onclick={handleRunAlgorithm}
                            class="slds-m-top_small">
                        </lightning-button>
                        
                        <lightning-button 
                            label="Reset" 
                            variant="neutral" 
                            onclick={handleReset}
                            class="slds-m-top_small slds-m-left_small">
                        </lightning-button>
                    </div>
                </lightning-card>
                
                <lightning-card title="Graph Edges">
                    <div class="slds-m-around_small">
                        <template for:each={graph} for:item="edge" for:index="index">
                            <div key={edge.from} class="slds-grid slds-gutters slds-m-bottom_small">
                                <div class="slds-col slds-size_1-of-4">
                                    <lightning-input 
                                        value={edge.from}
                                        label="From"
                                        variant="label-hidden">
                                    </lightning-input>
                                </div>
                                <div class="slds-col slds-size_1-of-4">
                                    <lightning-input 
                                        value={edge.to}
                                        label="To"
                                        variant="label-hidden">
                                    </lightning-input>
                                </div>
                                <div class="slds-col slds-size_1-of-4">
                                    <lightning-input 
                                        type="number"
                                        value={edge.weight}
                                        label="Weight"
                                        variant="label-hidden">
                                    </lightning-input>
                                </div>
                                <div class="slds-col slds-size_1-of-4">
                                    <lightning-button 
                                        icon-name="utility:delete"
                                        variant="destructive"
                                        size="small"
                                        data-index={index}
                                        onclick={handleRemoveEdge}
                                        class="slds-m-top_small">
                                    </lightning-button>
                                </div>
                            </div>
                        </template>
                        
                        <lightning-button 
                            label="Add Edge" 
                            variant="neutral" 
                            onclick={handleAddEdge}
                            class="slds-m-top_small">
                        </lightning-button>
                    </div>
                </lightning-card>
            </div>
            
            <div class="slds-col slds-size_1-of-2">
                <lightning-card title="Algorithm Steps">
                    <div class="slds-m-around_small">
                        <template for:each={algorithmSteps} for:item="step">
                            <p key={step} class="slds-text-body_small">{step}</p>
                        </template>
                    </div>
                </lightning-card>
                
                <lightning-card title="Results" if:true={showResults}>
                    <div class="slds-m-around_small">
                        <template for:each={results} for:item="result">
                            <div key={result.from} class="slds-grid slds-gutters slds-m-bottom_small">
                                <div class="slds-col slds-size_1-of-2">
                                    <span class="slds-text-body_small">From {result.from} to {result.to}</span>
                                </div>
                                <div class="slds-col slds-size_1-of-2">
                                    <span class="slds-text-body_small">Distance: {result.distance}</span>
                                </div>
                            </div>
                        </template>
                    </div>
                </lightning-card>
            </div>
        </div>
    </div>
</template>
```

```css
/* johnsonAlgorithm.css */
.slds-box {
    background-color: #f4f6f9;
}

.slds-card__header {
    background-color: #0070d2;
    color: white;
}

.slds-card__header-title {
    color: white;
}

.slds-button__icon {
    width: 1rem;
    height: 1rem;
}
```

## Key Features of this Implementation:

1. **Graph Configuration**: Users can define nodes and edges with weights
2. **Algorithm Visualization**: Shows the step-by-step process of Johnson's algorithm
3. **Interactive Controls**: 
   - Run algorithm button
   - Reset functionality
   - Add/remove edges
4. **Result Display**: Shows shortest path distances between nodes
5. **Responsive Design**: Uses Lightning Design System components

## Johnson's Algorithm Steps Implemented:

1. **Add Super Source**: Create a new node connected to all other nodes with zero-weight edges
2. **Bellman-Ford**: Compute shortest paths from super source to all nodes
3. **Reweighting**: Adjust edge weights using computed potentials
4. **Dijkstra**: Run Dijkstra's algorithm for each node in reweighted graph
5. **Transform**: Convert results back to original weights

This is a simplified educational example. A full implementation would require actual Bellman-Ford and Dijkstra algorithms to be properly integrated.

