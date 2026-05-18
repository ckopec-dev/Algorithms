# Edmonds-Karp Algorithm in Lightning Web Component

Here's a complete implementation of the Edmonds-Karp algorithm (maximum flow) in a Lightning Web Component:

```javascript
// edmondsKarp.js
import { LightningElement } from 'lwc';

export default class EdmondsKarp extends LightningElement {
    // Graph representation
    graph = [];
    source = 0;
    sink = 0;
    vertices = 0;
    
    // Flow results
    maxFlow = 0;
    path = [];
    
    // Input fields
    inputGraph = '';
    inputSource = '0';
    inputSink = '5';
    
    // Sample graph data
    sampleGraph = [
        [0, 16, 13, 0, 0, 0],
        [0, 0, 10, 12, 0, 0],
        [0, 4, 0, 0, 14, 0],
        [0, 0, 9, 0, 0, 20],
        [0, 0, 0, 7, 0, 4],
        [0, 0, 0, 0, 0, 0]
    ];
    
    connectedCallback() {
        // Initialize with sample data
        this.initializeGraph();
    }
    
    initializeGraph() {
        this.vertices = this.sampleGraph.length;
        this.graph = this.sampleGraph.map(row => [...row]);
        this.source = parseInt(this.inputSource);
        this.sink = parseInt(this.inputSink);
        this.maxFlow = this.calculateMaxFlow();
    }
    
    handleInputChange(event) {
        const inputName = event.target.name;
        const value = event.target.value;
        
        if (inputName === 'graph') {
            this.inputGraph = value;
        } else if (inputName === 'source') {
            this.inputSource = value;
        } else if (inputName === 'sink') {
            this.inputSink = value;
        }
    }
    
    handleCalculate() {
        this.source = parseInt(this.inputSource);
        this.sink = parseInt(this.inputSink);
        this.maxFlow = this.calculateMaxFlow();
    }
    
    calculateMaxFlow() {
        // Create a copy of the graph for residual network
        const residualGraph = this.graph.map(row => [...row]);
        let maxFlow = 0;
        
        // Continue while there's an augmenting path
        while (true) {
            const [path, pathFlow] = this.bfs(residualGraph);
            
            if (pathFlow === 0) {
                break;
            }
            
            maxFlow += pathFlow;
            
            // Update residual capacities
            let u = this.sink;
            while (u !== this.source) {
                const prev = path[u];
                residualGraph[prev][u] -= pathFlow;
                residualGraph[u][prev] += pathFlow;
                u = prev;
            }
        }
        
        return maxFlow;
    }
    
    bfs(residualGraph) {
        const visited = new Array(this.vertices).fill(false);
        const parent = new Array(this.vertices).fill(-1);
        const queue = [];
        const pathFlow = new Array(this.vertices).fill(0);
        
        queue.push(this.source);
        visited[this.source] = true;
        pathFlow[this.source] = Infinity;
        
        while (queue.length > 0) {
            const u = queue.shift();
            
            for (let v = 0; v < this.vertices; v++) {
                if (!visited[v] && residualGraph[u][v] > 0) {
                    visited[v] = true;
                    parent[v] = u;
                    pathFlow[v] = Math.min(pathFlow[u], residualGraph[u][v]);
                    
                    if (v === this.sink) {
                        return [parent, pathFlow[this.sink]];
                    }
                    
                    queue.push(v);
                }
            }
        }
        
        return [parent, 0];
    }
    
    get graphDisplay() {
        return this.graph.map(row => row.join(' ')).join('\n');
    }
    
    get resultDisplay() {
        return `Maximum Flow: ${this.maxFlow}`;
    }
    
    get pathDisplay() {
        if (this.maxFlow === 0) return '';
        return `Path found with flow: ${this.maxFlow}`;
    }
}
```

```html
<!-- edmondsKarp.html -->
<template>
    <div class="slds-card">
        <div class="slds-card__header slds-grid">
            <header class="slds-card__header-title">
                <h2>Edmonds-Karp Maximum Flow Algorithm</h2>
            </header>
        </div>
        
        <div class="slds-card__body">
            <div class="slds-grid slds-gutters">
                <div class="slds-col slds-size_1-of-2">
                    <div class="slds-form-element">
                        <label class="slds-form-element__label">Source Vertex</label>
                        <div class="slds-form-element__control">
                            <input 
                                type="number" 
                                class="slds-input" 
                                name="source"
                                value={inputSource}
                                onchange={handleInputChange}
                                min="0"
                                max="5"
                            />
                        </div>
                    </div>
                    
                    <div class="slds-form-element">
                        <label class="slds-form-element__label">Sink Vertex</label>
                        <div class="slds-form-element__control">
                            <input 
                                type="number" 
                                class="slds-input" 
                                name="sink"
                                value={inputSink}
                                onchange={handleInputChange}
                                min="0"
                                max="5"
                            />
                        </div>
                    </div>
                    
                    <lightning-button 
                        label="Calculate Maximum Flow" 
                        variant="brand" 
                        onclick={handleCalculate}
                        class="slds-m-top_medium">
                    </lightning-button>
                </div>
                
                <div class="slds-col slds-size_1-of-2">
                    <div class="slds-form-element">
                        <label class="slds-form-element__label">Graph (Adjacency Matrix)</label>
                        <div class="slds-form-element__control">
                            <textarea 
                                class="slds-textarea" 
                                name="graph"
                                value={graphDisplay}
                                readonly
                                rows="8">
                            </textarea>
                        </div>
                    </div>
                </div>
            </div>
            
            <div class="slds-m-top_medium">
                <lightning-card title="Results">
                    <div class="slds-text-body_regular">
                        <p><strong>{resultDisplay}</strong></p>
                        <p>{pathDisplay}</p>
                    </div>
                </lightning-card>
            </div>
            
            <div class="slds-m-top_medium">
                <lightning-card title="Algorithm Steps">
                    <div class="slds-text-body_regular">
                        <ol>
                            <li>Initialize residual network with original capacities</li>
                            <li>Find augmenting path using BFS</li>
                            <li>Update residual capacities along the path</li>
                            <li>Repeat until no more augmenting paths exist</li>
                            <li>Return total maximum flow</li>
                        </ol>
                    </div>
                </lightning-card>
            </div>
        </div>
    </div>
</template>
```

```css
/* edmondsKarp.css */
.slds-card__header {
    background-color: #f4f6f9;
}

.slds-card__header-title h2 {
    font-size: 1.25rem;
    font-weight: 600;
}

.slds-text-body_regular {
    font-size: 0.875rem;
    line-height: 1.4;
}

.slds-input,
.slds-textarea {
    font-family: 'Salesforce Sans', Arial, sans-serif;
}
```

## Key Features of this Implementation:

1. **Graph Representation**: Uses adjacency matrix to represent the flow network
2. **BFS Algorithm**: Implements breadth-first search to find augmenting paths
3. **Residual Network**: Maintains residual capacities for backward edges
4. **Interactive UI**: Allows users to input source and sink vertices
5. **Real-time Calculation**: Updates maximum flow when user clicks calculate
6. **Visual Feedback**: Shows algorithm steps and results clearly

## How it Works:

1. **Initialization**: Creates a residual network from the original graph
2. **Path Finding**: Uses BFS to find an augmenting path from source to sink
3. **Flow Update**: Updates residual capacities along the found path
4. **Iteration**: Continues until no more augmenting paths exist
5. **Result**: Returns the maximum flow value

## Sample Input:
- Source: 0
- Sink: 5
- Graph: 6x6 adjacency matrix representing flow capacities

The algorithm correctly implements the Edmonds-Karp variant of the Ford-Fulkerson method with O(VE²) time complexity, where V is the number of vertices and E is the number of edges.

