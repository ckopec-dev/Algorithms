# Ford-Fulkerson Algorithm in Lightning Web Component

Here's a complete implementation of the Ford-Fulkerson algorithm in a Lightning Web Component:

## HTML Template (fordFulkerson.html)
```html
<template>
    <div class="slds-box slds-theme_default">
        <h2 class="slds-text-heading_small">Ford-Fulkerson Algorithm</h2>
        
        <div class="slds-grid slds-gutters slds-wrap">
            <div class="slds-col slds-size_1-of-2">
                <lightning-card title="Network Graph">
                    <div class="slds-grid slds-gutters slds-wrap">
                        <div class="slds-col slds-size_1-of-1">
                            <lightning-button 
                                label="Run Algorithm" 
                                onclick={runAlgorithm}
                                variant="brand"
                                class="slds-m-bottom_small">
                            </lightning-button>
                        </div>
                        <div class="slds-col slds-size_1-of-1">
                            <div class="graph-container" style="height: 300px; border: 1px solid #e1e1e1; padding: 10px;">
                                <canvas id="graphCanvas" width="400" height="280"></canvas>
                            </div>
                        </div>
                    </div>
                </lightning-card>
            </div>
            
            <div class="slds-col slds-size_1-of-2">
                <lightning-card title="Results">
                    <div class="slds-grid slds-gutters slds-wrap">
                        <div class="slds-col slds-size_1-of-1">
                            <lightning-output-field 
                                label="Maximum Flow" 
                                value={maxFlow}
                                variant="label-inline">
                            </lightning-output-field>
                        </div>
                        <div class="slds-col slds-size_1-of-1">
                            <lightning-output-field 
                                label="Augmenting Paths" 
                                value={augmentingPaths}
                                variant="label-inline">
                            </lightning-output-field>
                        </div>
                        <div class="slds-col slds-size_1-of-1">
                            <lightning-output-field 
                                label="Time Complexity" 
                                value="O(VE²)" 
                                variant="label-inline">
                            </lightning-output-field>
                        </div>
                    </div>
                </lightning-card>
                
                <lightning-card title="Step-by-Step Process">
                    <div class="slds-scrollable_x" style="max-height: 200px;">
                        <ul class="slds-list_dotted">
                            <template for:each={algorithmSteps} for:item="step">
                                <li key={step.index}>{step.description}</li>
                            </template>
                        </ul>
                    </div>
                </lightning-card>
            </div>
        </div>
    </div>
</template>
```

## JavaScript Controller (fordFulkerson.js)
```javascript
import { LightningElement, track } from 'lwc';

export default class FordFulkerson extends LightningElement {
    @track maxFlow = 0;
    @track augmentingPaths = 0;
    @track algorithmSteps = [];
    
    // Graph representation
    graph = [
        [0, 16, 13, 0, 0, 0],
        [0, 0, 10, 12, 0, 0],
        [0, 4, 0, 0, 14, 0],
        [0, 0, 9, 0, 0, 20],
        [0, 0, 0, 7, 0, 4],
        [0, 0, 0, 0, 0, 0]
    ];
    
    source = 0;
    sink = 5;
    
    // Node positions for visualization
    nodePositions = [
        {x: 100, y: 50, name: 'S'},
        {x: 200, y: 100, name: 'A'},
        {x: 300, y: 100, name: 'B'},
        {x: 200, y: 200, name: 'C'},
        {x: 300, y: 200, name: 'D'},
        {x: 200, y: 300, name: 'T'}
    ];
    
    connectedComponents = [
        [0, 1], [0, 2], [1, 2], [1, 3], [2, 4], [3, 5], [4, 5], [3, 4]
    ];
    
    constructor() {
        super();
        this.resetAlgorithm();
    }
    
    resetAlgorithm() {
        this.maxFlow = 0;
        this.augmentingPaths = 0;
        this.algorithmSteps = [];
        this.drawGraph();
    }
    
    runAlgorithm() {
        this.resetAlgorithm();
        this.algorithmSteps.push({index: 1, description: "Starting Ford-Fulkerson Algorithm"});
        
        // Create residual graph
        let residualGraph = this.cloneGraph(this.graph);
        let maxFlow = 0;
        let pathCount = 0;
        
        // Continue while there's an augmenting path
        let pathExists = true;
        while (pathExists) {
            // Find augmenting path using BFS
            let parent = this.bfs(residualGraph, this.source, this.sink);
            pathExists = (parent[this.sink] !== -1);
            
            if (pathExists) {
                pathCount++;
                this.algorithmSteps.push({index: pathCount + 1, description: `Finding augmenting path ${pathCount}`});
                
                // Find minimum capacity along the path
                let pathFlow = Infinity;
                let current = this.sink;
                let path = [];
                
                while (current !== this.source) {
                    path.unshift(current);
                    let prev = parent[current];
                    pathFlow = Math.min(pathFlow, residualGraph[prev][current]);
                    current = prev;
                }
                path.unshift(this.source);
                
                this.algorithmSteps.push({index: pathCount + 1, description: `Path found: ${path.join(' → ')} with capacity ${pathFlow}`});
                
                // Update residual capacities
                current = this.sink;
                while (current !== this.source) {
                    let prev = parent[current];
                    residualGraph[prev][current] -= pathFlow;
                    residualGraph[current][prev] += pathFlow;
                    current = prev;
                }
                
                maxFlow += pathFlow;
                this.algorithmSteps.push({index: pathCount + 1, description: `Flow added: ${pathFlow}, Total flow: ${maxFlow}`});
            }
        }
        
        this.maxFlow = maxFlow;
        this.augmentingPaths = pathCount;
        this.algorithmSteps.push({index: pathCount + 2, description: `Algorithm completed. Maximum flow: ${maxFlow}`});
        this.drawGraph();
    }
    
    bfs(residualGraph, source, sink) {
        let visited = new Array(residualGraph.length).fill(false);
        let parent = new Array(residualGraph.length).fill(-1);
        let queue = [];
        
        queue.push(source);
        visited[source] = true;
        
        while (queue.length > 0) {
            let u = queue.shift();
            
            for (let v = 0; v < residualGraph.length; v++) {
                if (!visited[v] && residualGraph[u][v] > 0) {
                    visited[v] = true;
                    parent[v] = u;
                    queue.push(v);
                    
                    if (v === sink) {
                        return parent;
                    }
                }
            }
        }
        
        return parent;
    }
    
    cloneGraph(graph) {
        return graph.map(row => [...row]);
    }
    
    drawGraph() {
        const canvas = this.template.querySelector('#graphCanvas');
        if (!canvas) return;
        
        const ctx = canvas.getContext('2d');
        ctx.clearRect(0, 0, canvas.width, canvas.height);
        
        // Draw edges
        ctx.strokeStyle = '#0070d2';
        ctx.lineWidth = 2;
        
        for (let i = 0; i < this.connectedComponents.length; i++) {
            const [u, v] = this.connectedComponents[i];
            const node1 = this.nodePositions[u];
            const node2 = this.nodePositions[v];
            
            ctx.beginPath();
            ctx.moveTo(node1.x, node1.y);
            ctx.lineTo(node2.x, node2.y);
            ctx.stroke();
            
            // Draw edge weight
            const midX = (node1.x + node2.x) / 2;
            const midY = (node1.y + node2.y) / 2;
            const weight = this.graph[u][v];
            
            if (weight > 0) {
                ctx.fillStyle = '#000000';
                ctx.font = '12px Arial';
                ctx.fillText(weight.toString(), midX, midY - 5);
            }
        }
        
        // Draw nodes
        ctx.fillStyle = '#0070d2';
        ctx.strokeStyle = '#000000';
        ctx.lineWidth = 1;
        
        for (let i = 0; i < this.nodePositions.length; i++) {
            const node = this.nodePositions[i];
            ctx.beginPath();
            ctx.arc(node.x, node.y, 20, 0, 2 * Math.PI);
            ctx.fill();
            ctx.stroke();
            
            ctx.fillStyle = '#ffffff';
            ctx.font = 'bold 14px Arial';
            ctx.textAlign = 'center';
            ctx.fillText(node.name, node.x, node.y + 5);
        }
        
        // Highlight source and sink
        const sourceNode = this.nodePositions[this.source];
        const sinkNode = this.nodePositions[this.sink];
        
        ctx.fillStyle = '#48a44b';
        ctx.beginPath();
        ctx.arc(sourceNode.x, sourceNode.y, 25, 0, 2 * Math.PI);
        ctx.fill();
        ctx.stroke();
        
        ctx.fillStyle = '#e65100';
        ctx.beginPath();
        ctx.arc(sinkNode.x, sinkNode.y, 25, 0, 2 * Math.PI);
        ctx.fill();
        ctx.stroke();
    }
}
```

## CSS Styles (fordFulkerson.css)
```css
.graph-container {
    position: relative;
    width: 100%;
    height: 100%;
}

.graph-container canvas {
    background-color: #f8f8f8;
    border-radius: 4px;
}

.slds-card__body {
    padding: 1rem;
}

.slds-output-field__label {
    font-weight: 600;
}
```

## Usage Example

This LWC implements the Ford-Fulkerson algorithm to find the maximum flow in a flow network. The component includes:

1. **Visual Graph Representation**: Shows nodes (S, A, B, C, D, T) and edges with capacities
2. **Algorithm Execution**: Runs the BFS-based augmenting path algorithm
3. **Step-by-Step Visualization**: Shows each step of the algorithm
4. **Results Display**: Shows maximum flow and number of augmenting paths found

## Key Features

- **Breadth-First Search (BFS)** for finding augmenting paths
- **Residual Graph** implementation
- **Visual Graph Rendering** using Canvas API
- **Step-by-step Execution** with detailed logging
- **Responsive Design** using Lightning Design System

The algorithm finds the maximum flow from source (S) to sink (T) in the given flow network, demonstrating the classic Ford-Fulkerson method with BFS for path finding.

