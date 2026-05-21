# Push-Relabel Max-Flow Algorithm in Lightning Web Component

Here's a complete example of implementing the Push-Relabel max-flow algorithm in a Lightning Web Component:

```javascript
// pushRelabel.js
import { LightningElement } from 'lwc';

export default class PushRelabel extends LightningElement {
    // Graph representation
    graph = [];
    nodes = [];
    edges = [];
    source = 0;
    sink = 0;
    maxFlow = 0;

    // Initialize the component
    connectedCallback() {
        this.initializeGraph();
        this.calculateMaxFlow();
    }

    initializeGraph() {
        // Create a sample graph
        // Graph: s -> 0 -> 1 -> t
        //        s -> 2 -> 1
        //        0 -> 3 -> t
        //        2 -> 3 -> t
        //        1 -> 4 -> t
        
        this.nodes = [0, 1, 2, 3, 4, 5]; // 0=source, 5=sink
        this.source = 0;
        this.sink = 5;
        
        // Initialize adjacency matrix
        this.graph = [
            [0, 16, 13, 0, 0, 0],  // Node 0 (source)
            [0, 0, 10, 12, 0, 0],  // Node 1
            [0, 4, 0, 0, 14, 0],   // Node 2
            [0, 0, 9, 0, 0, 20],   // Node 3
            [0, 0, 0, 7, 0, 4],    // Node 4
            [0, 0, 0, 0, 0, 0]     // Node 5 (sink)
        ];
    }

    calculateMaxFlow() {
        const n = this.graph.length;
        const height = new Array(n).fill(0);
        const excess = new Array(n).fill(0);
        
        // Initialize height of source
        height[this.source] = n;
        
        // Initialize excess flow for all vertices except source
        for (let i = 0; i < n; i++) {
            if (i !== this.source) {
                excess[i] = 0;
            }
        }
        
        // Push flow from source to its neighbors
        for (let i = 0; i < n; i++) {
            if (this.graph[this.source][i] > 0) {
                excess[i] = this.graph[this.source][i];
                this.graph[this.source][i] = 0;
                this.graph[i][this.source] += excess[i];
            }
        }
        
        let i = 0;
        while (i < n) {
            if (excess[i] > 0 && i !== this.source && i !== this.sink) {
                let pushed = this.push(i, height, excess);
                if (pushed > 0) {
                    i = 0; // Reset to check from beginning
                } else {
                    // Relabel
                    height[i] = this.relabel(i, height);
                    i = 0; // Reset to check from beginning
                }
            } else {
                i++;
            }
        }
        
        // Calculate maximum flow
        this.maxFlow = excess[this.sink];
        
        // Display results
        this.displayResults();
    }

    push(u, height, excess) {
        const n = this.graph.length;
        for (let v = 0; v < n; v++) {
            if (this.graph[u][v] > 0 && height[u] > height[v]) {
                const pushed = Math.min(excess[u], this.graph[u][v]);
                if (pushed > 0) {
                    // Update residual capacities
                    this.graph[u][v] -= pushed;
                    this.graph[v][u] += pushed;
                    
                    // Update excess flow
                    excess[u] -= pushed;
                    excess[v] += pushed;
                    
                    return pushed;
                }
            }
        }
        return 0;
    }

    relabel(u, height) {
        const n = this.graph.length;
        let minHeight = Infinity;
        
        for (let v = 0; v < n; v++) {
            if (this.graph[u][v] > 0) {
                minHeight = Math.min(minHeight, height[v]);
            }
        }
        
        if (minHeight < Infinity) {
            return minHeight + 1;
        }
        return height[u];
    }

    displayResults() {
        console.log('Maximum Flow:', this.maxFlow);
        console.log('Final Residual Graph:');
        for (let i = 0; i < this.graph.length; i++) {
            console.log(this.graph[i].join(' '));
        }
    }

    get flowResult() {
        return `Maximum Flow: ${this.maxFlow}`;
    }

    get graphDisplay() {
        let result = '';
        this.graph.forEach((row, index) => {
            result += `Node ${index}: ${row.join(' ')}\n`;
        });
        return result;
    }
}
```

```html
<!-- pushRelabel.html -->
<template>
    <div class="container">
        <h2>Push-Relabel Max-Flow Algorithm</h2>
        
        <div class="flow-result">
            <p><strong>Result:</strong> {flowResult}</p>
        </div>
        
        <div class="graph-display">
            <h3>Final Residual Graph:</h3>
            <pre>{graphDisplay}</pre>
        </div>
        
        <div class="algorithm-info">
            <h3>Algorithm Steps:</h3>
            <ul>
                <li>Initialize heights and excess flows</li>
                <li>Push flow from source to neighbors</li>
                <li>While excess flow exists at non-source/sink nodes:</li>
                <li>Try to push flow to lower height neighbors</li>
                <li>If no push possible, relabel the node</li>
                <li>Continue until no excess flow remains</li>
            </ul>
        </div>
    </div>
</template>
```

```css
/* pushRelabel.css */
.container {
    padding: 20px;
    font-family: Arial, sans-serif;
}

.flow-result {
    background-color: #e8f5e8;
    padding: 15px;
    border-radius: 5px;
    margin-bottom: 20px;
}

.graph-display {
    background-color: #f0f8ff;
    padding: 15px;
    border-radius: 5px;
    margin-bottom: 20px;
}

.graph-display pre {
    background-color: #f8f9fa;
    padding: 10px;
    border-radius: 3px;
    overflow-x: auto;
}

.algorithm-info {
    background-color: #fff3cd;
    padding: 15px;
    border-radius: 5px;
}

.algorithm-info ul {
    margin: 10px 0;
    padding-left: 20px;
}

.algorithm-info li {
    margin-bottom: 5px;
}
```

## Key Features of this Implementation:

1. **Graph Representation**: Uses adjacency matrix to represent the flow network
2. **Push Operation**: Moves flow from higher height nodes to lower height neighbors
3. **Relabel Operation**: Increases node height when no push is possible
4. **Excess Flow Management**: Tracks and manages flow at each node
5. **Visualization**: Displays the final residual graph and maximum flow value

## Algorithm Steps:
1. Initialize heights and excess flows
2. Push initial flow from source to neighbors
3. Process nodes with excess flow:
   - Try to push flow to neighbors with lower height
   - If no push possible, relabel the node (increase height)
4. Continue until all excess flows are resolved
5. Return the maximum flow value

This implementation demonstrates the core principles of the Push-Relabel algorithm for finding maximum flow in a network.

