# Tarjan's Algorithm in Lightning Web Component

Here's an example implementation of Tarjan's algorithm for finding strongly connected components in a directed graph using Lightning Web Components:

```javascript
// tarjanAlgorithm.js
import { LightningElement } from 'lwc';

export default class TarjanAlgorithm extends LightningElement {
    graph = [
        [1],        // Node 0 points to node 1
        [2],        // Node 1 points to node 2
        [0, 3],     // Node 2 points to nodes 0 and 3
        [4],        // Node 3 points to node 4
        [5],        // Node 4 points to node 5
        [3]         // Node 5 points to node 3
    ];
    
    result = [];
    sccs = [];

    connectedComponents() {
        const n = this.graph.length;
        const index = new Array(n).fill(-1);
        const lowlink = new Array(n).fill(-1);
        const onStack = new Array(n).fill(false);
        const stack = [];
        let indexCounter = 0;
        this.sccs = [];

        for (let i = 0; i < n; i++) {
            if (index[i] === -1) {
                this.strongConnect(i, index, lowlink, onStack, stack, indexCounter);
            }
        }

        return this.sccs;
    }

    strongConnect(node, index, lowlink, onStack, stack, indexCounter) {
        index[node] = indexCounter;
        lowlink[node] = indexCounter;
        indexCounter++;
        stack.push(node);
        onStack[node] = true;

        // Consider successors of node
        const neighbors = this.graph[node] || [];
        for (let i = 0; i < neighbors.length; i++) {
            const neighbor = neighbors[i];
            if (index[neighbor] === -1) {
                // Successor has not yet been visited
                this.strongConnect(neighbor, index, lowlink, onStack, stack, indexCounter);
                lowlink[node] = Math.min(lowlink[node], lowlink[neighbor]);
            } else if (onStack[neighbor]) {
                // Successor is in stack and hence in the current SCC
                lowlink[node] = Math.min(lowlink[node], index[neighbor]);
            }
        }

        // If node is a root node, pop the stack and create an SCC
        if (lowlink[node] === index[node]) {
            const scc = [];
            let current;
            do {
                current = stack.pop();
                onStack[current] = false;
                scc.push(current);
            } while (current !== node);
            this.sccs.push(scc);
        }
    }

    handleFindComponents() {
        this.result = this.connectedComponents();
        console.log('Strongly Connected Components:', this.result);
    }

    get formattedResult() {
        return this.result.map((scc, index) => 
            `Component ${index + 1}: [${scc.join(', ')}]`
        ).join('<br/>');
    }
}
```

```html
<!-- tarjanAlgorithm.html -->
<template>
    <div class="container">
        <h2>Tarjan's Algorithm for Strongly Connected Components</h2>
        
        <div class="graph-info">
            <p><strong>Graph Structure:</strong></p>
            <p>0 → 1 → 2 → 0 (cycle)</p>
            <p>3 → 4 → 5 → 3 (cycle)</p>
            <p>2 → 3 (connection)</p>
        </div>

        <lightning-button 
            label="Find Strongly Connected Components" 
            variant="brand"
            onclick={handleFindComponents}>
        </lightning-button>

        <div class="result-section">
            <h3>Results:</h3>
            <template if:true={result.length}>
                <div class="scc-list" innerHTML={formattedResult}></div>
            </template>
            <template if:false={result.length}>
                <p>No components found yet. Click the button above.</p>
            </template>
        </div>
    </div>
</template>
```

```css
/* tarjanAlgorithm.css */
.container {
    padding: 20px;
    max-width: 800px;
    margin: 0 auto;
}

.graph-info {
    background-color: #f0f8ff;
    padding: 15px;
    border-radius: 8px;
    margin-bottom: 20px;
}

.graph-info p {
    margin: 5px 0;
    font-family: monospace;
}

.result-section {
    margin-top: 20px;
}

.scc-list {
    background-color: #e8f5e8;
    padding: 15px;
    border-radius: 8px;
    border-left: 4px solid #4caf50;
}

.scc-list p {
    margin: 5px 0;
    font-weight: bold;
}
```

```javascript
// tarjanAlgorithm.js (Alternative implementation with more detailed logging)
import { LightningElement } from 'lwc';

export default class TarjanAlgorithm extends LightningElement {
    // Sample graph: adjacency list representation
    graph = [
        [1],        // Node 0 → Node 1
        [2],        // Node 1 → Node 2  
        [0, 3],     // Node 2 → Nodes 0, 3
        [4],        // Node 3 → Node 4
        [5],        // Node 4 → Node 5
        [3]         // Node 5 → Node 3
    ];

    result = [];
    logs = [];

    connectedComponents() {
        const n = this.graph.length;
        const index = new Array(n).fill(-1);
        const lowlink = new Array(n).fill(-1);
        const onStack = new Array(n).fill(false);
        const stack = [];
        let indexCounter = 0;
        this.sccs = [];
        this.logs = [];

        this.log(`Starting Tarjan's algorithm on graph with ${n} nodes`);
        
        for (let i = 0; i < n; i++) {
            if (index[i] === -1) {
                this.strongConnect(i, index, lowlink, onStack, stack, indexCounter);
            }
        }

        this.log(`Found ${this.sccs.length} strongly connected components`);
        return this.sccs;
    }

    strongConnect(node, index, lowlink, onStack, stack, indexCounter) {
        index[node] = indexCounter;
        lowlink[node] = indexCounter;
        indexCounter++;
        stack.push(node);
        onStack[node] = true;

        this.log(`Visiting node ${node}: index=${index[node]}, lowlink=${lowlink[node]}`);

        const neighbors = this.graph[node] || [];
        for (let i = 0; i < neighbors.length; i++) {
            const neighbor = neighbors[i];
            if (index[neighbor] === -1) {
                this.log(`  Node ${node} → ${neighbor} (unvisited)`);
                this.strongConnect(neighbor, index, lowlink, onStack, stack, indexCounter);
                lowlink[node] = Math.min(lowlink[node], lowlink[neighbor]);
                this.log(`  Updated lowlink[${node}] = ${lowlink[node]}`);
            } else if (onStack[neighbor]) {
                this.log(`  Node ${node} → ${neighbor} (on stack)`);
                lowlink[node] = Math.min(lowlink[node], index[neighbor]);
                this.log(`  Updated lowlink[${node}] = ${lowlink[node]}`);
            }
        }

        if (lowlink[node] === index[node]) {
            const scc = [];
            let current;
            do {
                current = stack.pop();
                onStack[current] = false;
                scc.push(current);
            } while (current !== node);
            
            this.sccs.push(scc);
            this.log(`Found SCC: [${scc.join(', ')}]`);
        }
    }

    handleFindComponents() {
        this.result = this.connectedComponents();
    }

    log(message) {
        // In a real implementation, you might want to store logs
        console.log(message);
    }

    get formattedResult() {
        return this.result.map((scc, index) => 
            `Component ${index + 1}: [${scc.join(', ')}]`
        ).join('<br/>');
    }

    get showLogs() {
        return this.logs.length > 0;
    }
}
```

## How it works:

1. **Graph Representation**: The graph is represented as an adjacency list where each index represents a node and contains an array of its neighbors.

2. **Algorithm Steps**:
   - Initialize tracking arrays (index, lowlink, onStack)
   - For each unvisited node, perform DFS using `strongConnect`
   - During DFS, maintain stack of visited nodes
   - Update lowlink values based on back edges
   - When a component is found, pop from stack and form SCC

3. **Key Variables**:
   - `index`: Discovery time of each node
   - `lowlink`: Lowest index reachable from the node
   - `onStack`: Tracks if node is in current stack
   - `stack`: Maintains the current DFS path

4. **Output**: Returns strongly connected components as arrays of node indices.

This implementation demonstrates how to apply Tarjan's algorithm in the LWC framework for finding strongly connected components in directed graphs.

