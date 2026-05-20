# SPFA Algorithm in Lightning Web Component

Here's an example implementation of the Shortest Path Faster Algorithm (SPFA) in a Lightning Web Component:

```javascript
// spfa.js
import { LightningElement } from 'lwc';

export default class SpfaExample extends LightningElement {
    graph = [];
    distances = [];
    visited = [];
    queue = [];
    result = '';
    
    // Initialize the graph with sample data
    initializeGraph() {
        // Graph representation: [from, to, weight]
        this.graph = [
            [0, 1, 4],
            [0, 2, 2],
            [1, 2, 1],
            [1, 3, 5],
            [2, 3, 8],
            [2, 4, 10],
            [3, 4, 2]
        ];
        
        this.distances = [Infinity, Infinity, Infinity, Infinity, Infinity];
        this.visited = [false, false, false, false, false];
        this.queue = [];
    }
    
    // SPFA Algorithm Implementation
    spfa(startNode) {
        // Initialize distances
        this.distances = [Infinity, Infinity, Infinity, Infinity, Infinity];
        this.visited = [false, false, false, false, false];
        this.queue = [];
        
        // Set starting node distance to 0
        this.distances[startNode] = 0;
        this.queue.push(startNode);
        this.visited[startNode] = true;
        
        let result = '';
        let iteration = 0;
        
        while (this.queue.length > 0) {
            iteration++;
            result += `Iteration ${iteration}: `;
            
            const currentNode = this.queue.shift();
            this.visited[currentNode] = false;
            
            // Check all edges from current node
            for (let i = 0; i < this.graph.length; i++) {
                const [from, to, weight] = this.graph[i];
                
                if (from === currentNode) {
                    const newDistance = this.distances[currentNode] + weight;
                    
                    if (newDistance < this.distances[to]) {
                        this.distances[to] = newDistance;
                        
                        // If node not in queue, add it
                        if (!this.visited[to]) {
                            this.queue.push(to);
                            this.visited[to] = true;
                        }
                    }
                }
            }
            
            result += `Updated distances: [${this.distances.join(', ')}]\n`;
        }
        
        this.result = result;
        return this.distances;
    }
    
    // Handle button click to run SPFA
    handleRunSPFA() {
        this.initializeGraph();
        const distances = this.spfa(0); // Start from node 0
        
        // Display results
        let output = `Shortest distances from node 0:\n`;
        for (let i = 0; i < distances.length; i++) {
            if (distances[i] === Infinity) {
                output += `Node ${i}: Not reachable\n`;
            } else {
                output += `Node ${i}: ${distances[i]}\n`;
            }
        }
        
        this.result = output;
    }
    
    // Get the result for display
    get displayResult() {
        return this.result;
    }
}
```

```html
<!-- spfa.html -->
<template>
    <div class="slds-card">
        <div class="slds-card__header slds-grid">
            <header class="slds-media slds-media_center slds-has-flexi-truncate">
                <div class="slds-media__body">
                    <h2 class="slds-card__header-title slds-truncate" title="SPFA Algorithm">
                        Shortest Path Faster Algorithm (SPFA)
                    </h2>
                </div>
            </header>
        </div>
        
        <div class="slds-card__body">
            <p class="slds-text-body_small">
                SPFA is an improvement of the Bellman-Ford algorithm that uses a queue to optimize 
                the relaxation process. It's particularly efficient for sparse graphs.
            </p>
            
            <div class="slds-m-top_medium">
                <lightning-button 
                    label="Run SPFA Algorithm" 
                    variant="brand" 
                    onclick={handleRunSPFA}
                    class="slds-m-bottom_medium">
                </lightning-button>
            </div>
            
            <div class="slds-box slds-box_x-small slds-theme_default">
                <h3 class="slds-text-heading_small">Algorithm Steps:</h3>
                <ul class="slds-list_dotted">
                    <li>Initialize distances with infinity, except source (0)</li>
                    <li>Add source to queue</li>
                    <li>While queue is not empty:</li>
                    <ul class="slds-list_dotted">
                        <li>Remove node from queue</li>
                        <li>Relax all edges from this node</li>
                        <li>If distance updated, add neighbor to queue</li>
                    </ul>
                </ul>
            </div>
            
            <div class="slds-m-top_medium">
                <h3 class="slds-text-heading_small">Results:</h3>
                <pre class="slds-text-body_small slds-box slds-box_x-small">{displayResult}</pre>
            </div>
            
            <div class="slds-m-top_medium">
                <h3 class="slds-text-heading_small">Graph Structure:</h3>
                <p>Nodes: 0, 1, 2, 3, 4</p>
                <p>Edges: 0→1(4), 0→2(2), 1→2(1), 1→3(5), 2→3(8), 2→4(10), 3→4(2)</p>
            </div>
        </div>
    </div>
</template>
```

```css
/* spfa.css */
.slds-card__header {
    background-color: #f4f6f9;
}

.slds-text-heading_small {
    color: #080707;
    font-size: 1rem;
    font-weight: 600;
    line-height: 1.25;
}

.slds-list_dotted {
    list-style-type: disc;
    margin-left: 1rem;
}

.slds-box_x-small {
    padding: 0.5rem;
}
```

## Key Features of this Implementation:

1. **Graph Representation**: Uses an array of edges with [from, to, weight] format
2. **Queue Management**: Implements queue-based relaxation as required by SPFA
3. **Distance Tracking**: Maintains shortest distances from source node
4. **Visualization**: Shows step-by-step execution process
5. **Interactive**: Button to trigger algorithm execution
6. **Result Display**: Shows final shortest distances and intermediate steps

## How SPFA Works:

1. Initialize all distances to infinity except source (0)
2. Add source node to queue
3. While queue is not empty:
   - Remove node from queue
   - For each neighbor, relax the edge if shorter path found
   - If distance updated, add neighbor to queue
4. Continue until no more improvements possible

This implementation demonstrates the core concepts of SPFA while maintaining the LWC component structure and best practices.

