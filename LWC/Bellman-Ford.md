# Bellman-Ford Algorithm in Lightning Web Component

Here's a complete implementation of the Bellman-Ford algorithm in a Lightning Web Component:

```javascript
// bellmanFord.js
import { LightningElement } from 'lwc';

export default class BellmanFord extends LightningElement {
    source = '';
    edges = '';
    result = '';
    error = '';

    // Sample graph data
    connectedCallback() {
        this.source = '0';
        this.edges = '0,1,4\n0,2,2\n1,2,1\n1,3,5\n2,3,8\n2,4,10\n3,4,2';
    }

    handleSourceChange(event) {
        this.source = event.target.value;
    }

    handleEdgesChange(event) {
        this.edges = event.target.value;
    }

    handleCalculate() {
        try {
            this.error = '';
            const source = parseInt(this.source);
            const edges = this.parseEdges(this.edges);
            
            const result = this.bellmanFord(source, edges);
            this.result = JSON.stringify(result, null, 2);
        } catch (error) {
            this.error = error.message;
        }
    }

    parseEdges(edgesText) {
        const edges = [];
        const lines = edgesText.trim().split('\n');
        
        lines.forEach(line => {
            const [from, to, weight] = line.split(',').map(Number);
            if (isNaN(from) || isNaN(to) || isNaN(weight)) {
                throw new Error('Invalid edge format. Use: from,to,weight');
            }
            edges.push({ from, to, weight });
        });
        
        return edges;
    }

    bellmanFord(source, edges) {
        // Find all vertices
        const vertices = new Set();
        edges.forEach(edge => {
            vertices.add(edge.from);
            vertices.add(edge.to);
        });
        
        const vertexCount = vertices.size;
        const distances = new Array(vertexCount).fill(Infinity);
        const predecessors = new Array(vertexCount).fill(-1);
        
        // Initialize source
        distances[source] = 0;
        
        // Relax edges repeatedly
        for (let i = 0; i < vertexCount - 1; i++) {
            for (const edge of edges) {
                const { from, to, weight } = edge;
                if (distances[from] !== Infinity && 
                    distances[from] + weight < distances[to]) {
                    distances[to] = distances[from] + weight;
                    predecessors[to] = from;
                }
            }
        }
        
        // Check for negative weight cycles
        let hasNegativeCycle = false;
        for (const edge of edges) {
            const { from, to, weight } = edge;
            if (distances[from] !== Infinity && 
                distances[from] + weight < distances[to]) {
                hasNegativeCycle = true;
                break;
            }
        }
        
        if (hasNegativeCycle) {
            throw new Error('Graph contains negative weight cycle');
        }
        
        // Format result
        const result = {
            distances: distances,
            predecessors: predecessors,
            hasNegativeCycle: hasNegativeCycle
        };
        
        return result;
    }

    get formattedResult() {
        if (!this.result) return '';
        try {
            const parsed = JSON.parse(this.result);
            return `Distances: ${JSON.stringify(parsed.distances)}\n` +
                   `Predecessors: ${JSON.stringify(parsed.predecessors)}\n` +
                   `Has Negative Cycle: ${parsed.hasNegativeCycle}`;
        } catch (e) {
            return this.result;
        }
    }
}
```

```html
<!-- bellmanFord.html -->
<template>
    <div class="slds-card">
        <div class="slds-card__header slds-grid">
            <header class="slds-media slds-media_center slds-has-flexi-truncate">
                <div class="slds-media__body">
                    <h2 class="slds-card__header-title slds-truncate" title="Bellman-Ford Algorithm">
                        Bellman-Ford Algorithm
                    </h2>
                </div>
            </header>
        </div>
        
        <div class="slds-card__body">
            <div class="slds-grid slds-gutters">
                <div class="slds-col slds-size_1-of-2">
                    <lightning-input 
                        label="Source Vertex" 
                        value={source}
                        onchange={handleSourceChange}
                        type="number"
                        min="0">
                    </lightning-input>
                    
                    <lightning-textarea 
                        label="Edges (format: from,to,weight)" 
                        value={edges}
                        onchange={handleEdgesChange}
                        placeholder="0,1,4&#10;0,2,2&#10;1,2,1&#10;1,3,5&#10;2,3,8&#10;2,4,10&#10;3,4,2">
                    </lightning-textarea>
                    
                    <lightning-button 
                        label="Calculate Shortest Paths" 
                        variant="brand" 
                        onclick={handleCalculate}>
                    </lightning-button>
                </div>
                
                <div class="slds-col slds-size_1-of-2">
                    <template if:true={error}>
                        <div class="slds-notify slds-notify_alert slds-theme_error">
                            <span class="slds-assistive-text">Error</span>
                            <h2 class="slds-text-heading_small">{error}</h2>
                        </div>
                    </template>
                    
                    <template if:true={result}>
                        <h3>Results:</h3>
                        <pre>{formattedResult}</pre>
                    </template>
                </div>
            </div>
        </div>
    </div>
</template>
```

```css
/* bellmanFord.css */
.slds-card {
    margin: 1rem;
}

.slds-grid {
    margin: 1rem 0;
}

pre {
    background-color: #f4f4f4;
    padding: 1rem;
    border-radius: 0.25rem;
    overflow: auto;
}
```

## How to Use

1. **Source Vertex**: Enter the starting vertex (0-based indexing)
2. **Edges**: Enter edges in format `from,to,weight` (one per line)
3. **Calculate**: Click to run the Bellman-Ford algorithm

## Example Input

```
Source: 0
Edges:
0,1,4
0,2,2
1,2,1
1,3,5
2,3,8
2,4,10
3,4,2
```

## Algorithm Features

- **Time Complexity**: O(VE) where V = vertices, E = edges
- **Space Complexity**: O(V)
- **Handles negative weights**
- **Detects negative weight cycles**
- **Returns shortest distances and predecessors**

## Output

The component returns:
- Shortest distances from source to all vertices
- Predecessor array for path reconstruction
- Negative cycle detection

This implementation demonstrates a complete Bellman-Ford algorithm with proper error handling and user interface in Lightning Web Components.

