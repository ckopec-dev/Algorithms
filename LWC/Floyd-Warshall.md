# Floyd-Warshall Algorithm in Lightning Web Component

Here's a complete example of implementing the Floyd-Warshall algorithm in a Lightning Web Component:

## LWC Structure

### floydWarshall.js (JavaScript Controller)
```javascript
import { LightningElement } from 'lwc';

export default class FloydWarshall extends LightningElement {
    adjacencyMatrix = [
        [0, 3, 8, Infinity],
        [Infinity, 0, 4, Infinity],
        [Infinity, Infinity, 0, 2],
        [1, Infinity, Infinity, 0]
    ];
    
    shortestDistances = [];
    pathMatrix = [];
    vertices = 4;
    
    connected = false;
    
    handleCalculate() {
        this.floydWarshall();
        this.connected = true;
    }
    
    floydWarshall() {
        // Initialize distance matrix with adjacency matrix
        this.shortestDistances = [];
        for (let i = 0; i < this.vertices; i++) {
            this.shortestDistances[i] = [];
            for (let j = 0; j < this.vertices; j++) {
                this.shortestDistances[i][j] = this.adjacencyMatrix[i][j];
            }
        }
        
        // Initialize path matrix
        this.pathMatrix = [];
        for (let i = 0; i < this.vertices; i++) {
            this.pathMatrix[i] = [];
            for (let j = 0; j < this.vertices; j++) {
                if (this.adjacencyMatrix[i][j] !== Infinity) {
                    this.pathMatrix[i][j] = i;
                } else {
                    this.pathMatrix[i][j] = -1;
                }
            }
        }
        
        // Floyd-Warshall algorithm
        for (let k = 0; k < this.vertices; k++) {
            for (let i = 0; i < this.vertices; i++) {
                for (let j = 0; j < this.vertices; j++) {
                    if (this.shortestDistances[i][k] + this.shortestDistances[k][j] < 
                        this.shortestDistances[i][j]) {
                        this.shortestDistances[i][j] = this.shortestDistances[i][k] + 
                                                      this.shortestDistances[k][j];
                        this.pathMatrix[i][j] = this.pathMatrix[k][j];
                    }
                }
            }
        }
    }
    
    get distanceMatrix() {
        return this.shortestDistances.map(row => 
            row.map(val => val === Infinity ? '∞' : val)
        );
    }
    
    get formattedMatrix() {
        return this.distanceMatrix.map((row, index) => ({
            id: index,
            row: row
        }));
    }
    
    get matrixHeaders() {
        return Array.from({ length: this.vertices }, (_, i) => `V${i}`);
    }
}
```

### floydWarshall.html (Template)
```html
<template>
    <div class="slds-box slds-theme_default">
        <h2 class="slds-text-heading_small">Floyd-Warshall Algorithm</h2>
        
        <div class="slds-m-bottom_medium">
            <lightning-button 
                label="Calculate Shortest Paths" 
                variant="brand"
                onclick={handleCalculate}>
            </lightning-button>
        </div>
        
        <div class="slds-m-bottom_medium">
            <p class="slds-text-body_small">
                <strong>Input Matrix:</strong> Adjacency matrix representation of graph
            </p>
        </div>
        
        <div class="slds-grid slds-gutters slds-wrap">
            <div class="slds-col slds-size_1-of-2">
                <table class="slds-table slds-table_bordered slds-table_cell-buffer">
                    <thead>
                        <tr class="slds-line-height_reset">
                            <th class="slds-text-title_caps" scope="col">
                                <div>From \ To</div>
                            </th>
                            <template for:each={matrixHeaders} for:item="header">
                                <th key={header} class="slds-text-title_caps" scope="col">
                                    <div>{header}</div>
                                </th>
                            </template>
                        </tr>
                    </thead>
                    <tbody>
                        <template for:each={formattedMatrix} for:item="row">
                            <tr key={row.id}>
                                <th scope="row">{row.id}</th>
                                <template for:each={row.row} for:item="cell">
                                    <td key={cell}>{cell}</td>
                                </template>
                            </tr>
                        </template>
                    </tbody>
                </table>
            </div>
            
            <div class="slds-col slds-size_1-of-2">
                <div class="slds-m-bottom_medium">
                    <p class="slds-text-body_small">
                        <strong>Algorithm Explanation:</strong>
                    </p>
                    <ul class="slds-list_dotted">
                        <li>Computes shortest paths between all pairs of vertices</li>
                        <li>Time complexity: O(V³)</li>
                        <li>Space complexity: O(V²)</li>
                        <li>Works with negative edge weights (but not negative cycles)</li>
                    </ul>
                </div>
                
                <div class="slds-m-top_medium">
                    <p class="slds-text-body_small">
                        <strong>Steps:</strong>
                    </p>
                    <ol class="slds-list_ordered">
                        <li>Initialize distance matrix with adjacency matrix</li>
                        <li>For each intermediate vertex k:</li>
                        <li>For each source vertex i:</li>
                        <li>For each destination vertex j:</li>
                        <li>If distance i→k + k→j &lt; i→j, update distance</li>
                    </ol>
                </div>
            </div>
        </div>
        
        <template if:true={connected}>
            <div class="slds-m-top_medium">
                <h3 class="slds-text-heading_small">Shortest Distance Matrix</h3>
                <table class="slds-table slds-table_bordered slds-table_cell-buffer">
                    <thead>
                        <tr class="slds-line-height_reset">
                            <th class="slds-text-title_caps" scope="col">
                                <div>From \ To</div>
                            </th>
                            <template for:each={matrixHeaders} for:item="header">
                                <th key={header} class="slds-text-title_caps" scope="col">
                                    <div>{header}</div>
                                </th>
                            </template>
                        </tr>
                    </thead>
                    <tbody>
                        <template for:each={formattedMatrix} for:item="row">
                            <tr key={row.id}>
                                <th scope="row">{row.id}</th>
                                <template for:each={row.row} for:item="cell">
                                    <td key={cell}>{cell}</td>
                                </template>
                            </tr>
                        </template>
                    </tbody>
                </table>
            </div>
        </template>
    </div>
</template>
```

### floydWarshall.css (Styling)
```css
.slds-box {
    margin: 1rem 0;
}

.slds-table {
    margin: 1rem 0;
}

.slds-table_cell-buffer td,
.slds-table_cell-buffer th {
    padding: 0.75rem;
}
```

## How It Works

1. **Initialization**: The algorithm starts with the adjacency matrix as the initial distance matrix
2. **Triple Loop**: Uses three nested loops to consider each vertex as an intermediate point
3. **Path Update**: For each pair of vertices (i,j), it checks if going through vertex k provides a shorter path
4. **Result**: Produces a matrix showing the shortest distance between every pair of vertices

## Input Example

The example uses a 4-vertex graph with the following adjacency matrix:
```
    0   1   2   3
0  [0,  3,  8,  ∞]
1  [∞,  0,  4,  ∞]
2  [∞,  ∞,  0,  2]
3  [1,  ∞,  ∞,  0]
```

## Output

After running Floyd-Warshall, the algorithm produces the shortest path distances between all pairs of vertices, showing the minimum cost to travel from any vertex to any other vertex in the graph.

This implementation demonstrates the core Floyd-Warshall algorithm in a clean, reusable Lightning Web Component format that can be easily integrated into Salesforce applications.

