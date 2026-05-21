# Depth-First Search (DFS) in Lightning Web Component

Here's a complete example of implementing DFS algorithm in a Lightning Web Component:

```javascript
// dfsComponent.js
import { LightningElement } from 'lwc';

export default class DfsComponent extends LightningElement {
    graph = {
        'A': ['B', 'C'],
        'B': ['A', 'D', 'E'],
        'C': ['A', 'F'],
        'D': ['B'],
        'E': ['B', 'F'],
        'F': ['C', 'E']
    };

    visited = new Set();
    dfsResult = [];
    isRunning = false;

    connectedCallback() {
        // Initialize the graph visualization
        this.initializeGraph();
    }

    initializeGraph() {
        // This would typically be called to set up the visual representation
        console.log('Graph initialized:', this.graph);
    }

    handleStartDFS() {
        this.isRunning = true;
        this.dfsResult = [];
        this.visited.clear();
        
        // Start DFS from node 'A'
        this.dfs('A');
        
        this.isRunning = false;
        this.updateDisplay();
    }

    dfs(node) {
        // Mark current node as visited
        this.visited.add(node);
        this.dfsResult.push(node);
        
        // Recursively visit all unvisited neighbors
        const neighbors = this.graph[node];
        if (neighbors) {
            for (let neighbor of neighbors) {
                if (!this.visited.has(neighbor)) {
                    this.dfs(neighbor);
                }
            }
        }
    }

    updateDisplay() {
        // Update the UI with DFS results
        const resultElement = this.template.querySelector('.dfs-result');
        if (resultElement) {
            resultElement.textContent = this.dfsResult.join(' -> ');
        }
    }

    // Alternative implementation with path tracking
    dfsWithPath(node, visited = new Set(), path = []) {
        visited.add(node);
        path.push(node);
        
        const neighbors = this.graph[node];
        if (neighbors) {
            for (let neighbor of neighbors) {
                if (!visited.has(neighbor)) {
                    this.dfsWithPath(neighbor, visited, path);
                }
            }
        }
        
        return path;
    }

    // Method to find a specific path
    findPath(start, end) {
        const visited = new Set();
        const path = [];
        
        function dfsHelper(currentNode) {
            if (currentNode === end) {
                path.push(currentNode);
                return true;
            }
            
            visited.add(currentNode);
            path.push(currentNode);
            
            const neighbors = this.graph[currentNode];
            if (neighbors) {
                for (let neighbor of neighbors) {
                    if (!visited.has(neighbor)) {
                        if (dfsHelper(neighbor)) {
                            return true;
                        }
                    }
                }
            }
            
            path.pop(); // Backtrack
            return false;
        }
        
        dfsHelper(start);
        return path;
    }

    get graphData() {
        return JSON.stringify(this.graph, null, 2);
    }

    get visitedNodes() {
        return Array.from(this.visited).join(', ');
    }
}
```

```html
<!-- dfsComponent.html -->
<template>
    <div class="container">
        <h2>Depth-First Search (DFS) Algorithm</h2>
        
        <div class="graph-info">
            <h3>Graph Structure:</h3>
            <pre>{graphData}</pre>
            
            <div class="controls">
                <lightning-button 
                    label="Start DFS" 
                    variant="brand" 
                    onclick={handleStartDFS}
                    disabled={isRunning}>
                </lightning-button>
            </div>
        </div>

        <div class="results">
            <h3>DFS Traversal Result:</h3>
            <p class="dfs-result">Not executed yet</p>
            
            <h3>Visited Nodes:</h3>
            <p>{visitedNodes}</p>
        </div>

        <div class="algorithm-explanation">
            <h3>How DFS Works:</h3>
            <ul>
                <li>Start from a given node</li>
                <li>Mark the current node as visited</li>
                <li>Recursively visit all unvisited neighbors</li>
                <li>Backtrack when no unvisited neighbors remain</li>
            </ul>
        </div>
    </div>
</template>
```

```css
/* dfsComponent.css */
.container {
    padding: 20px;
    max-width: 800px;
    margin: 0 auto;
}

.graph-info {
    background-color: #f5f5f5;
    padding: 15px;
    border-radius: 8px;
    margin-bottom: 20px;
}

.controls {
    margin: 15px 0;
    text-align: center;
}

.dfs-result {
    font-family: monospace;
    font-size: 18px;
    font-weight: bold;
    color: #0070d2;
    padding: 10px;
    background-color: #e6f4ff;
    border-radius: 4px;
    border: 1px solid #b3d9ff;
}

.algorithm-explanation {
    background-color: #fff3cd;
    padding: 15px;
    border-radius: 8px;
    border: 1px solid #ffeaa7;
}

.algorithm-explanation ul {
    margin: 10px 0;
    padding-left: 20px;
}

.algorithm-explanation li {
    margin: 5px 0;
}
```

## Key Features of this DFS Implementation:

1. **Recursive DFS Algorithm**: Uses recursion to traverse the graph
2. **Visited Tracking**: Keeps track of visited nodes to avoid cycles
3. **Visual Results**: Displays the traversal order
4. **Interactive Controls**: Start button to initiate the search
5. **Graph Visualization**: Shows the graph structure being traversed

## Algorithm Steps:
1. Choose a starting node
2. Mark it as visited
3. For each unvisited neighbor, recursively apply DFS
4. Continue until all reachable nodes are visited

## Time Complexity:
- **Time**: O(V + E) where V is vertices and E is edges
- **Space**: O(V) for the visited set and recursion stack

This implementation demonstrates how to use the DFS algorithm in a Lightning Web Component environment while maintaining clean, reusable code structure.

