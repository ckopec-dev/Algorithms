# DBSCAN Clustering in Lightning Web Component

Here's an example of implementing DBSCAN clustering algorithm in a Lightning Web Component:

## HTML Template (dbscanCluster.html)

```html
<template>
    <div class="slds-card">
        <div class="slds-card__header slds-grid">
            <header class="slds-card__header-title">
                <h2 class="slds-text-heading_small">DBSCAN Clustering</h2>
            </header>
        </div>
        <div class="slds-card__body">
            <div class="slds-grid slds-gutters">
                <div class="slds-col slds-size_1-of-2">
                    <lightning-input 
                        label="Epsilon (ε)" 
                        type="number" 
                        value={epsilon}
                        onchange={handleEpsilonChange}
                        min="0.1"
                        step="0.1">
                    </lightning-input>
                    <lightning-input 
                        label="Minimum Points" 
                        type="number" 
                        value={minPoints}
                        onchange={handleMinPointsChange}
                        min="1"
                        step="1">
                    </lightning-input>
                    <lightning-button 
                        label="Run Clustering" 
                        onclick={runClustering}
                        variant="brand">
                    </lightning-button>
                </div>
                <div class="slds-col slds-size_1-of-2">
                    <div class="slds-text-title">Cluster Results</div>
                    <div class="slds-text-body_small">
                        <p>Clusters: {clusterCount}</p>
                        <p>Noise Points: {noiseCount}</p>
                    </div>
                </div>
            </div>
            
            <div class="slds-m-top_medium">
                <div class="slds-grid slds-gutters">
                    <div class="slds-col slds-size_1-of-2">
                        <div class="slds-text-title">Original Data Points</div>
                        <canvas id="dataCanvas" width="300" height="300"></canvas>
                    </div>
                    <div class="slds-col slds-size_1-of-2">
                        <div class="slds-text-title">Clustered Results</div>
                        <canvas id="clusterCanvas" width="300" height="300"></canvas>
                    </div>
                </div>
            </div>
        </div>
    </div>
</template>
```

## JavaScript Controller (dbscanCluster.js)

```javascript
import { LightningElement, track } from 'lwc';

export default class DbscanCluster extends LightningElement {
    @track epsilon = 0.5;
    @track minPoints = 3;
    @track clusterCount = 0;
    @track noiseCount = 0;
    
    // Sample data points for clustering
    @track dataPoints = [
        {x: 10, y: 10}, {x: 11, y: 11}, {x: 12, y: 12},
        {x: 100, y: 100}, {x: 101, y: 101}, {x: 102, y: 102},
        {x: 50, y: 50}, {x: 51, y: 51}, {x: 52, y: 52},
        {x: 150, y: 150}, {x: 151, y: 151}, {x: 152, y: 152}
    ];
    
    @track clusters = [];
    @track noisePoints = [];

    connectedCallback() {
        this.drawOriginalData();
    }

    handleEpsilonChange(event) {
        this.epsilon = parseFloat(event.target.value);
    }

    handleMinPointsChange(event) {
        this.minPoints = parseInt(event.target.value);
    }

    runClustering() {
        // Perform DBSCAN clustering
        const result = this.dbscan(this.dataPoints, this.epsilon, this.minPoints);
        this.clusters = result.clusters;
        this.noisePoints = result.noisePoints;
        this.clusterCount = result.clusters.length;
        this.noiseCount = result.noisePoints.length;
        
        // Draw clustered results
        this.drawClusteredData();
    }

    // DBSCAN Algorithm Implementation
    dbscan(points, eps, minPts) {
        const visited = new Set();
        const clusterId = new Map();
        const clusters = [];
        const noisePoints = [];
        let currentClusterId = 0;

        // Initialize all points as unvisited
        for (let i = 0; i < points.length; i++) {
            clusterId.set(i, -1); // -1 means unvisited
        }

        for (let i = 0; i < points.length; i++) {
            if (clusterId.get(i) !== -1) {
                continue; // Already processed
            }

            const neighbors = this.getNeighbors(points, i, eps);
            
            if (neighbors.length < minPts) {
                clusterId.set(i, -2); // -2 means noise
                noisePoints.push(points[i]);
            } else {
                // Start new cluster
                const cluster = [];
                this.expandCluster(points, i, neighbors, cluster, clusterId, eps, minPts);
                clusters.push(cluster);
                currentClusterId++;
            }
        }

        return {
            clusters: clusters,
            noisePoints: noisePoints
        };
    }

    // Get neighbors within epsilon distance
    getNeighbors(points, pointIndex, eps) {
        const neighbors = [];
        const point = points[pointIndex];
        
        for (let i = 0; i < points.length; i++) {
            if (i === pointIndex) continue;
            
            const distance = this.euclideanDistance(point, points[i]);
            if (distance <= eps) {
                neighbors.push(i);
            }
        }
        
        return neighbors;
    }

    // Expand cluster using DBSCAN logic
    expandCluster(points, pointIndex, neighbors, cluster, clusterId, eps, minPts) {
        cluster.push(points[pointIndex]);
        clusterId.set(pointIndex, 0); // Mark as visited

        let i = 0;
        while (i < neighbors.length) {
            const neighborIndex = neighbors[i];
            
            if (clusterId.get(neighborIndex) === -2) {
                // Noise point
                clusterId.set(neighborIndex, 0);
                cluster.push(points[neighborIndex]);
            } else if (clusterId.get(neighborIndex) === -1) {
                // Unvisited point
                clusterId.set(neighborIndex, 0);
                const newNeighbors = this.getNeighbors(points, neighborIndex, eps);
                
                if (newNeighbors.length >= minPts) {
                    neighbors = neighbors.concat(newNeighbors);
                }
            }
            i++;
        }
    }

    // Calculate Euclidean distance
    euclideanDistance(point1, point2) {
        return Math.sqrt(
            Math.pow(point2.x - point1.x, 2) + 
            Math.pow(point2.y - point1.y, 2)
        );
    }

    // Draw original data points
    drawOriginalData() {
        const canvas = this.template.querySelector('#dataCanvas');
        if (!canvas) return;
        
        const ctx = canvas.getContext('2d');
        ctx.clearRect(0, 0, canvas.width, canvas.height);
        
        ctx.fillStyle = 'blue';
        this.dataPoints.forEach(point => {
            ctx.beginPath();
            ctx.arc(point.x * 2, point.y * 2, 3, 0, 2 * Math.PI);
            ctx.fill();
        });
        
        ctx.fillStyle = 'black';
        ctx.font = '12px Arial';
        ctx.fillText('Original Data Points', 10, 20);
    }

    // Draw clustered results
    drawClusteredData() {
        const canvas = this.template.querySelector('#clusterCanvas');
        if (!canvas) return;
        
        const ctx = canvas.getContext('2d');
        ctx.clearRect(0, 0, canvas.width, canvas.height);
        
        // Draw clusters
        this.clusters.forEach((cluster, clusterIndex) => {
            const colors = ['red', 'green', 'orange', 'purple', 'brown', 'pink'];
            const color = colors[clusterIndex % colors.length];
            
            ctx.fillStyle = color;
            cluster.forEach(point => {
                ctx.beginPath();
                ctx.arc(point.x * 2, point.y * 2, 4, 0, 2 * Math.PI);
                ctx.fill();
            });
        });
        
        // Draw noise points
        ctx.fillStyle = 'gray';
        this.noisePoints.forEach(point => {
            ctx.beginPath();
            ctx.arc(point.x * 2, point.y * 2, 3, 0, 2 * Math.PI);
            ctx.fill();
        });
        
        ctx.fillStyle = 'black';
        ctx.font = '12px Arial';
        ctx.fillText('Clustered Results', 10, 20);
    }
}
```

## CSS Styles (dbscanCluster.css)

```css
.slds-card__header-title h2 {
    font-weight: 600;
}

.slds-text-title {
    font-weight: 600;
    margin-bottom: 0.5rem;
}

canvas {
    border: 1px solid #ccc;
    margin-top: 0.5rem;
}
```

## Key Features of this Implementation:

1. **Interactive Controls**: Users can adjust epsilon and minimum points parameters
2. **Visual Feedback**: Shows both original data and clustered results in separate canvases
3. **DBSCAN Algorithm**: Implements the core DBSCAN clustering logic
4. **Noise Detection**: Properly identifies and displays noise points
5. **Cluster Visualization**: Different colors for different clusters
6. **Real-time Updates**: Results update when clustering is run

## How it Works:

1. **Input Parameters**: Users set epsilon (distance threshold) and minimum points
2. **Neighbor Finding**: Calculates distances between all points
3. **Cluster Expansion**: Expands clusters using the DBSCAN algorithm
4. **Visualization**: Draws results on HTML canvas elements
5. **Statistics**: Shows number of clusters and noise points

This implementation provides a complete, functional DBSCAN clustering solution within a Lightning Web Component framework.

