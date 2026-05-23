# K-Means Clustering in Lightning Web Component

Here's a complete example of implementing K-means clustering algorithm in a Lightning Web Component:

## HTML Template (kmeansCluster.html)
```html
<template>
    <div class="slds-box slds-theme_default">
        <h2>K-Means Clustering</h2>
        
        <div class="slds-grid slds-gutters slds-wrap">
            <div class="slds-col slds-size_1-of-2">
                <lightning-card title="Data Input">
                    <div class="slds-p-around_small">
                        <lightning-input 
                            label="Number of Clusters (k)" 
                            type="number" 
                            value={kValue} 
                            min="2"
                            onchange={handleKChange}>
                        </lightning-input>
                        
                        <lightning-textarea 
                            label="Data Points (x,y format, one per line)" 
                            value={dataInput} 
                            onchange={handleDataChange}
                            placeholder="Example:
1,2
3,4
5,6
7,8">
                        </lightning-textarea>
                        
                        <lightning-button 
                            label="Run Clustering" 
                            onclick={runClustering}
                            variant="brand">
                        </lightning-button>
                    </div>
                </lightning-card>
            </div>
            
            <div class="slds-col slds-size_1-of-2">
                <lightning-card title="Results">
                    <div class="slds-p-around_small">
                        <template if:true={isClustered}>
                            <div class="slds-grid slds-gutters">
                                <div class="slds-col">
                                    <h3>Cluster Centers</h3>
                                    <template for:each={clusterCenters} for:item="center">
                                        <div key={center.id} class="slds-badge slds-badge_lightest">
                                            Center {center.id}: ({center.x}, {center.y})
                                        </div>
                                    </template>
                                </div>
                                <div class="slds-col">
                                    <h3>Cluster Assignments</h3>
                                    <template for:each={clusterAssignments} for:item="assignment">
                                        <div key={assignment.point.id}>
                                            Point ({assignment.point.x}, {assignment.point.y}) → Cluster {assignment.cluster}
                                        </div>
                                    </template>
                                </div>
                            </div>
                        </template>
                        
                        <template if:false={isClustered}>
                            <p>No clustering results yet. Please enter data and click "Run Clustering".</p>
                        </template>
                    </div>
                </lightning-card>
            </div>
        </div>
        
        <div class="slds-p-around_small">
            <lightning-card title="Visualization">
                <div class="slds-p-around_small">
                    <canvas id="clusterCanvas" width="400" height="400"></canvas>
                </div>
            </lightning-card>
        </div>
    </div>
</template>
```

## JavaScript Controller (kmeansCluster.js)
```javascript
import { LightningElement, track } from 'lwc';

export default class KmeansCluster extends LightningElement {
    @track kValue = 3;
    @track dataInput = "1,2\n3,4\n5,6\n7,8\n2,3\n4,5\n6,7\n8,9";
    @track isClustered = false;
    @track clusterCenters = [];
    @track clusterAssignments = [];
    
    // Parse input data
    parseData() {
        const points = [];
        const lines = this.dataInput.trim().split('\n');
        
        lines.forEach(line => {
            const coords = line.trim().split(',').map(Number);
            if (coords.length === 2 && !isNaN(coords[0]) && !isNaN(coords[1])) {
                points.push({ x: coords[0], y: coords[1] });
            }
        });
        
        return points;
    }
    
    // K-means clustering algorithm
    kmeans(points, k) {
        // Initialize centroids randomly
        let centroids = this.initializeCentroids(points, k);
        let assignments = new Array(points.length).fill(0);
        let newCentroids = [];
        let maxIterations = 100;
        let iteration = 0;
        
        while (iteration < maxIterations) {
            // Assign points to closest centroid
            for (let i = 0; i < points.length; i++) {
                let minDistance = Infinity;
                let closestCentroid = 0;
                
                for (let j = 0; j < centroids.length; j++) {
                    const distance = this.calculateDistance(
                        points[i], 
                        centroids[j]
                    );
                    
                    if (distance < minDistance) {
                        minDistance = distance;
                        closestCentroid = j;
                    }
                }
                
                assignments[i] = closestCentroid;
            }
            
            // Calculate new centroids
            newCentroids = [];
            for (let i = 0; i < k; i++) {
                const clusterPoints = points.filter((_, index) => assignments[index] === i);
                
                if (clusterPoints.length > 0) {
                    const sumX = clusterPoints.reduce((sum, point) => sum + point.x, 0);
                    const sumY = clusterPoints.reduce((sum, point) => sum + point.y, 0);
                    const centerX = sumX / clusterPoints.length;
                    const centerY = sumY / clusterPoints.length;
                    newCentroids.push({ x: centerX, y: centerY });
                } else {
                    // If cluster is empty, keep the old centroid
                    newCentroids.push(centroids[i]);
                }
            }
            
            // Check for convergence
            if (this.centroidsConverged(centroids, newCentroids)) {
                break;
            }
            
            centroids = newCentroids;
            iteration++;
        }
        
        return {
            centroids: centroids,
            assignments: assignments
        };
    }
    
    // Initialize centroids randomly
    initializeCentroids(points, k) {
        const centroids = [];
        const maxIndex = points.length - 1;
        
        for (let i = 0; i < k; i++) {
            const randomIndex = Math.floor(Math.random() * (maxIndex + 1));
            centroids.push({ ...points[randomIndex] });
        }
        
        return centroids;
    }
    
    // Calculate Euclidean distance between two points
    calculateDistance(point1, point2) {
        return Math.sqrt(
            Math.pow(point1.x - point2.x, 2) + 
            Math.pow(point1.y - point2.y, 2)
        );
    }
    
    // Check if centroids have converged
    centroidsConverged(oldCentroids, newCentroids) {
        const threshold = 0.001;
        
        for (let i = 0; i < oldCentroids.length; i++) {
            const distance = this.calculateDistance(
                oldCentroids[i], 
                newCentroids[i]
            );
            
            if (distance > threshold) {
                return false;
            }
        }
        
        return true;
    }
    
    // Run clustering when button is clicked
    runClustering() {
        const points = this.parseData();
        
        if (points.length < this.kValue) {
            alert('Not enough data points for the specified number of clusters');
            return;
        }
        
        const result = this.kmeans(points, this.kValue);
        
        this.clusterCenters = result.centroids.map((center, index) => ({
            id: index + 1,
            x: center.x,
            y: center.y
        }));
        
        this.clusterAssignments = points.map((point, index) => ({
            point: point,
            cluster: result.assignments[index] + 1
        }));
        
        this.isClustered = true;
        
        // Draw visualization
        this.drawVisualization(points, result.centroids, result.assignments);
    }
    
    // Draw clustering visualization
    drawVisualization(points, centroids, assignments) {
        const canvas = this.template.querySelector('#clusterCanvas');
        if (!canvas) return;
        
        const ctx = canvas.getContext('2d');
        const width = canvas.width;
        const height = canvas.height;
        
        // Clear canvas
        ctx.clearRect(0, 0, width, height);
        
        // Set up scaling
        const padding = 20;
        const scaleX = (width - 2 * padding) / 10;
        const scaleY = (height - 2 * padding) / 10;
        
        // Draw grid
        ctx.strokeStyle = '#e0e0e0';
        ctx.lineWidth = 1;
        
        for (let i = 0; i <= 10; i++) {
            const x = padding + i * scaleX;
            const y = padding + i * scaleY;
            
            ctx.beginPath();
            ctx.moveTo(x, padding);
            ctx.lineTo(x, height - padding);
            ctx.stroke();
            
            ctx.beginPath();
            ctx.moveTo(padding, y);
            ctx.lineTo(width - padding, y);
            ctx.stroke();
        }
        
        // Draw points with colors
        const colors = [
            '#FF6B6B', '#4ECDC4', '#45B7D1', '#96CEB4', 
            '#FFEAA7', '#DDA0DD', '#98D8C8', '#F7DC6F'
        ];
        
        points.forEach((point, index) => {
            const cluster = assignments[index];
            const color = colors[cluster % colors.length];
            
            const x = padding + point.x * scaleX;
            const y = height - padding - point.y * scaleY;
            
            ctx.beginPath();
            ctx.arc(x, y, 5, 0, 2 * Math.PI);
            ctx.fillStyle = color;
            ctx.fill();
            ctx.strokeStyle = '#333';
            ctx.lineWidth = 1;
            ctx.stroke();
        });
        
        // Draw centroids
        centroids.forEach((centroid, index) => {
            const x = padding + centroid.x * scaleX;
            const y = height - padding - centroid.y * scaleY;
            
            ctx.beginPath();
            ctx.arc(x, y, 8, 0, 2 * Math.PI);
            ctx.fillStyle = '#FF0000';
            ctx.fill();
            ctx.strokeStyle = '#FFFFFF';
            ctx.lineWidth = 2;
            ctx.stroke();
            
            ctx.fillStyle = '#FFFFFF';
            ctx.font = '12px Arial';
            ctx.textAlign = 'center';
            ctx.fillText(`C${index + 1}`, x, y + 4);
        });
    }
    
    // Event handlers
    handleKChange(event) {
        this.kValue = parseInt(event.target.value) || 3;
    }
    
    handleDataChange(event) {
        this.dataInput = event.target.value;
    }
}
```

## CSS Styles (kmeansCluster.css)
```css
.slds-badge {
    margin: 5px 0;
    padding: 5px 10px;
}

.slds-badge_lightest {
    background-color: #f4f6f9;
    border: 1px solid #e0e0e0;
}

.slds-grid .slds-col {
    margin-bottom: 10px;
}

.slds-card {
    margin-bottom: 15px;
}
```

## Key Features of this Implementation:

1. **Input Handling**: Accepts data points in x,y format
2. **K-means Algorithm**: Implements the standard K-means clustering algorithm
3. **Visualization**: Draws clusters on a canvas with different colors
4. **Interactive UI**: Allows users to adjust number of clusters
5. **Real-time Results**: Shows cluster centers and assignments
6. **Convergence Detection**: Stops when centroids stop changing significantly
7. **Error Handling**: Validates input data and cluster count

## How to Use:

1. Enter data points in the text area (one point per line, comma-separated)
2. Set the number of clusters (k)
3. Click "Run Clustering"
4. View results including cluster centers and assignments
5. See the visualization of clusters on the canvas

This implementation provides a complete, working K-means clustering solution within the Lightning Web Component framework.

