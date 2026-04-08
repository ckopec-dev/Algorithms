# K-Medoids Clustering in Lightning Web Component

Here's a complete example of implementing K-Medoids clustering algorithm in a Lightning Web Component:

## HTML Template (kMedoidsCluster.html)
```html
<template>
    <div class="slds-card">
        <div class="slds-card__header slds-grid">
            <header class="slds-card__header-title slds-truncate" title="K-Medoids Clustering">
                <h2 class="slds-text-heading_small">K-Medoids Clustering</h2>
            </header>
        </div>
        <div class="slds-card__body">
            <div class="slds-grid slds-gutters slds-wrap">
                <div class="slds-col slds-size_1-of-2">
                    <lightning-input 
                        label="Number of Clusters (k)" 
                        type="number" 
                        min="2" 
                        value={kValue}
                        onchange={handleKChange}>
                    </lightning-input>
                    
                    <lightning-button 
                        label="Run Clustering" 
                        variant="brand" 
                        onclick={runClustering}
                        disabled={isProcessing}>
                    </lightning-button>
                    
                    <lightning-button 
                        label="Reset" 
                        variant="neutral" 
                        onclick={resetData}>
                    </lightning-button>
                </div>
                
                <div class="slds-col slds-size_1-of-2">
                    <lightning-input 
                        label="Data Points" 
                        type="textarea" 
                        value={inputData}
                        onchange={handleDataChange}
                        placeholder="Enter data points (x,y) separated by commas">
                    </lightning-input>
                </div>
            </div>
            
            <div class="slds-m-top_medium">
                <lightning-datatable
                    data={clusterResults}
                    columns={columns}
                    key-field="id"
                    hide-checkbox-column="true">
                </lightning-datatable>
            </div>
            
            <div class="slds-m-top_medium">
                <lightning-card title="Cluster Visualization">
                    <div class="slds-grid slds-gutters slds-wrap">
                        <div class="slds-col slds-size_1-of-1">
                            <canvas ref="clusterCanvas" width="400" height="400"></canvas>
                        </div>
                    </div>
                </lightning-card>
            </div>
        </div>
    </div>
</template>
```

## JavaScript Controller (kMedoidsCluster.js)
```javascript
import { LightningElement, track } from 'lwc';

export default class KMedoidsCluster extends LightningElement {
    @track kValue = 3;
    @track inputData = "1,2\n2,3\n3,4\n4,5\n5,6\n6,7\n7,8\n8,9\n9,10\n10,11";
    @track clusterResults = [];
    @track isProcessing = false;
    
    columns = [
        { label: 'Point ID', fieldName: 'id' },
        { label: 'X Coordinate', fieldName: 'x', type: 'number' },
        { label: 'Y Coordinate', fieldName: 'y', type: 'number' },
        { label: 'Cluster', fieldName: 'cluster', type: 'number' },
        { label: 'Medoid', fieldName: 'isMedoid', type: 'boolean' }
    ];

    handleKChange(event) {
        this.kValue = parseInt(event.target.value);
    }

    handleDataChange(event) {
        this.inputData = event.target.value;
    }

    runClustering() {
        this.isProcessing = true;
        
        try {
            // Parse input data
            const points = this.parseData();
            
            if (points.length < this.kValue) {
                console.error('Not enough data points for clustering');
                this.isProcessing = false;
                return;
            }
            
            // Run K-Medoids clustering
            const clusters = this.kMedoidsClustering(points, this.kValue);
            
            // Format results for display
            this.clusterResults = this.formatResults(clusters);
            
            // Draw visualization
            this.drawClusters(clusters);
            
        } catch (error) {
            console.error('Clustering error:', error);
        } finally {
            this.isProcessing = false;
        }
    }

    parseData() {
        const lines = this.inputData.trim().split('\n');
        const points = [];
        
        lines.forEach((line, index) => {
            const [x, y] = line.split(',').map(Number);
            if (!isNaN(x) && !isNaN(y)) {
                points.push({
                    id: index + 1,
                    x: x,
                    y: y,
                    cluster: -1,
                    isMedoid: false
                });
            }
        });
        
        return points;
    }

    kMedoidsClustering(points, k) {
        // Initialize medoids randomly
        const medoids = this.initializeMedoids(points, k);
        
        let oldMedoids = [];
        let iterations = 0;
        const maxIterations = 100;
        
        do {
            oldMedoids = [...medoids];
            
            // Assign points to clusters
            this.assignPointsToClusters(points, medoids);
            
            // Update medoids
            this.updateMedoids(points, medoids);
            
            iterations++;
            
        } while (!this.areMedoidsEqual(oldMedoids, medoids) && iterations < maxIterations);
        
        return {
            points: points,
            medoids: medoids,
            k: k
        };
    }

    initializeMedoids(points, k) {
        // Randomly select k medoids from the data points
        const medoids = [];
        const shuffledPoints = [...points].sort(() => 0.5 - Math.random());
        
        for (let i = 0; i < Math.min(k, points.length); i++) {
            const point = shuffledPoints[i];
            point.isMedoid = true;
            medoids.push(point);
        }
        
        return medoids;
    }

    assignPointsToClusters(points, medoids) {
        points.forEach(point => {
            let minDistance = Infinity;
            let closestMedoid = null;
            
            medoids.forEach(medoid => {
                const distance = this.euclideanDistance(point, medoid);
                if (distance < minDistance) {
                    minDistance = distance;
                    closestMedoid = medoid;
                }
            });
            
            point.cluster = closestMedoid.id;
        });
    }

    updateMedoids(points, medoids) {
        medoids.forEach(medoid => {
            // Find the point in the cluster that minimizes the sum of distances to all other points in the cluster
            const clusterPoints = points.filter(p => p.cluster === medoid.id);
            
            if (clusterPoints.length > 0) {
                let bestMedoid = medoid;
                let minTotalDistance = Infinity;
                
                clusterPoints.forEach(point => {
                    let totalDistance = 0;
                    clusterPoints.forEach(clusterPoint => {
                        totalDistance += this.euclideanDistance(point, clusterPoint);
                    });
                    
                    if (totalDistance < minTotalDistance) {
                        minTotalDistance = totalDistance;
                        bestMedoid = point;
                    }
                });
                
                // Update medoid if we found a better one
                if (bestMedoid.id !== medoid.id) {
                    medoid.isMedoid = false;
                    bestMedoid.isMedoid = true;
                    // Update the medoid reference
                    const index = medoids.findIndex(m => m.id === medoid.id);
                    if (index !== -1) {
                        medoids[index] = bestMedoid;
                    }
                }
            }
        });
    }

    euclideanDistance(point1, point2) {
        return Math.sqrt(
            Math.pow(point1.x - point2.x, 2) + 
            Math.pow(point1.y - point2.y, 2)
        );
    }

    areMedoidsEqual(medoids1, medoids2) {
        if (medoids1.length !== medoids2.length) return false;
        
        return medoids1.every((medoid, index) => 
            medoid.id === medoids2[index].id
        );
    }

    formatResults(clusters) {
        const results = [];
        clusters.points.forEach(point => {
            results.push({
                id: point.id,
                x: point.x,
                y: point.y,
                cluster: point.cluster,
                isMedoid: point.isMedoid
            });
        });
        return results;
    }

    drawClusters(clusters) {
        const canvas = this.template.querySelector('canvas');
        if (!canvas) return;
        
        const ctx = canvas.getContext('2d');
        const width = canvas.width;
        const height = canvas.height;
        
        // Clear canvas
        ctx.clearRect(0, 0, width, height);
        
        // Draw points
        const points = clusters.points;
        const medoids = clusters.medoids;
        
        // Scale points to fit canvas
        const xValues = points.map(p => p.x);
        const yValues = points.map(p => p.y);
        const maxX = Math.max(...xValues);
        const minX = Math.min(...xValues);
        const maxY = Math.max(...yValues);
        const minY = Math.min(...yValues);
        
        const scaleX = (width - 40) / (maxX - minX);
        const scaleY = (height - 40) / (maxY - minY);
        const scale = Math.min(scaleX, scaleY);
        
        // Draw clusters
        const colors = ['#FF6B6B', '#4ECDC4', '#45B7D1', '#96CEB4', '#FFEAA7'];
        
        points.forEach(point => {
            const x = (point.x - minX) * scale + 20;
            const y = height - (point.y - minY) * scale - 20;
            
            ctx.beginPath();
            ctx.arc(x, y, 5, 0, 2 * Math.PI);
            
            if (point.isMedoid) {
                ctx.fillStyle = '#FF0000';
                ctx.fill();
                ctx.strokeStyle = '#000000';
                ctx.lineWidth = 2;
                ctx.stroke();
            } else {
                const clusterColor = colors[point.cluster % colors.length];
                ctx.fillStyle = clusterColor;
                ctx.fill();
            }
        });
        
        // Draw cluster boundaries (optional)
        ctx.strokeStyle = '#000000';
        ctx.lineWidth = 1;
        ctx.setLineDash([5, 5]);
        
        // Draw medoid connections (optional)
        medoids.forEach(medoid => {
            const x = (medoid.x - minX) * scale + 20;
            const y = height - (medoid.y - minY) * scale - 20;
            
            ctx.beginPath();
            ctx.arc(x, y, 10, 0, 2 * Math.PI);
            ctx.stroke();
        });
        
        ctx.setLineDash([]);
    }

    resetData() {
        this.kValue = 3;
        this.inputData = "1,2\n2,3\n3,4\n4,5\n5,6\n6,7\n7,8\n8,9\n9,10\n10,11";
        this.clusterResults = [];
    }
}
```

## CSS Styles (kMedoidsCluster.css)
```css
.slds-card {
    margin: 1rem;
}

.slds-card__header {
    background-color: #f4f6f9;
}

.slds-card__body {
    padding: 1rem;
}

.slds-grid {
    margin: 0.5rem 0;
}

.slds-col {
    margin: 0.5rem 0;
}

canvas {
    border: 1px solid #e1e8f0;
    background-color: #ffffff;
    border-radius: 0.25rem;
    box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
}
```

## Key Features of this Implementation:

1. **Interactive UI**: Users can input data points and specify the number of clusters
2. **K-Medoids Algorithm**: Implements the K-Medoids clustering algorithm with proper initialization and iteration
3. **Visualization**: Canvas-based visualization of clusters and medoids
4. **Data Management**: Parses input data and formats results for display
5. **Error Handling**: Includes validation and error handling
6. **Responsive Design**: Uses Salesforce Lightning design system components

## How to Use:

1. Enter data points in the textarea (format: "x,y" per line)
2. Set the number of clusters (k)
3. Click "Run Clustering" to execute the algorithm
4. View results in the table and visualization
5. Use "Reset" to clear all data and start over

This implementation demonstrates a complete K-Medoids clustering solution within the Lightning Web Component framework, suitable for Salesforce applications.

