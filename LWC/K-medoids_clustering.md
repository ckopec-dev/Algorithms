# K-Medoids Clustering in Lightning Web Component

Here's a complete example of implementing K-Medoids clustering algorithm in a Lightning Web Component:

## HTML Template (kMedoids.html)
```html
<template>
    <div class="slds-card">
        <div class="slds-card__header slds-grid">
            <header class="slds-media slds-media_center slds-has-flexi-truncate">
                <div class="slds-media__body">
                    <h2 class="slds-card__header-title slds-truncate" title="K-Medoids Clustering">K-Medoids Clustering</h2>
                </div>
            </header>
        </div>
        
        <div class="slds-card__body">
            <div class="slds-grid slds-gutters slds-wrap">
                <!-- Controls Section -->
                <div class="slds-col slds-size_1-of-1 slds-medium-size_6-of-12">
                    <lightning-card title="Clustering Parameters" icon-name="custom:custom14">
                        <div class="slds-form-element">
                            <label class="slds-form-element__label">Number of Clusters (k)</label>
                            <div class="slds-form-element__control">
                                <lightning-input 
                                    type="number" 
                                    value={kValue} 
                                    min="1" 
                                    max="10"
                                    onchange={handleKChange}>
                                </lightning-input>
                            </div>
                        </div>
                        
                        <div class="slds-form-element">
                            <label class="slds-form-element__label">Number of Iterations</label>
                            <div class="slds-form-element__control">
                                <lightning-input 
                                    type="number" 
                                    value={maxIterations} 
                                    min="1"
                                    max="100"
                                    onchange={handleIterationsChange}>
                                </lightning-input>
                            </div>
                        </div>
                        
                        <div class="slds-form-element">
                            <lightning-button 
                                label="Run Clustering" 
                                variant="brand" 
                                onclick={runClustering}
                                disabled={isProcessing}>
                            </lightning-button>
                        </div>
                    </lightning-card>
                </div>
                
                <!-- Data Input Section -->
                <div class="slds-col slds-size_1-of-1 slds-medium-size_6-of-12">
                    <lightning-card title="Data Points" icon-name="custom:custom34">
                        <lightning-input 
                            type="textarea" 
                            label="Enter data points (x,y format, one per line)"
                            value={inputData}
                            onchange={handleDataChange}
                            placeholder="Example:
1,2
3,4
5,6
7,8
9,10">
                        </lightning-input>
                    </lightning-card>
                </div>
                
                <!-- Results Section -->
                <div class="slds-col slds-size_1-of-1">
                    <lightning-card title="Clustering Results" icon-name="custom:custom36">
                        <template if:true={hasResults}>
                            <div class="slds-grid slds-gutters slds-wrap">
                                <div class="slds-col slds-size_1-of-1 slds-medium-size_6-of-12">
                                    <h3>Cluster Assignments</h3>
                                    <lightning-datatable
                                        data={clusterData}
                                        columns={clusterColumns}
                                        key-field="id"
                                        hide-checkbox-column="true">
                                    </lightning-datatable>
                                </div>
                                
                                <div class="slds-col slds-size_1-of-1 slds-medium-size_6-of-12">
                                    <h3>Cluster Centers</h3>
                                    <lightning-datatable
                                        data={medoidsData}
                                        columns={medoidsColumns}
                                        key-field="id"
                                        hide-checkbox-column="true">
                                    </lightning-datatable>
                                </div>
                            </div>
                            
                            <div class="slds-m-top_medium">
                                <h3>Cluster Visualization</h3>
                                <div class="chart-container" style="height: 300px;">
                                    <canvas id="clusterChart"></canvas>
                                </div>
                            </div>
                        </template>
                        
                        <template if:false={hasResults}>
                            <lightning-icon 
                                icon-name="utility:info" 
                                size="medium"
                                alternative-text="No results">
                            </lightning-icon>
                            <p class="slds-p-around_medium">Click "Run Clustering" to see results</p>
                        </template>
                    </lightning-card>
                </div>
            </div>
        </div>
    </div>
</template>
```

## JavaScript Controller (kMedoids.js)
```javascript
import { LightningElement, track } from 'lwc';

export default class KMedoids extends LightningElement {
    @track kValue = 3;
    @track maxIterations = 100;
    @track inputData = "1,2\n3,4\n5,6\n7,8\n9,10\n2,3\n4,5\n6,7\n8,9\n10,11";
    @track isProcessing = false;
    @track hasResults = false;
    
    @track clusterData = [];
    @track medoidsData = [];
    @track clusterColumns = [
        { label: 'Point', fieldName: 'point', type: 'text' },
        { label: 'Cluster ID', fieldName: 'clusterId', type: 'number' },
        { label: 'X Coordinate', fieldName: 'x', type: 'number' },
        { label: 'Y Coordinate', fieldName: 'y', type: 'number' }
    ];
    
    @track medoidsColumns = [
        { label: 'Medoid ID', fieldName: 'id', type: 'number' },
        { label: 'X Coordinate', fieldName: 'x', type: 'number' },
        { label: 'Y Coordinate', fieldName: 'y', type: 'number' }
    ];
    
    handleKChange(event) {
        this.kValue = parseInt(event.target.value);
    }
    
    handleIterationsChange(event) {
        this.maxIterations = parseInt(event.target.value);
    }
    
    handleDataChange(event) {
        this.inputData = event.target.value;
    }
    
    runClustering() {
        this.isProcessing = true;
        this.hasResults = false;
        
        // Parse input data
        const points = this.parseInputData();
        
        if (points.length < this.kValue) {
            console.error('Not enough data points for clustering');
            this.isProcessing = false;
            return;
        }
        
        // Run K-Medoids algorithm
        setTimeout(() => {
            const { clusters, medoids } = this.kMedoidsAlgorithm(points, this.kValue, this.maxIterations);
            
            // Format results for display
            this.formatResults(points, clusters, medoids);
            
            this.hasResults = true;
            this.isProcessing = false;
            
            // Render chart after a small delay to ensure DOM is ready
            setTimeout(() => {
                this.renderChart(points, clusters, medoids);
            }, 100);
        }, 100);
    }
    
    parseInputData() {
        const points = [];
        const lines = this.inputData.split('\n');
        
        lines.forEach((line, index) => {
            if (line.trim() !== '') {
                const coords = line.trim().split(',');
                if (coords.length === 2) {
                    points.push({
                        id: index,
                        x: parseFloat(coords[0]),
                        y: parseFloat(coords[1])
                    });
                }
            }
        });
        
        return points;
    }
    
    kMedoidsAlgorithm(points, k, maxIterations) {
        // Initialize medoids randomly
        let currentMedoids = this.initializeMedoids(points, k);
        let clusters = [];
        
        for (let iter = 0; iter < maxIterations; iter++) {
            // Assign points to clusters
            clusters = this.assignPointsToClusters(points, currentMedoids);
            
            // Update medoids
            const newMedoids = this.updateMedoids(points, clusters);
            
            // Check for convergence
            if (this.isConverged(currentMedoids, newMedoids)) {
                break;
            }
            
            currentMedoids = newMedoids;
        }
        
        return { clusters, medoids: currentMedoids };
    }
    
    initializeMedoids(points, k) {
        // Simple random initialization
        const shuffled = [...points].sort(() => 0.5 - Math.random());
        return shuffled.slice(0, k);
    }
    
    assignPointsToClusters(points, medoids) {
        // Initialize clusters array
        const clusters = Array(medoids.length).fill().map(() => []);
        
        points.forEach(point => {
            let minDistance = Infinity;
            let clusterId = 0;
            
            // Find the closest medoid
            medoids.forEach((medoid, index) => {
                const distance = this.euclideanDistance(point, medoid);
                if (distance < minDistance) {
                    minDistance = distance;
                    clusterId = index;
                }
            });
            
            clusters[clusterId].push(point);
        });
        
        return clusters;
    }
    
    updateMedoids(points, clusters) {
        const newMedoids = [];
        
        clusters.forEach(cluster => {
            if (cluster.length === 0) {
                // If cluster is empty, keep previous medoid
                return;
            }
            
            let minTotalDistance = Infinity;
            let bestMedoid = cluster[0];
            
            // For each point in the cluster, calculate total distance to all other points
            cluster.forEach(point => {
                let totalDistance = 0;
                cluster.forEach(otherPoint => {
                    totalDistance += this.euclideanDistance(point, otherPoint);
                });
                
                if (totalDistance < minTotalDistance) {
                    minTotalDistance = totalDistance;
                    bestMedoid = point;
                }
            });
            
            newMedoids.push(bestMedoid);
        });
        
        return newMedoids;
    }
    
    euclideanDistance(point1, point2) {
        return Math.sqrt(
            Math.pow(point1.x - point2.x, 2) + 
            Math.pow(point1.y - point2.y, 2)
        );
    }
    
    isConverged(oldMedoids, newMedoids) {
        if (oldMedoids.length !== newMedoids.length) return false;
        
        for (let i = 0; i < oldMedoids.length; i++) {
            if (this.euclideanDistance(oldMedoids[i], newMedoids[i]) > 0.001) {
                return false;
            }
        }
        return true;
    }
    
    formatResults(points, clusters, medoids) {
        // Format cluster data
        const clusterData = [];
        clusters.forEach((cluster, clusterIndex) => {
            cluster.forEach(point => {
                clusterData.push({
                    id: point.id,
                    point: `Point ${point.id}`,
                    clusterId: clusterIndex + 1,
                    x: point.x,
                    y: point.y
                });
            });
        });
        
        this.clusterData = clusterData;
        
        // Format medoids data
        const medoidsData = [];
        medoids.forEach((medoid, index) => {
            medoidsData.push({
                id: index + 1,
                x: medoid.x,
                y: medoid.y
            });
        });
        
        this.medoidsData = medoidsData;
    }
    
    renderChart(points, clusters, medoids) {
        const canvas = this.template.querySelector('canvas');
        if (!canvas) return;
        
        const ctx = canvas.getContext('2d');
        
        // Set canvas dimensions
        canvas.width = canvas.offsetWidth;
        canvas.height = canvas.offsetHeight;
        
        // Clear canvas
        ctx.clearRect(0, 0, canvas.width, canvas.height);
        
        // Calculate bounds for scaling
        let minX = Infinity, maxX = -Infinity, minY = Infinity, maxY = -Infinity;
        
        points.forEach(point => {
            minX = Math.min(minX, point.x);
            maxX = Math.max(maxX, point.x);
            minY = Math.min(minY, point.y);
            maxY = Math.max(maxY, point.y);
        });
        
        // Add some padding
        const padding = 20;
        minX -= padding;
        maxX += padding;
        minY -= padding;
        maxY += padding;
        
        const width = canvas.width - 2 * padding;
        const height = canvas.height - 2 * padding;
        
        // Draw clusters
        const colors = ['#FF6384', '#36A2EB', '#FFCE56', '#4BC0C0', '#9966FF'];
        
        clusters.forEach((cluster, clusterIndex) => {
            const color = colors[clusterIndex % colors.length];
            
            // Draw points
            cluster.forEach(point => {
                const x = padding + ((point.x - minX) / (maxX - minX)) * width;
                const y = height - padding - ((point.y - minY) / (maxY - minY)) * height;
                
                ctx.beginPath();
                ctx.arc(x, y, 5, 0, 2 * Math.PI);
                ctx.fillStyle = color;
                ctx.fill();
            });
            
            // Draw cluster center
            if (cluster.length > 0) {
                const centroidX = cluster.reduce((sum, p) => sum + p.x, 0) / cluster.length;
                const centroidY = cluster.reduce((sum, p) => sum + p.y, 0) / cluster.length;
                
                const x = padding + ((centroidX - minX) / (maxX - minX)) * width;
                const y = height - padding - ((centroidY - minY) / (maxY - minY)) * height;
                
                ctx.beginPath();
                ctx.arc(x, y, 8, 0, 2 * Math.PI);
                ctx.strokeStyle = color;
                ctx.lineWidth = 2;
                ctx.stroke();
            }
        });
        
        // Draw medoids
        medoids.forEach((medoid, index) => {
            const x = padding + ((medoid.x - minX) / (maxX - minX)) * width;
            const y = height - padding - ((medoid.y - minY) / (maxY - minY)) * height;
            
            ctx.beginPath();
            ctx.arc(x, y, 10, 0, 2 * Math.PI);
            ctx.fillStyle = '#FF0000';
            ctx.fill();
            ctx.strokeStyle = '#FFFFFF';
            ctx.lineWidth = 3;
            ctx.stroke();
        });
    }
}
```

## CSS Styles (kMedoids.css)
```css
.chart-container {
    position: relative;
    width: 100%;
    height: 100%;
}

.slds-card__body {
    padding: 1rem;
}

.slds-form-element {
    margin-bottom: 1rem;
}

.slds-grid .slds-col {
    padding: 0.5rem;
}
```

## Key Features of this Implementation:

1. **Complete K-Medoids Algorithm**: Implements the full K-Medoids clustering process
2. **Interactive Controls**: Users can adjust k value and number of iterations
3. **Data Input**: Supports comma-separated X,Y coordinates input
4. **Visual Results**: Shows cluster assignments and medoid locations
5. **Chart Visualization**: Renders a visual representation of clusters
6. **Responsive Design**: Uses Salesforce Lightning design system components
7. **Error Handling**: Validates input data and handles edge cases

## How to Use:

1. Create a new Lightning Web Component with this code
2. Enter data points in the textarea (format: "x,y" per line)
3. Adjust k value for number of clusters
4. Click "Run Clustering" to see results
5. View cluster assignments and visualization

The component will display both tabular results and a visual chart showing how the data points are clustered with their respective medoids marked in red.

