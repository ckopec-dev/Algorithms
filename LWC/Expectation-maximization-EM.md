# EM Algorithm in Lightning Web Component

Here's an example implementation of the Expectation-Maximization (EM) algorithm in Lightning Web Component using JavaScript:

```javascript
import { LightningElement } from 'lwc';

export default class EmAlgorithm extends LightningElement {
    // Input data points
    dataPoints = [
        { x: 1, y: 2 },
        { x: 2, y: 3 },
        { x: 3, y: 4 },
        { x: 4, y: 5 },
        { x: 5, y: 6 }
    ];

    // EM algorithm parameters
    numClusters = 2;
    maxIterations = 100;
    tolerance = 0.001;
    
    // Results
    clusters = [];
    converged = false;
    iterations = 0;

    // Initialize cluster centers randomly
    initializeClusters() {
        const clusters = [];
        for (let i = 0; i < this.numClusters; i++) {
            clusters.push({
                id: i,
                x: Math.random() * 10,
                y: Math.random() * 10,
                points: []
            });
        }
        return clusters;
    }

    // E-step: Assign points to clusters based on current centers
    expectationStep(clusters) {
        const updatedClusters = clusters.map(cluster => ({
            ...cluster,
            points: []
        }));

        this.dataPoints.forEach(point => {
            let minDistance = Infinity;
            let closestClusterId = 0;

            clusters.forEach(cluster => {
                const distance = Math.sqrt(
                    Math.pow(point.x - cluster.x, 2) + 
                    Math.pow(point.y - cluster.y, 2)
                );

                if (distance < minDistance) {
                    minDistance = distance;
                    closestClusterId = cluster.id;
                }
            });

            updatedClusters[closestClusterId].points.push(point);
        });

        return updatedClusters;
    }

    // M-step: Update cluster centers based on assigned points
    maximizationStep(clusters) {
        return clusters.map(cluster => {
            if (cluster.points.length === 0) {
                return cluster; // Keep original center if no points assigned
            }

            const sumX = cluster.points.reduce((sum, point) => sum + point.x, 0);
            const sumY = cluster.points.reduce((sum, point) => sum + point.y, 0);
            
            return {
                ...cluster,
                x: sumX / cluster.points.length,
                y: sumY / cluster.points.length
            };
        });
    }

    // Calculate convergence
    calculateConvergence(oldClusters, newClusters) {
        let totalMovement = 0;
        for (let i = 0; i < oldClusters.length; i++) {
            const movement = Math.sqrt(
                Math.pow(newClusters[i].x - oldClusters[i].x, 2) + 
                Math.pow(newClusters[i].y - oldClusters[i].y, 2)
            );
            totalMovement += movement;
        }
        return totalMovement / oldClusters.length;
    }

    // Run EM algorithm
    runEMAlgorithm() {
        let clusters = this.initializeClusters();
        let oldClusters = JSON.parse(JSON.stringify(clusters));
        this.iterations = 0;

        this.converged = false;

        while (!this.converged && this.iterations < this.maxIterations) {
            // E-step
            clusters = this.expectationStep(clusters);
            
            // M-step
            clusters = this.maximizationStep(clusters);
            
            // Check convergence
            const convergence = this.calculateConvergence(oldClusters, clusters);
            
            if (convergence < this.tolerance) {
                this.converged = true;
            }

            oldClusters = JSON.parse(JSON.stringify(clusters));
            this.iterations++;
        }

        this.clusters = clusters;
    }

    // Handle button click to run algorithm
    handleRunAlgorithm() {
        this.runEMAlgorithm();
    }

    // Handle reset
    handleReset() {
        this.clusters = [];
        this.converged = false;
        this.iterations = 0;
    }

    // Get cluster data for display
    get clusterData() {
        return this.clusters.map(cluster => ({
            id: cluster.id,
            x: cluster.x.toFixed(2),
            y: cluster.y.toFixed(2),
            pointCount: cluster.points.length
        }));
    }
}
```

```html
<!-- em-algorithm.html -->
<template>
    <div class="slds-box slds-theme_default">
        <h2>Expectation-Maximization (EM) Algorithm</h2>
        
        <div class="slds-grid slds-gutters slds-wrap">
            <div class="slds-col slds-size_12-of-12">
                <lightning-button 
                    label="Run EM Algorithm" 
                    variant="brand" 
                    onclick={handleRunAlgorithm}>
                </lightning-button>
                <lightning-button 
                    label="Reset" 
                    variant="neutral" 
                    onclick={handleReset}
                    class="slds-m-left_small">
                </lightning-button>
            </div>
            
            <div class="slds-col slds-size_12-of-12 slds-m-top_medium">
                <div class="slds-text-title">Algorithm Status</div>
                <div class="slds-text-body_small">
                    <template if:true={converged}>
                        <span class="slds-badge slds-theme_success">Converged after {iterations} iterations</span>
                    </template>
                    <template if:false={converged}>
                        <span class="slds-badge slds-theme_warning">Running... {iterations} iterations</span>
                    </template>
                </div>
            </div>
            
            <div class="slds-col slds-size_12-of-12 slds-m-top_medium">
                <div class="slds-text-title">Cluster Results</div>
                <template for:each={clusterData} for:item="cluster">
                    <div key={cluster.id} class="slds-card slds-m-bottom_small">
                        <div class="slds-card__header slds-grid">
                            <header class="slds-media slds-media_center slds-has-flexi-truncate">
                                <div class="slds-media__body">
                                    <h3 class="slds-card__header-title slds-truncate" title="Cluster {cluster.id}">
                                        Cluster {cluster.id}
                                    </h3>
                                </div>
                            </header>
                        </div>
                        <div class="slds-card__body">
                            <div class="slds-grid slds-gutters">
                                <div class="slds-col slds-size_1-of-3">
                                    <div class="slds-text-body_small">Center: ({cluster.x}, {cluster.y})</div>
                                </div>
                                <div class="slds-col slds-size_1-of-3">
                                    <div class="slds-text-body_small">Points: {cluster.pointCount}</div>
                                </div>
                            </div>
                        </div>
                    </div>
                </template>
            </div>
        </div>
    </div>
</template>
```

```css
/* em-algorithm.css */
.slds-card__header {
    border-bottom: 1px solid #e5e5e5;
}

.slds-badge {
    padding: 0.25rem 0.5rem;
    font-size: 0.75rem;
    font-weight: 600;
    border-radius: 0.25rem;
    text-transform: uppercase;
}
```

## Key Components of the EM Algorithm Implementation:

1. **Initialization**: Randomly initialize cluster centers
2. **E-step**: Assign each data point to the closest cluster center
3. **M-step**: Update cluster centers based on assigned points
4. **Convergence Check**: Stop when centers don't move significantly
5. **Iteration Control**: Maximum iterations to prevent infinite loops

## Features:

- Interactive web component with UI controls
- Real-time convergence monitoring
- Cluster visualization with center coordinates
- Reset functionality to restart the algorithm
- Responsive design using Salesforce Lightning design system

This implementation demonstrates the core EM algorithm concepts in a practical LWC context, showing how the algorithm iteratively improves cluster assignments and center positions until convergence.

