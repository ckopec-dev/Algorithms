# Principal Component Analysis (PCA) in Lightning Web Component

Here's an example implementation of PCA algorithm using Lightning Web Components:

```javascript
// pca.js
import { LightningElement, api, track } from 'lwc';

export default class PcaComponent extends LightningElement {
    @track data = [];
    @track components = [];
    @track explainedVariance = [];
    @track loading = false;
    @track error = null;

    // Sample data input
    @api sampleData = [
        [2.5, 2.4],
        [0.5, 0.7],
        [2.2, 2.9],
        [1.9, 2.2],
        [3.1, 3.0],
        [2.3, 2.7],
        [2.0, 1.6],
        [1.0, 1.1],
        [1.5, 1.6],
        [1.1, 0.9]
    ];

    connectedCallback() {
        this.performPCA();
    }

    @api
    performPCA() {
        this.loading = true;
        this.error = null;

        try {
            // Normalize the data
            const normalizedData = this.normalizeData(this.sampleData);
            
            // Calculate covariance matrix
            const covarianceMatrix = this.calculateCovarianceMatrix(normalizedData);
            
            // Calculate eigenvalues and eigenvectors
            const { eigenvalues, eigenvectors } = this.calculateEigenvaluesAndVectors(covarianceMatrix);
            
            // Sort by eigenvalues (descending)
            const sortedIndices = this.sortEigenvalues(eigenvalues);
            
            // Get principal components
            const principalComponents = this.getPrincipalComponents(eigenvectors, sortedIndices);
            
            // Calculate explained variance
            const totalVariance = eigenvalues.reduce((sum, val) => sum + val, 0);
            const explainedVariance = eigenvalues.map(val => (val / totalVariance) * 100);
            
            this.components = principalComponents;
            this.explainedVariance = explainedVariance;
            this.data = normalizedData;
            
        } catch (error) {
            this.error = error.message;
        } finally {
            this.loading = false;
        }
    }

    normalizeData(data) {
        // Calculate mean for each feature
        const means = this.calculateMeans(data);
        
        // Normalize data
        return data.map(row => 
            row.map((value, index) => value - means[index])
        );
    }

    calculateMeans(data) {
        const means = [];
        const numFeatures = data[0].length;
        
        for (let i = 0; i < numFeatures; i++) {
            let sum = 0;
            for (let j = 0; j < data.length; j++) {
                sum += data[j][i];
            }
            means.push(sum / data.length);
        }
        
        return means;
    }

    calculateCovarianceMatrix(data) {
        const n = data.length;
        const numFeatures = data[0].length;
        const covarianceMatrix = Array(numFeatures).fill().map(() => Array(numFeatures).fill(0));
        
        for (let i = 0; i < numFeatures; i++) {
            for (let j = 0; j < numFeatures; j++) {
                let sum = 0;
                for (let k = 0; k < n; k++) {
                    sum += data[k][i] * data[k][j];
                }
                covarianceMatrix[i][j] = sum / (n - 1);
            }
        }
        
        return covarianceMatrix;
    }

    calculateEigenvaluesAndVectors(covarianceMatrix) {
        // Simplified eigenvalue calculation (in practice, use numerical methods)
        const eigenvalues = this.calculateEigenvalues(covarianceMatrix);
        const eigenvectors = this.calculateEigenvectors(covarianceMatrix, eigenvalues);
        
        return { eigenvalues, eigenvectors };
    }

    calculateEigenvalues(matrix) {
        // This is a simplified version - in production, use proper numerical methods
        const n = matrix.length;
        const eigenvalues = [];
        
        // For 2x2 matrix, direct calculation
        if (n === 2) {
            const a = matrix[0][0];
            const b = matrix[0][1];
            const c = matrix[1][0];
            const d = matrix[1][1];
            
            const trace = a + d;
            const det = a * d - b * c;
            
            const discriminant = trace * trace - 4 * det;
            const sqrtDiscriminant = Math.sqrt(Math.abs(discriminant));
            
            eigenvalues.push((trace + sqrtDiscriminant) / 2);
            eigenvalues.push((trace - sqrtDiscriminant) / 2);
        }
        
        return eigenvalues;
    }

    calculateEigenvectors(matrix, eigenvalues) {
        const n = matrix.length;
        const eigenvectors = [];
        
        if (n === 2) {
            // For 2x2 matrix, calculate eigenvectors
            for (let i = 0; i < eigenvalues.length; i++) {
                const lambda = eigenvalues[i];
                const a = matrix[0][0] - lambda;
                const b = matrix[0][1];
                const c = matrix[1][0];
                const d = matrix[1][1] - lambda;
                
                // Solve (A - λI)v = 0
                // We can use [b, -(a)] or [-(d), c] as eigenvector
                const vector = [b, -(a)];
                const magnitude = Math.sqrt(vector[0] * vector[0] + vector[1] * vector[1]);
                
                if (magnitude > 0) {
                    eigenvectors.push([vector[0] / magnitude, vector[1] / magnitude]);
                } else {
                    eigenvectors.push([1, 0]);
                }
            }
        }
        
        return eigenvectors;
    }

    sortEigenvalues(eigenvalues) {
        return eigenvalues
            .map((value, index) => ({ value, index }))
            .sort((a, b) => b.value - a.value)
            .map(item => item.index);
    }

    getPrincipalComponents(eigenvectors, sortedIndices) {
        return sortedIndices.map(index => eigenvectors[index]);
    }

    get varianceInfo() {
        return this.explainedVariance.map((variance, index) => ({
            component: index + 1,
            variance: variance.toFixed(2),
            cumulative: this.explainedVariance.slice(0, index + 1).reduce((sum, val) => sum + val, 0).toFixed(2)
        }));
    }

    handleRefresh() {
        this.performPCA();
    }
}
```

```html
<!-- pca.html -->
<template>
    <div class="slds-box slds-theme_default">
        <div class="slds-grid slds-gutters slds-wrap">
            <div class="slds-col slds-size_12-of-12">
                <h2 class="slds-text-heading_small">Principal Component Analysis (PCA)</h2>
                <lightning-button 
                    label="Refresh PCA" 
                    onclick={handleRefresh}
                    variant="brand"
                    class="slds-m-bottom_small">
                </lightning-button>
            </div>
            
            <template if:true={loading}>
                <div class="slds-col slds-size_12-of-12">
                    <lightning-spinner alternative-text="Loading"></lightning-spinner>
                </div>
            </template>

            <template if:true={error}>
                <div class="slds-col slds-size_12-of-12">
                    <lightning-alert 
                        title="Error" 
                        message={error} 
                        theme="error">
                    </lightning-alert>
                </div>
            </template>

            <template if:false={loading}>
                <div class="slds-col slds-size_12-of-12 slds-p-bottom_medium">
                    <h3 class="slds-text-heading_small">Explained Variance</h3>
                    <lightning-datatable
                        data={varianceInfo}
                        columns={columns}
                        key-field="component"
                        hide-checkbox-column>
                    </lightning-datatable>
                </div>

                <div class="slds-col slds-size_12-of-12 slds-p-bottom_medium">
                    <h3 class="slds-text-heading_small">Principal Components</h3>
                    <template for:each={components} for:item="component" key={component.index}>
                        <div key={component.index} class="slds-box slds-theme_default slds-m-bottom_small">
                            <p>Component {component.index + 1}</p>
                            <p>Vector: [{component.value.join(', ')}]</p>
                        </div>
                    </template>
                </div>

                <div class="slds-col slds-size_12-of-12">
                    <h3 class="slds-text-heading_small">Original Data (Normalized)</h3>
                    <lightning-datatable
                        data={data}
                        columns={dataColumns}
                        key-field="id"
                        hide-checkbox-column>
                    </lightning-datatable>
                </div>
            </template>
        </div>
    </div>
</template>
```

```javascript
// pca.css
.slds-box {
    padding: 1rem;
}

.slds-text-heading_small {
    font-size: 1rem;
    font-weight: 600;
    margin-bottom: 0.5rem;
}
```

```javascript
// pca.js (columns definition)
import { LightningElement } from 'lwc';

export default class PcaComponent extends LightningElement {
    // ... previous code ...

    get columns() {
        return [
            { label: 'Component', fieldName: 'component', type: 'number' },
            { label: 'Variance (%)', fieldName: 'variance', type: 'number' },
            { label: 'Cumulative (%)', fieldName: 'cumulative', type: 'number' }
        ];
    }

    get dataColumns() {
        return this.sampleData[0].map((_, index) => ({
            label: `Feature ${index + 1}`,
            fieldName: index,
            type: 'number'
        }));
    }
}
```

## Key Features of this PCA Implementation:

1. **Data Normalization**: Centers the data around zero mean
2. **Covariance Matrix Calculation**: Computes relationships between features
3. **Eigenvalue/Eigenvector Computation**: Finds principal components
4. **Sorting**: Orders components by explained variance
5. **Variance Analysis**: Shows cumulative explained variance
6. **Responsive UI**: Uses Lightning Design System components
7. **Error Handling**: Graceful error management
8. **Refresh Capability**: Allows re-computation of PCA

## Usage:
1. Create a new Lightning Web Component
2. Copy the code into respective files
3. Add the component to a Lightning page
4. The PCA will automatically run on component initialization

This implementation provides a basic but functional PCA algorithm that can be extended with more sophisticated numerical methods for eigenvalue decomposition in production environments.

