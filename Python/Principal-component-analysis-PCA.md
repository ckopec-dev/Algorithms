# Principal Component Analysis (PCA) Example in Python

Here's a complete example demonstrating PCA using Python with scikit-learn:

```python
# Import necessary libraries
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.decomposition import PCA
from sklearn.preprocessing import StandardScaler
from sklearn.datasets import load_iris
import seaborn as sns

# Load sample dataset (Iris dataset)
iris = load_iris()
X = iris.data
y = iris.target
feature_names = iris.feature_names
target_names = iris.target_names

# Display basic information about the dataset
print("Dataset shape:", X.shape)
print("Features:", feature_names)
print("Target classes:", target_names)

# Standardize the features (important for PCA)
scaler = StandardScaler()
X_scaled = scaler.fit_transform(X)

# Apply PCA
# Let's reduce to 2 principal components for visualization
pca = PCA(n_components=2)
X_pca = pca.fit_transform(X_scaled)

# Display PCA results
print("\nExplained Variance Ratio:")
print(pca.explained_variance_ratio_)
print(f"\nTotal variance explained: {sum(pca.explained_variance_ratio_)*100:.2f}%")

print("\nPrincipal Components:")
print("Component 1:", pca.components_[0])
print("Component 2:", pca.components_[1])

# Visualize the results
plt.figure(figsize=(12, 5))

# Plot 1: PCA scatter plot
plt.subplot(1, 2, 1)
colors = ['red', 'green', 'blue']
for i, (color, target_name) in enumerate(zip(colors, target_names)):
    plt.scatter(X_pca[y == i, 0], X_pca[y == i, 1], 
                c=color, label=target_name, alpha=0.7)
plt.xlabel(f'First Principal Component (variance: {pca.explained_variance_ratio_[0]:.2f})')
plt.ylabel(f'Second Principal Component (variance: {pca.explained_variance_ratio_[1]:.2f})')
plt.title('PCA of Iris Dataset')
plt.legend()
plt.grid(True, alpha=0.3)

# Plot 2: Explained variance
plt.subplot(1, 2, 2)
plt.bar(range(1, len(pca.explained_variance_ratio_) + 1), 
        pca.explained_variance_ratio_, alpha=0.7, color='skyblue')
plt.xlabel('Principal Component')
plt.ylabel('Explained Variance Ratio')
plt.title('Explained Variance by Principal Components')
plt.xticks(range(1, len(pca.explained_variance_ratio_) + 1))

plt.tight_layout()
plt.show()

# Show the full PCA analysis with all components
print("\nFull PCA Analysis:")
pca_full = PCA()
pca_full.fit(X_scaled)

# Create a dataframe for better visualization
components_df = pd.DataFrame(
    pca_full.components_.T,
    columns=[f'PC{i+1}' for i in range(len(pca_full.components_))],
    index=feature_names
)

print("\nPrincipal Components Loadings:")
print(components_df)

# Plot the loadings
plt.figure(figsize=(10, 6))
sns.heatmap(components_df, annot=True, cmap='RdBu_r', center=0)
plt.title('PCA Component Loadings')
plt.ylabel('Original Features')
plt.xlabel('Principal Components')
plt.show()

# Example of how to determine optimal number of components
# Cumulative explained variance
cumulative_variance = np.cumsum(pca_full.explained_variance_ratio_)

plt.figure(figsize=(8, 6))
plt.plot(range(1, len(cumulative_variance) + 1), cumulative_variance, 'bo-')
plt.xlabel('Number of Components')
plt.ylabel('Cumulative Explained Variance')
plt.title('Cumulative Explained Variance vs Number of Components')
plt.grid(True, alpha=0.3)
plt.axhline(y=0.95, color='r', linestyle='--', alpha=0.7, label='95% variance')
plt.legend()
plt.show()

print(f"\nNumber of components needed to explain 95% variance: {np.argmax(cumulative_variance >= 0.95) + 1}")
```

## Key Steps in this PCA Implementation:

1. **Data Preparation**: Load dataset and standardize features (crucial for PCA)
2. **PCA Application**: Fit PCA model with desired number of components
3. **Results Analysis**: 
   - Explained variance ratio
   - Component loadings
   - Visualization of results
4. **Optimization**: Determine optimal number of components

## Key Concepts Demonstrated:

- **Standardization**: Essential for PCA when features have different scales
- **Explained Variance**: Shows how much information each component captures
- **Component Loadings**: Indicates feature importance in each principal component
- **Visualization**: Scatter plots and heatmaps for understanding results

## Output Features:

- Scatter plot showing data points in 2D PCA space
- Explained variance ratios
- Component loadings matrix
- Cumulative variance plot for optimal component selection

This example demonstrates the complete workflow of PCA from data preparation to interpretation of results.

