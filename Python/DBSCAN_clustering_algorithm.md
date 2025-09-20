# DBSCAN Clustering Algorithm Example

Here's a complete example of implementing DBSCAN clustering algorithm in Python:

```python
import numpy as np
import matplotlib.pyplot as plt
from sklearn.datasets import make_blobs
from sklearn.cluster import DBSCAN
from sklearn.preprocessing import StandardScaler

# Generate sample data
X, y_true = make_blobs(n_samples=300, centers=4, cluster_std=0.60, random_state=0)

# Standardize the data (important for DBSCAN)
X_scaled = StandardScaler().fit_transform(X)

# Apply DBSCAN clustering
dbscan = DBSCAN(eps=0.5, min_samples=5)
y_pred = dbscan.fit_predict(X_scaled)

# Get unique labels and count noise points
unique_labels = set(y_pred)
n_clusters_ = len(unique_labels) - (1 if -1 in y_pred else 0)
n_noise_ = list(y_pred).count(-1)

print(f"Number of clusters: {n_clusters_}")
print(f"Number of noise points: {n_noise_}")

# Plot the results
plt.figure(figsize=(12, 5))

# Plot 1: Original data
plt.subplot(1, 2, 1)
plt.scatter(X[:, 0], X[:, 1], c=y_true, cmap='viridis', alpha=0.7)
plt.title('Original Data')
plt.xlabel('Feature 1')
plt.ylabel('Feature 2')

# Plot 2: DBSCAN clustering results
plt.subplot(1, 2, 2)
colors = [plt.cm.Spectral(each) for each in np.linspace(0, 1, len(unique_labels))]

for k, col in zip(unique_labels, colors):
    if k == -1:
        # Black used for noise
        col = [0, 0, 0, 1]
    
    class_member_mask = (y_pred == k)
    xy = X_scaled[class_member_mask]
    plt.scatter(xy[:, 0], xy[:, 1], c=[col], s=50, alpha=0.7)

plt.title(f'DBSCAN Clustering\nClusters: {n_clusters_}, Noise: {n_noise_}')
plt.xlabel('Feature 1')
plt.ylabel('Feature 2')

plt.tight_layout()
plt.show()

# Print cluster information
print("\nCluster Information:")
for label in sorted(unique_labels):
    if label == -1:
        print(f"Noise points: {list(y_pred).count(-1)}")
    else:
        print(f"Cluster {label}: {list(y_pred).count(label)} points")
```

## Key Parameters Explained

- **eps (ε)**: The maximum distance between two samples for them to be considered as in the same neighborhood
- **min_samples**: The number of samples in a neighborhood for a point to be considered as a core point

## How DBSCAN Works

1. **Core Points**: Points that have at least `min_samples` within their ε-neighborhood
2. **Border Points**: Points that are in the neighborhood of core points but don't have enough neighbors themselves
3. **Noise Points**: Points that are neither core nor border points

## Advantages of DBSCAN

- Finds arbitrary shaped clusters
- Handles noise well (identifies outliers)
- Does not require specifying number of clusters beforehand
- Works well with datasets of varying densities

## Disadvantages of DBSCAN

- Sensitive to parameter selection (eps, min_samples)
- Struggles with varying density clusters
- Performance can degrade with high-dimensional data

This example demonstrates how DBSCAN automatically discovers the number of clusters and identifies noise points in your dataset.

