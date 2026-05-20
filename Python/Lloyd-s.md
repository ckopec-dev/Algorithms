# Lloyd's Algorithm Example in Python

Lloyd's algorithm is used for k-means clustering. Here's a complete implementation with an example:

```python
import numpy as np
import matplotlib.pyplot as plt
from sklearn.datasets import make_blobs

def lloyds_algorithm(X, k, max_iters=100, tol=1e-4):
    """
    Implement Lloyd's algorithm for k-means clustering
    
    Parameters:
    X: numpy array of shape (n_samples, n_features)
    k: number of clusters
    max_iters: maximum number of iterations
    tol: tolerance for convergence
    
    Returns:
    centroids: final cluster centroids
    labels: cluster labels for each point
    """
    
    # Initialize centroids randomly
    n_samples, n_features = X.shape
    centroids = X[np.random.choice(n_samples, k, replace=False)]
    
    for iteration in range(max_iters):
        # Assign points to closest centroid
        distances = np.sqrt(((X - centroids[:, np.newaxis])**2).sum(axis=2))
        labels = np.argmin(distances, axis=0)
        
        # Update centroids
        new_centroids = np.array([X[labels == i].mean(axis=0) for i in range(k)])
        
        # Check for convergence
        if np.all(np.abs(centroids - new_centroids) < tol):
            print(f"Converged after {iteration + 1} iterations")
            break
            
        centroids = new_centroids
    
    return centroids, labels

# Generate sample data
X, _ = make_blobs(n_samples=300, centers=4, cluster_std=0.60, random_state=0)

# Apply Lloyd's algorithm
k = 4
centroids, labels = lloyds_algorithm(X, k)

# Visualize results
plt.figure(figsize=(10, 8))
plt.scatter(X[:, 0], X[:, 1], c=labels, cmap='viridis', alpha=0.7)
plt.scatter(centroids[:, 0], centroids[:, 1], c='red', marker='x', s=200, linewidths=3, label='Centroids')
plt.title('Lloyd\'s Algorithm - K-Means Clustering')
plt.xlabel('Feature 1')
plt.ylabel('Feature 2')
plt.legend()
plt.grid(True, alpha=0.3)
plt.show()

# Print results
print("Final centroids:")
for i, centroid in enumerate(centroids):
    print(f"Centroid {i+1}: ({centroid[0]:.2f}, {centroid[1]:.2f})")

print(f"\nNumber of points in each cluster:")
unique, counts = np.unique(labels, return_counts=True)
for i, count in zip(unique, counts):
    print(f"Cluster {i}: {count} points")
```

## How it works:

1. **Initialization**: Randomly select k initial centroids
2. **Assignment**: Assign each data point to the nearest centroid
3. **Update**: Recalculate centroids as the mean of all points assigned to each cluster
4. **Repeat**: Continue steps 2-3 until convergence or maximum iterations

## Key Features:

- **Convergence Detection**: Stops when centroids don't change significantly
- **Flexible Input**: Works with any dimensional data
- **Visualization**: Shows clustering results with centroids marked
- **Progress Tracking**: Reports convergence iteration count

## Expected Output:
The algorithm will converge to a solution where data points are grouped into k clusters, with red 'x' markers showing the final cluster centroids. The exact results will vary due to random initialization, but the algorithm should consistently find reasonable cluster assignments.

