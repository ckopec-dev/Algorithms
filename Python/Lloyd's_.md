# Lloyd's Algorithm Example in Python

Lloyd's algorithm is used for k-means clustering. Here's a complete implementation with an example:

```python
import numpy as np
import matplotlib.pyplot as plt
from sklearn.datasets import make_blobs

def lloyds_algorithm(data, k, max_iters=100, tol=1e-4):
    """
    Implement Lloyd's algorithm for k-means clustering
    
    Parameters:
    data: numpy array of shape (n_samples, n_features)
    k: number of clusters
    max_iters: maximum number of iterations
    tol: tolerance for convergence
    
    Returns:
    centroids: final cluster centroids
    labels: cluster assignments for each data point
    """
    
    # Initialize centroids randomly
    centroids = data[np.random.choice(data.shape[0], k, replace=False)]
    
    for iteration in range(max_iters):
        # Assign points to closest centroid
        distances = np.sqrt(((data - centroids[:, np.newaxis])**2).sum(axis=2))
        labels = np.argmin(distances, axis=0)
        
        # Update centroids
        new_centroids = np.array([data[labels == i].mean(axis=0) for i in range(k)])
        
        # Check for convergence
        if np.all(np.abs(centroids - new_centroids) < tol):
            print(f"Converged after {iteration + 1} iterations")
            break
            
        centroids = new_centroids
    
    return centroids, labels

# Generate sample data
np.random.seed(42)
data, true_labels = make_blobs(n_samples=300, centers=4, cluster_std=0.60, random_state=0)

# Apply Lloyd's algorithm
k = 4
centroids, labels = lloyds_algorithm(data, k)

# Visualize results
plt.figure(figsize=(12, 5))

# Plot original data
plt.subplot(1, 2, 1)
plt.scatter(data[:, 0], data[:, 1], c=true_labels, cmap='viridis', alpha=0.6)
plt.title('Original Data with True Clusters')
plt.xlabel('Feature 1')
plt.ylabel('Feature 2')

# Plot clustered data
plt.subplot(1, 2, 2)
colors = ['red', 'blue', 'green', 'purple', 'orange']
for i in range(k):
    cluster_points = data[labels == i]
    plt.scatter(cluster_points[:, 0], cluster_points[:, 1], 
               c=colors[i], alpha=0.6, label=f'Cluster {i}')
    
# Plot centroids
plt.scatter(centroids[:, 0], centroids[:, 1], 
           c='black', marker='x', s=200, linewidths=3, label='Centroids')

plt.title('Lloyd\'s Algorithm Results')
plt.xlabel('Feature 1')
plt.ylabel('Feature 2')
plt.legend()
plt.tight_layout()
plt.show()

# Print final centroids
print("Final centroids:")
for i, centroid in enumerate(centroids):
    print(f"Centroid {i}: ({centroid[0]:.2f}, {centroid[1]:.2f})")

# Print cluster sizes
unique, counts = np.unique(labels, return_counts=True)
print("\nCluster sizes:")
for i, count in zip(unique, counts):
    print(f"Cluster {i}: {count} points")
```

## Key Components of Lloyd's Algorithm:

1. **Initialization**: Randomly select k initial centroids
2. **Assignment Step**: Assign each data point to the nearest centroid
3. **Update Step**: Recalculate centroids as the mean of assigned points
4. **Convergence**: Repeat until centroids don't change significantly

## Expected Output:
- The algorithm will converge to a local minimum of the k-means objective function
- Final centroids will represent the center of each cluster
- Cluster assignments will group similar points together

## Characteristics:
- **Time Complexity**: O(n*k*i*d) where n=points, k=clusters, i=iterations, d=dimensions
- **Space Complexity**: O(n*k + k*d)
- **Limitations**: May converge to local minima, sensitive to initial centroid placement

This implementation demonstrates the core mechanics of Lloyd's algorithm for unsupervised clustering.

