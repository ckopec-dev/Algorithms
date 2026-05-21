# K-Means Clustering Example in Python

```python
import numpy as np
import matplotlib.pyplot as plt
from sklearn.cluster import KMeans
from sklearn.datasets import make_blobs

# Generate sample data
X, y_true = make_blobs(n_samples=300, centers=4, cluster_std=0.60, random_state=0)

# Apply K-means clustering
kmeans = KMeans(n_clusters=4, random_state=0, n_init=10)
y_kmeans = kmeans.fit_predict(X)

# Get cluster centers
centers = kmeans.cluster_centers_

# Plot the results
plt.figure(figsize=(10, 8))

# Plot data points
plt.scatter(X[:, 0], X[:, 1], c=y_kmeans, cmap='viridis', alpha=0.7)

# Plot cluster centers
plt.scatter(centers[:, 0], centers[:, 1], c='red', marker='x', s=200, linewidths=3, label='Centroids')

# Add labels
plt.xlabel('Feature 1')
plt.ylabel('Feature 2')
plt.title('K-Means Clustering Results')
plt.legend()
plt.grid(True, alpha=0.3)

plt.show()

# Print cluster centers
print("Cluster Centers:")
for i, center in enumerate(centers):
    print(f"Cluster {i}: ({center[0]:.2f}, {center[1]:.2f})")

# Print inertia (within-cluster sum of squares)
print(f"\nInertia: {kmeans.inertia_:.2f}")
```

## Alternative Example with Manual Implementation

```python
import numpy as np
import matplotlib.pyplot as plt

def kmeans_manual(X, k, max_iters=100):
    """
    Manual implementation of K-means clustering
    """
    # Initialize centroids randomly
    centroids = X[np.random.choice(X.shape[0], k, replace=False)]
    
    for _ in range(max_iters):
        # Assign points to closest centroid
        distances = np.sqrt(((X - centroids[:, np.newaxis])**2).sum(axis=2))
        labels = np.argmin(distances, axis=0)
        
        # Update centroids
        new_centroids = np.array([X[labels == i].mean(axis=0) for i in range(k)])
        
        # Check for convergence
        if np.allclose(centroids, new_centroids):
            break
            
        centroids = new_centroids
    
    return labels, centroids

# Generate sample data
np.random.seed(42)
X = np.random.randn(100, 2)

# Apply manual K-means
labels, centroids = kmeans_manual(X, k=3)

# Plot results
plt.figure(figsize=(10, 8))
plt.scatter(X[:, 0], X[:, 1], c=labels, cmap='viridis', alpha=0.7)
plt.scatter(centroids[:, 0], centroids[:, 1], c='red', marker='x', s=200, linewidths=3)
plt.title('Manual K-Means Clustering')
plt.xlabel('Feature 1')
plt.ylabel('Feature 2')
plt.grid(True, alpha=0.3)
plt.show()

print("Manual K-means completed!")
```

## Key Points:

- **K-means** is an unsupervised learning algorithm that partitions data into K clusters
- **Steps**: Initialize centroids → Assign points to clusters → Update centroids → Repeat until convergence
- **Parameters**: `n_clusters` (number of clusters), `max_iter` (maximum iterations), `n_init` (number of runs)
- **Use Cases**: Customer segmentation, image compression, anomaly detection
- **Limitations**: Requires pre-specifying number of clusters, sensitive to initial centroids

The algorithm minimizes the within-cluster sum of squares (inertia) to find optimal cluster assignments.

