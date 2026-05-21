# K-Medoids Clustering Example in Python

```python
import numpy as np
import matplotlib.pyplot as plt
from sklearn.datasets import make_blobs
from sklearn.cluster import KMeans, AgglomerativeClustering
from sklearn.metrics import adjusted_rand_score
from sklearn.metrics.pairwise import euclidean_distances
import pandas as pd

# Generate sample data
np.random.seed(42)
X, y_true = make_blobs(n_samples=300, centers=4, cluster_std=0.60, random_state=0)

# Display the generated data
plt.figure(figsize=(10, 8))
plt.scatter(X[:, 0], X[:, 1], c=y_true, cmap='viridis', alpha=0.7)
plt.title('Original Data with True Clusters')
plt.xlabel('Feature 1')
plt.ylabel('Feature 2')
plt.show()

# Implement K-Medoids manually
def k_medoids(X, k, max_iters=100):
    """
    K-Medoids clustering algorithm implementation
    """
    n_samples = X.shape[0]
    
    # Initialize medoids randomly
    medoids_indices = np.random.choice(n_samples, k, replace=False)
    medoids = X[medoids_indices]
    
    for iteration in range(max_iters):
        # Assign each point to the nearest medoid
        distances = euclidean_distances(X, medoids)
        labels = np.argmin(distances, axis=1)
        
        # Update medoids
        new_medoids = []
        for i in range(k):
            # Find all points assigned to cluster i
            cluster_points = X[labels == i]
            if len(cluster_points) == 0:
                new_medoids.append(medoids[i])
                continue
                
            # Calculate total distance for each point in cluster
            distances_to_points = euclidean_distances(cluster_points, cluster_points)
            total_distances = np.sum(distances_to_points, axis=1)
            
            # Choose the point with minimum total distance as new medoid
            min_distance_idx = np.argmin(total_distances)
            new_medoids.append(cluster_points[min_distance_idx])
        
        new_medoids = np.array(new_medoids)
        
        # Check for convergence
        if np.allclose(medoids, new_medoids):
            break
            
        medoids = new_medoids
    
    return labels, medoids

# Apply K-Medoids
k = 4
labels_kmedoids, medoids = k_medoids(X, k)

# Compare with K-Means
kmeans = KMeans(n_clusters=4, random_state=0, n_init=10)
labels_kmeans = kmeans.fit_predict(X)

# Display results
fig, axes = plt.subplots(1, 3, figsize=(15, 5))

# Original data
axes[0].scatter(X[:, 0], X[:, 1], c=y_true, cmap='viridis', alpha=0.7)
axes[0].set_title('Original Data')
axes[0].set_xlabel('Feature 1')
axes[0].set_ylabel('Feature 2')

# K-Medoids results
scatter = axes[1].scatter(X[:, 0], X[:, 1], c=labels_kmedoids, cmap='viridis', alpha=0.7)
axes[1].scatter(medoids[:, 0], medoids[:, 1], c='red', marker='x', s=200, linewidths=3, label='Medoids')
axes[1].set_title('K-Medoids Clustering')
axes[1].set_xlabel('Feature 1')
axes[1].set_ylabel('Feature 2')
axes[1].legend()

# K-Means results
axes[2].scatter(X[:, 0], X[:, 1], c=labels_kmeans, cmap='viridis', alpha=0.7)
axes[2].set_title('K-Means Clustering')
axes[2].set_xlabel('Feature 1')
axes[2].set_ylabel('Feature 2')

plt.tight_layout()
plt.show()

# Calculate clustering metrics
ari_kmedoids = adjusted_rand_score(y_true, labels_kmedoids)
ari_kmeans = adjusted_rand_score(y_true, labels_kmeans)

print("Clustering Performance Comparison:")
print(f"K-Medoids ARI: {ari_kmedoids:.4f}")
print(f"K-Means ARI: {ari_kmeans:.4f}")

# Using scikit-learn's implementation of K-Medoids (PAM)
from sklearn_extra.cluster import KMedoids

# Note: You may need to install sklearn-extra: pip install sklearn-extra
try:
    kmedoids_sklearn = KMedoids(n_clusters=4, random_state=0)
    labels_sklearn = kmedoids_sklearn.fit_predict(X)
    
    print(f"Scikit-learn K-Medoids ARI: {adjusted_rand_score(y_true, labels_sklearn):.4f}")
    print(f"Number of medoids found: {len(kmedoids_sklearn.medoid_indices_)}")
    
    # Plot scikit-learn results
    plt.figure(figsize=(8, 6))
    plt.scatter(X[:, 0], X[:, 1], c=labels_sklearn, cmap='viridis', alpha=0.7)
    plt.scatter(kmedoids_sklearn.cluster_centers_[:, 0], 
               kmedoids_sklearn.cluster_centers_[:, 1], 
               c='red', marker='x', s=200, linewidths=3, label='Medoids')
    plt.title('K-Medoids (Scikit-learn Implementation)')
    plt.xlabel('Feature 1')
    plt.ylabel('Feature 2')
    plt.legend()
    plt.show()
    
except ImportError:
    print("sklearn-extra not installed. Install with: pip install sklearn-extra")
    print("To use KMedoids from sklearn_extra.cluster")

# Example with different dataset
print("\n" + "="*50)
print("Example with Simple 2D Dataset")
print("="*50)

# Create a simple dataset
simple_data = np.array([
    [1, 2], [1, 4], [1, 0],
    [4, 2], [4, 4], [4, 0],
    [7, 2], [7, 4], [7, 0]
])

print("Simple dataset:")
print(simple_data)

# Apply K-Medoids
labels_simple, medoids_simple = k_medoids(simple_data, 3)

print(f"\nCluster labels: {labels_simple}")
print(f"Medoids: {medoids_simple}")

# Visualization of simple example
plt.figure(figsize=(8, 6))
plt.scatter(simple_data[:, 0], simple_data[:, 1], c=labels_simple, cmap='viridis', s=100, alpha=0.7)
plt.scatter(medoids_simple[:, 0], medoids_simple[:, 1], c='red', marker='x', s=200, linewidths=3, label='Medoids')
plt.title('Simple 2D Dataset - K-Medoids Clustering')
plt.xlabel('X coordinate')
plt.ylabel('Y coordinate')
plt.legend()
plt.grid(True, alpha=0.3)
plt.show()
```

## Key Features of K-Medoids Algorithm:

1. **Robust to outliers**: Unlike K-Means, K-Medoids uses actual data points as medoids
2. **Handles non-spherical clusters**: More flexible than K-Means
3. **Uses distance matrix**: Works with any distance metric
4. **Stable convergence**: Less sensitive to initialization

## Advantages:
- More robust to outliers
- Medoids are actual data points
- Better handling of noise
- Works with any distance metric

## Disadvantages:
- Computationally more expensive
- No closed-form solution
- Sensitive to number of clusters
- Requires distance matrix computation

## When to Use:
- When data contains outliers
- When you need actual data points as cluster representatives
- When dealing with non-spherical clusters
- When robustness to noise is important

This implementation demonstrates both manual K-Medoids and the scikit-learn approach, showing how the algorithm works with different datasets and comparing it with traditional K-Means clustering.

