# Iterative Closest Point (ICP) Algorithm Example

Here's a complete implementation of the ICP algorithm in Python using NumPy and Matplotlib for visualization:

```python
import numpy as np
import matplotlib.pyplot as plt
from scipy.spatial.distance import cdist

def icp(source_points, target_points, max_iterations=50, tolerance=1e-6):
    """
    Iterative Closest Point algorithm implementation
    
    Parameters:
    source_points: numpy array of shape (n, 2) - source point cloud
    target_points: numpy array of shape (n, 2) - target point cloud
    max_iterations: maximum number of iterations
    tolerance: convergence threshold
    
    Returns:
    transformed_source: numpy array of shape (n, 2) - transformed source points
    transformation: numpy array of shape (3, 3) - transformation matrix
    """
    
    # Initialize transformation matrix (identity)
    transformation = np.eye(3)
    
    # Make a copy of source points
    source = source_points.copy()
    
    for iteration in range(max_iterations):
        # Find closest points in target for each source point
        distances = cdist(source, target_points)
        closest_indices = np.argmin(distances, axis=1)
        
        # Get corresponding target points
        target_correspondences = target_points[closest_indices]
        
        # Compute transformation using least squares
        # This is a simplified version - in practice, you might want to use SVD
        # for more robust estimation
        
        # Compute centroids
        source_centroid = np.mean(source, axis=0)
        target_centroid = np.mean(target_correspondences, axis=0)
        
        # Center the points
        source_centered = source - source_centroid
        target_centered = target_correspondences - target_centroid
        
        # Compute rotation matrix using SVD (more robust approach)
        H = np.dot(source_centered.T, target_centered)
        U, S, Vt = np.linalg.svd(H)
        R = np.dot(Vt.T, U.T)
        
        # Handle reflection case
        if np.linalg.det(R) < 0:
            Vt[-1, :] *= -1
            R = np.dot(Vt.T, U.T)
        
        # Compute translation
        t = target_centroid - np.dot(R, source_centroid)
        
        # Update transformation matrix
        transformation[:2, :2] = R
        transformation[:2, 2] = t
        
        # Apply transformation to source points
        source_homogeneous = np.vstack([source.T, np.ones(source.shape[0])])
        source_transformed = np.dot(transformation, source_homogeneous).T[:, :2]
        
        # Check for convergence
        if np.mean(np.linalg.norm(source_transformed - target_correspondences, axis=1)) < tolerance:
            print(f"Converged after {iteration + 1} iterations")
            break
            
        source = source_transformed
    
    return source_transformed, transformation

def generate_sample_data():
    """Generate sample point clouds for demonstration"""
    
    # Generate target points (original shape)
    theta = np.linspace(0, 2*np.pi, 50)
    target_points = np.column_stack([np.cos(theta), np.sin(theta)])
    
    # Add some noise to make it more realistic
    target_points += np.random.normal(0, 0.05, target_points.shape)
    
    # Generate source points (rotated and translated version of target)
    angle = np.pi/4  # 45 degrees rotation
    rotation_matrix = np.array([[np.cos(angle), -np.sin(angle)],
                               [np.sin(angle), np.cos(angle)]])
    
    source_points = np.dot(target_points, rotation_matrix.T)
    source_points += np.array([0.5, 0.3])  # Add translation
    
    return source_points, target_points

# Main execution
if __name__ == "__main__":
    # Generate sample data
    source_points, target_points = generate_sample_data()
    
    # Apply ICP algorithm
    print("Running ICP algorithm...")
    transformed_source, transformation = icp(source_points, target_points, max_iterations=30)
    
    print(f"Transformation matrix:\n{transformation}")
    
    # Visualization
    plt.figure(figsize=(10, 8))
    
    # Plot target points (blue)
    plt.scatter(target_points[:, 0], target_points[:, 1], 
               c='blue', marker='o', s=50, alpha=0.7, label='Target Points')
    
    # Plot original source points (red)
    plt.scatter(source_points[:, 0], source_points[:, 1], 
               c='red', marker='x', s=50, alpha=0.7, label='Original Source Points')
    
    # Plot transformed source points (green)
    plt.scatter(transformed_source[:, 0], transformed_source[:, 1], 
               c='green', marker='^', s=50, alpha=0.7, label='Transformed Source Points')
    
    # Plot connections between corresponding points (for visualization)
    distances = cdist(source_points, target_points)
    closest_indices = np.argmin(distances, axis=1)
    
    for i in range(0, len(source_points), 5):  # Show every 5th connection
        plt.plot([source_points[i, 0], target_points[closest_indices[i], 0]],
                [source_points[i, 1], target_points[closest_indices[i], 1]],
                'k--', alpha=0.3, linewidth=0.5)
    
    plt.xlabel('X coordinate')
    plt.ylabel('Y coordinate')
    plt.title('Iterative Closest Point (ICP) Algorithm')
    plt.legend()
    plt.axis('equal')
    plt.grid(True, alpha=0.3)
    
    plt.tight_layout()
    plt.show()
    
    # Print some statistics
    print(f"Number of points: {len(source_points)}")
    print(f"Final RMS error: {np.sqrt(np.mean(np.sum((transformed_source - target_points)**2, axis=1)))}")
```

## Key Features of This Implementation:

### 1. **Core ICP Algorithm Components:**
- **Closest Point Matching**: Uses scipy's `cdist` to find nearest neighbors
- **Transformation Estimation**: Computes rotation and translation using SVD
- **Iterative Refinement**: Repeatedly applies transformation until convergence

### 2. **Mathematical Foundation:**
- Uses **Singular Value Decomposition (SVD)** for robust rotation estimation
- Handles **reflection** cases properly
- Implements **least squares** optimization

### 3. **Convergence Criteria:**
- Stops when mean distance between corresponding points is below tolerance
- Maximum iteration limit to prevent infinite loops

### 4. **Visualization Features:**
- Shows original and transformed point clouds
- Displays connections between corresponding points
- Provides transformation matrix output

## Usage Example:

```python
# Simple usage
source_points = np.array([[0, 0], [1, 0], [0, 1], [1, 1]])
target_points = np.array([[0.1, 0.1], [1.1, 0.1], [0.1, 1.1], [1.1, 1.1]])

transformed_points, transform_matrix = icp(source_points, target_points)
```

This implementation provides a solid foundation for ICP that can be extended for 3D point clouds, different distance metrics, or more sophisticated convergence criteria.

