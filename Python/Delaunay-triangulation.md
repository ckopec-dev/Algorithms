# Delaunay Triangulation in Python

Here's a complete example of implementing Delaunay triangulation using Python with the `scipy` library:

```python
import numpy as np
import matplotlib.pyplot as plt
from scipy.spatial import Delaunay

# Generate random points
np.random.seed(42)  # For reproducible results
points = np.random.rand(20, 2)  # 20 random points in 2D

# Create Delaunay triangulation
tri = Delaunay(points)

# Plot the results
plt.figure(figsize=(10, 8))

# Plot the points
plt.scatter(points[:, 0], points[:, 1], c='red', s=50, zorder=5)

# Plot the triangulation
for simplex in tri.simplices:
    triangle = points[simplex]
    plt.plot(triangle[:, 0], triangle[:, 1], 'b-', linewidth=1)

# Add point labels
for i, (x, y) in enumerate(points):
    plt.annotate(str(i), (x, y), xytext=(5, 5), textcoords='offset points', fontsize=8)

plt.title('Delaunay Triangulation')
plt.xlabel('X coordinate')
plt.ylabel('Y coordinate')
plt.grid(True, alpha=0.3)
plt.axis('equal')
plt.show()

# Print triangulation information
print(f"Number of points: {len(points)}")
print(f"Number of triangles: {len(tri.simplices)}")
print(f"Triangle indices:\n{tri.simplices}")
```

## Alternative Implementation Using Triangle Library

For more advanced features, you can also use the `triangle` library:

```python
# First install: pip install triangle
import triangle
import numpy as np
import matplotlib.pyplot as plt

# Generate random points
np.random.seed(42)
points = np.random.rand(20, 2)

# Create a dictionary for triangle input
vertices = points.tolist()
input_dict = {
    'vertices': vertices
}

# Generate triangulation
result = triangle.triangulate(input_dict, 'p')  # 'p' for plotting

# Plot the result
plt.figure(figsize=(10, 8))
plt.triplot(result['vertices'][:, 0], result['vertices'][:, 1], 
           result['triangles'], 'b-', linewidth=0.5)
plt.scatter(result['vertices'][:, 0], result['vertices'][:, 1], 
           c='red', s=20, zorder=5)
plt.title('Delaunay Triangulation using triangle library')
plt.axis('equal')
plt.show()
```

## Key Features of Delaunay Triangulation:

1. **Maximizes Minimum Angle**: Creates triangles with the largest possible minimum angle
2. **No Point Inside Circumcircle**: For any triangle in the triangulation, no other point lies inside its circumcircle
3. **Delaunay Property**: The triangulation is unique for a given set of points (unless there are degenerate cases)

## Output Description:

The code will display:
- Red dots representing the input points
- Blue lines forming the Delaunay triangulation
- Point numbers for identification
- Statistics about the triangulation (number of points and triangles)

The Delaunay triangulation is widely used in computer graphics, mesh generation, geographic information systems, and computational geometry applications.

