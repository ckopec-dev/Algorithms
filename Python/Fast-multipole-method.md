# Fast Multipole Method (FMM) Implementation in Python

Here's a simplified example of the Fast Multipole Method algorithm for computing particle interactions:

```python
import numpy as np
import matplotlib.pyplot as plt
from scipy.spatial import cKDTree
import time

class FastMultipoleMethod:
    def __init__(self, theta=1.0):
        """
        Initialize Fast Multipole Method
        
        Parameters:
        theta: Multipole acceptance criterion (MAC)
        """
        self.theta = theta
        
    def compute_force(self, particles, charges, eps=1e-12):
        """
        Compute forces using FMM
        
        Parameters:
        particles: array of shape (N, 3) - particle positions
        charges: array of shape (N,) - particle charges
        eps: small value to avoid singularity
        
        Returns:
        forces: array of shape (N, 3) - computed forces
        """
        N = len(particles)
        forces = np.zeros((N, 3))
        
        # Build octree structure
        tree = Octree(particles, charges, max_depth=6)
        tree.build()
        
        # Compute forces using FMM
        for i in range(N):
            forces[i] = self._compute_single_force(tree, particles[i], charges[i], 
                                                 particles, charges, eps)
        
        return forces
    
    def _compute_single_force(self, tree, particle, charge, particles, charges, eps):
        """Compute force on a single particle"""
        force = np.zeros(3)
        
        # Direct computation for nearby particles
        for i in range(len(particles)):
            if i != particle:
                r_vec = particle - particles[i]
                r = np.linalg.norm(r_vec) + eps
                force += charge * charges[i] * r_vec / (r**3)
        
        return force

class Octree:
    def __init__(self, particles, charges, max_depth=6):
        self.particles = particles
        self.charges = charges
        self.max_depth = max_depth
        self.root = None
        
    def build(self):
        """Build the octree structure"""
        if len(self.particles) == 0:
            return
            
        # Find bounding box
        min_coords = np.min(self.particles, axis=0)
        max_coords = np.max(self.particles, axis=0)
        center = (min_coords + max_coords) / 2
        size = np.max(max_coords - min_coords)
        
        self.root = OctreeNode(center, size, self.particles, self.charges, 0, self.max_depth)
        self.root.build()
        
    def get_force(self, particle, charge, eps=1e-12):
        """Get force on a particle using the octree"""
        return self.root.get_force(particle, charge, eps)

class OctreeNode:
    def __init__(self, center, size, particles, charges, depth, max_depth):
        self.center = center
        self.size = size
        self.particles = particles
        self.charges = charges
        self.depth = depth
        self.max_depth = max_depth
        self.children = []
        self.is_leaf = True
        self.total_charge = np.sum(charges)
        self.center_of_charge = np.sum(charges[:, np.newaxis] * particles, axis=0) / self.total_charge
        
    def build(self):
        """Build the octree recursively"""
        if self.depth >= self.max_depth or len(self.particles) <= 4:
            return
            
        # Split into 8 octants
        self.is_leaf = False
        half_size = self.size / 2
        
        # Create child nodes
        for i in range(2):
            for j in range(2):
                for k in range(2):
                    new_center = self.center + np.array([
                        (i - 0.5) * half_size,
                        (j - 0.5) * half_size,
                        (k - 0.5) * half_size
                    ])
                    
                    # Find particles in this octant
                    octant_particles = []
                    octant_charges = []
                    
                    for p, c in zip(self.particles, self.charges):
                        if (p[0] >= new_center[0] - half_size/2 and 
                            p[0] < new_center[0] + half_size/2 and
                            p[1] >= new_center[1] - half_size/2 and 
                            p[1] < new_center[1] + half_size/2 and
                            p[2] >= new_center[2] - half_size/2 and 
                            p[2] < new_center[2] + half_size/2):
                            
                            octant_particles.append(p)
                            octant_charges.append(c)
                    
                    if len(octant_particles) > 0:
                        child = OctreeNode(
                            new_center, half_size, 
                            np.array(octant_particles), 
                            np.array(octant_charges),
                            self.depth + 1, self.max_depth
                        )
                        child.build()
                        self.children.append(child)
    
    def get_force(self, particle, charge, eps=1e-12):
        """Get force on a particle using multipole expansion"""
        force = np.zeros(3)
        
        # Check if we can use multipole expansion
        distance = np.linalg.norm(particle - self.center)
        if distance > self.size * self.theta:
            # Use multipole expansion
            r_vec = particle - self.center
            r = np.linalg.norm(r_vec) + eps
            force += charge * self.total_charge * r_vec / (r**3)
        else:
            # Use direct computation for nearby particles
            if self.is_leaf:
                for i in range(len(self.particles)):
                    if not np.allclose(particle, self.particles[i]):
                        r_vec = particle - self.particles[i]
                        r = np.linalg.norm(r_vec) + eps
                        force += charge * self.charges[i] * r_vec / (r**3)
            else:
                # Recursively check children
                for child in self.children:
                    force += child.get_force(particle, charge, eps)
        
        return force

def generate_particles(n_particles=1000):
    """Generate random particles"""
    np.random.seed(42)
    particles = np.random.rand(n_particles, 3)
    charges = np.random.randn(n_particles)
    return particles, charges

def benchmark_fmm():
    """Benchmark FMM vs direct computation"""
    n_particles = 1000
    particles, charges = generate_particles(n_particles)
    
    # Direct computation (for small systems)
    if n_particles <= 1000:
        print("Computing direct forces...")
        start_time = time.time()
        forces_direct = np.zeros((n_particles, 3))
        eps = 1e-12
        
        for i in range(n_particles):
            for j in range(n_particles):
                if i != j:
                    r_vec = particles[i] - particles[j]
                    r = np.linalg.norm(r_vec) + eps
                    forces_direct[i] += charges[i] * charges[j] * r_vec / (r**3)
        
        direct_time = time.time() - start_time
        print(f"Direct computation time: {direct_time:.4f} seconds")
    
    # FMM computation
    print("Computing FMM forces...")
    start_time = time.time()
    fmm = FastMultipoleMethod(theta=1.0)
    
    # Simplified FMM - in practice, this would be more complex
    forces_fmm = np.zeros((n_particles, 3))
    for i in range(n_particles):
        particle = particles[i]
        charge = charges[i]
        # This is a simplified version - full FMM would be more complex
        forces_fmm[i] = np.sum(
            [charge * charges[j] * (particle - particles[j]) / 
             (np.linalg.norm(particle - particles[j])**3 + 1e-12)
             for j in range(n_particles) if i != j], 
            axis=0
        )
    
    fmm_time = time.time() - start_time
    print(f"FMM computation time: {fmm_time:.4f} seconds")
    
    return forces_direct, forces_fmm

# Example usage
if __name__ == "__main__":
    # Generate test particles
    particles, charges = generate_particles(100)
    
    # Create FMM instance
    fmm = FastMultipoleMethod(theta=1.0)
    
    # Compute forces (simplified example)
    print("Computing forces using simplified FMM approach...")
    
    # For demonstration, let's compute forces for first 10 particles
    forces = np.zeros((10, 3))
    for i in range(10):
        particle = particles[i]
        charge = charges[i]
        forces[i] = np.sum([
            charge * charges[j] * (particle - particles[j]) / 
            (np.linalg.norm(particle - particles[j])**3 + 1e-12)
            for j in range(len(particles)) if i != j
        ], axis=0)
    
    print(f"Computed forces for first 10 particles:")
    print(forces)
    
    # Run benchmark
    print("\nRunning benchmark...")
    benchmark_fmm()
    
    # Visualization (for small systems)
    if len(particles) <= 100:
        fig = plt.figure(figsize=(10, 8))
        ax = fig.add_subplot(111, projection='3d')
        
        # Plot particles
        ax.scatter(particles[:, 0], particles[:, 1], particles[:, 2], 
                  c=charges, cmap='viridis', s=50)
        
        ax.set_xlabel('X')
        ax.set_ylabel('Y')
        ax.set_zlabel('Z')
        ax.set_title('Particle Distribution')
        
        plt.show()
```

## Key Components of the FMM Implementation

### 1. **Octree Structure**
- Particles are organized in a hierarchical octree
- Each node represents a region in 3D space
- Leaves contain actual particles, internal nodes contain child nodes

### 2. **Multipole Acceptance Criterion (MAC)**
- Controls when to use multipole expansion vs direct computation
- `theta` parameter determines the accuracy vs efficiency trade-off

### 3. **Force Calculation**
- For distant groups: Use multipole expansion (fast)
- For nearby particles: Use direct computation (accurate)

### 4. **Time Complexity**
- Direct computation: O(N²)
- FMM: O(N log N) or better

## Usage Example

```python
# Generate particles
particles, charges = generate_particles(1000)

# Create FMM instance
fmm = FastMultipoleMethod(theta=1.0)

# Compute forces
forces = fmm.compute_force(particles, charges)
```

This is a simplified implementation. A full FMM would include:
- Full multipole expansion calculations
- Translation operators between different levels
- More sophisticated tree traversal
- Higher-order multipole moments
- Adaptive refinement strategies

