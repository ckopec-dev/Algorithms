# Fast Multipole Method (FMM) in Rust

Here's a simplified implementation of the Fast Multipole Method algorithm in Rust. This example demonstrates the core concepts of FMM for gravitational or electrostatic calculations.

```rust
use std::collections::HashMap;
use std::f64::consts::PI;

#[derive(Debug, Clone)]
struct Point {
    x: f64,
    y: f64,
    z: f64,
}

#[derive(Debug, Clone)]
struct Charge {
    position: Point,
    charge: f64,
}

#[derive(Debug, Clone)]
struct OctreeNode {
    center: Point,
    size: f64,
    charges: Vec<Charge>,
    children: Option<Vec<OctreeNode>>,
    is_leaf: bool,
    total_charge: f64,
    multipole_expansion: Option<MultipoleExpansion>,
}

#[derive(Debug, Clone)]
struct MultipoleExpansion {
    coefficients: Vec<f64>,
    order: usize,
}

impl OctreeNode {
    fn new(center: Point, size: f64) -> Self {
        OctreeNode {
            center,
            size,
            charges: Vec::new(),
            children: None,
            is_leaf: true,
            total_charge: 0.0,
            multipole_expansion: None,
        }
    }

    fn is_inside(&self, point: &Point) -> bool {
        let half_size = self.size / 2.0;
        point.x >= self.center.x - half_size && point.x <= self.center.x + half_size &&
        point.y >= self.center.y - half_size && point.y <= self.center.y + half_size &&
        point.z >= self.center.z - half_size && point.z <= self.center.z + half_size
    }

    fn insert(&mut self, charge: Charge) -> bool {
        if !self.is_inside(&charge.position) {
            return false;
        }

        if self.is_leaf && self.charges.len() < 4 {
            self.charges.push(charge);
            self.total_charge += charge.charge;
            return true;
        }

        if self.is_leaf {
            self.subdivide();
        }

        for child in self.children.as_mut().unwrap() {
            if child.insert(charge.clone()) {
                return true;
            }
        }

        false
    }

    fn subdivide(&mut self) {
        let half_size = self.size / 2.0;
        let mut children = Vec::new();
        
        for i in 0..2 {
            for j in 0..2 {
                for k in 0..2 {
                    let center = Point {
                        x: self.center.x + (i as f64 - 0.5) * half_size,
                        y: self.center.y + (j as f64 - 0.5) * half_size,
                        z: self.center.z + (k as f64 - 0.5) * half_size,
                    };
                    children.push(OctreeNode::new(center, half_size));
                }
            }
        }

        self.children = Some(children);
        self.is_leaf = false;

        // Reinsert existing charges
        let charges = std::mem::replace(&mut self.charges, Vec::new());
        for charge in charges {
            self.insert(charge);
        }
    }

    fn compute_multipole_expansion(&mut self) {
        if self.is_leaf {
            if !self.charges.is_empty() {
                let expansion = self.compute_local_expansion();
                self.multipole_expansion = Some(expansion);
            }
        } else if let Some(ref mut children) = self.children {
            for child in children.iter_mut() {
                child.compute_multipole_expansion();
            }
            
            // Compute multipole expansion for this node
            self.multipole_expansion = Some(self.compute_upward_sweep());
        }
    }

    fn compute_local_expansion(&self) -> MultipoleExpansion {
        // Simplified local expansion computation
        // In practice, this would involve computing multipole coefficients
        let order = 3; // Simplified to order 3
        let mut coefficients = vec![0.0; order * order];
        
        for charge in &self.charges {
            // Simplified computation - in reality, this would involve spherical harmonics
            coefficients[0] += charge.charge;
        }
        
        MultipoleExpansion { coefficients, order }
    }

    fn compute_upward_sweep(&self) -> MultipoleExpansion {
        // Simplified upward sweep computation
        let order = 3;
        let mut coefficients = vec![0.0; order * order];
        
        if let Some(ref children) = self.children {
            for child in children {
                if let Some(ref expansion) = child.multipole_expansion {
                    for i in 0..expansion.coefficients.len() {
                        coefficients[i] += expansion.coefficients[i];
                    }
                }
            }
        }
        
        MultipoleExpansion { coefficients, order }
    }

    fn evaluate_potential(&self, target: &Point) -> f64 {
        if self.is_leaf {
            let mut potential = 0.0;
            for charge in &self.charges {
                let dx = target.x - charge.position.x;
                let dy = target.y - charge.position.y;
                let dz = target.z - charge.position.z;
                let distance = (dx * dx + dy * dy + dz * dz).sqrt();
                
                if distance > 1e-10 {
                    potential += charge.charge / distance;
                }
            }
            return potential;
        }

        // Use multipole expansion for distant nodes
        if let Some(ref expansion) = self.multipole_expansion {
            let dx = target.x - self.center.x;
            let dy = target.y - self.center.y;
            let dz = target.z - self.center.z;
            let distance = (dx * dx + dy * dy + dz * dz).sqrt();
            
            // Simplified multipole evaluation
            if distance > self.size {
                // Use multipole expansion
                return self.evaluate_multipole(target);
            }
        }

        // Recursively evaluate children
        if let Some(ref children) = self.children {
            let mut potential = 0.0;
            for child in children {
                potential += child.evaluate_potential(target);
            }
            return potential;
        }

        0.0
    }

    fn evaluate_multipole(&self, target: &Point) -> f64 {
        // Simplified multipole evaluation
        // In practice, this would involve evaluating spherical harmonics
        if let Some(ref expansion) = self.multipole_expansion {
            let dx = target.x - self.center.x;
            let dy = target.y - self.center.y;
            let dz = target.z - self.center.z;
            let distance = (dx * dx + dy * dy + dz * dz).sqrt();
            
            if distance > 1e-10 {
                // Simplified: just return the monopole term
                return expansion.coefficients[0] / distance;
            }
        }
        0.0
    }
}

struct FastMultipoleMethod {
    root: OctreeNode,
    charges: Vec<Charge>,
}

impl FastMultipoleMethod {
    fn new(center: Point, size: f64) -> Self {
        FastMultipoleMethod {
            root: OctreeNode::new(center, size),
            charges: Vec::new(),
        }
    }

    fn add_charge(&mut self, charge: Charge) {
        self.charges.push(charge.clone());
        self.root.insert(charge);
    }

    fn build_tree(&mut self) {
        self.root.compute_multipole_expansion();
    }

    fn evaluate_potential(&self, target: &Point) -> f64 {
        self.root.evaluate_potential(target)
    }

    fn evaluate_all_potentials(&self, targets: &[Point]) -> Vec<f64> {
        targets.iter().map(|target| self.evaluate_potential(target)).collect()
    }
}

// Example usage
fn main() {
    // Create FMM system with root node centered at origin, size 100
    let mut fmm = FastMultipoleMethod::new(
        Point { x: 0.0, y: 0.0, z: 0.0 },
        100.0
    );

    // Add some charges
    fmm.add_charge(Charge {
        position: Point { x: 10.0, y: 10.0, z: 0.0 },
        charge: 1.0,
    });

    fmm.add_charge(Charge {
        position: Point { x: -10.0, y: -10.0, z: 0.0 },
        charge: -1.0,
    });

    fmm.add_charge(Charge {
        position: Point { x: 0.0, y: 20.0, z: 0.0 },
        charge: 2.0,
    });

    // Build the tree
    fmm.build_tree();

    // Evaluate potential at some points
    let test_points = vec![
        Point { x: 0.0, y: 0.0, z: 0.0 },
        Point { x: 5.0, y: 5.0, z: 0.0 },
        Point { x: 15.0, y: 15.0, z: 0.0 },
    ];

    println!("Potential at test points:");
    for point in &test_points {
        let potential = fmm.evaluate_potential(point);
        println!("Point ({:.1}, {:.1}, {:.1}): {:.4}", 
                 point.x, point.y, point.z, potential);
    }

    // Evaluate all potentials at once
    let potentials = fmm.evaluate_all_potentials(&test_points);
    println!("\nAll potentials: {:?}", potentials);
}
```

## Key Components Explained

### 1. **Octree Structure**
- `OctreeNode`: Represents nodes in the 3D octree
- Each node can have up to 8 children (octants)
- Stores charges and multipole expansions

### 2. **Multipole Expansion**
- `MultipoleExpansion`: Stores coefficients for the multipole expansion
- Used to approximate far-field interactions efficiently

### 3. **Core Algorithms**
- **Insertion**: Adds charges to the octree structure
- **Subdivision**: Splits nodes when they exceed capacity
- **Upward Sweep**: Computes multipole expansions from leaves to root
- **Downward Sweep**: Evaluates potentials using the tree structure

### 4. **Performance Benefits**
- **O(N log N)** complexity instead of O(N²) for direct summation
- **Hierarchical approximation** for distant interactions
- **Adaptive tree refinement** based on charge distribution

This implementation provides a foundation for FMM that can be extended with:
- More sophisticated multipole expansion calculations
- Spherical harmonics for better accuracy
- Parallel processing capabilities
- Memory optimization techniques

The algorithm efficiently handles large numbers of particles by grouping distant charges together and computing their collective effect using multipole expansions rather than individual pairwise interactions.

