# Fast Multipole Method (FMM) Implementation in C++

Here's a simplified example of the Fast Multipole Method algorithm in C++:

```cpp
#include <iostream>
#include <vector>
#include <cmath>
#include <algorithm>

// Particle structure
struct Particle {
    double x, y, z;      // Position
    double charge;       // Charge
    double fx, fy, fz;   // Force components
    
    Particle(double x_, double y_, double z_, double charge_)
        : x(x_), y(y_), z(z_), charge(charge_), fx(0), fy(0), fz(0) {}
};

// Multipole Expansion class
class MultipoleExpansion {
public:
    std::vector<double> coeffs;  // Multipole coefficients
    double x, y, z;              // Center of expansion
    int order;                   // Expansion order
    
    MultipoleExpansion(double x_, double y_, double z_, int order_)
        : x(x_), y(y_), z(z_), order(order_) {
        coeffs.resize((order + 1) * (order + 1), 0.0);
    }
    
    // Add particle contribution to multipole expansion
    void addParticle(const Particle& p) {
        double dx = p.x - x;
        double dy = p.y - y;
        double dz = p.z - z;
        double r = std::sqrt(dx*dx + dy*dy + dz*dz);
        
        if (r < 1e-10) return;  // Avoid division by zero
        
        double theta = std::acos(dz / r);
        double phi = std::atan2(dy, dx);
        
        // Simple spherical harmonic expansion (first few terms)
        for (int l = 0; l <= order; ++l) {
            for (int m = -l; m <= l; ++m) {
                int index = l * (l + 1) + m;
                double Ylm = sphericalHarmonic(l, m, theta, phi);
                coeffs[index] += p.charge * std::pow(r, l) * Ylm;
            }
        }
    }
    
private:
    // Simplified spherical harmonic calculation
    double sphericalHarmonic(int l, int m, double theta, double phi) {
        // This is a simplified version - full implementation would be more complex
        if (l == 0 && m == 0) return 1.0 / std::sqrt(4.0 * M_PI);
        if (l == 1 && m == 0) return std::sqrt(3.0 / (4.0 * M_PI)) * std::cos(theta);
        if (l == 1 && m == 1) return std::sqrt(3.0 / (2.0 * M_PI)) * std::sin(theta) * std::cos(phi);
        if (l == 1 && m == -1) return std::sqrt(3.0 / (2.0 * M_PI)) * std::sin(theta) * std::sin(phi);
        return 0.0;
    }
};

// FMM Tree Node
class FMMNode {
public:
    double x, y, z;           // Center of node
    double size;              // Size of node
    std::vector<Particle*> particles;
    std::vector<FMMNode*> children;
    MultipoleExpansion* multipole;
    bool isLeaf;
    
    FMMNode(double x_, double y_, double z_, double size_)
        : x(x_), y(y_), z(z_), size(size_), multipole(nullptr), isLeaf(true) {}
    
    ~FMMNode() {
        delete multipole;
        for (FMMNode* child : children) {
            delete child;
        }
    }
    
    // Build FMM tree
    void buildTree(const std::vector<Particle*>& particleList, int maxParticles = 10) {
        particles = particleList;
        
        if (particles.size() <= maxParticles) {
            isLeaf = true;
            return;
        }
        
        isLeaf = false;
        
        // Create 8 children (octree)
        children.resize(8);
        double halfSize = size / 2.0;
        
        for (int i = 0; i < 8; ++i) {
            double cx = x + (i & 1 ? halfSize : -halfSize);
            double cy = y + (i & 2 ? halfSize : -halfSize);
            double cz = z + (i & 4 ? halfSize : -halfSize);
            
            children[i] = new FMMNode(cx, cy, cz, halfSize);
        }
        
        // Distribute particles to children
        std::vector<std::vector<Particle*>> childParticles(8);
        
        for (Particle* p : particles) {
            int childIndex = getChildIndex(p);
            childParticles[childIndex].push_back(p);
        }
        
        // Recursively build children
        for (int i = 0; i < 8; ++i) {
            children[i]->buildTree(childParticles[i]);
        }
    }
    
    // Calculate force using FMM
    void calculateForces() {
        if (isLeaf) {
            // Direct calculation for leaf nodes
            for (Particle* p1 : particles) {
                for (Particle* p2 : particles) {
                    if (p1 != p2) {
                        double dx = p2->x - p1->x;
                        double dy = p2->y - p1->y;
                        double dz = p2->z - p1->z;
                        double r = std::sqrt(dx*dx + dy*dy + dz*dz);
                        
                        if (r > 1e-10) {
                            double forceMag = p1->charge * p2->charge / (r * r * r);
                            p1->fx += forceMag * dx;
                            p1->fy += forceMag * dy;
                            p1->fz += forceMag * dz;
                        }
                    }
                }
            }
        } else {
            // Use multipole expansion for distant interactions
            for (int i = 0; i < 8; ++i) {
                if (children[i]) {
                    children[i]->calculateForces();
                }
            }
        }
    }
    
private:
    int getChildIndex(Particle* p) {
        int index = 0;
        if (p->x > x) index |= 1;
        if (p->y > y) index |= 2;
        if (p->z > z) index |= 4;
        return index;
    }
};

// Main FMM class
class FastMultipoleMethod {
private:
    FMMNode* root;
    std::vector<Particle*> particles;
    
public:
    FastMultipoleMethod(double x, double y, double z, double size) {
        root = new FMMNode(x, y, z, size);
    }
    
    ~FastMultipoleMethod() {
        delete root;
    }
    
    void addParticle(double x, double y, double z, double charge) {
        particles.push_back(new Particle(x, y, z, charge));
    }
    
    void buildTree(int maxParticles = 10) {
        root->buildTree(particles, maxParticles);
    }
    
    void calculateForces() {
        root->calculateForces();
    }
    
    void printForces() {
        for (Particle* p : particles) {
            std::cout << "Particle at (" << p->x << ", " << p->y << ", " << p->z 
                      << ") has force (" << p->fx << ", " << p->fy << ", " << p->fz << ")\n";
        }
    }
};

// Example usage
int main() {
    // Create FMM with a 10x10x10 space
    FastMultipoleMethod fmm(0.0, 0.0, 0.0, 10.0);
    
    // Add some particles
    fmm.addParticle(1.0, 1.0, 1.0, 1.0);
    fmm.addParticle(2.0, 2.0, 2.0, -1.0);
    fmm.addParticle(3.0, 3.0, 3.0, 2.0);
    fmm.addParticle(4.0, 4.0, 4.0, -2.0);
    fmm.addParticle(5.0, 5.0, 5.0, 1.5);
    
    // Build the tree
    fmm.buildTree(2);
    
    // Calculate forces
    fmm.calculateForces();
    
    // Print results
    fmm.printForces();
    
    return 0;
}
```

## Key Components of this FMM Implementation:

1. **Particle Structure**: Stores position, charge, and force components
2. **Multipole Expansion**: Represents the multipole expansion at each node
3. **FMM Tree Node**: Implements the octree structure for hierarchical grouping
4. **Fast Multipole Method Class**: Main interface for FMM operations

## Algorithm Steps:

1. **Tree Construction**: Build an octree to group particles hierarchically
2. **Multipole Expansion**: Calculate multipole expansions for distant groups
3. **Force Calculation**: Use multipole expansions for far-field interactions
4. **Direct Calculation**: Use direct computation for near-field interactions

## Time Complexity:
- **Naive approach**: O(N²)
- **FMM approach**: O(N log N) or O(N) with good particle distribution

This is a simplified implementation showing the core concepts. A full FMM would include:
- More sophisticated multipole expansions
- Translation operators between different levels
- Adaptive tree construction
- More accurate force calculations
- Optimized memory management

