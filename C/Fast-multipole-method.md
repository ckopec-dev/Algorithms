# Fast Multipole Method (FMM) Implementation in C

Here's a simplified example of the Fast Multipole Method algorithm implemented in C, demonstrating the core concepts for particle interactions.

```c
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>

#define MAX_PARTICLES 1000
#define MAX_LEVELS 5
#define THETA 1.0

// Particle structure
typedef struct {
    double x, y, z;     // Position
    double charge;      // Charge value
    double fx, fy, fz;  // Force components
} Particle;

// Octree node structure for FMM
typedef struct Node {
    double x, y, z;         // Center of the node
    double size;            // Size of the node
    int particle_count;
    Particle *particles;    // List of particles in this node
    
    struct Node *children[8];  // 8 children for octree
    double total_charge;
    double center_of_mass_x, center_of_mass_y, center_of_mass_z;
    
    int is_leaf;
} Node;

// Function to initialize a node
Node* create_node(double x, double y, double z, double size) {
    Node *node = (Node*)malloc(sizeof(Node));
    node->x = x;
    node->y = y;
    node->z = z;
    node->size = size;
    node->particle_count = 0;
    node->particles = NULL;
    node->total_charge = 0.0;
    node->center_of_mass_x = 0.0;
    node->center_of_mass_y = 0.0;
    node->center_of_mass_z = 0.0;
    node->is_leaf = 1;
    
    for (int i = 0; i < 8; i++) {
        node->children[i] = NULL;
    }
    
    return node;
}

// Function to insert a particle into the octree
void insert_particle(Node *node, Particle *particle) {
    if (node->is_leaf && node->particle_count < 4) {
        // Add particle to current node
        node->particles = (Particle*)realloc(node->particles, 
                                            (node->particle_count + 1) * sizeof(Particle));
        node->particles[node->particle_count] = *particle;
        node->particle_count++;
        
        // Update center of mass
        node->total_charge += particle->charge;
        node->center_of_mass_x += particle->charge * particle->x;
        node->center_of_mass_y += particle->charge * particle->y;
        node->center_of_mass_z += particle->charge * particle->z;
        
        return;
    }
    
    if (node->is_leaf) {
        // Split the node into 8 children
        node->is_leaf = 0;
        double half_size = node->size / 2.0;
        
        for (int i = 0; i < 8; i++) {
            double child_x = node->x + (i & 1 ? half_size : -half_size);
            double child_y = node->y + ((i >> 1) & 1 ? half_size : -half_size);
            double child_z = node->z + ((i >> 2) & 1 ? half_size : -half_size);
            
            node->children[i] = create_node(child_x, child_y, child_z, half_size);
        }
        
        // Reinsert existing particles
        for (int i = 0; i < node->particle_count; i++) {
            insert_particle(node, &node->particles[i]);
        }
        
        // Insert new particle
        insert_particle(node, particle);
    } else {
        // Find which child to insert into
        int child_index = 0;
        if (particle->x > node->x) child_index |= 1;
        if (particle->y > node->y) child_index |= 2;
        if (particle->z > node->z) child_index |= 4;
        
        insert_particle(node->children[child_index], particle);
    }
}

// Function to calculate force between two particles
void calculate_force(Particle *p1, Particle *p2, double *fx, double *fy, double *fz) {
    double dx = p2->x - p1->x;
    double dy = p2->y - p1->y;
    double dz = p2->z - p1->z;
    
    double r_squared = dx*dx + dy*dy + dz*dz;
    double r = sqrt(r_squared);
    
    // Avoid division by zero
    if (r < 1e-10) return;
    
    // Coulomb's law: F = k * q1 * q2 / r^2
    double force_magnitude = 1.0 * p1->charge * p2->charge / r_squared;
    double force_x = force_magnitude * dx / r;
    double force_y = force_magnitude * dy / r;
    double force_z = force_magnitude * dz / r;
    
    *fx = force_x;
    *fy = force_y;
    *fz = force_z;
}

// Function to compute multipole expansion (simplified)
void compute_multipole_expansion(Node *node) {
    if (node->is_leaf) {
        // For leaf nodes, compute direct interaction
        for (int i = 0; i < node->particle_count; i++) {
            for (int j = i + 1; j < node->particle_count; j++) {
                double fx, fy, fz;
                calculate_force(&node->particles[i], &node->particles[j], &fx, &fy, &fz);
                
                node->particles[i].fx += fx;
                node->particles[i].fy += fy;
                node->particles[i].fz += fz;
                
                node->particles[j].fx -= fx;
                node->particles[j].fy -= fy;
                node->particles[j].fz -= fz;
            }
        }
    } else {
        // For non-leaf nodes, use multipole expansion
        for (int i = 0; i < 8; i++) {
            if (node->children[i] != NULL) {
                compute_multipole_expansion(node->children[i]);
            }
        }
        
        // Interaction between distant nodes using multipole expansion
        // This is a simplified version - in practice this would be more complex
        for (int i = 0; i < 8; i++) {
            if (node->children[i] != NULL) {
                double distance = sqrt(pow(node->children[i]->x - node->x, 2) +
                                     pow(node->children[i]->y - node->y, 2) +
                                     pow(node->children[i]->z - node->z, 2));
                
                // If the node is far enough away, use multipole expansion
                if (distance > THETA * node->size) {
                    // Simplified interaction calculation
                    double total_charge = node->children[i]->total_charge;
                    double dx = node->children[i]->x - node->x;
                    double dy = node->children[i]->y - node->y;
                    double dz = node->children[i]->z - node->z;
                    
                    double r_squared = dx*dx + dy*dy + dz*dz;
                    double r = sqrt(r_squared);
                    
                    if (r > 1e-10) {
                        double force_magnitude = 1.0 * total_charge * node->total_charge / r_squared;
                        double force_x = force_magnitude * dx / r;
                        double force_y = force_magnitude * dy / r;
                        double force_z = force_magnitude * dz / r;
                        
                        // Apply force to particles in this node
                        for (int j = 0; j < node->particle_count; j++) {
                            node->particles[j].fx += force_x;
                            node->particles[j].fy += force_y;
                            node->particles[j].fz += force_z;
                        }
                    }
                }
            }
        }
    }
}

// Function to initialize particles
void initialize_particles(Particle *particles, int num_particles) {
    srand(time(NULL));
    
    for (int i = 0; i < num_particles; i++) {
        particles[i].x = (rand() % 1000) / 10.0;
        particles[i].y = (rand() % 1000) / 10.0;
        particles[i].z = (rand() % 1000) / 10.0;
        particles[i].charge = (rand() % 100) / 10.0 - 5.0;  // Random charge between -5 and 5
        particles[i].fx = particles[i].fy = particles[i].fz = 0.0;
    }
}

// Main function demonstrating FMM
int main() {
    int num_particles = 100;
    Particle *particles = (Particle*)malloc(num_particles * sizeof(Particle));
    
    // Initialize particles
    initialize_particles(particles, num_particles);
    
    // Create root node of the octree
    Node *root = create_node(50.0, 50.0, 50.0, 100.0);
    
    // Insert all particles into the tree
    printf("Inserting particles into octree...\n");
    for (int i = 0; i < num_particles; i++) {
        insert_particle(root, &particles[i]);
    }
    
    printf("Computing forces using FMM...\n");
    clock_t start = clock();
    
    // Compute forces using Fast Multipole Method
    compute_multipole_expansion(root);
    
    clock_t end = clock();
    double cpu_time_used = ((double)(end - start)) / CLOCKS_PER_SEC;
    
    printf("FMM computation completed in %f seconds\n", cpu_time_used);
    
    // Display some results
    printf("\nFirst 5 particles with forces:\n");
    for (int i = 0; i < 5 && i < num_particles; i++) {
        printf("Particle %d: (%.2f, %.2f, %.2f) charge=%.2f, force=(%.2f, %.2f, %.2f)\n",
               i, particles[i].x, particles[i].y, particles[i].z,
               particles[i].charge, particles[i].fx, particles[i].fy, particles[i].fz);
    }
    
    // Cleanup
    free(particles);
    // Note: In a complete implementation, you would also need to free the octree nodes
    
    return 0;
}
```

## Key Components of this FMM Implementation:

### 1. **Particle Structure**
- Stores position (x,y,z), charge, and force components

### 2. **Octree Node Structure**
- Represents spatial hierarchy for grouping particles
- Contains center coordinates and size information
- Supports 8 children for octree structure

### 3. **Core Functions**
- `create_node()`: Initialize octree node
- `insert_particle()`: Build octree by inserting particles
- `calculate_force()`: Compute Coulomb force between particles
- `compute_multipole_expansion()`: Main FMM algorithm logic

### 4. **FMM Concepts Implemented**
- **Tree Construction**: Octree hierarchical structure
- **Multipole Expansion**: Approximate distant interactions
- **Near Field Calculation**: Direct computation for nearby particles
- **Theta Criterion**: Determines when to use multipole approximation

### 5. **Time Complexity**
- Standard brute force: O(N²)
- FMM: O(N log N) or better with optimizations

This example demonstrates the fundamental structure and concepts of FMM. A production implementation would include additional optimizations, proper memory management, and more sophisticated multipole expansions for better accuracy.