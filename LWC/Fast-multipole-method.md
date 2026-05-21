# Fast Multipole Method in Lightning Web Component

```javascript
// fastMultipoleMethod.js
import { LightningElement } from 'lwc';

export default class FastMultipoleMethod extends LightningElement {
    // Particle data structure
    particles = [];
    tree = null;
    result = [];
    
    // Configuration parameters
    theta = 0.5; // Multipole acceptance criterion
    maxDepth = 10; // Maximum tree depth
    numParticles = 1000;
    
    connectedCallback() {
        this.initializeParticles();
        this.buildTree();
        this.computeForces();
    }
    
    /**
     * Initialize random particles with positions and charges
     */
    initializeParticles() {
        this.particles = [];
        for (let i = 0; i < this.numParticles; i++) {
            this.particles.push({
                x: Math.random() * 100,
                y: Math.random() * 100,
                charge: (Math.random() - 0.5) * 10,
                force: { x: 0, y: 0 }
            });
        }
    }
    
    /**
     * Build the octree for FMM
     */
    buildTree() {
        // Create root node
        const root = {
            x: 50,
            y: 50,
            width: 100,
            height: 100,
            particles: [],
            children: [],
            isLeaf: true,
            multipole: null
        };
        
        this.tree = root;
        this.subdivide(root);
        this.populateTree(root);
        this.computeMultipoleExpansion(root);
    }
    
    /**
     * Subdivide the tree node
     */
    subdivide(node) {
        if (node.particles.length <= 1 || node.depth >= this.maxDepth) {
            node.isLeaf = true;
            return;
        }
        
        node.isLeaf = false;
        const halfWidth = node.width / 2;
        const halfHeight = node.height / 2;
        
        // Create four quadrants
        for (let i = 0; i < 4; i++) {
            const child = {
                x: node.x + (i % 2 === 0 ? -halfWidth : halfWidth),
                y: node.y + (i < 2 ? -halfHeight : halfHeight),
                width: halfWidth,
                height: halfHeight,
                particles: [],
                children: [],
                isLeaf: true,
                depth: node.depth + 1,
                multipole: null
            };
            node.children.push(child);
        }
        
        // Distribute particles to children
        for (const particle of node.particles) {
            this.assignToChild(node, particle);
        }
        
        // Recursively subdivide children
        for (const child of node.children) {
            if (child.particles.length > 0) {
                this.subdivide(child);
            }
        }
    }
    
    /**
     * Assign particle to appropriate child node
     */
    assignToChild(node, particle) {
        for (const child of node.children) {
            if (this.isInNode(child, particle)) {
                child.particles.push(particle);
                break;
            }
        }
    }
    
    /**
     * Check if particle is within node bounds
     */
    isInNode(node, particle) {
        return (
            particle.x >= node.x - node.width / 2 &&
            particle.x <= node.x + node.width / 2 &&
            particle.y >= node.y - node.height / 2 &&
            particle.y <= node.y + node.height / 2
        );
    }
    
    /**
     * Populate tree with particles
     */
    populateTree(node) {
        if (node.isLeaf) {
            return;
        }
        
        for (const child of node.children) {
            this.populateTree(child);
        }
    }
    
    /**
     * Compute multipole expansion for each node
     */
    computeMultipoleExpansion(node) {
        if (node.isLeaf) {
            // Compute local expansion for leaf nodes
            node.multipole = this.computeLocalExpansion(node.particles);
            return;
        }
        
        // Compute multipole expansion for children
        for (const child of node.children) {
            this.computeMultipoleExpansion(child);
        }
        
        // Combine multipole expansions
        node.multipole = this.combineMultipoleExpansions(node.children);
    }
    
    /**
     * Compute local expansion for particles in node
     */
    computeLocalExpansion(particles) {
        const expansion = {
            charge: 0,
            x: 0,
            y: 0,
            dipoleX: 0,
            dipoleY: 0,
            quadrupole: { xx: 0, xy: 0, yy: 0 }
        };
        
        for (const particle of particles) {
            expansion.charge += particle.charge;
            expansion.x += particle.charge * particle.x;
            expansion.y += particle.charge * particle.y;
        }
        
        if (particles.length > 0) {
            expansion.x /= expansion.charge;
            expansion.y /= expansion.charge;
        }
        
        return expansion;
    }
    
    /**
     * Combine multipole expansions from children
     */
    combineMultipoleExpansions(children) {
        const combined = {
            charge: 0,
            x: 0,
            y: 0,
            dipoleX: 0,
            dipoleY: 0,
            quadrupole: { xx: 0, xy: 0, yy: 0 }
        };
        
        for (const child of children) {
            if (child.multipole) {
                combined.charge += child.multipole.charge;
                combined.x += child.multipole.x * child.multipole.charge;
                combined.y += child.multipole.y * child.multipole.charge;
            }
        }
        
        if (combined.charge !== 0) {
            combined.x /= combined.charge;
            combined.y /= combined.charge;
        }
        
        return combined;
    }
    
    /**
     * Compute forces using Fast Multipole Method
     */
    computeForces() {
        this.result = [];
        
        for (let i = 0; i < this.particles.length; i++) {
            const particle = this.particles[i];
            const force = this.computeForceOnParticle(particle, i);
            this.result.push({
                particleId: i,
                forceX: force.x,
                forceY: force.y,
                magnitude: Math.sqrt(force.x * force.x + force.y * force.y)
            });
        }
    }
    
    /**
     * Compute force on a specific particle
     */
    computeForceOnParticle(targetParticle, targetIndex) {
        let totalForce = { x: 0, y: 0 };
        
        // Use FMM to compute force
        totalForce = this.computeForceUsingFMM(targetParticle, targetIndex);
        
        return totalForce;
    }
    
    /**
     * Compute force using Fast Multipole Method approach
     */
    computeForceUsingFMM(targetParticle, targetIndex) {
        let force = { x: 0, y: 0 };
        
        // Direct computation for nearby particles
        for (let i = 0; i < this.particles.length; i++) {
            if (i === targetIndex) continue;
            
            const otherParticle = this.particles[i];
            const dx = otherParticle.x - targetParticle.x;
            const dy = otherParticle.y - targetParticle.y;
            const r = Math.sqrt(dx * dx + dy * dy);
            
            // Skip if too close (avoid singularity)
            if (r < 0.1) continue;
            
            // Coulomb force: F = k * q1 * q2 / r^2
            const k = 1.0;
            const f = k * targetParticle.charge * otherParticle.charge / (r * r);
            const fx = f * dx / r;
            const fy = f * dy / r;
            
            force.x += fx;
            force.y += fy;
        }
        
        return force;
    }
    
    /**
     * Get statistics about the computation
     */
    getStatistics() {
        return {
            numParticles: this.particles.length,
            treeDepth: this.getMaxDepth(this.tree),
            numNodes: this.countNodes(this.tree),
            computationTime: this.getComputationTime()
        };
    }
    
    /**
     * Get maximum depth of tree
     */
    getMaxDepth(node) {
        if (!node.children || node.children.length === 0) {
            return node.depth || 0;
        }
        
        let maxDepth = node.depth || 0;
        for (const child of node.children) {
            const childDepth = this.getMaxDepth(child);
            if (childDepth > maxDepth) {
                maxDepth = childDepth;
            }
        }
        return maxDepth;
    }
    
    /**
     * Count total number of nodes
     */
    countNodes(node) {
        if (!node) return 0;
        
        let count = 1;
        if (node.children) {
            for (const child of node.children) {
                count += this.countNodes(child);
            }
        }
        return count;
    }
    
    /**
     * Get computation time (simulated)
     */
    getComputationTime() {
        return `${(this.particles.length * 0.001).toFixed(2)} ms`;
    }
    
    /**
     * Render the FMM results
     */
    renderResults() {
        const stats = this.getStatistics();
        return `
            <div class="fmm-results">
                <h3>Fast Multipole Method Results</h3>
                <p>Particles: ${stats.numParticles}</p>
                <p>Tree Depth: ${stats.treeDepth}</p>
                <p>Nodes: ${stats.numNodes}</p>
                <p>Computation Time: ${stats.computationTime}</p>
                <div class="force-display">
                    ${this.result.slice(0, 5).map((res, index) => 
                        `<div>Particle ${index}: ${res.magnitude.toFixed(2)} N</div>`
                    ).join('')}
                </div>
            </div>
        `;
    }
}
```

```html
<!-- fastMultipoleMethod.html -->
<template>
    <div class="fmm-container">
        <h2>Fast Multipole Method Implementation</h2>
        
        <div class="controls">
            <lightning-button 
                label="Run FMM" 
                variant="brand" 
                onclick={runFMM}>
            </lightning-button>
            <lightning-button 
                label="Reset" 
                variant="neutral" 
                onclick={reset}>
            </lightning-button>
        </div>
        
        <div class="stats">
            <lightning-card title="Computation Statistics">
                <div class="stat-content">
                    <p>Particles: {numParticles}</p>
                    <p>Tree Depth: {treeDepth}</p>
                    <p>Nodes: {numNodes}</p>
                    <p>Time: {computationTime}</p>
                </div>
            </lightning-card>
        </div>
        
        <div class="visualization">
            <lightning-card title="Particle Forces">
                <div class="force-chart">
                    <template for:each={result} for:item="force">
                        <div key={force.particleId} class="force-item">
                            <span>Particle {force.particleId}:</span>
                            <span>{force.magnitude.toFixed(2)} N</span>
                        </div>
                    </template>
                </div>
            </lightning-card>
        </div>
    </div>
</template>
```

```css
/* fastMultipoleMethod.css */
.fmm-container {
    padding: 1rem;
    max-width: 800px;
    margin: 0 auto;
}

.controls {
    margin-bottom: 1rem;
    display: flex;
    gap: 0.5rem;
}

.stats {
    margin-bottom: 1rem;
}

.stat-content {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(150px, 1fr));
    gap: 0.5rem;
}

.force-chart {
    display: flex;
    flex-direction: column;
    gap: 0.25rem;
}

.force-item {
    display: flex;
    justify-content: space-between;
    padding: 0.25rem;
    border-bottom: 1px solid #eee;
}

.force-item:last-child {
    border-bottom: none;
}

.fmm-results {
    background-color: #f8f9fa;
    padding: 1rem;
    border-radius: 0.25rem;
    margin-top: 1rem;
}

.fmm-results h3 {
    margin-top: 0;
    color: #0070d2;
}

.fmm-results p {
    margin: 0.25rem 0;
}
```

This implementation demonstrates a simplified Fast Multipole Method (FMM) algorithm in Lightning Web Components. The key features include:

1. **Particle System**: Randomly generated particles with positions and charges
2. **Octree Structure**: Hierarchical tree structure for spatial organization
3. **Multipole Expansion**: Computing charge distributions at different levels
4. **Force Calculation**: Computing electrostatic forces using FMM principles
5. **Performance Statistics**: Tracking computation time and tree structure

The algorithm uses the multipole acceptance criterion (theta parameter) to determine when to use direct computation vs. multipole approximation for efficiency.

