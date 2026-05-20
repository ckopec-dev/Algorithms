# Fast Multipole Method (FMM) in Kotlin

Here's a simplified implementation of the Fast Multipole Method algorithm in Kotlin. This example demonstrates the core concepts of FMM for particle interaction calculations.

```kotlin
import kotlin.math.*

// Particle class representing a point charge/mass
data class Particle(
    val x: Double,
    val y: Double,
    val z: Double,
    val charge: Double = 1.0
) {
    fun distanceTo(other: Particle): Double {
        return sqrt(
            (x - other.x) * (x - other.x) +
            (y - other.y) * (y - other.y) +
            (z - other.z) * (z - other.z)
        )
    }
}

// Multipole expansion coefficient
data class MultipoleCoefficients(
    val moments: DoubleArray
) {
    // Initialize with zeros
    constructor(size: Int) : this(DoubleArray(size) { 0.0 })
}

// FMM Tree Node
class FMMTreeNode(
    val center: DoubleArray,
    val size: Double,
    val particles: MutableList<Particle> = mutableListOf(),
    val children: MutableList<FMMTreeNode> = mutableListOf(),
    var multipole: MultipoleCoefficients? = null
) {
    val isLeaf: Boolean
        get() = children.isEmpty()
    
    val isRoot: Boolean
        get() = multipole == null
    
    fun addParticle(particle: Particle) {
        particles.add(particle)
    }
    
    fun isInside(particle: Particle): Boolean {
        return particle.x >= center[0] - size/2 && particle.x <= center[0] + size/2 &&
               particle.y >= center[1] - size/2 && particle.y <= center[1] + size/2 &&
               particle.z >= center[2] - size/2 && particle.z <= center[2] + size/2
    }
    
    fun divide() {
        val halfSize = size / 2
        val childCenters = listOf(
            doubleArrayOf(center[0] - halfSize/2, center[1] - halfSize/2, center[2] - halfSize/2),
            doubleArrayOf(center[0] + halfSize/2, center[1] - halfSize/2, center[2] - halfSize/2),
            doubleArrayOf(center[0] - halfSize/2, center[1] + halfSize/2, center[2] - halfSize/2),
            doubleArrayOf(center[0] + halfSize/2, center[1] + halfSize/2, center[2] - halfSize/2),
            doubleArrayOf(center[0] - halfSize/2, center[1] - halfSize/2, center[2] + halfSize/2),
            doubleArrayOf(center[0] + halfSize/2, center[1] - halfSize/2, center[2] + halfSize/2),
            doubleArrayOf(center[0] - halfSize/2, center[1] + halfSize/2, center[2] + halfSize/2),
            doubleArrayOf(center[0] + halfSize/2, center[1] + halfSize/2, center[2] + halfSize/2)
        )
        
        for (childCenter in childCenters) {
            children.add(FMMTreeNode(childCenter, halfSize))
        }
        
        // Distribute particles to children
        val particlesToMove = particles.toList()
        particles.clear()
        
        for (particle in particlesToMove) {
            for (child in children) {
                if (child.isInside(particle)) {
                    child.addParticle(particle)
                    break
                }
            }
        }
        
        // Recursively divide children if they have too many particles
        for (child in children) {
            if (child.particles.size > 10) { // Threshold for subdivision
                child.divide()
            }
        }
    }
    
    fun computeMultipole() {
        if (isLeaf) {
            // Compute local multipole expansion for leaf node
            val numCoeffs = 1 // Simplified: just monopole moment
            multipole = MultipoleCoefficients(numCoeffs)
            multipole!!.moments[0] = particles.sumOf { it.charge }
        } else {
            // Compute multipole for internal node
            multipole = MultipoleCoefficients(1)
            multipole!!.moments[0] = 0.0
            
            for (child in children) {
                child.computeMultipole()
                if (child.multipole != null) {
                    multipole!!.moments[0] += child.multipole!!.moments[0]
                }
            }
        }
    }
    
    fun evaluatePotential(target: Particle): Double {
        if (isLeaf && particles.isEmpty()) {
            return 0.0
        }
        
        if (isLeaf) {
            // Direct calculation for nearby particles
            return particles.sumOf { particle ->
                val distance = target.distanceTo(particle)
                if (distance > 0) {
                    particle.charge / distance
                } else {
                    0.0
                }
            }
        } else {
            // Use multipole expansion for distant nodes
            val distance = target.distanceTo(
                Particle(center[0], center[1], center[2])
            )
            
            // If node is far enough, use multipole expansion
            if (size / distance < 0.5) { // Threshold for far field approximation
                val moment = multipole?.moments?.getOrNull(0) ?: 0.0
                return moment / distance
            } else {
                // Otherwise, recursively evaluate children
                return children.sumOf { child -> child.evaluatePotential(target) }
            }
        }
    }
}

// Fast Multipole Method Implementation
class FastMultipoleMethod {
    private var root: FMMTreeNode? = null
    
    fun buildTree(particles: List<Particle>, maxDepth: Int = 5): FMMTreeNode {
        // Find bounding box
        val minX = particles.minOf { it.x }
        val maxX = particles.maxOf { it.x }
        val minY = particles.minOf { it.y }
        val maxY = particles.maxOf { it.y }
        val minZ = particles.minOf { it.z }
        val maxZ = particles.maxOf { it.z }
        
        val centerX = (minX + maxX) / 2
        val centerY = (minY + maxY) / 2
        val centerZ = (minZ + maxZ) / 2
        
        val size = maxOf(
            maxX - minX,
            maxY - minY,
            maxZ - minZ
        ) * 1.1 // Add some padding
        
        root = FMMTreeNode(
            doubleArrayOf(centerX, centerY, centerZ),
            size
        )
        
        // Add all particles to root
        for (particle in particles) {
            root!!.addParticle(particle)
        }
        
        // Subdivide tree
        divideTree(root!!, maxDepth)
        
        // Compute multipole expansions
        root!!.computeMultipole()
        
        return root!!
    }
    
    private fun divideTree(node: FMMTreeNode, maxDepth: Int, currentDepth: Int = 0) {
        if (currentDepth >= maxDepth || node.particles.size <= 10) {
            return
        }
        
        node.divide()
        
        for (child in node.children) {
            divideTree(child, maxDepth, currentDepth + 1)
        }
    }
    
    fun calculateForces(particles: List<Particle>): List<Double> {
        val forces = mutableListOf<Double>()
        
        for (particle in particles) {
            val force = root?.evaluatePotential(particle) ?: 0.0
            forces.add(force)
        }
        
        return forces
    }
    
    fun calculatePotential(target: Particle): Double {
        return root?.evaluatePotential(target) ?: 0.0
    }
}

// Example usage
fun main() {
    // Create sample particles
    val particles = listOf(
        Particle(0.0, 0.0, 0.0, 1.0),
        Particle(1.0, 0.0, 0.0, 2.0),
        Particle(0.0, 1.0, 0.0, -1.0),
        Particle(1.0, 1.0, 0.0, 3.0),
        Particle(0.5, 0.5, 0.5, 1.5)
    )
    
    // Create FMM instance
    val fmm = FastMultipoleMethod()
    
    // Build the tree
    val tree = fmm.buildTree(particles, maxDepth = 4)
    
    // Calculate potential at a test point
    val testPoint = Particle(0.5, 0.5, 0.5)
    val potential = fmm.calculatePotential(testPoint)
    
    println("Potential at test point: $potential")
    
    // Calculate forces for all particles
    val forces = fmm.calculateForces(particles)
    println("Forces calculated for ${particles.size} particles")
    
    // Print results
    particles.forEachIndexed { index, particle ->
        println("Particle ${index + 1}: (${particle.x}, ${particle.y}, ${particle.z}) " +
                "charge=${particle.charge}, force=${forces[index]}")
    }
}
```

## Key Components Explained

### 1. **Particle Class**
- Represents a point with 3D coordinates and charge
- Includes distance calculation method

### 2. **FMMTreeNode Class**
- Implements the hierarchical tree structure
- Contains particles and child nodes
- Computes multipole expansions
- Evaluates potentials using FMM

### 3. **FastMultipoleMethod Class**
- Main FMM implementation
- Builds tree structure
- Computes multipole expansions
- Evaluates particle interactions

### 4. **Key FMM Concepts**
- **Tree Construction**: Particles are organized in a hierarchical tree
- **Multipole Expansion**: Computes approximate interactions for distant groups
- **Near Field vs Far Field**: Direct calculation for nearby particles, multipole for distant ones
- **Adaptive Tree**: Subdivides based on particle density and distance thresholds

This implementation provides a simplified but functional example of FMM that demonstrates the core algorithmic concepts while maintaining readability and educational value.

