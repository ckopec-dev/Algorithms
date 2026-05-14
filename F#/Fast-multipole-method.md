# Fast Multipole Method (FMM) in F#

```fsharp
// Fast Multipole Method implementation in F#
open System

// Particle type representing a charge/mass point
type Particle = {
    Position: float32[]
    Charge: float32
}

// Octree node for FMM
type OctreeNode = {
    Center: float32[]
    Size: float32
    Particles: Particle list
    Children: OctreeNode option array
    MultipoleExpansion: float32[]
    LocalExpansion: float32[]
    IsLeaf: bool
}

// FMM implementation
module FastMultipoleMethod = 
    
    // Calculate distance between two points
    let distance (p1: float32[]) (p2: float32[]) : float32 =
        let mutable sum = 0.0f
        for i in 0 .. p1.Length - 1 do
            let diff = p1.[i] - p2.[i]
            sum <- sum + diff * diff
        sqrt sum
    
    // Create a new octree node
    let createOctreeNode (center: float32[]) (size: float32) : OctreeNode =
        {
            Center = center
            Size = size
            Particles = []
            Children = Array.create 8 None
            MultipoleExpansion = Array.create 10 0.0f
            LocalExpansion = Array.create 10 0.0f
            IsLeaf = true
        }
    
    // Insert particle into octree
    let rec insertParticle (node: OctreeNode) (particle: Particle) : OctreeNode =
        // If this is a leaf node and has space, add particle
        if node.IsLeaf && node.Particles.Length < 4 then
            { node with Particles = particle :: node.Particles }
        // If this is a leaf node with full capacity, subdivide
        elif node.IsLeaf then
            let newNode = { node with IsLeaf = false }
            let newParticles = particle :: node.Particles
            let subdividedNode = subdivideNode newNode newParticles
            subdividedNode
        // If not a leaf, recursively insert
        else
            let childIndex = getChildIndex node center particle.Position
            match node.Children.[childIndex] with
            | Some child ->
                let updatedChild = insertParticle child particle
                let updatedChildren = Array.copy node.Children
                updatedChildren.[childIndex] <- Some updatedChild
                { node with Children = updatedChildren }
            | None ->
                let child = createOctreeNode (getChildCenter node childIndex) (node.Size / 2.0f)
                let updatedChild = insertParticle child particle
                let updatedChildren = Array.copy node.Children
                updatedChildren.[childIndex] <- Some updatedChild
                { node with Children = updatedChildren }
    
    // Subdivide a node into 8 children
    let subdivideNode (node: OctreeNode) (particles: Particle list) : OctreeNode =
        let newChildren = Array.create 8 None
        let updatedNode = { node with Children = newChildren }
        
        // Distribute particles to children
        particles |> List.iter (fun particle ->
            let childIndex = getChildIndex updatedNode updatedNode.Center particle.Position
            match updatedNode.Children.[childIndex] with
            | Some child ->
                let updatedChild = insertParticle child particle
                let updatedChildren = Array.copy updatedNode.Children
                updatedChildren.[childIndex] <- Some updatedChild
                { updatedNode with Children = updatedChildren }
            | None ->
                let childCenter = getChildCenter updatedNode childIndex
                let child = createOctreeNode childCenter (updatedNode.Size / 2.0f)
                let updatedChild = insertParticle child particle
                let updatedChildren = Array.copy updatedNode.Children
                updatedChildren.[childIndex] <- Some updatedChild
                { updatedNode with Children = updatedChildren }
        )
        updatedNode
    
    // Get child index based on particle position
    let getChildIndex (node: OctreeNode) (center: float32[]) (position: float32[]) : int =
        let mutable index = 0
        for i in 0 .. position.Length - 1 do
            if position.[i] > center.[i] then
                index <- index ||| (1 <<< i)
        index
    
    // Get child center for a given index
    let getChildCenter (node: OctreeNode) (index: int) : float32[] =
        let halfSize = node.Size / 2.0f
        let center = node.Center |> Array.copy
        for i in 0 .. center.Length - 1 do
            if (index &&& (1 <<< i)) <> 0 then
                center.[i] <- center.[i] + halfSize
            else
                center.[i] <- center.[i] - halfSize
        center
    
    // Compute multipole expansion for a node
    let computeMultipoleExpansion (node: OctreeNode) : OctreeNode =
        if node.IsLeaf then
            // Compute expansion coefficients for particles in this node
            let mutable coeffs = Array.create 10 0.0f
            for particle in node.Particles do
                // Simplified multipole expansion calculation
                coeffs.[0] <- coeffs.[0] + particle.Charge
                coeffs.[1] <- coeffs.[1] + particle.Charge * particle.Position.[0]
                coeffs.[2] <- coeffs.[2] + particle.Charge * particle.Position.[1]
                coeffs.[3] <- coeffs.[3] + particle.Charge * particle.Position.[2]
            { node with MultipoleExpansion = coeffs }
        else
            // Recursively compute for children and sum up
            let mutable coeffs = Array.create 10 0.0f
            for i in 0 .. 7 do
                match node.Children.[i] with
                | Some child ->
                    let childExp = computeMultipoleExpansion child
                    for j in 0 .. 9 do
                        coeffs.[j] <- coeffs.[j] + childExp.MultipoleExpansion.[j]
                | None -> ()
            { node with MultipoleExpansion = coeffs }
    
    // Compute local expansion for a node
    let computeLocalExpansion (node: OctreeNode) : OctreeNode =
        if node.IsLeaf then
            { node with LocalExpansion = Array.create 10 0.0f }
        else
            // Compute local expansion from children
            let mutable coeffs = Array.create 10 0.0f
            for i in 0 .. 7 do
                match node.Children.[i] with
                | Some child ->
                    let childExp = computeLocalExpansion child
                    for j in 0 .. 9 do
                        coeffs.[j] <- coeffs.[j] + childExp.LocalExpansion.[j]
                | None -> ()
            { node with LocalExpansion = coeffs }
    
    // Evaluate force using FMM
    let evaluateForce (tree: OctreeNode) (targetParticle: Particle) : float32[] =
        let force = Array.create 3 0.0f
        let epsilon = 0.5f  // Threshold for far field approximation
        
        let rec evaluateNode (node: OctreeNode) : unit =
            if node.IsLeaf then
                // Direct computation for nearby particles
                for particle in node.Particles do
                    if particle <> targetParticle then
                        let dist = distance targetParticle.Position particle.Position
                        if dist > 0.0f then
                            let forceMag = particle.Charge * targetParticle.Charge / (dist * dist)
                            for i in 0 .. 2 do
                                force.[i] <- force.[i] + forceMag * (particle.Position.[i] - targetParticle.Position.[i]) / dist
            else
                // Check if far field approximation is valid
                let distToCenter = distance targetParticle.Position node.Center
                let sizeToDist = node.Size / distToCenter
                
                if sizeToDist < epsilon then
                    // Use multipole expansion
                    let coeffs = node.MultipoleExpansion
                    // Simplified force calculation using multipole expansion
                    let mutable forceMag = 0.0f
                    for i in 0 .. 2 do
                        forceMag <- forceMag + coeffs.[i] * (targetParticle.Position.[i] - node.Center.[i])
                    for i in 0 .. 2 do
                        force.[i] <- force.[i] + forceMag * (targetParticle.Position.[i] - node.Center.[i])
                else
                    // Recursively evaluate children
                    for i in 0 .. 7 do
                        match node.Children.[i] with
                        | Some child -> evaluateNode child
                        | None -> ()
        
        evaluateNode tree
        force

// Example usage
[<EntryPoint>]
let main argv =
    // Create sample particles
    let particles = [
        { Position = [| 1.0f; 2.0f; 3.0f |]; Charge = 1.0f }
        { Position = [| 4.0f; 5.0f; 6.0f |]; Charge = -1.0f }
        { Position = [| 7.0f; 8.0f; 9.0f |]; Charge = 2.0f }
        { Position = [| 2.0f; 3.0f; 4.0f |]; Charge = 0.5f }
        { Position = [| 5.0f; 6.0f; 7.0f |]; Charge = -0.5f }
    ]
    
    // Create octree
    let rootNode = createOctreeNode [| 0.0f; 0.0f; 0.0f |] 20.0f
    
    // Insert particles
    let finalTree = 
        particles |> List.fold (fun tree particle -> insertParticle tree particle) rootNode
    
    // Compute multipole expansions
    let treeWithExpansions = computeMultipoleExpansion finalTree
    
    // Evaluate force on a target particle
    let target = { Position = [| 3.0f; 4.0f; 5.0f |]; Charge = 1.0f }
    let force = evaluateForce treeWithExpansions target
    
    printfn "Force on particle at %A: %A" target.Position force
    
    0
```

## Key Features of this FMM Implementation

1. **Octree Structure**: Uses a 3D octree to organize particles spatially
2. **Particle Management**: Efficient insertion and subdivision of particles
3. **Multipole Expansions**: Computes multipole coefficients for far-field approximations
4. **Local Expansions**: Handles near-field interactions
5. **Force Evaluation**: Computes forces using FMM acceleration

## Algorithm Steps

1. **Build Octree**: Partition space into octants
2. **Insert Particles**: Distribute particles to appropriate nodes
3. **Compute Expansions**: Calculate multipole and local expansions
4. **Evaluate Forces**: Use FMM to compute interactions efficiently

This implementation demonstrates the core concepts of FMM while maintaining F#'s functional programming style with immutable data structures and recursive algorithms.

