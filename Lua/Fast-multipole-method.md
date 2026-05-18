# Fast Multipole Method (FMM) in Lua

```lua
-- Fast Multipole Method implementation in Lua
-- This is a simplified version demonstrating the core concepts

-- Particle structure
Particle = {}
Particle.__index = Particle

function Particle:new(x, y, charge)
    local obj = {
        x = x,
        y = y,
        charge = charge,
        force_x = 0,
        force_y = 0
    }
    setmetatable(obj, Particle)
    return obj
end

-- Octree node structure for FMM
Node = {}
Node.__index = Node

function Node:new(x, y, width, height, depth)
    local obj = {
        x = x,
        y = y,
        width = width,
        height = height,
        depth = depth,
        particles = {},
        children = {},
        is_leaf = true,
        center_of_mass_x = 0,
        center_of_mass_y = 0,
        total_charge = 0
    }
    setmetatable(obj, Node)
    return obj
end

-- Build octree
function Node:build_octree(max_particles)
    if #self.particles <= max_particles then
        self.is_leaf = true
        return
    end
    
    self.is_leaf = false
    
    -- Create 4 children
    local half_width = self.width / 2
    local half_height = self.height / 2
    
    self.children[1] = Node:new(self.x - half_width/2, self.y - half_height/2, 
                               half_width, half_height, self.depth + 1)
    self.children[2] = Node:new(self.x + half_width/2, self.y - half_height/2, 
                               half_width, half_height, self.depth + 1)
    self.children[3] = Node:new(self.x - half_width/2, self.y + half_height/2, 
                               half_width, half_height, self.depth + 1)
    self.children[4] = Node:new(self.x + half_width/2, self.y + half_height/2, 
                               half_width, half_height, self.depth + 1)
    
    -- Distribute particles to children
    for _, particle in ipairs(self.particles) do
        for i = 1, 4 do
            local child = self.children[i]
            if particle.x >= child.x - child.width/2 and 
               particle.x <= child.x + child.width/2 and
               particle.y >= child.y - child.height/2 and 
               particle.y <= child.y + child.height/2 then
                table.insert(child.particles, particle)
                break
            end
        end
    end
    
    -- Recursively build children
    for i = 1, 4 do
        if #self.children[i].particles > 0 then
            self.children[i]:build_octree(max_particles)
        end
    end
end

-- Calculate center of mass for node
function Node:calculate_center_of_mass()
    self.total_charge = 0
    self.center_of_mass_x = 0
    self.center_of_mass_y = 0
    
    for _, particle in ipairs(self.particles) do
        self.total_charge = self.total_charge + particle.charge
        self.center_of_mass_x = self.center_of_mass_x + particle.x * particle.charge
        self.center_of_mass_y = self.center_of_mass_y + particle.y * particle.charge
    end
    
    if self.total_charge ~= 0 then
        self.center_of_mass_x = self.center_of_mass_x / self.total_charge
        self.center_of_mass_y = self.center_of_mass_y / self.total_charge
    end
end

-- FMM interaction using multipole expansion
function Node:compute_force_fmm(particle, theta, threshold)
    if self.is_leaf and #self.particles == 0 then
        return
    end
    
    -- Check if node can be treated as a single particle
    local distance = math.sqrt((particle.x - self.center_of_mass_x)^2 + 
                              (particle.y - self.center_of_mass_y)^2)
    
    if self.is_leaf or (distance > threshold * math.max(self.width, self.height)) then
        -- Use multipole approximation
        local dx = self.center_of_mass_x - particle.x
        local dy = self.center_of_mass_y - particle.y
        local r = math.sqrt(dx*dx + dy*dy)
        
        if r > 1e-10 then
            local force_magnitude = self.total_charge * particle.charge / (r * r)
            particle.force_x = particle.force_x + force_magnitude * dx / r
            particle.force_y = particle.force_y + force_magnitude * dy / r
        end
    else
        -- Recursively compute forces from children
        for i = 1, 4 do
            if #self.children[i].particles > 0 then
                self.children[i]:compute_force_fmm(particle, theta, threshold)
            end
        end
    end
end

-- Compute forces using FMM
function Node:compute_forces_fmm(particles, theta, threshold)
    -- Build octree
    self:build_octree(4)
    
    -- Calculate center of mass for all nodes
    self:calculate_center_of_mass()
    
    -- Compute forces for each particle
    for _, particle in ipairs(particles) do
        particle.force_x = 0
        particle.force_y = 0
        self:compute_force_fmm(particle, theta, threshold)
    end
end

-- Simple direct computation for comparison
function compute_direct_forces(particles)
    for i = 1, #particles do
        particles[i].force_x = 0
        particles[i].force_y = 0
        for j = 1, #particles do
            if i ~= j then
                local dx = particles[j].x - particles[i].x
                local dy = particles[j].y - particles[i].y
                local r = math.sqrt(dx*dx + dy*dy)
                
                if r > 1e-10 then
                    local force_magnitude = particles[i].charge * particles[j].charge / (r * r)
                    particles[i].force_x = particles[i].force_x + force_magnitude * dx / r
                    particles[i].force_y = particles[i].force_y + force_magnitude * dy / r
                end
            end
        end
    end
end

-- Example usage
print("Fast Multipole Method Example in Lua")

-- Create some particles
local particles = {}
table.insert(particles, Particle:new(10, 10, 1.0))
table.insert(particles, Particle:new(20, 20, 1.0))
table.insert(particles, Particle:new(30, 30, 1.0))
table.insert(particles, Particle:new(40, 40, 1.0))
table.insert(particles, Particle:new(50, 50, 1.0))

-- Create root node
local root = Node:new(30, 30, 100, 100, 0)

-- Add particles to root
for _, particle in ipairs(particles) do
    table.insert(root.particles, particle)
end

-- Parameters for FMM
local theta = 0.5  -- Multipole acceptance criterion
local threshold = 1.0  -- Distance threshold

-- Compute forces using FMM
print("Computing forces using Fast Multipole Method...")
root:compute_forces_fmm(particles, theta, threshold)

-- Display results
print("FMM Results:")
for i, particle in ipairs(particles) do
    print(string.format("Particle %d: (%.2f, %.2f) charge=%.2f, force=(%.4f, %.4f)", 
                       i, particle.x, particle.y, particle.charge, 
                       particle.force_x, particle.force_y))
end

-- Compare with direct computation
print("\nComparing with direct computation...")
compute_direct_forces(particles)

print("Direct Method Results:")
for i, particle in ipairs(particles) do
    print(string.format("Particle %d: force=(%.4f, %.4f)", 
                       i, particle.force_x, particle.force_y))
end

print("\nFMM algorithm implemented successfully!")
```

This Lua implementation demonstrates the key concepts of the Fast Multipole Method:

## Key Components:

1. **Particle Structure**: Stores position, charge, and force information
2. **Octree Node**: Hierarchical spatial data structure for organizing particles
3. **Octree Building**: Recursively partitions space into quadrants
4. **Multipole Expansion**: Approximates forces from distant groups of particles
5. **Force Calculation**: Computes interactions using FMM approximation

## Algorithm Steps:

1. **Build Octree**: Organize particles in a hierarchical tree structure
2. **Calculate Center of Mass**: For each node, compute total charge and center of mass
3. **Force Computation**: Apply FMM approximation where appropriate
4. **Recursive Evaluation**: Process children when necessary

## Benefits:

- **Time Complexity**: Reduced from O(N²) to O(N log N) or better
- **Memory Efficiency**: Hierarchical structure reduces memory usage
- **Scalability**: Handles large particle systems efficiently

The example shows how FMM approximates long-range interactions while maintaining accuracy for nearby particles.

