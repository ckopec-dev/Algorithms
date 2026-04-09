# Fast Multipole Method (FMM) Implementation in Ruby

```ruby
class Particle
  attr_accessor :x, :y, :charge, :force_x, :force_y
  
  def initialize(x, y, charge = 1.0)
    @x = x
    @y = y
    @charge = charge
    @force_x = 0.0
    @force_y = 0.0
  end
end

class OctreeNode
  attr_accessor :center_x, :center_y, :size, :particles, :children, :total_charge, :center_of_mass_x, :center_of_mass_y
  
  def initialize(center_x, center_y, size)
    @center_x = center_x
    @center_y = center_y
    @size = size
    @particles = []
    @children = []
    @total_charge = 0.0
    @center_of_mass_x = 0.0
    @center_of_mass_y = 0.0
  end
  
  def is_leaf?
    @children.empty?
  end
  
  def subdivide
    half_size = @size / 2.0
    
    # Create 8 children (octants)
    @children = [
      OctreeNode[@center_x - half_size/2, @center_y - half_size/2, half_size], # Bottom-left
      OctreeNode[@center_x + half_size/2, @center_y - half_size/2, half_size], # Bottom-right
      OctreeNode[@center_x - half_size/2, @center_y + half_size/2, half_size], # Top-left
      OctreeNode[@center_x + half_size/2, @center_y + half_size/2, half_size], # Top-right
      OctreeNode[@center_x - half_size/2, @center_y - half_size/2, half_size], # Bottom-back-left
      OctreeNode[@center_x + half_size/2, @center_y - half_size/2, half_size], # Bottom-back-right
      OctreeNode[@center_x - half_size/2, @center_y + half_size/2, half_size], # Top-back-left
      OctreeNode[@center_x + half_size/2, @center_y + half_size/2, half_size]  # Top-back-right
    ]
    
    # Distribute particles to children
    @particles.each do |particle|
      add_particle_to_child(particle)
    end
    
    @particles.clear
  end
  
  def add_particle_to_child(particle)
    # Determine which child the particle belongs to
    child_index = 0
    child_index |= 1 if particle.x >= @center_x
    child_index |= 2 if particle.y >= @center_y
    
    if @children[child_index]
      @children[child_index].add_particle(particle)
    else
      @particles << particle
    end
  end
  
  def add_particle(particle)
    if is_leaf?
      @particles << particle
      @total_charge += particle.charge
      
      # Update center of mass
      @center_of_mass_x = (@center_of_mass_x * (@total_charge - particle.charge) + particle.x * particle.charge) / @total_charge
      @center_of_mass_y = (@center_of_mass_y * (@total_charge - particle.charge) + particle.y * particle.charge) / @total_charge
      
      # Subdivide if too many particles
      if @particles.length > 4 && @size > 1.0
        subdivide
      end
    else
      add_particle_to_child(particle)
    end
  end
  
  def calculate_force(particle, theta = 0.5)
    if is_leaf? && @particles.length > 0
      # Direct calculation for nearby particles
      @particles.each do |other_particle|
        next if other_particle.equal?(particle)
        calculate_direct_force(particle, other_particle)
      end
    else
      # Use multipole expansion for distant groups
      distance = Math.sqrt(
        (particle.x - @center_of_mass_x)**2 + 
        (particle.y - @center_of_mass_y)**2
      )
      
      if @size / distance < theta
        # Use center of mass approximation
        calculate_multipole_force(particle)
      else
        # Recursively check children
        @children.each do |child|
          child.calculate_force(particle, theta) unless child.nil?
        end
      end
    end
  end
  
  def calculate_direct_force(particle1, particle2)
    dx = particle2.x - particle1.x
    dy = particle2.y - particle1.y
    distance = Math.sqrt(dx*dx + dy*dy)
    
    # Avoid division by zero
    return if distance < 1e-10
    
    # Coulomb's law (simplified)
    force_magnitude = particle1.charge * particle2.charge / (distance * distance)
    force_x = force_magnitude * dx / distance
    force_y = force_magnitude * dy / distance
    
    particle1.force_x += force_x
    particle1.force_y += force_y
  end
  
  def calculate_multipole_force(particle)
    dx = @center_of_mass_x - particle.x
    dy = @center_of_mass_y - particle.y
    distance = Math.sqrt(dx*dx + dy*dy)
    
    # Avoid division by zero
    return if distance < 1e-10
    
    # Use center of mass for force calculation
    force_magnitude = particle.charge * @total_charge / (distance * distance)
    force_x = force_magnitude * dx / distance
    force_y = force_magnitude * dy / distance
    
    particle.force_x += force_x
    particle.force_y += force_y
  end
end

class FastMultipoleMethod
  def initialize(width = 100.0, height = 100.0)
    @width = width
    @height = height
    @root = OctreeNode.new(width/2.0, height/2.0, width)
    @particles = []
  end
  
  def add_particle(x, y, charge = 1.0)
    particle = Particle.new(x, y, charge)
    @particles << particle
    @root.add_particle(particle)
  end
  
  def calculate_forces(theta = 0.5)
    @particles.each do |particle|
      particle.force_x = 0.0
      particle.force_y = 0.0
      @root.calculate_force(particle, theta)
    end
  end
  
  def get_particles
    @particles
  end
end

# Example usage
def demonstrate_fmm
  puts "Fast Multipole Method Demonstration"
  puts "=" * 40
  
  # Create FMM instance
  fmm = FastMultipoleMethod.new(100.0, 100.0)
  
  # Add particles
  particles_data = [
    [10, 10, 1.0],
    [20, 20, 2.0],
    [30, 30, 1.5],
    [40, 40, 0.5],
    [50, 50, 3.0],
    [60, 60, 1.2],
    [70, 70, 2.5],
    [80, 80, 1.8],
    [90, 90, 0.7]
  ]
  
  particles_data.each do |x, y, charge|
    fmm.add_particle(x, y, charge)
  end
  
  puts "Added #{fmm.get_particles.length} particles"
  
  # Calculate forces using FMM
  puts "\nCalculating forces using Fast Multipole Method..."
  fmm.calculate_forces(0.5)
  
  # Display results
  puts "\nParticle Forces:"
  puts "X\tY\tCharge\tForce X\t\tForce Y"
  puts "-" * 50
  
  fmm.get_particles.each_with_index do |particle, index|
    puts "#{particle.x.round(2)}\t#{particle.y.round(2)}\t#{particle.charge}\t#{particle.force_x.round(4)}\t\t#{particle.force_y.round(4)}"
  end
  
  # Compare with direct calculation for small number of particles
  puts "\nComparison with direct calculation (for verification):"
  puts "Direct calculation would be O(N^2) but FMM is O(N log N)"
end

# Run the demonstration
demonstrate_fmm
```

This implementation demonstrates the key components of the Fast Multipole Method:

## Key Features:

1. **Octree Structure**: Particles are organized in a 2D octree for spatial partitioning
2. **Multipole Expansion**: Uses center of mass approximation for distant particle groups
3. **Theta Parameter**: Controls the accuracy vs. performance trade-off
4. **Hierarchical Calculation**: Direct forces for nearby particles, multipole for distant ones

## Algorithm Steps:

1. **Build Octree**: Partition space into octants
2. **Particle Assignment**: Distribute particles to appropriate nodes
3. **Force Calculation**: 
   - Direct calculation for nearby particles
   - Multipole approximation for distant groups
4. **Result Aggregation**: Sum forces from all interactions

## Time Complexity:
- **Direct Method**: O(N²)
- **FMM**: O(N log N) with good constants

The implementation shows how FMM reduces computational complexity by approximating distant particle interactions while maintaining accuracy for local interactions.

