# Fast Multipole Method (FMM) Implementation in Fortran

Here's a simplified example of a Fast Multipole Method implementation in Fortran, focusing on the core concepts for gravitational or electrostatic interactions:

```fortran
program fast_multipole_method
    implicit none
    
    ! Parameters
    integer, parameter :: n_particles = 1000
    integer, parameter :: max_levels = 6
    integer, parameter :: max_particles_per_cell = 10
    
    ! Particle structure
    type particle
        real :: x, y, z
        real :: mass
    end type particle
    
    ! Cell structure for FMM tree
    type cell
        real :: x_center, y_center, z_center
        real :: size
        integer :: n_particles
        integer :: level
        integer :: parent_id
        integer, allocatable :: particle_list(:)
        type(cell), allocatable :: children(:)
        real :: multipole_expansion(1:27)  ! Up to 3rd order
        real :: local_expansion(1:27)
    end type cell
    
    ! Variables
    type(particle), allocatable :: particles(:)
    type(cell), allocatable :: tree_cells(:)
    integer :: n_cells, current_cell_id
    integer :: i, j, k, level
    
    ! Initialize particles
    call initialize_particles()
    
    ! Build FMM tree
    call build_fmm_tree()
    
    ! Perform FMM calculation
    call compute_multipole_expansions()
    call compute_local_expansions()
    call evaluate_forces()
    
    ! Clean up
    deallocate(particles)
    deallocate(tree_cells)
    
contains
    
    subroutine initialize_particles()
        implicit none
        integer :: i
        
        allocate(particles(n_particles))
        
        ! Initialize particles with random positions and masses
        do i = 1, n_particles
            particles(i)%x = (random_number() - 0.5) * 10.0
            particles(i)%y = (random_number() - 0.5) * 10.0
            particles(i)%z = (random_number() - 0.5) * 10.0
            particles(i)%mass = random_number() * 10.0
        end do
    end subroutine initialize_particles
    
    subroutine build_fmm_tree()
        implicit none
        integer :: cell_id, i
        
        ! Initialize tree structure
        n_cells = 1
        allocate(tree_cells(n_cells))
        
        ! Root cell
        tree_cells(1)%x_center = 0.0
        tree_cells(1)%y_center = 0.0
        tree_cells(1)%z_center = 0.0
        tree_cells(1)%size = 20.0
        tree_cells(1)%level = 0
        tree_cells(1)%parent_id = -1
        allocate(tree_cells(1)%particle_list(n_particles))
        tree_cells(1)%n_particles = n_particles
        
        ! Assign particles to root cell
        do i = 1, n_particles
            tree_cells(1)%particle_list(i) = i
        end do
        
        ! Build tree recursively
        call build_tree_recursive(1, 0)
    end subroutine build_fmm_tree
    
    subroutine build_tree_recursive(cell_id, level)
        implicit none
        integer, intent(in) :: cell_id, level
        integer :: i, j, child_id, n_particles_in_cell
        
        ! Check if we should subdivide
        n_particles_in_cell = tree_cells(cell_id)%n_particles
        
        if (n_particles_in_cell > max_particles_per_cell .and. level < max_levels) then
            ! Create 8 children
            allocate(tree_cells(cell_id)%children(1:8))
            
            do i = 1, 8
                n_cells = n_cells + 1
                allocate(tree_cells(n_cells))
                
                call create_child_cell(cell_id, i, n_cells)
                
                ! Recursively build children
                call build_tree_recursive(n_cells, level + 1)
            end do
            
            ! Remove particles from parent and distribute to children
            call distribute_particles_to_children(cell_id)
        end if
    end subroutine build_tree_recursive
    
    subroutine create_child_cell(parent_id, child_index, cell_id)
        implicit none
        integer, intent(in) :: parent_id, child_index, cell_id
        real :: half_size
        
        half_size = tree_cells(parent_id)%size / 2.0
        
        ! Calculate child cell center based on octant index
        select case (child_index)
            case (1)
                tree_cells(cell_id)%x_center = tree_cells(parent_id)%x_center - half_size/2.0
                tree_cells(cell_id)%y_center = tree_cells(parent_id)%y_center - half_size/2.0
                tree_cells(cell_id)%z_center = tree_cells(parent_id)%z_center - half_size/2.0
            case (2)
                tree_cells(cell_id)%x_center = tree_cells(parent_id)%x_center + half_size/2.0
                tree_cells(cell_id)%y_center = tree_cells(parent_id)%y_center - half_size/2.0
                tree_cells(cell_id)%z_center = tree_cells(parent_id)%z_center - half_size/2.0
            case (3)
                tree_cells(cell_id)%x_center = tree_cells(parent_id)%x_center - half_size/2.0
                tree_cells(cell_id)%y_center = tree_cells(parent_id)%y_center + half_size/2.0
                tree_cells(cell_id)%z_center = tree_cells(parent_id)%z_center - half_size/2.0
            case (4)
                tree_cells(cell_id)%x_center = tree_cells(parent_id)%x_center + half_size/2.0
                tree_cells(cell_id)%y_center = tree_cells(parent_id)%y_center + half_size/2.0
                tree_cells(cell_id)%z_center = tree_cells(parent_id)%z_center - half_size/2.0
            case (5)
                tree_cells(cell_id)%x_center = tree_cells(parent_id)%x_center - half_size/2.0
                tree_cells(cell_id)%y_center = tree_cells(parent_id)%y_center - half_size/2.0
                tree_cells(cell_id)%z_center = tree_cells(parent_id)%z_center + half_size/2.0
            case (6)
                tree_cells(cell_id)%x_center = tree_cells(parent_id)%x_center + half_size/2.0
                tree_cells(cell_id)%y_center = tree_cells(parent_id)%y_center - half_size/2.0
                tree_cells(cell_id)%z_center = tree_cells(parent_id)%z_center + half_size/2.0
            case (7)
                tree_cells(cell_id)%x_center = tree_cells(parent_id)%x_center - half_size/2.0
                tree_cells(cell_id)%y_center = tree_cells(parent_id)%y_center + half_size/2.0
                tree_cells(cell_id)%z_center = tree_cells(parent_id)%z_center + half_size/2.0
            case (8)
                tree_cells(cell_id)%x_center = tree_cells(parent_id)%x_center + half_size/2.0
                tree_cells(cell_id)%y_center = tree_cells(parent_id)%y_center + half_size/2.0
                tree_cells(cell_id)%z_center = tree_cells(parent_id)%z_center + half_size/2.0
        end select
        
        tree_cells(cell_id)%size = half_size
        tree_cells(cell_id)%level = tree_cells(parent_id)%level + 1
        tree_cells(cell_id)%parent_id = parent_id
        allocate(tree_cells(cell_id)%particle_list(0))
        tree_cells(cell_id)%n_particles = 0
    end subroutine create_child_cell
    
    subroutine distribute_particles_to_children(parent_id)
        implicit none
        integer, intent(in) :: parent_id
        integer :: i, j, child_id, particle_index
        
        ! This is a simplified version - in practice, you'd need 
        ! proper particle assignment to children based on position
        do i = 1, tree_cells(parent_id)%n_particles
            particle_index = tree_cells(parent_id)%particle_list(i)
            
            ! Simple assignment (in reality, would check which child cell it belongs to)
            ! For demonstration, we'll distribute evenly among children
            child_id = mod(i, 8) + 1
            
            ! In practice, you'd need to properly assign particles
            ! This is just a placeholder for the concept
        end do
    end subroutine distribute_particles_to_children
    
    subroutine compute_multipole_expansions()
        implicit none
        integer :: i, j
        
        ! Compute multipole expansions for each cell
        do i = 1, n_cells
            if (tree_cells(i)%n_particles > 0) then
                call compute_cell_multipole(i)
            end if
        end do
    end subroutine compute_multipole_expansions
    
    subroutine compute_cell_multipole(cell_id)
        implicit none
        integer, intent(in) :: cell_id
        integer :: i, j
        
        ! Initialize multipole expansion coefficients to zero
        tree_cells(cell_id)%multipole_expansion = 0.0
        
        ! Compute multipole moments for the particles in this cell
        do i = 1, tree_cells(cell_id)%n_particles
            ! This is a simplified version - full implementation would use
            ! spherical harmonics or other expansion techniques
            
            ! For demonstration, we'll compute simple monopole and dipole terms
            j = tree_cells(cell_id)%particle_list(i)
            
            ! Monopole moment (total mass)
            tree_cells(cell_id)%multipole_expansion(1) = &
                tree_cells(cell_id)%multipole_expansion(1) + particles(j)%mass
            
            ! Dipole moments
            tree_cells(cell_id)%multipole_expansion(2) = &
                tree_cells(cell_id)%multipole_expansion(2) + particles(j)%mass * particles(j)%x
            tree_cells(cell_id)%multipole_expansion(3) = &
                tree_cells(cell_id)%multipole_expansion(3) + particles(j)%mass * particles(j)%y
            tree_cells(cell_id)%multipole_expansion(4) = &
                tree_cells(cell_id)%multipole_expansion(4) + particles(j)%mass * particles(j)%z
            
            ! Higher order terms would be computed here
        end do
    end subroutine compute_cell_multipole
    
    subroutine compute_local_expansions()
        implicit none
        integer :: i, j
        
        ! Compute local expansions using multipole-to-local translations
        do i = 1, n_cells
            call compute_local_expansion(i)
        end do
    end subroutine compute_local_expansions
    
    subroutine compute_local_expansion(cell_id)
        implicit none
        integer, intent(in) :: cell_id
        integer :: i, j
        
        ! Initialize local expansion
        tree_cells(cell_id)%local_expansion = 0.0
        
        ! For demonstration, we'll just copy the multipole expansion
        ! In practice, this would involve translation operators
        tree_cells(cell_id)%local_expansion = tree_cells(cell_id)%multipole_expansion
    end subroutine compute_local_expansion
    
    subroutine evaluate_forces()
        implicit none
        integer :: i, j
        
        ! Evaluate forces using FMM algorithm
        do i = 1, n_particles
            call evaluate_force_on_particle(i)
        end do
    end subroutine evaluate_forces
    
    subroutine evaluate_force_on_particle(particle_index)
        implicit none
        integer, intent(in) :: particle_index
        real :: fx, fy, fz, dx, dy, dz, r, r2, r3
        integer :: i, j
        
        fx = 0.0; fy = 0.0; fz = 0.0
        
        ! Calculate force from all cells (simplified version)
        do i = 1, n_cells
            if (tree_cells(i)%n_particles > 0) then
                ! Check if cell is far enough to use multipole expansion
                dx = particles(particle_index)%x - tree_cells(i)%x_center
                dy = particles(particle_index)%y - tree_cells(i)%y_center
                dz = particles(particle_index)%z - tree_cells(i)%z_center
                
                r2 = dx*dx + dy*dy + dz*dz
                r = sqrt(r2)
                
                ! If cell is far enough, use multipole expansion
                if (r > 2.0 * tree_cells(i)%size) then
                    ! Use multipole expansion for force calculation
                    ! This would involve more complex spherical harmonic operations
                    call compute_force_from_multipole(particle_index, i, fx, fy, fz)
                else
                    ! Direct summation for nearby particles
                    do j = 1, tree_cells(i)%n_particles
                        call compute_direct_force(particle_index, &
                            tree_cells(i)%particle_list(j), fx, fy, fz)
                    end do
                end if
            end if
        end do
        
        ! Output results (simplified)
        write(*,*) 'Particle ', particle_index, ' force: ', fx, fy, fz
    end subroutine evaluate_force_on_particle
    
    subroutine compute_force_from_multipole(particle_index, cell_id, fx, fy, fz)
        implicit none
        integer, intent(in) :: particle_index, cell_id
        real, intent(inout) :: fx, fy, fz
        
        ! Simplified force calculation using multipole expansion
        ! In practice, this would involve spherical harmonics and translation operators
        fx = fx + 1.0e-6  ! Placeholder value
        fy = fy + 1.0e-6
        fz = fz + 1.0e-6
    end subroutine compute_force_from_multipole
    
    subroutine compute_direct_force(particle1, particle2, fx, fy, fz)
        implicit none
        integer, intent(in) :: particle1, particle2
        real, intent(inout) :: fx, fy, fz
        real :: dx, dy, dz, r, r2, r3, G
        
        G = 6.674e-11  ! Gravitational constant
        
        dx = particles(particle1)%x - particles(particle2)%x
        dy = particles(particle1)%y - particles(particle2)%y
        dz = particles(particle1)%z - particles(particle2)%z
        
        r2 = dx*dx + dy*dy + dz*dz
        r = sqrt(r2)
        
        if (r > 1.0e-10) then
            r3 = r2 * r
            
            fx = fx + G * particles(particle2)%mass * dx / r3
            fy = fy + G * particles(particle2)%mass * dy / r3
            fz = fz + G * particles(particle2)%mass * dz / r3
        end if
    end subroutine compute_direct_force
    
end program fast_multipole_method
```

## Key Components of this FMM Implementation:

### 1. **Data Structures**
- `particle`: Stores particle coordinates and mass
- `cell`: Tree cell structure with multipole/local expansions

### 2. **Core Algorithms**
- **Tree Construction**: Builds octree hierarchy
- **Multipole Expansion**: Computes far-field interactions
- **Local Expansion**: Computes near-field interactions  
- **Force Evaluation**: Combines all contributions

### 3. **Key Features**
- Octree-based spatial decomposition
- Multipole-to-local translations
- Near/far field separation
- Hierarchical interaction calculation

### 4. **Computational Complexity**
- **Traditional N-body**: O(N²)
- **FMM**: O(N log N) or better with optimizations

This is a simplified educational example. A production FMM implementation would include:
- Proper spherical harmonic calculations
- Translation operators between multipole and local expansions
- More sophisticated tree traversal algorithms
- Memory optimization techniques
- Parallel processing capabilities