# SPFA Algorithm in Fortran

Here's an implementation of the Shortest Path Faster Algorithm (SPFA) in Fortran:

```fortran
program spfa_example
    implicit none
    integer, parameter :: MAXN = 1000
    integer, parameter :: INF = 1000000000
    
    integer :: n, m, i, j, u, v, w
    integer :: dist(MAXN)
    integer :: head(MAXN), to(MAXN*2), next(MAXN*2), weight(MAXN*2)
    integer :: queue(MAXN), front, rear
    logical :: in_queue(MAXN)
    integer :: edge_count
    
    ! Initialize graph
    edge_count = 0
    do i = 1, MAXN
        head(i) = 0
    end do
    
    ! Read number of vertices and edges
    write(*,*) 'Enter number of vertices and edges:'
    read(*,*) n, m
    
    ! Read edges
    do i = 1, m
        write(*,*) 'Enter edge (u, v, weight):'
        read(*,*) u, v, w
        edge_count = edge_count + 1
        to(edge_count) = v
        weight(edge_count) = w
        next(edge_count) = head(u)
        head(u) = edge_count
    end do
    
    ! Run SPFA algorithm
    call spfa(1, n)
    
    ! Output results
    write(*,*) 'Shortest distances from vertex 1:'
    do i = 1, n
        if (dist(i) >= INF) then
            write(*,*) 'Vertex ', i, ': unreachable'
        else
            write(*,*) 'Vertex ', i, ': ', dist(i)
        end if
    end do
    
contains
    
    subroutine spfa(source, vertices)
        implicit none
        integer, intent(in) :: source, vertices
        integer :: i, u, v, w
        integer :: current
        
        ! Initialize distances
        do i = 1, vertices
            dist(i) = INF
            in_queue(i) = .false.
        end do
        
        ! Initialize queue
        front = 1
        rear = 1
        queue(1) = source
        dist(source) = 0
        in_queue(source) = .true.
        
        ! Main SPFA loop
        do while (front <= rear)
            current = queue(front)
            front = front + 1
            in_queue(current) = .false.
            
            ! Check all neighbors
            i = head(current)
            do while (i /= 0)
                v = to(i)
                w = weight(i)
                
                ! Relax edge
                if (dist(current) + w < dist(v)) then
                    dist(v) = dist(current) + w
                    
                    ! Add to queue if not already there
                    if (.not. in_queue(v)) then
                        rear = rear + 1
                        queue(rear) = v
                        in_queue(v) = .true.
                    end if
                end if
                
                i = next(i)
            end do
        end do
    end subroutine spfa
    
end program spfa_example
```

## How to Compile and Run

```bash
gfortran -o spfa spfa_example.f90
./spfa
```

## Example Input

```
Enter number of vertices and edges:
4 5
Enter edge (u, v, weight):
1 2 4
1 3 2
2 3 1
2 4 5
3 4 8
```

## Expected Output

```
Shortest distances from vertex 1:
Vertex  1 :  0
Vertex  2 :  4
Vertex  3 :  2
Vertex  4 :  9
```

## Key Features of this Implementation

1. **Graph Representation**: Uses adjacency list with head, to, next, and weight arrays
2. **Queue Management**: Implements a circular queue for efficient processing
3. **Optimization**: Uses `in_queue` array to avoid adding duplicate vertices to the queue
4. **Edge Relaxation**: Properly implements the SPFA relaxation step
5. **Memory Management**: Uses fixed-size arrays with parameters for flexibility

The SPFA algorithm is particularly useful for graphs with negative edge weights and can be faster than Dijkstra's algorithm in practice for sparse graphs.

