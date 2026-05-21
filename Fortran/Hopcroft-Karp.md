# Hopcroft-Karp Algorithm in Fortran

Here's a complete implementation of the Hopcroft-Karp algorithm for finding maximum matching in bipartite graphs using Fortran:

```fortran
program hopcroft_karp_example
    implicit none
    integer, parameter :: MAXN = 100
    integer :: n1, n2, m, i, j, k, match1(MAXN), match2(MAXN)
    integer :: adj(MAXN, MAXN), adj_size(MAXN)
    integer :: dist(MAXN), queue(MAXN), qhead, qtail
    logical :: used(MAXN)
    
    ! Example: bipartite graph with 4 vertices on left side (1-4) 
    ! and 4 vertices on right side (5-8)
    n1 = 4
    n2 = 4
    m = 6
    
    ! Initialize adjacency list
    do i = 1, MAXN
        adj_size(i) = 0
    end do
    
    ! Add edges: (1,5), (1,6), (2,5), (2,7), (3,6), (4,8)
    call add_edge(1, 5)
    call add_edge(1, 6)
    call add_edge(2, 5)
    call add_edge(2, 7)
    call add_edge(3, 6)
    call add_edge(4, 8)
    
    ! Initialize matching arrays
    do i = 1, MAXN
        match1(i) = 0
        match2(i) = 0
    end do
    
    ! Find maximum matching
    write(*,*) 'Maximum matching size:', hopcroft_karp()
    
    ! Print the matching pairs
    write(*,*) 'Matching pairs:'
    do i = 1, n1
        if (match1(i) /= 0) then
            write(*,*) 'Left vertex ', i, ' matches to right vertex ', match1(i)
        end if
    end do
    
contains
    
    subroutine add_edge(u, v)
        integer, intent(in) :: u, v
        adj_size(u) = adj_size(u) + 1
        adj(u, adj_size(u)) = v
    end subroutine add_edge
    
    integer function hopcroft_karp()
        integer :: i, result
        result = 0
        
        ! Initialize matching arrays
        do i = 1, n1
            match1(i) = 0
        end do
        do i = 1, n2
            match2(i) = 0
        end do
        
        ! Main loop
        do while (bfs())
            do i = 1, n1
                if (match1(i) == 0) then
                    if (dfs(i)) then
                        result = result + 1
                    end if
                end if
            end do
        end do
        
        hopcroft_karp = result
    end function hopcroft_karp
    
    logical function bfs()
        integer :: i, u, v, len
        logical :: found
        found = .false.
        
        ! Initialize distance array
        do i = 1, n1
            dist(i) = 0
        end do
        
        ! Clear queue
        qhead = 1
        qtail = 0
        
        ! Add unmatched vertices from left side to queue
        do i = 1, n1
            if (match1(i) == 0) then
                dist(i) = 1
                qtail = qtail + 1
                queue(qtail) = i
            end if
        end do
        
        ! BFS to find augmenting paths
        do while (qhead <= qtail)
            u = queue(qhead)
            qhead = qhead + 1
            
            do i = 1, adj_size(u)
                v = adj(u, i)
                if (match2(v) == 0) then
                    found = .true.
                else
                    if (dist(match2(v)) == 0) then
                        dist(match2(v)) = dist(u) + 1
                        qtail = qtail + 1
                        queue(qtail) = match2(v)
                    end if
                end if
            end do
        end do
        
        bfs = found
    end function bfs
    
    logical function dfs(u)
        integer, intent(in) :: u
        integer :: i, v, w
        logical :: found
        
        found = .false.
        
        do i = 1, adj_size(u)
            v = adj(u, i)
            
            if (match2(v) == 0 .or. (dist(match2(v)) == dist(u) + 1 .and. dfs(match2(v)))) then
                match1(u) = v
                match2(v) = u
                found = .true.
                exit
            end if
        end do
        
        dfs = found
    end function dfs
    
end program hopcroft_karp_example
```

## How the Algorithm Works

The Hopcroft-Karp algorithm finds a maximum cardinality matching in a bipartite graph. Here's the breakdown:

### Key Components:

1. **Bipartite Graph Representation**: Uses adjacency list with `adj` array
2. **Matching Arrays**: 
   - `match1(i)` stores the right vertex matched to left vertex `i`
   - `match2(j)` stores the left vertex matched to right vertex `j`
3. **Distance Array**: `dist(i)` stores the distance from unmatched vertices to matched vertices

### Algorithm Steps:

1. **BFS Phase**: Find augmenting paths of minimum length using BFS
2. **DFS Phase**: For each unmatched vertex, try to find augmenting paths using DFS
3. **Repeat**: Continue until no more augmenting paths exist

### Time Complexity:
- **Time**: O(E × √V) where E is edges and V is vertices
- **Space**: O(V + E) for adjacency list and auxiliary arrays

### Sample Output:
```
Maximum matching size: 4
Matching pairs:
Left vertex  1 matches to right vertex  5
Left vertex  2 matches to right vertex  7
Left vertex  3 matches to right vertex  6
Left vertex  4 matches to right vertex  8
```

This implementation handles the example bipartite graph with 4 vertices on each side and finds a maximum matching of size 4.

