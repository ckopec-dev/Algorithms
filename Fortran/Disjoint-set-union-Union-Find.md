# Disjoint Set Union (Union-Find) Algorithm in Fortran

Here's an implementation of the Union-Find data structure in Fortran:

```fortran
program union_find_example
    implicit none
    integer, parameter :: MAX_NODES = 100
    integer :: parent(MAX_NODES)
    integer :: rank(MAX_NODES)
    integer :: i, j, n
    
    ! Initialize the Union-Find structure
    n = 10
    call make_set(n)
    
    ! Perform some union operations
    call union_sets(1, 2)
    call union_sets(3, 4)
    call union_sets(2, 4)
    call union_sets(5, 6)
    call union_sets(7, 8)
    call union_sets(6, 8)
    
    ! Test find operations
    print *, 'Find(1): ', find_set(1)
    print *, 'Find(2): ', find_set(2)
    print *, 'Find(3): ', find_set(3)
    print *, 'Find(4): ', find_set(4)
    print *, 'Find(5): ', find_set(5)
    print *, 'Find(6): ', find_set(6)
    print *, 'Find(7): ', find_set(7)
    print *, 'Find(8): ', find_set(8)
    
    ! Check if elements are in same set
    print *, '1 and 3 in same set: ', are_connected(1, 3)
    print *, '1 and 5 in same set: ', are_connected(1, 5)
    
contains
    
    ! Initialize each element as its own parent
    subroutine make_set(n)
        implicit none
        integer, intent(in) :: n
        integer :: i
        
        do i = 1, n
            parent(i) = i
            rank(i) = 0
        end do
    end subroutine make_set
    
    ! Find root of element with path compression
    integer function find_set(x)
        implicit none
        integer, intent(in) :: x
        integer :: root, temp
        
        ! Find root
        root = x
        do while (parent(root) /= root)
            root = parent(root)
        end do
        
        ! Path compression
        temp = x
        do while (parent(temp) /= root)
            temp = parent(temp)
            parent(temp) = root
        end do
        
        find_set = root
    end function find_set
    
    ! Union two sets
    subroutine union_sets(x, y)
        implicit none
        integer, intent(in) :: x, y
        integer :: root_x, root_y
        
        root_x = find_set(x)
        root_y = find_set(y)
        
        ! If they are already in the same set, do nothing
        if (root_x == root_y) return
        
        ! Union by rank
        if (rank(root_x) < rank(root_y)) then
            parent(root_x) = root_y
        else if (rank(root_x) > rank(root_y)) then
            parent(root_y) = root_x
        else
            parent(root_y) = root_x
            rank(root_x) = rank(root_x) + 1
        end if
    end subroutine union_sets
    
    ! Check if two elements are in the same set
    logical function are_connected(x, y)
        implicit none
        integer, intent(in) :: x, y
        
        are_connected = (find_set(x) == find_set(y))
    end function are_connected
    
end program union_find_example
```

## Key Features of this Implementation:

### 1. **Data Structure**
- `parent`: Array storing parent of each element
- `rank`: Array storing rank (depth) of each tree

### 2. **Core Operations**

**Make Set**: Initialize each element as its own parent
```fortran
subroutine make_set(n)
    do i = 1, n
        parent(i) = i
        rank(i) = 0
    end do
end subroutine make_set
```

**Find Set**: Find root with path compression optimization
```fortran
integer function find_set(x)
    ! Find root
    root = x
    do while (parent(root) /= root)
        root = parent(root)
    end do
    
    ! Path compression
    temp = x
    do while (parent(temp) /= root)
        temp = parent(temp)
        parent(temp) = root
    end do
    
    find_set = root
end function find_set
```

**Union Sets**: Union two sets by rank
```fortran
subroutine union_sets(x, y)
    root_x = find_set(x)
    root_y = find_set(y)
    
    if (root_x == root_y) return
    
    if (rank(root_x) < rank(root_y)) then
        parent(root_x) = root_y
    else if (rank(root_x) > rank(root_y)) then
        parent(root_y) = root_x
    else
        parent(root_y) = root_x
        rank(root_x) = rank(root_x) + 1
    end if
end subroutine union_sets
```

### 3. **Time Complexity**
- **Find**: Nearly O(1) amortized with path compression
- **Union**: Nearly O(1) amortized with union by rank
- **Space**: O(n)

### 4. **Applications**
- Kruskal's Minimum Spanning Tree algorithm
- Connected components in graphs
- Network connectivity problems
- Image processing (connected components)

This implementation provides an efficient way to manage disjoint sets and is commonly used in graph algorithms and network analysis problems.

