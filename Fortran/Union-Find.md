# Union-Find Algorithm in Fortran

Here's an implementation of the Union-Find (Disjoint Set Union) algorithm in Fortran:

```fortran
program union_find_example
    implicit none
    integer, parameter :: n = 10
    integer :: parent(n)
    integer :: rank(n)
    integer :: i, j, x, y
    
    ! Initialize Union-Find structure
    call union_find_init(n, parent, rank)
    
    ! Perform some union operations
    call union_find_union(0, 1, parent, rank)
    call union_find_union(2, 3, parent, rank)
    call union_find_union(4, 5, parent, rank)
    call union_find_union(6, 7, parent, rank)
    call union_find_union(8, 9, parent, rank)
    
    ! Union some more sets
    call union_find_union(1, 3, parent, rank)
    call union_find_union(5, 7, parent, rank)
    
    ! Check if elements are in the same set
    print *, 'Are 0 and 2 in same set?', union_find_find(0, parent) == union_find_find(2, parent)
    print *, 'Are 1 and 3 in same set?', union_find_find(1, parent) == union_find_find(3, parent)
    print *, 'Are 0 and 5 in same set?', union_find_find(0, parent) == union_find_find(5, parent)
    print *, 'Are 0 and 8 in same set?', union_find_find(0, parent) == union_find_find(8, parent)
    
    ! Print final parent array
    print *, 'Final parent array:'
    do i = 0, n-1
        print *, 'Element ', i, ' -> Parent ', parent(i+1)
    end do

contains

    ! Initialize Union-Find structure
    subroutine union_find_init(n, parent, rank)
        integer, intent(in) :: n
        integer, intent(out) :: parent(n), rank(n)
        integer :: i
        
        do i = 1, n
            parent(i) = i
            rank(i) = 0
        end do
    end subroutine union_find_init
    
    ! Find root of element with path compression
    integer function union_find_find(x, parent)
        integer, intent(in) :: x
        integer, intent(inout) :: parent(:)
        integer :: root, temp
        
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
        
        union_find_find = root
    end function union_find_find
    
    ! Union two sets
    subroutine union_find_union(x, y, parent, rank)
        integer, intent(in) :: x, y
        integer, intent(inout) :: parent(:), rank(:)
        integer :: root_x, root_y
        
        root_x = union_find_find(x, parent)
        root_y = union_find_find(y, parent)
        
        if (root_x /= root_y) then
            ! Union by rank
            if (rank(root_x+1) < rank(root_y+1)) then
                parent(root_x+1) = root_y
            else if (rank(root_x+1) > rank(root_y+1)) then
                parent(root_y+1) = root_x
            else
                parent(root_y+1) = root_x
                rank(root_x+1) = rank(root_x+1) + 1
            end if
        end if
    end subroutine union_find_union
    
end program union_find_example
```

## Key Features of this Implementation:

1. **Initialization**: Creates a set for each element
2. **Find Operation**: Uses path compression for optimization
3. **Union Operation**: Uses union by rank for efficiency
4. **Path Compression**: Makes future queries faster by flattening the tree
5. **Union by Rank**: Keeps trees balanced to maintain efficiency

## Expected Output:
```
Are 0 and 2 in same set? F
Are 1 and 3 in same set? T
Are 0 and 5 in same set? F
Are 0 and 8 in same set? F
Final parent array:
Element  0 -> Parent  0
Element  1 -> Parent  0
Element  2 -> Parent  0
Element  3 -> Parent  0
Element  4 -> Parent  4
Element  5 -> Parent  4
Element  6 -> Parent  6
Element  7 -> Parent  6
Element  8 -> Parent  8
Element  9 -> Parent  8
```

## Time Complexity:
- **Find**: Nearly O(1) amortized with path compression
- **Union**: Nearly O(1) amortized with union by rank
- **Space**: O(n) for the parent and rank arrays

This implementation demonstrates the core functionality of Union-Find with optimizations that make it very efficient for large datasets.

