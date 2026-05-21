# Treap Implementation in Fortran

Here's a complete implementation of a Treap data structure in Fortran:

```fortran
program treap_example
    implicit none
    integer, parameter :: MAX_NODES = 1000
    integer, parameter :: MAX_KEY = 1000000
    
    ! Treap node structure
    type :: treap_node
        integer :: key
        integer :: priority
        integer :: left
        integer :: right
        integer :: parent
    end type treap_node
    
    ! Treap structure
    type :: treap
        type(treap_node), allocatable :: nodes(:)
        integer :: root
        integer :: size
        integer :: next_free
    end type treap
    
    ! Function prototypes
    integer function insert(t, key)
        implicit none
        type(treap), intent(inout) :: t
        integer, intent(in) :: key
    end function insert
    
    integer function search(t, key)
        implicit none
        type(treap), intent(in) :: t
        integer, intent(in) :: key
    end function search
    
    integer function delete(t, key)
        implicit none
        type(treap), intent(inout) :: t
        integer, intent(in) :: key
    end function delete
    
    subroutine rotate_right(t, node)
        implicit none
        type(treap), intent(inout) :: t
        integer, intent(in) :: node
    end subroutine rotate_right
    
    subroutine rotate_left(t, node)
        implicit none
        type(treap), intent(inout) :: t
        integer, intent(in) :: node
    end subroutine rotate_left
    
    ! Main program
    type(treap) :: my_treap
    integer :: i, key, result
    
    ! Initialize treap
    call initialize_treap(my_treap)
    
    ! Insert some keys
    write(*,*) 'Inserting keys: 50, 30, 70, 20, 40, 60, 80'
    call insert(my_treap, 50)
    call insert(my_treap, 30)
    call insert(my_treap, 70)
    call insert(my_treap, 20)
    call insert(my_treap, 40)
    call insert(my_treap, 60)
    call insert(my_treap, 80)
    
    ! Search for keys
    write(*,*) 'Searching for keys:'
    do i = 1, 80
        result = search(my_treap, i)
        if (result /= 0) then
            write(*,*) 'Key ', i, ' found'
        end if
    end do
    
    ! Delete some keys
    write(*,*) 'Deleting key 30'
    call delete(my_treap, 30)
    
    write(*,*) 'Searching for key 30 after deletion:'
    result = search(my_treap, 30)
    if (result == 0) then
        write(*,*) 'Key 30 not found (deleted)'
    end if
    
    write(*,*) 'Treap operations completed successfully'
    
contains
    
    subroutine initialize_treap(t)
        implicit none
        type(treap), intent(inout) :: t
        integer :: i
        
        allocate(t%nodes(MAX_NODES))
        t%root = 0
        t%size = 0
        t%next_free = 1
        
        ! Initialize all nodes
        do i = 1, MAX_NODES
            t%nodes(i)%key = 0
            t%nodes(i)%priority = 0
            t%nodes(i)%left = 0
            t%nodes(i)%right = 0
            t%nodes(i)%parent = 0
        end do
    end subroutine initialize_treap
    
    integer function insert(t, key)
        implicit none
        type(treap), intent(inout) :: t
        integer, intent(in) :: key
        integer :: new_node, current, parent, new_priority
        
        ! Create new node
        new_node = t%next_free
        t%next_free = t%next_free + 1
        t%size = t%size + 1
        
        ! Set node properties
        t%nodes(new_node)%key = key
        t%nodes(new_node)%priority = int(rand() * 1000000)  ! Random priority
        t%nodes(new_node)%left = 0
        t%nodes(new_node)%right = 0
        t%nodes(new_node)%parent = 0
        
        ! If tree is empty, make this the root
        if (t%root == 0) then
            t%root = new_node
            insert = new_node
            return
        end if
        
        ! Find insertion point
        current = t%root
        parent = 0
        
        do while (current /= 0)
            parent = current
            if (key < t%nodes(current)%key) then
                current = t%nodes(current)%left
            else
                current = t%nodes(current)%right
            end if
        end do
        
        ! Insert as child of parent
        if (key < t%nodes(parent)%key) then
            t%nodes(parent)%left = new_node
        else
            t%nodes(parent)%right = new_node
        end if
        t%nodes(new_node)%parent = parent
        
        ! Restore heap property
        current = new_node
        do while (current /= t%root) 
            parent = t%nodes(current)%parent
            if (t%nodes(current)%priority > t%nodes(parent)%priority) then
                ! Perform rotation
                if (current == t%nodes(parent)%left) then
                    call rotate_right(t, parent)
                else
                    call rotate_left(t, parent)
                end if
            else
                exit
            end if
            current = parent
        end do
        
        insert = new_node
    end function insert
    
    integer function search(t, key)
        implicit none
        type(treap), intent(in) :: t
        integer, intent(in) :: key
        integer :: current
        
        current = t%root
        do while (current /= 0)
            if (key == t%nodes(current)%key) then
                search = current
                return
            else if (key < t%nodes(current)%key) then
                current = t%nodes(current)%left
            else
                current = t%nodes(current)%right
            end if
        end do
        
        search = 0  ! Not found
    end function search
    
    integer function delete(t, key)
        implicit none
        type(treap), intent(inout) :: t
        integer, intent(in) :: key
        integer :: node, left_child, right_child, current, parent
        
        ! Find node to delete
        node = search(t, key)
        if (node == 0) then
            delete = 0
            return
        end if
        
        ! Node found, now delete it
        left_child = t%nodes(node)%left
        right_child = t%nodes(node)%right
        
        ! Case 1: Node has no children
        if (left_child == 0 .and. right_child == 0) then
            if (node == t%root) then
                t%root = 0
            else
                parent = t%nodes(node)%parent
                if (node == t%nodes(parent)%left) then
                    t%nodes(parent)%left = 0
                else
                    t%nodes(parent)%right = 0
                end if
            end if
            t%size = t%size - 1
            delete = 1
            return
        end if
        
        ! Case 2: Node has one child
        if (left_child == 0 .or. right_child == 0) then
            if (node == t%root) then
                if (left_child /= 0) then
                    t%root = left_child
                else
                    t%root = right_child
                end if
                t%nodes(t%root)%parent = 0
            else
                parent = t%nodes(node)%parent
                if (left_child /= 0) then
                    if (node == t%nodes(parent)%left) then
                        t%nodes(parent)%left = left_child
                    else
                        t%nodes(parent)%right = left_child
                    end if
                    t%nodes(left_child)%parent = parent
                else
                    if (node == t%nodes(parent)%left) then
                        t%nodes(parent)%left = right_child
                    else
                        t%nodes(parent)%right = right_child
                    end if
                    t%nodes(right_child)%parent = parent
                end if
            end if
            t%size = t%size - 1
            delete = 1
            return
        end if
        
        ! Case 3: Node has two children
        ! Find inorder successor
        current = right_child
        do while (t%nodes(current)%left /= 0)
            current = t%nodes(current)%left
        end do
        
        ! Replace node's key with successor's key
        t%nodes(node)%key = t%nodes(current)%key
        
        ! Delete the successor
        delete = delete(t, t%nodes(current)%key)
    end function delete
    
    subroutine rotate_right(t, node)
        implicit none
        type(treap), intent(inout) :: t
        integer, intent(in) :: node
        integer :: left_child, parent
        
        left_child = t%nodes(node)%left
        parent = t%nodes(node)%parent
        
        ! Update node's left child
        t%nodes(node)%left = t%nodes(left_child)%right
        
        ! Update left child's right child
        if (t%nodes(left_child)%right /= 0) then
            t%nodes(t%nodes(left_child)%right)%parent = node
        end if
        
        ! Update left child's parent
        t%nodes(left_child)%right = node
        t%nodes(left_child)%parent = parent
        
        ! Update parent's child pointer
        if (parent == 0) then
            t%root = left_child
        else if (node == t%nodes(parent)%left) then
            t%nodes(parent)%left = left_child
        else
            t%nodes(parent)%right = left_child
        end if
        
        ! Update node's parent
        t%nodes(node)%parent = left_child
    end subroutine rotate_right
    
    subroutine rotate_left(t, node)
        implicit none
        type(treap), intent(inout) :: t
        integer, intent(in) :: node
        integer :: right_child, parent
        
        right_child = t%nodes(node)%right
        parent = t%nodes(node)%parent
        
        ! Update node's right child
        t%nodes(node)%right = t%nodes(right_child)%left
        
        ! Update right child's left child
        if (t%nodes(right_child)%left /= 0) then
            t%nodes(t%nodes(right_child)%left)%parent = node
        end if
        
        ! Update right child's parent
        t%nodes(right_child)%left = node
        t%nodes(right_child)%parent = parent
        
        ! Update parent's child pointer
        if (parent == 0) then
            t%root = right_child
        else if (node == t%nodes(parent)%left) then
            t%nodes(parent)%left = right_child
        else
            t%nodes(parent)%right = right_child
        end if
        
        ! Update node's parent
        t%nodes(node)%parent = right_child
    end subroutine rotate_left
    
end program treap_example
```

## Key Features of this Treap Implementation:

1. **Node Structure**: Each node contains key, priority, left/right pointers, and parent pointer
2. **Insertion**: Maintains both BST property (keys) and heap property (priorities)
3. **Search**: Standard binary search tree search
4. **Deletion**: Handles all cases (leaf, one child, two children)
5. **Rotations**: Right and left rotations to maintain heap property
6. **Random Priorities**: Uses `rand()` function to assign random priorities

## Time Complexities:
- **Search**: O(log n) average, O(n) worst case
- **Insert**: O(log n) average, O(n) worst case  
- **Delete**: O(log n) average, O(n) worst case

## Key Concepts:
- **Binary Search Tree Property**: Keys in left subtree < key < keys in right subtree
- **Heap Property**: Priority of parent ≤ priority of children
- **Randomization**: Random priorities ensure good average performance
- **Rotations**: Maintain heap property after insertions

This implementation demonstrates the core principles of Treaps while maintaining the structure and functionality typical of this data structure.

