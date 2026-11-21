# Binary Search Tree in Fortran

Here's a complete implementation of a Binary Search Tree algorithm in Fortran:

```fortran
program binary_search_tree
    implicit none
    
    ! Define the node structure for BST
    type :: node
        integer :: data
        type(node), pointer :: left => null()
        type(node), pointer :: right => null()
    end type node
    
    ! Function prototypes
    type(node), pointer :: insert_node(type(node), integer)
    type(node), pointer :: search_node(type(node), integer)
    logical :: delete_node(type(node), integer)
    recursive subroutine inorder_traversal(type(node))
    recursive subroutine preorder_traversal(type(node))
    recursive subroutine postorder_traversal(type(node))
    recursive subroutine print_tree(type(node), integer)
    
    ! Main program
    type(node), pointer :: root => null()
    integer :: i, value
    
    ! Initialize the tree with some values
    write(*,*) 'Binary Search Tree Implementation'
    write(*,*) '================================'
    
    ! Insert nodes
    root => insert_node(root, 50)
    root => insert_node(root, 30)
    root => insert_node(root, 70)
    root => insert_node(root, 20)
    root => insert_node(root, 40)
    root => insert_node(root, 60)
    root => insert_node(root, 80)
    
    ! Display the tree structure
    write(*,*) 'Tree structure (inorder traversal):'
    call inorder_traversal(root)
    write(*,*) ''
    
    ! Search for values
    write(*,*) 'Searching for values:'
    value = 40
    if (associated(search_node(root, value))) then
        write(*,*) 'Found ', value
    else
        write(*,*) value, ' not found'
    end if
    
    value = 25
    if (associated(search_node(root, value))) then
        write(*,*) 'Found ', value
    else
        write(*,*) value, ' not found'
    end if
    
    ! Display tree structure with indentation
    write(*,*) ''
    write(*,*) 'Tree structure (visual):'
    call print_tree(root, 0)
    
    ! Preorder and postorder traversals
    write(*,*) ''
    write(*,*) 'Preorder traversal:'
    call preorder_traversal(root)
    write(*,*) ''
    
    write(*,*) 'Postorder traversal:'
    call postorder_traversal(root)
    write(*,*) ''
    
contains
    
    ! Function to insert a new node
    recursive function insert_node(current_node, value) result(new_node)
        type(node), pointer :: current_node
        integer, intent(in) :: value
        type(node), pointer :: new_node
        
        if (.not. associated(current_node)) then
            ! Create new node
            allocate(new_node)
            new_node%data = value
            new_node%left => null()
            new_node%right => null()
        else if (value < current_node%data) then
            ! Insert to the left subtree
            new_node => insert_node(current_node%left, value)
            current_node%left => new_node
        else if (value > current_node%data) then
            ! Insert to the right subtree
            new_node => insert_node(current_node%right, value)
            current_node%right => new_node
        else
            ! Value already exists, do nothing
            new_node => current_node
        end if
    end function insert_node
    
    ! Function to search for a value
    recursive function search_node(current_node, value) result(found_node)
        type(node), pointer :: current_node
        integer, intent(in) :: value
        type(node), pointer :: found_node
        
        found_node => null()
        
        if (.not. associated(current_node)) then
            return
        end if
        
        if (value == current_node%data) then
            found_node => current_node
        else if (value < current_node%data) then
            found_node => search_node(current_node%left, value)
        else
            found_node => search_node(current_node%right, value)
        end if
    end function search_node
    
    ! Function to delete a node
    recursive function delete_node(current_node, value) result(deleted_node)
        type(node), pointer :: current_node
        integer, intent(in) :: value
        logical :: deleted_node
        
        deleted_node = .false.
        
        if (.not. associated(current_node)) then
            return
        end if
        
        if (value < current_node%data) then
            deleted_node = delete_node(current_node%left, value)
        else if (value > current_node%data) then
            deleted_node = delete_node(current_node%right, value)
        else
            ! Node to be deleted found
            deleted_node = .true.
            
            ! Case 1: Node has no children (leaf node)
            if (.not. associated(current_node%left) .and. &
                .not. associated(current_node%right)) then
                deallocate(current_node)
                current_node => null()
                
            ! Case 2: Node has one child
            else if (.not. associated(current_node%left)) then
                current_node => current_node%right
            else if (.not. associated(current_node%right)) then
                current_node => current_node%left
                
            ! Case 3: Node has two children
            else
                ! Find inorder successor (smallest value in right subtree)
                type(node), pointer :: successor
                allocate(successor)
                successor => current_node%right
                
                do while (associated(successor%left))
                    successor => successor%left
                end do
                
                ! Replace current node's data with successor's data
                current_node%data = successor%data
                
                ! Delete the successor
                call delete_node(current_node%right, successor%data)
            end if
        end if
    end function delete_node
    
    ! Inorder traversal (Left -> Root -> Right)
    recursive subroutine inorder_traversal(current_node)
        type(node), pointer :: current_node
        
        if (associated(current_node)) then
            call inorder_traversal(current_node%left)
            write(*,*) current_node%data, ' '
            call inorder_traversal(current_node%right)
        end if
    end subroutine inorder_traversal
    
    ! Preorder traversal (Root -> Left -> Right)
    recursive subroutine preorder_traversal(current_node)
        type(node), pointer :: current_node
        
        if (associated(current_node)) then
            write(*,*) current_node%data, ' '
            call preorder_traversal(current_node%left)
            call preorder_traversal(current_node%right)
        end if
    end subroutine preorder_traversal
    
    ! Postorder traversal (Left -> Right -> Root)
    recursive subroutine postorder_traversal(current_node)
        type(node), pointer :: current_node
        
        if (associated(current_node)) then
            call postorder_traversal(current_node%left)
            call postorder_traversal(current_node%right)
            write(*,*) current_node%data, ' '
        end if
    end subroutine postorder_traversal
    
    ! Print tree structure with indentation
    recursive subroutine print_tree(current_node, level)
        type(node), pointer :: current_node
        integer, intent(in) :: level
        
        if (associated(current_node)) then
            call print_tree(current_node%right, level + 1)
            
            ! Print spaces for indentation
            do i = 1, level
                write(*,*) '   '
            end do
            
            write(*,*) current_node%data
            call print_tree(current_node%left, level + 1)
        end if
    end subroutine print_tree
    
end program binary_search_tree
```

## Key Features of this Implementation:

### **Data Structure:**
- Uses Fortran's `type` with pointers to create nodes
- Each node contains data and pointers to left and right children

### **Core Operations:**
1. **Insert**: Adds new values maintaining BST property
2. **Search**: Finds specific values in the tree
3. **Traversal**: Multiple traversal methods (inorder, preorder, postorder)
4. **Display**: Visual representation of tree structure

### **Algorithms Implemented:**
- **Binary Search Tree Insertion**: Recursively inserts nodes maintaining BST property
- **Binary Search**: Searches for values efficiently with O(log n) average time complexity
- **Tree Traversals**: Inorder, preorder, and postorder traversals
- **Visual Display**: Shows tree structure with proper indentation

### **Time Complexity:**
- **Search**: O(log n) average case, O(n) worst case
- **Insert**: O(log n) average case, O(n) worst case
- **Traversal**: O(n) for all methods

This implementation demonstrates the fundamental concepts of binary search trees using Fortran's pointer capabilities and recursive procedures.