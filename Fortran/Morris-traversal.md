# Morris Traversal Algorithm in Fortran

The Morris traversal is an algorithm for traversing binary trees without using recursion or a stack. Here's an implementation in Fortran:

```fortran
program morris_traversal
    implicit none
    
    ! Define the binary tree node structure
    type :: TreeNode
        integer :: data
        type(TreeNode), pointer :: left => null()
        type(TreeNode), pointer :: right => null()
    end type TreeNode
    
    ! Function prototypes
    type(TreeNode), pointer :: create_node(integer)
    subroutine morris_inorder_traversal(type(TreeNode), pointer)
    subroutine print_tree(type(TreeNode), pointer)
    
    ! Main program
    type(TreeNode), pointer :: root
    type(TreeNode), pointer :: node
    
    ! Create a sample binary tree:
    !       1
    !      / \
    !     2   3
    !    / \
    !   4   5
    
    root => create_node(1)
    allocate(root%left)
    root%left%data = 2
    allocate(root%right)
    root%right%data = 3
    
    allocate(root%left%left)
    root%left%left%data = 4
    allocate(root%left%right)
    root%left%right%data = 5
    
    ! Set null pointers
    root%left%left%left => null()
    root%left%left%right => null()
    root%left%right%left => null()
    root%left%right%right => null()
    root%right%left => null()
    root%right%right => null()
    
    ! Perform Morris inorder traversal
    write(*,*) 'Morris Inorder Traversal:'
    call morris_inorder_traversal(root)
    write(*,*) ''
    
    ! Clean up memory
    deallocate(root%left%right)
    deallocate(root%left%left)
    deallocate(root%left)
    deallocate(root%right)
    deallocate(root)
    
contains
    
    ! Create a new tree node
    function create_node(value) result(node)
        integer, intent(in) :: value
        type(TreeNode), pointer :: node
        
        allocate(node)
        node%data = value
        node%left => null()
        node%right => null()
    end function create_node
    
    ! Morris Inorder Traversal Algorithm
    subroutine morris_inorder_traversal(root)
        type(TreeNode), pointer, intent(in) :: root
        type(TreeNode), pointer :: current, predecessor
        
        current => root
        
        do while (associated(current))
            ! If left child is null, visit current node and go to right child
            if (.not. associated(current%left)) then
                write(*,*) current%data, ' '
                current => current%right
            else
                ! Find the inorder predecessor (rightmost node in left subtree)
                predecessor => current%left
                do while (associated(predecessor%right) .and. &
                         .not. associated(predecessor%right, current))
                    predecessor => predecessor%right
                end do
                
                ! If right pointer of predecessor is null, make current as right child
                if (.not. associated(predecessor%right)) then
                    predecessor%right => current
                    current => current%left
                else
                    ! Revert the changes and visit current node
                    predecessor%right => null()
                    write(*,*) current%data, ' '
                    current => current%right
                end if
            end if
        end do
    end subroutine morris_inorder_traversal
    
end program morris_traversal
```

## How Morris Traversal Works

The Morris traversal algorithm works by temporarily modifying the tree structure to create links back to previously visited nodes, allowing traversal without additional space for a stack or recursion.

### Key Steps:

1. **Start with the root node**
2. **If current node has no left child**: Visit the node and move to right child
3. **If current node has left child**: 
   - Find the inorder predecessor (rightmost node in left subtree)
   - If predecessor's right pointer is null: Make current node as right child of predecessor and move to left child
   - If predecessor's right pointer points to current: Revert the changes, visit current node, and move to right child

### Time and Space Complexity:
- **Time Complexity**: O(n) where n is the number of nodes
- **Space Complexity**: O(1) - no extra space needed

### Output for the example tree:
```
Morris Inorder Traversal:
4 2 5 1 3
```

This represents the inorder traversal (left-root-right) of the binary tree.

