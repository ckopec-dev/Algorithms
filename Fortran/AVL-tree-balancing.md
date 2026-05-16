# AVL Tree Balancing Algorithm in Fortran

Here's a complete implementation of an AVL tree with balancing operations in Fortran:

```fortran
program avl_tree_example
    implicit none
    integer, parameter :: MAX_NODES = 100
    integer :: root
    
    ! Initialize the tree
    root = 0
    
    ! Insert nodes and show balancing
    call insert(root, 10)
    call insert(root, 20)
    call insert(root, 30)
    call insert(root, 40)
    call insert(root, 50)
    call insert(root, 25)
    
    write(*,*) 'AVL Tree structure after insertions:'
    call inorder_traversal(root)
    
contains

    ! AVL Tree node structure
    type :: avl_node
        integer :: data
        integer :: height
        integer :: left
        integer :: right
    end type avl_node
    
    type(avl_node), dimension(MAX_NODES) :: tree
    integer :: node_count = 0
    
    ! Get height of node
    integer function get_height(node)
        integer, intent(in) :: node
        if (node == 0) then
            get_height = 0
        else
            get_height = tree(node)%height
        end if
    end function get_height
    
    ! Get balance factor
    integer function get_balance(node)
        integer, intent(in) :: node
        if (node == 0) then
            get_balance = 0
        else
            get_balance = get_height(tree(node)%left) - get_height(tree(node)%right)
        end if
    end function get_balance
    
    ! Right rotate
    integer function rotate_right(y)
        integer, intent(in) :: y
        integer :: x, t2
        
        x = tree(y)%left
        t2 = tree(x)%right
        
        ! Perform rotation
        tree(x)%right = y
        tree(y)%left = t2
        
        ! Update heights
        tree(y)%height = max(get_height(tree(y)%left), get_height(tree(y)%right)) + 1
        tree(x)%height = max(get_height(tree(x)%left), get_height(tree(x)%right)) + 1
        
        rotate_right = x
    end function rotate_right
    
    ! Left rotate
    integer function rotate_left(x)
        integer, intent(in) :: x
        integer :: y, t2
        
        y = tree(x)%right
        t2 = tree(y)%left
        
        ! Perform rotation
        tree(y)%left = x
        tree(x)%right = t2
        
        ! Update heights
        tree(x)%height = max(get_height(tree(x)%left), get_height(tree(x)%right)) + 1
        tree(y)%height = max(get_height(tree(y)%left), get_height(tree(y)%right)) + 1
        
        rotate_left = y
    end function rotate_left
    
    ! Insert a node
    integer function insert_node(node, key)
        integer, intent(in) :: node, key
        integer :: new_node
        
        ! Standard BST insertion
        if (node == 0) then
            node_count = node_count + 1
            new_node = node_count
            tree(new_node)%data = key
            tree(new_node)%height = 1
            tree(new_node)%left = 0
            tree(new_node)%right = 0
            insert_node = new_node
            return
        end if
        
        if (key < tree(node)%data) then
            tree(node)%left = insert_node(tree(node)%left, key)
        else if (key > tree(node)%data) then
            tree(node)%right = insert_node(tree(node)%right, key)
        else
            ! Duplicate keys not allowed
            insert_node = node
            return
        end if
        
        ! Update height
        tree(node)%height = max(get_height(tree(node)%left), get_height(tree(node)%right)) + 1
        
        ! Get balance factor
        integer :: balance
        balance = get_balance(node)
        
        ! Perform rotations if unbalanced
        
        ! Left Left Case
        if (balance > 1 .and. key < tree(tree(node)%left)%data) then
            write(*,*) 'Performing Right Rotation on node ', tree(node)%data
            insert_node = rotate_right(node)
            return
        end if
        
        ! Right Right Case
        if (balance < -1 .and. key > tree(tree(node)%right)%data) then
            write(*,*) 'Performing Left Rotation on node ', tree(node)%data
            insert_node = rotate_left(node)
            return
        end if
        
        ! Left Right Case
        if (balance > 1 .and. key > tree(tree(node)%left)%data) then
            write(*,*) 'Performing Left-Right Rotation on node ', tree(node)%data
            tree(node)%left = rotate_left(tree(node)%left)
            insert_node = rotate_right(node)
            return
        end if
        
        ! Right Left Case
        if (balance < -1 .and. key < tree(tree(node)%right)%data) then
            write(*,*) 'Performing Right-Left Rotation on node ', tree(node)%data
            tree(node)%right = rotate_right(tree(node)%right)
            insert_node = rotate_left(node)
            return
        end if
        
        ! Return the (unchanged) node pointer
        insert_node = node
    end function insert_node
    
    ! Wrapper for insert
    subroutine insert(root_ref, key)
        integer, intent(inout) :: root_ref
        integer, intent(in) :: key
        root_ref = insert_node(root_ref, key)
    end subroutine insert
    
    ! Inorder traversal
    subroutine inorder_traversal(node)
        integer, intent(in) :: node
        if (node /= 0) then
            call inorder_traversal(tree(node)%left)
            write(*,*) 'Node: ', tree(node)%data, ' Height: ', tree(node)%height, &
                       ' Balance: ', get_balance(node)
            call inorder_traversal(tree(node)%right)
        end if
    end subroutine inorder_traversal
    
end program avl_tree_example
```

## Key Features of this Implementation:

### 1. **Node Structure**
- Contains data, height, and left/right pointers
- Uses an array-based approach for simplicity

### 2. **Balance Operations**
- **get_height()**: Returns height of a node
- **get_balance()**: Calculates balance factor (left_height - right_height)
- **rotate_right()**: Right rotation for LL case
- **rotate_left()**: Left rotation for RR case

### 3. **Rotation Types**
- **LL Case**: Left-left imbalance - Right rotation
- **RR Case**: Right-right imbalance - Left rotation  
- **LR Case**: Left-right imbalance - Left-Right rotation
- **RL Case**: Right-left imbalance - Right-Left rotation

### 4. **Balancing Logic**
The algorithm automatically detects imbalances and performs appropriate rotations to maintain AVL property (balance factor between -1 and 1).

### 5. **Example Output**
When run, the program will show:
- Node insertions
- Rotation operations performed
- Height and balance factors for each node
- Final balanced tree structure

This implementation demonstrates the core AVL balancing algorithm in Fortran, showing how rotations maintain the tree's balance property during insertions.

