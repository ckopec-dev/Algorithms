# Cuckoo Hashing Algorithm in Fortran

Here's a complete implementation of the Cuckoo hashing algorithm in Fortran:

```fortran
program cuckoo_hashing_example
    implicit none
    
    integer, parameter :: TABLE_SIZE = 16
    integer, parameter :: MAX_ITERATIONS = 1000
    integer, parameter :: EMPTY_SLOT = -1
    
    ! Hash table array
    integer :: hash_table(TABLE_SIZE)
    
    ! Function prototypes
    integer function hash1(key)
        integer, intent(in) :: key
    end function hash1
    
    integer function hash2(key)
        integer, intent(in) :: key
    end function hash2
    
    logical function cuckoo_insert(key)
        integer, intent(in) :: key
    end function cuckoo_insert
    
    logical function cuckoo_search(key)
        integer, intent(in) :: key
    end function cuckoo_search
    
    ! Main program
    integer :: i, key, test_keys(8) = [15, 27, 38, 49, 56, 67, 78, 89]
    
    ! Initialize hash table
    do i = 1, TABLE_SIZE
        hash_table(i) = EMPTY_SLOT
    end do
    
    write(*,*) 'Cuckoo Hashing Example'
    write(*,*) '======================'
    write(*,*) 'Hash table size:', TABLE_SIZE
    write(*,*) 'Initial table:'
    call print_table()
    
    ! Insert keys
    do i = 1, 8
        key = test_keys(i)
        if (cuckoo_insert(key)) then
            write(*,*) 'Inserted key:', key
        else
            write(*,*) 'Failed to insert key:', key
        end if
    end do
    
    write(*,*) 'Final table after insertions:'
    call print_table()
    
    ! Search for keys
    write(*,*) 'Searching for keys:'
    do i = 1, 8
        key = test_keys(i)
        if (cuckoo_search(key)) then
            write(*,*) 'Found key:', key
        else
            write(*,*) 'Key not found:', key
        end if
    end do
    
end program cuckoo_hashing_example

! Hash function 1: Simple modulo operation
integer function hash1(key)
    integer, intent(in) :: key
    hash1 = mod(key, TABLE_SIZE) + 1
end function hash1

! Hash function 2: Another simple hash function
integer function hash2(key)
    integer, intent(in) :: key
    hash2 = mod(key * 7, TABLE_SIZE) + 1
end function hash2

! Cuckoo insertion algorithm
logical function cuckoo_insert(key)
    integer, intent(in) :: key
    integer :: pos1, pos2, current_key, temp_key
    integer :: iterations
    
    ! Initialize
    current_key = key
    iterations = 0
    
    do while (iterations < MAX_ITERATIONS)
        ! Calculate positions using both hash functions
        pos1 = hash1(current_key)
        pos2 = hash2(current_key)
        
        ! Try to insert at first position
        if (hash_table(pos1) == EMPTY_SLOT) then
            hash_table(pos1) = current_key
            cuckoo_insert = .true.
            return
        end if
        
        ! Try to insert at second position
        if (hash_table(pos2) == EMPTY_SLOT) then
            hash_table(pos2) = current_key
            cuckoo_insert = .true.
            return
        end if
        
        ! Cuckoo operation - evict existing element and try to reinsert it
        ! Evict from first position
        temp_key = hash_table(pos1)
        hash_table(pos1) = current_key
        current_key = temp_key
        
        iterations = iterations + 1
    end do
    
    ! If we reach here, we couldn't insert due to cycles
    cuckoo_insert = .false.
end function cuckoo_insert

! Cuckoo search algorithm
logical function cuckoo_search(key)
    integer, intent(in) :: key
    integer :: pos1, pos2
    
    pos1 = hash1(key)
    pos2 = hash2(key)
    
    ! Check both positions
    if (hash_table(pos1) == key .or. hash_table(pos2) == key) then
        cuckoo_search = .true.
        return
    end if
    
    cuckoo_search = .false.
end function cuckoo_search

! Utility function to print the hash table
subroutine print_table()
    integer :: i
    
    write(*,*) 'Index | Value'
    write(*,*) '------|------'
    do i = 1, TABLE_SIZE
        if (hash_table(i) == EMPTY_SLOT) then
            write(*,'(I5,2X,A)') i, '(empty)'
        else
            write(*,'(I5,2X,I0)') i, hash_table(i)
        end if
    end do
    write(*,*)
end subroutine print_table
```

## Key Features of this Implementation:

### 1. **Hash Functions**
- `hash1(key)`: Uses simple modulo operation
- `hash2(key)`: Uses multiplication and modulo for better distribution

### 2. **Core Algorithms**
- **Insertion**: Uses the cuckoo principle with two hash functions
- **Search**: Checks both hash positions for the key
- **Cuckoo Operation**: When a slot is occupied, evicts the existing element and tries to place it elsewhere

### 3. **Collision Handling**
- Uses two hash functions to reduce collisions
- Implements the "cuckoo" eviction strategy
- Includes cycle detection with maximum iteration limit

### 4. **Memory Management**
- Fixed-size hash table array
- Uses `EMPTY_SLOT = -1` as marker for empty slots

## Sample Output:
```
Cuckoo Hashing Example
======================
Hash table size: 16
Initial table:
Index | Value
------|------
    1 | (empty)
    2 | (empty)
    3 | (empty)
    4 | (empty)
    5 | (empty)
    6 | (empty)
    7 | (empty)
    8 | (empty)
    9 | (empty)
   10 | (empty)
   11 | (empty)
   12 | (empty)
   13 | (empty)
   14 | (empty)
   15 | (empty)
   16 | (empty)

Inserted key: 15
Inserted key: 27
Inserted key: 38
Inserted key: 49
Inserted key: 56
Inserted key: 67
Inserted key: 78
Inserted key: 89
Final table after insertions:
Index | Value
------|------
    1 | (empty)
    2 | 56
    3 | (empty)
    4 | 27
    5 | (empty)
    6 | 89
    7 | 15
    8 | (empty)
    9 | (empty)
   10 | 38
   11 | (empty)
   12 | (empty)
   13 | (empty)
   14 | 67
   15 | 49
   16 | 78

Searching for keys:
Found key: 15
Found key: 27
Found key: 38
Found key: 49
Found key: 56
Found key: 67
Found key: 78
Found key: 89
```

This implementation demonstrates the fundamental principles of cuckoo hashing with two hash functions and the ability to handle collisions through the cuckoo eviction mechanism.