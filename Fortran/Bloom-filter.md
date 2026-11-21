# Bloom Filter Algorithm in Fortran

Here's a complete implementation of a Bloom filter algorithm in Fortran:

```fortran
program bloom_filter_example
    implicit none
    integer, parameter :: n_bits = 1000
    integer, parameter :: n_hashes = 3
    logical, allocatable :: bit_array(:)
    integer :: i, j, hash_value
    character(len=100) :: input_string
    
    ! Allocate the bit array
    allocate(bit_array(n_bits))
    
    ! Initialize all bits to false
    bit_array = .false.
    
    ! Hash functions (simple examples)
    ! In practice, you would use more sophisticated hash functions
    contains
    
    ! Simple hash function 1
    integer function hash1(str) result(h)
        character(len=*), intent(in) :: str
        integer :: i
        h = 0
        do i = 1, len_trim(str)
            h = mod(h + ichar(str(i:i)), n_bits)
        end do
    end function hash1
    
    ! Simple hash function 2
    integer function hash2(str) result(h)
        character(len=*), intent(in) :: str
        integer :: i
        h = 0
        do i = 1, len_trim(str)
            h = mod(h * 31 + ichar(str(i:i)), n_bits)
        end do
    end function hash2
    
    ! Simple hash function 3
    integer function hash3(str) result(h)
        character(len=*), intent(in) :: str
        integer :: i
        h = 0
        do i = 1, len_trim(str)
            h = mod(h * 37 + ichar(str(i:i)) * i, n_bits)
        end do
    end function hash3
    
    ! Insert a string into the Bloom filter
    subroutine insert_string(str)
        character(len=*), intent(in) :: str
        integer :: hash_val(n_hashes)
        integer :: j
        
        ! Calculate all hash values
        hash_val(1) = hash1(str)
        hash_val(2) = hash2(str)
        hash_val(3) = hash3(str)
        
        ! Set corresponding bits in the array
        do j = 1, n_hashes
            bit_array(mod(hash_val(j), n_bits) + 1) = .true.
        end do
    end subroutine insert_string
    
    ! Check if a string might be in the Bloom filter
    logical function check_string(str)
        character(len=*), intent(in) :: str
        integer :: hash_val(n_hashes)
        integer :: j
        logical :: found
        
        ! Calculate all hash values
        hash_val(1) = hash1(str)
        hash_val(2) = hash2(str)
        hash_val(3) = hash3(str)
        
        ! Check if all corresponding bits are set
        found = .true.
        do j = 1, n_hashes
            if (.not. bit_array(mod(hash_val(j), n_bits) + 1)) then
                found = .false.
                exit
            end if
        end do
        
        check_string = found
    end function check_string
    
    ! Clear all bits (reset filter)
    subroutine clear_filter()
        integer :: i
        do i = 1, n_bits
            bit_array(i) = .false.
        end do
    end subroutine clear_filter
    
end program bloom_filter_example

! Example usage in a separate subroutine
subroutine example_usage()
    implicit none
    character(len=50) :: test_strings(4)
    integer :: i
    logical :: result
    
    ! Initialize test strings
    test_strings(1) = "apple"
    test_strings(2) = "banana"
    test_strings(3) = "cherry"
    test_strings(4) = "date"
    
    ! Insert strings into filter
    do i = 1, 4
        call insert_string(test_strings(i))
        write(*,*) "Inserted: ", trim(test_strings(i))
    end do
    
    ! Test for various strings
    write(*,*) "Testing membership:"
    write(*,*) "apple: ", check_string("apple")
    write(*,*) "grape: ", check_string("grape")
    write(*,*) "banana: ", check_string("banana")
    write(*,*) "orange: ", check_string("orange")
    
end subroutine example_usage
```

## Key Components Explained:

### 1. **Bit Array**
```fortran
logical, allocatable :: bit_array(:)
allocate(bit_array(n_bits))
bit_array = .false.
```

### 2. **Hash Functions**
- Three simple hash functions are implemented
- In production code, you'd use more sophisticated cryptographic hash functions

### 3. **Insert Operation**
```fortran
subroutine insert_string(str)
    ! Calculate multiple hash values
    ! Set corresponding bits in the array
end subroutine
```

### 4. **Membership Check**
```fortran
logical function check_string(str)
    ! Calculate hash values
    ! Check if ALL corresponding bits are set
end function
```

## Usage Example:
```fortran
! Insert items
call insert_string("apple")
call insert_string("banana")

! Check membership
result = check_string("apple")   ! Returns .true.
result = check_string("grape")   ! Returns .false. (with low probability of false positive)
```

## Advantages:
- **Space efficient**: Uses only bits, not actual data storage
- **Fast operations**: O(k) time complexity for both insert and lookup
- **No false negatives**: If an item is in the filter, it will always be detected

## Limitations:
- **False positives possible**: Items not in the set may be reported as present
- **Cannot delete items**: Standard Bloom filters are immutable
- **Requires pre-defined size**: Cannot grow dynamically