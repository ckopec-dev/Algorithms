# Boyer-Moore Majority Vote Algorithm in Fortran

```fortran
program boyer_moore_majority_vote
    implicit none
    integer, parameter :: n = 7
    integer :: arr(n) = [3, 2, 3, 3, 1, 2, 3]
    integer :: candidate, count, i
    integer :: result
    
    ! Print the input array
    write(*,*) 'Input array:'
    write(*,*) arr
    
    ! Phase 1: Find candidate
    candidate = arr(1)
    count = 1
    
    do i = 2, n
        if (arr(i) == candidate) then
            count = count + 1
        else
            count = count - 1
            if (count == 0) then
                candidate = arr(i)
                count = 1
            end if
        end if
    end do
    
    write(*,*) 'Candidate:', candidate
    
    ! Phase 2: Verify candidate is majority element
    count = 0
    do i = 1, n
        if (arr(i) == candidate) then
            count = count + 1
        end if
    end do
    
    if (count > n/2) then
        result = candidate
        write(*,*) 'Majority element:', result
    else
        write(*,*) 'No majority element found'
    end if
    
end program boyer_moore_majority_vote
```

## Output:
```
Input array:
           3           2           3           3           1           2           3
Candidate:           3
Majority element:           3
```

## Algorithm Explanation:

1. **Phase 1 - Candidate Selection**: 
   - Start with first element as candidate
   - If next element equals candidate, increment count
   - If different, decrement count
   - If count reaches 0, select new candidate

2. **Phase 2 - Verification**:
   - Count occurrences of candidate
   - If count > n/2, it's the majority element

## Time and Space Complexity:
- **Time Complexity**: O(n)
- **Space Complexity**: O(1)

The algorithm works because if an element appears more than n/2 times, it will survive the elimination process in Phase 1 and be confirmed as the majority element in Phase 2.

