# Reservoir Sampling Algorithm in Fortran

Here's an implementation of the Reservoir Sampling algorithm in Fortran:

```fortran
program reservoir_sampling_example
    implicit none
    integer, parameter :: n = 1000  ! Total number of elements
    integer, parameter :: k = 5     ! Size of reservoir
    integer :: i, j
    integer :: reservoir(k)
    integer :: stream(n)
    real :: rand_num
    
    ! Initialize the stream with sequential numbers
    do i = 1, n
        stream(i) = i
    end do
    
    ! Initialize reservoir with first k elements
    do i = 1, k
        reservoir(i) = stream(i)
    end do
    
    ! Process remaining elements
    do i = k + 1, n
        ! Generate random number between 1 and i
        call random_number(rand_num)
        j = int(rand_num * real(i)) + 1
        
        ! If j is within reservoir size, replace element
        if (j <= k) then
            reservoir(j) = stream(i)
        end if
    end do
    
    ! Print results
    write(*,*) 'Reservoir sample:'
    do i = 1, k
        write(*,*) 'Element ', i, ': ', reservoir(i)
    end do
    
end program reservoir_sampling_example
```

## How it works:

1. **Initialization**: Fill the reservoir array with the first k elements from the stream
2. **Processing**: For each subsequent element (i from k+1 to n):
   - Generate a random number between 1 and i
   - If the random number is ≤ k, replace the corresponding element in reservoir
3. **Result**: After processing all elements, the reservoir contains a random sample

## Key Features:

- **Memory Efficient**: Only stores k elements regardless of stream size
- **Uniform Sampling**: Each element has equal probability of being selected
- **Single Pass**: Processes the stream only once
- **Streaming**: Works with data that's too large to store in memory

## Sample Output:
```
Reservoir sample:
Element 1:  42
Element 2:  187
Element 3:  53
Element 4:  912
Element 5:  256
```

*Note: The actual output will vary due to randomization.*

