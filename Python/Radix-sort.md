# Radix Sort Algorithm in Python

Radix sort is a non-comparative sorting algorithm that sorts integers by processing individual digits. It works by sorting elements based on each digit's place value, starting from the least significant digit to the most significant digit.

## Implementation

```python
def counting_sort_for_radix(arr, exp):
    """
    A counting sort function that sorts the array based on the digit at the given exponent place
    """
    n = len(arr)
    output = [0] * n  # Output array
    count = [0] * 10  # Count array for digits 0-9
    
    # Count occurrences of each digit
    for i in range(n):
        index = arr[i] // exp
        count[index % 10] += 1
    
    # Change count[i] to actual position
    for i in range(1, 10):
        count[i] += count[i - 1]
    
    # Build the output array
    i = n - 1
    while i >= 0:
        index = arr[i] // exp
        output[count[index % 10] - 1] = arr[i]
        count[index % 10] -= 1
        i -= 1
    
    # Copy the output array to arr
    for i in range(n):
        arr[i] = output[i]

def radix_sort(arr):
    """
    Main radix sort function
    """
    if not arr:
        return arr
    
    # Find the maximum number to know the number of digits
    max_num = max(arr)
    
    # Do counting sort for every digit
    exp = 1
    while max_num // exp > 0:
        counting_sort_for_radix(arr, exp)
        exp *= 10
    
    return arr

# Example usage
if __name__ == "__main__":
    # Test array
    test_array = [170, 45, 75, 90, 2, 802, 24, 66]
    
    print("Original array:", test_array)
    
    # Sort the array
    sorted_array = radix_sort(test_array.copy())
    
    print("Sorted array:", sorted_array)
    
    # Another example
    test_array2 = [329, 457, 657, 839, 436, 720, 355]
    print("\nOriginal array:", test_array2)
    sorted_array2 = radix_sort(test_array2.copy())
    print("Sorted array:", sorted_array2)
```

## Output
```
Original array: [170, 45, 75, 90, 2, 802, 24, 66]
Sorted array: [2, 24, 45, 66, 75, 90, 170, 802]

Original array: [329, 457, 657, 839, 436, 720, 355]
Sorted array: [329, 355, 436, 457, 657, 720, 839]
```

## How it works:

1. **Find maximum**: Determine the maximum number to know the number of digits
2. **Process each digit**: Starting from the least significant digit (units place), sort the array using counting sort
3. **Move to next digit**: Continue with tens place, hundreds place, etc.
4. **Repeat**: Continue until all digits are processed

## Time Complexity:
- **Best Case**: O(d × n)
- **Average Case**: O(d × n)  
- **Worst Case**: O(d × n)

Where:
- n = number of elements
- d = number of digits in the maximum number

## Space Complexity: O(n + k)
Where k = 10 (for counting array)

## Key Features:
- **Non-comparative**: Doesn't compare elements directly
- **Stable**: Maintains relative order of equal elements
- **Efficient**: Works well for fixed-length integers
- **Not suitable**: For floating-point numbers or variable-length strings without modification

