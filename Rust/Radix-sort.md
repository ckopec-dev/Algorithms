# Radix Sort Algorithm in Rust

Here's an implementation of the Radix sort algorithm in Rust:

```rust
fn radix_sort(arr: &mut Vec<i32>) {
    if arr.is_empty() {
        return;
    }
    
    // Find the maximum number to know the number of digits
    let max = *arr.iter().max().unwrap();
    
    // Do counting sort for every digit
    let mut exp = 1;
    while max / exp > 0 {
        counting_sort_by_digit(arr, exp);
        exp *= 10;
    }
}

fn counting_sort_by_digit(arr: &mut Vec<i32>, exp: i32) {
    let n = arr.len();
    let mut output = vec![0; n];
    let mut count = vec![0; 10]; // Count array for digits 0-9
    
    // Store count of occurrences of each digit
    for i in 0..n {
        let digit = (arr[i] / exp) % 10;
        count[digit as usize] += 1;
    }
    
    // Change count[i] to actual position
    for i in 1..10 {
        count[i] += count[i - 1];
    }
    
    // Build the output array
    for i in (0..n).rev() {
        let digit = (arr[i] / exp) % 10;
        output[count[digit as usize] - 1] = arr[i];
        count[digit as usize] -= 1;
    }
    
    // Copy the output array to arr
    for i in 0..n {
        arr[i] = output[i];
    }
}

fn main() {
    let mut arr = vec![170, 45, 75, 90, 2, 802, 24, 66];
    
    println!("Original array: {:?}", arr);
    
    radix_sort(&mut arr);
    
    println!("Sorted array: {:?}", arr);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_radix_sort() {
        let mut arr = vec![170, 45, 75, 90, 2, 802, 24, 66];
        let expected = vec![2, 24, 45, 66, 75, 90, 170, 802];
        
        radix_sort(&mut arr);
        assert_eq!(arr, expected);
    }

    #[test]
    fn test_empty_array() {
        let mut arr: Vec<i32> = vec![];
        radix_sort(&mut arr);
        assert_eq!(arr, vec![]);
    }

    #[test]
    fn test_single_element() {
        let mut arr = vec![42];
        radix_sort(&mut arr);
        assert_eq!(arr, vec![42]);
    }

    #[test]
    fn test_already_sorted() {
        let mut arr = vec![1, 2, 3, 4, 5];
        radix_sort(&mut arr);
        assert_eq!(arr, vec![1, 2, 3, 4, 5]);
    }
}
```

## How it works:

1. **Find Maximum**: First, we find the maximum number to determine how many digits we need to process
2. **Process Each Digit**: Starting from the least significant digit (units place), we perform counting sort for each digit position
3. **Counting Sort**: For each digit position, we use counting sort to maintain the relative order of elements with the same digit
4. **Repeat**: Continue with higher order digits until we process all digits of the maximum number

## Time Complexity:
- **Time**: O(d × n) where d is the number of digits and n is the number of elements
- **Space**: O(n + k) where k is the range of digits (0-9)

## Example Output:
```
Original array: [170, 45, 75, 90, 2, 802, 24, 66]
Sorted array: [2, 24, 45, 66, 75, 90, 170, 802]
```

This implementation handles positive integers. For negative numbers, you would need to modify the algorithm to handle the sign separately.

