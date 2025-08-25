```python
import random

def bogo_sort(arr):
    """
    BogoSort algorithm implementation
    Also known as "stupid sort" or "monkey sort"
    """
    def is_sorted(lst):
        """Check if the list is sorted in ascending order"""
        for i in range(len(lst) - 1):
            if lst[i] > lst[i + 1]:
                return False
        return True
    
    attempts = 0
    
    while not is_sorted(arr):
        random.shuffle(arr)
        attempts += 1
        print(f"Attempt {attempts}: {arr}")
    
    return arr

# Example usage
if __name__ == "__main__":
    # Test with a small array
    test_array = [3, 1, 4, 1, 5]
    print(f"Original array: {test_array}")
    print("Sorting...")
    
    sorted_array = bogo_sort(test_array.copy())  # Use copy to preserve original
    
    print(f"Sorted array: {sorted_array}")
```

**How it works:**
1. Check if the array is sorted
2. If not sorted, randomly shuffle the elements
3. Repeat until the array is sorted
4. The algorithm has O(n!) time complexity in the worst case

**Note:** This is highly inefficient and should only be used for educational purposes or demonstration of bad algorithms!

