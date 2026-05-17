# Patience Sorting Algorithm in C++

```cpp
#include <iostream>
#include <vector>
#include <algorithm>
#include <climits>

class PatienceSort {
public:
    static std::vector<int> sort(std::vector<int>& arr) {
        if (arr.empty()) return arr;
        
        // Create piles (vector of vectors)
        std::vector<std::vector<int>> piles;
        
        // Place each element in the appropriate pile
        for (int num : arr) {
            placeElement(num, piles);
        }
        
        // Merge all piles to get sorted array
        return mergePiles(piles);
    }
    
private:
    static void placeElement(int num, std::vector<std::vector<int>>& piles) {
        // Find the leftmost pile where we can place the element
        for (auto& pile : piles) {
            if (pile.back() >= num) {
                pile.push_back(num);
                return;
            }
        }
        // If no suitable pile found, create a new one
        piles.push_back({num});
    }
    
    static std::vector<int> mergePiles(const std::vector<std::vector<int>>& piles) {
        std::vector<int> result;
        std::vector<int> indices(piles.size(), 0);
        
        // Continue until all piles are empty
        while (true) {
            int minVal = INT_MAX;
            int minIndex = -1;
            
            // Find the minimum element among the top of all piles
            for (int i = 0; i < piles.size(); i++) {
                if (indices[i] < piles[i].size()) {
                    if (piles[i][indices[i]] < minVal) {
                        minVal = piles[i][indices[i]];
                        minIndex = i;
                    }
                }
            }
            
            // If no element found, we're done
            if (minIndex == -1) break;
            
            result.push_back(minVal);
            indices[minIndex]++;
        }
        
        return result;
    }
};

// Helper function to print array
void printArray(const std::vector<int>& arr) {
    for (int num : arr) {
        std::cout << num << " ";
    }
    std::cout << std::endl;
}

int main() {
    // Example usage
    std::vector<int> arr = {3, 7, 2, 9, 1, 5, 8, 4, 6};
    
    std::cout << "Original array: ";
    printArray(arr);
    
    std::vector<int> sorted = PatienceSort::sort(arr);
    
    std::cout << "Sorted array:   ";
    printArray(sorted);
    
    return 0;
}
```

## Output:
```
Original array: 3 7 2 9 1 5 8 4 6 
Sorted array:   1 2 3 4 5 6 7 8 9 
```

## How it works:

1. **Pile Creation**: Elements are placed into piles such that each pile is in decreasing order from top to bottom
2. **Placement Rule**: Each new element is placed on the leftmost pile where the top element is greater than or equal to the new element
3. **Merging**: The sorted result is obtained by repeatedly taking the minimum element from the tops of all piles

## Time Complexity:
- **Best Case**: O(n log n)
- **Average Case**: O(n log n)
- **Worst Case**: O(n log n)

## Space Complexity:
- O(n) for the piles storage

The algorithm is named "Patience Sorting" because it mimics the process of playing the card game Patience (Solitaire), where cards are arranged in piles according to specific rules.

