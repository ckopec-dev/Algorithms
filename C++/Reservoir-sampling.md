# Reservoir Sampling Algorithm in C++

Reservoir sampling is a family of algorithms for randomly sampling k items from a large dataset of unknown size, where we can only make one pass through the data.

## Implementation

```cpp
#include <iostream>
#include <vector>
#include <random>
#include <algorithm>

class ReservoirSampler {
private:
    std::vector<int> reservoir;
    int k;  // Size of reservoir
    int count;  // Number of items seen so far
    
public:
    ReservoirSampler(int reservoirSize) : k(reservoirSize), count(0) {
        reservoir.resize(k);
    }
    
    void add(int item) {
        count++;
        
        if (count <= k) {
            // Fill the reservoir with first k items
            reservoir[count - 1] = item;
        } else {
            // Replace elements with decreasing probability
            std::random_device rd;
            std::mt19937 gen(rd());
            std::uniform_int_distribution<> dis(0, count - 1);
            
            int j = dis(gen);
            if (j < k) {
                reservoir[j] = item;
            }
        }
    }
    
    std::vector<int> getSample() const {
        return reservoir;
    }
    
    int getCount() const {
        return count;
    }
};

// Alternative implementation using a more explicit approach
std::vector<int> reservoirSample(std::vector<int>& data, int k) {
    if (k >= data.size()) {
        return data;  // Return all elements if k >= total size
    }
    
    std::vector<int> reservoir(k);
    std::random_device rd;
    std::mt19937 gen(rd());
    
    // Fill reservoir with first k elements
    for (int i = 0; i < k; i++) {
        reservoir[i] = data[i];
    }
    
    // Process remaining elements
    for (int i = k; i < data.size(); i++) {
        std::uniform_int_distribution<> dis(0, i);
        int j = dis(gen);
        
        if (j < k) {
            reservoir[j] = data[i];
        }
    }
    
    return reservoir;
}

// Example usage
int main() {
    // Sample dataset (could be very large)
    std::vector<int> data = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15};
    
    std::cout << "Original data: ";
    for (int x : data) {
        std::cout << x << " ";
    }
    std::cout << "\n";
    
    // Using class-based approach
    ReservoirSampler sampler(5);
    
    for (int item : data) {
        sampler.add(item);
    }
    
    std::cout << "Reservoir sample (5 items): ";
    for (int x : sampler.getSample()) {
        std::cout << x << " ";
    }
    std::cout << "\n";
    
    // Using function-based approach
    auto sample = reservoirSample(data, 3);
    std::cout << "Function-based sample (3 items): ";
    for (int x : sample) {
        std::cout << x << " ";
    }
    std::cout << "\n";
    
    return 0;
}
```

## How it works:

1. **Initialization**: Fill the reservoir with the first k elements
2. **Processing**: For each subsequent element at position i (starting from k+1):
   - Generate a random number j between 0 and i-1
   - If j < k, replace reservoir[j] with the current element
3. **Probability**: Each element has equal probability of being selected

## Key Properties:

- **Time Complexity**: O(n) where n is the total number of elements
- **Space Complexity**: O(k) where k is the sample size
- **Memory Efficient**: Only requires storage for k elements regardless of input size
- **Single Pass**: Processes data only once

## Output Example:
```
Original data: 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 
Reservoir sample (5 items): 3 7 11 14 2 
Function-based sample (3 items): 5 12 8 
```

This implementation demonstrates how reservoir sampling can efficiently select random samples from large datasets without knowing the total size beforehand.