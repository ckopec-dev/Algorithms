# Reservoir Sampling Algorithm in C

Reservoir sampling is a family of algorithms for randomly sampling k items from a large dataset of unknown size, where we can only make one pass through the data.

```c
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

// Function to perform reservoir sampling
void reservoir_sampling(int *stream, int n, int k) {
    // Allocate memory for reservoir array
    int *reservoir = (int*)malloc(k * sizeof(int));
    
    // Initialize reservoir with first k elements
    for (int i = 0; i < k; i++) {
        reservoir[i] = stream[i];
    }
    
    // Process remaining elements
    for (int i = k; i < n; i++) {
        // Generate a random number between 0 and i (inclusive)
        int j = rand() % (i + 1);
        
        // If the random number is less than k, replace an element in reservoir
        if (j < k) {
            reservoir[j] = stream[i];
        }
    }
    
    // Print the sampled elements
    printf("Sampled elements: ");
    for (int i = 0; i < k; i++) {
        printf("%d ", reservoir[i]);
    }
    printf("\n");
    
    free(reservoir);
}

// Alternative implementation that works with streams of unknown size
void reservoir_sampling_unknown_size(int *stream, int n, int k) {
    // This version assumes we know the stream size
    // For truly unknown size, we'd read elements one by one
    
    printf("Reservoir sampling with k=%d from stream of size %d:\n", k, n);
    
    if (k >= n) {
        printf("k is greater than or equal to stream size. Returning all elements.\n");
        for (int i = 0; i < n; i++) {
            printf("%d ", stream[i]);
        }
        printf("\n");
        return;
    }
    
    // Allocate memory for reservoir array
    int *reservoir = (int*)malloc(k * sizeof(int));
    
    // Initialize reservoir with first k elements
    for (int i = 0; i < k; i++) {
        reservoir[i] = stream[i];
    }
    
    // Process remaining elements
    for (int i = k; i < n; i++) {
        // Generate a random number between 0 and i (inclusive)
        int j = rand() % (i + 1);
        
        // If the random number is less than k, replace an element in reservoir
        if (j < k) {
            reservoir[j] = stream[i];
        }
    }
    
    // Print the sampled elements
    printf("Sampled elements: ");
    for (int i = 0; i < k; i++) {
        printf("%d ", reservoir[i]);
    }
    printf("\n");
    
    free(reservoir);
}

// Example usage
int main() {
    // Seed the random number generator
    srand(time(NULL));
    
    // Sample stream of integers
    int stream[] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15};
    int n = sizeof(stream) / sizeof(stream[0]);
    int k = 5; // Number of samples to take
    
    printf("Original stream: ");
    for (int i = 0; i < n; i++) {
        printf("%d ", stream[i]);
    }
    printf("\n\n");
    
    // Perform reservoir sampling
    reservoir_sampling_unknown_size(stream, n, k);
    
    // Demonstrate with different sample size
    printf("\n--- Different sample size ---\n");
    k = 3;
    reservoir_sampling_unknown_size(stream, n, k);
    
    return 0;
}
```

## How it works:

1. **Initialization**: Fill the reservoir array with the first k elements from the stream
2. **Processing**: For each subsequent element at position i (where i ≥ k):
   - Generate a random number j between 0 and i (inclusive)
   - If j < k, replace reservoir[j] with the current element
3. **Result**: After processing all elements, the reservoir contains k randomly selected elements

## Key Properties:

- **Time Complexity**: O(n) where n is the size of the stream
- **Space Complexity**: O(k) where k is the number of samples
- **Probability**: Each element has equal probability (k/n) of being selected
- **Single Pass**: Only one traversal of the data is needed

## Sample Output:
```
Original stream: 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 

Reservoir sampling with k=5 from stream of size 15:
Sampled elements: 7 12 3 15 9 

--- Different sample size ---
Reservoir sampling with k=3 from stream of size 15:
Sampled elements: 1 8 14 
```

This implementation demonstrates the classic reservoir sampling algorithm that can be used when you need to randomly select a fixed number of items from a large dataset without knowing its total size beforehand.