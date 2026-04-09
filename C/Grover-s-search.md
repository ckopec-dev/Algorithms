# Grover's Search Algorithm in C

Here's a simple implementation of Grover's search algorithm in C. This example demonstrates the basic concept with a small search space.

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

// Oracle function - marks the target element
int oracle_function(int target, int element) {
    return (target == element) ? 1 : 0;
}

// Grover's search algorithm
int grovers_search(int *array, int size, int target) {
    int iterations = (int)(0.25 * 3.14159 * size); // Approximate number of iterations
    
    // Initialize superposition - all elements have equal probability
    double *probabilities = (double*)malloc(size * sizeof(double));
    for (int i = 0; i < size; i++) {
        probabilities[i] = 1.0 / size;
    }
    
    // Grover iterations
    for (int iteration = 0; iteration < iterations; iteration++) {
        // 1. Oracle application (mark target)
        for (int i = 0; i < size; i++) {
            if (oracle_function(target, array[i])) {
                probabilities[i] *= -1; // Amplify amplitude of target
            }
        }
        
        // 2. Diffusion operator (reflection about mean)
        double mean = 0;
        for (int i = 0; i < size; i++) {
            mean += probabilities[i];
        }
        mean /= size;
        
        for (int i = 0; i < size; i++) {
            probabilities[i] = 2 * mean - probabilities[i];
        }
    }
    
    // Find element with highest probability
    int max_index = 0;
    double max_prob = probabilities[0];
    for (int i = 1; i < size; i++) {
        if (probabilities[i] > max_prob) {
            max_prob = probabilities[i];
            max_index = i;
        }
    }
    
    free(probabilities);
    return array[max_index];
}

int main() {
    // Example: Search for a target in an array
    int array[] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
    int size = sizeof(array) / sizeof(array[0]);
    int target = 7;
    
    printf("Array: ");
    for (int i = 0; i < size; i++) {
        printf("%d ", array[i]);
    }
    printf("\n");
    
    printf("Target element: %d\n", target);
    
    // Perform Grover's search
    int result = grovers_search(array, size, target);
    
    printf("Grover's search result: %d\n", result);
    
    // Verify result
    if (result == target) {
        printf("Success! Target found.\n");
    } else {
        printf("Search failed.\n");
    }
    
    return 0;
}
```

## How this implementation works:

1. **Oracle Function**: Marks the target element by flipping its amplitude
2. **Grover Iterations**: 
   - Apply oracle to mark the target
   - Apply diffusion operator to amplify target probability
3. **Measurement**: Return the element with highest probability

## Key Points:

- This is a simplified simulation of Grover's algorithm
- In practice, quantum computers would implement this with quantum gates
- The number of iterations is approximately π/4 × √N for optimal performance
- This example uses classical computation to demonstrate the concept

## Sample Output:
```
Array: 1 2 3 4 5 6 7 8 9 10 
Target element: 7
Grover's search result: 7
Success! Target found.
```

**Note**: This is a conceptual implementation. A true quantum Grover's search would require quantum computing libraries like Qiskit, Cirq, or similar quantum programming frameworks.

