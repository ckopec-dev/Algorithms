# Patience Sorting Algorithm in C

Here's a complete implementation of the Patience Sorting algorithm in C:

```c
#include <stdio.h>
#include <stdlib.h>

// Structure to represent a pile in patience sorting
typedef struct Pile {
    int value;
    struct Pile* next;
} Pile;

// Structure to represent a deck of cards (pile)
typedef struct Deck {
    Pile* top;
    int size;
} Deck;

// Function to create a new pile
Pile* createPile(int value) {
    Pile* pile = (Pile*)malloc(sizeof(Pile));
    pile->value = value;
    pile->next = NULL;
    return pile;
}

// Function to insert a value into the correct pile
void insertIntoPile(Deck* piles, int value, int pileCount) {
    // Find the first pile with top value greater than or equal to value
    for (int i = 0; i < pileCount; i++) {
        if (piles[i].top != NULL && piles[i].top->value >= value) {
            // Insert at the top of this pile
            Pile* newPile = createPile(value);
            newPile->next = piles[i].top;
            piles[i].top = newPile;
            piles[i].size++;
            return;
        }
    }
    
    // If no suitable pile found, create a new pile
    piles[pileCount].top = createPile(value);
    piles[pileCount].size = 1;
}

// Function to perform patience sorting
void patienceSort(int arr[], int n) {
    if (n <= 1) return;
    
    // Create array of piles
    Deck* piles = (Deck*)malloc(n * sizeof(Deck));
    
    // Initialize all piles
    for (int i = 0; i < n; i++) {
        piles[i].top = NULL;
        piles[i].size = 0;
    }
    
    int pileCount = 0;
    
    // Distribute elements into piles
    for (int i = 0; i < n; i++) {
        insertIntoPile(piles, arr[i], pileCount);
        if (piles[pileCount].top != NULL) {
            pileCount++;
        }
    }
    
    // Merge piles back into array
    int index = 0;
    while (index < n) {
        int min = 1000000;  // Large number
        int minIndex = -1;
        
        // Find minimum value among top of all piles
        for (int i = 0; i < pileCount; i++) {
            if (piles[i].top != NULL && piles[i].top->value < min) {
                min = piles[i].top->value;
                minIndex = i;
            }
        }
        
        // Remove minimum value from its pile and add to array
        if (minIndex != -1) {
            arr[index++] = min;
            Pile* temp = piles[minIndex].top;
            piles[minIndex].top = piles[minIndex].top->next;
            free(temp);
            piles[minIndex].size--;
        }
    }
    
    // Free memory
    for (int i = 0; i < pileCount; i++) {
        while (piles[i].top != NULL) {
            Pile* temp = piles[i].top;
            piles[i].top = piles[i].top->next;
            free(temp);
        }
    }
    free(piles);
}

// Function to print array
void printArray(int arr[], int n) {
    for (int i = 0; i < n; i++) {
        printf("%d ", arr[i]);
    }
    printf("\n");
}

// Example usage
int main() {
    int arr[] = {64, 34, 25, 12, 22, 11, 90};
    int n = sizeof(arr) / sizeof(arr[0]);
    
    printf("Original array: ");
    printArray(arr, n);
    
    patienceSort(arr, n);
    
    printf("Sorted array:   ");
    printArray(arr, n);
    
    return 0;
}
```

## How Patience Sorting Works

1. **Pile Creation**: Elements are placed into piles such that each pile is in decreasing order from top to bottom
2. **Insertion**: Each new element is placed on the leftmost pile whose top element is greater than or equal to the new element
3. **Merging**: The sorted array is constructed by repeatedly selecting the smallest element from the top of all piles

## Example Output

```
Original array: 64 34 25 12 22 11 90 
Sorted array:   11 12 22 25 34 64 90 
```

## Time and Space Complexity

- **Time Complexity**: O(n log n) average case, O(n²) worst case
- **Space Complexity**: O(n) for the piles storage

## Key Features

- **Stable Sorting**: Maintains relative order of equal elements
- **In-place**: Minimal additional memory usage
- **Adaptive**: Performs well on partially sorted data
- **Online Algorithm**: Can sort data as it arrives

This implementation demonstrates the core concepts of patience sorting while maintaining good performance characteristics for most practical applications.

