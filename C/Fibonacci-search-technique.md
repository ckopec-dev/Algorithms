Here is a concise, self‑contained implementation of the **Fibonacci Search** technique in C.  
Fibonacci search works on a **sorted** array and uses Fibonacci numbers to narrow down the search interval, achieving O(log n) time complexity without the need for division or modulus operations.

```c
/*  Fibonacci Search in C
 *  -------------------------------------------------
 *  Compile:  gcc -Wall -Wextra -std=c11 fib_search.c -o fib_search
 *  Run:      ./fib_search
 *
 *  The function returns the index of `key` if found,
 *  otherwise -1.
 */

#include <stdio.h>
#include <stdlib.h>

/* -------------------------------------------------
 *  Returns the smallest Fibonacci number >= n
 *  and also stores the two preceding Fibonacci numbers
 *  in fibMm2 and fibMm1 (by reference).
 * ------------------------------------------------- */
static void getFibNumbers(int n, int *fibM, int *fibMm1, int *fibMm2)
{
    /* Initialize the first two Fibonacci numbers */
    int fibMMm2 = 0;   /* (m-2)'th Fibonacci No. */
    int fibMMm1 = 1;   /* (m-1)'th Fibonacci No. */
    int fibM   = fibMMm2 + fibMMm1; /* m'th Fibonacci No. */

    /* fibM is going to store the smallest Fibonacci
       Number greater than or equal to n */
    while (fibM < n) {
        fibMMm2 = fibMMm1;
        fibMMm1 = fibM;
        fibM   = fibMMm2 + fibMMm1;
    }

    *fibM   = fibM;
    *fibMm1 = fibMMm1;
    *fibMm2 = fibMMm2;
}

/* -------------------------------------------------
 *  Fibonacci Search
 *  arr[]   : sorted input array
 *  n       : number of elements in arr[]
 *  key     : value to be searched
 *  return  : index of key if present, else -1
 * ------------------------------------------------- */
int fibonacciSearch(const int arr[], int n, int key)
{
    /* Edge cases */
    if (n == 0) return -1;

    /* Initialize fibonacci numbers */
    int fibM, fibMm1, fibMm2;
    getFibNumbers(n, &fibM, &fibMm1, &fibMm2);

    /* Marks the eliminated range from front */
    int offset = -1;

    /* While there are elements to be inspected.
       Note that we compare arr[fibMm2 + offset] with key */
    while (fibM > 1) {
        /* Check if fibMm2 is a valid location */
        int i = (offset + fibMm2 < n) ? offset + fibMm2 : n - 1;

        if (arr[i] < key) {
            /* Move three Fibonacci variables down one */
            fibM   = fibMm1;
            fibMm1 = fibMm2;
            fibMm2 = fibM - fibMm1;
            offset = i;                     /* offset = i */
        }
        else if (arr[i] > key) {
            /* Move three Fibonacci variables down two */
            fibM   = fibMm2;
            fibMm1 = fibMm1 - fibMm2;
            fibMm2 = fibM - fibMm1;
            /* offset unchanged */
        }
        else {
            return i; /* key found */
        }
    }

    /* Comparing the last element with key */
    if (fibMm1 && (offset + 1 < n) && arr[offset + 1] == key)
        return offset + 1;

    /* Element not found. return -1 */
    return -1;
}

/* -------------------------------------------------
 *  Simple test driver
 * ------------------------------------------------- */
int main(void)
{
    int arr[] = {10, 22, 35, 40, 45, 50, 80, 82, 85, 90, 100};
    int n = sizeof(arr) / sizeof(arr[0]);
    int key;

    printf("Sorted array: ");
    for (int i = 0; i < n; ++i) printf("%d ", arr[i]);
    printf("\n");

    while (1) {
        printf("\nEnter value to search (or non‑numeric to quit): ");
        if (scanf("%d", &key) != 1) break;

        int index = fibonacciSearch(arr, n, key);
        if (index >= 0)
            printf("Found %d at index %d.\n", key, index);
        else
            printf("%d is not present in the array.\n", key);
    }
    return 0;
}
```

### How It Works
1. **Fibonacci Initialization** – Find the smallest Fibonacci number `fibM` that is **≥ n** (the array size).  
   The two preceding Fibonacci numbers (`fibMm1`, `fibMm2`) help us compute probe positions.
2. **Search Loop** –  
   * Compare `key` with the element at index `offset + fibMm2`.  
   * If the element is smaller, discard the left part (`offset` moves forward).  
   * If the element is larger, discard the right part (`fibM` shifts down two Fibonacci steps).  
   * If equal, we have found the key.
3. **Final Check** – After the loop, there may be one element left to check (`offset + 1`).

### Complexity
* **Time:** `O(log n)` – each iteration reduces the search space by a fraction related to the golden ratio.  
* **Space:** `O(1)` – only a few integer variables are used.

Feel free to copy the code into a file (e.g., `fib_search.c`), compile, and run it to see Fibonacci search in action!