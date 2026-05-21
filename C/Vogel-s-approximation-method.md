# Vogel's Approximation Method (VAM) in C

Vogel's Approximation Method is used to find an initial basic feasible solution for transportation problems. Here's a complete implementation:

```c
#include <stdio.h>
#include <stdlib.h>
#include <limits.h>

#define MAX 100

// Structure to represent a cell in transportation table
struct Cell {
    int supply;
    int demand;
    int cost;
    int allocation;
};

// Function to find the row with maximum penalty
int findMaxPenaltyRow(struct Cell table[][MAX], int rows, int cols, int supply[], int demand[]) {
    int maxPenalty = INT_MIN;
    int maxRow = -1;
    
    for (int i = 0; i < rows; i++) {
        if (supply[i] > 0) {
            int penalty = 0;
            int min1 = INT_MAX, min2 = INT_MAX;
            
            for (int j = 0; j < cols; j++) {
                if (demand[j] > 0 && table[i][j].cost < min1) {
                    min2 = min1;
                    min1 = table[i][j].cost;
                } else if (demand[j] > 0 && table[i][j].cost < min2) {
                    min2 = table[i][j].cost;
                }
            }
            
            if (min1 != INT_MAX && min2 != INT_MAX) {
                penalty = min2 - min1;
            }
            
            if (penalty > maxPenalty) {
                maxPenalty = penalty;
                maxRow = i;
            }
        }
    }
    
    return maxRow;
}

// Function to find the column with maximum penalty
int findMaxPenaltyCol(struct Cell table[][MAX], int rows, int cols, int supply[], int demand[]) {
    int maxPenalty = INT_MIN;
    int maxCol = -1;
    
    for (int j = 0; j < cols; j++) {
        if (demand[j] > 0) {
            int penalty = 0;
            int min1 = INT_MAX, min2 = INT_MAX;
            
            for (int i = 0; i < rows; i++) {
                if (supply[i] > 0 && table[i][j].cost < min1) {
                    min2 = min1;
                    min1 = table[i][j].cost;
                } else if (supply[i] > 0 && table[i][j].cost < min2) {
                    min2 = table[i][j].cost;
                }
            }
            
            if (min1 != INT_MAX && min2 != INT_MAX) {
                penalty = min2 - min1;
            }
            
            if (penalty > maxPenalty) {
                maxPenalty = penalty;
                maxCol = j;
            }
        }
    }
    
    return maxCol;
}

// Function to allocate minimum of supply and demand
void allocate(struct Cell table[][MAX], int supply[], int demand[], int row, int col) {
    int allocation = (supply[row] < demand[col]) ? supply[row] : demand[col];
    table[row][col].allocation = allocation;
    supply[row] -= allocation;
    demand[col] -= allocation;
}

// Main VAM function
void vogelApproximationMethod(struct Cell table[][MAX], int rows, int cols, int supply[], int demand[]) {
    printf("\n--- Vogel's Approximation Method ---\n");
    
    int totalCost = 0;
    int iterations = 0;
    
    while (1) {
        // Check if all supplies and demands are satisfied
        int allZero = 1;
        for (int i = 0; i < rows; i++) {
            if (supply[i] > 0) {
                allZero = 0;
                break;
            }
        }
        for (int j = 0; j < cols; j++) {
            if (demand[j] > 0) {
                allZero = 0;
                break;
            }
        }
        
        if (allZero) break;
        
        iterations++;
        printf("\nIteration %d:\n", iterations);
        
        // Find maximum penalty row
        int maxRow = findMaxPenaltyRow(table, rows, cols, supply, demand);
        // Find maximum penalty column
        int maxCol = findMaxPenaltyCol(table, rows, cols, supply, demand);
        
        printf("Max penalty row: %d, Max penalty col: %d\n", maxRow, maxCol);
        
        // Choose the row or column with maximum penalty
        if (maxRow != -1 && maxCol != -1) {
            if (table[maxRow][0].cost - table[maxRow][1].cost >= table[0][maxCol].cost - table[1][maxCol].cost) {
                // Allocate in row maxRow
                int minCostCol = -1;
                int minCost = INT_MAX;
                
                for (int j = 0; j < cols; j++) {
                    if (demand[j] > 0 && table[maxRow][j].cost < minCost) {
                        minCost = table[maxRow][j].cost;
                        minCostCol = j;
                    }
                }
                
                if (minCostCol != -1) {
                    printf("Allocating in row %d, col %d\n", maxRow, minCostCol);
                    allocate(table, supply, demand, maxRow, minCostCol);
                    totalCost += table[maxRow][minCostCol].cost * table[maxRow][minCostCol].allocation;
                }
            } else {
                // Allocate in column maxCol
                int minCostRow = -1;
                int minCost = INT_MAX;
                
                for (int i = 0; i < rows; i++) {
                    if (supply[i] > 0 && table[i][maxCol].cost < minCost) {
                        minCost = table[i][maxCol].cost;
                        minCostRow = i;
                    }
                }
                
                if (minCostRow != -1) {
                    printf("Allocating in row %d, col %d\n", minCostRow, maxCol);
                    allocate(table, supply, demand, minCostRow, maxCol);
                    totalCost += table[minCostRow][maxCol].cost * table[minCostRow][maxCol].allocation;
                }
            }
        } else if (maxRow != -1) {
            // Allocate in row maxRow
            int minCostCol = -1;
            int minCost = INT_MAX;
            
            for (int j = 0; j < cols; j++) {
                if (demand[j] > 0 && table[maxRow][j].cost < minCost) {
                    minCost = table[maxRow][j].cost;
                    minCostCol = j;
                }
            }
            
            if (minCostCol != -1) {
                printf("Allocating in row %d, col %d\n", maxRow, minCostCol);
                allocate(table, supply, demand, maxRow, minCostCol);
                totalCost += table[maxRow][minCostCol].cost * table[maxRow][minCostCol].allocation;
            }
        } else if (maxCol != -1) {
            // Allocate in column maxCol
            int minCostRow = -1;
            int minCost = INT_MAX;
            
            for (int i = 0; i < rows; i++) {
                if (supply[i] > 0 && table[i][maxCol].cost < minCost) {
                    minCost = table[i][maxCol].cost;
                    minCostRow = i;
                }
            }
            
            if (minCostRow != -1) {
                printf("Allocating in row %d, col %d\n", minCostRow, maxCol);
                allocate(table, supply, demand, minCostRow, maxCol);
                totalCost += table[minCostRow][maxCol].cost * table[minCostRow][maxCol].allocation;
            }
        }
        
        // Print current status
        printf("Current supply: ");
        for (int i = 0; i < rows; i++) {
            printf("%d ", supply[i]);
        }
        printf("\nCurrent demand: ");
        for (int j = 0; j < cols; j++) {
            printf("%d ", demand[j]);
        }
        printf("\nTotal cost so far: %d\n", totalCost);
    }
    
    printf("\n--- Final Result ---\n");
    printf("Total transportation cost: %d\n", totalCost);
    
    // Print final allocation table
    printf("\nFinal Allocation Table:\n");
    printf("   ");
    for (int j = 0; j < cols; j++) {
        printf("D%d  ", j + 1);
    }
    printf("\n");
    
    for (int i = 0; i < rows; i++) {
        printf("S%d ", i + 1);
        for (int j = 0; j < cols; j++) {
            printf("%d   ", table[i][j].allocation);
        }
        printf("\n");
    }
}

// Function to initialize the transportation table
void initializeTable(struct Cell table[][MAX], int rows, int cols, int supply[], int demand[], int costs[][MAX]) {
    for (int i = 0; i < rows; i++) {
        for (int j = 0; j < cols; j++) {
            table[i][j].supply = supply[i];
            table[i][j].demand = demand[j];
            table[i][j].cost = costs[i][j];
            table[i][j].allocation = 0;
        }
    }
}

int main() {
    int rows, cols;
    
    printf("Enter number of sources (rows): ");
    scanf("%d", &rows);
    printf("Enter number of destinations (columns): ");
    scanf("%d", &cols);
    
    // Arrays to store supply, demand, and cost matrix
    int supply[MAX], demand[MAX];
    int costs[MAX][MAX];
    struct Cell table[MAX][MAX];
    
    // Input supply values
    printf("Enter supply values:\n");
    for (int i = 0; i < rows; i++) {
        printf("Supply for source %d: ", i + 1);
        scanf("%d", &supply[i]);
    }
    
    // Input demand values
    printf("Enter demand values:\n");
    for (int j = 0; j < cols; j++) {
        printf("Demand for destination %d: ", j + 1);
        scanf("%d", &demand[j]);
    }
    
    // Input cost matrix
    printf("Enter cost matrix:\n");
    for (int i = 0; i < rows; i++) {
        for (int j = 0; j < cols; j++) {
            printf("Cost from source %d to destination %d: ", i + 1, j + 1);
            scanf("%d", &costs[i][j]);
        }
    }
    
    // Initialize the table
    initializeTable(table, rows, cols, supply, demand, costs);
    
    // Apply Vogel's Approximation Method
    vogelApproximationMethod(table, rows, cols, supply, demand);
    
    return 0;
}
```

## Example Usage

Here's a sample run with a 3x4 transportation problem:

```
Enter number of sources (rows): 3
Enter number of destinations (columns): 4
Enter supply values:
Supply for source 1: 300
Supply for source 2: 400
Supply for source 3: 500
Enter demand values:
Demand for destination 1: 250
Demand for destination 2: 350
Demand for destination 3: 400
Demand for destination 4: 200
Enter cost matrix:
Cost from source 1 to destination 1: 8
Cost from source 1 to destination 2: 6
Cost from source 1 to destination 3: 10
Cost from source 1 to destination 4: 9
Cost from source 2 to destination 1: 9
Cost from source 2 to destination 2: 12
Cost from source 2 to destination 3: 13
Cost from source 2 to destination 4: 7
Cost from source 3 to destination 1: 14
Cost from source 3 to destination 2: 9
Cost from source 3 to destination 3: 16
Cost from source 3 to destination 4: 5
```

## Key Features of the Implementation

1. **Penalty Calculation**: Calculates penalties for each row and column
2. **Maximum Penalty Selection**: Chooses the row/column with maximum penalty
3. **Minimum Cost Allocation**: Allocates in the cell with minimum cost in the selected row/column
4. **Iteration Process**: Continues until all supplies and demands are satisfied
5. **Cost Tracking**: Keeps track of total transportation cost
6. **Detailed Output**: Shows step-by-step process and final result

The algorithm provides a good initial feasible solution that is often close to the optimal solution for transportation problems.

