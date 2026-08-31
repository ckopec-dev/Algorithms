# Vogel's Approximation Method (VAM) in C++

Vogel's Approximation Method is used to find an initial basic feasible solution for transportation problems. Here's a complete implementation:

```cpp
#include <iostream>
#include <vector>
#include <climits>
#include <iomanip>

using namespace std;

class TransportationProblem {
private:
    vector<vector<int>> cost;        // Cost matrix
    vector<int> supply;              // Supply quantities
    vector<int> demand;              // Demand quantities
    int rows, cols;
    
public:
    TransportationProblem(int r, int c) : rows(r), cols(c) {
        cost.resize(rows, vector<int>(cols));
        supply.resize(rows);
        demand.resize(cols);
    }
    
    void inputCostMatrix() {
        cout << "Enter the cost matrix:" << endl;
        for (int i = 0; i < rows; i++) {
            for (int j = 0; j < cols; j++) {
                cin >> cost[i][j];
            }
        }
    }
    
    void inputSupply() {
        cout << "Enter supply quantities:" << endl;
        for (int i = 0; i < rows; i++) {
            cin >> supply[i];
        }
    }
    
    void inputDemand() {
        cout << "Enter demand quantities:" << endl;
        for (int j = 0; j < cols; j++) {
            cin >> demand[j];
        }
    }
    
    // Calculate penalty for a row
    int calculateRowPenalty(int row) {
        if (supply[row] == 0) return 0;
        
        int min1 = INT_MAX, min2 = INT_MAX;
        for (int j = 0; j < cols; j++) {
            if (demand[j] > 0 && cost[row][j] < min1) {
                min2 = min1;
                min1 = cost[row][j];
            } else if (demand[j] > 0 && cost[row][j] < min2) {
                min2 = cost[row][j];
            }
        }
        
        return (min1 == INT_MAX) ? 0 : (min2 - min1);
    }
    
    // Calculate penalty for a column
    int calculateColPenalty(int col) {
        if (demand[col] == 0) return 0;
        
        int min1 = INT_MAX, min2 = INT_MAX;
        for (int i = 0; i < rows; i++) {
            if (supply[i] > 0 && cost[i][col] < min1) {
                min2 = min1;
                min1 = cost[i][col];
            } else if (supply[i] > 0 && cost[i][col] < min2) {
                min2 = cost[i][col];
            }
        }
        
        return (min1 == INT_MAX) ? 0 : (min2 - min1);
    }
    
    // Find maximum penalty
    pair<int, char> findMaxPenalty() {
        int maxRowPenalty = -1;
        int maxColPenalty = -1;
        char maxPenaltyType = 'r'; // 'r' for row, 'c' for column
        
        // Calculate row penalties
        for (int i = 0; i < rows; i++) {
            if (supply[i] > 0) {
                int penalty = calculateRowPenalty(i);
                if (penalty > maxRowPenalty) {
                    maxRowPenalty = penalty;
                    maxPenaltyType = 'r';
                }
            }
        }
        
        // Calculate column penalties
        for (int j = 0; j < cols; j++) {
            if (demand[j] > 0) {
                int penalty = calculateColPenalty(j);
                if (penalty > maxColPenalty) {
                    maxColPenalty = penalty;
                    maxPenaltyType = 'c';
                }
            }
        }
        
        if (maxRowPenalty >= maxColPenalty) {
            return make_pair(maxRowPenalty, 'r');
        } else {
            return make_pair(maxColPenalty, 'c');
        }
    }
    
    // Find minimum cost cell in the selected row/column
    pair<int, int> findMinCostCell(int index, char type) {
        if (type == 'r') {  // Row selection
            int minCost = INT_MAX;
            int colIndex = -1;
            for (int j = 0; j < cols; j++) {
                if (demand[j] > 0 && cost[index][j] < minCost) {
                    minCost = cost[index][j];
                    colIndex = j;
                }
            }
            return make_pair(index, colIndex);
        } else {  // Column selection
            int minCost = INT_MAX;
            int rowIndex = -1;
            for (int i = 0; i < rows; i++) {
                if (supply[i] > 0 && cost[i][index] < minCost) {
                    minCost = cost[i][index];
                    rowIndex = i;
                }
            }
            return make_pair(rowIndex, index);
        }
    }
    
    // Apply Vogel's Approximation Method
    vector<vector<int>> vogelsApproximationMethod() {
        vector<vector<int>> solution(rows, vector<int>(cols, 0));
        
        int totalSupply = 0, totalDemand = 0;
        
        for (int i = 0; i < rows; i++) totalSupply += supply[i];
        for (int j = 0; j < cols; j++) totalDemand += demand[j];
        
        if (totalSupply != totalDemand) {
            cout << "Warning: Supply and demand are not balanced!" << endl;
            return solution;
        }
        
        // Copy arrays to avoid modifying original
        vector<int> tempSupply = supply;
        vector<int> tempDemand = demand;
        
        while (true) {
            pair<int, char> maxPenalty = findMaxPenalty();
            
            if (maxPenalty.first == -1) break;  // No more cells to process
            
            pair<int, int> minCell = findMinCostCell(maxPenalty.first, maxPenalty.second);
            
            int i = minCell.first;
            int j = minCell.second;
            
            // Allocate maximum possible quantity
            int allocation = min(tempSupply[i], tempDemand[j]);
            solution[i][j] = allocation;
            
            // Update supply and demand
            tempSupply[i] -= allocation;
            tempDemand[j] -= allocation;
        }
        
        return solution;
    }
    
    // Display the cost matrix
    void displayCostMatrix() {
        cout << "\nCost Matrix:" << endl;
        cout << "     ";
        for (int j = 0; j < cols; j++) {
            cout << "D" << j + 1 << "   ";
        }
        cout << endl;
        
        for (int i = 0; i < rows; i++) {
            cout << "S" << i + 1 << "  ";
            for (int j = 0; j < cols; j++) {
                cout << cost[i][j] << "   ";
            }
            cout << endl;
        }
    }
    
    // Display supply and demand
    void displaySupplyDemand() {
        cout << "\nSupply: ";
        for (int i = 0; i < rows; i++) {
            cout << supply[i] << " ";
        }
        cout << endl;
        
        cout << "Demand: ";
        for (int j = 0; j < cols; j++) {
            cout << demand[j] << " ";
        }
        cout << endl;
    }
    
    // Display the solution
    void displaySolution(vector<vector<int>>& solution) {
        cout << "\nInitial Basic Feasible Solution:" << endl;
        cout << "     ";
        for (int j = 0; j < cols; j++) {
            cout << "D" << j + 1 << "   ";
        }
        cout << endl;
        
        for (int i = 0; i < rows; i++) {
            cout << "S" << i + 1 << "  ";
            for (int j = 0; j < cols; j++) {
                cout << solution[i][j] << "   ";
            }
            cout << endl;
        }
        
        // Calculate total cost
        int totalCost = 0;
        for (int i = 0; i < rows; i++) {
            for (int j = 0; j < cols; j++) {
                totalCost += solution[i][j] * cost[i][j];
            }
        }
        
        cout << "\nTotal Transportation Cost: " << totalCost << endl;
    }
};

int main() {
    int rows, cols;
    
    cout << "Vogel's Approximation Method for Transportation Problem" << endl;
    cout << "Enter number of sources (rows): ";
    cin >> rows;
    cout << "Enter number of destinations (columns): ";
    cin >> cols;
    
    TransportationProblem tp(rows, cols);
    
    tp.inputCostMatrix();
    tp.inputSupply();
    tp.inputDemand();
    
    tp.displayCostMatrix();
    tp.displaySupplyDemand();
    
    vector<vector<int>> solution = tp.vogelsApproximationMethod();
    tp.displaySolution(solution);
    
    return 0;
}
```

## Example Usage

**Sample Input:**
```
Enter number of sources (rows): 3
Enter number of destinations (columns): 4

Enter the cost matrix:
19 23 35 25
17 21 33 29
18 20 30 26

Enter supply quantities:
300 400 500

Enter demand quantities:
250 350 400 200
```

## How the Algorithm Works:

1. **Calculate Penalties**: For each row and column, find the difference between the two smallest costs
2. **Select Maximum Penalty**: Choose the row or column with the highest penalty
3. **Allocate Minimum Cost**: In the selected row/column, allocate to the cell with minimum cost
4. **Update Supply/Demand**: Reduce supply/demand accordingly
5. **Repeat**: Continue until all allocations are made

## Key Features:

- Handles balanced transportation problems
- Calculates penalties for both rows and columns
- Finds optimal initial solution using Vogel's method
- Displays step-by-step solution with total cost
- Includes error checking for unbalanced problems

The algorithm typically provides a much better initial solution compared to other methods like North-West Corner Rule, often closer to the optimal solution.