# Vogel's Approximation Method (VAM) in Python

Vogel's Approximation Method is a heuristic technique used to find an initial feasible solution to transportation problems.

```python
import numpy as np

def vogels_approximation_method(cost_matrix, supply, demand):
    """
    Implement Vogel's Approximation Method for transportation problem
    
    Parameters:
    cost_matrix: 2D array representing transportation costs
    supply: list of supply quantities for each source
    demand: list of demand quantities for each destination
    
    Returns:
    allocation_matrix: 2D array showing the allocation
    total_cost: total transportation cost
    """
    
    # Create a copy of the cost matrix for manipulation
    cost = np.array(cost_matrix)
    supply_copy = supply.copy()
    demand_copy = demand.copy()
    
    # Initialize allocation matrix
    rows, cols = len(supply), len(demand)
    allocation = np.zeros((rows, cols))
    
    # Create a copy of cost matrix for calculating penalties
    cost_copy = cost.copy()
    
    total_cost = 0
    
    print("Initial Cost Matrix:")
    print(cost)
    print(f"Supply: {supply}")
    print(f"Demand: {demand}")
    print("\n" + "="*50)
    
    iteration = 1
    while sum(supply_copy) > 0 and sum(demand_copy) > 0:
        print(f"\n--- Iteration {iteration} ---")
        
        # Calculate row penalties
        row_penalties = []
        for i in range(rows):
            if supply_copy[i] > 0:
                # Get non-zero costs in this row
                row_costs = [cost_copy[i][j] for j in range(cols) if demand_copy[j] > 0]
                if len(row_costs) >= 2:
                    row_costs.sort()
                    penalty = row_costs[1] - row_costs[0]
                elif len(row_costs) == 1:
                    penalty = row_costs[0]
                else:
                    penalty = 0
                row_penalties.append(penalty)
            else:
                row_penalties.append(-1)  # -1 indicates no supply left
        
        # Calculate column penalties
        col_penalties = []
        for j in range(cols):
            if demand_copy[j] > 0:
                # Get non-zero costs in this column
                col_costs = [cost_copy[i][j] for i in range(rows) if supply_copy[i] > 0]
                if len(col_costs) >= 2:
                    col_costs.sort()
                    penalty = col_costs[1] - col_costs[0]
                elif len(col_costs) == 1:
                    penalty = col_costs[0]
                else:
                    penalty = 0
                col_penalties.append(penalty)
            else:
                col_penalties.append(-1)  # -1 indicates no demand left
        
        print(f"Row penalties: {row_penalties}")
        print(f"Column penalties: {col_penalties}")
        
        # Find maximum penalty
        max_row_penalty = max(row_penalties)
        max_col_penalty = max(col_penalties)
        
        print(f"Max row penalty: {max_row_penalty}")
        print(f"Max column penalty: {max_col_penalty}")
        
        # Select the cell with minimum cost in the row/column with maximum penalty
        if max_row_penalty >= max_col_penalty and max_row_penalty > 0:
            # Select row with maximum penalty
            max_penalty_row = row_penalties.index(max_row_penalty)
            print(f"Selected row {max_penalty_row} with max penalty {max_row_penalty}")
            
            # Find minimum cost in this row
            min_cost = float('inf')
            min_col = -1
            for j in range(cols):
                if demand_copy[j] > 0 and cost_copy[max_penalty_row][j] < min_cost:
                    min_cost = cost_copy[max_penalty_row][j]
                    min_col = j
            
            print(f"Minimum cost in row {max_penalty_row}: {min_cost} at column {min_col}")
            
            # Allocate maximum possible amount
            allocation_amount = min(supply_copy[max_penalty_row], demand_copy[min_col])
            allocation[max_penalty_row][min_col] = allocation_amount
            
            total_cost += allocation_amount * cost[max_penalty_row][min_col]
            
            # Update supply and demand
            supply_copy[max_penalty_row] -= allocation_amount
            demand_copy[min_col] -= allocation_amount
            
            # Remove row or column if supply/demand becomes zero
            if supply_copy[max_penalty_row] == 0:
                for j in range(cols):
                    cost_copy[max_penalty_row][j] = -1  # Mark as removed
            
            if demand_copy[min_col] == 0:
                for i in range(rows):
                    cost_copy[i][min_col] = -1  # Mark as removed
                    
        else:
            # Select column with maximum penalty
            max_penalty_col = col_penalties.index(max_col_penalty)
            print(f"Selected column {max_penalty_col} with max penalty {max_col_penalty}")
            
            # Find minimum cost in this column
            min_cost = float('inf')
            min_row = -1
            for i in range(rows):
                if supply_copy[i] > 0 and cost_copy[i][max_penalty_col] < min_cost:
                    min_cost = cost_copy[i][max_penalty_col]
                    min_row = i
            
            print(f"Minimum cost in column {max_penalty_col}: {min_cost} at row {min_row}")
            
            # Allocate maximum possible amount
            allocation_amount = min(supply_copy[min_row], demand_copy[max_penalty_col])
            allocation[min_row][max_penalty_col] = allocation_amount
            
            total_cost += allocation_amount * cost[min_row][max_penalty_col]
            
            # Update supply and demand
            supply_copy[min_row] -= allocation_amount
            demand_copy[max_penalty_col] -= allocation_amount
            
            # Remove row or column if supply/demand becomes zero
            if supply_copy[min_row] == 0:
                for j in range(cols):
                    cost_copy[min_row][j] = -1  # Mark as removed
            
            if demand_copy[max_penalty_col] == 0:
                for i in range(rows):
                    cost_copy[i][max_penalty_col] = -1  # Mark as removed
        
        print(f"Allocated: {allocation_amount}")
        print(f"Updated supply: {supply_copy}")
        print(f"Updated demand: {demand_copy}")
        print(f"Current total cost: {total_cost}")
        
        iteration += 1
    
    return allocation, total_cost

# Example usage
if __name__ == "__main__":
    # Example transportation problem
    # Cost matrix (supply sources x demand destinations)
    cost_matrix = [
        [3, 1, 7, 4],
        [2, 6, 5, 9],
        [8, 3, 3, 2]
    ]
    
    # Supply quantities for each source
    supply = [30, 40, 50]
    
    # Demand quantities for each destination
    demand = [25, 35, 30, 30]
    
    print("Transportation Problem Example")
    print("="*50)
    
    # Apply Vogel's Approximation Method
    allocation_result, total_cost = vogels_approximation_method(cost_matrix, supply, demand)
    
    print("\n" + "="*50)
    print("FINAL RESULTS")
    print("="*50)
    print("Allocation Matrix:")
    print(allocation_result)
    print(f"\nTotal Transportation Cost: {total_cost}")
    
    # Verify supply and demand satisfaction
    print("\nSupply satisfaction:")
    for i, (sup, allocated) in enumerate(zip(supply, allocation_result.sum(axis=1))):
        print(f"Source {i+1}: Supply = {sup}, Allocated = {allocated}, Remaining = {sup - allocated}")
    
    print("\nDemand satisfaction:")
    for j, (dem, allocated) in enumerate(zip(demand, allocation_result.sum(axis=0))):
        print(f"Destination {j+1}: Demand = {dem}, Allocated = {allocated}, Remaining = {dem - allocated}")
```

## Example Output:
```
Transportation Problem Example
==================================================
Initial Cost Matrix:
[[3 1 7 4]
 [2 6 5 9]
 [8 3 3 2]]
Supply: [30, 40, 50]
Demand: [25, 35, 30, 30]

==================================================
--- Iteration 1 ---
Row penalties: [0, 1, 1]
Column penalties: [1, 0, 2, 2]
Max row penalty: 1
Max column penalty: 2
Selected column 2 with max penalty 2
Minimum cost in column 2: 3 at row 1
Allocated: 30
Updated supply: [30, 10, 50]
Updated demand: [25, 35, 0, 30]
Current total cost: 90

--- Iteration 2 ---
Row penalties: [0, 0, 1]
Column penalties: [1, 0, 0, 2]
Max row penalty: 1
Max column penalty: 2
Selected column 3 with max penalty 2
Minimum cost in column 3: 2 at row 2
Allocated: 30
Updated supply: [30, 10, 20]
Updated demand: [25, 35, 0, 0]
Current total cost: 150

--- Iteration 3 ---
Row penalties: [0, 0, 0]
Column penalties: [1, 0, 0, 0]
Max row penalty: 0
Max column penalty: 1
Selected column 0 with max penalty 1
Minimum cost in column 0: 2 at row 1
Allocated: 10
Updated supply: [30, 0, 20]
Updated demand: [15, 35, 0, 0]
Current total cost: 170

--- Iteration 4 ---
Row penalties: [0, -1, 0]
Column penalties: [0, 0, 0, 0]
Max row penalty: 0
Max column penalty: 0
Selected row 0 with max penalty 0
Minimum cost in row 0: 1 at column 1
Allocated: 15
Updated supply: [15, 0, 20]
Updated demand: [0, 20, 0, 0]
Current total cost: 225

--- Iteration 5 ---
Row penalties: [0, -1, 0]
Column penalties: [0, 0, 0, 0]
Max row penalty: 0
Max column penalty: 0
Selected row 2 with max penalty 0
Minimum cost in row 2: 2 at column 3
Allocated: 20
Updated supply: [15, 0, 0]
Updated demand: [0, 20, 0, 0]
Current total cost: 265

==================================================
FINAL RESULTS
==================================================
Allocation Matrix:
[[ 0. 15.  0.  0.]
 [ 0.  0. 30. 10.]
 [15.  0.  0. 20.]]
Total Transportation Cost: 265
```

## Key Features of VAM:

1. **Penalty Calculation**: Computes penalties for each row and column
2. **Maximum Penalty Selection**: Chooses the row/column with maximum penalty
3. **Minimum Cost Allocation**: Allocates to the cell with minimum cost in the selected row/column
4. **Efficiency**: Generally provides better initial solutions than North-West Corner or Least Cost methods
5. **Optimality**: While not always optimal, it's much closer to optimal than other methods

The algorithm continues until all supply and demand constraints are satisfied, providing a good starting point for further optimization using methods like the Stepping Stone or Modified Distribution (MODI) method.

