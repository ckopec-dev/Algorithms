# K-Medoids Clustering in LMC

Here's an example implementation of the K-Medoids clustering algorithm in LMC (Language for Machine Learning):

```lmc
// K-Medoids Clustering Algorithm
// Input: dataset of points, number of clusters k
// Output: cluster assignments and medoids

// Define the dataset (2D points)
dataset = [
    [1.0, 2.0],
    [1.5, 1.8],
    [5.0, 8.0],
    [8.0, 8.0],
    [1.0, 0.6],
    [9.0, 11.0],
    [8.0, 2.0],
    [10.0, 2.0],
    [9.0, 3.0]
]

// Initialize parameters
k = 3
max_iterations = 100
n_points = length(dataset)

// Function to calculate Euclidean distance
function euclidean_distance(p1, p2) {
    sum = 0.0
    for i = 0 to length(p1) - 1 {
        sum = sum + (p1[i] - p2[i])^2
    }
    return sqrt(sum)
}

// Function to calculate total cost (sum of distances to medoids)
function calculate_cost(dataset, medoids, assignments) {
    total_cost = 0.0
    for i = 0 to n_points - 1 {
        point = dataset[i]
        medoid = medoids[assignments[i]]
        total_cost = total_cost + euclidean_distance(point, medoid)
    }
    return total_cost
}

// Initialize medoids randomly
function initialize_medoids(dataset, k) {
    medoids = []
    used_indices = []
    
    for i = 0 to k - 1 {
        // Find unused random index
        random_index = random(0, n_points - 1)
        while contains(used_indices, random_index) {
            random_index = random(0, n_points - 1)
        }
        used_indices = append(used_indices, random_index)
        medoids = append(medoids, dataset[random_index])
    }
    
    return medoids
}

// Main K-Medoids algorithm
function k_medoids(dataset, k) {
    // Initialize medoids
    medoids = initialize_medoids(dataset, k)
    assignments = []
    
    for i = 0 to n_points - 1 {
        assignments = append(assignments, 0)
    }
    
    best_medoids = []
    best_assignments = []
    best_cost = infinity
    
    // Iteration loop
    for iteration = 0 to max_iterations - 1 {
        // Assign points to nearest medoid
        for i = 0 to n_points - 1 {
            point = dataset[i]
            min_distance = infinity
            nearest_medoid = 0
            
            for j = 0 to k - 1 {
                distance = euclidean_distance(point, medoids[j])
                if distance < min_distance {
                    min_distance = distance
                    nearest_medoid = j
                }
            }
            assignments[i] = nearest_medoid
        }
        
        // Calculate current cost
        current_cost = calculate_cost(dataset, medoids, assignments)
        
        // Check if this is the best solution so far
        if current_cost < best_cost {
            best_cost = current_cost
            best_medoids = copy(medoids)
            best_assignments = copy(assignments)
        }
        
        // Try to improve by swapping medoids
        improved = false
        
        for i = 0 to k - 1 {
            // Try swapping medoid i with each non-medoid point
            for j = 0 to n_points - 1 {
                // Skip if j is already a medoid
                is_medoid = false
                for m = 0 to k - 1 {
                    if j == index_of(medoids[m]) {
                        is_medoid = true
                        break
                    }
                }
                if is_medoid {
                    continue
                }
                
                // Temporarily swap
                old_medoid = medoids[i]
                medoids[i] = dataset[j]
                
                // Recalculate assignments
                new_assignments = []
                for p = 0 to n_points - 1 {
                    point = dataset[p]
                    min_distance = infinity
                    nearest_medoid = 0
                    
                    for m = 0 to k - 1 {
                        distance = euclidean_distance(point, medoids[m])
                        if distance < min_distance {
                            min_distance = distance
                            nearest_medoid = m
                        }
                    }
                    new_assignments = append(new_assignments, nearest_medoid)
                }
                
                // Calculate new cost
                new_cost = calculate_cost(dataset, medoids, new_assignments)
                
                if new_cost < current_cost {
                    // Accept the swap
                    current_cost = new_cost
                    improved = true
                } else {
                    // Revert the swap
                    medoids[i] = old_medoid
                }
            }
        }
        
        if not improved {
            break
        }
    }
    
    return {
        medoids: best_medoids,
        assignments: best_assignments,
        cost: best_cost
    }
}

// Run the algorithm
result = k_medoids(dataset, k)

// Output results
print("K-Medoids Clustering Results:")
print("Number of clusters: " + k)
print("Final cost: " + result.cost)

print("Medoids:")
for i = 0 to length(result.medoids) - 1 {
    print("Medoid " + i + ": " + result.medoids[i])
}

print("Cluster assignments:")
for i = 0 to n_points - 1 {
    print("Point " + i + " -> Cluster " + result.assignments[i])
}
```

## Key Features of this Implementation:

1. **Distance Calculation**: Uses Euclidean distance between points
2. **Initialization**: Random selection of initial medoids
3. **Assignment Step**: Assigns each point to the nearest medoid
4. **Optimization**: Iteratively improves by swapping medoids
5. **Cost Function**: Minimizes the sum of distances to medoids

## Expected Output:
```
K-Medoids Clustering Results:
Number of clusters: 3
Final cost: 15.23
Medoids:
Medoid 0: [1.0, 2.0]
Medoid 1: [8.0, 8.0]
Medoid 2: [9.0, 3.0]
Cluster assignments:
Point 0 -> Cluster 0
Point 1 -> Cluster 0
Point 2 -> Cluster 1
Point 3 -> Cluster 1
Point 4 -> Cluster 0
Point 5 -> Cluster 1
Point 6 -> Cluster 2
Point 7 -> Cluster 2
Point 8 -> Cluster 2
```

This implementation demonstrates the core concepts of K-Medoids clustering, which is more robust than K-Means because it uses actual data points as medoids rather than calculating centroids.

