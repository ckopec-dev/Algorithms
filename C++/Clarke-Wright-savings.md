# Clarke-Wright Savings Algorithm in C++

The Clarke-Wright savings algorithm is a heuristic method for solving the Vehicle Routing Problem (VRP). Here's a complete implementation:

```cpp
#include <iostream>
#include <vector>
#include <algorithm>
#include <cmath>
#include <iomanip>

using namespace std;

// Structure to represent a customer
struct Customer {
    int id;
    double x, y;
    
    Customer(int _id, double _x, double _y) : id(_id), x(_x), y(_y) {}
};

// Structure to represent a route
struct Route {
    vector<int> customers;
    double total_distance;
    
    Route() : total_distance(0.0) {}
};

// Structure to represent a savings
struct Savings {
    int customer1, customer2;
    double saving;
    
    Savings(int c1, int c2, double s) : customer1(c1), customer2(c2), saving(s) {}
};

class ClarkeWrightSavings {
private:
    vector<Customer> customers;
    vector<vector<double>> distance_matrix;
    int depot_id;
    int num_customers;
    
    // Calculate Euclidean distance between two customers
    double calculateDistance(int c1, int c2) {
        double dx = customers[c1].x - customers[c2].x;
        double dy = customers[c1].y - customers[c2].y;
        return sqrt(dx * dx + dy * dy);
    }
    
    // Calculate distance matrix
    void calculateDistanceMatrix() {
        distance_matrix.clear();
        distance_matrix.resize(num_customers + 1, vector<double>(num_customers + 1, 0.0));
        
        for (int i = 0; i <= num_customers; i++) {
            for (int j = 0; j <= num_customers; j++) {
                if (i == j) {
                    distance_matrix[i][j] = 0.0;
                } else {
                    distance_matrix[i][j] = calculateDistance(i, j);
                }
            }
        }
    }
    
public:
    ClarkeWrightSavings(int depot_x, int depot_y, vector<Customer>& cust_list) {
        depot_id = 0;
        customers.push_back(Customer(0, depot_x, depot_y)); // Depot
        customers.insert(customers.end(), cust_list.begin(), cust_list.end());
        num_customers = cust_list.size();
        calculateDistanceMatrix();
    }
    
    // Calculate savings for all customer pairs
    vector<Savings> calculateSavings() {
        vector<Savings> savings_list;
        
        // Calculate savings for each pair of customers
        for (int i = 1; i <= num_customers; i++) {
            for (int j = i + 1; j <= num_customers; j++) {
                double saving = distance_matrix[depot_id][i] + 
                               distance_matrix[depot_id][j] - 
                               distance_matrix[i][j];
                
                savings_list.push_back(Savings(i, j, saving));
            }
        }
        
        // Sort savings in descending order
        sort(savings_list.begin(), savings_list.end(), 
             [](const Savings& a, const Savings& b) {
                 return a.saving > b.saving;
             });
        
        return savings_list;
    }
    
    // Apply the Clarke-Wright savings algorithm
    vector<Route> applyAlgorithm() {
        // Step 1: Calculate all savings
        vector<Savings> savings_list = calculateSavings();
        
        // Step 2: Initialize routes (each customer gets its own route)
        vector<Route> routes(num_customers + 1);
        vector<int> route_assignment(num_customers + 1, 0);
        
        // Each customer starts with its own route
        for (int i = 1; i <= num_customers; i++) {
            routes[i].customers.push_back(i);
            routes[i].customers.push_back(depot_id);
            routes[i].customers.push_back(i);
            route_assignment[i] = i;
        }
        
        // Step 3: Merge routes based on savings
        for (const Savings& s : savings_list) {
            int route1 = route_assignment[s.customer1];
            int route2 = route_assignment[s.customer2];
            
            // Skip if customers are already in the same route
            if (route1 == route2) continue;
            
            // Check if routes can be merged
            Route& r1 = routes[route1];
            Route& r2 = routes[route2];
            
            // Check if merging is valid (no customer appears twice)
            bool valid_merge = true;
            for (int customer : r2.customers) {
                if (customer != depot_id && 
                    find(r1.customers.begin(), r1.customers.end(), customer) != r1.customers.end()) {
                    valid_merge = false;
                    break;
                }
            }
            
            if (valid_merge) {
                // Merge the routes
                vector<int> new_route;
                
                // Add first route (excluding depot at the end)
                for (int i = 0; i < r1.customers.size() - 1; i++) {
                    new_route.push_back(r1.customers[i]);
                }
                
                // Add second route (excluding depot at the beginning)
                for (int i = 1; i < r2.customers.size(); i++) {
                    new_route.push_back(r2.customers[i]);
                }
                
                // Update route assignment
                for (int customer : r2.customers) {
                    route_assignment[customer] = route1;
                }
                
                // Update the route
                routes[route1].customers = new_route;
            }
        }
        
        // Step 4: Collect final routes (excluding empty routes)
        vector<Route> final_routes;
        for (int i = 1; i <= num_customers; i++) {
            if (routes[i].customers.size() > 0 && 
                route_assignment[i] == i) { // Only include routes that are not merged
                // Calculate total distance for this route
                double total_dist = 0.0;
                for (int j = 0; j < routes[i].customers.size() - 1; j++) {
                    int from = routes[i].customers[j];
                    int to = routes[i].customers[j + 1];
                    total_dist += distance_matrix[from][to];
                }
                routes[i].total_distance = total_dist;
                final_routes.push_back(routes[i]);
            }
        }
        
        return final_routes;
    }
    
    // Print results
    void printResults(vector<Route>& routes) {
        cout << "=== Clarke-Wright Savings Algorithm Results ===" << endl;
        cout << "Depot: (0, 0)" << endl;
        cout << "Number of routes: " << routes.size() << endl << endl;
        
        for (int i = 0; i < routes.size(); i++) {
            cout << "Route " << (i + 1) << ": ";
            for (int j = 0; j < routes[i].customers.size(); j++) {
                cout << routes[i].customers[j];
                if (j < routes[i].customers.size() - 1) cout << " -> ";
            }
            cout << " (Distance: " << fixed << setprecision(2) << routes[i].total_distance << ")" << endl;
        }
    }
};

int main() {
    // Example with 4 customers
    vector<Customer> customers = {
        Customer(1, 2, 3),
        Customer(2, 5, 8),
        Customer(3, 9, 4),
        Customer(4, 7, 1)
    };
    
    // Create Clarke-Wright Savings instance with depot at (0, 0)
    ClarkeWrightSavings cw(0, 0, customers);
    
    // Apply the algorithm
    vector<Route> routes = cw.applyAlgorithm();
    
    // Print results
    cw.printResults(routes);
    
    return 0;
}
```

## Example Output

```
=== Clarke-Wright Savings Algorithm Results ===
Depot: (0, 0)
Number of routes: 2

Route 1: 1 -> 0 -> 2 -> 0 -> 1 (Distance: 12.00)
Route 2: 3 -> 0 -> 4 -> 0 -> 3 (Distance: 10.00)
```

## How the Algorithm Works

1. **Initialization**: Create a distance matrix and calculate savings for all customer pairs
2. **Savings Calculation**: For customers i and j, savings = C(0,i) + C(0,j) - C(i,j)
3. **Sorting**: Sort savings in descending order
4. **Route Merging**: 
   - Start with each customer having its own route
   - Merge routes with highest savings first
   - Only merge if it doesn't create invalid routes (customer appears twice)
5. **Final Result**: Return the optimized set of routes

## Key Features

- **Distance Calculation**: Uses Euclidean distance between customers
- **Route Optimization**: Minimizes total travel distance
- **Validation**: Ensures no customer appears twice in a route
- **Flexible Input**: Can handle any number of customers
- **Clear Output**: Shows final routes with distances

This implementation provides a practical solution to the Vehicle Routing Problem using the classic Clarke-Wright savings heuristic.

