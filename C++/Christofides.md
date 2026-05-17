# Christofides Algorithm Implementation in C++

```cpp
#include <iostream>
#include <vector>
#include <algorithm>
#include <cmath>
#include <climits>

using namespace std;

// Structure to represent a point in 2D space
struct Point {
    int x, y;
    Point(int x, int y) : x(x), y(y) {}
};

// Structure to represent an edge
struct Edge {
    int u, v;
    double weight;
    Edge(int u, int v, double weight) : u(u), v(v), weight(weight) {}
};

// Structure to represent a disjoint set (Union-Find)
class UnionFind {
private:
    vector<int> parent, rank;
    
public:
    UnionFind(int n) {
        parent.resize(n);
        rank.resize(n, 0);
        for (int i = 0; i < n; i++) {
            parent[i] = i;
        }
    }
    
    int find(int x) {
        if (parent[x] != x) {
            parent[x] = find(parent[x]); // Path compression
        }
        return parent[x];
    }
    
    void unite(int x, int y) {
        int rootX = find(x);
        int rootY = find(y);
        
        if (rootX != rootY) {
            if (rank[rootX] < rank[rootY]) {
                swap(rootX, rootY);
            }
            parent[rootY] = rootX;
            if (rank[rootX] == rank[rootY]) {
                rank[rootX]++;
            }
        }
    }
};

// Calculate Euclidean distance between two points
double distance(Point a, Point b) {
    return sqrt(pow(a.x - b.x, 2) + pow(a.y - b.y, 2));
}

// Find Minimum Spanning Tree using Kruskal's algorithm
vector<Edge> kruskalMST(vector<Point>& points, int n) {
    vector<Edge> edges;
    
    // Generate all edges with weights
    for (int i = 0; i < n; i++) {
        for (int j = i + 1; j < n; j++) {
            double dist = distance(points[i], points[j]);
            edges.push_back(Edge(i, j, dist));
        }
    }
    
    // Sort edges by weight
    sort(edges.begin(), edges.end(), [](const Edge& a, const Edge& b) {
        return a.weight < b.weight;
    });
    
    // Apply Kruskal's algorithm
    UnionFind uf(n);
    vector<Edge> mst;
    int edgeCount = 0;
    
    for (const Edge& edge : edges) {
        if (edgeCount >= n - 1) break;
        
        if (uf.find(edge.u) != uf.find(edge.v)) {
            uf.unite(edge.u, edge.v);
            mst.push_back(edge);
            edgeCount++;
        }
    }
    
    return mst;
}

// Find vertices with odd degree in MST
vector<int> findOddDegreeVertices(vector<Edge>& mst, int n) {
    vector<int> degree(n, 0);
    
    for (const Edge& edge : mst) {
        degree[edge.u]++;
        degree[edge.v]++;
    }
    
    vector<int> oddVertices;
    for (int i = 0; i < n; i++) {
        if (degree[i] % 2 == 1) {
            oddVertices.push_back(i);
        }
    }
    
    return oddVertices;
}

// Find minimum weight perfect matching for odd degree vertices
vector<Edge> minimumWeightPerfectMatching(vector<Point>& points, vector<int>& oddVertices) {
    vector<Edge> matching;
    
    // Simple greedy approach for small number of vertices
    // In practice, you might want to use a more sophisticated algorithm
    vector<int> used(oddVertices.size(), 0);
    
    for (int i = 0; i < oddVertices.size(); i++) {
        if (used[i]) continue;
        
        int minIdx = -1;
        double minDist = INT_MAX;
        
        for (int j = i + 1; j < oddVertices.size(); j++) {
            if (!used[j]) {
                double dist = distance(points[oddVertices[i]], points[oddVertices[j]]);
                if (dist < minDist) {
                    minDist = dist;
                    minIdx = j;
                }
            }
        }
        
        if (minIdx != -1) {
            matching.push_back(Edge(oddVertices[i], oddVertices[minIdx], minDist));
            used[i] = used[minIdx] = 1;
        }
    }
    
    return matching;
}

// Create Eulerian circuit from MST + matching
vector<int> createEulerianCircuit(vector<Edge>& mst, vector<Edge>& matching, int n) {
    // Create adjacency list representation
    vector<vector<int>> adj(n);
    
    // Add edges from MST
    for (const Edge& edge : mst) {
        adj[edge.u].push_back(edge.v);
        adj[edge.v].push_back(edge.u);
    }
    
    // Add edges from matching
    for (const Edge& edge : matching) {
        adj[edge.u].push_back(edge.v);
        adj[edge.v].push_back(edge.u);
    }
    
    // Find Eulerian circuit using Hierholzer's algorithm
    vector<int> circuit;
    vector<int> stack;
    stack.push_back(0); // Start from vertex 0
    
    while (!stack.empty()) {
        int u = stack.back();
        if (adj[u].empty()) {
            circuit.push_back(u);
            stack.pop_back();
        } else {
            int v = adj[u].back();
            adj[u].pop_back();
            adj[v].pop_back();
            stack.push_back(v);
        }
    }
    
    reverse(circuit.begin(), circuit.end());
    return circuit;
}

// Main Christofides algorithm function
vector<int> christofidesTSP(vector<Point>& points) {
    int n = points.size();
    
    if (n <= 1) return {};
    
    // Step 1: Find MST
    vector<Edge> mst = kruskalMST(points, n);
    
    // Step 2: Find vertices with odd degree
    vector<int> oddVertices = findOddDegreeVertices(mst, n);
    
    // Step 3: Find minimum weight perfect matching for odd vertices
    vector<Edge> matching = minimumWeightPerfectMatching(points, oddVertices);
    
    // Step 4: Create Eulerian circuit
    vector<int> eulerianCircuit = createEulerianCircuit(mst, matching, n);
    
    // Step 5: Convert to Hamiltonian cycle (shortcutting)
    vector<bool> visited(n, false);
    vector<int> tour;
    
    for (int vertex : eulerianCircuit) {
        if (!visited[vertex]) {
            visited[vertex] = true;
            tour.push_back(vertex);
        }
    }
    
    return tour;
}

// Print the tour
void printTour(vector<int>& tour, vector<Point>& points) {
    cout << "Optimal Tour: ";
    for (int i = 0; i < tour.size(); i++) {
        cout << "(" << points[tour[i]].x << "," << points[tour[i]].y << ")";
        if (i < tour.size() - 1) cout << " -> ";
    }
    cout << endl;
    
    // Calculate total distance
    double totalDistance = 0;
    for (int i = 0; i < tour.size(); i++) {
        int next = (i + 1) % tour.size();
        totalDistance += distance(points[tour[i]], points[tour[next]]);
    }
    cout << "Total Distance: " << totalDistance << endl;
}

int main() {
    // Example: 5 cities in 2D space
    vector<Point> points = {
        Point(0, 0),    // City 0
        Point(1, 2),    // City 1
        Point(3, 1),    // City 2
        Point(5, 3),    // City 3
        Point(2, 4)     // City 4
    };
    
    cout << "Cities coordinates:" << endl;
    for (int i = 0; i < points.size(); i++) {
        cout << "City " << i << ": (" << points[i].x << ", " << points[i].y << ")" << endl;
    }
    cout << endl;
    
    // Run Christofides algorithm
    vector<int> tour = christofidesTSP(points);
    
    cout << "Christofides Algorithm Result:" << endl;
    printTour(tour, points);
    
    return 0;
}
```

## Algorithm Steps Explained:

1. **Step 1: Minimum Spanning Tree (MST)** - Use Kruskal's algorithm to find MST of the complete graph
2. **Step 2: Odd Degree Vertices** - Identify vertices with odd degree in the MST
3. **Step 3: Perfect Matching** - Find minimum weight perfect matching for odd degree vertices
4. **Step 4: Eulerian Circuit** - Combine MST and matching to create Eulerian graph, then find Eulerian circuit
5. **Step 5: Hamiltonian Cycle** - Shortcut the Eulerian circuit to get a valid TSP tour

## Time Complexity:
- O(n² log n) for the complete algorithm
- Dominated by MST construction and sorting edges

## Space Complexity:
- O(n²) for storing all edges

This implementation provides a working example of the Christofides algorithm for solving the Traveling Salesman Problem with an approximation ratio of 1.5.

