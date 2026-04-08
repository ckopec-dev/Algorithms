Below is a **self‑contained** implementation of a **Fibonacci heap** in C++17.  
It shows the most common operations:

* `makeHeap()` – create an empty heap  
* `insert(key)` – insert a node with a given key (returns a handle)  
* `getMin()` – return the minimum key (or `nullptr` if empty)  
* `extractMin()` – remove and return the minimum key  
* `decreaseKey(handle, newKey)` – decrease the key of a node  
* `remove(handle)` – delete an arbitrary node (via `decreaseKey` + `extractMin`)  

The code is deliberately kept short but still follows the classic lazy‑consolidation
strategy of Fibonacci heaps, giving **amortized O(1)** for insert, decrease‑key,
and merge, and **O(log n)** for `extractMin`/`delete`.

```cpp
// FibonacciHeap.cpp
// Compile with: g++ -std=c++17 -O2 -Wall FibonacciHeap.cpp -o FibonacciHeap
#include <iostream>
#include <vector>
#include <cmath>
#include <cassert>
#include <limits>

/* ---------- Node definition ---------- */
struct FibNode {
    int key;                     // stored key
    FibNode *parent = nullptr;   // parent pointer
    FibNode *child = nullptr;    // any child
    FibNode *left = this;        // circular doubly linked list left
    FibNode *right = this;       // circular doubly linked list right
    int degree = 0;              // number of children
    bool mark = false;           // has lost a child since becoming a child
};

/* ---------- Helper functions ---------- */

/* Link y as a child of x (assuming x.key <= y.key) */
static void link(FibNode* y, FibNode* x) {
    // remove y from root list
    y->left->right = y->right;
    y->right->left = y->left;

    // make y a child of x
    y->parent = x;
    if (x->child == nullptr) {
        x->child = y;
        y->left = y->right = y;
    } else {
        // insert y into x's child list
        y->right = x->child;
        y->left = x->child->left;
        x->child->left->right = y;
        x->child->left = y;
    }
    x->degree++;
    y->mark = false;
}

/* Consolidate the root list: combine trees of equal degree */
static void consolidate(FibNode*& min) {
    if (min == nullptr) return;

    // Upper bound on degree: log_phi(n) ~ log2(n) * 1.44
    int maxDegree = static_cast<int>(std::log2(std::numeric_limits<int>::max())) + 5;
    std::vector<FibNode*> A(maxDegree, nullptr);

    // collect all roots into a vector to avoid iterator invalidation
    std::vector<FibNode*> roots;
    FibNode* start = min;
    do {
        roots.push_back(start);
        start = start->right;
    } while (start != min);

    for (FibNode* w : roots) {
        int d = w->degree;
        while (A[d] != nullptr) {
            FibNode* y = A[d];
            // ensure w is the smaller root
            if (w->key > y->key) std::swap(w, y);
            link(y, w);
            A[d] = nullptr;
            ++d;
        }
        A[d] = w;
    }

    // rebuild root list and find new min
    min = nullptr;
    for (FibNode* node : A) {
        if (node == nullptr) continue;
        // insert node into root list
        if (min == nullptr) {
            min = node;
            node->left = node->right = node;
        } else {
            node->right = min->right;
            node->left = min;
            min->right->left = node;
            min->right = node;
            if (node->key < min->key) min = node;
        }
    }
}

/* ---------- Public FibonacciHeap class ---------- */
class FibonacciHeap {
public:
    FibonacciHeap() : minNode(nullptr), n(0) {}
    ~FibonacciHeap() { clear(minNode); }

    // Insert a new key and return a handle (pointer) to the node.
    FibNode* insert(int key) {
        FibNode* x = new FibNode{key};
        if (minNode == nullptr) {
            minNode = x;
            x->left = x->right = x;
        } else {
            // insert x into root list
            x->right = minNode->right;
            x->left = minNode;
            minNode->right->left = x;
            minNode->right = x;
            if (x->key < minNode->key) minNode = x;
        }
        ++n;
        return x;
    }

    // Return pointer to the node with minimum key (nullptr if empty).
    FibNode* getMin() const { return minNode; }

    // Extract and remove the minimum node; return its key.
    int extractMin() {
        if (minNode == nullptr)
            throw std::runtime_error("extractMin from empty heap");

        FibNode* z = minNode;
        int minKey = z->key;

        // move each child of z to the root list
        if (z->child != nullptr) {
            FibNode* child = z->child;
            do {
                FibNode* next = child->right;
                child->parent = nullptr;
                // insert child into root list
                child->right = minNode->right;
                child->left = minNode;
                minNode->right->left = child;
                minNode->right = child;
                child = next;
            } while (child != z->child);
        }

        // remove z from root list
        z->left->right = z->right;
        z->right->left = z->left;
        if (z == z->right)               // heap had only one node
            minNode = nullptr;
        else {
            minNode = z->right;          // temporary min
            consolidate(minNode);
        }
        delete z;
        --n;
        return minKey;
    }

    // Decrease the key of node x to newKey (must be <= current key).
    void decreaseKey(FibNode* x, int newKey) {
        if (newKey > x->key)
            throw std::invalid_argument("new key is greater than current key");
        x->key = newKey;
        FibNode* y = x->parent;
        if (y != nullptr && x->key < y->key) {
            cut(x, y);
            cascadingCut(y);
        }
        if (x->key < minNode->key) minNode = x;
    }

    // Delete arbitrary node x.
    void remove(FibNode* x) {
        decreaseKey(x, std::numeric_limits<int>::lowest());
        extractMin();
    }

    // Return number of elements.
    size_t size() const { return n; }

private:
    FibNode* minNode;   // pointer to minimum node (or nullptr)
    size_t n;           // number of nodes

    /* ----- internal helpers ----- */
    // Remove x from its parent's child list and add it to the root list.
    void cut(FibNode* x, FibNode* y) {
        // detach x from y's child list
        if (x->right == x)          // only child
            y->child = nullptr;
        else {
            x->left->right = x->right;
            x->right->left = x->left;
            if (y->child == x) y->child = x->right;
        }
        y->degree--;

        // add x to root list
        x->parent = nullptr;
        x->mark = false;
        x->right = minNode->right;
        x->left = minNode;
        minNode->right->left = x;
        minNode->right = x;
    }

    // Perform cascading cut on y.
    void cascadingCut(FibNode* y) {
        FibNode* z = y->parent;
        if (z != nullptr) {
            if (!y->mark) {
                y->mark = true;
            } else {
                cut(y, z);
                cascadingCut(z);
            }
        }
    }

    // Recursively delete all nodes (used in destructor).
    void clear(FibNode* start) {
        if (start == nullptr) return;
        // traverse all nodes via root list and children
        std::vector<FibNode*> todo;
        FibNode* w = start;
        do {
            todo.push_back(w);
            w = w->right;
        } while (w != start);

        for (FibNode* node : todo) {
            if (node->child) clear(node->child);
            delete node;
        }
        minNode = nullptr;
        n = 0;
    }
};

/* ---------- Demo / simple test ---------- */
int main() {
    FibonacciHeap heap;
    auto n1 = heap.insert(10);
    auto n2 = heap.insert(2);
    auto n3 = heap.insert(15);
    auto n4 = heap.insert(6);
    auto n5 = heap.insert(1);

    std::cout << "Min: " << heap.getMin()->key << "\n"; // 1

    heap.decreaseKey(n3, 0); // decrease 15 -> 0
    std::cout << "After decreaseKey(15->0), Min: " << heap.getMin()->key << "\n"; // 0

    std::cout << "Extract mins in order: ";
    while (!heap.empty()) {
        std::cout << heap.extractMin() << ' ';
    }
    std::cout << "\n";

    return 0;
}

/* Helper to check empty (not a member for brevity) */
inline bool FibonacciHeap::empty() const { return minNode == nullptr; }
```

### How the code works

| Part | Purpose |
|------|---------|
| `FibNode` | Stores key, parent, child, and pointers for the circular doubly‑linked root list; also tracks `degree` and `mark` for cascading cuts. |
| `link(y,x)` | Makes node `y` a child of `x` (assuming `x.key ≤ y.key`). Updates degree and removes `y` from the root list. |
| `consolidate(min)` | Scans the root list, linking trees of equal degree until each degree appears at most once; then rebuilds the root list and finds the new minimum. |
| `insert` | Creates a node and inserts it into the root list; O(1) amortized. |
| `getMin` | Returns the pointer to the current minimum node; O(1). |
| `extractMin` | Removes the minimum node, adds its children to the root list, then calls `consolidate`; O(log n) amortized. |
| `decreaseKey` | Lowers a node’s key, cuts it from its parent if heap property violated, and possibly performs cascading cuts; O(1) amortized. |
| `remove` | Decreases the key to −∞ then extracts the min; O(log n) amortized. |
| Destructor (`clear`) | Frees all nodes to avoid leaks. |

The `main()` function demonstrates a tiny workload: inserting a few numbers, decreasing a key, and repeatedly extracting the minimum to verify that the heap returns elements in sorted order.

Feel free to extend the class (e.g., add `merge`, `union`, or iterator support) – the core Fibonacci‑heap mechanics are all present above. Happy coding!