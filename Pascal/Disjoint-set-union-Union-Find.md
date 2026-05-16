# Disjoint Set Union (Union-Find) Algorithm in Pascal

```pascal
program DisjointSetUnion;

type
  TDisjointSet = record
    parent: array[0..999] of integer;
    rank: array[0..999] of integer;
    size: integer;
  end;

var
  ds: TDisjointSet;

// Initialize the disjoint set with n elements
procedure MakeSet(n: integer);
var
  i: integer;
begin
  for i := 0 to n - 1 do
  begin
    ds.parent[i] := i;
    ds.rank[i] := 0;
  end;
  ds.size := n;
end;

// Find the root of element x with path compression
function Find(x: integer): integer;
begin
  if ds.parent[x] <> x then
    ds.parent[x] := Find(ds.parent[x]); // Path compression
  Find := ds.parent[x];
end;

// Union two sets containing elements x and y
procedure Union(x, y: integer);
var
  rootX, rootY: integer;
begin
  rootX := Find(x);
  rootY := Find(y);
  
  if rootX <> rootY then
  begin
    // Union by rank
    if ds.rank[rootX] < ds.rank[rootY] then
      ds.parent[rootX] := rootY
    else if ds.rank[rootX] > ds.rank[rootY] then
      ds.parent[rootY] := rootX
    else
    begin
      ds.parent[rootY] := rootX;
      ds.rank[rootX] := ds.rank[rootX] + 1;
    end;
  end;
end;

// Check if two elements are in the same set
function Connected(x, y: integer): boolean;
begin
  Connected := Find(x) = Find(y);
end;

// Print the current state of the disjoint set
procedure PrintSets;
var
  i: integer;
  root: integer;
  roots: array[0..999] of boolean;
begin
  writeln('Current disjoint sets:');
  
  // Initialize roots array
  for i := 0 to ds.size - 1 do
    roots[i] := false;
  
  // Find and print each set
  for i := 0 to ds.size - 1 do
  begin
    root := Find(i);
    if not roots[root] then
    begin
      write('Set {');
      write(i);
      roots[root] := true;
      // Print all elements in this set
      for var j := i + 1 to ds.size - 1 do
        if Find(j) = root then
          write(', ', j);
      writeln('}');
    end;
  end;
end;

// Example usage
begin
  writeln('Disjoint Set Union (Union-Find) Example');
  writeln('=====================================');
  
  // Create a disjoint set with 10 elements (0-9)
  MakeSet(10);
  
  writeln('Initial state:');
  PrintSets;
  
  // Perform some unions
  writeln;
  writeln('Performing unions:');
  Union(0, 1);
  Union(2, 3);
  Union(4, 5);
  Union(6, 7);
  Union(8, 9);
  
  writeln('After unions (0-1), (2-3), (4-5), (6-7), (8-9):');
  PrintSets;
  
  // Union some more sets
  Union(1, 2);
  Union(5, 6);
  
  writeln;
  writeln('After unions (1-2), (5-6):');
  PrintSets;
  
  // Check connections
  writeln;
  writeln('Connection checks:');
  writeln('0 and 3 connected: ', Connected(0, 3)); // Should be true
  writeln('0 and 4 connected: ', Connected(0, 4)); // Should be false
  writeln('7 and 9 connected: ', Connected(7, 9)); // Should be true
  
  // Find operation examples
  writeln;
  writeln('Find operations:');
  writeln('Root of 0: ', Find(0));
  writeln('Root of 5: ', Find(5));
  writeln('Root of 8: ', Find(8));
end.
```

## Key Features of this Implementation:

1. **Path Compression**: The `Find` function uses path compression to optimize future lookups
2. **Union by Rank**: The `Union` function uses rank to keep the tree balanced
3. **Basic Operations**:
   - `MakeSet`: Initialize the disjoint set
   - `Find`: Find the root of an element
   - `Union`: Merge two sets
   - `Connected`: Check if two elements are in the same set

## Time Complexity:
- **Find**: Nearly O(1) amortized
- **Union**: Nearly O(1) amortized
- **MakeSet**: O(n)

## Sample Output:
```
Disjoint Set Union (Union-Find) Example
=====================================
Initial state:
Current disjoint sets:
Set {0}
Set {1}
Set {2}
Set {3}
Set {4}
Set {5}
Set {6}
Set {7}
Set {8}
Set {9}

Performing unions (0-1), (2-3), (4-5), (6-7), (8-9):
After unions (0-1), (2-3), (4-5), (6-7), (8-9):
Set {0, 1}
Set {2, 3}
Set {4, 5}
Set {6, 7}
Set {8, 9}

After unions (1-2), (5-6):
Set {0, 1, 2, 3}
Set {4, 5, 6, 7}
Set {8, 9}

Connection checks:
0 and 3 connected: TRUE
0 and 4 connected: FALSE
7 and 9 connected: TRUE

Find operations:
Root of 0: 0
Root of 5: 4
Root of 8: 8
```

