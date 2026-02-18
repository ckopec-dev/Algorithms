# Union-Find Algorithm in Pascal

Here's an implementation of the Union-Find (Disjoint Set Union) algorithm in Pascal with path compression and union by rank optimizations:

```pascal
program UnionFindExample;

type
  TUnionFind = record
    parent: array[0..1000] of integer;
    rank: array[0..1000] of integer;
    size: integer;
  end;

var
  uf: TUnionFind;

// Initialize the Union-Find structure
procedure MakeSet(n: integer);
var
  i: integer;
begin
  for i := 0 to n - 1 do
  begin
    uf.parent[i] := i;
    uf.rank[i] := 0;
  end;
  uf.size := n;
end;

// Find with path compression
function Find(x: integer): integer;
begin
  if uf.parent[x] <> x then
    uf.parent[x] := Find(uf.parent[x]);  // Path compression
  Find := uf.parent[x];
end;

// Union with union by rank
procedure Union(x, y: integer);
var
  rootX, rootY: integer;
begin
  rootX := Find(x);
  rootY := Find(y);
  
  if rootX <> rootY then
  begin
    // Union by rank
    if uf.rank[rootX] < uf.rank[rootY] then
      uf.parent[rootX] := rootY
    else if uf.rank[rootX] > uf.rank[rootY] then
      uf.parent[rootY] := rootX
    else
    begin
      uf.parent[rootY] := rootX;
      uf.rank[rootX] := uf.rank[rootX] + 1;
    end;
  end;
end;

// Check if two elements are in the same set
function Connected(x, y: integer): boolean;
begin
  Connected := Find(x) = Find(y);
end;

// Main program to demonstrate the Union-Find algorithm
procedure DemonstrateUnionFind;
var
  i: integer;
begin
  writeln('Union-Find Algorithm Demonstration');
  writeln('==================================');
  
  // Initialize with 10 elements (0-9)
  MakeSet(10);
  
  writeln('Initial state - All elements in separate sets:');
  for i := 0 to 9 do
    writeln('Element ', i, ' belongs to set ', Find(i));
  
  writeln();
  writeln('Performing unions:');
  
  // Perform some union operations
  Union(0, 1);
  Union(2, 3);
  Union(4, 5);
  Union(6, 7);
  Union(0, 2);
  Union(4, 6);
  
  writeln('After unions: (0,1), (2,3), (4,5), (6,7), (0,2), (4,6)');
  
  writeln();
  writeln('Final state:');
  for i := 0 to 9 do
    writeln('Element ', i, ' belongs to set ', Find(i));
  
  writeln();
  writeln('Checking connections:');
  writeln('Are 0 and 3 connected? ', Connected(0, 3));
  writeln('Are 1 and 3 connected? ', Connected(1, 3));
  writeln('Are 4 and 7 connected? ', Connected(4, 7));
  writeln('Are 0 and 5 connected? ', Connected(0, 5));
end;

begin
  DemonstrateUnionFind;
end.
```

## Key Features of this Implementation:

### 1. **Data Structure**
- `parent`: Array storing the parent of each element
- `rank`: Array storing the rank (depth) of each tree
- `size`: Total number of elements

### 2. **Operations**
- **MakeSet**: Initialize each element as its own parent
- **Find**: Find the root of an element with path compression
- **Union**: Merge two sets with union by rank optimization
- **Connected**: Check if two elements belong to the same set

### 3. **Optimizations**
- **Path Compression**: Makes future lookups faster by flattening the tree
- **Union by Rank**: Always attach the smaller tree under the root of the larger tree

### 4. **Time Complexity**
- Find operation: Nearly O(1) amortized
- Union operation: Nearly O(1) amortized
- Space complexity: O(n)

This implementation demonstrates how Union-Find can efficiently manage disjoint sets and is commonly used in algorithms like Kruskal's minimum spanning tree and connectivity problems.

