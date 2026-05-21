# Fibonacci Heap Implementation in Pascal

```pascal
program FibonacciHeapExample;

type
  // Node structure for Fibonacci heap
  FiboNode = record
    key: integer;
    degree: integer;
    marked: boolean;
    child: ^FiboNode;
    parent: ^FiboNode;
    left: ^FiboNode;
    right: ^FiboNode;
  end;

  // Fibonacci heap structure
  FibonacciHeap = record
    min: ^FiboNode;
    n: integer;
  end;

// Function to create a new node
function CreateNode(key: integer): ^FiboNode;
begin
  new(CreateNode);
  CreateNode^.key := key;
  CreateNode^.degree := 0;
  CreateNode^.marked := false;
  CreateNode^.child := nil;
  CreateNode^.parent := nil;
  CreateNode^.left := CreateNode;
  CreateNode^.right := CreateNode;
end;

// Function to initialize empty Fibonacci heap
function MakeFibHeap: FibonacciHeap;
begin
  MakeFibHeap.min := nil;
  MakeFibHeap.n := 0;
end;

// Function to insert a node into Fibonacci heap
procedure FibHeapInsert(var H: FibonacciHeap; node: ^FiboNode);
begin
  if H.min = nil then
  begin
    H.min := node;
    node^.left := node;
    node^.right := node;
  end
  else
  begin
    // Insert node into root list
    node^.right := H.min^.right;
    node^.left := H.min;
    H.min^.right^.left := node;
    H.min^.right := node;
    
    // Update minimum if necessary
    if node^.key < H.min^.key then
      H.min := node;
  end;
  H.n := H.n + 1;
end;

// Function to find minimum node
function FibHeapMinimum(var H: FibonacciHeap): ^FiboNode;
begin
  FibHeapMinimum := H.min;
end;

// Function to extract minimum node
function FibHeapExtractMin(var H: FibonacciHeap): ^FiboNode;
var
  z, x, y: ^FiboNode;
  temp: ^FiboNode;
begin
  z := H.min;
  if z <> nil then
  begin
    // Add all children to root list
    x := z^.child;
    if x <> nil then
    begin
      repeat
        y := x;
        x := x^.right;
        // Remove y from child list
        y^.parent := nil;
        // Add y to root list
        y^.right := H.min^.right;
        y^.left := H.min;
        H.min^.right^.left := y;
        H.min^.right := y;
        y := y^.right;
      until y = z^.child;
    end;
    
    // Remove z from root list
    z^.left^.right := z^.right;
    z^.right^.left := z^.left;
    
    if z = z^.right then
      H.min := nil
    else
    begin
      H.min := z^.right;
      // Consolidate the heap
      FibHeapConsolidate(H);
    end;
    
    H.n := H.n - 1;
    FibHeapExtractMin := z;
  end
  else
    FibHeapExtractMin := nil;
end;

// Function to consolidate the heap
procedure FibHeapConsolidate(var H: FibonacciHeap);
var
  A: array[0..100] of ^FiboNode;
  i, d: integer;
  x, y, temp: ^FiboNode;
begin
  // Initialize auxiliary array
  for i := 0 to 100 do
    A[i] := nil;
  
  // Process all nodes in root list
  x := H.min;
  if x <> nil then
  begin
    repeat
      d := x^.degree;
      while A[d] <> nil do
      begin
        y := A[d];
        // Make sure x has smaller key
        if x^.key > y^.key then
        begin
          temp := x;
          x := y;
          y := temp;
        end;
        // Link y as child of x
        FibHeapLink(H, y, x);
        A[d] := nil;
        d := d + 1;
      end;
      A[d] := x;
      x := x^.right;
    until x = H.min;
    
    // Update min pointer
    H.min := nil;
    for i := 0 to 100 do
    begin
      if A[i] <> nil then
      begin
        if H.min = nil then
        begin
          H.min := A[i];
          A[i]^.left := A[i];
          A[i]^.right := A[i];
        end
        else
        begin
          A[i]^.right := H.min^.right;
          A[i]^.left := H.min;
          H.min^.right^.left := A[i];
          H.min^.right := A[i];
          if A[i]^.key < H.min^.key then
            H.min := A[i];
        end;
      end;
    end;
  end;
end;

// Function to link two nodes
procedure FibHeapLink(var H: FibonacciHeap; var y, x: ^FiboNode);
begin
  // Remove y from root list
  y^.left^.right := y^.right;
  y^.right^.left := y^.left;
  
  // Make y child of x
  if x^.child = nil then
  begin
    x^.child := y;
    y^.left := y;
    y^.right := y;
  end
  else
  begin
    y^.right := x^.child^.right;
    y^.left := x^.child;
    x^.child^.right^.left := y;
    x^.child^.right := y;
  end;
  
  y^.parent := x;
  y^.marked := false;
  x^.degree := x^.degree + 1;
end;

// Example usage
procedure TestFibonacciHeap;
var
  heap: FibonacciHeap;
  node1, node2, node3, node4, node5: ^FiboNode;
  minNode: ^FiboNode;
begin
  writeln('Fibonacci Heap Example');
  writeln('====================');
  
  // Initialize heap
  heap := MakeFibHeap;
  
  // Create nodes
  node1 := CreateNode(10);
  node2 := CreateNode(20);
  node3 := CreateNode(5);
  node4 := CreateNode(15);
  node5 := CreateNode(25);
  
  // Insert nodes
  FibHeapInsert(heap, node1);
  FibHeapInsert(heap, node2);
  FibHeapInsert(heap, node3);
  FibHeapInsert(heap, node4);
  FibHeapInsert(heap, node5);
  
  writeln('Heap created with 5 nodes');
  
  // Extract minimum
  minNode := FibHeapExtractMin(heap);
  if minNode <> nil then
    writeln('Extracted minimum: ', minNode^.key);
  
  // Extract minimum again
  minNode := FibHeapExtractMin(heap);
  if minNode <> nil then
    writeln('Extracted minimum: ', minNode^.key);
  
  writeln('Heap size: ', heap.n);
end;

begin
  TestFibonacciHeap;
end.
```

## Key Features of this Fibonacci Heap Implementation:

1. **Node Structure**: Each node contains key, degree, marked flag, and pointers to parent, child, left, and right siblings
2. **Basic Operations**:
   - `MakeFibHeap`: Initialize empty heap
   - `FibHeapInsert`: Insert a new node
   - `FibHeapMinimum`: Find minimum node
   - `FibHeapExtractMin`: Extract minimum node
   - `FibHeapConsolidate`: Consolidate heap after extraction

3. **Time Complexity**:
   - Insert: O(1) amortized
   - ExtractMin: O(log n) amortized
   - FindMin: O(1)

4. **Advantages**:
   - Efficient for operations that require frequent insertions and deletions
   - Better amortized time complexity compared to binary heaps for certain operations

This implementation demonstrates the core concepts of Fibonacci heaps while maintaining Pascal syntax and structure.

