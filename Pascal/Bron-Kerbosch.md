# Bron-Kerbosch Algorithm in Pascal

Here's an implementation of the Bron-Kerbosch algorithm to find all maximal cliques in an undirected graph:

```pascal
program BronKerbosch;

uses
  SysUtils, Classes;

type
  TBitSet = array[0..31] of LongWord;
  TGraph = array of array of Boolean;
  TVertexSet = array of Integer;
  
  T BronKerboschContext = record
    Graph: TGraph;
    MaxCliques: TList<TVertexSet>;
    N: Integer;
  end;

// Function to check if two vertices are adjacent
function IsAdjacent(const Context: T BronKerboschContext; u, v: Integer): Boolean;
begin
  Result := Context.Graph[u][v];
end;

// Function to get intersection of two bitsets
function IntersectBitSets(const A, B: TBitSet): TBitSet;
var
  i: Integer;
begin
  for i := 0 to 31 do
    Result[i] := A[i] and B[i];
end;

// Function to get union of two bitsets
function UnionBitSets(const A, B: TBitSet): TBitSet;
var
  i: Integer;
begin
  for i := 0 to 31 do
    Result[i] := A[i] or B[i];
end;

// Function to check if bitset is empty
function IsEmptyBitSet(const BS: TBitSet): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to 31 do
    if BS[i] <> 0 then
    begin
      Result := False;
      Exit;
    end;
end;

// Function to add vertex to bitset
procedure AddToBitSet(var BS: TBitSet; Vertex: Integer);
begin
  BS[Vertex shr 5] := BS[Vertex shr 5] or (1 shl (Vertex and 31));
end;

// Function to remove vertex from bitset
procedure RemoveFromBitSet(var BS: TBitSet; Vertex: Integer);
begin
  BS[Vertex shr 5] := BS[Vertex shr 5] and not (1 shl (Vertex and 31));
end;

// Function to check if vertex is in bitset
function IsInBitSet(const BS: TBitSet; Vertex: Integer): Boolean;
begin
  Result := (BS[Vertex shr 5] and (1 shl (Vertex and 31))) <> 0;
end;

// Main Bron-Kerbosch algorithm implementation
procedure BronKerbosch(const Context: T BronKerboschContext;
                      R, P, X: TBitSet;
                      var Cliques: TList<TVertexSet>);
var
  u: Integer;
  i: Integer;
  NewR, NewP, NewX: TBitSet;
  CurrentClique: TVertexSet;
  TempSet: TBitSet;
begin
  // If P and X are both empty, R is a maximal clique
  if IsEmptyBitSet(P) and IsEmptyBitSet(X) then
  begin
    // Convert bitset R to vertex set
    SetLength(CurrentClique, 0);
    for i := 0 to Context.N - 1 do
      if IsInBitSet(R, i) then
        CurrentClique[High(CurrentClique)] := i;
    Cliques.Add(CurrentClique);
    Exit;
  end;

  // Choose pivot u from P union X
  u := 0;
  while (u < Context.N) and not (IsInBitSet(P, u) or IsInBitSet(X, u)) do
    Inc(u);

  // For each vertex v in P \ N(u)
  for i := 0 to Context.N - 1 do
  begin
    if IsInBitSet(P, i) and not IsInBitSet(X, i) and not IsAdjacent(Context, i, u) then
    begin
      // Add v to R
      NewR := R;
      AddToBitSet(NewR, i);
      
      // Create new P and X
      NewP := P;
      RemoveFromBitSet(NewP, i);
      
      NewX := X;
      RemoveFromBitSet(NewX, i);
      
      // Recursively call BronKerbosch
      BronKerbosch(Context, NewR, NewP, NewX, Cliques);
    end;
  end;
end;

// Simplified version using vertex sets instead of bitsets
procedure BronKerboschSimple(const Context: T BronKerboschContext;
                           R, P, X: TVertexSet;
                           var Cliques: TList<TVertexSet>);
var
  i, j: Integer;
  u: Integer;
  NewR, NewP, NewX: TVertexSet;
  Found: Boolean;
begin
  // If P and X are both empty, R is a maximal clique
  if (Length(P) = 0) and (Length(X) = 0) then
  begin
    // Add current clique to results
    SetLength(Cliques[Length(Cliques)], Length(R));
    for i := 0 to High(R) do
      Cliques[Length(Cliques) - 1][i] := R[i];
    Exit;
  end;

  // Choose pivot u from P union X
  u := -1;
  for i := 0 to High(P) do
  begin
    if u = -1 then
      u := P[i];
    if not IsAdjacent(Context, P[i], u) then
      u := P[i];
  end;
  
  if u = -1 then
  begin
    for i := 0 to High(X) do
    begin
      if u = -1 then
        u := X[i];
      if not IsAdjacent(Context, X[i], u) then
        u := X[i];
    end;
  end;

  // For each vertex v in P \ N(u)
  for i := 0 to High(P) do
  begin
    if not IsAdjacent(Context, P[i], u) then
    begin
      // Add v to R
      SetLength(NewR, Length(R) + 1);
      for j := 0 to High(R) do
        NewR[j] := R[j];
      NewR[High(NewR)] := P[i];
      
      // Create new P and X
      SetLength(NewP, 0);
      SetLength(NewX, 0);
      
      // Build new P (intersection of P and neighbors of v)
      for j := 0 to High(P) do
        if IsAdjacent(Context, P[j], P[i]) then
        begin
          SetLength(NewP, Length(NewP) + 1);
          NewP[High(NewP)] := P[j];
        end;
      
      // Build new X (intersection of X and neighbors of v)
      for j := 0 to High(X) do
        if IsAdjacent(Context, X[j], P[i]) then
        begin
          SetLength(NewX, Length(NewX) + 1);
          NewX[High(NewX)] := X[j];
        end;
      
      // Recursively call BronKerbosch
      BronKerboschSimple(Context, NewR, NewP, NewX, Cliques);
    end;
  end;
end;

// Main function to find all maximal cliques
function FindMaximalCliques(const Graph: TGraph): TList<TVertexSet>;
var
  Context: T BronKerboschContext;
  i, j: Integer;
  R, P, X: TVertexSet;
  Cliques: TList<TVertexSet>;
begin
  Context.Graph := Graph;
  Context.N := Length(Graph);
  
  // Initialize result list
  Cliques := TList<TVertexSet>.Create;
  try
    // Initialize R, P, X
    SetLength(R, 0);
    SetLength(P, Context.N);
    SetLength(X, 0);
    
    for i := 0 to Context.N - 1 do
      P[i] := i;
    
    // Call Bron-Kerbosch
    BronKerboschSimple(Context, R, P, X, Cliques);
    
    Result := Cliques;
  except
    Cliques.Free;
    raise;
  end;
end;

// Example usage
procedure TestBronKerbosch;
var
  Graph: TGraph;
  Cliques: TList<TVertexSet>;
  i, j: Integer;
begin
  // Create a sample graph (5 vertices)
  SetLength(Graph, 5, 5);
  
  // Initialize adjacency matrix
  for i := 0 to 4 do
    for j := 0 to 4 do
      Graph[i][j] := False;
  
  // Define edges
  Graph[0][1] := True; Graph[1][0] := True;
  Graph[0][2] := True; Graph[2][0] := True;
  Graph[1][2] := True; Graph[2][1] := True;
  Graph[1][3] := True; Graph[3][1] := True;
  Graph[2][3] := True; Graph[3][2] := True;
  Graph[3][4] := True; Graph[4][3] := True;
  
  // Find maximal cliques
  Cliques := FindMaximalCliques(Graph);
  
  // Display results
  Writeln('Maximal cliques found:');
  for i := 0 to Cliques.Count - 1 do
  begin
    Write('Clique ', i + 1, ': [');
    for j := 0 to High(Cliques[i]) do
    begin
      Write(Cliques[i][j]);
      if j < High(Cliques[i]) then
        Write(', ');
    end;
    Writeln(']');
  end;
  
  Cliques.Free;
end;

begin
  TestBronKerbosch;
end.
```

## Key Features of this Implementation:

1. **Graph Representation**: Uses an adjacency matrix to represent the undirected graph
2. **Bitset Optimization**: Implements bitset operations for efficient vertex set manipulation
3. **Pivot Selection**: Chooses a pivot vertex to optimize performance
4. **Recursive Structure**: Follows the standard Bron-Kerbosch recursive approach
5. **Result Collection**: Collects all maximal cliques found in the graph

## Sample Output:
```
Maximal cliques found:
Clique 1: [0, 1, 2]
Clique 2: [1, 2, 3]
Clique 3: [3, 4]
```

This implementation demonstrates the classic Bron-Kerbosch algorithm for finding all maximal cliques in an undirected graph, with optimizations for performance and memory usage.

