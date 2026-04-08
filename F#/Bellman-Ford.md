Here is a compact, easy‑to‑read implementation of the **Bellman‑Ford** shortest‑path algorithm in **F#**.  
It works on a directed weighted graph that may contain negative edges (but no negative‑weight cycles reachable from the source).  
The function returns an option of the distance map – `None` signals that a negative cycle was detected.

```fsharp
// ---------- Bellman‑Ford in F# ----------
open System
open System.Collections.Generic

/// Edge of a weighted directed graph
type Edge<'V> = { Source: 'V; Target: 'V; Weight: int }

/// Bellman‑Ford shortest‑path from a source vertex.
///   vertices – collection of all vertices (must support equality)
///   edges    – sequence of directed edges
///   src      – source vertex
/// Returns:   Some distances (Map<'V,int>)  if no negative cycle,
///            None                           otherwise
let bellmanFord<'V when 'V : equality> (vertices: seq<'V>) (edges: seq<Edge<'V>>) (src: 'V) =
    // Initialise distances: 0 for src, +∞ for everybody else
    let INF = System.Int32.MaxValue
    let dist0 =
        vertices
        |> Seq.map (fun v -> if v = src then 0 else INF)
        |> Seq.zip vertices
        |> Map.ofSeq

    // Relax all edges |V|-1 times
    let relaxed =
        vertices
        |> Seq.toList
        |> List.length   // |V|
        |> fun n ->
            [0 .. n-2]   // we need |V|-1 iterations
            |> List.fold (fun d _ ->
                // one relaxation pass over all edges
                edges
                |> Seq.fold (fun acc e ->
                    match Map.tryFind e.Source acc, Map.tryFind e.Target acc with
                    | Some du, Some dv when du <> INF && du + e.Weight < dv ->
                        // improve distance to e.Target
                        Map.add e.Target (du + e.Weight) acc
                    | _ -> acc) acc) dist0

    // One more pass to detect negative‑weight cycles
    let hasNegCycle =
        edges
        |> Seq.exists (fun e ->
            match Map.tryFind e.Source relaxed, Map.tryFind e.Target relaxed with
            | Some du, Some dv when du <> INF && du + e.Weight < dv -> true
            | _ -> false)

    if hasNegCycle then None else Some relaxed
// ---------------------------------------

// Example usage -------------------------------------------------
let vertices = [ 'A'; 'B'; 'C'; 'D' ]
let edges = [
    { Source='A'; Target='B'; Weight= 6 }
    { Source='A'; Target='C'; Weight= 5 }
    { Source='B'; Target='C'; Weight=-2 }
    { Source='B'; Target='D'; Weight= 1 }
    { Source='C'; Target='B'; Weight= 2 }
    { Source='C'; Target='D'; Weight= 6 }
    { Source='D'; Target='B'; Weight=-3 }  // creates a negative cycle B->C->D->B
]

match bellmanFord vertices edges 'A' with
| Some dist ->
    printfn "Shortest distances from 'A':"
    for (v,d) in dist do
        printfn "  %c -> %d" v d
| None ->
    printfn "Negative-weight cycle detected!"
// --------------------------------------------------------------
```

### How it works
1. **Initialization** – distance to the source is `0`; all others are set to `INT_MAX` (representing ∞).  
2. **Relaxation** – we run `|V|-1` passes over all edges, each time trying to improve the distance to the target vertex (`du + w < dv`).  
3. **Negative‑cycle check** – one extra pass; if any edge can still be relaxed, a reachable negative‑weight cycle exists.  
4. The result is wrapped in an `Option<Map<'V,int>>` to make the presence of a negative cycle explicit.

Feel free to copy the snippet into an F# script (`.fsx`) or a project and adapt the vertex/edge types to your needs (e.g., using strings or custom identifiers). Happy coding!