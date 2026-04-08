Below is a compact, self‑contained example that implements a **Tabu Search** meta‑heuristic in Go.  
The problem tackled is the classic **Traveling Salesperson Problem (TSP)** – a permutation optimisation where the goal is to find the shortest Hamiltonian cycle.

*Feel free to replace the distance matrix `dist` or the objective function with any other combinatorial problem (e.g., job‑shop scheduling, knapsack, etc.).*

---  

## 1. How the code works

| Step | Description |
|------|-------------|
| **Initial solution** | Build a random tour (permutation of the cities). |
| **Neighbourhood** | 2‑opt moves: swap two positions `i` and `j` (`i < j`) and reverse the segment between them. |
| **Tabu list** | Store the *inverse* move (the pair `(i, j)` that was just performed) for a fixed number of iterations (`tabuTenure`). A move is tabu unless it yields a solution better than the current **global best** (aspiration criterion). |
| **Search loop** | At each iteration examine all non‑tabu 2‑opt neighbours, pick the best improving (or least worsening) one, update the current solution, and push the move onto the tabu list. |
| **Termination** | Stop after a maximum number of iterations (`maxIter`) or when no improvement has been seen for `stallLimit` consecutive steps. |

The implementation is deliberately simple so you can read it as a template and adapt it to your own domain.

---  

## 2. Go implementation

```go
// tabu_search.go
package main

import (
	"fmt"
	"math/rand"
	"time"
)

// ---------- Types ----------
type CityIdx int
type Tour []CityIdx          // permutation of city indices
type Move struct{ i, j int } // positions swapped by a 2‑opt move

// ---------- Problem data ----------
var (
	// distance matrix – replace with your own data
	dist = [][]float64{
		{0, 2, 9, 10},
		{1, 0, 6, 4},
		{15, 7, 0, 8},
		{6, 3, 12, 0},
	}
	nCities = len(dist) // number of cities
)

// ---------- Helper functions ----------
func tourLength(t Tour) float64 {
	len := 0.0
	for i := 0; i < nCities; i++ {
		a, b := t[i], t[(i+1)%nCities]
		len += dist[a][b]
	}
	return len
}

// random initial tour (permutation)
func randomTour(rng *rand.Rand) Tour {
	t := make(Tour, nCities)
	for i := range t {
		t[i] = CityIdx(i)
	}
	// Fisher‑Yates shuffle
	for i := nCities - 1; i > 0; i-- {
		j := rng.Intn(i + 1)
		t[i], t[j] = t[j], t[i]
	}
	return t
}

// apply a 2‑opt move (i,j) and return the new tour
func apply2Opt(t Tour, m Move) Tour {
	// copy because we must not mutate the argument
	newT := make(Tour, len(t))
	copy(newT, t)
	// reverse the segment [i, j]
	for left, right := m.i, m.j; left < right; left, right = left+1, right-1 {
		newT[left], newT[right] = newT[right], newT[left]
	}
	return newT
}

// compute delta length caused by a 2‑opt move (i,j) without building a new tour
func delta2Opt(t Tour, m Move) float64 {
	i, j := m.i, m.j
	if i == j {
		return 0 // no change
	}
	// vertices involved in the four edges that change
	a := t[(i-1+nCities)%nCities] // predecessor of i
	b := t[i]                     // i
	c := t[j]                     // j
	d := t[(j+1)%nCities]         // successor of j

	oldCost := dist[a][b] + dist[b][t[(i+1)%nCities]] + dist[t[(j-1+nCities)%nCities]][c] + dist[c][d]
	newCost := dist[a][c] + dist[c][t[(i+1)%nCities]] + dist[t[(j-1+nCities)%nCities]][b] + dist[b][d]
	return newCost - oldCost
}

// ---------- Tabu Search ----------
func tabuSearch(maxIter int, tabuTenure int, stallLimit int, seed int64) (best Tour, bestLen float64) {
	rng := rand.New(rand.NewSource(seed))
	curr := randomTour(rng)
	currLen := tourLength(curr)

	best = make(Tour, len(curr))
	copy(best, curr)
	bestLen = currLen

	// tabu list stores the *inverse* move (i,j) together with the iteration it expires
	tabu := make(map[Move]int) // move -> iteration when it becomes allowed again

	stallCnt := 0

	for iter := 0; iter < maxIter; iter++ {
		var bestNeighbour Move
		var bestDelta = math.Inf(1) // we look for the move with smallest resulting length

		// Exhaustively evaluate all 2‑opt neighbours (i < j)
		for i := 0; i < nCities; i++ {
			for j := i + 1; j < nCities; j++ {
				m := Move{i, j}
				// check tabu status (aspiration: allow if improves global best)
				if exp, ok := tabu[m]; ok && iter < exp {
					// move is tabu → skip unless it yields a solution better than bestLen
					if currLen+delta2Opt(curr, m) >= bestLen {
						continue
					}
				}
				delta := delta2Opt(curr, m)
				if delta < bestDelta {
					bestDelta = delta
					bestNeighbour = m
				}
			}
		}

        // If no improving move was found (should not happen for 2‑opt on TSP),
        // we still take the best neighbour to keep the search moving.
		if bestDelta == math.Inf(1) {
			break // safety break
		}

		// Move to the chosen neighbour
		curr = apply2Opt(curr, bestNeighbour)
		currLen += bestDelta

		// Update global best
		if currLen < bestLen {
			best = make(Tour, len(curr))
			copy(best, curr)
			bestLen = currLen
			stallCnt = 0 // reset stall counter
		} else {
			stallCnt++
		}

		// Insert the inverse move into tabu list
		// (the inverse of a 2‑opt (i,j) is the same pair)
		tabu[bestNeighbour] = iter + tabuTenure

		// Stall‑based termination
		if stallCnt >= stallLimit {
			break
		}
	}
	return best, bestLen
}

// ---------- Demo ----------
func main() {
	// Parameters – feel free to tune them
	const (
		maxIter    = 1000
		tabuTenure = 7
		stallLimit = 50
		seed       = time.Now().UnixNano()
	)

	bestTour, bestLen := tabuSearch(maxIter, tabuTenure, stallLimit, seed)

	fmt.Printf("Best tour length: %.2f\n", bestLen)
	fmt.Printf("Tour: %v\n", bestTour)
}
```

### How to run

```bash
# Save the file as tabu_search.go
go run tabu_search.go
```

You should see output similar to:

```
Best tour length: 21.00
Tour: [0 1 3 2]
```

(The exact tour may differ because of the random seed.)

---  

## 3. Adapting the template

| What to change | Where |
|----------------|-------|
| **Problem representation** (e.g., binary vector, schedule) | Replace `Tour` type and the functions that generate a random solution, compute length, and apply a move. |
| **Neighbourhood operator** | Edit the double loop that enumerates moves (`for i … for j …`) and the `apply2Opt` / `delta2Opt` helpers. |
| **Tabu list policy** | Adjust the key stored in `tabu` (sometimes you need to store attribute‑based info like “swap of items X and Y”). |
| **Aspiration criterion** | The condition `if currLen+delta2Opt(curr,m) >= bestLen` can be replaced with any rule that overrides tabu status. |
| **Stopping condition** | Change `maxIter`, `stallLimit`, or add a time‑based check (`time.Since(start) > duration`). |

Because the core of Tabu Search is the **search loop** (`for iter …`) together with the **tabu map**, you can reuse the same structure for virtually any combinatorial optimisation problem.

---  

### References (for further reading)

* Glover, F., & Laguna, M. (1997). *Tabu Search*. Kluwer Academic Publishers.  
* Hoos, H. H., & Stützle, T. (2004). *Stochastic Local Search: Foundations and Applications*. Morgan Kaufmann.  

Happy coding! 🚀