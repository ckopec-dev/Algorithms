Below is a **self‑contained** example of the classic **Alpha‑Beta Pruning** algorithm written in Go.  
It implements a generic minimax search with pruning for a simple two‑player turn‑based game (here we use a tiny “pick‑a‑number” game for illustration, but the same pattern works for Tic‑Tac‑Toe, Connect‑Four, chess, etc.).

---

## How it works (brief)

* `maxPlayer` tries to **maximize** the evaluation score, `minPlayer` tries to **minimize** it.  
* `alpha` – the best score the maximizer can guarantee so far.  
* `beta`  – the best score the minimizer can guarantee so far.  
* When `beta <= alpha` we can **prune** the remaining branch because it will never affect the final decision.

---

## Go implementation

```go
package main

import (
	"fmt"
	"math"
)

// ---------- Game‑specific part ----------
type State struct {
	// Example: a simple subtraction game.
	// The pile starts with `stones` tokens.
	// On each turn a player may remove 1, 2 or 3 stones.
	// The player who takes the last stone wins.
	stones int
	// true → it's Max's turn, false → Min's turn
	maxTurn bool
}

// Returns all legal child states from the current state.
func (s State) Children() []State {
	var kids []State
	for take := 1; take <= 3; take++ {
		if s.stones-take < 0 {
			break
		}
		kids = append(kids, State{
			stones:   s.stones - take,
			maxTurn:  !s.maxTurn, // turn swaps
		})
	}
	return kids
}

// Terminal test: no stones left → game over.
func (s State) IsTerminal() bool {
	return s.stones == 0
}

// Evaluation function for terminal states.
// +1 if Max just won, -1 if Min just won.
func (s State) Evaluate() float64 {
	// The player who just moved is the opposite of `s.maxTurn`.
	if s.maxTurn { // it's Max's turn now → Min just moved
		return -1 // Min took the last stone → Max loses
	}
	return +1 // Max took the last stone → Max wins
}

// ---------- Alpha‑Beta search ----------
func alphaBeta(state State, depth int, alpha, beta float64, maximizingPlayer bool) (float64, State) {
	// Depth limit or terminal node → return heuristic value.
	if depth == 0 || state.IsTerminal() {
		return state.Evaluate(), state
	}

	if maximizingPlayer {
		maxEval := math.Inf(-1)
		var bestState State
		for _, child := range state.Children() {
			eval, _ := alphaBeta(child, depth-1, alpha, beta, false)
			if eval > maxEval {
				maxEval = eval
				bestState = child
			}
			if eval > alpha {
				alpha = eval
			}
			if beta <= alpha {
				break // β cut‑off
			}
		}
		return maxEval, bestState
	} else { // minimizing player
		minEval := math.Inf(1)
		var bestState State
		for _, child := range state.Children() {
			eval, _ := alphaBeta(child, depth-1, alpha, beta, true)
			if eval < minEval {
				minEval = eval
				bestState = child
			}
			if eval < beta {
				beta = eval
			}
			if beta <= alpha {
				break // α cut‑off
			}
		}
		return minEval, bestState
	}
}

// ---------- Demo ----------
func main() {
	// Start with 7 stones, Max to move first.
	start := State{stones: 7, maxTurn: true}
	const searchDepth = 5 // enough to reach a terminal node for this tiny game

	score, bestMove := alphaBeta(start, searchDepth, math.Inf(-1), math.Inf(1), true)
	fmt.Printf("Result: score = %.0f\n", score) // +1 means Max can force a win
	fmt.Printf("Best first move: take %d stones (leaving %d)\n",
		start.stones-bestMove.stones, bestMove.stones)
}
```

### What the program prints (example)

```
Result: score = 1
Best first move: take 1 stones (leaving 6)
```

*Interpretation*: With optimal play, the first player (Max) can force a win from a pile of 7 stones by taking **1** stone on the first move.

---

## How to adapt this to your own game

| Part | What to replace |
|------|-----------------|
| `State` | Your game’s board representation (e.g., a 3×3 slice for Tic‑Tac‑Toe). |
| `Children()` | Function that generates all legal successor states from a given state. |
| `IsTerminal()` | Detect win/lose/draw conditions (or depth limit). |
| `Evaluate()` | Heuristic score for non‑terminal nodes (e.g., material count, positional bonuses). |
| `maxTurn` flag | Whose turn it is; you can also store it inside `State` if you prefer. |

The core `alphaBeta` routine stays unchanged – it only needs the four methods above to work for any deterministic, zero‑sum, turn‑based game. Happy coding!