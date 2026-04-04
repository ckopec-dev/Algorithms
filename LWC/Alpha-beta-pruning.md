# Alpha-Beta Pruning in Lightning Web Component

Here's an example implementation of the Alpha-Beta pruning algorithm in a Lightning Web Component:

```javascript
// gameTree.js
import { LightningElement } from 'lwc';

export default class GameTree extends LightningElement {
    board = [0, 0, 0, 0, 0, 0, 0, 0, 0]; // 3x3 Tic-Tac-Toe board
    currentPlayer = 1; // 1 for X, -1 for O
    gameOver = false;
    winner = null;

    // Alpha-Beta pruning implementation
    alphaBeta(board, depth, alpha, beta, maximizingPlayer) {
        const winner = this.checkWinner(board);
        
        // Terminal state
        if (winner !== 0) {
            return winner * (10 - depth);
        }
        
        if (depth === 0) {
            return 0;
        }

        if (maximizingPlayer) {
            let maxEval = -Infinity;
            for (let i = 0; i < 9; i++) {
                if (board[i] === 0) {
                    board[i] = 1; // Place X
                    const eval = this.alphaBeta(board, depth - 1, alpha, beta, false);
                    board[i] = 0; // Undo move
                    
                    maxEval = Math.max(maxEval, eval);
                    alpha = Math.max(alpha, eval);
                    
                    if (beta <= alpha) {
                        break; // Alpha-beta pruning
                    }
                }
            }
            return maxEval;
        } else {
            let minEval = Infinity;
            for (let i = 0; i < 9; i++) {
                if (board[i] === 0) {
                    board[i] = -1; // Place O
                    const eval = this.alphaBeta(board, depth - 1, alpha, beta, true);
                    board[i] = 0; // Undo move
                    
                    minEval = Math.min(minEval, eval);
                    beta = Math.min(beta, eval);
                    
                    if (beta <= alpha) {
                        break; // Alpha-beta pruning
                    }
                }
            }
            return minEval;
        }
    }

    // Find best move using Alpha-Beta pruning
    findBestMove(board) {
        let bestValue = -Infinity;
        let bestMove = -1;
        
        for (let i = 0; i < 9; i++) {
            if (board[i] === 0) {
                board[i] = 1; // Try placing X
                const moveValue = this.alphaBeta(board, 9, -Infinity, Infinity, false);
                board[i] = 0; // Undo move
                
                if (moveValue > bestValue) {
                    bestValue = moveValue;
                    bestMove = i;
                }
            }
        }
        
        return bestMove;
    }

    // Check for winner
    checkWinner(board) {
        // Check rows
        for (let i = 0; i < 9; i += 3) {
            if (board[i] !== 0 && board[i] === board[i + 1] && board[i] === board[i + 2]) {
                return board[i];
            }
        }
        
        // Check columns
        for (let i = 0; i < 3; i++) {
            if (board[i] !== 0 && board[i] === board[i + 3] && board[i] === board[i + 6]) {
                return board[i];
            }
        }
        
        // Check diagonals
        if (board[0] !== 0 && board[0] === board[4] && board[0] === board[8]) {
            return board[0];
        }
        if (board[2] !== 0 && board[2] === board[4] && board[2] === board[6]) {
            return board[2];
        }
        
        // Check for tie
        if (!board.includes(0)) {
            return 0;
        }
        
        return null;
    }

    // Handle player move
    handleCellClick(event) {
        if (this.gameOver || this.currentPlayer !== 1) return;
        
        const cellIndex = parseInt(event.target.dataset.index);
        
        if (this.board[cellIndex] === 0) {
            const newBoard = [...this.board];
            newBoard[cellIndex] = 1;
            
            this.board = newBoard;
            this.currentPlayer = -1;
            
            // Check if game is over
            const winner = this.checkWinner(newBoard);
            if (winner !== null) {
                this.gameOver = true;
                this.winner = winner;
                return;
            }
            
            // AI's turn
            setTimeout(() => {
                this.makeAIMove();
            }, 500);
        }
    }

    // AI makes a move using Alpha-Beta pruning
    makeAIMove() {
        if (this.gameOver) return;
        
        const bestMove = this.findBestMove(this.board);
        const newBoard = [...this.board];
        newBoard[bestMove] = -1;
        
        this.board = newBoard;
        this.currentPlayer = 1;
        
        // Check if game is over
        const winner = this.checkWinner(newBoard);
        if (winner !== null) {
            this.gameOver = true;
            this.winner = winner;
        }
    }

    // Reset game
    resetGame() {
        this.board = [0, 0, 0, 0, 0, 0, 0, 0, 0];
        this.currentPlayer = 1;
        this.gameOver = false;
        this.winner = null;
    }
}
```

```html
<!-- gameTree.html -->
<template>
    <div class="game-container">
        <h2>Tic-Tac-Toe with Alpha-Beta Pruning</h2>
        
        <div class="status">
            <p>
                <template if:true={gameOver}>
                    Game Over! Winner: 
                    <template if:true={winner}>
                        <span class="winner">{winner === 1 ? 'X' : 'O'}</span>
                    </template>
                    <template if:true={winner === 0}>
                        <span class="tie">Tie</span>
                    </template>
                </template>
                <template if:false={gameOver}>
                    Current Player: <span class="current-player">{currentPlayer === 1 ? 'X' : 'O'}</span>
                </template>
            </p>
        </div>

        <div class="board">
            <template for:each={board} for:item="cell" for:index="index">
                <button 
                    key={index}
                    data-index={index}
                    class="cell"
                    onclick={handleCellClick}
                    disabled={gameOver || currentPlayer !== 1}
                >
                    <template if:true={cell === 1}>X</template>
                    <template if:true={cell === -1}>O</template>
                </button>
            </template>
        </div>

        <button class="reset-btn" onclick={resetGame}>Reset Game</button>
    </div>
</template>
```

```css
/* gameTree.css */
.game-container {
    max-width: 400px;
    margin: 20px auto;
    padding: 20px;
    border: 1px solid #ccc;
    border-radius: 8px;
    text-align: center;
}

.board {
    display: grid;
    grid-template-columns: repeat(3, 1fr);
    gap: 5px;
    margin: 20px 0;
}

.cell {
    width: 80px;
    height: 80px;
    font-size: 24px;
    font-weight: bold;
    cursor: pointer;
    border: 2px solid #333;
    background-color: #fff;
    transition: background-color 0.3s;
}

.cell:hover:not(:disabled) {
    background-color: #f0f0f0;
}

.cell:disabled {
    cursor: not-allowed;
    opacity: 0.7;
}

.reset-btn {
    padding: 10px 20px;
    font-size: 16px;
    background-color: #007bff;
    color: white;
    border: none;
    border-radius: 4px;
    cursor: pointer;
    margin-top: 20px;
}

.reset-btn:hover {
    background-color: #0056b3;
}

.status p {
    margin: 10px 0;
    font-size: 18px;
}

.winner {
    color: #28a745;
    font-weight: bold;
}

.tie {
    color: #ffc107;
    font-weight: bold;
}

.current-player {
    color: #007bff;
    font-weight: bold;
}
```

## Key Features of this Implementation:

1. **Alpha-Beta Pruning**: The `alphaBeta` method implements the core algorithm with pruning optimization
2. **Minimax Integration**: Uses minimax logic with alpha-beta optimization for game tree search
3. **Depth Limiting**: Sets a maximum search depth (9 in this case) for performance
4. **Game State Management**: Handles player turns, win detection, and game reset
5. **Performance Optimization**: Early termination when alpha ≥ beta

## How Alpha-Beta Pruning Works:

1. **Maximizing Player (X)**: Tries to maximize the evaluation score
2. **Minimizing Player (O)**: Tries to minimize the evaluation score  
3. **Alpha**: Best value the maximizing player can guarantee
4. **Beta**: Best value the minimizing player can guarantee
5. **Pruning**: When beta ≤ alpha, the branch is pruned (not explored further)

This implementation provides an efficient AI opponent for Tic-Tac-Toe that makes optimal moves using the Alpha-Beta pruning algorithm.

