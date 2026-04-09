# N-Queens Problem in Lightning Web Component

Here's a complete example of solving the N-Queens problem using Lightning Web Components:

## HTML Template (nQueens.html)

```html
<template>
    <div class="container">
        <h2>N-Queens Problem Solver</h2>
        
        <div class="controls">
            <lightning-input 
                type="number" 
                label="Board Size (N)" 
                value={boardSize} 
                min="1" 
                max="15"
                onchange={handleSizeChange}>
            </lightning-input>
            <lightning-button 
                label="Solve" 
                onclick={solveNQueens} 
                disabled={isSolving}>
            </lightning-button>
            <lightning-button 
                label="Clear" 
                onclick={clearBoard} 
                variant="destructive">
            </lightning-button>
        </div>

        <div class="status">
            <p>Queens placed: {queensCount}</p>
            <p>Status: {statusMessage}</p>
        </div>

        <div class="board-container" if:true={showBoard}>
            <div class="chess-board">
                <template for:each={board} for:item="row">
                    <div key={row.index} class="board-row">
                        <template for:each={row.squares} for:item="square">
                            <div 
                                key={square.index} 
                                class="square {square.class}"
                                onclick={handleSquareClick}>
                                <template if:true={square.hasQueen}>
                                    <lightning-icon 
                                        icon-name="utility:check" 
                                        size="small" 
                                        variant="inverse">
                                    </lightning-icon>
                                </template>
                            </div>
                        </template>
                    </div>
                </template>
            </div>
        </div>

        <div class="solution-info" if:true={showSolutionInfo}>
            <h3>Solution Found!</h3>
            <p>Found {solutionCount} solutions</p>
            <p>Time taken: {executionTime} ms</p>
        </div>
    </div>
</template>
```

## JavaScript Controller (nQueens.js)

```javascript
import { LightningElement, track } from 'lwc';

export default class NQueens extends LightningElement {
    @track boardSize = 4;
    @track board = [];
    @track queensCount = 0;
    @track isSolving = false;
    @track statusMessage = 'Ready to solve';
    @track showBoard = false;
    @track showSolutionInfo = false;
    @track solutionCount = 0;
    @track executionTime = 0;
    @track solutions = [];

    connectedCallback() {
        this.initializeBoard();
    }

    initializeBoard() {
        this.board = [];
        for (let i = 0; i < this.boardSize; i++) {
            const row = {
                index: i,
                squares: []
            };
            for (let j = 0; j < this.boardSize; j++) {
                row.squares.push({
                    index: j,
                    hasQueen: false,
                    class: (i + j) % 2 === 0 ? 'white-square' : 'black-square'
                });
            }
            this.board.push(row);
        }
        this.showBoard = true;
    }

    handleSizeChange(event) {
        this.boardSize = parseInt(event.target.value) || 4;
        this.initializeBoard();
        this.clearBoard();
    }

    clearBoard() {
        this.queensCount = 0;
        this.solutionCount = 0;
        this.executionTime = 0;
        this.showSolutionInfo = false;
        this.solutions = [];
        
        for (let i = 0; i < this.boardSize; i++) {
            for (let j = 0; j < this.boardSize; j++) {
                this.board[i].squares[j].hasQueen = false;
            }
        }
        this.statusMessage = 'Ready to solve';
        this.updateBoard();
    }

    handleSquareClick(event) {
        if (this.isSolving) return;
        
        const row = parseInt(event.target.closest('.board-row').dataset.index);
        const col = parseInt(event.target.closest('.square').dataset.index);
        
        if (this.board[row].squares[col].hasQueen) {
            // Remove queen
            this.board[row].squares[col].hasQueen = false;
            this.queensCount--;
        } else {
            // Add queen
            this.board[row].squares[col].hasQueen = true;
            this.queensCount++;
        }
        this.updateBoard();
    }

    updateBoard() {
        // Trigger re-render
        this.board = [...this.board];
    }

    solveNQueens() {
        if (this.isSolving) return;
        
        this.isSolving = true;
        this.statusMessage = 'Solving...';
        this.solutionCount = 0;
        this.solutions = [];
        
        const startTime = performance.now();
        
        // Solve using backtracking
        this.solutions = this.solveBacktracking(0, []);
        
        const endTime = performance.now();
        this.executionTime = Math.round(endTime - startTime);
        
        if (this.solutions.length > 0) {
            this.statusMessage = `Solution found! ${this.solutions.length} solutions`;
            this.showSolutionInfo = true;
            // Display first solution
            this.displaySolution(this.solutions[0]);
        } else {
            this.statusMessage = 'No solution found';
        }
        
        this.isSolving = false;
    }

    solveBacktracking(row, queens) {
        if (row === this.boardSize) {
            // Found a solution
            return [queens.map(q => [...q])];
        }

        let solutions = [];
        
        for (let col = 0; col < this.boardSize; col++) {
            if (this.isSafe(row, col, queens)) {
                queens.push([row, col]);
                const subSolutions = this.solveBacktracking(row + 1, queens);
                solutions = solutions.concat(subSolutions);
                queens.pop();
            }
        }
        
        return solutions;
    }

    isSafe(row, col, queens) {
        for (let [qRow, qCol] of queens) {
            // Check column
            if (qCol === col) return false;
            
            // Check diagonal
            if (Math.abs(qRow - row) === Math.abs(qCol - col)) return false;
        }
        return true;
    }

    displaySolution(solution) {
        this.clearBoard();
        
        for (let [row, col] of solution) {
            this.board[row].squares[col].hasQueen = true;
        }
        this.queensCount = solution.length;
        this.updateBoard();
    }
}
```

## CSS Styles (nQueens.css)

```css
.container {
    max-width: 800px;
    margin: 0 auto;
    padding: 20px;
    font-family: Arial, sans-serif;
}

.controls {
    display: flex;
    gap: 10px;
    margin-bottom: 20px;
    flex-wrap: wrap;
}

.board-container {
    display: flex;
    justify-content: center;
    margin: 20px 0;
}

.chess-board {
    border: 2px solid #333;
    display: inline-block;
}

.board-row {
    display: flex;
}

.square {
    width: 40px;
    height: 40px;
    display: flex;
    align-items: center;
    justify-content: center;
    cursor: pointer;
    border: 1px solid #ccc;
    transition: background-color 0.2s;
}

.square:hover {
    background-color: #e0e0e0;
}

.white-square {
    background-color: #f0d9b5;
}

.black-square {
    background-color: #b58863;
}

.square.has-queen {
    background-color: #4CAF50;
}

.square.has-queen:hover {
    background-color: #45a049;
}

.status {
    margin: 15px 0;
    padding: 10px;
    background-color: #f5f5f5;
    border-radius: 5px;
}

.solution-info {
    margin-top: 20px;
    padding: 15px;
    background-color: #e8f5e8;
    border-radius: 5px;
    border-left: 4px solid #4CAF50;
}

.lightning-button {
    margin: 5px;
}
```

## Key Features of this Implementation:

1. **Interactive Board**: Click on squares to place/remove queens
2. **Backtracking Algorithm**: Efficiently solves the N-Queens problem
3. **Visual Feedback**: Clear indication of queen positions
4. **Performance Tracking**: Shows execution time
5. **Solution Display**: Shows first solution found
6. **Responsive Design**: Works well on different screen sizes

## How it Works:

1. **Initialization**: Creates an N×N chess board
2. **Input Validation**: Validates board size (1-15)
3. **Backtracking**: Uses recursive backtracking to find solutions
4. **Safety Check**: Ensures no two queens attack each other
5. **Visualization**: Updates the UI to show current state

This implementation demonstrates the classic N-Queens problem solution using modern Lightning Web Component architecture with proper state management and user interaction.

