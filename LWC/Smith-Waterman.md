# Smith-Waterman Algorithm in Lightning Web Component

Here's a complete implementation of the Smith-Waterman sequence alignment algorithm in Lightning Web Component:

## HTML Template (smithWaterman.html)

```html
<template>
    <div class="slds-box slds-box_small slds-m-around_medium">
        <h2 class="slds-text-heading_small slds-m-bottom_small">Smith-Waterman Sequence Alignment</h2>
        
        <div class="slds-grid slds-gutters slds-wrap">
            <div class="slds-col slds-size_1-of-2">
                <lightning-input 
                    label="Sequence 1" 
                    value={sequence1} 
                    onchange={handleSequence1Change}
                    variant="label-hidden">
                </lightning-input>
            </div>
            <div class="slds-col slds-size_1-of-2">
                <lightning-input 
                    label="Sequence 2" 
                    value={sequence2} 
                    onchange={handleSequence2Change}
                    variant="label-hidden">
                </lightning-input>
            </div>
        </div>

        <div class="slds-grid slds-gutters slds-wrap slds-m-top_small">
            <div class="slds-col slds-size_1-of-2">
                <lightning-button 
                    label="Align Sequences" 
                    onclick={performAlignment}
                    variant="brand">
                </lightning-button>
            </div>
            <div class="slds-col slds-size_1-of-2">
                <lightning-button 
                    label="Clear Results" 
                    onclick={clearResults}
                    variant="destructive">
                </lightning-button>
            </div>
        </div>

        <div class="slds-m-top_small">
            <lightning-card title="Alignment Results">
                <div class="slds-p-around_small">
                    <template if:true={showResults}>
                        <div class="slds-grid slds-gutters slds-wrap">
                            <div class="slds-col slds-size_1-of-2">
                                <p class="slds-text-title">Best Alignment</p>
                                <p class="slds-text-body_small">{bestAlignment1}</p>
                                <p class="slds-text-body_small">{bestAlignment2}</p>
                                <p class="slds-text-body_small">Score: {bestScore}</p>
                            </div>
                            <div class="slds-col slds-size_1-of-2">
                                <p class="slds-text-title">Score Matrix</p>
                                <div class="slds-scrollable_x">
                                    <table class="slds-table slds-table_bordered slds-table_cell-buffer">
                                        <thead>
                                            <tr class="slds-text-title_caps">
                                                <th scope="col">-</th>
                                                <template for:each={matrixHeaders} for:item="header">
                                                    <th scope="col" key={header}>{header}</th>
                                                </template>
                                            </tr>
                                        </thead>
                                        <tbody>
                                            <template for:each={scoreMatrix} for:item="row" for:index="index">
                                                <tr key={index}>
                                                    <th scope="row">{matrixHeaders[index]}</th>
                                                    <template for:each={row} for:item="cell">
                                                        <td key={cell}>{cell}</td>
                                                    </template>
                                                </tr>
                                            </template>
                                        </tbody>
                                    </table>
                                </div>
                            </div>
                        </div>
                    </template>
                    <template if:false={showResults}>
                        <p class="slds-text-body_small">Click "Align Sequences" to see results</p>
                    </template>
                </div>
            </lightning-card>
        </div>
    </div>
</template>
```

## JavaScript Controller (smithWaterman.js)

```javascript
import { LightningElement } from 'lwc';

export default class SmithWaterman extends LightningElement {
    sequence1 = 'ACGTACGT';
    sequence2 = 'ACGTACGT';
    scoreMatrix = [];
    matrixHeaders = [];
    bestAlignment1 = '';
    bestAlignment2 = '';
    bestScore = 0;
    showResults = false;

    // Handle sequence input changes
    handleSequence1Change(event) {
        this.sequence1 = event.target.value.toUpperCase();
    }

    handleSequence2Change(event) {
        this.sequence2 = event.target.value.toUpperCase();
    }

    // Perform Smith-Waterman alignment
    performAlignment() {
        if (!this.sequence1 || !this.sequence2) {
            return;
        }

        // Initialize scoring matrix
        const matrix = this.initializeMatrix();
        
        // Fill the matrix
        this.fillMatrix(matrix);
        
        // Find the maximum score and its position
        const maxScore = this.findMaxScore(matrix);
        
        // Trace back to find the best alignment
        const alignments = this.traceBack(matrix, maxScore);
        
        // Store results
        this.scoreMatrix = matrix;
        this.bestAlignment1 = alignments.alignment1;
        this.bestAlignment2 = alignments.alignment2;
        this.bestScore = maxScore.score;
        this.showResults = true;
    }

    // Initialize the scoring matrix
    initializeMatrix() {
        const rows = this.sequence2.length + 1;
        const cols = this.sequence1.length + 1;
        const matrix = Array(rows).fill().map(() => Array(cols).fill(0));
        
        // Set headers
        this.matrixHeaders = ['-'].concat(this.sequence1.split(''));
        return matrix;
    }

    // Fill the scoring matrix using Smith-Waterman algorithm
    fillMatrix(matrix) {
        const matchScore = 2;
        const mismatchScore = -1;
        const gapPenalty = -1;

        for (let i = 1; i < matrix.length; i++) {
            for (let j = 1; j < matrix[0].length; j++) {
                const diagonal = matrix[i-1][j-1] + 
                    (this.sequence2[i-1] === this.sequence1[j-1] ? matchScore : mismatchScore);
                const up = matrix[i-1][j] + gapPenalty;
                const left = matrix[i][j-1] + gapPenalty;
                
                // Smith-Waterman: take maximum of three options or 0
                matrix[i][j] = Math.max(0, diagonal, up, left);
            }
        }
    }

    // Find maximum score in the matrix
    findMaxScore(matrix) {
        let maxScore = 0;
        let maxRow = 0;
        let maxCol = 0;

        for (let i = 0; i < matrix.length; i++) {
            for (let j = 0; j < matrix[0].length; j++) {
                if (matrix[i][j] > maxScore) {
                    maxScore = matrix[i][j];
                    maxRow = i;
                    maxCol = j;
                }
            }
        }

        return {
            score: maxScore,
            row: maxRow,
            col: maxCol
        };
    }

    // Trace back to find the alignment
    traceBack(matrix, maxScore) {
        let alignment1 = '';
        let alignment2 = '';
        let i = maxScore.row;
        let j = maxScore.col;
        let currentScore = matrix[i][j];

        // Trace back from maximum score
        while (i > 0 && j > 0 && currentScore > 0) {
            const diagonal = matrix[i-1][j-1];
            const up = matrix[i-1][j];
            const left = matrix[i][j-1];

            // Determine which direction we came from
            if (currentScore === diagonal + (this.sequence2[i-1] === this.sequence1[j-1] ? 2 : -1)) {
                alignment1 = this.sequence1[j-1] + alignment1;
                alignment2 = this.sequence2[i-1] + alignment2;
                i--;
                j--;
            } else if (currentScore === up + -1) {
                alignment1 = '-' + alignment1;
                alignment2 = this.sequence2[i-1] + alignment2;
                i--;
            } else {
                alignment1 = this.sequence1[j-1] + alignment1;
                alignment2 = '-' + alignment2;
                j--;
            }

            currentScore = matrix[i][j];
        }

        return {
            alignment1: alignment1,
            alignment2: alignment2
        };
    }

    // Clear all results
    clearResults() {
        this.showResults = false;
        this.bestAlignment1 = '';
        this.bestAlignment2 = '';
        this.bestScore = 0;
        this.scoreMatrix = [];
    }
}
```

## CSS Styles (smithWaterman.css)

```css
.slds-table_cell-buffer td,
.slds-table_cell-buffer th {
    padding: 0.25rem;
    text-align: center;
}

.slds-scrollable_x {
    max-width: 100%;
    overflow-x: auto;
}

.slds-text-title_caps {
    text-transform: uppercase;
}
```

## Usage Example

When you use this component with the following sequences:
- Sequence 1: `ACGTACGT`
- Sequence 2: `ACGTACGT`

The algorithm will:
1. Create a scoring matrix
2. Fill it using Smith-Waterman scoring rules
3. Find the optimal local alignment
4. Display both the alignment and the scoring matrix

## Key Features

1. **Interactive Input**: Users can modify both sequences
2. **Visual Matrix Display**: Shows the full scoring matrix
3. **Alignment Results**: Displays the best local alignment
4. **Clear Results**: Reset functionality to start over
5. **Responsive Design**: Works well on different screen sizes

## Algorithm Details

The Smith-Waterman algorithm uses:
- **Match score**: +2 for identical characters
- **Mismatch score**: -1 for different characters  
- **Gap penalty**: -1 for insertions/deletions
- **Matrix initialization**: All values start at 0 (local alignment)
- **Traceback**: Follows the path of maximum scores back to find alignment

This implementation provides a complete, functional Smith-Waterman sequence alignment tool within the Lightning Web Component framework.

