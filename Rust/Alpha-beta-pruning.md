# Alpha-Beta Pruning Algorithm in Rust

Here's a complete implementation of the Alpha-Beta pruning algorithm in Rust for a minimax game tree:

```rust
#[derive(Debug, Clone)]
pub struct GameState {
    pub board: Vec<i32>,
    pub is_maximizing: bool,
    pub depth: i32,
}

impl GameState {
    pub fn new(board: Vec<i32>, is_maximizing: bool, depth: i32) -> Self {
        GameState {
            board,
            is_maximizing,
            depth,
        }
    }
    
    pub fn is_terminal(&self) -> bool {
        // Game is terminal if depth is 0 or board is full
        self.depth == 0 || self.board.iter().all(|&x| x != 0)
    }
    
    pub fn get_valid_moves(&self) -> Vec<usize> {
        self.board
            .iter()
            .enumerate()
            .filter(|&(_, &value)| value == 0)
            .map(|(index, _)| index)
            .collect()
    }
    
    pub fn make_move(&self, index: usize, player: i32) -> Self {
        let mut new_board = self.board.clone();
        new_board[index] = player;
        GameState::new(
            new_board,
            !self.is_maximizing,
            self.depth - 1,
        )
    }
    
    pub fn evaluate(&self) -> i32 {
        // Simple evaluation function for demonstration
        // In a real game, this would be more complex
        let mut score = 0;
        
        // Simple win/loss evaluation
        for i in 0..3 {
            // Check rows
            if self.board[i*3] == 1 && self.board[i*3+1] == 1 && self.board[i*3+2] == 1 {
                score += 10;
            }
            if self.board[i*3] == -1 && self.board[i*3+1] == -1 && self.board[i*3+2] == -1 {
                score -= 10;
            }
            
            // Check columns
            if self.board[i] == 1 && self.board[i+3] == 1 && self.board[i+6] == 1 {
                score += 10;
            }
            if self.board[i] == -1 && self.board[i+3] == -1 && self.board[i+6] == -1 {
                score -= 10;
            }
        }
        
        // Check diagonals
        if self.board[0] == 1 && self.board[4] == 1 && self.board[8] == 1 {
            score += 10;
        }
        if self.board[0] == -1 && self.board[4] == -1 && self.board[8] == -1 {
            score -= 10;
        }
        if self.board[2] == 1 && self.board[4] == 1 && self.board[6] == 1 {
            score += 10;
        }
        if self.board[2] == -1 && self.board[4] == -1 && self.board[6] == -1 {
            score -= 10;
        }
        
        score
    }
}

pub fn alpha_beta_pruning(
    state: &GameState,
    alpha: i32,
    beta: i32,
    depth: i32,
) -> (i32, Option<usize>) {
    // Base case: terminal state or maximum depth reached
    if state.is_terminal() {
        return (state.evaluate(), None);
    }
    
    if state.is_maximizing {
        // Maximizing player's turn
        let mut max_eval = i32::MIN;
        let mut best_move = None;
        
        for move_index in state.get_valid_moves() {
            let new_state = state.make_move(move_index, 1); // Player 1 (maximizing)
            let (eval, _) = alpha_beta_pruning(&new_state, alpha, beta, depth - 1);
            
            if eval > max_eval {
                max_eval = eval;
                best_move = Some(move_index);
            }
            
            alpha = alpha.max(eval);
            
            // Alpha-beta pruning
            if beta <= alpha {
                break; // Beta cutoff
            }
        }
        
        (max_eval, best_move)
    } else {
        // Minimizing player's turn
        let mut min_eval = i32::MAX;
        let mut best_move = None;
        
        for move_index in state.get_valid_moves() {
            let new_state = state.make_move(move_index, -1); // Player -1 (minimizing)
            let (eval, _) = alpha_beta_pruning(&new_state, alpha, beta, depth - 1);
            
            if eval < min_eval {
                min_eval = eval;
                best_move = Some(move_index);
            }
            
            beta = beta.min(eval);
            
            // Alpha-beta pruning
            if beta <= alpha {
                break; // Alpha cutoff
            }
        }
        
        (min_eval, best_move)
    }
}

// Example usage
fn main() {
    // Create a simple 3x3 Tic-Tac-Toe board
    let initial_board = vec![0, 0, 0, 0, 0, 0, 0, 0, 0]; // Empty board
    let initial_state = GameState::new(initial_board, true, 9); // Maximize first
    
    println!("Initial board state:");
    print_board(&initial_state.board);
    
    let (best_score, best_move) = alpha_beta_pruning(&initial_state, i32::MIN, i32::MAX, 9);
    
    println!("\nBest score: {}", best_score);
    println!("Best move: {:?}", best_move);
    
    if let Some(move_pos) = best_move {
        let mut new_board = initial_state.board.clone();
        new_board[move_pos] = 1; // Player 1 makes the move
        println!("After move {}:", move_pos);
        print_board(&new_board);
    }
}

fn print_board(board: &[i32]) {
    for i in 0..3 {
        for j in 0..3 {
            let cell = board[i * 3 + j];
            match cell {
                1 => print!(" X "),
                -1 => print!(" O "),
                0 => print!(" . "),
                _ => print!(" {} ", cell),
            }
        }
        println!();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_alpha_beta_pruning() {
        let board = vec![0, 0, 0, 0, 0, 0, 0, 0, 0];
        let state = GameState::new(board, true, 9);
        let (score, _) = alpha_beta_pruning(&state, i32::MIN, i32::MAX, 9);
        assert!(score >= -10 && score <= 10); // Should be a valid score
    }
}
```

## Key Features of this Implementation:

1. **GameState Structure**: Represents the current game state with board, player turn, and depth
2. **Alpha-Beta Pruning Logic**: Implements the core pruning algorithm with proper alpha and beta values
3. **Minimax Integration**: Works as a minimax algorithm with pruning optimization
4. **Tic-Tac-Toe Example**: Demonstrates usage with a concrete game example
5. **Terminal State Detection**: Properly identifies when the game is over
6. **Move Generation**: Generates valid moves for the current player
7. **Evaluation Function**: Simple but functional evaluation for demonstration

## How Alpha-Beta Pruning Works:

- **Alpha**: The best value that the maximizing player can guarantee at that level or above
- **Beta**: The best value that the minimizing player can guarantee at that level or above
- **Pruning**: When `beta <= alpha`, the remaining branches are pruned (not explored)
- **Optimization**: Reduces the number of nodes evaluated from O(b^d) to O(b^(d/2)) in best case

This implementation provides a solid foundation that can be adapted for various game-playing scenarios!

