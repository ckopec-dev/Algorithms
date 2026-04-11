# Minimax Algorithm in Perl

Here's a complete implementation of the Minimax algorithm in Perl for a simple game (Tic-Tac-Toe):

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Game board representation
my @board = (0, 0, 0, 0, 0, 0, 0, 0, 0);

# Player constants
my $PLAYER_X = 1;
my $PLAYER_O = -1;
my $EMPTY = 0;

# Check if the game is over
sub is_game_over {
    my @board = @_;
    
    # Check for winning conditions
    my @winning_combinations = (
        [0, 1, 2], [3, 4, 5], [6, 7, 8],  # Rows
        [0, 3, 6], [1, 4, 7], [2, 5, 8],  # Columns
        [0, 4, 8], [2, 4, 6]              # Diagonals
    );
    
    foreach my $combo (@winning_combinations) {
        my ($a, $b, $c) = @$combo;
        if ($board[$a] != 0 && $board[$a] == $board[$b] && $board[$b] == $board[$c]) {
            return $board[$a];  # Return the winning player
        }
    }
    
    # Check for draw (board full)
    my $is_full = 1;
    foreach my $cell (@board) {
        if ($cell == 0) {
            $is_full = 0;
            last;
        }
    }
    
    return $is_full ? 0 : undef;  # 0 for draw, undef for ongoing
}

# Minimax algorithm
sub minimax {
    my ($board_ref, $depth, $is_maximizing, $alpha, $beta) = @_;
    my @board = @$board_ref;
    
    # Get the game result
    my $result = is_game_over(@board);
    
    # Terminal states
    if (defined $result) {
        if ($result == $PLAYER_X) {
            return 10 - $depth;  # Prefer quicker wins
        } elsif ($result == $PLAYER_O) {
            return $depth - 10;  # Prefer slower losses
        } else {
            return 0;  # Draw
        }
    }
    
    if ($is_maximizing) {
        my $max_eval = -1000;
        for my $i (0..8) {
            if ($board[$i] == 0) {
                $board[$i] = $PLAYER_X;
                my $eval = minimax(\@board, $depth + 1, 0, $alpha, $beta);
                $board[$i] = 0;
                $max_eval = $eval if $eval > $max_eval;
                $alpha = $eval if $eval > $alpha;
                last if $beta <= $alpha;  # Alpha-beta pruning
            }
        }
        return $max_eval;
    } else {
        my $min_eval = 1000;
        for my $i (0..8) {
            if ($board[$i] == 0) {
                $board[$i] = $PLAYER_O;
                my $eval = minimax(\@board, $depth + 1, 1, $alpha, $beta);
                $board[$i] = 0;
                $min_eval = $eval if $eval < $min_eval;
                $beta = $eval if $eval < $beta;
                last if $beta <= $alpha;  # Alpha-beta pruning
            }
        }
        return $min_eval;
    }
}

# Find the best move for the AI
sub find_best_move {
    my @board = @_;
    my $best_move = -1;
    my $best_value = -1000;
    
    for my $i (0..8) {
        if ($board[$i] == 0) {
            $board[$i] = $PLAYER_X;
            my $move_value = minimax(\@board, 0, 0, -1000, 1000);
            $board[$i] = 0;
            
            if ($move_value > $best_value) {
                $best_value = $move_value;
                $best_move = $i;
            }
        }
    }
    
    return $best_move;
}

# Print the board
sub print_board {
    my @board = @_;
    print "\n";
    for my $i (0..2) {
        for my $j (0..2) {
            my $index = $i * 3 + $j;
            my $symbol = " ";
            $symbol = "X" if $board[$index] == $PLAYER_X;
            $symbol = "O" if $board[$index] == $PLAYER_O;
            print " $symbol ";
            print "|" if $j < 2;
        }
        print "\n";
        print "---+---+---\n" if $i < 2;
    }
    print "\n";
}

# Main game loop
sub main {
    print "Tic-Tac-Toe Game\n";
    print "You are O, AI is X\n";
    
    my @current_board = (0, 0, 0, 0, 0, 0, 0, 0, 0);
    
    while (1) {
        # Print current board
        print_board(@current_board);
        
        # Check if game is over
        my $result = is_game_over(@current_board);
        if (defined $result) {
            if ($result == 0) {
                print "Game ended in a draw!\n";
            } elsif ($result == $PLAYER_X) {
                print "AI (X) wins!\n";
            } elsif ($result == $PLAYER_O) {
                print "You (O) win!\n";
            }
            last;
        }
        
        # Human player's turn
        print "Enter your move (0-8): ";
        my $move = <STDIN>;
        chomp $move;
        
        if ($move >= 0 && $move <= 8 && $current_board[$move] == 0) {
            $current_board[$move] = $PLAYER_O;
        } else {
            print "Invalid move! Try again.\n";
            next;
        }
        
        # Check if game is over after human move
        $result = is_game_over(@current_board);
        if (defined $result) {
            print_board(@current_board);
            if ($result == 0) {
                print "Game ended in a draw!\n";
            } elsif ($result == $PLAYER_X) {
                print "AI (X) wins!\n";
            } elsif ($result == $PLAYER_O) {
                print "You (O) win!\n";
            }
            last;
        }
        
        # AI's turn
        print "AI is thinking...\n";
        my $ai_move = find_best_move(@current_board);
        $current_board[$ai_move] = $PLAYER_X;
    }
}

# Run the game
main();
```

## Key Features of this Implementation:

1. **Minimax Algorithm**: Implements the core minimax logic with recursive evaluation
2. **Alpha-Beta Pruning**: Optimizes performance by pruning unnecessary branches
3. **Game State Management**: Handles board representation and game rules
4. **Tic-Tac-Toe Interface**: Complete game loop with user interaction
5. **Heuristic Evaluation**: Values quicker wins and slower losses

## How to Run:

1. Save the code to a file (e.g., `minimax_tictactoe.pl`)
2. Run with: `perl minimax_tictactoe.pl`
3. Follow the prompts to play against the AI

## Algorithm Explanation:

- **Maximizing Player (X)**: Tries to maximize the score
- **Minimizing Player (O)**: Tries to minimize the score
- **Terminal States**: Win (+10), Loss (-10), Draw (0)
- **Depth-based Scoring**: Quicker wins are preferred
- **Alpha-Beta Pruning**: Reduces the search space significantly

This implementation demonstrates the classic application of minimax in game theory and provides a solid foundation for understanding adversarial search algorithms.

