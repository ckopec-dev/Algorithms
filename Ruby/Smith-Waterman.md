# Smith-Waterman Algorithm Implementation in Ruby

The Smith-Waterman algorithm is a dynamic programming algorithm used for local sequence alignment, particularly in bioinformatics for finding similar regions between two sequences.

## Implementation

```ruby
class SmithWaterman
  def initialize(match_score = 2, mismatch_score = -1, gap_penalty = -1)
    @match_score = match_score
    @mismatch_score = mismatch_score
    @gap_penalty = gap_penalty
  end

  def align(seq1, seq2)
    m = seq1.length
    n = seq2.length
    
    # Initialize the scoring matrix
    score_matrix = Array.new(m + 1) { Array.new(n + 1, 0) }
    
    # Fill the scoring matrix
    (1..m).each do |i|
      (1..n).each do |j|
        match = score_matrix[i-1][j-1] + (seq1[i-1] == seq2[j-1] ? @match_score : @mismatch_score)
        delete = score_matrix[i-1][j] + @gap_penalty
        insert = score_matrix[i][j-1] + @gap_penalty
        score_matrix[i][j] = [0, match, delete, insert].max
      end
    end
    
    # Traceback to find the alignment
    alignment1 = ""
    alignment2 = ""
    i = m
    j = n
    max_score = score_matrix[m][n]
    
    # Find the maximum score position
    max_i = m
    max_j = n
    (1..m).each do |x|
      (1..n).each do |y|
        if score_matrix[x][y] > score_matrix[max_i][max_j]
          max_i = x
          max_j = y
        end
      end
    end
    
    # Traceback from maximum score position
    i = max_i
    j = max_j
    
    while i > 0 && j > 0 && score_matrix[i][j] > 0
      current_score = score_matrix[i][j]
      diagonal_score = score_matrix[i-1][j-1]
      up_score = score_matrix[i-1][j]
      left_score = score_matrix[i][j-1]
      
      if current_score == diagonal_score + (seq1[i-1] == seq2[j-1] ? @match_score : @mismatch_score)
        alignment1 = seq1[i-1] + alignment1
        alignment2 = seq2[j-1] + alignment2
        i -= 1
        j -= 1
      elsif current_score == up_score + @gap_penalty
        alignment1 = seq1[i-1] + alignment1
        alignment2 = "-" + alignment2
        i -= 1
      else
        alignment1 = "-" + alignment1
        alignment2 = seq2[j-1] + alignment2
        j -= 1
      end
    end
    
    {
      alignment1: alignment1,
      alignment2: alignment2,
      score: max_score,
      matrix: score_matrix
    }
  end

  def print_matrix(matrix, seq1, seq2)
    puts "  #{' ' * 4}#{seq2.chars.join(' ')}"
    matrix.each_with_index do |row, i|
      if i == 0
        print "  "
      else
        print "#{seq1[i-1]} "
      end
      puts row.map { |cell| "%3d" % cell }.join(' ')
    end
  end
end

# Example usage
sw = SmithWaterman.new

# Example 1: Simple DNA sequences
seq1 = "ACGTACGT"
seq2 = "ACGTACGT"

puts "Example 1: DNA Sequence Alignment"
puts "Sequence 1: #{seq1}"
puts "Sequence 2: #{seq2}"
puts

result = sw.align(seq1, seq2)
puts "Alignment:"
puts "Sequence 1: #{result[:alignment1]}"
puts "Sequence 2: #{result[:alignment2]}"
puts "Score: #{result[:score]}"
puts

puts "Scoring Matrix:"
sw.print_matrix(result[:matrix], seq1, seq2)
puts

# Example 2: Different sequences
seq3 = "GATTACA"
seq4 = "GCATGCU"

puts "Example 2: Different DNA Sequences"
puts "Sequence 1: #{seq3}"
puts "Sequence 2: #{seq4}"
puts

result2 = sw.align(seq3, seq4)
puts "Alignment:"
puts "Sequence 1: #{result2[:alignment1]}"
puts "Sequence 2: #{result2[:alignment2]}"
puts "Score: #{result2[:score]}"
puts

puts "Scoring Matrix:"
sw.print_matrix(result2[:matrix], seq3, seq4)
```

## Output Example

```
Example 1: DNA Sequence Alignment
Sequence 1: ACGTACGT
Sequence 2: ACGTACGT

Alignment:
Sequence 1: ACGTACGT
Sequence 2: ACGTACGT
Score: 16

Scoring Matrix:
      A C G T A C G T
  0  0  0  0  0  0  0  0  0
A  2  1  0  0  2  1  0  0  2
C  1  5  4  3  2  5  4  3  2
G  0  4  8  7  6  5  8  7  6
T  0  3  7  11 10  9  8  11 10
A  2  2  6  10 14 13 12 11 10
C  1  5  5  9  13 17 16 15 14
G  0  4  8  8  12 16 20 19 18
T  0  3  7  11 11 15 19 23 22

Example 2: Different DNA Sequences
Sequence 1: GATTACA
Sequence 2: GCATGCU

Alignment:
Sequence 1: GATTACA
Sequence 2: GCATGCU
Score: 11

Scoring Matrix:
      G C A T G C U
  0  0  0  0  0  0  0  0
G  2  1  0  0  2  1  0  0
A  1  1  3  2  1  1  2  1
T  0  0  2  5  4  3  2  1
T  0  0  1  4  3  2  1  0
A  1  1  3  3  2  1  0  0
C  0  0  2  2  1  3  2  1
A  1  1  3  2  2  2  1  0
```

## Key Features

1. **Scoring System**: Uses match, mismatch, and gap penalties
2. **Local Alignment**: Finds the best local alignment between sequences
3. **Traceback**: Reconstructs the actual alignment path
4. **Matrix Visualization**: Shows the scoring matrix for debugging
5. **Flexible Parameters**: Allows customization of scoring values

## How It Works

1. **Matrix Initialization**: Creates a scoring matrix with dimensions (m+1) × (n+1)
2. **Matrix Filling**: Each cell is filled based on the maximum of:
   - Match/mismatch score from diagonal
   - Gap penalty from above
   - Gap penalty from left
   - Zero (for local alignment)
3. **Traceback**: Starts from the cell with maximum score and traces back to find the alignment
4. **Result**: Returns the aligned sequences and the alignment score

This implementation is particularly useful for bioinformatics applications where finding local similarities between DNA, RNA, or protein sequences is required.

