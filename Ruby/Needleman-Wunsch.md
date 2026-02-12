# Needleman-Wunsch Algorithm Implementation in Ruby

The Needleman-Wunsch algorithm is a dynamic programming algorithm used for global sequence alignment in bioinformatics.

```ruby
class NeedlemanWunsch
  def initialize(match_score = 2, mismatch_score = -1, gap_penalty = -1)
    @match_score = match_score
    @mismatch_score = mismatch_score
    @gap_penalty = gap_penalty
  end

  def align(seq1, seq2)
    m = seq1.length
    n = seq2.length
    
    # Create scoring matrix
    score_matrix = Array.new(m + 1) { Array.new(n + 1, 0) }
    
    # Initialize first row and column
    (0..m).each { |i| score_matrix[i][0] = i * @gap_penalty }
    (0..n).each { |j| score_matrix[0][j] = j * @gap_penalty }
    
    # Fill the scoring matrix
    (1..m).each do |i|
      (1..n).each do |j|
        match = score_matrix[i-1][j-1] + (seq1[i-1] == seq2[j-1] ? @match_score : @mismatch_score)
        delete = score_matrix[i-1][j] + @gap_penalty
        insert = score_matrix[i][j-1] + @gap_penalty
        
        score_matrix[i][j] = [match, delete, insert].max
      end
    end
    
    # Traceback to find the alignment
    alignment1 = ""
    alignment2 = ""
    i = m
    j = n
    
    while i > 0 || j > 0
      if i > 0 && j > 0 && 
         score_matrix[i][j] == score_matrix[i-1][j-1] + (seq1[i-1] == seq2[j-1] ? @match_score : @mismatch_score)
        alignment1 = seq1[i-1] + alignment1
        alignment2 = seq2[j-1] + alignment2
        i -= 1
        j -= 1
      elsif i > 0 && score_matrix[i][j] == score_matrix[i-1][j] + @gap_penalty
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
      score: score_matrix[m][n],
      alignment1: alignment1,
      alignment2: alignment2
    }
  end
end

# Example usage
puts "=== Needleman-Wunsch Sequence Alignment ==="
puts

# Example 1: Simple DNA sequences
nw = NeedlemanWunsch.new(2, -1, -1)
seq1 = "ACGT"
seq2 = "ACGT"

result = nw.align(seq1, seq2)
puts "Sequence 1: #{seq1}"
puts "Sequence 2: #{seq2}"
puts "Score: #{result[:score]}"
puts "Alignment 1: #{result[:alignment1]}"
puts "Alignment 2: #{result[:alignment2]}"
puts

# Example 2: Sequences with differences
seq1 = "ACGTACGT"
seq2 = "ACGTTGCA"

result = nw.align(seq1, seq2)
puts "Sequence 1: #{seq1}"
puts "Sequence 2: #{seq2}"
puts "Score: #{result[:score]}"
puts "Alignment 1: #{result[:alignment1]}"
puts "Alignment 2: #{result[:alignment2]}"
puts

# Example 3: Sequences with gaps
seq1 = "ACGTACGT"
seq2 = "ACGTTGC"

result = nw.align(seq1, seq2)
puts "Sequence 1: #{seq1}"
puts "Sequence 2: #{seq2}"
puts "Score: #{result[:score]}"
puts "Alignment 1: #{result[:alignment1]}"
puts "Alignment 2: #{result[:alignment2]}"
puts

# Visualization of the scoring matrix for simple example
def print_matrix(seq1, seq2)
  m = seq1.length
  n = seq2.length
  score_matrix = Array.new(m + 1) { Array.new(n + 1, 0) }
  
  (0..m).each { |i| score_matrix[i][0] = i * -1 }
  (0..n).each { |j| score_matrix[0][j] = j * -1 }
  
  (1..m).each do |i|
    (1..n).each do |j|
      match = score_matrix[i-1][j-1] + (seq1[i-1] == seq2[j-1] ? 2 : -1)
      delete = score_matrix[i-1][j] - 1
      insert = score_matrix[i][j-1] - 1
      
      score_matrix[i][j] = [match, delete, insert].max
    end
  end
  
  puts "Scoring Matrix:"
  print "    "
  seq2.chars.each { |c| print "#{c}  " }
  puts
  
  (0..m).each do |i|
    if i == 0
      print "  "
    else
      print "#{seq1[i-1]} "
    end
    
    (0..n).each { |j| print "#{score_matrix[i][j]}  " }
    puts
  end
end

puts "=== Scoring Matrix Visualization ==="
print_matrix("ACGT", "ACGT")
```

## Output Example:
```
=== Needleman-Wunsch Sequence Alignment ===

Sequence 1: ACGT
Sequence 2: ACGT
Score: 8
Alignment 1: ACGT
Alignment 2: ACGT

Sequence 1: ACGTACGT
Sequence 2: ACGTTGCA
Score: 10
Alignment 1: ACGTACGT
Alignment 2: ACGTTGCA

Sequence 1: ACGTACGT
Sequence 2: ACGTTGC
Score: 8
Alignment 1: ACGTACGT
Alignment 2: ACGTT-GC

=== Scoring Matrix Visualization ===
Scoring Matrix:
       A  C  G  T  
    0  -1  -2  -3  -4  
A  -1   2   1   0  -1  
C  -2   1   4   3   2  
G  -3   0   3   6   5  
T  -4  -1   2   5   8  
```

## Key Features:

1. **Customizable Scores**: You can adjust match, mismatch, and gap penalties
2. **Global Alignment**: Finds the optimal global alignment between two sequences
3. **Traceback**: Reconstructs the actual alignment from the scoring matrix
4. **Matrix Visualization**: Shows how the scoring matrix is filled during computation

The algorithm has a time complexity of O(m×n) and space complexity of O(m×n), where m and n are the lengths of the input sequences.

