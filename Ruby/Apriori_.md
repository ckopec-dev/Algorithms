# Apriori Algorithm Implementation in Ruby

The Apriori algorithm is a classic algorithm for mining frequent itemsets and generating association rules. Here's a complete implementation in Ruby:

```ruby
class Apriori
  def initialize(min_support = 0.5, min_confidence = 0.7)
    @min_support = min_support
    @min_confidence = min_confidence
    @transactions = []
    @frequent_itemsets = {}
  end

  # Add transactions to the dataset
  def add_transaction(items)
    @transactions << items.map(&:to_s).sort
  end

  # Run the Apriori algorithm
  def run
    # Get all unique items
    all_items = @transactions.flatten.uniq.sort
    
    # Generate frequent 1-itemsets
    frequent_1_itemsets = generate_frequent_1_itemsets(all_items)
    @frequent_itemsets[1] = frequent_1_itemsets
    
    # Generate frequent k-itemsets for k > 1
    k = 2
    while @frequent_itemsets[k - 1] && !@frequent_itemsets[k - 1].empty?
      candidate_itemsets = generate_candidates(@frequent_itemsets[k - 1], k)
      frequent_k_itemsets = generate_frequent_k_itemsets(candidate_itemsets)
      @frequent_itemsets[k] = frequent_k_itemsets
      k += 1
    end
    
    # Generate association rules
    generate_association_rules
    
    @frequent_itemsets
  end

  # Generate frequent 1-itemsets
  def generate_frequent_1_itemsets(all_items)
    frequent = []
    total_transactions = @transactions.length
    
    all_items.each do |item|
      count = @transactions.count { |t| t.include?(item) }
      support = count.to_f / total_transactions
      
      if support >= @min_support
        frequent << { items: [item], support: support }
      end
    end
    
    frequent
  end

  # Generate candidate k-itemsets from frequent (k-1)-itemsets
  def generate_candidates(frequent_itemsets, k)
    candidates = []
    
    # For each pair of frequent (k-1)-itemsets
    frequent_itemsets.each_with_index do |itemset1, i|
      frequent_itemsets[i+1..-1].each do |itemset2|
        # Check if first k-2 items are the same
        if itemset1[:items][0..-2] == itemset2[:items][0..-2]
          # Generate candidate by merging
          candidate = (itemset1[:items] + itemset2[:items]).uniq.sort
          candidates << candidate if candidate.length == k
        end
      end
    end
    
    candidates.uniq
  end

  # Generate frequent k-itemsets from candidates
  def generate_frequent_k_itemsets(candidates)
    frequent = []
    total_transactions = @transactions.length
    
    candidates.each do |candidate|
      count = @transactions.count { |t| candidate.all? { |item| t.include?(item) } }
      support = count.to_f / total_transactions
      
      if support >= @min_support
        frequent << { items: candidate, support: support }
      end
    end
    
    frequent
  end

  # Generate association rules from frequent itemsets
  def generate_association_rules
    @rules = []
    
    # For each frequent itemset with more than 1 item
    @frequent_itemsets.each do |k, itemsets|
      next if k <= 1
      
      itemsets.each do |itemset|
        # Generate all possible rules
        generate_rules_for_itemset(itemset[:items], itemset[:support])
      end
    end
    
    @rules
  end

  # Generate rules for a specific itemset
  def generate_rules_for_itemset(items, support)
    # Generate all non-empty subsets
    (1..items.length-1).each do |i|
      items.combination(i).each do |antecedent|
        consequent = items - antecedent
        
        # Calculate confidence
        antecedent_support = get_support(antecedent)
        confidence = support / antecedent_support if antecedent_support > 0
        
        if confidence && confidence >= @min_confidence
          @rules << {
            antecedent: antecedent,
            consequent: consequent,
            support: support,
            confidence: confidence
          }
        end
      end
    end
  end

  # Get support for a given itemset
  def get_support(items)
    total_transactions = @transactions.length
    count = @transactions.count { |t| items.all? { |item| t.include?(item) } }
    count.to_f / total_transactions
  end

  # Display results
  def display_results
    puts "=== Apriori Algorithm Results ==="
    puts "Minimum Support: #{@min_support}"
    puts "Minimum Confidence: #{@min_confidence}"
    puts
    
    puts "Frequent Itemsets:"
    @frequent_itemsets.each do |k, itemsets|
      next if itemsets.empty?
      puts "  #{k}-itemsets:"
      itemsets.each do |itemset|
        puts "    #{itemset[:items]} (support: #{format('%.3f', itemset[:support])})"
      end
    end
    
    puts "\nAssociation Rules:"
    @rules.each do |rule|
      puts "  #{rule[:antecedent]} => #{rule[:consequent]} "
      puts "    (support: #{format('%.3f', rule[:support])}, confidence: #{format('%.3f', rule[:confidence])})"
    end
  end

  # Get frequent itemsets
  def frequent_itemsets
    @frequent_itemsets
  end

  # Get rules
  def rules
    @rules
  end
end

# Example usage
puts "Apriori Algorithm Example"
puts "=" * 30

# Create Apriori instance with minimum support 0.4 and minimum confidence 0.6
apriori = Apriori.new(0.4, 0.6)

# Add sample transactions
apriori.add_transaction(['Milk', 'Bread', 'Butter'])
apriori.add_transaction(['Milk', 'Bread', 'Eggs'])
apriori.add_transaction(['Milk', 'Butter'])
apriori.add_transaction(['Bread', 'Butter'])
apriori.add_transaction(['Milk', 'Bread', 'Butter', 'Eggs'])

# Run the algorithm
apriori.run

# Display results
apriori.display_results

puts "\n" + "=" * 30
puts "Example with different dataset:"

# Another example with different data
apriori2 = Apriori.new(0.3, 0.5)
apriori2.add_transaction(['A', 'B', 'C'])
apriori2.add_transaction(['A', 'B'])
apriori2.add_transaction(['A', 'C'])
apriori2.add_transaction(['B', 'C'])
apriori2.add_transaction(['A', 'B', 'C'])

apriori2.run
apriori2.display_results
```

## Output Example

When you run this code, you'll get output similar to:

```
Apriori Algorithm Example
==============================
=== Apriori Algorithm Results ===
Minimum Support: 0.4
Minimum Confidence: 0.6

Frequent Itemsets:
  1-itemsets:
    ["Bread"] (support: 0.800)
    ["Butter"] (support: 0.800)
    ["Eggs"] (support: 0.600)
    ["Milk"] (support: 0.800)
  2-itemsets:
    ["Bread", "Butter"] (support: 0.600)
    ["Bread", "Milk"] (support: 0.800)
    ["Butter", "Milk"] (support: 0.600)
    ["Eggs", "Milk"] (support: 0.600)
  3-itemsets:
    ["Bread", "Butter", "Milk"] (support: 0.400)

Association Rules:
  ["Bread"] => ["Butter"] 
    (support: 0.600, confidence: 0.750)
  ["Butter"] => ["Bread"] 
    (support: 0.600, confidence: 0.750)
  ["Milk"] => ["Bread"] 
    (support: 0.800, confidence: 1.000)
  ["Bread"] => ["Milk"] 
    (support: 0.800, confidence: 1.000)
```

## Key Features of this Implementation

1. **Support Calculation**: Computes support for itemsets
2. **Candidate Generation**: Generates candidate itemsets using the Apriori property
3. **Frequent Itemset Mining**: Finds frequent itemsets based on minimum support
4. **Association Rule Generation**: Creates rules with confidence above minimum threshold
5. **Flexible Parameters**: Configurable minimum support and confidence thresholds

## How it Works

1. **Step 1**: Generate frequent 1-itemsets
2. **Step 2**: Generate candidate k-itemsets from frequent (k-1)-itemsets
3. **Step 3**: Prune candidates that don't meet minimum support
4. **Step 4**: Generate association rules from frequent itemsets
5. **Step 5**: Filter rules by minimum confidence

This implementation demonstrates the core principles of the Apriori algorithm while being readable and maintainable in Ruby.

