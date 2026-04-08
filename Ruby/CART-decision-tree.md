# CART Decision Tree Algorithm in Ruby

```ruby
class Node
  attr_accessor :feature, :threshold, :left, :right, :prediction
  
  def initialize(feature = nil, threshold = nil, left = nil, right = nil, prediction = nil)
    @feature = feature
    @threshold = threshold
    @left = left
    @right = right
    @prediction = prediction
  end
end

class CARTDecisionTree
  def initialize(max_depth = 10, min_samples_split = 2)
    @max_depth = max_depth
    @min_samples_split = min_samples_split
    @root = nil
  end
  
  def fit(X, y)
    @root = build_tree(X, y, 0)
  end
  
  def predict(X)
    if X.is_a?(Array) && X[0].is_a?(Array)
      # Multiple samples
      X.map { |sample| predict_sample(sample) }
    else
      # Single sample
      predict_sample(X)
    end
  end
  
  private
  
  def build_tree(X, y, depth)
    num_samples = X.length
    num_features = X[0].length
    
    # Stopping criteria
    if depth >= @max_depth || num_samples < @min_samples_split || y.uniq.length == 1
      return Node.new(prediction: y.mode)
    end
    
    # Find best split
    best_feature, best_threshold = find_best_split(X, y)
    
    # If no good split found
    if best_feature.nil?
      return Node.new(prediction: y.mode)
    end
    
    # Split the data
    left_indices, right_indices = split_data(X, best_feature, best_threshold)
    
    # Create child nodes
    left_child = build_tree(X[left_indices], y[left_indices], depth + 1)
    right_child = build_tree(X[right_indices], y[right_indices], depth + 1)
    
    Node.new(
      feature: best_feature,
      threshold: best_threshold,
      left: left_child,
      right: right_child
    )
  end
  
  def find_best_split(X, y)
    best_gini = Float::INFINITY
    best_feature = nil
    best_threshold = nil
    
    num_features = X[0].length
    
    # Try each feature
    (0...num_features).each do |feature|
      values = X.map { |row| row[feature] }.uniq.sort
      
      # Try each threshold between consecutive values
      (0...values.length - 1).each do |i|
        threshold = (values[i] + values[i + 1]) / 2.0
        
        left_indices, right_indices = split_data(X, feature, threshold)
        
        gini = calculate_gini(y, left_indices, right_indices)
        
        if gini < best_gini
          best_gini = gini
          best_feature = feature
          best_threshold = threshold
        end
      end
    end
    
    [best_feature, best_threshold]
  end
  
  def split_data(X, feature, threshold)
    left_indices = []
    right_indices = []
    
    X.each_with_index do |row, index|
      if row[feature] <= threshold
        left_indices << index
      else
        right_indices << index
      end
    end
    
    [left_indices, right_indices]
  end
  
  def calculate_gini(y, left_indices, right_indices)
    total_samples = y.length
    
    return 0 if total_samples == 0
    
    left_y = left_indices.map { |i| y[i] }
    right_y = right_indices.map { |i| y[i] }
    
    left_gini = calculate_node_gini(left_y)
    right_gini = calculate_node_gini(right_y)
    
    left_weight = left_y.length.to_f / total_samples
    right_weight = right_y.length.to_f / total_samples
    
    left_weight * left_gini + right_weight * right_gini
  end
  
  def calculate_node_gini(y)
    return 0 if y.empty?
    
    total = y.length
    gini = 1.0
    
    y.uniq.each do |value|
      count = y.count(value)
      probability = count.to_f / total
      gini -= probability * probability
    end
    
    gini
  end
  
  def predict_sample(sample)
    node = @root
    
    while node.prediction.nil?
      if sample[node.feature] <= node.threshold
        node = node.left
      else
        node = node.right
      end
    end
    
    node.prediction
  end
end

# Example usage
if __FILE__ == $0
  # Sample dataset: [age, income] -> [decision]
  X = [
    [25, 50000],
    [35, 80000],
    [45, 120000],
    [23, 40000],
    [33, 70000],
    [55, 150000],
    [28, 60000],
    [41, 100000],
    [38, 90000],
    [48, 130000]
  ]
  
  y = [0, 1, 1, 0, 1, 1, 0, 1, 1, 1]  # 0 = No, 1 = Yes
  
  # Create and train the model
  tree = CARTDecisionTree.new(max_depth: 5, min_samples_split: 2)
  tree.fit(X, y)
  
  # Make predictions
  test_data = [
    [30, 65000],
    [50, 140000],
    [20, 35000]
  ]
  
  puts "Predictions:"
  test_data.each_with_index do |sample, index|
    prediction = tree.predict(sample)
    puts "Sample #{index + 1}: #{sample} -> Decision: #{prediction}"
  end
end
```

This implementation includes:

- **Node class**: Represents tree nodes with feature, threshold, left/right children, and prediction
- **CARTDecisionTree class**: Main decision tree implementation with:
  - `fit()` method for training
  - `predict()` method for making predictions
  - Recursive tree building with Gini impurity calculation
  - Stopping criteria based on depth and sample size
- **Key algorithms**:
  - Gini impurity calculation
  - Best split finding
  - Data splitting based on threshold values
  - Recursive tree construction

The example demonstrates training a decision tree on sample age/income data to predict decisions, showing how the algorithm builds a tree structure to make predictions based on feature thresholds.

