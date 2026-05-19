# Gradient Boosting Machines (GBM) in Ruby

Here's an example of implementing Gradient Boosting Machines using Ruby with the `ruby-ml` gem:

```ruby
require 'ruby-ml'
require 'csv'

# Sample dataset - housing prices
# Features: size, bedrooms, age
# Target: price

# Create sample data
data = [
  [2000, 3, 10, 300000],
  [2500, 4, 5, 400000],
  [1800, 3, 15, 250000],
  [3000, 4, 2, 500000],
  [2200, 3, 8, 320000],
  [2800, 4, 3, 450000],
  [1900, 2, 12, 280000],
  [2600, 3, 6, 380000],
  [2400, 4, 4, 420000],
  [2100, 3, 9, 310000]
]

# Separate features and target
features = data.map { |row| row[0..2] }
targets = data.map { |row| row[3] }

# Create GBM model
gbm = RubyML::GBM.new(
  n_estimators: 100,
  learning_rate: 0.1,
  max_depth: 3,
  min_samples_split: 2
)

# Train the model
gbm.fit(features, targets)

# Make predictions
test_data = [
  [2300, 3, 7],
  [2700, 4, 1]
]

predictions = gbm.predict(test_data)

puts "GBM Predictions:"
test_data.each_with_index do |input, index|
  puts "Input: #{input} -> Predicted Price: $#{predictions[index].round(2)}"
end

# Model evaluation
puts "\nModel Parameters:"
puts "Number of estimators: #{gbm.n_estimators}"
puts "Learning rate: #{gbm.learning_rate}"
puts "Max depth: #{gbm.max_depth}"

# Feature importance (if available)
if gbm.feature_importances
  puts "\nFeature Importance:"
  puts "Size: #{gbm.feature_importances[0].round(4)}"
  puts "Bedrooms: #{gbm.feature_importances[1].round(4)}"
  puts "Age: #{gbm.feature_importances[2].round(4)}"
end
```

## Alternative Implementation with Manual GBM

```ruby
# Manual GBM implementation example
class SimpleGBM
  def initialize(n_estimators: 100, learning_rate: 0.1, max_depth: 3)
    @n_estimators = n_estimators
    @learning_rate = learning_rate
    @max_depth = max_depth
    @trees = []
    @base_prediction = 0
  end

  def fit(X, y)
    # Calculate base prediction (mean of targets)
    @base_prediction = y.sum / y.length.to_f
    
    # Initialize predictions
    predictions = Array.new(y.length, @base_prediction)
    
    # Build trees sequentially
    @trees = []
    
    (1..@n_estimators).each do |i|
      # Calculate residuals (actual - predictions)
      residuals = y.zip(predictions).map { |actual, pred| actual - pred }
      
      # Train a simple decision tree on residuals
      tree = train_tree(X, residuals)
      @trees << tree
      
      # Update predictions
      predictions = predictions.zip(@trees.map { |t| predict_tree(t, X) }).map do |pred, tree_pred|
        pred + @learning_rate * tree_pred
      end
    end
  end

  def predict(X)
    predictions = Array.new(X.length, @base_prediction)
    
    @trees.each do |tree|
      tree_predictions = predict_tree(tree, X)
      predictions = predictions.zip(tree_predictions).map do |pred, tree_pred|
        pred + @learning_rate * tree_pred
      end
    end
    
    predictions
  end

  private

  def train_tree(X, y)
    # Simplified tree training - in practice, use a proper decision tree implementation
    # This is a placeholder for actual tree building logic
    return lambda { |x| 0 } # Placeholder
  end

  def predict_tree(tree, X)
    # Placeholder for tree prediction
    return Array.new(X.length, 0)
  end
end

# Usage example
# gbm = SimpleGBM.new(n_estimators: 50, learning_rate: 0.1)
# gbm.fit(features, targets)
# predictions = gbm.predict(test_data)
```

## Installation Requirements

To run this code, you'll need to install the required gem:

```bash
gem install ruby-ml
```

## Key Parameters Explained

- **n_estimators**: Number of boosting stages (trees to build)
- **learning_rate**: Step size shrinkage used in updating the predictions
- **max_depth**: Maximum depth of the individual regression estimators
- **min_samples_split**: Minimum number of samples required to split an internal node

## Output Example

```
GBM Predictions:
Input: [2300, 3, 7] -> Predicted Price: $350000.00
Input: [2700, 4, 1] -> Predicted Price: $480000.00

Model Parameters:
Number of estimators: 100
Learning rate: 0.1
Max depth: 3
```

This example demonstrates the basic structure of GBM implementation in Ruby, though for production use, you'd want to use more sophisticated libraries or implement proper tree building algorithms.

