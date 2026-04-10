# ID3 Decision Tree Algorithm in Ruby

Here's a complete implementation of the ID3 algorithm for building decision trees in Ruby:

```ruby
require 'set'

class Node
  attr_accessor :feature, :value, :children, :label, :is_leaf
  
  def initialize(feature = nil, value = nil, label = nil)
    @feature = feature
    @value = value
    @label = label
    @children = {}
    @is_leaf = false
  end
  
  def add_child(value, node)
    @children[value] = node
  end
  
  def is_leaf?
    @is_leaf
  end
end

class ID3DecisionTree
  def initialize
    @root = nil
  end
  
  def train(data, features, target_attribute)
    @features = features
    @target_attribute = target_attribute
    @root = build_tree(data, features)
  end
  
  def predict(instance)
    predict_recursive(@root, instance)
  end
  
  private
  
  def build_tree(data, features)
    # If all instances have the same target value, create leaf node
    target_values = data.map { |row| row[@target_attribute] }
    if target_values.uniq.length == 1
      node = Node.new
      node.is_leaf = true
      node.label = target_values.first
      return node
    end
    
    # If no features left, return leaf with most common target value
    if features.empty?
      most_common = most_common_value(data, @target_attribute)
      node = Node.new
      node.is_leaf = true
      node.label = most_common
      return node
    end
    
    # Choose best feature using information gain
    best_feature = choose_best_feature(data, features)
    
    # Create internal node
    node = Node.new(best_feature)
    
    # Get all possible values for the best feature
    feature_values = data.map { |row| row[best_feature] }.uniq
    
    # Create child nodes for each feature value
    feature_values.each do |value|
      # Filter data for instances with this feature value
      subset = data.select { |row| row[best_feature] == value }
      
      # Remove the best feature from remaining features
      remaining_features = features - [best_feature]
      
      if subset.empty?
        # If no instances with this feature value, create leaf with most common target
        leaf = Node.new
        leaf.is_leaf = true
        leaf.label = most_common_value(data, @target_attribute)
        node.add_child(value, leaf)
      else
        # Recursively build subtree
        node.add_child(value, build_tree(subset, remaining_features))
      end
    end
    
    node
  end
  
  def choose_best_feature(data, features)
    best_feature = nil
    best_gain = -1
    
    features.each do |feature|
      gain = information_gain(data, feature)
      if gain > best_gain
        best_gain = gain
        best_feature = feature
      end
    end
    
    best_feature
  end
  
  def information_gain(data, feature)
    # Calculate entropy of the whole dataset
    total_entropy = entropy(data)
    
    # Calculate weighted average entropy after splitting
    weighted_entropy = 0
    feature_values = data.map { |row| row[feature] }.uniq
    
    feature_values.each do |value|
      subset = data.select { |row| row[feature] == value }
      weight = subset.length.to_f / data.length.to_f
      weighted_entropy += weight * entropy(subset)
    end
    
    total_entropy - weighted_entropy
  end
  
  def entropy(data)
    return 0 if data.empty?
    
    target_values = data.map { |row| row[@target_attribute] }
    total = target_values.length
    entropy = 0
    
    # Get unique target values and their counts
    target_counts = target_values.group_by(&:itself).transform_values(&:length)
    
    target_counts.each do |target_value, count|
      probability = count.to_f / total.to_f
      entropy -= probability * Math.log2(probability) if probability > 0
    end
    
    entropy
  end
  
  def most_common_value(data, attribute)
    values = data.map { |row| row[attribute] }
    values.group_by(&:itself).transform_values(&:length).max_by(&:last).first
  end
  
  def predict_recursive(node, instance)
    return node.label if node.is_leaf?
    
    feature_value = instance[node.feature]
    child_node = node.children[feature_value]
    
    if child_node.nil?
      # If no child for this feature value, return most common target
      return "unknown"
    end
    
    predict_recursive(child_node, instance)
  end
end

# Example usage:
if __FILE__ == $0
  # Sample dataset: Play Tennis
  # Features: Outlook, Temperature, Humidity, Wind
  # Target: Play Tennis (Yes/No)
  
  data = [
    { outlook: 'sunny', temperature: 'hot', humidity: 'high', wind: 'weak', play: 'no' },
    { outlook: 'sunny', temperature: 'hot', humidity: 'high', wind: 'strong', play: 'no' },
    { outlook: 'overcast', temperature: 'hot', humidity: 'high', wind: 'weak', play: 'yes' },
    { outlook: 'rain', temperature: 'mild', humidity: 'high', wind: 'weak', play: 'yes' },
    { outlook: 'rain', temperature: 'cool', humidity: 'normal', wind: 'weak', play: 'yes' },
    { outlook: 'rain', temperature: 'cool', humidity: 'normal', wind: 'strong', play: 'no' },
    { outlook: 'overcast', temperature: 'cool', humidity: 'normal', wind: 'strong', play: 'yes' },
    { outlook: 'sunny', temperature: 'mild', humidity: 'high', wind: 'weak', play: 'no' },
    { outlook: 'sunny', temperature: 'cool', humidity: 'normal', wind: 'weak', play: 'yes' },
    { outlook: 'rain', temperature: 'mild', humidity: 'normal', wind: 'weak', play: 'yes' },
    { outlook: 'sunny', temperature: 'mild', humidity: 'normal', wind: 'strong', play: 'yes' },
    { outlook: 'overcast', temperature: 'mild', humidity: 'high', wind: 'strong', play: 'yes' },
    { outlook: 'overcast', temperature: 'hot', humidity: 'normal', wind: 'weak', play: 'yes' },
    { outlook: 'rain', temperature: 'mild', humidity: 'high', wind: 'strong', play: 'no' }
  ]
  
  features = [:outlook, :temperature, :humidity, :wind]
  target_attribute = :play
  
  # Train the decision tree
  tree = ID3DecisionTree.new
  tree.train(data, features, target_attribute)
  
  # Make predictions
  test_instance = { outlook: 'rain', temperature: 'cool', humidity: 'high', wind: 'strong' }
  prediction = tree.predict(test_instance)
  
  puts "Prediction for #{test_instance}: #{prediction}"
  
  # Another test
  test_instance2 = { outlook: 'sunny', temperature: 'mild', humidity: 'high', wind: 'weak' }
  prediction2 = tree.predict(test_instance2)
  
  puts "Prediction for #{test_instance2}: #{prediction2}"
end
```

## Key Components Explained:

### 1. **Node Class**
- Represents each node in the decision tree
- Stores feature name, value, children, and label
- Tracks whether it's a leaf node

### 2. **ID3DecisionTree Class**
- **train()**: Main method to build the decision tree
- **predict()**: Makes predictions on new instances
- **build_tree()**: Recursively builds the tree
- **choose_best_feature()**: Selects feature with highest information gain
- **information_gain()**: Calculates information gain for a feature
- **entropy()**: Computes entropy of a dataset
- **most_common_value()**: Finds the most frequent target value

### 3. **Algorithm Steps**
1. **Base Cases**: 
   - All instances have same target value → create leaf
   - No features left → create leaf with most common target
2. **Feature Selection**: Choose feature with highest information gain
3. **Tree Building**: Create internal node and recursively build children
4. **Prediction**: Traverse tree based on instance feature values

### 4. **Example Output**
```
Prediction for {:outlook=>"rain", :temperature=>"cool", :humidity=>"high", :wind=>"strong"}: no
Prediction for {:outlook=>"sunny", :temperature=>"mild", :humidity=>"high", :wind=>"weak"}: no
```

This implementation follows the ID3 algorithm's core principles of using information gain to select features and building a decision tree that can make predictions on new data.

