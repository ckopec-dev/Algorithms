# CART Decision Tree Algorithm in Rust

Here's a complete implementation of a CART (Classification and Regression Trees) decision tree algorithm in Rust:

```rust
use std::collections::HashMap;
use std::fmt;

#[derive(Debug, Clone)]
pub enum NodeType {
    Leaf { prediction: f64 },
    Split { feature_index: usize, threshold: f64, left: Box<Node>, right: Box<Node> },
}

#[derive(Debug, Clone)]
pub struct Node {
    pub node_type: NodeType,
}

impl fmt::Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.node_type {
            NodeType::Leaf { prediction } => write!(f, "Leaf: {}", prediction),
            NodeType::Split { feature_index, threshold, .. } => {
                write!(f, "Split on feature {} <= {}", feature_index, threshold)
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct CART {
    pub root: Option<Node>,
    pub max_depth: usize,
    pub min_samples_split: usize,
}

impl CART {
    pub fn new(max_depth: usize, min_samples_split: usize) -> Self {
        CART {
            root: None,
            max_depth,
            min_samples_split,
        }
    }

    pub fn fit(&mut self, X: &Vec<Vec<f64>>, y: &Vec<f64>) {
        if X.is_empty() || y.is_empty() {
            return;
        }

        self.root = Some(self.build_tree(X, y, 0));
    }

    fn build_tree(&self, X: &Vec<Vec<f64>>, y: &Vec<f64>, depth: usize) -> Node {
        let n_samples = X.len();
        let n_features = X[0].len();

        // Stopping criteria
        if depth >= self.max_depth || n_samples < self.min_samples_split || self.is_pure(y) {
            let prediction = self.calculate_mean(y);
            return Node {
                node_type: NodeType::Leaf { prediction },
            };
        }

        // Find the best split
        let (best_feature, best_threshold) = self.find_best_split(X, y, n_features);

        // If no good split found, create leaf
        if best_feature.is_none() {
            let prediction = self.calculate_mean(y);
            return Node {
                node_type: NodeType::Leaf { prediction },
            };
        }

        let feature_index = best_feature.unwrap();
        let threshold = best_threshold;

        // Split the data
        let (left_X, left_y, right_X, right_y) = self.split_data(X, y, feature_index, threshold);

        // Recursively build left and right subtrees
        let left_node = self.build_tree(&left_X, &left_y, depth + 1);
        let right_node = self.build_tree(&right_X, &right_y, depth + 1);

        Node {
            node_type: NodeType::Split {
                feature_index,
                threshold,
                left: Box::new(left_node),
                right: Box::new(right_node),
            },
        }
    }

    fn find_best_split(&self, X: &Vec<Vec<f64>>, y: &Vec<f64>, n_features: usize) -> (Option<usize>, f64) {
        let mut best_gini = f64::MAX;
        let mut best_feature = None;
        let mut best_threshold = 0.0;

        for feature_index in 0..n_features {
            let mut feature_values: Vec<f64> = X.iter()
                .map(|row| row[feature_index])
                .collect();
            feature_values.sort_by(|a, b| a.partial_cmp(b).unwrap());

            // Try different thresholds
            for i in 1..feature_values.len() {
                let threshold = (feature_values[i - 1] + feature_values[i]) / 2.0;
                let (left_y, right_y) = self.split_y(y, X, feature_index, threshold);

                let gini = self.calculate_gini(&left_y, &right_y);
                if gini < best_gini {
                    best_gini = gini;
                    best_feature = Some(feature_index);
                    best_threshold = threshold;
                }
            }
        }

        (best_feature, best_threshold)
    }

    fn split_data(&self, X: &Vec<Vec<f64>>, y: &Vec<f64>, feature_index: usize, threshold: f64) -> (Vec<Vec<f64>>, Vec<f64>, Vec<Vec<f64>>, Vec<f64>) {
        let mut left_X = Vec::new();
        let mut left_y = Vec::new();
        let mut right_X = Vec::new();
        let mut right_y = Vec::new();

        for (i, row) in X.iter().enumerate() {
            if row[feature_index] <= threshold {
                left_X.push(row.clone());
                left_y.push(y[i]);
            } else {
                right_X.push(row.clone());
                right_y.push(y[i]);
            }
        }

        (left_X, left_y, right_X, right_y)
    }

    fn split_y(&self, y: &Vec<f64>, X: &Vec<Vec<f64>>, feature_index: usize, threshold: f64) -> (Vec<f64>, Vec<f64>) {
        let mut left_y = Vec::new();
        let mut right_y = Vec::new();

        for (i, row) in X.iter().enumerate() {
            if row[feature_index] <= threshold {
                left_y.push(y[i]);
            } else {
                right_y.push(y[i]);
            }
        }

        (left_y, right_y)
    }

    fn calculate_gini(&self, left_y: &Vec<f64>, right_y: &Vec<f64>) -> f64 {
        if left_y.is_empty() || right_y.is_empty() {
            return 0.0;
        }

        let total = left_y.len() + right_y.len();
        let left_weight = left_y.len() as f64 / total as f64;
        let right_weight = right_y.len() as f64 / total as f64;

        let gini_left = self.gini_impurity(left_y);
        let gini_right = self.gini_impurity(right_y);

        left_weight * gini_left + right_weight * gini_right
    }

    fn gini_impurity(&self, y: &Vec<f64>) -> f64 {
        if y.is_empty() {
            return 0.0;
        }

        let mut counts: HashMap<f64, usize> = HashMap::new();
        for &value in y {
            *counts.entry(value).or_insert(0) += 1;
        }

        let mut gini = 1.0;
        let n = y.len() as f64;

        for &count in counts.values() {
            let p = count as f64 / n;
            gini -= p * p;
        }

        gini
    }

    fn is_pure(&self, y: &Vec<f64>) -> bool {
        y.iter().all(|&x| x == y[0])
    }

    fn calculate_mean(&self, y: &Vec<f64>) -> f64 {
        if y.is_empty() {
            return 0.0;
        }
        y.iter().sum::<f64>() / y.len() as f64
    }

    pub fn predict(&self, X: &Vec<Vec<f64>>) -> Vec<f64> {
        X.iter()
            .map(|row| self.predict_single(row))
            .collect()
    }

    fn predict_single(&self, row: &Vec<f64>) -> f64 {
        match &self.root {
            Some(root_node) => self.predict_recursive(root_node, row),
            None => 0.0,
        }
    }

    fn predict_recursive(&self, node: &Node, row: &Vec<f64>) -> f64 {
        match &node.node_type {
            NodeType::Leaf { prediction } => *prediction,
            NodeType::Split { feature_index, threshold, left, right } => {
                if row[*feature_index] <= *threshold {
                    self.predict_recursive(left, row)
                } else {
                    self.predict_recursive(right, row)
                }
            }
        }
    }
}

// Example usage
fn main() {
    // Sample dataset: [feature1, feature2, feature3] -> target
    let X = vec![
        vec![2.0, 3.0, 1.0],
        vec![5.0, 4.0, 2.0],
        vec![1.0, 2.0, 3.0],
        vec![3.0, 1.0, 4.0],
        vec![4.0, 5.0, 5.0],
        vec![6.0, 6.0, 6.0],
    ];

    let y = vec![0.0, 1.0, 0.0, 0.0, 1.0, 1.0];

    // Create and train CART model
    let mut cart = CART::new(5, 2);
    cart.fit(&X, &y);

    // Make predictions
    let predictions = cart.predict(&X);
    
    println!("Predictions:");
    for (i, &pred) in predictions.iter().enumerate() {
        println!("Sample {}: predicted = {}, actual = {}", i, pred, y[i]);
    }

    // Print the tree structure
    println!("\nTree structure:");
    if let Some(root) = &cart.root {
        println!("{:?}", root);
    }
}
```

## Key Features of this Implementation:

### 1. **Core Components**
- `Node` struct representing tree nodes
- `CART` struct managing the decision tree
- Support for both splits and leaf nodes

### 2. **Key Algorithms**
- **Gini Impurity**: Used for measuring node purity
- **Best Split Selection**: Finds optimal feature and threshold
- **Recursive Tree Building**: Builds the tree structure

### 3. **Stopping Criteria**
- Maximum depth limit
- Minimum samples required to split
- Pure node detection (all samples have same target value)

### 4. **Prediction**
- Traverses the tree from root to leaf
- Returns the mean value for leaf nodes

### 5. **Usage Example**
The code includes a complete example with sample data, training, and prediction.

This implementation provides a solid foundation for decision tree algorithms and can be extended with additional features like pruning, handling missing values, or different splitting criteria.

