# CART Decision Tree Implementation in Rust

Here's a complete implementation of a Classification and Regression Tree (CART) algorithm in Rust:

```rust
use std::collections::HashMap;
use std::cmp::Ordering;

#[derive(Debug, Clone)]
pub struct Node {
    pub feature_index: Option<usize>,
    pub threshold: Option<f64>,
    pub left: Option<Box<Node>>,
    pub right: Option<Box<Node>>,
    pub prediction: Option<f64>, // For leaf nodes
    pub is_leaf: bool,
}

impl Node {
    pub fn new() -> Self {
        Node {
            feature_index: None,
            threshold: None,
            left: None,
            right: None,
            prediction: None,
            is_leaf: false,
        }
    }

    pub fn new_leaf(prediction: f64) -> Self {
        Node {
            feature_index: None,
            threshold: None,
            left: None,
            right: None,
            prediction: Some(prediction),
            is_leaf: true,
        }
    }
}

#[derive(Debug)]
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
        self.root = Some(self.build_tree(X, y, 0));
    }

    fn build_tree(&self, X: &Vec<Vec<f64>>, y: &Vec<f64>, depth: usize) -> Node {
        let n_samples = X.len();
        let n_features = X[0].len();

        // Stopping criteria
        if depth >= self.max_depth || n_samples < self.min_samples_split || self.is_pure(y) {
            let prediction = self.calculate_mean(y);
            return Node::new_leaf(prediction);
        }

        // Find the best split
        let (best_feature, best_threshold) = self.find_best_split(X, y);

        if best_feature.is_none() {
            let prediction = self.calculate_mean(y);
            return Node::new_leaf(prediction);
        }

        let feature_index = best_feature.unwrap();
        let threshold = best_threshold.unwrap();

        // Split the data
        let (left_indices, right_indices) = self.split_data(X, feature_index, threshold);

        // Create left and right subtrees
        let left_X = self.get_subset(X, &left_indices);
        let left_y = self.get_subset(y, &left_indices);
        let right_X = self.get_subset(X, &right_indices);
        let right_y = self.get_subset(y, &right_indices);

        let mut node = Node::new();
        node.feature_index = Some(feature_index);
        node.threshold = Some(threshold);

        node.left = Some(Box::new(self.build_tree(&left_X, &left_y, depth + 1)));
        node.right = Some(Box::new(self.build_tree(&right_X, &right_y, depth + 1)));

        node
    }

    fn find_best_split(&self, X: &Vec<Vec<f64>>, y: &Vec<f64>) -> (Option<usize>, Option<f64>) {
        let mut best_gini = f64::INFINITY;
        let mut best_feature = None;
        let mut best_threshold = None;

        let n_features = X[0].len();
        let n_samples = X.len();

        for feature_index in 0..n_features {
            let mut feature_values = Vec::new();
            for i in 0..n_samples {
                feature_values.push(X[i][feature_index]);
            }
            feature_values.sort_by(|a, b| a.partial_cmp(b).unwrap_or(Ordering::Equal));

            // Try different thresholds
            for i in 0..feature_values.len() - 1 {
                let threshold = (feature_values[i] + feature_values[i + 1]) / 2.0;
                let (left_indices, right_indices) = self.split_data_by_threshold(X, feature_index, threshold);

                if !left_indices.is_empty() && !right_indices.is_empty() {
                    let gini = self.calculate_weighted_gini(y, &left_indices, &right_indices);
                    if gini < best_gini {
                        best_gini = gini;
                        best_feature = Some(feature_index);
                        best_threshold = Some(threshold);
                    }
                }
            }
        }

        (best_feature, best_threshold)
    }

    fn split_data_by_threshold(&self, X: &Vec<Vec<f64>>, feature_index: usize, threshold: f64) -> (Vec<usize>, Vec<usize>) {
        let mut left_indices = Vec::new();
        let mut right_indices = Vec::new();

        for (i, row) in X.iter().enumerate() {
            if row[feature_index] <= threshold {
                left_indices.push(i);
            } else {
                right_indices.push(i);
            }
        }

        (left_indices, right_indices)
    }

    fn split_data(&self, X: &Vec<Vec<f64>>, feature_index: usize, threshold: f64) -> (Vec<usize>, Vec<usize>) {
        self.split_data_by_threshold(X, feature_index, threshold)
    }

    fn calculate_weighted_gini(&self, y: &Vec<f64>, left_indices: &Vec<usize>, right_indices: &Vec<usize>) -> f64 {
        let n_total = y.len() as f64;
        let n_left = left_indices.len() as f64;
        let n_right = right_indices.len() as f64;

        let left_gini = self.calculate_gini(y, left_indices);
        let right_gini = self.calculate_gini(y, right_indices);

        (n_left / n_total) * left_gini + (n_right / n_total) * right_gini
    }

    fn calculate_gini(&self, y: &Vec<f64>, indices: &Vec<usize>) -> f64 {
        if indices.is_empty() {
            return 0.0;
        }

        let mut class_counts: HashMap<f64, usize> = HashMap::new();
        for &idx in indices {
            *class_counts.entry(y[idx as usize]).or_insert(0) += 1;
        }

        let n_samples = indices.len() as f64;
        let mut gini = 1.0;

        for &count in class_counts.values() {
            let probability = count as f64 / n_samples;
            gini -= probability * probability;
        }

        gini
    }

    fn calculate_mean(&self, y: &Vec<f64>) -> f64 {
        if y.is_empty() {
            return 0.0;
        }
        let sum: f64 = y.iter().sum();
        sum / y.len() as f64
    }

    fn is_pure(&self, y: &Vec<f64>) -> bool {
        if y.is_empty() {
            return true;
        }
        let first = y[0];
        y.iter().all(|&val| val == first)
    }

    fn get_subset<T>(&self, data: &Vec<T>, indices: &Vec<usize>) -> Vec<T> 
    where 
        T: Clone 
    {
        indices.iter().map(|&i| data[i].clone()).collect()
    }

    pub fn predict(&self, X: &Vec<Vec<f64>>) -> Vec<f64> {
        X.iter().map(|row| self.predict_single(row)).collect()
    }

    fn predict_single(&self, x: &Vec<f64>) -> f64 {
        match &self.root {
            Some(root_node) => self.predict_recursive(root_node, x),
            None => 0.0,
        }
    }

    fn predict_recursive(&self, node: &Node, x: &Vec<f64>) -> f64 {
        if node.is_leaf {
            return node.prediction.unwrap();
        }

        let feature_index = node.feature_index.unwrap();
        let threshold = node.threshold.unwrap();

        if x[feature_index] <= threshold {
            match &node.left {
                Some(left_child) => self.predict_recursive(left_child, x),
                None => 0.0,
            }
        } else {
            match &node.right {
                Some(right_child) => self.predict_recursive(right_child, x),
                None => 0.0,
            }
        }
    }
}

// Example usage
fn main() {
    // Sample dataset (features: [age, income], target: [0=low, 1=high])
    let X = vec![
        vec![25.0, 50000.0],
        vec![35.0, 80000.0],
        vec![45.0, 120000.0],
        vec![23.0, 40000.0],
        vec![33.0, 70000.0],
        vec![55.0, 150000.0],
        vec![28.0, 60000.0],
        vec![41.0, 100000.0],
    ];

    let y = vec![0.0, 1.0, 1.0, 0.0, 1.0, 1.0, 0.0, 1.0];

    // Create and train the CART model
    let mut cart = CART::new(5, 2);
    cart.fit(&X, &y);

    // Make predictions
    let test_data = vec![
        vec![30.0, 65000.0],
        vec![50.0, 110000.0],
    ];

    let predictions = cart.predict(&test_data);
    
    println!("Predictions:");
    for (i, &prediction) in predictions.iter().enumerate() {
        println!("Sample {}: {:.2}", i + 1, prediction);
    }
}
```

## Key Features of this Implementation

### 1. **Core Structure**
- `Node` struct representing tree nodes with feature index, threshold, and child pointers
- `CART` struct containing the tree and training parameters

### 2. **Key Methods**
- `fit()`: Trains the decision tree on the dataset
- `build_tree()`: Recursively builds the tree structure
- `find_best_split()`: Finds optimal feature and threshold for splitting
- `predict()`: Makes predictions on new data

### 3. **CART Algorithm Features**
- **Binary splits**: Each node splits into exactly two children
- **Gini impurity**: Used for measuring split quality
- **Stopping criteria**: Maximum depth and minimum samples per leaf
- **Recursive construction**: Builds tree bottom-up

### 4. **Usage Example**
The example demonstrates training on a simple dataset with age and income features to predict low/high categories, then making predictions on new samples.

This implementation provides a solid foundation for a CART decision tree that can be extended with additional features like pruning, handling of missing values, or different splitting criteria.

