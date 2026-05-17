# Gradient Boosting Machines (GBM) in Rust

Here's an example implementation of a Gradient Boosting Machine using Rust. This example demonstrates the core concepts of GBM with a simple regression implementation.

```rust
use std::collections::HashMap;

// Simple decision tree node structure
#[derive(Debug, Clone)]
struct TreeNode {
    feature_index: Option<usize>,
    threshold: Option<f64>,
    left: Option<Box<TreeNode>>,
    right: Option<Box<TreeNode>>,
    prediction: Option<f64>,
    is_leaf: bool,
}

impl TreeNode {
    fn new() -> Self {
        TreeNode {
            feature_index: None,
            threshold: None,
            left: None,
            right: None,
            prediction: None,
            is_leaf: true,
        }
    }

    fn new_leaf(prediction: f64) -> Self {
        TreeNode {
            feature_index: None,
            threshold: None,
            left: None,
            right: None,
            prediction: Some(prediction),
            is_leaf: true,
        }
    }

    fn new_split(feature_index: usize, threshold: f64) -> Self {
        TreeNode {
            feature_index: Some(feature_index),
            threshold: Some(threshold),
            left: None,
            right: None,
            prediction: None,
            is_leaf: false,
        }
    }
}

// Simple decision tree implementation
#[derive(Debug, Clone)]
struct DecisionTree {
    root: Option<TreeNode>,
    max_depth: usize,
    min_samples_split: usize,
}

impl DecisionTree {
    fn new(max_depth: usize, min_samples_split: usize) -> Self {
        DecisionTree {
            root: None,
            max_depth,
            min_samples_split,
        }
    }

    fn fit(&mut self, X: &Vec<Vec<f64>>, y: &Vec<f64>) {
        let root = self.build_tree(X, y, 0);
        self.root = Some(root);
    }

    fn build_tree(&self, X: &Vec<Vec<f64>>, y: &Vec<f64>, depth: usize) -> TreeNode {
        if depth >= self.max_depth || y.len() < self.min_samples_split {
            let prediction = y.iter().sum::<f64>() / y.len() as f64;
            return TreeNode::new_leaf(prediction);
        }

        // Find best split (simplified version)
        let best_split = self.find_best_split(X, y);
        
        if best_split.gain < 1e-6 {
            let prediction = y.iter().sum::<f64>() / y.len() as f64;
            return TreeNode::new_leaf(prediction);
        }

        let mut node = TreeNode::new_split(best_split.feature_index, best_split.threshold);
        
        let (left_indices, right_indices) = self.split_data(X, best_split.feature_index, best_split.threshold);
        
        let left_X: Vec<Vec<f64>> = left_indices.iter().map(|&i| X[i].clone()).collect();
        let left_y: Vec<f64> = left_indices.iter().map(|&i| y[i]).collect();
        
        let right_X: Vec<Vec<f64>> = right_indices.iter().map(|&i| X[i].clone()).collect();
        let right_y: Vec<f64> = right_indices.iter().map(|&i| y[i]).collect();
        
        node.left = Some(Box::new(self.build_tree(&left_X, &left_y, depth + 1)));
        node.right = Some(Box::new(self.build_tree(&right_X, &right_y, depth + 1)));
        
        node
    }

    fn find_best_split(&self, X: &Vec<Vec<f64>>, y: &Vec<f64>) -> Split {
        let mut best_gain = -1.0;
        let mut best_feature = 0;
        let mut best_threshold = 0.0;
        
        let n_features = X[0].len();
        let n_samples = X.len();
        
        for feature_idx in 0..n_features {
            let mut feature_values: Vec<f64> = X.iter().map(|row| row[feature_idx]).collect();
            feature_values.sort_by(|a, b| a.partial_cmp(b).unwrap());
            
            for i in 1..(n_samples - 1) {
                let threshold = (feature_values[i] + feature_values[i + 1]) / 2.0;
                
                let (left_indices, right_indices) = self.split_data_by_threshold(X, feature_idx, threshold);
                
                if left_indices.len() > 0 && right_indices.len() > 0 {
                    let gain = self.calculate_gini_gain(y, &left_indices, &right_indices);
                    if gain > best_gain {
                        best_gain = gain;
                        best_feature = feature_idx;
                        best_threshold = threshold;
                    }
                }
            }
        }
        
        Split {
            feature_index: best_feature,
            threshold: best_threshold,
            gain: best_gain,
        }
    }

    fn split_data(&self, X: &Vec<Vec<f64>>, feature_index: usize, threshold: f64) -> (Vec<usize>, Vec<usize>) {
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

    fn calculate_gini_gain(&self, y: &Vec<f64>, left_indices: &Vec<usize>, right_indices: &Vec<usize>) -> f64 {
        let n_total = y.len() as f64;
        let n_left = left_indices.len() as f64;
        let n_right = right_indices.len() as f64;
        
        if n_total == 0.0 || n_left == 0.0 || n_right == 0.0 {
            return 0.0;
        }
        
        let left_gini = self.calculate_gini(&y, left_indices);
        let right_gini = self.calculate_gini(&y, right_indices);
        
        let weighted_gini = (n_left / n_total) * left_gini + (n_right / n_total) * right_gini;
        let total_gini = self.calculate_gini(y, &(0..y.len()).collect::<Vec<usize>>());
        
        total_gini - weighted_gini
    }

    fn calculate_gini(&self, y: &Vec<f64>, indices: &Vec<usize>) -> f64 {
        let n = indices.len() as f64;
        if n == 0.0 {
            return 0.0;
        }
        
        let mut gini = 1.0;
        let mut class_counts: HashMap<f64, usize> = HashMap::new();
        
        for &idx in indices {
            *class_counts.entry(y[idx as usize]).or_insert(0) += 1;
        }
        
        for count in class_counts.values() {
            let p = *count as f64 / n;
            gini -= p * p;
        }
        
        gini
    }

    fn predict(&self, x: &Vec<f64>) -> f64 {
        match &self.root {
            Some(root) => self.predict_recursive(root, x),
            None => 0.0,
        }
    }

    fn predict_recursive(&self, node: &TreeNode, x: &Vec<f64>) -> f64 {
        if node.is_leaf {
            node.prediction.unwrap_or(0.0)
        } else {
            let feature_index = node.feature_index.unwrap();
            let threshold = node.threshold.unwrap();
            
            if x[feature_index] <= threshold {
                self.predict_recursive(node.left.as_ref().unwrap(), x)
            } else {
                self.predict_recursive(node.right.as_ref().unwrap(), x)
            }
        }
    }
}

// Gradient Boosting Machine implementation
#[derive(Debug, Clone)]
struct GradientBoostingRegressor {
    n_estimators: usize,
    learning_rate: f64,
    max_depth: usize,
    trees: Vec<DecisionTree>,
    initial_prediction: f64,
}

#[derive(Debug, Clone)]
struct Split {
    feature_index: usize,
    threshold: f64,
    gain: f64,
}

impl GradientBoostingRegressor {
    fn new(n_estimators: usize, learning_rate: f64, max_depth: usize) -> Self {
        GradientBoostingRegressor {
            n_estimators,
            learning_rate,
            max_depth,
            trees: Vec::new(),
            initial_prediction: 0.0,
        }
    }

    fn fit(&mut self, X: &Vec<Vec<f64>>, y: &Vec<f64>) {
        // Initialize with mean of target values
        self.initial_prediction = y.iter().sum::<f64>() / y.len() as f64;
        let mut predictions = vec![self.initial_prediction; y.len()];
        
        // Train trees sequentially
        for _ in 0..self.n_estimators {
            // Calculate residuals (negative gradient)
            let residuals: Vec<f64> = y.iter()
                .zip(predictions.iter())
                .map(|(&actual, &pred)| actual - pred)
                .collect();
            
            // Train a new tree on residuals
            let mut tree = DecisionTree::new(self.max_depth, 2);
            tree.fit(X, &residuals);
            
            // Add tree to ensemble
            self.trees.push(tree);
            
            // Update predictions
            for (i, tree) in self.trees.iter().enumerate() {
                for (j, x) in X.iter().enumerate() {
                    let prediction = tree.predict(x);
                    predictions[j] += self.learning_rate * prediction;
                }
            }
        }
    }

    fn predict(&self, X: &Vec<Vec<f64>>) -> Vec<f64> {
        let mut predictions = vec![self.initial_prediction; X.len()];
        
        for tree in &self.trees {
            for (i, x) in X.iter().enumerate() {
                let prediction = tree.predict(x);
                predictions[i] += self.learning_rate * prediction;
            }
        }
        
        predictions
    }
}

// Example usage
fn main() {
    // Sample dataset (simple 2D regression)
    let X = vec![
        vec![1.0, 2.0],
        vec![2.0, 3.0],
        vec![3.0, 4.0],
        vec![4.0, 5.0],
        vec![5.0, 6.0],
        vec![6.0, 7.0],
        vec![7.0, 8.0],
        vec![8.0, 9.0],
    ];
    
    let y = vec![3.0, 5.0, 7.0, 9.0, 11.0, 13.0, 15.0, 17.0];
    
    // Create and train GBM model
    let mut gbm = GradientBoostingRegressor::new(100, 0.1, 3);
    gbm.fit(&X, &y);
    
    // Make predictions
    let test_data = vec![
        vec![2.5, 3.5],
        vec![5.5, 6.5],
        vec![7.5, 8.5],
    ];
    
    let predictions = gbm.predict(&test_data);
    
    println!("GBM Predictions:");
    for (i, &pred) in predictions.iter().enumerate() {
        println!("Test sample {}: {:.2}", i + 1, pred);
    }
    
    // Print model information
    println!("\nModel Info:");
    println!("Number of trees: {}", gbm.trees.len());
    println!("Learning rate: {}", gbm.learning_rate);
    println!("Initial prediction: {:.2}", gbm.initial_prediction);
}
```

## Key Features of this GBM Implementation:

1. **Decision Tree**: Basic decision tree implementation with Gini impurity for splitting
2. **Gradient Boosting Framework**: Sequential tree building with residuals
3. **Learning Rate**: Controls the contribution of each tree
4. **Ensemble**: Combines multiple weak learners into a strong predictor

## How it Works:

1. **Initialization**: Start with the mean of target values
2. **Iterative Training**: 
   - Calculate residuals (actual - current predictions)
   - Train a new tree on residuals
   - Add tree to ensemble with learning rate
3. **Prediction**: Sum predictions from all trees

## Usage:

```bash
cargo run
```

This example provides a simplified but functional GBM implementation that demonstrates the core concepts. For production use, consider using established crates like `rustlearn` or `linfa` which provide more robust and optimized implementations.

