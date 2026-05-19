# Naïve Bayes Classifier in Rust

Here's a complete implementation of a Naïve Bayes classifier in Rust:

```rust
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct NaiveBayesClassifier {
    class_priors: HashMap<String, f64>,
    feature_likelihoods: HashMap<String, HashMap<String, HashMap<String, f64>>>,
    classes: Vec<String>,
    features: Vec<String>,
}

impl NaiveBayesClassifier {
    pub fn new() -> Self {
        NaiveBayesClassifier {
            class_priors: HashMap::new(),
            feature_likelihoods: HashMap::new(),
            classes: Vec::new(),
            features: Vec::new(),
        }
    }

    pub fn fit(&mut self, X: &[Vec<String>], y: &[String]) {
        let n_samples = X.len();
        
        // Calculate class priors
        let mut class_counts: HashMap<String, usize> = HashMap::new();
        for class in y {
            *class_counts.entry(class.clone()).or_insert(0) += 1;
        }
        
        for (class, count) in &class_counts {
            self.class_priors.insert(class.clone(), *count as f64 / n_samples as f64);
            self.classes.push(class.clone());
        }
        
        // Get all unique features
        let mut feature_set: Vec<String> = Vec::new();
        for sample in X {
            for feature in sample {
                if !feature_set.contains(feature) {
                    feature_set.push(feature.clone());
                }
            }
        }
        self.features = feature_set;
        
        // Calculate feature likelihoods for each class
        for class in &self.classes {
            let mut class_features: Vec<Vec<String>> = Vec::new();
            for (i, &label) in y.iter().enumerate() {
                if label == *class {
                    class_features.push(X[i].clone());
                }
            }
            
            // For each feature in this class
            for (feature_idx, feature_name) in self.features.iter().enumerate() {
                let mut feature_value_counts: HashMap<String, usize> = HashMap::new();
                
                // Count occurrences of each feature value in this class
                for sample in &class_features {
                    let feature_value = &sample[feature_idx];
                    *feature_value_counts.entry(feature_value.clone()).or_insert(0) += 1;
                }
                
                // Store likelihoods for this feature and class
                if !self.feature_likelihoods.contains_key(class) {
                    self.feature_likelihoods.insert(class.clone(), HashMap::new());
                }
                
                let class_likelihoods = self.feature_likelihoods.get_mut(class).unwrap();
                class_likelihoods.insert(feature_name.clone(), HashMap::new());
                
                let feature_likelihoods = class_likelihoods.get_mut(feature_name).unwrap();
                
                // Add Laplace smoothing to avoid zero probabilities
                let total_features_in_class = class_features.len() as f64;
                for (feature_value, count) in &feature_value_counts {
                    let probability = (*count as f64 + 1.0) / (total_features_in_class + 1.0);
                    feature_likelihoods.insert(feature_value.clone(), probability);
                }
            }
        }
    }

    pub fn predict(&self, X: &[Vec<String>]) -> Vec<String> {
        let mut predictions: Vec<String> = Vec::new();
        
        for sample in X {
            let mut best_class: Option<String> = None;
            let mut best_score = f64::NEG_INFINITY;
            
            for class in &self.classes {
                // Start with log prior probability
                let mut score = self.class_priors.get(class).unwrap().log2();
                
                // Multiply by log likelihoods for each feature
                for (feature_idx, feature_value) in sample.iter().enumerate() {
                    let feature_name = &self.features[feature_idx];
                    
                    if let Some(class_likelihoods) = self.feature_likelihoods.get(class) {
                        if let Some(feature_likelihoods) = class_likelihoods.get(feature_name) {
                            if let Some(&likelihood) = feature_likelihoods.get(feature_value) {
                                score += likelihood.log2();
                            }
                        }
                    }
                }
                
                if score > best_score {
                    best_score = score;
                    best_class = Some(class.clone());
                }
            }
            
            predictions.push(best_class.unwrap());
        }
        
        predictions
    }
}

// Example usage
fn main() {
    // Sample dataset: [outlook, temperature, humidity, windy]
    let X = vec![
        vec!["sunny".to_string(), "hot".to_string(), "high".to_string(), "false".to_string()],
        vec!["sunny".to_string(), "hot".to_string(), "high".to_string(), "true".to_string()],
        vec!["overcast".to_string(), "hot".to_string(), "high".to_string(), "false".to_string()],
        vec!["rain".to_string(), "mild".to_string(), "high".to_string(), "false".to_string()],
        vec!["rain".to_string(), "cool".to_string(), "normal".to_string(), "false".to_string()],
        vec!["rain".to_string(), "cool".to_string(), "normal".to_string(), "true".to_string()],
        vec!["overcast".to_string(), "cool".to_string(), "normal".to_string(), "true".to_string()],
        vec!["sunny".to_string(), "mild".to_string(), "high".to_string(), "false".to_string()],
        vec!["sunny".to_string(), "cool".to_string(), "normal".to_string(), "false".to_string()],
        vec!["rain".to_string(), "mild".to_string(), "normal".to_string(), "false".to_string()],
        vec!["sunny".to_string(), "mild".to_string(), "normal".to_string(), "true".to_string()],
        vec!["overcast".to_string(), "mild".to_string(), "high".to_string(), "true".to_string()],
        vec!["overcast".to_string(), "hot".to_string(), "normal".to_string(), "false".to_string()],
        vec!["rain".to_string(), "mild".to_string(), "high".to_string(), "true".to_string()],
    ];
    
    let y = vec![
        "no".to_string(),
        "no".to_string(),
        "yes".to_string(),
        "yes".to_string(),
        "yes".to_string(),
        "no".to_string(),
        "yes".to_string(),
        "no".to_string(),
        "yes".to_string(),
        "yes".to_string(),
        "yes".to_string(),
        "yes".to_string(),
        "yes".to_string(),
        "no".to_string(),
    ];
    
    // Create and train the classifier
    let mut classifier = NaiveBayesClassifier::new();
    classifier.fit(&X, &y);
    
    // Make predictions
    let test_samples = vec![
        vec!["sunny".to_string(), "cool".to_string(), "high".to_string(), "false".to_string()],
        vec!["overcast".to_string(), "hot".to_string(), "normal".to_string(), "false".to_string()],
    ];
    
    let predictions = classifier.predict(&test_samples);
    
    println!("Predictions:");
    for (i, prediction) in predictions.iter().enumerate() {
        println!("Sample {}: {}", i + 1, prediction);
    }
    
    println!("\nClassifier details:");
    println!("Classes: {:?}", classifier.classes);
    println!("Features: {:?}", classifier.features);
    println!("Class priors: {:?}", classifier.class_priors);
}
```

## Key Features of this Implementation:

1. **Generic Structure**: The classifier handles string features and classes
2. **Laplace Smoothing**: Prevents zero probabilities by adding 1 to all counts
3. **Log Probabilities**: Uses logarithms to prevent numerical underflow
4. **Complete Training**: Calculates priors and likelihoods from training data
5. **Prediction**: Makes predictions for new samples using Bayes' theorem

## How it Works:

1. **Training Phase**: 
   - Calculate class priors (P(class))
   - Calculate feature likelihoods P(feature|class) for each class

2. **Prediction Phase**:
   - For each sample, calculate P(class|features) ∝ P(class) × ∏P(feature|class)
   - Choose the class with highest probability

3. **Mathematical Foundation**:
   - P(class|features) = P(features|class) × P(class) / P(features)
   - Using log probabilities for numerical stability

This implementation provides a solid foundation that can be extended with additional features like continuous feature handling or different smoothing techniques.

