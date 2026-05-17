# Decision Tree Learning Algorithm in Rust

Here's a complete implementation of a decision tree learning algorithm in Rust:

```rust
use std::collections::HashMap;
use std::fmt;

#[derive(Debug, Clone)]
pub enum AttributeValue {
    String(String),
    Number(f64),
}

#[derive(Debug, Clone)]
pub struct Example {
    pub attributes: HashMap<String, AttributeValue>,
    pub class: String,
}

#[derive(Debug, Clone)]
pub enum Node {
    Leaf { class: String },
    Decision { attribute: String, branches: HashMap<AttributeValue, Box<Node>> },
}

impl fmt::Display for AttributeValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            AttributeValue::String(s) => write!(f, "{}", s),
            AttributeValue::Number(n) => write!(f, "{}", n),
        }
    }
}

impl Node {
    fn new_leaf(class: String) -> Self {
        Node::Leaf { class }
    }

    fn new_decision(attribute: String, branches: HashMap<AttributeValue, Box<Node>>) -> Self {
        Node::Decision { attribute, branches }
    }
}

pub struct DecisionTree {
    pub root: Option<Node>,
    pub attributes: Vec<String>,
}

impl DecisionTree {
    pub fn new(attributes: Vec<String>) -> Self {
        DecisionTree {
            root: None,
            attributes,
        }
    }

    pub fn train(&mut self, examples: &[Example]) {
        if examples.is_empty() {
            self.root = Some(Node::new_leaf("unknown".to_string()));
            return;
        }

        let classes = examples.iter().map(|e| e.class.clone()).collect::<Vec<_>>();
        if classes.iter().all(|c| c == &classes[0]) {
            self.root = Some(Node::new_leaf(classes[0].clone()));
            return;
        }

        if self.attributes.is_empty() {
            let most_common_class = self.get_most_common_class(examples);
            self.root = Some(Node::new_leaf(most_common_class));
            return;
        }

        let best_attribute = self.find_best_attribute(examples);
        let mut branches: HashMap<AttributeValue, Vec<Example>> = HashMap::new();

        for example in examples {
            let value = example.attributes.get(&best_attribute).unwrap().clone();
            branches.entry(value).or_insert_with(Vec::new).push(example.clone());
        }

        let mut tree = Node::new_decision(best_attribute.clone(), HashMap::new());
        if let Node::Decision { branches: ref mut branch_map, .. } = tree {
            for (value, subset) in branches {
                let mut subtree = DecisionTree::new(self.attributes.clone());
                subtree.attributes.retain(|attr| attr != &best_attribute);
                subtree.train(&subset);
                branch_map.insert(value, Box::new(subtree.root.unwrap()));
            }
        }

        self.root = Some(tree);
    }

    fn find_best_attribute(&self, examples: &[Example]) -> String {
        let mut best_gain = -1.0;
        let mut best_attribute = String::new();

        for attribute in &self.attributes {
            let gain = self.calculate_information_gain(examples, attribute);
            if gain > best_gain {
                best_gain = gain;
                best_attribute = attribute.clone();
            }
        }

        best_attribute
    }

    fn calculate_information_gain(&self, examples: &[Example], attribute: &str) -> f64 {
        let total_entropy = self.calculate_entropy(examples);
        let weighted_entropy = self.calculate_weighted_entropy(examples, attribute);
        total_entropy - weighted_entropy
    }

    fn calculate_entropy(&self, examples: &[Example]) -> f64 {
        let mut class_counts: HashMap<String, usize> = HashMap::new();
        let total = examples.len() as f64;

        for example in examples {
            *class_counts.entry(example.class.clone()).or_insert(0) += 1;
        }

        let mut entropy = 0.0;
        for count in class_counts.values() {
            let probability = *count as f64 / total;
            if probability > 0.0 {
                entropy -= probability * probability.log2();
            }
        }

        entropy
    }

    fn calculate_weighted_entropy(&self, examples: &[Example], attribute: &str) -> f64 {
        let mut value_counts: HashMap<AttributeValue, usize> = HashMap::new();
        let total = examples.len() as f64;

        for example in examples {
            let value = example.attributes.get(attribute).unwrap().clone();
            *value_counts.entry(value).or_insert(0) += 1;
        }

        let mut weighted_entropy = 0.0;
        for (value, count) in value_counts {
            let subset: Vec<Example> = examples
                .iter()
                .filter(|e| e.attributes.get(attribute).unwrap() == &value)
                .cloned()
                .collect();

            let probability = count as f64 / total;
            let subset_entropy = self.calculate_entropy(&subset);
            weighted_entropy += probability * subset_entropy;
        }

        weighted_entropy
    }

    fn get_most_common_class(&self, examples: &[Example]) -> String {
        let mut class_counts: HashMap<String, usize> = HashMap::new();

        for example in examples {
            *class_counts.entry(example.class.clone()).or_insert(0) += 1;
        }

        class_counts
            .into_iter()
            .max_by_key(|&(_, count)| count)
            .map(|(class, _)| class)
            .unwrap_or_else(|| "unknown".to_string())
    }

    pub fn predict(&self, example: &Example) -> String {
        match &self.root {
            Some(Node::Leaf { class }) => class.clone(),
            Some(Node::Decision { attribute, branches }) => {
                if let Some(value) = example.attributes.get(attribute) {
                    if let Some(subtree) = branches.get(value) {
                        return subtree.predict(example);
                    }
                }
                "unknown".to_string()
            }
            None => "unknown".to_string(),
        }
    }
}

// Example usage
fn main() {
    // Create sample data
    let mut examples = vec![
        Example {
            attributes: HashMap::from([
                ("outlook".to_string(), AttributeValue::String("sunny".to_string())),
                ("temperature".to_string(), AttributeValue::Number(85.0)),
                ("humidity".to_string(), AttributeValue::Number(85.0)),
                ("windy".to_string(), AttributeValue::String("true".to_string())),
            ]),
            class: "no".to_string(),
        },
        Example {
            attributes: HashMap::from([
                ("outlook".to_string(), AttributeValue::String("sunny".to_string())),
                ("temperature".to_string(), AttributeValue::Number(80.0)),
                ("humidity".to_string(), AttributeValue::Number(90.0)),
                ("windy".to_string(), AttributeValue::String("true".to_string())),
            ]),
            class: "no".to_string(),
        },
        Example {
            attributes: HashMap::from([
                ("outlook".to_string(), AttributeValue::String("overcast".to_string())),
                ("temperature".to_string(), AttributeValue::Number(83.0)),
                ("humidity".to_string(), AttributeValue::Number(78.0)),
                ("windy".to_string(), AttributeValue::String("false".to_string())),
            ]),
            class: "yes".to_string(),
        },
        Example {
            attributes: HashMap::from([
                ("outlook".to_string(), AttributeValue::String("rain".to_string())),
                ("temperature".to_string(), AttributeValue::Number(70.0)),
                ("humidity".to_string(), AttributeValue::Number(96.0)),
                ("windy".to_string(), AttributeValue::String("true".to_string())),
            ]),
            class: "no".to_string(),
        },
        Example {
            attributes: HashMap::from([
                ("outlook".to_string(), AttributeValue::String("rain".to_string())),
                ("temperature".to_string(), AttributeValue::Number(68.0)),
                ("humidity".to_string(), AttributeValue::Number(80.0)),
                ("windy".to_string(), AttributeValue::String("false".to_string())),
            ]),
            class: "yes".to_string(),
        },
    ];

    // Create and train the decision tree
    let attributes = vec![
        "outlook".to_string(),
        "temperature".to_string(),
        "humidity".to_string(),
        "windy".to_string(),
    ];

    let mut tree = DecisionTree::new(attributes);
    tree.train(&examples);

    // Test prediction
    let test_example = Example {
        attributes: HashMap::from([
            ("outlook".to_string(), AttributeValue::String("sunny".to_string())),
            ("temperature".to_string(), AttributeValue::Number(75.0)),
            ("humidity".to_string(), AttributeValue::Number(80.0)),
            ("windy".to_string(), AttributeValue::String("false".to_string())),
        ]),
        class: "unknown".to_string(),
    };

    let prediction = tree.predict(&test_example);
    println!("Prediction for test example: {}", prediction);
}
```

## Key Features of this Implementation

### 1. **Data Structures**
- `AttributeValue`: Enum to handle both string and numeric attribute values
- `Example`: Represents a training instance with attributes and class label
- `Node`: Tree node that can be either a leaf or decision node

### 2. **Core Algorithms**
- **Information Gain Calculation**: Uses entropy to determine the best attribute for splitting
- **Recursive Tree Building**: Builds the tree by recursively splitting on the best attributes
- **Prediction**: Traverses the tree to make predictions on new examples

### 3. **Key Methods**
- `train()`: Main training method that builds the decision tree
- `find_best_attribute()`: Selects the attribute with highest information gain
- `calculate_information_gain()`: Computes the information gain for an attribute
- `predict()`: Makes predictions on new examples

### 4. **Example Usage**
The code includes a complete example with weather prediction data, showing how to:
1. Create training examples
2. Initialize and train the decision tree
3. Make predictions on new data

This implementation provides a solid foundation for decision tree learning that can be extended with additional features like pruning, handling missing values, or different splitting criteria.

