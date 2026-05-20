# Naïve Bayes Classifier in Lua

Here's a complete implementation of a Naïve Bayes classifier in Lua:

```lua
-- Naïve Bayes Classifier Implementation in Lua

-- Naïve Bayes Classifier Class
local NaiveBayes = {}
NaiveBayes.__index = NaiveBayes

function NaiveBayes:new()
    local obj = {
        class_counts = {},           -- Count of each class
        feature_counts = {},         -- Count of features for each class
        total_features = {},         -- Total features for each class
        total_documents = 0,         -- Total number of documents
        classes = {},                -- List of classes
        features = {}                -- List of all features
    }
    setmetatable(obj, NaiveBayes)
    return obj
end

-- Train the classifier with documents and their classes
function NaiveBayes:train(documents, classes)
    self.total_documents = #documents
    
    -- Initialize data structures
    for i, class in ipairs(classes) do
        if not self.class_counts[class] then
            self.class_counts[class] = 0
            self.feature_counts[class] = {}
            self.total_features[class] = 0
        end
        self.class_counts[class] = self.class_counts[class] + 1
    end
    
    -- Process each document
    for i, doc in ipairs(documents) do
        local class = classes[i]
        local features = doc
        
        -- Count features for this class
        for _, feature in ipairs(features) do
            if not self.feature_counts[class][feature] then
                self.feature_counts[class][feature] = 0
            end
            self.feature_counts[class][feature] = self.feature_counts[class][feature] + 1
            self.total_features[class] = self.total_features[class] + 1
            
            -- Keep track of all features
            if not self.features[feature] then
                self.features[feature] = true
            end
        end
    end
    
    -- Store classes
    self.classes = {}
    for class, _ in pairs(self.class_counts) do
        table.insert(self.classes, class)
    end
end

-- Calculate probability of a feature given a class
function NaiveBayes:feature_probability(feature, class)
    local feature_count = self.feature_counts[class][feature] or 0
    local total_features = self.total_features[class]
    
    -- Add-one smoothing (Laplace smoothing)
    return (feature_count + 1) / (total_features + #self.features)
end

-- Calculate prior probability of a class
function NaiveBayes:prior_probability(class)
    return self.class_counts[class] / self.total_documents
end

-- Predict the class for a given document
function NaiveBayes:predict(document)
    local best_class = nil
    local best_probability = -math.huge
    
    -- Calculate probability for each class
    for _, class in ipairs(self.classes) do
        local probability = math.log(self:prior_probability(class))
        
        -- Multiply by feature probabilities
        for _, feature in ipairs(document) do
            probability = probability + math.log(self:feature_probability(feature, class))
        end
        
        if probability > best_probability then
            best_probability = probability
            best_class = class
        end
    end
    
    return best_class
end

-- Calculate accuracy of the classifier
function NaiveBayes:accuracy(test_documents, test_classes)
    local correct = 0
    local total = #test_documents
    
    for i, doc in ipairs(test_documents) do
        local predicted = self:predict(doc)
        if predicted == test_classes[i] then
            correct = correct + 1
        end
    end
    
    return correct / total
end

-- Example usage
print("Naïve Bayes Classifier Example")

-- Create classifier instance
local nb = NaiveBayes:new()

-- Training data: documents and their classes
local training_documents = {
    {"sunny", "hot", "high", "weak"},
    {"sunny", "hot", "high", "strong"},
    {"overcast", "hot", "high", "weak"},
    {"rain", "mild", "high", "weak"},
    {"rain", "cool", "normal", "weak"},
    {"rain", "cool", "normal", "strong"},
    {"overcast", "cool", "normal", "strong"},
    {"sunny", "mild", "high", "weak"},
    {"sunny", "cool", "normal", "weak"},
    {"rain", "mild", "normal", "weak"},
    {"sunny", "mild", "normal", "strong"},
    {"overcast", "mild", "high", "strong"},
    {"overcast", "hot", "normal", "weak"},
    {"rain", "mild", "high", "strong"}
}

local training_classes = {
    "no", "no", "yes", "yes", "yes", "no", "yes", "no", "yes", "yes", "yes", "yes", "yes", "no"
}

-- Train the classifier
nb:train(training_documents, training_classes)

-- Test data
local test_documents = {
    {"sunny", "cool", "high", "strong"},
    {"rain", "mild", "normal", "weak"},
    {"overcast", "hot", "high", "strong"}
}

local test_classes = {"no", "yes", "yes"}

-- Make predictions
print("\nPredictions:")
for i, doc in ipairs(test_documents) do
    local prediction = nb:predict(doc)
    print("Document: " .. table.concat(doc, " ") .. " -> Predicted: " .. prediction)
end

-- Calculate accuracy
local accuracy = nb:accuracy(test_documents, test_classes)
print("\nAccuracy: " .. string.format("%.2f", accuracy * 100) .. "%")
```

## Key Features of this Implementation:

1. **Class Structure**: Uses Lua tables to create a proper class structure
2. **Training**: Processes documents and builds feature counts for each class
3. **Smoothing**: Implements Laplace smoothing to handle zero probabilities
4. **Log Probabilities**: Uses logarithms to prevent underflow
5. **Prediction**: Calculates probability for each class and selects the maximum
6. **Evaluation**: Includes accuracy calculation method

## How it Works:

1. **Training Phase**: Counts features for each class and calculates prior probabilities
2. **Prediction Phase**: For a new document, calculates the probability of each class using:
   - Prior probability of the class
   - Likelihood of each feature given the class (with smoothing)
3. **Decision**: Selects the class with the highest probability

This implementation demonstrates the core principles of Naïve Bayes classification while being fully functional in Lua.

