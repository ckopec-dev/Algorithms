# Naive Bayes Classifier in C++

Here's a complete implementation of a Naive Bayes classifier for text classification:

```cpp
#include <iostream>
#include <vector>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <sstream>
#include <algorithm>
#include <cmath>

class NaiveBayesClassifier {
private:
    // Class priors P(class)
    std::unordered_map<std::string, double> classPriors;
    
    // Word frequencies for each class P(word|class)
    std::unordered_map<std::string, std::unordered_map<std::string, int>> wordCounts;
    
    // Total word count for each class
    std::unordered_map<std::string, int> totalWordsPerClass;
    
    // Vocabulary size (used for smoothing)
    std::unordered_set<std::string> vocabulary;
    
    // Total documents per class
    std::unordered_map<std::string, int> classDocCount;
    
    // Number of documents in training set
    int totalDocuments;
    
    // Laplace smoothing parameter
    double alpha;
    
public:
    NaiveBayesClassifier(double smoothing = 1.0) : totalDocuments(0), alpha(smoothing) {}
    
    // Tokenize text into words
    std::vector<std::string> tokenize(const std::string& text) {
        std::vector<std::string> tokens;
        std::string word;
        std::stringstream ss(text);
        
        while (ss >> word) {
            // Convert to lowercase and remove punctuation
            std::transform(word.begin(), word.end(), word.begin(), ::tolower);
            word.erase(std::remove_if(word.begin(), word.end(), ::ispunct), word.end());
            
            if (!word.empty()) {
                tokens.push_back(word);
            }
        }
        return tokens;
    }
    
    // Train the classifier
    void train(const std::vector<std::pair<std::string, std::string>>& documents) {
        totalDocuments = documents.size();
        
        // Count documents per class
        for (const auto& doc : documents) {
            const std::string& className = doc.first;
            classDocCount[className]++;
        }
        
        // Calculate priors P(class)
        for (const auto& pair : classDocCount) {
            classPriors[pair.first] = static_cast<double>(pair.second) / totalDocuments;
        }
        
        // Process each document
        for (const auto& doc : documents) {
            const std::string& className = doc.first;
            const std::string& text = doc.second;
            
            std::vector<std::string> tokens = tokenize(text);
            
            // Count words in this class
            for (const std::string& word : tokens) {
                wordCounts[className][word]++;
                totalWordsPerClass[className]++;
                vocabulary.insert(word);
            }
        }
    }
    
    // Calculate probability P(word|class)
    double wordProbability(const std::string& word, const std::string& className) {
        int wordCount = wordCounts[className][word];
        int totalWords = totalWordsPerClass[className];
        int vocabSize = vocabulary.size();
        
        // Apply Laplace smoothing
        return static_cast<double>(wordCount + alpha) / 
               (totalWords + alpha * vocabSize);
    }
    
    // Classify a document
    std::string classify(const std::string& text) {
        std::vector<std::string> tokens = tokenize(text);
        
        std::unordered_map<std::string, double> classScores;
        
        // Calculate score for each class
        for (const auto& pair : classPriors) {
            const std::string& className = pair.first;
            double score = log(pair.second); // P(class)
            
            // Multiply by P(word|class) for each word
            for (const std::string& word : tokens) {
                double prob = wordProbability(word, className);
                score += log(prob);
            }
            
            classScores[className] = score;
        }
        
        // Return class with highest score
        auto maxIt = std::max_element(classScores.begin(), classScores.end(),
            [](const auto& a, const auto& b) {
                return a.second < b.second;
            });
        
        return maxIt->first;
    }
    
    // Get prediction probabilities for all classes
    std::unordered_map<std::string, double> predictProbabilities(const std::string& text) {
        std::vector<std::string> tokens = tokenize(text);
        std::unordered_map<std::string, double> classScores;
        
        for (const auto& pair : classPriors) {
            const std::string& className = pair.first;
            double score = log(pair.second);
            
            for (const std::string& word : tokens) {
                double prob = wordProbability(word, className);
                score += log(prob);
            }
            
            classScores[className] = score;
        }
        
        // Convert log probabilities to actual probabilities using softmax
        double maxScore = -1e9;
        for (const auto& pair : classScores) {
            maxScore = std::max(maxScore, pair.second);
        }
        
        double sum = 0.0;
        for (auto& pair : classScores) {
            pair.second = exp(pair.second - maxScore);
            sum += pair.second;
        }
        
        // Normalize
        for (auto& pair : classScores) {
            pair.second /= sum;
        }
        
        return classScores;
    }
};

// Example usage
int main() {
    // Create classifier
    NaiveBayesClassifier nb;
    
    // Training data: (class, document)
    std::vector<std::pair<std::string, std::string>> trainingData = {
        {"sports", "The football team won the championship game yesterday"},
        {"sports", "Basketball players practiced hard for the tournament"},
        {"sports", "The soccer match was exciting and intense"},
        {"sports", "Tennis players competed in the finals last week"},
        {"sports", "The baseball game was postponed due to rain"},
        {"technology", "New smartphone features include AI integration"},
        {"technology", "Software development requires strong programming skills"},
        {"technology", "Artificial intelligence is transforming industries"},
        {"technology", "Computer science students learn coding techniques"},
        {"technology", "Mobile apps are becoming more sophisticated"},
        {"politics", "Election results were announced yesterday evening"},
        {"politics", "Government policies affect citizens daily"},
        {"politics", "Political debates focus on economic issues"},
        {"politics", "Legislation passed in parliament this week"},
        {"politics", "Voting procedures are being reviewed by experts"}
    };
    
    // Train the classifier
    nb.train(trainingData);
    
    // Test classification
    std::vector<std::string> testDocuments = {
        "The football team played well today",
        "AI technology is advancing rapidly",
        "Political candidates debated last night"
    };
    
    std::cout << "Naive Bayes Classifier Results:\n";
    std::cout << "================================\n\n";
    
    for (const std::string& doc : testDocuments) {
        std::string prediction = nb.classify(doc);
        auto probabilities = nb.predictProbabilities(doc);
        
        std::cout << "Document: \"" << doc << "\"\n";
        std::cout << "Prediction: " << prediction << "\n";
        
        std::cout << "Probabilities:\n";
        for (const auto& prob : probabilities) {
            std::cout << "  " << prob.first << ": " << prob.second << "\n";
        }
        std::cout << "\n";
    }
    
    return 0;
}
```

## Key Features of this Implementation:

### 1. **Core Components:**
- Class priors (P(class))
- Word frequencies per class (P(word|class))
- Laplace smoothing for zero probability handling
- Tokenization with text preprocessing

### 2. **Key Methods:**
- `train()`: Builds the model from training data
- `classify()`: Predicts class for new documents
- `predictProbabilities()`: Returns confidence scores for all classes

### 3. **Features:**
- Text preprocessing (lowercase, punctuation removal)
- Laplace smoothing to handle unseen words
- Log probability calculations to prevent underflow
- Softmax conversion for probability interpretation

### 4. **Usage Example:**
The classifier can be trained on text documents with labels and then used to classify new, unseen text.

This implementation demonstrates the fundamental principles of Naive Bayes classification while handling practical considerations like smoothing and text processing.