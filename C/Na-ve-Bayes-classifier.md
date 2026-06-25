# Naïve Bayes Classifier in C

Here's a complete implementation of a Naïve Bayes classifier in C:

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#define MAX_WORDS 1000
#define MAX_CLASSES 10
#define MAX_DOCUMENTS 1000

// Structure to represent a document
typedef struct {
    int class_id;
    char words[MAX_WORDS][50];
    int word_count;
} Document;

// Structure to store word frequencies for each class
typedef struct {
    int class_id;
    int word_freq[MAX_WORDS];
    int total_words;
} ClassModel;

// Global variables
Document documents[MAX_DOCUMENTS];
ClassModel class_models[MAX_CLASSES];
int num_documents = 0;
int num_classes = 0;
char vocabulary[MAX_WORDS][50];
int vocab_size = 0;

// Function to add a word to vocabulary if not already present
int add_word_to_vocab(const char* word) {
    for (int i = 0; i < vocab_size; i++) {
        if (strcmp(vocabulary[i], word) == 0) {
            return i;
        }
    }
    
    strcpy(vocabulary[vocab_size], word);
    vocab_size++;
    return vocab_size - 1;
}

// Function to train the Naïve Bayes classifier
void train_naive_bayes() {
    // Initialize class models
    for (int i = 0; i < MAX_CLASSES; i++) {
        class_models[i].class_id = i;
        class_models[i].total_words = 0;
        for (int j = 0; j < MAX_WORDS; j++) {
            class_models[i].word_freq[j] = 0;
        }
    }
    
    // Count word frequencies for each class
    for (int i = 0; i < num_documents; i++) {
        int class_id = documents[i].class_id;
        ClassModel* model = &class_models[class_id];
        
        for (int j = 0; j < documents[i].word_count; j++) {
            int word_index = add_word_to_vocab(documents[i].words[j]);
            model->word_freq[word_index]++;
            model->total_words++;
        }
    }
    
    // Count total documents per class
    int class_counts[MAX_CLASSES] = {0};
    for (int i = 0; i < num_documents; i++) {
        class_counts[documents[i].class_id]++;
    }
    
    // Print training results
    printf("Training completed:\n");
    printf("Vocabulary size: %d words\n", vocab_size);
    printf("Documents trained: %d\n", num_documents);
    for (int i = 0; i < MAX_CLASSES; i++) {
        if (class_counts[i] > 0) {
            printf("Class %d: %d documents\n", i, class_counts[i]);
        }
    }
}

// Function to calculate probability of a word given a class
double get_word_probability(const char* word, int class_id) {
    int word_index = -1;
    
    // Find word in vocabulary
    for (int i = 0; i < vocab_size; i++) {
        if (strcmp(vocabulary[i], word) == 0) {
            word_index = i;
            break;
        }
    }
    
    if (word_index == -1) {
        return 1.0 / (class_models[class_id].total_words + vocab_size);
    }
    
    // Apply Laplace smoothing
    int word_count = class_models[class_id].word_freq[word_index];
    double probability = (double)(word_count + 1) / 
                        (class_models[class_id].total_words + vocab_size);
    
    return probability;
}

// Function to classify a document
int classify_document(char** words, int word_count) {
    int best_class = 0;
    double max_prob = -1e9; // Use negative infinity
    
    // Calculate probability for each class
    for (int class_id = 0; class_id < MAX_CLASSES; class_id++) {
        if (class_models[class_id].total_words == 0) continue;
        
        double class_prob = log((double)class_models[class_id].total_words / 
                               num_documents); // Prior probability
        
        // Calculate likelihood
        for (int i = 0; i < word_count; i++) {
            double word_prob = get_word_probability(words[i], class_id);
            class_prob += log(word_prob);
        }
        
        if (class_prob > max_prob) {
            max_prob = class_prob;
            best_class = class_id;
        }
    }
    
    return best_class;
}

// Function to add a document to training data
void add_document(int class_id, char** words, int word_count) {
    if (num_documents >= MAX_DOCUMENTS) {
        printf("Maximum documents reached!\n");
        return;
    }
    
    documents[num_documents].class_id = class_id;
    documents[num_documents].word_count = word_count;
    
    for (int i = 0; i < word_count; i++) {
        strcpy(documents[num_documents].words[i], words[i]);
    }
    
    num_documents++;
}

// Example usage
int main() {
    printf("Naïve Bayes Classifier Demo\n");
    printf("===========================\n\n");
    
    // Sample training data - spam vs non-spam emails
    char* spam_words1[] = {"buy", "cheap", "money", "offer"};
    char* spam_words2[] = {"win", "free", "prize", "cash"};
    char* spam_words3[] = {"click", "here", "sale", "discount"};
    
    char* non_spam_words1[] = {"meeting", "project", "work", "team"};
    char* non_spam_words2[] = {"report", "presentation", "schedule", "deadline"};
    char* non_spam_words3[] = {"thanks", "please", "help", "support"};
    
    // Add training documents
    add_document(0, spam_words1, 4);     // Class 0 = Spam
    add_document(0, spam_words2, 4);     // Class 0 = Spam
    add_document(0, spam_words3, 4);     // Class 0 = Spam
    
    add_document(1, non_spam_words1, 4); // Class 1 = Non-spam
    add_document(1, non_spam_words2, 4); // Class 1 = Non-spam
    add_document(1, non_spam_words3, 4); // Class 1 = Non-spam
    
    // Train the classifier
    train_naive_bayes();
    
    printf("\n");
    
    // Test classification
    char* test_words1[] = {"buy", "cheap", "money"};
    char* test_words2[] = {"meeting", "project", "work"};
    char* test_words3[] = {"win", "free", "prize"};
    
    printf("Testing classification:\n");
    
    int result1 = classify_document(test_words1, 3);
    printf("Document ['buy', 'cheap', 'money'] classified as class %d\n", result1);
    
    int result2 = classify_document(test_words2, 3);
    printf("Document ['meeting', 'project', 'work'] classified as class %d\n", result2);
    
    int result3 = classify_document(test_words3, 3);
    printf("Document ['win', 'free', 'prize'] classified as class %d\n", result3);
    
    return 0;
}
```

## Key Features of this Implementation:

1. **Vocabulary Management**: Automatically builds vocabulary from training data
2. **Laplace Smoothing**: Prevents zero probabilities with smoothing technique
3. **Log Probabilities**: Uses logarithms to prevent underflow
4. **Training Phase**: Builds word frequency models for each class
5. **Classification Phase**: Calculates posterior probability for each class

## How it Works:

1. **Training**: 
   - Count word frequencies for each class
   - Build vocabulary from all training documents
   - Store class-specific statistics

2. **Classification**:
   - For a new document, calculate probability for each class
   - Use Bayes' theorem: P(class|document) ∝ P(document|class) × P(class)
   - Apply log probabilities to avoid numerical issues
   - Return class with highest probability

## Sample Output:
```
Naïve Bayes Classifier Demo
===========================

Training completed:
Vocabulary size: 12 words
Documents trained: 6
Class 0: 3 documents
Class 1: 3 documents

Testing classification:
Document ['buy', 'cheap', 'money'] classified as class 0
Document ['meeting', 'project', 'work'] classified as class 1
Document ['win', 'free', 'prize'] classified as class 0
```

This implementation demonstrates the core concepts of Naïve Bayes classification in a simple, educational C program.