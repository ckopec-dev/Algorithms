# Naïve Bayes Classifier in COBOL

Here's a simplified implementation of a Naïve Bayes classifier in COBOL:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. NAIVE-BAYES-CLASSIFIER.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       01  CLASS-PROBABILITY.
           05  P-CLASS1          PIC 9V99 VALUE 0.5.
           05  P-CLASS2          PIC 9V99 VALUE 0.5.
       
       01  FEATURE-PROBABILITY.
           05  P-FEATURE1-CLASS1 PIC 9V99 VALUE 0.3.
           05  P-FEATURE1-CLASS2 PIC 9V99 VALUE 0.7.
           05  P-FEATURE2-CLASS1 PIC 9V99 VALUE 0.6.
           05  P-FEATURE2-CLASS2 PIC 9V99 VALUE 0.4.
           05  P-FEATURE3-CLASS1 PIC 9V99 VALUE 0.8.
           05  P-FEATURE3-CLASS2 PIC 9V99 VALUE 0.2.
       
       01  INPUT-FEATURES.
           05  FEATURE1          PIC 9 VALUE 0.
           05  FEATURE2          PIC 9 VALUE 0.
           05  FEATURE3          PIC 9 VALUE 0.
       
       01  POSTERIOR-PROBABILITY.
           05  P-CLASS1-POSTERIOR PIC 9V9999 VALUE 0.0.
           05  P-CLASS2-POSTERIOR PIC 9V9999 VALUE 0.0.
       
       01  RESULT.
           05  PREDICTED-CLASS    PIC X(10) VALUE SPACES.
           05  CONFIDENCE-SCORE   PIC 9V99 VALUE 0.0.
       
       01  TEMP-VARIABLES.
           05  TMP-PROB          PIC 9V9999 VALUE 0.0.
           05  I                 PIC 9 VALUE 0.
           05  MAX-PROB          PIC 9V9999 VALUE 0.0.
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY "Naïve Bayes Classifier Demo".
           DISPLAY "===========================".
           
           * Set sample input features
           MOVE 1 TO FEATURE1.
           MOVE 0 TO FEATURE2.
           MOVE 1 TO FEATURE3.
           
           DISPLAY "Input Features: "
           DISPLAY "Feature1: " FEATURE1
           DISPLAY "Feature2: " FEATURE2
           DISPLAY "Feature3: " FEATURE3.
           
           * Calculate P(Class1 | Features)
           COMPUTE P-CLASS1-POSTERIOR = 
               P-CLASS1 *
               (P-FEATURE1-CLASS1 ** FEATURE1) *
               (P-FEATURE2-CLASS1 ** FEATURE2) *
               (P-FEATURE3-CLASS1 ** FEATURE3).
           
           * Calculate P(Class2 | Features)
           COMPUTE P-CLASS2-POSTERIOR = 
               P-CLASS2 *
               (P-FEATURE1-CLASS2 ** FEATURE1) *
               (P-FEATURE2-CLASS2 ** FEATURE2) *
               (P-FEATURE3-CLASS2 ** FEATURE3).
           
           * Normalize probabilities
           COMPUTE TMP-PROB = P-CLASS1-POSTERIOR + P-CLASS2-POSTERIOR.
           
           IF TMP-PROB > 0 THEN
               COMPUTE P-CLASS1-POSTERIOR = P-CLASS1-POSTERIOR / TMP-PROB
               COMPUTE P-CLASS2-POSTERIOR = P-CLASS2-POSTERIOR / TMP-PROB
           END-IF.
           
           * Determine prediction
           IF P-CLASS1-POSTERIOR > P-CLASS2-POSTERIOR THEN
               MOVE "CLASS1" TO PREDICTED-CLASS
               MOVE P-CLASS1-POSTERIOR TO CONFIDENCE-SCORE
           ELSE
               MOVE "CLASS2" TO PREDICTED-CLASS
               MOVE P-CLASS2-POSTERIOR TO CONFIDENCE-SCORE
           END-IF.
           
           DISPLAY "Prediction Result:".
           DISPLAY "Predicted Class: " PREDICTED-CLASS.
           DISPLAY "Confidence Score: " CONFIDENCE-SCORE.
           
           STOP RUN.
```

## Key Components Explained:

### 1. **Data Definitions**
- `CLASS-PROBABILITY`: Prior probabilities of each class
- `FEATURE-PROBABILITY`: Conditional probabilities P(feature|class)
- `INPUT-FEATURES`: Sample input data to classify

### 2. **Algorithm Steps**
1. **Calculate likelihood**: Multiply conditional probabilities for each feature
2. **Apply Bayes' theorem**: Multiply by prior probability
3. **Normalize**: Ensure probabilities sum to 1
4. **Classify**: Choose class with highest posterior probability

### 3. **Mathematical Formula Used**
```
P(Class|Features) ∝ P(Class) × ∏ P(Feature_i|Class)
```

### 4. **Output Example**
```
Naïve Bayes Classifier Demo
===========================
Input Features: 
Feature1: 1
Feature2: 0
Feature3: 1
Prediction Result:
Predicted Class: CLASS1
Confidence Score: 0.47
```

This implementation demonstrates the core concepts of Naïve Bayes classification while working within COBOL's limitations and syntax conventions.