# Apriori Algorithm Example in R

## Installation and Setup

```r
# Install required packages
install.packages(c("arules", "arulesViz"))

# Load libraries
library(arules)
library(arulesViz)
```

## Sample Dataset Creation

```r
# Create a sample transaction dataset
transactions <- list(
  c("Milk", "Bread", "Butter"),
  c("Milk", "Bread", "Eggs"),
  c("Milk", "Butter"),
  c("Bread", "Butter", "Eggs"),
  c("Milk", "Bread", "Butter", "Eggs"),
  c("Milk", "Bread"),
  c("Bread", "Butter"),
  c("Milk", "Eggs"),
  c("Bread", "Eggs"),
  c("Milk", "Bread", "Butter", "Eggs", "Cheese")
)

# Convert to transaction format
trans <- as(transactions, "transactions")
trans
```

## Apriori Algorithm Execution

```r
# Run Apriori algorithm
# minSupport = 0.3 (30% minimum support)
# minConfidence = 0.6 (60% minimum confidence)
rules <- apriori(trans, 
                  parameter = list(support = 0.3, confidence = 0.6))

# View the rules
rules
```

## Examining Results

```r
# Summary of rules
summary(rules)

# View the first 5 rules
inspect(head(rules, n = 5))

# View rules with high confidence
high_conf_rules <- sort(rules, by = "confidence", decreasing = TRUE)
inspect(head(high_conf_rules, n = 5))
```

## Visualization

```r
# Visualize rules
# Plot rules using graph method
plot(rules, method = "graph", control = list(type = "items"))

# Plot rules using matrix method
plot(rules, method = "matrix", interactive = TRUE)
```

## Rule Analysis

```r
# Get rules with specific items
# Find rules containing "Milk"
rules_with_milk <- subset(rules, items %in% "Milk")
inspect(rules_with_milk)

# Get rules with high lift
high_lift_rules <- sort(rules, by = "lift", decreasing = TRUE)
inspect(head(high_lift_rules, n = 5))

# Rule statistics
summary(rules)
```

## Advanced Example with Real Dataset

```r
# Using built-in dataset (if available)
# Load the "Groceries" dataset
data("Groceries")
head(Groceries, 3)

# Run apriori on groceries dataset
grocery_rules <- apriori(Groceries, 
                        parameter = list(support = 0.01, confidence = 0.1))

# View top rules
inspect(sort(grocery_rules, by = "lift", decreasing = TRUE)[1:5])
```

## Exporting Results

```r
# Convert rules to data frame for easier analysis
rules_df <- as(rules, "data.frame")
head(rules_df)

# Save rules to CSV
write.csv(rules_df, "association_rules.csv", row.names = FALSE)
```

## Key Parameters Explanation

- **Support**: Minimum frequency of itemsets in the dataset
- **Confidence**: Minimum confidence threshold for rules
- **Lift**: Measure of how much more likely the consequent is given the antecedent

The Apriori algorithm finds frequent itemsets and generates association rules that can be used for market basket analysis, recommendation systems, and other pattern mining applications.

