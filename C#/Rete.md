# Rete Algorithm Example in C#

The Rete algorithm is a popular pattern matching algorithm used in expert systems and business rule engines. Here's a simple implementation in C#:

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

// Base class for all facts
public abstract class Fact
{
    public int Id { get; set; }
}

// Example fact types
public class Person : Fact
{
    public string Name { get; set; }
    public int Age { get; set; }
    public string City { get; set; }
}

public class Order : Fact
{
    public string CustomerName { get; set; }
    public decimal Amount { get; set; }
    public DateTime Date { get; set; }
}

// Rule condition class
public class Condition
{
    public string Field { get; set; }
    public string Operator { get; set; }
    public object Value { get; set; }
}

// Rule class
public class Rule
{
    public string Name { get; set; }
    public List<Condition> Conditions { get; set; }
    public Action<Fact[]> Action { get; set; }
    
    public Rule(string name, List<Condition> conditions, Action<Fact[]> action)
    {
        Name = name;
        Conditions = conditions ?? new List<Condition>();
        Action = action;
    }
}

// Rete Network implementation
public class ReteNetwork
{
    private List<Rule> rules;
    private List<Fact> facts;
    
    public ReteNetwork()
    {
        rules = new List<Rule>();
        facts = new List<Fact>();
    }
    
    public void AddFact(Fact fact)
    {
        facts.Add(fact);
        Console.WriteLine($"Added fact: {fact.GetType().Name} with ID {fact.Id}");
    }
    
    public void AddRule(Rule rule)
    {
        rules.Add(rule);
        Console.WriteLine($"Added rule: {rule.Name}");
    }
    
    public void FireRules()
    {
        Console.WriteLine("\n=== Firing Rules ===");
        
        foreach (var rule in rules)
        {
            var matchingFacts = FindMatchingFacts(rule);
            
            if (matchingFacts.Count > 0)
            {
                Console.WriteLine($"Rule '{rule.Name}' matched {matchingFacts.Count} facts");
                rule.Action(matchingFacts.ToArray());
            }
        }
    }
    
    private List<Fact> FindMatchingFacts(Rule rule)
    {
        var matchingFacts = new List<Fact>();
        
        // For simplicity, this example matches against all facts
        // In a real Rete implementation, this would be more sophisticated
        foreach (var fact in facts)
        {
            if (EvaluateConditions(fact, rule.Conditions))
            {
                matchingFacts.Add(fact);
            }
        }
        
        return matchingFacts;
    }
    
    private bool EvaluateConditions(Fact fact, List<Condition> conditions)
    {
        // Simple evaluation - in a real implementation, this would use
        // the Rete network's alpha and beta memory structures
        
        foreach (var condition in conditions)
        {
            var property = fact.GetType().GetProperty(condition.Field);
            
            if (property == null)
                return false;
                
            var factValue = property.GetValue(fact);
            
            if (!CompareValues(factValue, condition.Value, condition.Operator))
                return false;
        }
        
        return true;
    }
    
    private bool CompareValues(object factValue, object ruleValue, string op)
    {
        try
        {
            switch (op.ToLower())
            {
                case "=":
                case "==":
                    return factValue?.Equals(ruleValue) ?? false;
                case ">":
                    return Comparer<object>.Default.Compare(factValue, ruleValue) > 0;
                case "<":
                    return Comparer<object>.Default.Compare(factValue, ruleValue) < 0;
                case ">=":
                    return Comparer<object>.Default.Compare(factValue, ruleValue) >= 0;
                case "<=":
                    return Comparer<object>.Default.Compare(factValue, ruleValue) <= 0;
                default:
                    return false;
            }
        }
        catch
        {
            return false;
        }
    }
}

// Example usage
public class Program
{
    public static void Main()
    {
        // Create Rete network
        var rete = new ReteNetwork();
        
        // Add facts
        rete.AddFact(new Person { Id = 1, Name = "John", Age = 25, City = "New York" });
        rete.AddFact(new Person { Id = 2, Name = "Jane", Age = 30, City = "Boston" });
        rete.AddFact(new Order { Id = 3, CustomerName = "John", Amount = 150.00m, Date = DateTime.Now });
        rete.AddFact(new Order { Id = 4, CustomerName = "Jane", Amount = 200.00m, Date = DateTime.Now });
        
        // Add rules
        var rule1 = new Rule(
            "Young Person Rule",
            new List<Condition>
            {
                new Condition { Field = "Age", Operator = "<", Value = 35 }
            },
            facts =>
            {
                Console.WriteLine("Executing: Young Person Rule");
                foreach (var fact in facts)
                {
                    if (fact is Person person)
                        Console.WriteLine($"  - {person.Name} is young (age {person.Age})");
                }
            }
        );
        
        var rule2 = new Rule(
            "High Value Order Rule",
            new List<Condition>
            {
                new Condition { Field = "Amount", Operator = ">", Value = 100.00m }
            },
            facts =>
            {
                Console.WriteLine("Executing: High Value Order Rule");
                foreach (var fact in facts)
                {
                    if (fact is Order order)
                        Console.WriteLine($"  - {order.CustomerName} placed high value order (${order.Amount})");
                }
            }
        );
        
        var rule3 = new Rule(
            "Person from New York Rule",
            new List<Condition>
            {
                new Condition { Field = "City", Operator = "=", Value = "New York" }
            },
            facts =>
            {
                Console.WriteLine("Executing: Person from New York Rule");
                foreach (var fact in facts)
                {
                    if (fact is Person person)
                        Console.WriteLine($"  - {person.Name} lives in {person.City}");
                }
            }
        );
        
        rete.AddRule(rule1);
        rete.AddRule(rule2);
        rete.AddRule(rule3);
        
        // Fire all rules
        rete.FireRules();
    }
}
```

## Expected Output:
```
Added fact: Person with ID 1
Added fact: Person with ID 2
Added fact: Order with ID 3
Added fact: Order with ID 4
Added rule: Young Person Rule
Added rule: High Value Order Rule
Added rule: Person from New York Rule

=== Firing Rules ===
Rule 'Young Person Rule' matched 2 facts
Executing: Young Person Rule
  - John is young (age 25)
  - Jane is young (age 30)
Rule 'High Value Order Rule' matched 2 facts
Executing: High Value Order Rule
  - John placed high value order ($150.00)
  - Jane placed high value order ($200.00)
Rule 'Person from New York Rule' matched 1 facts
Executing: Person from New York Rule
  - John lives in New York
```

## Key Components of this Rete Implementation:

1. **Facts**: Data objects that represent information in the system
2. **Rules**: Conditions that define when actions should be triggered
3. **Conditions**: Specific criteria that facts must match
4. **ReteNetwork**: Main engine that manages facts and rules
5. **Pattern Matching**: Logic to determine which facts match which rules

This simplified implementation demonstrates the core concepts of the Rete algorithm, though a full production implementation would include:
- Alpha memory for storing condition nodes
- Beta memory for storing partial matches
- Network optimization techniques
- Conflict resolution strategies
- More sophisticated pattern matching